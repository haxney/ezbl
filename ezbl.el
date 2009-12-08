;;; ezbl.el --- Emacs interface for Uzbl (uzbl.org)
;;
;; Author: Daniel Hackney
;; Copyright (C) 2009 Daniel Hackney
;; Version: 0.3
;; URL: http://github.com/dhax/ezbl/tree/master

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:
;;
;;  Uzbl version 2009.07.18 or greater is required. It has not been tested with
;;  other versions.
;;
;;  Run `ezbl-open' to start a Uzbl instance and browse to a URL. Also, check
;;  `ezbl-mode' for a listing of the key bindings of Ezbl.

(eval-when-compile
  (require 'cl))
(require 'url-cookie)

(defgroup ezbl nil "Settings for Ezbl, the Emacs frontend for Uzbl.")

(defcustom ezbl-exec-path "/usr/bin/uzbl"
  "The location of the Uzbl executable."
  :group 'ezbl
  :type 'file)

(defcustom ezbl-cookie-socket "/tmp/ezbl.sock"
  "The location of the socket through which to handle cookies."
  :group 'ezbl
  :type 'file)

(defvar ezbl-inst-list nil
  "A list of Uzbl instances.

Has the format:

  ((pid  . instance)
   (pid2 . instance2))

See `ezbl-inst-start' for a description of the format of the instance
variable.")

(defvar ezbl-inst nil
  "A buffer-local variable storing the current Ezbl instance.

See `ezbl-inst-start' for a description of the format of this
variable.")

(defvar ezbl-cookie-process nil
  "The process which is listening for cookie requests from Uzbl
processes.")

(defvar ezbl-initialized nil
  "Keeps track of whether or not Ezbl has been initialized. This
should be set by `ezbl-init'.")


(defstruct ezbl-inst
  "A structure containing the properties of an Ezbl instance."
  args
  process
  pid
  output-buffer
  display-buffer
  vars)

(defconst ezbl-inst-slots
  '(args
    process
    pid
    output-buffer
    display-buffer
    vars)
  "A list of the slot names in the `ezbl-inst' structure.")

(defconst ezbl-inst-get-first
  '(ezbl-inst-get-first
    nil ;; protected
    t   ;; enabled
    (advice . (lambda ()
                (ad-set-arg 0 (ezbl-inst-get (ad-get-arg 0))))))
  "Computed advice to apply to each of the `ezbl-inst' slot
accessors.")

(defconst ezbl-output-buffer-format " *ezbl-output-%s*"
  "The format used for transforming ezbl instance names into
buffer names.")

(defconst ezbl-display-buffer-format "*ezbl-display-%s*"
  "The format used for transforming ezbl instance names into
buffer names.")

(defvar ezbl-commands
  '(((name . "set")
     (format . "set <key> = <value>")
     (interactive . (let* ((var-name (completing-read "Variable to set: "
                                                      (mapcar '(lambda (item)
                                                                 (symbol-name (car item))) ezbl-variables)
                                                      nil ;; predicate
                                                      t   ;; require-match
                                                      nil ;; initial-input
                                                      'ezbl-command-set-history)) ;; hist
                           (default (ezbl-variable-get nil var-name))
                           (new-val (read-string (format "New value (%s): " default) nil nil default)))
                      (list nil var-name new-val)))
     (key-binding . "C-c C-s")
     (doc . "Used for changing variables on the fly. Sets KEY
     equal to VALUE.

* the changes are effective immediately; for example, setting the
  variable uri will make uzbl start loading, and changing
  status_format will make the status bar react immediately

* if you want to unset a string, use `set' with one space as the
  value."))

    ((name . "print")
     (format . "print <key>")
     (doc . "Print the value of KEY.

If KEY contains a string of the form '@var', the value of the Uzl
variable 'var' is printed.

* use this to print the value of a variable."))
    ((name . "bind")
     (format . "bind <string> = <command>")
     (interactive . "U\nsKey sequence: \nsCommand: ")
     (doc . "Sets the character sequence STRING to invoke COMMAND
     when typed interactively in Uzbl.

* there are a few tricks you can do:

  - STRING ends with an underscore: the command will only be
    invoked after pressing return/enter. If the user enters text
    where STRING has the underscore, %s in the COMMAND string
    will be replaced by this text. (optional)

  - STRING ends with an asterisk: similar behavior as with an
    underscore, but also makes the binding incremental (i.e. the
    command will be invoked on every keystroke).

  - STRING ends on a different character: you need to type the
    full string, which will trigger the command immediately,
    without pressing enter/return.

* examples:
  - bind o _ = uri %s
                + uzbl will load the url when you type: 'o '
  - bind /* = search %s

    + a search command which is called on every character typed
      after the slash, letting you see the search narrow down
      while typing.

    + Hitting return, enter or esc will terminate the search.

  - bind ZZ = exit

    + When you type ZZ and nothing else, the exit command will be
      triggered immediately."))
    ((name . "back")
     (format . "back")
     (interactive . "U")
     (key-binding . "C-c C-b")
     (doc . "Move backwards in the browser history."))
    ((name . "forward")
     (format . "forward")
     (interactive . "U")
     (key-binding . "C-c C-f")
     (doc . "Move forwards in the browser history."))
    ((name . "scroll_vert")
     (format . "scroll_vert <amount>")
     (interactive . "U\nnScroll amount: ")
     (key-binding . "C-c C-v")
     (doc . "Scroll vertically by AMOUNT.

AMOUNT is specified either in pixels, with a 'px' ending (55px)
or percentage (55%).

* amount is given in pixels(?) or as a percentage of the size of
  the view

* set amount to 100% to scroll a whole page."))
    ((name . "scroll_horz")
     (format . "scroll_horz <amount>")
     (interactive . "U\nnScroll amount: ")
     (key-binding . "M-v")
     (doc . "Scroll horizontally by AMOUNT.

AMOUNT is specified either in pixels, with a 'px' ending (55px)
or percentage (55%).

* amount is given in pixels(?) or as a percentage of the size of
  the view

* set amount to 100% to scroll a whole page"))
    ((name . "scroll_begin")
     (format . "scroll_begin")
     (interactive . "U")
     (key-binding . "C-a")
     (doc . "Scroll to the beginning of the page."))
    ((name . "scroll_end")
     (format . "scroll_end")
     (interactive . "U")
     (key-binding . "C-e")
     (doc . "Scroll to the end of the page."))
    ((name . "reload")
     (format . "reload")
     (interactive . "U")
     (key-binding . "C-c C-r")
     (doc . "Reload the current page."))
    ((name . "reload_ign_cache")
     (format . "reload_ign_cache")
     (interactive . "U")
     (doc . "Reload the current page, clearing the cache."))
    ((name . "stop")
     (format . "stop")
     (interactive . "U")
     (key-binding . "C-c C-g")
     (doc . "Stop the currently loading page."))
    ((name . "zoom_in")
     (format . "zoom_in")
     (interactive . "U")
     (key-binding . "C-c z i")
     (doc . "Increase the zoom level."))
    ((name . "zoom_out")
     (format . "zoom_out")
     (interactive . "U")
     (key-binding . "C-c z o")
     (doc . "Decrease the zoom level."))
    ((name . "uri")
     (format . "uri <address>")
     (interactive . "U\nsAddress: ")
     (key-binding . "C-c C-o")
     (doc . "Visit the Uri ADDRESS"))
    ((name . "js")
     (format . "js <body>")
     (interactive . "U\nsJavascript to execute: ")
     (doc . "Execute JavaScript within the browser.

* execute the javascript in BODY.
* remember that the commands must not contain line breaks."))
    ((name . "script")
     (format . "script <file>")
     (interactive . "U\nfJavascript file to execute: ")
     (key-binding . "C-c C-j")
     (doc . "execute the JavaScript in FILE."))
    ((name . "toggle_status")
     (format . "toggle_status")
     (interactive . "U")
     (doc . ""))
    ((name . "spawn")
     (format . "spawn <executable> <additonal_args>")
     (interactive . "U\nFFile to spawn\nsAdditional arguments:")
     (doc . "Runs a command.

* See the \"external scripts\" section of the Uzbl readme for
  details.

* PATH is searched so giving the full path to commands is not
  necessary.

* note that the arguments as specified in \"external scripts\"
  are appended at the end, so the argument numbers will be
  higher."))
    ((name . "sh")
     (format . "sh <command>")
     (interactive . "U\nsCommand to execute: ")
     (doc . "Run a shell command.

* runs a shell command by expanding %s in the shell_cmd variable
  with the specified command; primarily useful as a shortcut for
  \"spawn sh -c BODY\"

* note that the arguments as specified in \"external scripts\"
  are appended at the end, so the argument numbers will be
  higher."))
    ((name . "sync_spawn")
     (format . "sync_spawn <executable> <additional_args>")
     (interactive . "U\nFFile to spawn\nsAdditional arguments:")
     (doc . "Tell Uzbl to synchronously spawn a command.

See `ezbl-command-spawn' for details.

* these are synchronous variants of spawn and sh, which means
  uzbl will wait for them to return.

* you should only need to use these manually if you want to use a
  chain command in a handler that wants output from the command
  it runs"))
    ((name . "sync_sh")
     (format . "sync_sh <command>")
     (interactive . "U\nsCommand to run: ")
     (doc . "Tell Uzbl to synchronously run a shell command.

See `ezbl-command-sh' for details.

* these are synchronous variants of spawn and sh, which means
  uzbl will wait for them to return.

* you should only need to use these manually if you want to use a
  chain command in a handler that wants output from the command
  it runs"))
    ((name . "exit")
     (format . "exit")
     (interactive . "U")
     (key-binding . "C-c C-q")
     (doc . "Close this instance of Uzbl."))
    ((name . "search")
     (format . "search <string>")
     (interactive . "U\nsSearch: ")
     (key-binding . "C-s")
     (doc . "Search for STRING within the content of the current
     Uzbl page.

* search with no string will search for the next/previous
  occurrence of the string previously searched for."))
    ((name . "search_reverse")
     (format . "search_reverse <string>")
     (interactive . "U\nsSearch backward: ")
     (key-binding . "C-r")
     (doc . "Search backwards for STRING in the current page.

* search with no string will search for the next/previous
  occurrence of the string previously searched for."))
    ((name . "toggle_insert_mode")
     (format . "toggle_insert_mode <optional_state>")
     (interactive . "U")
     (doc . "Set the insert mode to OPTIONAL_STATE.

If the optional state is 0, disable insert mode. If 1, enable
insert mode."))
    ((name . "dump_config")
     (format . "dump_config")
     (doc . "Dump the current Uzbl configuration.

* dumps your current config (which may have been changed at
  runtime) to stdout, in a format you can use to pipe into uzbl
  again (or use as config file)"))
    ((name . "keycmd")
     (format . "keycmd <string>")
     (interactive . "U\nsSet command buffer to: ")
     (doc . "Set the interactive command buffer to STRING.

If STRING is a valid binding, it will execute."))
    ((name . "keycmd_nl")
     (format . "keycmd_nl <string>")
     (interactive . "U\nsSet command buffer to: ")
     (doc . "Set the interactive command buffer to STRING and emulate pressing return.

See `ezbl-command-keycmd'.

`ezbl-command-keycmd_nl' is like `ezbl-command-keycmd', but it
also emulates a press of return, causing bindings with a
parameter to execute. For example, keycmd_nl o google.com would
load the said url if you have a binding like \"bind o _ = uri %s\"."))
    ((name . "keycmd_bs")
     (format . "keycmd_bs")
     (interactive . "U")
     (doc . "Erase (backspace) one character from the command buffer."))
    ((name . "chain")
     (format . "chain <command> <command2>")
     (interactive . "U\nsCommand 1: \nsCommand 2: ")
     (doc . "Use for chaining multiple commands.

* remember to quote the commands; one command must come as one
  parameter.

* If you use chain with a handler script which must return some
  output (such as a cookie handler -- uzbl will wait for and use
  its output), use 'sync_spawn' or 'sync_sh' instead of 'spawn'
  or 'sh' in the command that should give the output.")))
  "A list of commands which Uzbl accepts. These are used to
generate the functions to call each command.

The following attributes can be used in each alist:

- name (mandatory)

  The name of the command. This is the string that is passed to
  Uzbl when invoking the command.

- format (mandatory)

  The format of the command, for example

    scroll_vert <amount>

- interactive (optional)

  The interactive spec for the function. If this attribute is not
  present, then the function will not be interactive.

- doc (optional)

  The documentation string to provide for the function.

- key-binding (optional)

  The key binding in `ezbl-mode-map' of the function. If it is
  not included, no keybinding is set for that command. The value
  must be a string to be supplied to the `kbd' macro.

All variables must be enclosed in angle brackets.")

(defvar ezbl-variables
  '((uri . "(callback: load the uri)")
    (verbose . "affects output on stdout")
    (mode . "insert or command mode")
    (inject_html . "base_url: used when passing html through stdin")
    (html_endmarker . "delimiter when passing html through stdin")
    (html_mode_timeout . "consider end of html input after x seconds when no endmarker found")
    (keycmd . "holds the input buffer (callback: update input buffer)")
    (status_message . "(callback: update title)")
    (show_status . "show statusbar or not")
    (status_top . "statusbar on top?")
    (status_format . "marked up, to be expanded string for statusbar (callback: update statusbar)")
    (status_pbar_done . "character to denote done % of pageload")
    (status_pbar_pending . "character to denote pending % of pageload")
    (status_pbar_width . "width of progressbar")
    (status_background . "color which can be used to override Gtk theme.")
    (insert_indicator . "string to denote insert mode")
    (command_indicator . "string to denote command mode")
    (title_format_long . "titlebar string when no statusbar shown (will be expanded")
    (title_format_short . "titlebar string when statusbar shown (will be expanded)")
    (icon . "path to icon for Gtk")
    (insert_mode . "whether insert mode is active")
    (always_insert_mode . "set this to true if you don't like modal (vim-like) interfaces")
    (reset_command_mode . "automatically revert to command mode on pageload (unless alwaysinsertmode is set)")
    (modkey . "modkey which can be pressed to activate keybind from inside insert mode")
    (load_finish_handler)
    (load_start_handler)
    (load_commit_handler)
    (history_handler)
    (download_handler)
    (cookie_handler)
    (new_window . "handler to execute to invoke new uzbl window (TODO better name)")
    (fifo_dir . "location to store fifo's")
    (socket_dir . "location to store sockets")
    (http_debug . "http debug mode (value 0-3)")
    (shell_cmd . "alias which will be expanded to use shell commands (eg sh -c)")
    (proxy_url . "http traffic socks proxy (eg: http://:)")
    (max_conns)
    (max_conns_host)
    (useragent . "to be expanded strin")
    (zoom_level)
    (font_size)
    (monospace_size)
    (minimum_font_size)
    (disable_plugins . "(TODO rename to enable)")
    (disable_scripts . "(TODO rename to enable)")
    (autoload_images)
    (autoshrink_images . "shrink images to window size (default 0)")
    (enable_spellcheck)
    (enable_private)
    (print_backgrounds . "print background images? (default 0)")
    (stylesheet_uri . "use this to override the pagelayout with a custom stylesheet")
    (resizable_text_areas)
    (default_encoding . "iso-8859-1 by default")
    (enforce_96_dpi . "1 by default")
    (caret_browsing . ""))
  "The variables available from Uzbl.")

(defconst ezbl-handlers '("load_finish_handler"
                          "load_start_handler"
                          "load_commit_handler"
                          "history_handler"
                          "download_handler"
                          "cookie_handler"
                          "new_window")
  "A list of the Uzbl handler variables.")

(defvar ezbl-handler-path (if (null load-file-name)
                              (locate-library "handler.py" t)
                            (concat (file-name-directory load-file-name) "handler.py"))
  "The path of the Uzbl callback handler.")

(defvar ezbl-xwidget-id-counter 0
  "Counter for xwidget IDs. IDs must be unique, or Emacs will crash.")

(defconst ezbl-xwidget-type 3
  "The type attribute for xwidget embedded widgets.")

(defvar ezbl-xembed-ready-hook nil
  "Commands to run when an ezbl instance receives the
`xembed-ready' signal.")

(defun ezbl-command-get-args (command)
  "Extracts the arguments (as symbols) from a Uzbl command specification.

For example, the spec

  scroll_vert <amount>

Would return (amount)."
  (let ((start 0)
        (args nil))
    (while (string-match "<\\([[:alnum:]_-]+\\)>" command start)
      (setq args (add-to-list 'args (intern (match-string 1 command)) t))
      (setq start (match-end 1)))
    args))

(defun ezbl-command-make-func (spec)
  "Creates a function which produces the Uzbl command string described by SPEC.

The function created takes a number of arguments specified by the
`format' attribute of SPEC and returns a string suitable for
`ezbl-exec-command'.

Sets the `interactive' declaration to the `interactive' attribute
of SPEC.

It also defines a variable `ezbl-command-NAME-history' which can
be used by the function to record the input history of the
function when called interactively.

See `ezbl-commands' for a description of the format of SPEC."
  (let* ((name (cdr (assq 'name spec)))
         (format (cdr (assq 'format spec)))
         (args (ezbl-command-get-args format))
         (doc (cdr (assq 'doc spec)))
         (output-format (replace-regexp-in-string "<[[:alnum:]_-]+>" "%s" format))
         (interactive-spec (cdr (assq 'interactive spec)))
         (command-name (intern (concat "ezbl-command-" name)))
         (history-list (intern (format "%s-history" command-name))))
    ;; Make the symbol contained in `history-list' bound.
    (set history-list nil)
    (fset command-name
          `(lambda (inst ,@args)
             ,doc
             ,(when interactive-spec `(interactive ,interactive-spec))
             (ezbl-exec-command inst (format ,output-format ,@args))))))

(defun ezbl-command-init ()
  "Create Emacs functions from `ezbl-commands' and `ezbl-instance-spec'.

Read through `ezbl-commands' and call `ezbl-command-make-func' on
each one. Also, run through `ezbl-instance-spec' and call
`ezbl-make-instance-accessor-func' on each one."
  (interactive)
  (append (mapcar 'ezbl-command-make-func ezbl-commands)))

(defun ezbl-init ()
  "Starts up the services needed for Ezbl to run."
  (unless ezbl-initialized
    (when (or (not (boundp 'server-name)) (null server-name))
      (error "Emacs server is required for Ezbl, but `server-name' is nil."))
    (when (null server-process)
      (error "Emacs server is required for Ezbl, but the server is not started."))
    (unless (featurep 'xwidget)
      (error "This version of Emacs does not support embedding windows. Please get a patched version from http://github.com/jave/emacs"))
    (unless (featurep 'make-network-process '(:type seqpacket))
      (error "This version of Emacs does not support SEQPACKET sockets"))
    (ezbl-cookie-socket-listen nil t)
    (ezbl-inst-define-advice)
    (setq ezbl-initialized t)))

(defun ezbl-inst-start (&rest args)
  "Start an instance of Uzbl. ARGS is a keyword list of
options and values to pass to the Uzbl instance.

The following keywords are used:

:class VALUE
        Program class as used by the window manager
:gtk-name NAME
        Program name as used by the window manager
:screen SCREEN
        X screen to use
:sync nil
        Make X calls synchronous. Must include `nil' as the argument.
:gtk-module MODULES
        Load additional GTK+ modules. MODULES should be a list of
        string names of modules.
:g-fatal-warnings nil
        Make all warnings fatal. Must include `nil' as the argument.
:uri URI
        Uri to load at startup (equivalent to 'set uri = URI')
:verbose nil
        Whether to print all messages or just errors. Must
        include `nil' as the argument.
:name NAME
          Name of the current instance (defaults to Xorg window id)
:config FILE
        Config file (this is pretty much equivalent to 'uzbl < FILE' )
:socket SOCKET
        Socket ID for GtkSocket.
:display DISPLAY
        X display to use

Returns an `ezbl-inst' struct."
  (let (program-args)
    ;; Process keywords
    (while args
      (let ((arg (car args)))
        (setq args (cdr args))
        (unless (symbolp arg)
          (error "Junk in args %S" args))
        (let ((keyword arg)
              (value (car args)))
          (unless args
            (error "Keyword %s is missing an argument" keyword))
          (setq args (cdr args))
          (cond
           ((eq keyword :class)
            (setq program-args (append program-args (list "--class") (list value))))
           ((eq keyword :gtk-name)
            (setq program-args (append program-args (list "--gtk-name") (list value))))
           ((eq keyword :screen)
            (setq program-args (append program-args (list "--screen") (list value))))
           ((eq keyword :sync)
            (setq program-args (append program-args (list "--sync"))))
           ((eq keyword :gtk-module)
            (setq program-args (append program-args (list "--gtk-module")
                                       (list (mapconcat 'identity value ",")))))
           ((eq keyword :g-fatal-warnings)
            (setq program-args (append program-args (list "--g-fatal-warnings "))))
           ((eq keyword :uri)
            (setq program-args (append program-args (list "--uri") (list value))))
           ((eq keyword :verbose)
            (setq program-args (append program-args (list "--verbose"))))
           ((eq keyword :name)
            (setq program-args (append program-args (list "--name") (list value))))
           ((eq keyword :config)
            (setq program-args (append program-args (list "--config") (list value))))
           ((eq keyword :socket)
            (setq program-args (append program-args (list "--socket") (list value))))
           ((eq keyword :display)
            (setq program-args (append program-args (list "--display") (list value))))))))

    ;; Start process
    (let* (inst
           (proc-name "ezbl-process")
           (output-buffer (generate-new-buffer "*ezbl-output*"))
           (proc (apply 'start-process
                        (append (list proc-name
                                      output-buffer
                                      ezbl-exec-path)
                                program-args)))
           (pid (process-id proc)))

      (setq inst (make-ezbl-inst
                  :args program-args
                  :process proc
                  :pid pid
                  :output-buffer output-buffer
                  :display-buffer (current-buffer)
                  :vars (make-hash-table)))
      (set-process-filter proc 'ezbl-event-listener)

      (with-current-buffer output-buffer
        (rename-buffer (format ezbl-output-buffer-format (int-to-string pid)))
        (set (make-local-variable 'ezbl-inst) inst))
      (rename-buffer (format ezbl-display-buffer-format pid))
      (set (make-local-variable 'ezbl-inst) inst)

      ;; Make `ezbl-inst' survive `kill-all-local-variables'
      (put 'ezbl-inst 'permanent-local t)

      (add-to-list 'ezbl-inst-list (cons pid inst))
      (ezbl-mode)
      inst)))

(defun ezbl-inst-get (&optional inst strict)
  "Returns the ezbl instance from INST.

If INST is an ezbl instance, then it is returned unchanged. If it
is a buffer, then the local variable of `ezbl-inst' is
returned. If it is an integer, then `ezbl-inst-list' is searched
for an instance with a matching pid. If it is nil or not
supplied, then the value of `ezbl-inst' in the current buffer
is returned.

If STRICT is non-nil, raise an error if INST is not resolvable to
an instance.

Returns an `ezbl-inst'."
  (when (null inst)
    (set 'inst ezbl-inst))

  (let ((instance
         (cond
          ;; Is an instance.
          ((ezbl-inst-p inst)
           inst)
          ;; Is a buffer.
          ((bufferp inst)
           (with-current-buffer inst
             ezbl-inst))
          ;; Is a pid.
          ((integerp inst)
           (cdr-safe (assq inst
                           ezbl-inst-list)))
          ((processp inst)
           (cdr-safe (assq (process-id inst)
                           ezbl-inst-list)))
          ;; Is the name of a buffer.
          ((stringp inst)
           (if (and (bufferp (get-buffer inst))
                    (not (null (with-current-buffer inst
                                 ezbl-inst))))
               (with-current-buffer inst
                 ezbl-inst)
             ;; Is the name of an instance, so open the output buffer which
             ;; corresponds to this name.
             (when (get-buffer (format ezbl-output-buffer-format inst))
               (with-current-buffer (format ezbl-output-buffer-format inst)
                 ezbl-inst)))))))
    (if (ezbl-inst-p instance)
        instance
      (if strict
          (error (format "`%s' is not an Ezbl instance or resolvable to an Ezbl instance" inst))
        nil))))

(defun ezbl-inst-define-advice ()
  "Define and activate the advice for each slot in `ezbl-inst'.

Makes the accessors call `ezbl-inst-get' before operating, so
that the accessors work on things which are resolvable to an
`ezbl-inst', rather than only allowing the insts themselves."
  (mapc '(lambda (item)
           (let ((func (intern (concat "ezbl-inst-" (symbol-name item)))))
             (ad-add-advice func
                            ezbl-inst-get-first 'before 'first)
             (ad-activate func)))
        ezbl-inst-slots))

(defun ezbl-exec-command (inst command)
  "Sends the string COMMAND to the Uzbl instance INST.

If INST is a buffer, use the value of
`ezbl-inst' in that buffer. If

COMMAND is a Uzbl command as described by the Uzbl
readme (http://www.uzbl.org/readme.php).

See `ezbl-inst-start' for a description of the format of INST."
  ;; Append a newline (\n) to the end of COMMAND if one is not already there.
  (when (not (string= "\n" (substring command -1)))
    (setq command (concat command "\n")))
  (process-send-string (ezbl-inst-process inst) command))

(defun ezbl-sync-request (inst req)
  "Request Uzl to evaluate a request string REQ and wait for the result.

Uses the Uzbl \"print\" command to make a request to the Uzbl
process specified by INST. Waits until Uzbl replies and returns
the response that Uzbl produces.

The following substitutions are supported (see the Uzbl readme
for more info):

  @var: Returns the value of the variable \"var\".

  @{var}: Returns the value of the variable \"var\". Used to
          denote the beginning and end of a variable name.

  @(command)@: Executes the shell command \"command\" and returns
               its result.

  @<code>@: Executes the string \"code\" as javascript in the
            current page and returns the result.

  @[xml]@: Escapes any XML in the brackets."
  (let ((tag (sha1 (int-to-string (random)))))
    (with-current-buffer (ezbl-inst-output-buffer inst)
      (ezbl-command-print inst
                          (format "%s{%s}%s" tag req tag))
      (goto-char (point-max))
      ;; Keep trying until tag is found. TODO: avoid searching backwards through
      ;; the whole buffer.
      (while (not (re-search-backward (format "%s{\\(.*\\)}%s" tag tag) (point-min) t))
        (goto-char (point-max))
        ;; Sleep for 1 millisecond
        (sleep-for 0 1))
      (match-string 1))))

(defun ezbl-variable-get (inst var)
  "Return the value of VAR from the ezbl instance INST."
  (gethash (intern-soft var) (ezbl-inst-vars inst)))

(defun ezbl-run-js (inst js)
  "Execute the Javascript in JS on the Uzbl instance INST and
return the result."
  (ezbl-sync-request inst (concat "@<" js ">@")))

(defun ezbl-xwidget-insert (where id type title width height)
  "Insert an embedded widget.into the current buffer.

WHERE - The point at which to insert the widget.
ID - The id of the widget. MUST be unique and < 100!
TYPE - 1=button, 2=toggle btn, 3=xembed socket(id will be printed to stdout)
TITLE - The title of the embedded widget
WIDTH - The width of the widget
HEIGHT - The height of the widget"
  (save-excursion
    (goto-char where)
    (put-text-property
     (point)
     (1+ (point))
     'display
     (list 'xwidget ':xwidget-id id ':type type ':title title ':width width ':height height))))

(defun ezbl-embed ()
  "Embed the Uzbl window specified by NAME in its buffer."
  (save-excursion
    (use-local-map (make-sparse-keymap))
    (define-key (current-local-map) [(xwidget-event)] 'ezbl-xwidget-handler)
    (ezbl-xwidget-insert
     (point)                     ;; Where
     (ezbl-xwidget-next-id)      ;; ID
     ezbl-xwidget-type           ;; Type
     "ezbl"                      ;; Name
     600                         ;; Width
     600)))                      ;; Height

(defun ezbl-xwidget-next-id ()
  "Returns the next xwidget id based on the value of `ezbl-xwidget-id-counter'."
  (setq ezbl-xwidget-id-counter (1+ ezbl-xwidget-id-counter)))

(defun ezbl-xwidget-handler ()
  "Handle an xwidget event (such as when it is initialized."
  (interactive)
  (let* ((xwidget-event-type (nth 2 last-input-event))
         (xwidget-id (nth 1 last-input-event)))
    (cond
     ((eq xwidget-event-type 'xembed-ready)
      (let* ((xembed-id (nth 3 last-input-event)))
        (ezbl-inst-start :socket (number-to-string xembed-id)
                         :config "-") ;; Use stdin for config
        (run-hooks 'ezbl-xembed-ready-hook))))))

(defun ezbl-open (uri)
  "Create a new instance in its own buffer and browse to URI."
  (interactive "sUri: ")

  (ezbl-init)

  (switch-to-buffer (generate-new-buffer uri))

  ;; Currently has problems embedding into an empty buffer, so insert a space.
  (insert " ")
  (backward-char)

  (ezbl-embed)

  (add-hook 'ezbl-xembed-ready-hook
            `(lambda ()
               (ezbl-command-uri ezbl-inst ,uri))
            nil
            t)
  (put 'ezbl-xembed-ready-hook 'permanent-local t)

  (current-buffer))

(defun ezbl-callback (type &rest args)
  "The handler for all Uzbl handler functions.

TYPE must be one of:
  - \"load_finish_handler\"
  - \"load_start_handler\"
  - \"load_commit_handler\"
  - \"history_handler\"
  - \"download_handler\"
  - \"cookie_handler\"
  - \"new_window\"

The remaining arguments ARGS are specified by the \"EXTERNAL
SCRIPTS\" section of the Uzbl readme, and are as follows:

Handler scripts that are called by uzbl are passed the following
arguments:

$1 uzbl-config-file
$2 uzbl-pid
$3 uzbl-x-window-id
$4 uzbl-fifo-filename
$5 uzbl-socket-filename
$6 current page url
$7 current page title
.. [ script specific ] (optional)

The script specific arguments are this:

  - history:
      $8 date of visit (Y-m-d H:i:s localtime)
  - add bookmark:
      none
  - download:
      $8 url of item to download
  - cookie handler
      $8 GET/PUT
      $9 request address scheme (e.g. http or https)
      $10 request address host (if current page url is
          www.foo.com/somepage, this could be something else than
          foo, eg advertising from another host)
      $11 request address path $12 cookie (only with PUT requests)"
  (let* ((config-file (nth 0 args))
         (pid (string-to-int (nth 1 args)))
         (window-id (nth 2 args))
         (fifo-filename (nth 3 args))
         (socket-filename (nth 4 args))
         (current-url (nth 5 args))
         (current-title (nth 6 args))
         (buffer (ezbl-inst-display-buffer pid)))
    (with-current-buffer buffer
      (cond
       ((equal type "load_finish_handler"))
       ((equal type "load_start_handler"))
       ((equal type "load_commit_handler"))
       ((equal type "history_handler"))
       ((equal type "download_handler"))
       ((equal type "cookie_handler")
        ;;        (ezbl-cookie-handler args)
        )
       ((equal type "new_window"))
       (t
        (error (format "Unknown callback type '%s' received." type)))))))

(defun ezbl-cookie-listener (proc answer)
  "Handle a cookie request over a socket."
  (let* ((args (split-string answer "\0"))
         (result (apply 'ezbl-cookie-handler args)))
    (when (and result
               (> 0 (length (split-string result))))
      (process-send-string proc result))
    (process-send-eof proc)))

(defun ezbl-cookie-handler (op scheme host path &optional data &rest ignored)
  (let ((secure (if (equal scheme "https")
                    t
                  nil))
        (url-current-object (url-parse-make-urlobj scheme nil nil host nil path)))
    (when (equal op "PUT")
      (url-cookie-handle-set-cookie data))
    (when (equal op "GET")
      (url-cookie-generate-header-lines host path secure))))

(defun ezbl-cookie-socket-listen (&optional path force)
  "Begin listening for Uzbl cookie requests.

Creates a server process on a local socket at PATH, or
`ezbl-cookie-socket' if PATH is nil.

Starts a process and stores it in `ezbl-cookie-process' if it is
nil. If `ezbl-cookie-process' is non-nil, then don't create a
process unless FORCE is non-nil, in which case kill the existing
process and start a new one."
  (when ezbl-cookie-process
    (unless force
      (error "A cookie process already exists")))

  (let* ((sock-path (or path ezbl-cookie-socket)))
    (when (file-exists-p sock-path)
      (if force
          (delete-file sock-path)
        (error (format "Cannot listen on `%s', file exists" sock-path))))

    (make-network-process :name "ezbl-cookie"
                          :type 'seqpacket
                          :server t
                          :service sock-path
                          :family 'local
                          :filter 'ezbl-cookie-listener)))

(defun ezbl-cookie-set-handler (inst &optional path)
  "Set Ezbl instance INST's cookie_handler to
  `ezbl-cookie-socket' or PATH, if it's given."
  (ezbl-command-set inst "cookie_handler" (format "talk_to_socket %s" (or path ezbl-cookie-socket))))

(defun ezbl-init-handlers (&optional inst)
  "Initialize the Uzbl external script handlers.

INST is a valid input to `ezbl-inst-get'.

Sets the server-name parameter to the current value of `server-name'."
  (when (null ezbl-handler-path)
    (error "`ezbl-handler-path' is null, so \"handler.py\" could not be located.."))
  (mapc '(lambda (type)
           (ezbl-command-set inst type (format "spawn %s %s %s" ezbl-handler-path type server-name)))
        ezbl-handlers)
  (ezbl-cookie-set-handler inst))

(defun ezbl-update-mode-line-format ()
  "Updates the mode-line format in each ezbl display-window
  according to `ezbl-mode-line-format'."
  (mapc '(lambda (inst)
           (with-current-buffer (ezbl-inst-display-buffer (car inst))
             (setq mode-line-format ezbl-mode-line-format)))
        ezbl-inst-list))

(defun ezbl-set-mode-line-format (symbol value)
  "Used for setting `ezbl-mode-line-format'; sets SYMBOL's value
  to VALUE and runs `ezbl-update-mode-line-format'."
  (set-default symbol value)
  (ezbl-update-mode-line-format))

(defcustom ezbl-mode-line-format
  '("-"
    mode-line-mule-info
    mode-line-modified
    mode-line-frame-identification
    (:propertize (:eval (ezbl-variable-get ezbl-inst 'title))
                 face bold)
    " -- "
    (:eval (ezbl-variable-get ezbl-inst 'uri))
    "   "
    mode-line-modes
    (which-func-mode ("" which-func-format "--"))
    "-%-")
  "The format of the mode line in an Ezbl display buffer."
  :group 'ezbl
  :type 'sexp
  :set 'ezbl-set-mode-line-format)

(defvar ezbl-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc '(lambda (item)
             (let* ((binding (cdr (assq 'key-binding item)))
                    (name (cdr (assq 'name item)))
                    (func (intern (concat "ezbl-command-" name))))
               (when binding
                 (define-key map (read-kbd-macro binding) func))))
          ezbl-commands)
    map)
  "Keymap for `ezbl-mode'.")

(define-derived-mode ezbl-mode nil "Ezbl"
  "Mode for interacting with Ezbl processes
\\{ezbl-mode-map}"
  :group 'ezbl
  (toggle-read-only t)
  (set-buffer-modified-p nil)
  (add-hook 'window-configuration-change-hook 'ezbl-fill-window nil t))

(add-hook 'ezbl-mode-hook 'ezbl-init-handlers)
(add-hook 'ezbl-mode-hook 'ezbl-fill-window)
(add-hook 'ezbl-mode-hook 'ezbl-update-mode-line-format)

(defun ezbl-xwidget-resize-at (pos width height)
  "Resize xwidget at postion POS to WIDTH and HEIGHT.
theres no corresponding resize-id fn yet, because of display
property/xwidget id impedance mismatch.
"
  (let* ((xwidget-prop (cdr (get-text-property pos 'display)))
         (id (plist-get  xwidget-prop ':xwidget-id)))

    (setq xwidget-prop (plist-put xwidget-prop ':width width))
    (setq xwidget-prop (plist-put xwidget-prop ':height height))

    (put-text-property pos (+ 1 pos) 'display (cons 'xwidget xwidget-prop))
    (xwidget-resize id width height)))

(defun ezbl-fill-window (&optional inst)
  "Re-sizes the xwidget in the display-buffer of INST to fill its
entire window."
  (let ((buffer (ezbl-inst-display-buffer inst)))
    (with-current-buffer buffer
      (let* ((edges-list (window-inside-pixel-edges (get-buffer-window buffer)))
             (left (nth 0 edges-list))
             (top (nth 1 edges-list))
             (right (nth 2 edges-list))
             (bottom (nth 3 edges-list))
             (height (- bottom top))
             (width (- right left)))
        (toggle-read-only -1)
        (ezbl-xwidget-resize-at 1 width height)
        (toggle-read-only t)
        (set-buffer-modified-p nil)))))

(defun ezbl-event-listener (inst answer)
  "Filter for ezbl processes.

INST should be the `ezbl-inst' of the associated Uzbl process
and ANSWER is the string returned by the process."

  (let ((answers (split-string answer "\n" t)))
    (dolist (ans answers)
      (if (string-match "^EVENT \\[\\([0-9]+\\)\\] \\([A-Z_]+\\) ?\\(.*\\)$" ans)
          (let ((app-name (match-string-no-properties 1 ans))
                (event (intern (match-string-no-properties 2 ans)))
                (detail (match-string-no-properties 3 ans)))
            (ezbl-event-handler inst event detail app-name))
        (ezbl-process-append inst (concat ans "\n"))))))

(defun ezbl-event-handler (inst event detail &optional app-name)
  "Respond to a Uzbl-generated event.

EVENT is the interned symbol of the event Uzbl returned, while
DETAIL is a (possibly-empty) string containing any additional
information included with the event.

INST is resolvable to an ezbl instance."
  (cond
   ((eq event 'INSTANCE_START))
   ((eq event 'INSTANCE_EXIT))
   ((eq event 'VARIABLE_SET)
    (if (string-match "\\([a-z0-9_]+\\) \\(str\\|int\\|float\\) \\(.*\\)" detail)
        (let ((var-name (intern (match-string-no-properties 1 detail)))
              (type (match-string-no-properties 2 detail))
              (value (match-string-no-properties 3 detail)))
          (puthash var-name value (ezbl-inst-vars inst)))
      (error "VARIABLE_SET event had invalid details: `%s'" detail)))
   ((eq event 'COMMAND_EXECUTED))
   ;; ((eq event 'COMMAND_ERROR))
   ;; ((eq event 'GEOMETRY_CHANGED))
   ;; ((eq event 'FIFO_SET))
   ;; ((eq event 'SOCKET_SET))
   ((eq event 'LOAD_COMMIT)
    (puthash 'uri detail (ezbl-inst-vars inst)))
   ((eq event 'LOAD_START))
   ((eq event 'LOAD_FINISH))
   ;; ((eq event 'LOAD_ERROR))
   ((eq event 'LOAD_PROGRESS))
   ((eq event 'TITLE_CHANGED)
    (puthash 'title detail (ezbl-inst-vars inst)))
   ;; ((eq event 'DOWNLOAD_REQUEST))
   ((eq event 'LINK_HOVER))
   ((eq event 'LINK_UNHOVER))
   ;; ((eq event 'KEY_PRESS))
   ;; ((eq event 'KEY_RELEASE))
   ;; ((eq event 'SELECTION_CHANGED))
   ;; ((eq event 'NEW_WINDOW))
   ;; ((eq event 'WEBINSPECTOR))
   ;; ((eq event 'WEBINSPECTOR))
   ;; ((eq event 'FOCUS_GAINED))
   ;; ((eq event 'FOCUS_LOST))
   ;; ((eq event 'FORM_ACTIVE))
   ;; ((eq event 'ROOT_ACTIVE))
   ;; ((eq event 'FILE_INCLUDED))
   ((eq event 'PLUG_CREATED))
   ((eq event 'BUILTINS))
   (t
    (ezbl-process-append inst (format "EVENT [%s] %s %s\n" app-name event detail)))))

(defun ezbl-process-append (inst string)
  "Insert STRING at the end of the process buffer of INST.

Takes into account the current position of process-mark for the
process owning buffer."
  (with-current-buffer (ezbl-inst-output-buffer inst)
    (let* ((proc (ezbl-inst-process inst))
           (moving (= (point) (process-mark proc))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(ezbl-command-init)

(provide 'ezbl)

;;; ezbl.el ends here
