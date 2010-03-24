;;; ezbl.el --- Emacs interface for Uzbl (uzbl.org)
;;
;; Version: 0.4
;; URL: http://github.com/dhax/ezbl

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
;;  Uzbl version 2009.11.30 or greater is required. Ezbl will not work with
;;  older versions.
;;
;;  Run `ezbl-open' to start a Uzbl instance and browse to a URL. Also, check
;;  `ezbl-mode' for a listing of the key bindings of Ezbl.

(eval-when-compile
  (require 'cl))
(require 'url-cookie)

(defgroup ezbl nil "Settings for Ezbl, the Emacs frontend for Uzbl.")

(defcustom ezbl-exec-path (or (executable-find "uzbl-core") "/usr/bin/uzbl-core")
  "The location of the Uzbl executable."
  :group 'ezbl
  :type 'file)

(defcustom ezbl-cookie-socket "/tmp/ezbl-cookies"
  "The location of the socket through which to handle cookies."
  :group 'ezbl
  :type 'file)

(defvar ezbl-inst-list nil
  "An alist of Uzbl instances and their pids.

Has the format:

  ((pid  . instance)
   (pid2 . instance2))

The values are instances of the `ezbl-inst' struct.")

(defvar ezbl-inst nil
  "A buffer-local variable storing the current Ezbl instance.

It is an instance of the `ezbl-inst' struct.")

(defvar ezbl-cookie-process nil
  "The process which is listening for cookie requests from Uzbl
processes.")

(defvar ezbl-initialized nil
  "Keeps track of whether or not Ezbl has been initialized. This
should only be set by `ezbl-init'.")

(defconst ezbl-inst-slots
  '(args
    process
    pid
    output-buffer
    display-buffer
    vars)
  "A list of the slot names in the `ezbl-inst' structure.

`defstruct' does not keep a list of the fields of the struct for
later use, so do this manually.")

(eval `(defstruct ezbl-inst
         "A structure containing the properties of an Ezbl instance."
         ,@ezbl-inst-slots))

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
  '(((name . "back")
     (format . "back")
     (interactive . "U")
     (key-binding . "C-c C-b")
     (doc . "Move backwards in the browser history."))

    ((name . "forward")
     (format . "forward")
     (interactive . "U")
     (key-binding . "C-c C-f")
     (doc . "Move forwards in the browser history."))

    ((name . "scroll")
     (format . "scroll <direction> <argument>")
     (interactive . "U\nnScroll amount: ")
     (key-binding . "C-c C-v")
     (doc . "Scroll in DIRECTION by ARGUMENT.

* argument can be `begin`, `end`, or an amount given in pixels or
  as a percentage of the size of the view.

* set the amount to 100% to scroll a whole page"))

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

    ((name . "toggle_zoom_type")
     (format . "toggle_zoom_type")
     (interactive)
     (doc . "Toggles the variable 'zoom_type' between
'full-content' and 'text-only' zoom. In 'text-only' zoom, only
the text of the page is zoomed, while in 'full-content' zoom,
images and other page elements are zoomed along with the text."))

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
     (doc . "Toggle the display of the status bar."))

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

    ((name . "talk_to_socket")
     (format . "talk_to_socket <socketfile> <args>")
     (doc . "Lets uzbl talk to a socketfile."))

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

    ((name . "search_clear")
     (format . "search_clear")
     (interactive . "U")
     (doc . "Unmark and clear the search string"))

    ((name . "dehilight")
     (format . "dehilight")
     (interactive)
     (doc . "Remove highlighting of search matches."))

    ((name . "set")
     (format . "set <key> = <value>")
     (interactive . (let* ((var-name
                            (completing-read "Variable to set: "
                                             (mapcar '(lambda (item)
                                                        (cdr-safe (assq 'name item))) ezbl-variables)
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

    ((name . "dump_config")
     (format . "dump_config")
     (doc . "Dump the current Uzbl configuration.

* dumps your current config (which may have been changed at
  runtime) to stdout, in a format you can use to pipe into uzbl
  again (or use as config file)"))

    ((name . "dump_config_as_events")
     (format . "dump_config_as_events")
     (doc . "Dump the current config as a series of
'VARIABLE_SET' events, which can be handled by an event manager."))

    ((name . "chain")
     (format . "chain <command> <command2>")
     (interactive . "U\nsCommand 1: \nsCommand 2: ")
     (doc . "Use for chaining multiple commands.

* remember to quote the commands; one command must come as one
  parameter.

* If you use chain with a handler script which must return some
  output (such as a cookie handler -- uzbl will wait for and use
  its output), use 'sync_spawn' or 'sync_sh' instead of 'spawn'
  or 'sh' in the command that should give the output."))

    ((name . "print")
     (format . "print <key>")
     (doc . "Print the value of KEY.

If KEY contains a string of the form '@var', the value of the Uzl
variable 'var' is printed.

* use this to print the value of a variable."))

    ((name . "event")
     (format . "event <name> <details>")
     (interactive . "U")
     (doc . "Send custom event.
NAME is the event name and DETAILS is additional information to include."))

    ((name . "request")
     (format . "request <name> <details>")
     (interactive . "U")
     (doc . "Send custom request.
Same idea as events, but to be processed by EM, not uzbl-core."))

    ((name . "menu_add")
     (format . "menu_add <label>  = <command>"))

    ((name . "menu_link_add")
     (format . "menu_link_add <label>  = <command>"))

    ((name . "menu_image_add")
     (format . "menu_image_add <label>  = <command>"))

    ((name . "menu_editable_add")
     (format . "menu_editable_add <label>  = <command>")
     (doc . "add a new entry LABEL that will execute COMMAND to one
of the right click context menus."))

    ((name . "menu_separator")
     (format . "menu_separator <label>"))

    ((name . "menu_link_separator")
     (format . "menu_link_separator <label>"))

    ((name . "menu_image_separator")
     (format . "menu_image_separator <label>"))

    ((name . "menu_editable_separator")
     (format . "menu_editable_separator <label>")
     (doc . "Adds a separator line to one of the right click context menus."))

    ((name . "menu_remove")
     (format . "menu_remove <label>"))

    ((name . "menu_link_remove")
     (format . "menu_link_remove <label>"))

    ((name . "menu_image_remove")
     (format . "menu_image_remove <label>"))

    ((name . "menu_editable_remove")
     (format . "menu_editable_remove <label>")
     (doc . "Removes the entry LABEL from one of the right click context menus."))

    ((name . "hardcopy")
     (format . "hardcopy")
     (doc . "open print dialog"))

    ((name . "include")
     (format . "include <file>")
     (doc . "read contents of file and interpret commands")))
  "A list of commands which Uzbl accepts. These are used to
generate the functions to call each command.

The following attributes can be used in each alist:

- name (mandatory)

  The name of the command. This is the string that is passed to
  Uzbl when invoking the command.

- format (mandatory)

  The format of the command, for example

    scroll <direction> <amount>

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
  '(((name . "uri")
     (doc . "(callback: load the uri)"))

    ((name . "verbose")
     (doc . "affects output on stdout"))

    ((name . "inject_html")
     (doc . "base_url: used when passing html through stdin"))

    ((name . "geometry")
     (doc . "Geometry and position of the Uzbl window. Format is \"<width>x<height>+<x-offset>+<y-offset>\"."))

    ((name . "keycmd")
     (doc . "Holds the input buffer (callback: update input buffer)"))

    ((name . "show_status")
     (doc . "show statusbar or not"))

    ((name . "status_top")
     (doc . "statusbar on top?"))

    ((name . "status_format")
     (doc . "marked up, to be expanded string for statusbar (callback: update statusbar)"))

    ((name . "status_background")
     (doc . "color which can be used to override Gtk theme."))

    ((name . "title_format_long")
     (doc . "titlebar string when no statusbar shown (will be expanded"))

    ((name . "title_format_short")
     (doc . "titlebar string when statusbar shown (will be expanded)"))

    ((name . "icon")
     (doc . "path to icon for Gtk"))

    ((name . "forward_keys")
     (doc . "whether uzbl-core should send key events to the webkit view")
     (default . "1"))

    ((name . "download_handler")
     (doc . "The command to call (usually a shell script) when the page requests a download"))

    ((name . "cookie_handler")
     (doc . "The command to call to handle cookies.")
     (default . (eval (format "talk_to_socket %s" ezbl-cookie-socket))))

    ((name . "new_window")
     (doc . "handler to execute to invoke new uzbl window (TODO better name)"))

    ((name . "scheme_handler")
     (doc . "handler to execute for each URI navigated
to - the navigation request will be ignored if handler prints USED."))

    ((name . "fifo_dir")
     (doc . "location to store fifo's"))

    ((name . "socket_dir")
     (doc . "location to store sockets"))

    ((name . "http_debug")
     (doc . "http debug mode (value 0-3)"))

    ((name . "shell_cmd")
     (doc . "alias which will be expanded to use shell commands (eg sh -c)"))

    ((name . "proxy_url")
     (doc . "http traffic socks proxy (eg: http://<host>:<port>)"))

    ((name . "max_conns")
     (doc . "max simultaneous connections (default: 100)"))

    ((name . "max_conns_host")
     (doc . "max simultaneous connections per hostname (default: 6)"))

    ((name . "view_source")
     (doc . "View the page source."))

    ((name . "useragent")
     (doc . "to be expanded string"))

    ((name . "zoom_level")
     (doc . "The level of zoom of the page."))

    ((name . "zoom_type")
     (doc . "The type of zoom."))

    ((name . "font_size")
     (doc . "The current font size."))

    ((name . "default_font_family")
     (doc . "sans-serif"))

    ((name . "monospace_font_family")
     (doc . "monospace (example Aerial Mono)"))

    ((name . "cursive_font_family")
     (doc . "sans"))

    ((name . "sans_serif_font_family")
     (doc . "sans (example DejaVu Sans)"))

    ((name . "serif_font_family")
     (doc . "serif (example DejaVu Serif)"))

    ((name . "fantasy_font_family")
     (doc . "Pterra"))

    ((name . "monospace_size")
     (doc . "Default monospace font size."))

    ((name . "minimum_font_size")
     (doc . "The minimum font size."))

    ((name . "disable_plugins")
     (doc . "(TODO rename to enable)"))

    ((name . "disable_scripts")
     (doc . "(TODO rename to enable)"))

    ((name . "autoload_images")
     (doc . "Images are automatically loaded."))

    ((name . "autoshrink_images")
     (doc . "shrink images to window size (default 0)"))

    ((name . "enable_spellcheck")
     (doc . "Spellcheck is enabled."))

    ((name . "enable_private")
     (doc . "Private mode is enabled."))

    ((name . "print_backgrounds")
     (doc . "print background images? (default 0)"))

    ((name . "stylesheet_uri")
     (doc . "use this to override the pagelayout with a custom stylesheet"))

    ((name . "resizable_text_areas")
     (doc . "Are text areas resizable?"))

    ((name . "default_encoding")
     (doc . "iso-8859-1 by default"))

    ((name . "enforce_96_dpi")
     (doc . "1 by default"))

    ((name . "caret_browsing")
     (doc . "Whether to use caret browsing.")))
  "The variables available from Uzbl.

This is a list of alists, where each alist has the attributes:

- name (mandatory)

  The name of the variable. Must exactly match the name used by
  Uzbl.

- doc (mandatory)

  A description of the variable. Usually taken straight from the
  Uzbl README.

- default (optional)

  An optional default value for the variable. Note that this is
  not the same as Uzbl's default values; values here will
  overwrite Uzbl defaults.

  Either a string to be sent to Uzbl or a list of the form (eval
  FORM), where FORM is a Lisp expression, evaluated after each
  Uzbl instance is started, which returns a string to use as the
  default value.")

(defvar ezbl-xwidget-id-counter 0
  "Keeps track of the current value to assign to newly-created xwidgets.

IDs must be unique, or Emacs will crash.")

(defconst ezbl-xwidget-type 3
  "The type attribute for xwidget embedded widgets.

For whatever reason, the Xwidget patch specifies that embedded X windows have
this type.")

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
`ezbl-command-exec'.

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
             (ezbl-command-exec inst (format ,output-format ,@args))))))

(defun ezbl-command-init ()
  "Create Emacs functions from `ezbl-commands' and `ezbl-instance-spec'.

Read through `ezbl-commands' and call `ezbl-command-make-func' on
each one. Also, run through `ezbl-instance-spec' and call
`ezbl-make-instance-accessor-func' on each one."
  (interactive)
  (append (mapcar 'ezbl-command-make-func ezbl-commands)))

(defun ezbl-variable-set-defaults (&optional inst)
  "Set the variable defaults according to `ezbl-variables'."
  (mapc '(lambda (spec)
           (let ((name (cdr (assq 'name spec)))
                 (default (cdr-safe (assq 'default spec))))
             (when default
               (if (and (consp default) (eq (car default) 'eval))
                   (setq default (eval (cadr default))))
               (ezbl-command-set inst name default))))
        ezbl-variables))

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
          ((ezbl-inst-p inst)
           inst)
          ((bufferp inst)
           (with-current-buffer inst
             ezbl-inst))
          ((integerp inst)
           (cdr-safe (assq inst
                           ezbl-inst-list)))
          ((processp inst)
           (cdr-safe (assq (process-id inst)
                           ezbl-inst-list)))
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
      (when strict
        (error (format "`%s' is not an Ezbl instance or resolvable to an Ezbl instance" inst))))))

(defun ezbl-inst-define-advice ()
  "Define and activate the advice for each slot in `ezbl-inst'.

Makes the accessors call `ezbl-inst-get' before operating, so
that the accessors work on things which are resolvable to an
`ezbl-inst', rather than only allowing the insts themselves.

Also redefines the `setf-method' for each slot, since `setf'
doesn't actually call the slot accessor, so it wouldn't resolve
its argument to an `ezbl-inst'."
  (mapc '(lambda (item)
           (let ((func (intern (concat "ezbl-inst-" (symbol-name item)))))
             (ad-add-advice func
                            ezbl-inst-get-first 'before 'first)
             (ad-activate func)

             ;; This is tricky. Redefines the `setf-method' to be the same as
             ;; the old one, except that it is passed the result of
             ;; `ezbl-inst-get' on the `eval'ed argument.
             (eval (list 'define-setf-method
                         func
                         '(cl-x)
                         `(apply ,(get func 'setf-method)
                                 (list (ezbl-inst-get (eval cl-x) t)))))))
        ezbl-inst-slots))

(defun ezbl-command-exec (inst command)
  "Sends the string COMMAND to the Uzbl instance INST.

If INST is a buffer, use the value of `ezbl-inst' in that
buffer. If COMMAND is a Uzbl command as described by the Uzbl
Readme (http://www.uzbl.org/readme.php).

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
  "Insert an xwidget into the current buffer.

Also, set a callback (using the `xwidget-event' \"keybinding\")
to launch Uzbl once the widget is fully initialized."
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
  (incf ezbl-xwidget-id-counter))

(defun ezbl-xwidget-handler ()
  "Respond to the creation of an xwidget.

Once the xwidget is set up, start a Uzbl process and give it the
xwidget's socket id."
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
  "Create a new Uzbl instance in a new buffer and browse to URI."
  (interactive "sUri: ")

  (ezbl-init)
  (switch-to-buffer (generate-new-buffer uri))
  ;; Currently has problems embedding into an empty buffer, so insert a space.
  (insert " ")
  (backward-char)
  (ezbl-embed)

  (add-hook 'ezbl-xembed-ready-hook
            `(lambda () (ezbl-command-uri ezbl-inst ,uri))
            nil t)
  (put 'ezbl-xembed-ready-hook 'permanent-local t)
  (current-buffer))

(defun ezbl-cookie-listener (proc answer)
  "Handle a cookie request over a socket.

This function is intended to be set as the filter of the
server-process listening on the cookie socket. It receives the
process PROC with which it is communicating and ANSWER, the text
sent by Uzbl."
  (let* ((args (split-string answer "\0"))
         (result (apply 'ezbl-cookie-handler args)))
    (when (and result
               (> 0 (length (split-string result))))
      (process-send-string proc result))
    (process-send-eof proc)))

(defun ezbl-cookie-handler (op scheme host path &optional data &rest ignored)
  "Process a single cookie.

OP - either \"PUT\" or \"GET\", indicating whether a cookie
     should be stored or retrieved, respectively.

SCHEME - The URL scheme of the request, usually \"http\".

HOST - The host requesting the cookie.

PATH - The path of the cookie.

DATA - In a \"PUT\" request, this is the data to store."
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
  (when (processp ezbl-cookie-process)
    (unless force
      (error "A cookie process already exists")))

  (let* ((sock-path (or path ezbl-cookie-socket)))
    (when (file-exists-p sock-path)
      (if force
          (delete-file sock-path)
        (error (format "Cannot listen on `%s', file exists" sock-path))))

    (setq ezbl-cookie-process
          (make-network-process :name "ezbl-cookie"
                                :type 'seqpacket
                                :server t
                                :service sock-path
                                :family 'local
                                :filter 'ezbl-cookie-listener))))

(defun ezbl-update-mode-line-format ()
  "Updates the mode-line format in each ezbl display-window,

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
  "Mode for interacting with Ezbl processes.

\\{ezbl-mode-map}"
  :group 'ezbl
  (toggle-read-only t)
  (set-buffer-modified-p nil)
  (add-hook 'window-configuration-change-hook 'ezbl-fill-window nil t))

(add-hook 'ezbl-mode-hook 'ezbl-variable-set-defaults)
(add-hook 'ezbl-mode-hook 'ezbl-fill-window)
(add-hook 'ezbl-mode-hook 'ezbl-update-mode-line-format)

(defun ezbl-xwidget-resize-at (pos width height)
  "Resize xwidget at postion POS to WIDTH and HEIGHT.

There is no corresponding resize-id fn yet, because of display
property/xwidget id impedance mismatch."
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
        ;; Turn read-only off while modifying the size of the xwidget, then
        ;; reactivate it.
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
      (if (string-match "^EVENT \\[\\([[:alnum:]]+\\)\\] \\([A-Z_]+\\) ?\\(.*\\)$" ans)
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

;; Should always remain at the end, just before "(provide 'ezbl)".
(ezbl-command-init)

(provide 'ezbl)

;;; ezbl.el ends here
