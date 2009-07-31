;;; ezbl.el --- Emacs interface for Uzbl (uzbl.org)
;;
;; Author: Daniel Hackney
;; Copyright (C) 2009 Daniel Hackney

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

(defgroup ezbl nil "Settings for Ezbl, the Emacs frontend for Uzbl.")

(defcustom ezbl-exec-path "/usr/bin/uzbl"
  "The location of the Uzbl executable."
  :group 'ezbl
  :type 'file)

(defvar ezbl-processes nil "A list of Uzbl processes")

(defvar ezbl-commands
  '((name . "set")
    (format . "set <key> = <value>")
    (doc . "* used for changing variables on the fly

* the changes are effective immediately; for example, setting the
  variable uri will make uzbl start loading, and changing
  status_format will make the status bar react immediately

* if you want to unset a string, use `set' with one space as the
  value."))
  "A list of commands which Uzbl accepts. These are used to
generate the functions to call each command.

The following attributes can be used in each alist:

- name (mandatory)

  The name of the command. This is the string that is passed to
  Uzbl when invoking the command.

- format (mandatory)

  The format of the command, for example

    scroll_vert <amount>

  All variables must be enclosed in angle brackets.")

(defun ezbl-get-command-args (command)
  "Extracts the arguments (as symbols) from a Uzbl command specification.

For example, the spec

  scroll_vert <amount>

Would return (amount)."
  (let ((start 0)
        (args nil))
    (while (string-match "<\\([[:alnum:]_-]+\\)>" command start)
      (setq args (append (list (make-symbol (match-string 1 command))) args))
      (setq start (match-end 1)))
    args))

(defun ezbl-make-command-func (spec)
  "Creates a function which produces the Uzbl command string described by SPEC.

The function created takes a number of arguments specified by the
`format' attribute of SPEC and returns a string suitable for
`ezbl-exec-command'.

See `ezbl-commands' for a description of the format of SPEC."
  (let* ((name (cdr (assq 'name spec)))
         (format (cdr (assq 'format spec)))
         (args (ezbl-get-command-args format))
         (doc (cdr (assq 'doc spec)))
         (output-format (replace-regexp-in-string "<[[:alnum:]_-]+>" "%s" format)))
  `(defun ,(make-symbol (concat "ezbl-command-" name)) ,args
     ,doc
     (format ,output-format ,@args))))

(defun ezbl-start (suffix &rest args)
  "Start an instance of Uzbl. ARGS is a keyword list of
options and values to pass to the Uzbl instance.

SUFFIX is the string suffix of the process and buffer names. If
SUFFIX is '1234', then the process 'uzbl-1234' and buffer
'*uzbl-1234*' would be created.

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

Returns an ezbl instance alist of the form:

  ((arguments . (\"--option1\" \"value\" \"--switch\"))
   (process . #<process ezbl-suffix>)
   (suffix . SUFFIX)
   (buffer . \"ezbl-suffix*\")
   (proc-name . \"ezbl-suffix\"))

This 'ezbl instance' is used in various other functions.
"
  (let ((program-args nil)
        (instance `((suffix . ,suffix)
                    (buffer . ,(concat "*ezbl-" suffix "*"))
                    (proc-name . ,(concat "ezbl-" suffix)))))
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
    (let* ((proc-name (cdr (assq 'proc-name instance)))
           (buffer-name (cdr (assq 'buffer instance)))
           (proc (apply 'start-process
                        (append (list proc-name
                                      buffer-name
                                      ezbl-exec-path)
                                program-args))))
      (setq instance (append `((arguments . ,program-args)
                               (process . ,proc))
                             instance)))))

(defun ezbl-exec-command (instance command)
  "Sends the string COMMAND to the Uzbl instance INSTANCE.

COMMAND is a Uzbl command as described by the Uzbl
readme (http://www.uzbl.org/readme.php).

See `ezbl-start' for a description of the format of INSTANCE."
  ;; Append a newline (\n) to the end of COMMAND if one is not already there.
  (when (not (string= "\n" (substring command -1)))
    (setq command (concat command "\n")))
  (process-send-string (cdr (assq 'process instance)) command))

(defun ezbl-uri-set (instance uri)
  "Set the uri of the Uzbl INSTANCE to URI.

See `ezbl-start' for a description of the format of INSTANCE."
  (ezbl-command instance (concat "uri " uri)))

;;; ezbl.el ends here
