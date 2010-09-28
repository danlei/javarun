;;; javarun.el --- minor mode for quick development of Java programs

;; Copyright (C) 2010 Daniel H. Leidisch

;; Author: Daniel H. Leidisch <spam AT leidisch DOT net>
;; Keywords: tools, languages, java

;; This file is NOT part of GNU Emacs.

;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; See <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode allows for quick compilation and running of Java
;; programs, making the edit-compile-run cycle a little less annoying.

;;; Code:


(defvar javarun-java-path ""
  "The path where java and javac can be found.

This variable only needs to be set, if the java and javac
commands are not on the system path.")

(defvar javarun-javac-command "javac"
  "The command used to compile the Java program.")

(defvar javarun-java-command "java"
  "The command used to run the compiled Java program.")

(defvar javarun-cygdir ""
  "The cygwin root directory path.

This variable is meant be used with Cygwin Emacs in combination
with non-Cygwin Java. It is only relevant, if `system-type' is
set to cygwin.")

(defvar javarun-old-window-configuration nil
  "The window configuration as it was before a javarun popup.")

(define-derived-mode javarun-popup-mode special-mode "Javarun Output")

(define-key javarun-popup-mode-map (kbd "q") 'javarun-kill-popup-buffer)
(define-key javarun-popup-mode-map (kbd "Q") 'javarun-burry-popup-buffer)

(define-minor-mode javarun-mode
    "Toggle Javarun mode.

With no argument, this command toggles the mode. A non-null
prefix argument turns on the mode; a null prefix argument turns
off the mode.

Javarun is intended to be a useful shortcut when developing small
command-line Java programs. This minor mode provides the command
\\[javarun], which will compile and run a Java file and show its
results in a popup buffer."
  :init-value nil
  :lighter " JRun"
  :keymap '(("\C-c\C-c" . javarun)))

(defun javarun-kill-popup-buffer (&optional buffer)
  "Kill BUFFER and restore old window configuration.

If no BUFFER is given, defaults to the `current-buffer'."
  (interactive)
  (kill-buffer (or buffer (current-buffer)))
  (set-window-configuration javarun-old-window-configuration))

(defun javarun-burry-popup-buffer (&optional buffer)
  "Burry buffer and restore old window configuration.

If no BUFFER is given, defaults to the `current-buffer'."
  (interactive)
  (bury-buffer (or buffer (current-buffer)))
  (set-window-configuration javarun-old-window-configuration))

(defun javarun-popup-buffer (buffer)
  "Splits window vertically and popups BUFFER in a new window.

The old window configuration is saved in
`javarun-old-window-configuration'. `javarun-burry-popup-buffer'
closes the window and buries the buffer;
`javarun-kill-popup-buffer' closes the popup window and kills the
buffer. Both restore the old window configuration afterwards."
  (setq javarun-old-window-configuration (current-window-configuration))
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer buffer)
  (javarun-popup-mode))

(defun javarun-read-args ()
  "Read command line arguments interactively.

All non-string arguments are evaluated."
  (mapcar (lambda (x)
            (if (stringp x)
                x
                (prin1-to-string (eval x))))
          (car (read-from-string
                (concat "(" (read-string "Command line arguments: ") ")")))))

(defun javarun (argsp)
  "Compile, and (if successful) run a Java program.

The program's output (or the compiler error messages, if the
program didn't compile) are shown in a popup window by
`javarun-popup-buffer'.

If a positive prefix arg ARGSP is given, reads a string of command line
arguments interactively via `javarun-read-args'."
  (interactive "p")
  (if (/= 0 (call-process (concat (file-name-as-directory javarun-java-path)
                                  javarun-javac-command)
                          nil "*javac-output*" t
                          (if (eq system-type 'cygwin)
                              (concat (file-name-as-directory javarun-cygdir)
                                      (substring (buffer-file-name) 1))
                              (buffer-file-name))))
      (javarun-popup-buffer "*javac-output*")
      (progn
        (apply 'call-process
               javarun-java-command nil "*java-output*" t
               (file-name-nondirectory
                (file-name-sans-extension (buffer-file-name)))
               (when (/= argsp 1) (javarun-read-args))))
      (javarun-popup-buffer "*java-output*")))

(provide 'javarun)

;;; javarun.el ends here
