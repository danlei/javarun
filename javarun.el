;;; javarun.el --- minor mode for quick development of Java programs

;; Copyright (C) 2010 Daniel H. Leidisch

;; Author: Daniel H. Leidisch <spam AT leidisch DOT net>
;; Keywords: languages

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


(defgroup javarun '()
  "Java compilation convenience for small command-line programs."
  :group 'tools
  :prefix "javarun")

(defcustom javarun-java-path ""
  "The path where java and javac can be found.

This variable only needs to be set, if the java and javac
programs are not on the system path."
  :type '(directory))

(defcustom javarun-javac-program "javac"
  "The program used to compile the Java program."
  :type '(file))

(defcustom javarun-java-program "java"
  "The program used to run the compiled Java program."
  :type '(file))

(defcustom javarun-cygdir "C:/cygwin/"
  "The Cygwin root directory path.

This variable is meant be used with Cygwin Emacs in combination
with non-Cygwin Java. It is only relevant, if `system-type' is
set to cygwin."
  :type '(directory))

(defvar javarun-old-window-configuration nil
  "The window configuration as it was before a javarun popup.")

(define-derived-mode javarun-popup-mode special-mode "Javarun Output")

(define-key javarun-popup-mode-map (kbd "q") 'javarun-bury-popup-buffer)

(define-minor-mode javarun-mode
    "Toggle Javarun mode.

With no argument, this command toggles the mode. A non-null
prefix argument turns on the mode; a null prefix argument turns
off the mode.

Javarun is intended to be a useful shortcut when developing small
command-line Java programs. This minor mode provides the command
`javarun', which will compile and run a Java file and show its
results in a popup buffer.

javarun.el is located at URL `http://github.com/danlei/javarun'.

Keybindings:
\\{javarun-mode}"
  :init-value nil
  :lighter " JRun"
  :keymap '(("\C-c\C-c" . javarun)))

(defun javarun-bury-popup-buffer (&optional buffer)
  "Bury buffer and restore old window configuration.

If no BUFFER is given, it defaults to the `current-buffer'."
  (interactive)
  (bury-buffer (or buffer (current-buffer)))
  (set-window-configuration javarun-old-window-configuration))

(defun javarun-popup-buffer (buffer)
  "Split window vertically and popup BUFFER in a new window.

The old window configuration is saved in the variable
`javarun-old-window-configuration'. The function
`javarun-bury-popup-buffer' closes the window, buries the
buffer, and restores the old window configuration afterwards."
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

The program's output (or the compiler error messages, if
compilation failed) are shown in a popup window by
`javarun-popup-buffer'.

If a positive prefix argument ARGSP is given, read a string of
command line arguments interactively using the function
`javarun-read-args'."
  (interactive "p")
  (if (not (javarun-compile (javarun-generate-buffer-file-name)))
      (javarun-popup-buffer "*javac-output*")
    (apply 'call-process
           (concat (file-name-as-directory javarun-java-path)
                   javarun-java-program)
           nil "*java-output*" t
           (file-name-nondirectory
            (file-name-sans-extension (buffer-file-name)))
           (when (/= argsp 1) (javarun-read-args)))
    (javarun-popup-buffer "*java-output*")))

(defun javarun-generate-buffer-file-name (&optional buffer)
  "Return buffer file name of current buffer or BUFFER.

Mangle the path for use under Cygwin. Throw an error, if BUFFER
has no associated file."
  (let* ((buffer-file (or (buffer-file-name (or buffer (current-buffer)))
                          (error "Buffer has no associated file."))))
    (if (eq system-type 'cygwin)
        (concat (file-name-as-directory javarun-cygdir)
                (substring buffer-file 1))
      buffer-file)))

(defun javarun-compile (java-file)
  "Compile JAVA-FILE if necessary.

Compile JAVA-FILE using `javarun-javac-program', unless there are
no changes since last compilation. Return t on success."
  (unless (file-exists-p java-file)
    (error "Java file not found."))
  (let ((class-file (concat (file-name-sans-extension java-file) ".class")))
    (if (and (file-exists-p class-file)
             (time-less-p (nth 5 (file-attributes java-file))
                          (nth 5 (file-attributes class-file))))
        t
      (zerop (call-process (concat (file-name-as-directory javarun-java-path)
                                   javarun-javac-program)
                           nil "*javac-output*" t java-file)))))

(provide 'javarun)

;;; javarun.el ends here
