#!/usr/bin/env emacs --script

;;; check-package-order.el --- Check if use-package declarations are sorted alphabetically

(defun get-package-name-from-form (form)
  "Extract package name from a (use-package <name> ...) form.
Returns the package name as a string, or nil if not a valid use-package form."
  (when (and (listp form)
             (>= (length form) 2)
             (eq (car form) 'use-package)
             (symbolp (cadr form)))
    (symbol-name (cadr form))))

(defun check-package-order (filename)
  "Check if (use-package ...) forms in FILENAME are alphabetically sorted.
Prints a message and exits with 0 if sorted, or 1 if not sorted or an error occurs."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let ((package-names '())
              (current-form nil)
              (parse-error nil))
          (while (not (or (eobp) parse-error))
            (condition-case read-err
                (setq current-form (read (current-buffer)))
              ('error
               (message "Error parsing Elisp syntax: %s" (error-message-string read-err))
               (setq parse-error t)))
            (when current-form
              (let ((pkg-name (get-package-name-from-form current-form)))
                (when pkg-name
                  (push pkg-name package-names)))))
          (when parse-error
            (exit 1))

          (setq package-names (nreverse package-names))

          (if (or (null package-names) (< (length package-names) 2))
              (progn
                (message "Package declarations are sorted.")
                (exit 0))
            (let ((prev-pkg (pop package-names))
                  (current-pkg nil)
                  (sorted t))
              (while (and package-names sorted)
                (setq current-pkg (pop package-names))
                (if (string-lessp (downcase current-pkg) (downcase prev-pkg))
                    (progn
                      (message "Package '%s' should come before '%s'." current-pkg prev-pkg)
                      (setq sorted nil))
                  (setq prev-pkg current-pkg)))
              (if sorted
                  (progn
                    (message "Package declarations are sorted.")
                    (exit 0))
                (exit 1))))))
    ('file-error
     (message "Error opening file: %s" (error-message-string err))
     (exit 1))
    ('error ; Catch any other unexpected errors
     (message "An unexpected error occurred: %s" (error-message-string err))
     (exit 1))))

;; Entry point for command-line execution
(if (null command-line-args-left)
    (progn
      (message "Usage: %s <filename>"
               (if command-line-args-raw
                   (car command-line-args-raw)
                 "check-package-order.el"))
      (exit 2)) ; Using 2 for usage errors
  (check-package-order (car command-line-args-left)))
