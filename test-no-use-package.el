;; test-no-use-package.el
(message "This is a test file.")
(defun my-function ()
  (interactive)
  (message "Hello!"))
(setq my-variable 42)
;; No use-package forms here
