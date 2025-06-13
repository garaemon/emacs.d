;; test-syntax-error.el
(use-package good-package)

(use-package bad-package-form ; missing closing parenthesis
  :init
  (message "This package has an error in its definition")

(use-package another-good-one)

;; Example of an unterminated string
(setq my-string "this string is not closed...
