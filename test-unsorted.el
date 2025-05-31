;; test-unsorted.el
(use-package beta)
(use-package alpha) ; Error: alpha should come before beta
(use-package gamma)
