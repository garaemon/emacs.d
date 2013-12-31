;; -*- mode: emacs-lisp; -*-

;; written by R.Ueda

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq emacs-submodules '("nyan-mode" "git-modes" "magit" 
                         "elpa" "elpa/packages/cl-lib"
                         "markdown-mode" "less-css-mode"
                         "gist" "gh"
                         "php-mode" "puppet"
                         "expand-region.el" "smartrep.el" "multiple-cursors.el"
                         "highlight-symbol.el"))
(add-to-list 'load-path "~/.emacs.d/modules")
(dolist (module emacs-submodules)
  (add-to-list 'load-path (format "~/.emacs.d/modules/%s" module)))


(require 'garaemon-dot-emacs)

;; (setenv "PATH" (format "%s:/usr/local/bin" (getenv "PATH")))
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")


;; minimum settings
(setq-default tab-width 2)
(global-set-key "\C-h" 'backward-delete-char)
(global-unset-key "\M-p")
(global-unset-key "\M-n")

(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(global-set-key "\M-p" 'scroll-up-in-place)
(global-set-key [M-up] 'scroll-up-in-place)
(global-set-key "\M-n" 'scroll-down-in-place)
(global-set-key [M-down] 'scroll-down-in-place)

(setq mac-command-modifier 'meta)

(let* ((zshpath (shell-command-to-string
         "/usr/bin/env zsh -c 'printenv PATH'"))
       (pathlst (split-string zshpath ":")))
  (setq exec-path pathlst)
  (setq eshell-path-env zshpath)
  (setenv "PATH" zshpath))
;;(setq inferior-js-program-command "~/.nvm/v0.10.3/bin/node")

(server-start)

(setq-default c-basic-offset 2)

(require 'edit-server)
(edit-server-start)
