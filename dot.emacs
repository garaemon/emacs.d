;; -*- mode: emacs-lisp; -*-

;; written by R.Ueda, a.k.a. garaemon
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq emacs-submodules '("nyan-mode" "git-modes" "magit" "yatex"
                         "markdown-mode" "less-css-mode"
                         "milkode"
                         "gist" "gh" "powerline" "yasnippet"
                         "helm-c-yasnippet" "helm-swoop"
                         "php-mode" "puppet" "pcache" "logito"
                         "expand-region.el" "smartrep.el" "multiple-cursors.el"
                         "highlight-symbol.el" "solarized"
                         "flycheck" "s" "dash" "f" "undo-tree" "yaml-mode"
                         "helm" "helm-ag" "rainbow-delimiters" "trr" "anzu"
                         "yascroll" "indent-guide" "volatile-highlights"
                         "twittering-mode" "sublimity" "emoji-cheat-sheet"
                         "git-gutter" "auto-complete" "popup" "zenburn-emacs"
                         "graphviz-dot-mode" "dockerfile-mode"
                         "direx-el" "popwin-el" "foreign-regexp" "w3m"
                         "wakatime-mode" "wanderlust" "smart-cursor-color"
                         "dired-hacks" "dired-plus"
                         "helm-ls-git"
                         "gtags" "helm-gtags" "helm-replace-string"
                         "symon"
                         "ham-mode" "gmail-mode" "ace-isearch"
                         "ace-jump-mode" "clang-complete-async"
                         "swoop" "async" "pcre2el" "ht" "bm"
                         "elpa/packages/let-alist"
                         "judge-indent"
                         "scss-mode"))
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

;; (let* ((zshpath (shell-command-to-string
;;          "/usr/bin/env zsh -c 'printenv PATH'"))
;;        (pathlst (split-string zshpath ":")))
;;   (setq exec-path pathlst)
;;   (setq eshell-path-env zshpath)
;;   (setenv "PATH" zshpath)e
;;  )
(unless (server-running-p)
  (server-start))

(setq-default c-basic-offset 2)


(provide 'dot)
;;; dot.emacs ends here
