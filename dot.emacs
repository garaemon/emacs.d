;; -*- mode: emacs-lisp; -*-

;; written by R.Ueda, a.k.a. garaemon
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq emacs-submodules '("nyan-mode" "git-modes" "magit"
                         "markdown-mode" "less-css-mode"
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
                         "gtags" "helm-gtags" "helm-replace-string"))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-ask-to-save-history (quote always))
 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-use-in-dired t nil (em-ls))
 '(eshell-modules-list (quote (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prefer-o-shell t nil (eshell))
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(eshell-visual-commands (quote ("vi" "top" "screen" "less" "lynx" "ssh" "rlogin" "telnet")))
 '(wakatime-cli-path "~/gprog/wakatime/wakatime-cli.py")
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))
