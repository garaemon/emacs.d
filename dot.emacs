;; -*- mode: emacs-lisp; -*-

;; written by R.Ueda

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq emacs-submodules '("nyan-mode" "git-modes" "magit" 
                         "elpa" "elpa/packages/cl-lib"))
(add-to-list 'load-path "~/.emacs.d/modules")
(dolist (module emacs-submodules)
  (add-to-list 'load-path (format "~/.emacs.d/modules/%s" module)))
;;(require 'git-commit)
(require 'magit)
(add-to-list 'load-path "/Users/garaemon/gprog/git-your-emacs/emacs.d")

(require 'garaemon-dot-emacs)
(server-start)

(prefer-coding-system 'utf-8)

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))

;; need to remove .emacs.keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(setenv "PATH" (format "%s:/Users/garaemon/.nvm/v0.6.16/bin/" (getenv "PATH")))
(setenv "PATH" (format "%s:/usr/local/Cellar/pypy/1.9/bin/" (getenv "PATH")))

;; for objective c

;; mode config

;; load
(load "~/gprog/hhhungry/objc/util/dot.emacs")

(add-hook 'objc-mode-hook
          (function (lambda ()
                      (setq c-basic-offset 2))))

(add-to-list 'load-path "~/gprog/git-your-emacs/elisps")

(require 'less-css-mode)
(require 'github-issues)
(setq mumamo-background-colors nil)


(require 'ucs-normalize) 
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(setq system-uses-terminfo nil)

(require 'python-mode)
(require 'gist)

(setenv "PATH" (format "%s:/usr/local/bin" (getenv "PATH")))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(load-library "php-mode")
(require 'php-mode)


(setq whitespace-style '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings '((space-mark ?\x3000 [?\□])
                                    (tab-mark   ?\t   [?\xBB ?\t])))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

(global-set-key "\C-h" 'backward-delete-char)
(setq mac-command-modifier 'meta)


(global-set-key (kbd "C-c h") 'helm-mini)

;; cmake
(require 'cmake-mode)
(setq auto-mode-alist (cons '("CMakeLists.txt" . cmake-mode) auto-mode-alist))


(setq-default tab-width 4)

(add-to-list 'load-path "/Users/garaemon/gprog/puppet-3.2.4/ext/emacs")
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;(add-to-list 'load-path "/Users/garaemon/gprog/nyan-mode")
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)

