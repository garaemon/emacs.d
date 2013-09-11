;; -*- mode: emacs-lisp; -*-

;; emacs-settings
;; (require 'cl)
;;  (defun update-emacs-settings-site-dir (dir)
;;    "add dir and subdirectories of it to load-path"
;;    (let ((dirs (remove-if-not #'file-directory-p
;;                               (directory-files dir t "^[^.]"))))
;;      (dolist (d dirs)
;;        (update-emacs-settings-site-dir d))
;;      (setq load-path (cons dir load-path))))
;;  (update-emacs-settings-site-dir "/Users/garaemon/gprog/emacs-settings/emacs.d")

;;  (load "/Users/garaemon/gprog/emacs-settings/init.el")
;;  (load-emacs-settings "/Users/garaemon/gprog/emacs-settings")
;;(zone nil)                              ;disable zone mode

;; anthy

(add-to-list 'load-path "/Users/garaemon/gprog/magit-1.2.0")
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

(setenv "PATH" (format "%s:/Users/garaemon/.nvm/v0.6.16/bin/"
                       (getenv "PATH")))
(setenv "PATH" (format "%s:/usr/local/Cellar/pypy/1.9/bin/"
                       (getenv "PATH")))

;; for objective c

;; mode config

;; load
(load "~/gprog/hhhungry/objc/util/dot.emacs")

(add-hook 'objc-mode-hook
          (function (lambda ()
                      (setq c-basic-offset 2))))

(add-to-list 'load-path "~/gprog/tumblr-mode")
 (require 'tumblr-mode)
(custom-set-variables '(tumblr-post-format "html"))

(setq tumblr-email "garaemon@gmail.com")
(setq tumblr-hostnames '("garaemon.tumblr.com"))
(setq tumblr-password nil)
(setq tumblr-api-key "bC5xWGEcIY4SSdzkjWln954bvu0MoEJAgSbgctmPlDVZ5VQJtr")
(setq tumblr-post-format "html")
;;(setq load-path (cons "~/gprog/markdown-mode" load-path))
;;(add-to-list 'load-path "~/gprog/markdown-mode")
;;(require 'markdown-mode)
;;(require 'markdown)
(load "~/gprog/markdown-mode/markdown-mode.el")
(setq auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist))
(setq markdown-command "~/gprog/gh-markdown-cli/bin/mdown")
(setq markdown-xhtml-header-content "
<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"http://github.github.com/github-flavored-markdown/shared/css/documentation.css\" />
<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"file:///users/garaemon/gprog/garaemon-memo/github.css\" />
")
;;(setq markdown-css-path "http://github.github.com/github-flavored-markdown/shared/css/documentation.css")
(add-to-list 'load-path "~/gprog/git-your-emacs/elisps")

;; (add-to-list 'load-path "~/gprog/swank-js")
;; (require 'slime)
;; (slime-setup '(slime-repl slime-js))

(require 'less-css-mode)
(require 'github-issues)
;;(load "~/gprog/git-your-emacs/emacs.d/autostart.el")
;;(require 'nxhtml-autoload)
(setq mumamo-background-colors nil)
;;(add-to-list 'auto-mode-alist '("\\.ejs$" . nxhtml-mumamo-mode))

;;(remove-hook 'js2-mode-hook 'my-js2-mode-hook)

;; term mode
(require 'term)

(require 'multi-term)

(setq multi-term-program shell-file-name)
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-map "\C-y" 'term-paste)
             (define-key term-raw-map "\C-q" 'move-beginning-of-line)
             (define-key term-raw-map "\C-f" 'forward-char)
             (define-key term-raw-map "\C-b" 'backward-char)
             (define-key term-raw-map "\C-t" 'set-mark-command)
             (define-key term-raw-map (kbd "ESC") 'term-send-raw)
             (define-key term-raw-map [delete] 'term-send-raw)
             (define-key term-raw-map "\C-z"
               (lookup-key (current-global-map) "\C-z"))))
(global-set-key (kbd "C-c c") 'multi-term)
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

(defun shell () (interactive)           ;override!
  (multi-term))

(require 'ucs-normalize) 
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(setq system-uses-terminfo nil)

;; (global-set-key "\C-t" '(lambda ()(interactive)(term "/bin/zsh")))
;; (defvar ansi-term-after-hook nil)
;; (add-hook 'ansi-term-after-hook
;;           (function
;;            (lambda ()
;;              (define-key term-raw-map
;;                "\C-t" '(lambda ()(interactive)(term "/bin/zsh"))))))
;; (defadvice ansi-term (after ansi-term-after-advice (arg))
;;   "run hook as after advice"
;;   (run-hooks 'ansi-term-after-hook))
;; (ad-activate 'ansi-term)
;; (load-library "~/gprog/emacs-settings-repo/shell-toggle-patched.el")
;; (autoload 'shell-toggle "shell-toggle"
;;   "Toggles between the *shell* buffer and whatever buffer you are editing."
;;   t)
;; (autoload 'shell-toggle-cd "shell-toggle"
;;   "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
;; (global-set-key "\C-ct" 'shell-toggle)
;; (global-set-key "\C-cd" 'shell-toggle-cd)

;; (add-hook 'term-mode-hook
;;           '(lambda ()
;;              (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
;;              (define-key term-raw-map (kbd "C-y") 'term-paste)
;;              ))

;; (set-language-environment "Japanese")
;; (setq file-name-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8-unix)
;; (set-default-coding-systems 'utf-8-unix)
;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-strict-missing-semi-warning nil)
;;(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(load "~/python-mode.el")

(require 'gist)

(setenv "PATH" (format "%s:/usr/local/bin"
                       (getenv "PATH")))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(load-library "php-mode")
(require 'php-mode)


(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

(global-set-key "\C-h" 'backward-delete-char)
(setq mac-command-modifier 'meta)


(global-set-key (kbd "C-c h") 'helm-mini)

(add-to-list 'load-path "/Users/garaemon/.emacs.d/")
(require 'cmake-mode)
(setq auto-mode-alist (cons '("CMakeLists.txt" . cmake-mode) auto-mode-alist))


(setq-default tab-width 4)

(add-to-list 'load-path "/Users/garaemon/gprog/puppet-3.2.4/ext/emacs")
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(add-to-list 'load-path "/Users/garaemon/gprog/nyan-mode")
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)

