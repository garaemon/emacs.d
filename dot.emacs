;;; dot.emacs --- entrypoint of setting emacs

;;; This elisp provides minimum settings for coding and load
;;; other settings.

;;; Commentary:

;;; Code:
;;; ln -sf ~/.emacs.d/dot.emacs ~/.emacs

;; Load path from shellenv.el
(let ((shellenv-file (expand-file-name "~/.emacs.d/shellenv.el")))
  (if (file-exists-p shellenv-file)
      (load-file shellenv-file)))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; minimum settings
(setq-default tab-width 4)
;; disable hard-tab
(setq-default indent-tabs-mode nil)
;; use C-h as backspace
(global-set-key "\C-h" 'backward-delete-char)

;; Use M-p and M-n to scoll in place
(global-unset-key "\M-p")
(global-unset-key "\M-n")

(defun scroll-up-in-place (n)
  "Scroll up the current buffer with keeping cursor position.

- N the number of the lines to scroll up"
  (interactive "p")
  (forward-line n)
  (scroll-down n))

(defun scroll-down-in-place (n)
  "Scroll down the current buffer with keeping cursor position.

- N the number of the lines to scroll down"
  (interactive "p")
  (forward-line n)
  (scroll-up n))

(global-set-key "\M-p" 'scroll-up-in-place)
(global-set-key [M-up] 'scroll-up-in-place)
(global-set-key "\M-n" 'scroll-down-in-place)
(global-set-key [M-down] 'scroll-down-in-place)


(global-set-key "\C-o" 'dabbrev-expand)
(setq mac-command-modifier 'meta)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless
    (require 'el-get nil 'noerror)
  (with-current-buffer (url-retrieve-synchronously
                        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar el-get-recipe-path)
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes/")
(el-get-bundle sync)
(when (executable-find "pdftex")
  (el-get-bundle auctex)) ;; it depends on tex
(el-get-bundle ace-isearch)
(el-get-bundle ace-jump-mode)
(el-get-bundle anzu)
(el-get-bundle async)
(el-get-bundle auto-highlight-symbol)
(el-get-bundle auto-save-buffers-enhanced)
(el-get-bundle backup-each-save)
(el-get-bundle bm)
(el-get-bundle calfw)
(el-get-bundle code-format)
(el-get-bundle coffee-mode)
(el-get-bundle column-marker)
(el-get-bundle company)
(el-get-bundle company-jedi)
(el-get-bundle company-irony)
(el-get-bundle company-tern)
(el-get-bundle dash)
(el-get-bundle deferred)
(el-get-bundle dired-hacks)
(el-get-bundle dired-plus)
(el-get-bundle direx)
(el-get-bundle dockerfile-mode)
(el-get-bundle elisp-format
  :url "http://www.emacswiki.org/emacs/download/elisp-format.el")
(el-get-bundle emoji-cheat-sheet)
(el-get-bundle expand-region)
(el-get-bundle f)
(el-get-bundle fill-column-indicator)
(el-get-bundle flycheck)
(el-get-bundle flycheck-google-cpplint)
(if (not (eq system-type 'darwin))
    (el-get-bundle flycheck-pos-tip))
(el-get-bundle gh)
(el-get-bundle gist)
(el-get-bundle git-gutter+)
(el-get-bundle git-gutter-fringe+)
(el-get-bundle google-c-style)
(el-get-bundle graphviz-dot-mode)
(el-get-bundle gtags)
(el-get-bundle ham-mode)
(el-get-bundle helm)
(el-get-bundle helm-ag)
(el-get-bundle helm-c-yasnippet)
(el-get-bundle helm-etags-plus)
(el-get-bundle helm-google)
(el-get-bundle helm-gtags)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-swoop)
(el-get-bundle highlight-symbol)
(el-get-bundle hyde)
(el-get-bundle jedi-core-git)
(el-get-bundle js2-mode)
(el-get-bundle json-mode)
(el-get-bundle judge-indent)
(el-get-bundle less-css-mode)
(el-get-bundle let-alist)
(el-get-bundle linkd)
(el-get-bundle logito)
(el-get-bundle lua-mode)
(el-get-bundle magit)
(el-get-bundle magit-gh-pulls)
(el-get-bundle markdown-mode)
(el-get-bundle migemo)
(el-get-bundle milkode)
(el-get-bundle multiple-cursors)
(el-get-bundle nlinum)
(el-get-bundle ob-ipython)
(el-get-bundle org-mode)
(el-get-bundle pcache)
(el-get-bundle per-buffer-theme)
(el-get-bundle php-mode)
(el-get-bundle pomodoro)
(el-get-bundle popup)
(el-get-bundle popwin)
(el-get-bundle powerline)
(el-get-bundle protobuf-mode)
(el-get-bundle puppet-mode)
(el-get-bundle rainbow-delimiters)
(el-get-bundle recentf-ext)
(el-get-bundle rust-mode)
(el-get-bundle s)
(el-get-bundle save-visited-files)
(el-get-bundle scss-mode)
(el-get-bundle smart-cursor-color)
(el-get-bundle smartrep)
(el-get-bundle solarized-emacs)
(el-get-bundle string-inflection)
(el-get-bundle symon)
(el-get-bundle tabbar)
(el-get-bundle thingopt)
(el-get-bundle trr)
(el-get-bundle twittering-mode)
(el-get-bundle undo-tree)
(el-get-bundle undohist)
(el-get-bundle volatile-highlights)
(el-get-bundle web-mode)
(el-get-bundle win-switch-git)
(el-get-bundle yaml-mode)
(el-get-bundle yasnippet)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(require 'garaemon-dot-emacs)

(unless (server-running-p)
  (server-start))

(provide 'dot)

;; load machine local setup
(if (file-exists-p "~/.emacs.d/dot.emacs.local.el")
    (load "~/.emacs.d/dot.emacs.local.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
                              "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
                              default)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(helm-gtags-auto-update nil)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-prefix-key "C-t")
 '(helm-gtags-suggested-key-mapping t)
 '(helm-mini-default-sources (quote (helm-source-buffers-list helm-source-ls-git
                                                              helm-source-files-in-current-dir
                                                              helm-source-recentf
                                                              helm-source-rospack-list
                                                              helm-source-buffer-not-found)))
 '(package-selected-packages (quote ("paradox" paradox nlinum company nil lua-mode el-get)))
 '(sort-fold-case t t)
 '(win-switch-feedback-background-color "blue")
 '(win-switch-feedback-foreground-color "white")
 '(win-switch-idle-time 1.5)
 '(win-switch-window-threshold 1)
 '(yas-trigger-key "Enter" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t
                                     (:foreground "#7f8c8d")))))
