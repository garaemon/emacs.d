;; -*- mode: emacs-lisp -*-

;; shell env hook for mac os x

(require 'garaemon-util)
(setq gc-cons-threshold 134217728)

;; Load elscreen first.
;; Because elscreen collides with color theme
;; setgins.
(require 'elscreen)
(setq elscreen-prefix-key (kbd "C-t"))
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)


(defvar my/color-theme nil
  "color theme to use")


;; (setq file-name-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)

;; anthy
(when (or (eq system-type 'cygwin)
      (eq system-type 'gnu/linux))
  (setq load-path (append '("/usr/share/emacs/site-lisp/anthy/")
              load-path))
  (load-library "anthy")
  (global-unset-key "\C-\\")
  (setq default-input-method "japanese-anthy")
  (global-set-key "\C-\\" 'anthy-mode))

(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-h" 'help-for-help)

(when-carbon
 (require 'carbon-font)
 (setq mac-allow-anti-aliasing t)
 (fixed-width-set-fontset "hiramaru" 12))

(setq auto-mode-alist (cons (cons "\\.cg?$" 'c-mode) auto-mode-alist))

(when-darwin
 ;; フォントフェースの設定
 ;; see http://d.hatena.ne.jp/kazu-yamamoto/20090122/1232589385
 (set-face-attribute 'default nil
             :family "monaco"
             :height 120)
 ;; 日本語フォント: ヒラギノ丸ゴシック
 (set-fontset-font
  nil 'japanese-jisx0208
  ;; (font-spec :family "Hiragino Mincho Pro")) ;; font
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
 )

(when-darwin
 (setq ns-command-modifier (quote meta))
 (setq ns-alternate-modifier (quote super)))

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :height 120) ;font size
  (set-frame-font "Ricty Diminished-12")
  ;;(set-frame-font "Ricty Diminished Discord-12")
  )



(require 'column-marker)
;; fci mode conflicts against company mode
;; (require 'fill-column-indicator)
;; (setq-default fci-rule-column 100)
(dolist (mode '(c-mode-hook
                c++-mode-hook
                sh-mode-hook
                markdown-mode-hook
                python-mode-hook
                lisp-mode-hook euslisp-mode-hook
                cmake-mode-hook
                javascript-mode-hook js-mode-hook
                emacs-lisp-mode-hook))
  (add-hook mode (lambda ()
                   (column-marker-1 100))))
;;   (add-hook mode (lambda () (interactive)
;;                    (fci-mode))))

(when-meadow
 (setq inhibit-default-init t))

(setq auto-mode-alist (cons (cons "\\.cu?$" 'c-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.h?$" 'c++-mode) auto-mode-alist))


(when (eq window-system 'windows-nt)
  (require 'cygwin-mount-mw32)
  (cygwin-mount-activate))

(global-set-key "\C-o" 'dabbrev-expand)


(setq mac-pass-control-to-system nil)

(line-number-mode 1)
;;(column-number-mode 1)

(display-time)

(setq-default transient-mark-mode t)

(require 'euslisp-mode)
(setq auto-mode-alist (cons (cons "\\.l$" 'euslisp-mode) auto-mode-alist))

(defun lisp-other-window ()
  "Run lisp on other window"
  (interactive)
  (if (not (string= (buffer-name) "*inferior-lisp*"))
      (switch-to-buffer-other-window
       (get-buffer-create "*inferior-lisp*")))
  (run-lisp inferior-euslisp-program))

;; (shell-quote-argument "rosrun roseus roseus")
(set-variable 'inferior-euslisp-program "~/.emacs.d/roseus.sh")

(global-set-key "\C-cE" 'lisp-other-window)

;;(autoload 'js-mode "js")
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)
      (save-excursion
        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ js-indent-level 2))))
        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))
      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))
(defun my-js2-mode-hook ()
  (require 'js)
  (setq js-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
;  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)]
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (rainbow-delimiters-mode)
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(require 'js2-mode)
(setq auto-mode-alist (cons (cons "\\.js$" 'js2-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.jsx$" 'js2-jsx-mode) auto-mode-alist))
(define-key global-map [165] nil)
(define-key global-map [67109029] nil)
(define-key global-map [134217893] nil)
(define-key global-map [201326757] nil)
(define-key function-key-map [165] [?\\])
(define-key function-key-map [67109029] [?\C-\\])
(define-key function-key-map [134217893] [?\M-\\])
(define-key function-key-map [201326757] [?\C-\M-\\])

(autoload 'goby "goby" nil t)

(global-set-key "\M-g" 'goto-line)

(cond ((carbon-emacs-p)
       (setq default-frame-alist
         (append (list '(alpha . (90 90))) default-frame-alist)))
      ((meadowp)
       (modify-all-frames-parameters
        (list (cons 'alpha  '(nil nil nil nil)))))
      )

(setq auto-mode-alist
      (append auto-mode-alist
          '(("\\.[hg]s$"  . haskell-mode)
        ("\\.hi$"     . haskell-mode)
        ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(setq haskell-literate-default 'latex)
(setq haskell-doc-idle-delay 0)

(global-set-key "\C-c(" 'hs-hide-block)
(global-set-key "\C-c)" 'hs-show-block)
(global-set-key "\C-c{" 'hs-hide-all)
(global-set-key "\C-c}" 'hs-show-all)

(setq auto-mode-alist (cons (cons "\\.html$" 'html-mode)
                            auto-mode-alist))

(global-set-key "\C-x\C-b" 'ibuffer)

(setq completion-ignore-case t)

(global-set-key "\C-cg" 'imenu)

(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

(setq c-default-style "stroustrup")

;; (eval-after-load "ispell"
;;   '(progn
;;      (setq ispell-skip-region-alist
;;            (cons '("[^¥000-¥377]")
;;                  ispell-skip-region-alist))
;;      (setq ispell-extra-args '("-C"))
;;      ))
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;(iswitchb-mode 1)

(global-unset-key "\C-\\")
(when (carbon-emacs-p)
 (mac-input-method-mode 1))


(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")

(font-lock-add-keywords 'lisp-mode
            (list
             ;; *hoge*に色を付ける
             (list "\\(\\*\\w\+\\*\\)\\>"
                   '(1 font-lock-constant-face nil t))
             ;; +hoge+に色を付ける
             (list "\\(\\+\\w\+\\+\\)\\>"
                   '(1 font-lock-constant-face nil t))
             ;; <hoge>に色を付ける
             (list "\\(<\\w\+>\\)\\>"
                   '(1 font-lock-constant-face nil t))
             ;; defclass*に色を付ける
             (list "\\(defclass\\*\\)"
                   '(1 font-lock-keyword-face nil t))
            ))

(defun cl-indent (sym indent)
  (put sym 'common-lisp-indent-function
       (if (symbolp indent)
       (get indent 'common-lisp-indent-function)
     indent)))

(cl-indent 'iterate 'let)
(cl-indent 'collect 'progn)
(cl-indent 'mapping 'let)
(cl-indent 'mapping 'let)
(cl-indent 'define-test 'let)

;; magit!
(add-to-list 'exec-path "/opt/local/bin")
(require 'magit)
(global-set-key "\C-cl" 'magit-status)
(global-set-key "\C-cL" 'magit-status)


(require 'markdown-mode)

(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
(define-key markdown-mode-map (kbd "M-p") nil)
(define-key markdown-mode-map (kbd "M-n") nil)

(when-meadow
 (let ((make-spec
    (function
     (lambda (size charset fontname &optional windows-charset)
       (setq size (- size))
       (if (not windows-charset)
           (setq windows-charset
             (cadr (assq charset
                 mw32-charset-windows-font-info-alist))))
       `(((:char-spec ,charset :height any)
          strict
          (w32-logfont ,fontname 0 ,size 400 0
               nil nil nil ,windows-charset 1 3 0))
         ((:char-spec ,charset :height any :weight bold)
          strict
          (w32-logfont ,fontname 0 ,size 700 0
               nil nil nil ,windows-charset 1 3 0)
          ((spacing . -1)))
         ((:char-spec ,charset :height any :slant italic)
          strict
          (w32-logfont ,fontname 0 ,size 400 0
               t nil nil ,windows-charset 1 3 0))
         ((:char-spec ,charset :height any :weight bold :slant italic)
          strict
          (w32-logfont ,fontname 0 ,size 700 0
               t nil nil ,windows-charset 1 3 0)
          ((spacing . -1)))))))
       (make-spec-list
    (function
     (lambda (size params-list)
           (list
        (cons 'spec
          (apply 'append
             (mapcar (lambda (params)
                   (apply make-spec (cons size params)))
                 params-list)))))))
       (define-fontset
     (function
      (lambda (fontname size fontset-list)
        (let ((spec (funcall make-spec-list size fontset-list)))
          (if (w32-list-fonts fontname)
          (w32-change-font fontname spec)
        (w32-add-font fontname spec))))))
       (arisaka-fontset-list
    '((ascii "monaco")
      (katakana-jisx0201 "ARISAKA-等幅")
      (japanese-jisx0208 "ARISAKA-等幅")))
       )
   (funcall define-fontset "Arisaka 10" 10 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 12" 12 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 14" 14 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 16" 16 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 18" 18 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 20" 20 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 22" 22 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 24" 24 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 36" 36 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 48" 48 arisaka-fontset-list)
   )
 ;; 初期フレームの設定
 (setq default-frame-alist
       (append (list '(font . "Arisaka 12")
             '(ime-font . (w32-logfont "Arisaka"
                           0 16 400 0 nil nil nil
                           128 1 3 49))) ; TrueType のみ
           default-frame-alist))
 )

(when-meadow
 (setq w32-hide-mouse-on-key t))

;; sudo apt-get install cmigemo is required
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  )

(setq visible-bell nil)
 (setq ring-bell-function 'ignore)

(setq make-backup-files nil)

(unless-cygwin
 (scroll-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq scroll-conservatively 1)

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|kml\\|gpx\\)\\'" . html-mode)
            auto-mode-alist))
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
(global-set-key [M-down] 'scroll-down-in-place);

;; python-mode, pycomplete
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)

;;(require 'ipython)

(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "gosh")
(require 'cmuscheme)
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)
(setq gosh-program-name "/usr/bin/env gosh -i")
(setq scheme-program-name "gosh -i")

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(put 'if 'scheme-indent-function 2)
(put 'dotimes 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'instance 'scheme-indent-function 1)
(put 'set! 'scheme-indent-function 1)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'defun 'scheme-indent-function 2)
(put 'defclass 'scheme-indent-function 2)
(put 'defmethod 'scheme-indent-function 2)
(put 'define-method* 'scheme-indent-function 2)
(put 'define-class* 'scheme-indent-function 2)
(put 'define-function* 'scheme-indent-function 1)
(put 'let-keywords 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-optionals 'scheme-indent-function 2)
(put 'let-values 'scheme-indent-function 2)
(put 'receive 'scheme-indent-function 1)
(put 'mutex-block 'scheme-indent-function 2)
(put 'unless 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'defmethod 'scheme-indent-function 1)

;;font-lock
(font-lock-add-keywords 'scheme-mode
                        (list
                         (list (concat
                                "(" (regexp-opt '("use") t) "\\>") '(1 font-lock-keyword-face nil t))
                         (list "\\(self\\)\\>" '(1 font-lock-constant-face nil t))
                         (list "\\(\\*\\w\+\\*\\)\\>" '(1 font-lock-constant-face nil t))
                         (list "\\(#\\(\\+\\|\\-\\)\.\*\\)" '(1 font-lock-variable-name-face))
                         (cons "\\(dotimes\\|unless\\|when\\|dolist\\|while\\)\\>" 1)
                         (cons "\\(let-\\(keywords\\|optionals\\|values\\|keywords\\*\\|optionals\\*\\|values\\*\\)\\)\\>" 1)
                         (list "\\(warn\\)\\>" '(1 font-lock-warning-face))
                         (list "\\(#t\\|#f\\)\\>" '(1 font-lock-constant-face))
                         (cons "\\(defclass\\|defmethod\\)\\>" 1)
             (cons "\\(define-\\(function\\*\\|class\\*\\|method\\*\\)\\)\\>" 1)
                         )
                        )

(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point
  "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)


(show-paren-mode t)
(setq show-paren-style 'mixed)

(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(require 'tramp)
(setq tramp-verbose 10)
(setq tramp-default-method "sshx")
;;(setq tramp-default-method "scpx")
;;(setq tramp-shell-prompt-pattern "^.*[#$%>] *")

;; (setq truncate-lines nil)
;; (setq truncate-partial-width-windows nil)


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(fset 'yes-or-no-p 'y-or-n-p)

;; for emacs24 x mac
(setq mac-command-modifier 'meta)

(require 'magit)

;;(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.launch$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cfg$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.test$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(defvar ruby-indent-level 2)
(defvar nxml-child-indent 2)

;; (require 'keyfreq)
;; (keyfreq-mode 1)
;; (keyfreq-autosave-mode 1)

(require 'less-css-mode)

(setq mumamo-background-colors nil)


(require 'ucs-normalize)
(setq system-uses-terminfo nil)

(require 'php-mode)

;; mark zenkaku-whitespaces and tabs
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

;; cmake
(require 'cmake-mode)
(setq auto-mode-alist (cons '("CMakeLists.txt" . cmake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cmake$" . cmake-mode) auto-mode-alist))

(setq-default tab-width 8)
(setq-default c-basic-offset 4)
(c-set-offset 'substatement-open 0)
;; (require 'judge-indent)
;; (global-judge-indent-mode t)
;; (setq judge-indent-major-modes '(c-mode python-mode sh-mode c++-mode lisp-mode euslisp-mode html-mode))


;; nyan-mode
;; (require 'nyan-mode)
;; (nyan-mode -1)
;; nyan mode animation is heavy
;; (nyan-start-animation)

;; objective-c
;; Path to iOS SDK
(defvar xcode:sdk "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))

(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(require 'expand-region)
(when (<= emacs-major-version 24)
  (defmacro save-mark-and-excursion (&rest body)
    `(save-excursion ,@body)))

(require 'multiple-cursors)
(require 'smartrep)

(global-set-key (kbd "C-^") 'er/expand-region)
(global-set-key (kbd "C-M-^") 'er/contract-region)

(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(global-set-key (kbd "C-M-j") 'mc/edit-lines)

;; (require 'highlight-symbol)
;; (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

;; (global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
;; (global-set-key (kbd "<f4>") 'highlight-symbol-remove-all)
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
;; (require 'auto-highlight-symbol-config)

(require 'git-gutter-fringe+)
(global-git-gutter+-mode)
;; (global-linum-mode)
;;(git-gutter:linum-setup)
(require 'nlinum)
(global-nlinum-mode)

;; defining keymap with C-q prefix
(declare-function smartrep-define-key "smartrep")
(global-unset-key "\C-q")
(smartrep-define-key global-map "C-q"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

(setq compilation-scroll-output t)

(require 'recentf-ext)

;; helm
(when (>= emacs-major-version 24)
  (require 'helm)
  (require 'helm-config)
  (require 'helm-swoop)
  (require 'helm-gtags)
  (setq helm-gtags-auto-update t)

  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (custom-set-variables
   '(helm-gtags-prefix-key "C-t")
   '(helm-gtags-suggested-key-mapping t)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-auto-update nil)
   )
  (eval-after-load "helm-gtags"
    '(progn
       (define-key helm-gtags-mode-map (kbd "C-:") 'helm-gtags-find-pattern)
       ;; (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
       ;; (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
       ;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
       ;; (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
       ;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
       ;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
       ;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))
       ))

  (helm-autoresize-mode 1)
  (helm-mode t)
  (require 'helm-ag)
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-thing-at-point 'symbol)
  (define-key global-map (kbd "C-x g") 'helm-ag)
  (define-key global-map (kbd "C-]") 'helm-ag)
  ;; does not activate helm for find-file
  ;; For find-file etc.
  ;;(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; For helm-find-files etc.
  ;;(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; (require 'helm-regexp)
  (custom-set-variables
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-ls-git
                                 ;; helm-c-source-replace-string
                                 helm-source-files-in-current-dir
                                 helm-source-recentf
                                 helm-source-grep-ag
                                 helm-source-rospack-list
                                 helm-source-buffer-not-found)))
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(find-ros-file . nil))
  (define-key global-map (kbd "M-x")     'helm-M-x)
  ;;(define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-mini)

  ;; fix ctrl-h in helm
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  ;;(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents
    (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (require 'helm-ls-git)

  (require 'helm-ros)

  (setq helm-source-catkin-root "~/ros_catkin_ws/hydro/src")

  (global-set-key (kbd "M-i") 'helm-swoop)

  ;; (require 'swoop)
  ;; (require 'ace-isearch)
  ;; (global-ace-isearch-mode +1)
  ;; (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  ;; (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  ;; (define-key helm-swoop-map (kbd "C-n") 'helm-exit-minibuffer)
  ;; (define-key helm-swoop-map (kbd "C-p") 'helm-exit-minibuffer)
  )
;; defining keymap with C-q prefix
(declare-function smartrep-define-key "smartrep")
(global-unset-key "\C-q")
(smartrep-define-key global-map "C-q"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

(setq my/color-theme 'solarized-dark)

(when (>= emacs-major-version 24)
  (load-theme my/color-theme t)
  )


(require 'rainbow-delimiters)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'js2-mode-hook #'rainbow-delimiters-mode)

;; undo tree
;; C-x u
(require 'undo-tree)
(global-undo-tree-mode)
(define-key undo-tree-visualizer-mode-map
  "\C-m" 'undo-tree-visualizer-quit)

(require 'undohist)
(undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("/tmp/"))
;;; NTEmacsだと動かないらしいので再定義
;;; http://d.hatena.ne.jp/Lian/20120420/1334856445
(when (eq system-type 'windows-nt)
  (defun make-undohist-file-name (file)
    (setq file (convert-standard-filename (expand-file-name file)))
    (if (eq (aref file 1) ?:)
        (setq file (concat "/"
                           "drive_"
                           (char-to-string (downcase (aref file 0)))
                           (if (eq (aref file 2) ?/)
                               ""
                             (if (eq (aref file 2) ?\\)
                                 ""
                               "/"))
                           (substring file 2))))
    (setq file (expand-file-name
                (subst-char-in-string
                 ?/ ?!
                 (subst-char-in-string
                  ?\\ ?!
                  (replace-regexp-in-string "!" "!!"  file)))
                undohist-directory))))


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\|rosinstall\\)$" . yaml-mode))

;; powerline
(when (>= emacs-major-version 24)
  (require 'powerline)
  (powerline-default-theme)
  (defun arrow-right-xpm (color1 color2)
    "Return an XPM right arrow string representing."
    (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

  (defun arrow-left-xpm (color1 color2)
    "Return an XPM right arrow string representing."
    (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))

  (defconst color1 "#FF6699")
  (defconst color3 "#CDC0B0")
  (defconst color2 "#FF0066")
  (defconst color4 "#CDC0B0")

  (defvar arrow-right-1
    (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
  (defvar arrow-right-2
    (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
  (defvar arrow-left-1
    (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
  (defvar arrow-left-2
    (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

  (setq-default mode-line-format
                (list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
                                       (propertize " " 'display arrow-right-1)))
                       '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
                                       (propertize " " 'display arrow-right-2)))
                       ;; Justify right by filling with spaces to right fringe - 16
                       ;; (16 should be computed rahter than hardcoded)
                       '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

                       '(:eval (concat (propertize " " 'display arrow-left-2)
                                       (propertize " %p " 'face 'mode-line-color-2)))
                       '(:eval (concat (propertize " " 'display arrow-left-1)
                                       (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
                       ))
  (make-face 'mode-line-color-1)
  (set-face-attribute 'mode-line-color-1 nil
                      :foreground "#fff"
                      :background color1)

  ;; (make-face 'mode-line-color-2)
  ;; (set-face-attribute 'mode-line-color-2 nil
  ;;                     :foreground "#fff"
  ;;                     :background color2)

  (set-face-attribute 'mode-line nil
                      :foreground "#fff"
                      :background color3
                      :box nil)
  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :foreground "#fff"
  ;;                     :background color4)

  (set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#FF0066"
                    :box nil)

  (set-face-attribute 'powerline-active1 nil
                      :foreground "#fff"
                      :background "#FF6699"
                      :inherit 'mode-line)

  (set-face-attribute 'powerline-active2 nil
                      :foreground "#000"
                      :background "#ffaeb9"
                      :inherit 'mode-line)
  )

(require 'trr)

(require 'volatile-highlights)
(volatile-highlights-mode)

(require 'anzu)
(global-anzu-mode +1)
(setq anzu-search-threshold 1000)
;; (setq anzu-minimum-input-length 3)

;; (global-set-key (kbd "M-%") 'anzu-query-replace)
;; (global-set-key (kbd "M-&") 'anzu-query-replace) ;for mistype

;; 除外したい拡張子
(setq delete-trailing-whitespace-exclude-patterns
      (list "\\.cpp$" "\\.h$" "\\.hpp$" "\\.l" "\\.py" "\\.launch" "\\.test"
            "\\.cmake" "\\.xml"))

(require 'cl)

(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond ((equal nil (loop for pattern in delete-trailing-whitespace-exclude-patterns
                          thereis (string-match pattern buffer-file-name)))
         (delete-trailing-whitespace))))

;; (defun delete-trailing-whitespace-with-exclude-pattern () (interactive))

(add-hook 'before-save-hook 'delete-trailing-whitespace-with-exclude-pattern)

(require 'emoji-cheat-sheet)

;; Force to load yasnippet/yasnippet.el in order to avoid
;; to use yasnippet.el under elpa packages.
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/el-get/yasnippet/snippets"))
(yas-global-mode)
;;(custom-set-variables '(yas-trigger-key "TAB"))

;; insert new snippet
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; create a new snippet
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; edit a snippet
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
;; (setq yas-buffer-local-condition
;;       '(or (not (or (string= "font-lock-comment-face"
;;                              (get-char-property (point) 'face))
;;                     (string= "font-lock-string-face"
;;                              (get-char-property (point) 'face))))
;;            '(require-snippet-condition . force-in-comment)))
;; bind M-- to list snippets
(when (require 'helm-c-yasnippet nil t)
  (setq helm-c-yas-space-match-any-greedy t) ;[default: nil]
  (global-set-key (kbd "M--") 'helm-c-yas-complete)
  )

(windmove-default-keybindings 'meta)

(require 'multi-term)

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ\n" nil t)))

(require 'graphviz-dot-mode)
(defun graphviz-compile-preview ()
  (interactive)
  (compile compile-command)
  (sleep-for 1)
  (graphviz-dot-preview))
(global-set-key [f5] 'graphviz-compile-preview)

;; direx
(require 'direx)
(require 'direx-project)
;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
(setq display-buffer-function 'popwin:display-buffer)

(require 'popwin)
(push '(direx:direx-mode :position left :width 50 :dedicated t)
      popwin:special-display-config)

;; re-redifine function in order to support .repo
(defun direx-project:vc-repo-p (dirname)
  (cl-loop for vc-dir in '(".repo")
           thereis (file-exists-p (expand-file-name vc-dir dirname))))

(defun direx-project:vc-root-p (dirname)
   (cl-loop for vc-dir in '(".git" ".hg" ".bzr")
            thereis (file-exists-p (expand-file-name vc-dir dirname))))

(defun direx-project:project-root-p (dirname)
  (cl-some (lambda (fun) (funcall fun dirname))
           direx-project:project-root-predicate-functions))

(defun direx-project:project-repo-root-p (dirname)
  (cl-some (lambda (fun) (funcall fun dirname))
           '(direx-project:vc-repo-p)))

(defun direx-project:find-project-root-noselect (filename)
  (interactive)
  (or (cl-loop for parent-dirname in (if (file-directory-p filename)
                                     (cons filename
                                           (direx:directory-parents filename))
                                   (direx:directory-parents filename))
           if (direx-project:project-repo-root-p parent-dirname)
           return (direx:find-directory-noselect parent-dirname))
      (cl-loop for parent-dirname in (if (file-directory-p filename)
                                         (cons filename
                                               (direx:directory-parents filename))
                                       (direx:directory-parents filename))
               if (direx-project:project-root-p parent-dirname)
               return (direx:find-directory-noselect parent-dirname))))

(global-set-key (kbd "C-x C-j") 'direx-project:jump-to-project-root-other-window)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'smart-cursor-color)
(smart-cursor-color-mode +1)

(require 'dired-subtree)

(require 'dired+)

(require 'hl-line)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))

;;(global-hl-line-mode -1)

(defun un-camelcase-word-at-point ()
  "un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.

The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore.
"
  (interactive)
  (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning)(region-end))
       (downcase-region (region-beginning)(region-end))))
(global-set-key "\M-\C-C"  'un-camelcase-word-at-point)

(defalias 'qrr 'query-replace-regexp)
(when nil
  (require 'symon)
  (custom-set-variables
   '(symon-sparkline-type symon-sparkline-type-gridded)
   '(symon-delay 5))
  (symon-mode)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(yas-trigger-key "Enter"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))

;; for mistype :)
(global-set-key "\M-%" 'query-replace)

(require 'bm)
(global-set-key [?\C-\M-\ ] 'bm-toggle)
(global-set-key [?\C-\M-n] 'bm-next)
(global-set-key [?\C-\M-p] 'bm-previous)
;; (setq-default bm-buffer-persistence nil)
;; (setq bm-restore-repository-on-load t)
;; (add-hook 'find-file-hook 'bm-buffer-restore)
;; (add-hook 'kill-buffer-hook 'bm-buffer-save)
;; (add-hook 'after-save-hook 'bm-buffer-save)
;; (add-hook 'after-revert-hook 'bm-buffer-restore)
;; (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
;; (add-hook 'kill-emacs-hook '(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))
(set-face-background bm-face "orange")
(set-face-foreground bm-face "black")


(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(when (require 'rosemacs nil t)
  (invoke-rosemacs)
  (global-set-key "\C-x\C-r" ros-keymap)
  )

(require 'gist)
;; gist-buffer to save buffer to gist

(defun klf-region-run ()
  "Run KLatexFormula"
  (interactive)
  (start-process "klatexformula" "*KLatexFormula*" "klatexformula" "--daemonize" "-I" "--latexinput" (buffer-substring (region-beginning) (region-end)))
)
(global-set-key [(control c) (control k)] 'klf-region-run)

(defun open-with-shiba ()
  "open a current markdown file with shiba"
  (interactive)
  (start-process "shiba" "*shiba*" "shiba" "--detach" buffer-file-name))
(define-key markdown-mode-map (kbd "C-c C-c") 'open-with-shiba)
(define-key markdown-mode-map (kbd "C-c m") 'newline)
;; For emacs 24
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))

(require 'milkode)
;; Path-to-gmilk
;; (setq gmilk-command "~/.rvm/gems/ruby-2.2.0/bin/gmilk")
(global-set-key (kbd "C-c C-s") 'milkode:search-from-all-packages)
(global-set-key (kbd "C-c C-m") 'milkode:search-from-all-packages)

(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(global-set-key "\C-x@" '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(global-set-key "\C-x#" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))
(require 'thingopt)
(define-thing-commands)
(global-set-key "\M-@" 'mark-word*)
(require 'coffee-mode)
(add-hook 'coffee-mode-hook
          '(lambda() (set (make-local-variable 'tab-width) 2)
             (setq coffee-tab-width 2)))
(setq enable-flycheck t)
(when enable-flycheck
  (require 'flycheck)
  (require 'flycheck-pos-tip)
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  (setq
   flycheck-googlelint-filter "-runtime/references,-readability/braces"
   flycheck-googlelint-verbose "3"
   )


  ;;(add-hook 'after-init-hook 'global-flycheck-mode)
  (eval-after-load 'flycheck
    '(progn
       (require 'flycheck-google-cpplint)
       (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint))))


  ;; Enable flycheck-googlelint in C++-mode only.
  ;; (add-hook 'c++-mode-hook (lambda() (flycheck-select-checker 'c/c++-cppcheck)))
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (defconst flycheck-hooks-alist
    '(;; Handle events that may start automatic syntax checks
      (after-save-hook        . flycheck-handle-save)
      ;; (after-change-functions . flycheck-handle-change)
      ;; Handle events that may triggered pending deferred checks
      ;; (window-configuration-change-hook . flycheck-perform-deferred-syntax-check)
      (post-command-hook                . flycheck-perform-deferred-syntax-check)
      ;; Teardown Flycheck whenever the buffer state is about to get lost, to
      ;; clean up temporary files and directories.
      (kill-buffer-hook       . flycheck-teardown)
      (change-major-mode-hook . flycheck-teardown)
      (before-revert-hook     . flycheck-teardown)
      ;; Update the error list if necessary
      (post-command-hook . flycheck-error-list-update-source)
      (post-command-hook . flycheck-error-list-highlight-errors)
      ;; Display errors.  Show errors at point after commands (like movements) and
      ;; when Emacs gets focus.  Cancel the display timer when Emacs looses focus
      ;; (as there's no need to display errors if the user can't see them), and
      ;; hide the error buffer (for large error messages) if necessary.  Note that
      ;; the focus hooks only work on Emacs 24.4 and upwards, but since undefined
      ;; hooks are perfectly ok we don't need a version guard here.  They'll just
      ;; not work silently.
      (post-command-hook . flycheck-display-error-at-point-soon)
      (focus-in-hook     . flycheck-display-error-at-point-soon)
      (focus-out-hook    . flycheck-cancel-error-display-error-at-point-timer)
      (post-command-hook . flycheck-hide-error-buffer)
      ;; Immediately show error popups when navigating to an error
      (next-error-hook . flycheck-display-error-at-point))
    "Hooks which Flycheck needs to hook in.
  The `car' of each pair is a hook variable, the `cdr' a function
  to be added or removed from the hook variable if Flycheck mode is
  enabled and disabled respectively.")

  ;; redefine with idle-timer
  (defun flycheck-display-error-at-point-soon ()
    "Display the first error message at point in minibuffer delayed."
    (flycheck-cancel-error-display-error-at-point-timer)
    (when (flycheck-overlays-at (point))
      (setq flycheck-display-error-at-point-timer
            (run-with-idle-timer flycheck-display-errors-delay nil 'flycheck-display-error-at-point))))

  (global-flycheck-mode t)
  )

(require 'typescript)
(setq auto-mode-alist (cons (cons "\\.ts?$" 'typescript-mode) auto-mode-alist))

(defun calc-eval-region (arg beg end)
  "Calculate the region and display the result in the echo area.
With prefix ARG non-nil, insert the result at the end of region."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (insert (format "\n%s" result))))
(global-set-key "\C-cc" 'calc-eval-region)

(defun replace-punctuation (a1 a2 b1 b2)
  "Replace periods and commas"
  (let ((s1 (if mark-active "選択領域" "バッファ全体"))
        (s2 (concat a2 b2))
        (b (if mark-active (region-beginning) (point-min)))
        (e (if mark-active (region-end) (point-max))))
    (if (y-or-n-p (concat s1 "の句読点を「" s2 "」にしますがよろしいですか?"))
        (progn
          (replace-string a1 a2 nil b e)
          (replace-string b1 b2 nil b e)))))

(defun commaperiod ()
  "選択領域またはバッファ全体の句読点を「，．」にします"
  (interactive)
  (replace-punctuation "、" ", " "。" ". "))


;;バッファ全体の句読点と読点をコンマとピリオドに変換
(defun replace-commaperiod-buffer ()
  (interactive "r")
  (save-excursion
    (replace-string "、" ", " nil (point-min) (point-max))
    (replace-string "。" ". " nil (point-min) (point-max))))

;;選択範囲内の全角英数字を半角英数字に変換
(defun hankaku-eisuu-region (start end)
  (interactive "r")
  (while (string-match
          "[０-９Ａ-Ｚａ-ｚ]+"
          (buffer-substring start end))
    (save-excursion
      (japanese-hankaku-region
       (+ start (match-beginning 0))
       (+ start (match-end 0))
       ))))

;;バッファ全体の全角英数字を半角英数字に変換
(defun hankaku-eisuu-buffer ()
  (interactive)
  (hankaku-eisuu-region (point-min) (point-max)))

(defun replace-commaperiod-before-save-if-needed ()
  (when (memq major-mode
              '(latex-mode))
    (replace-commaperiod-buffer)(hankaku-eisuu-buffer)))

;;保存前フックに追加
(add-hook 'before-save-hook 'replace-commaperiod-before-save-if-needed)
;; (require 'google-this)
;; (global-set-key (kbd "C-x g") 'google-this-mode-submap)

(require 'lua-mode)

;; Colorize Protobuf
(require 'protobuf-mode)
(defconst my-protobuf-style '((c-basic-offset . 4) (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook (lambda () (c-add-style "my-style" my-protobuf-style t)))
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
;; (add-to-list 'auto-mode-alist '("\\.pb.txt$" . protobuf-mode))

(require 'google-c-style)

;; customize google-c-style
(setf (cdr (assoc 'c-basic-offset google-c-style)) 4)
(setf (cdr (assoc 'c-offsets-alist google-c-style))
      '((arglist-intro . ++)
        (func-decl-cont . ++)
        (member-init-intro . ++)
        (inher-intro . ++)
        (comment-intro . 0)
        (arglist-close . c-lineup-arglist)
        (topmost-intro . 0)
        (block-open . 0)
        (inline-open . 0)
        (substatement-open . 0)
        (statement-cont
         .
         (,(when (fboundp 'c-no-indent-after-java-annotations)
             'c-no-indent-after-java-annotations)
          ,(when (fboundp 'c-lineup-assignments)
             'c-lineup-assignments)
          ++))
        (label . /)
        (case-label . +)
        (statement-case-open . +)
        (statement-case-intro . +) ; case w/o {
        ;;(access-label . /)
        (innamespace . 0)))

;; Quick & Dirty C++11 support
(defun c++-mode-hook-c++11 ()
  (font-lock-add-keywords
   nil '(;; complete some fundamental keywords
  ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
  ;; namespace names and tags - these are rendered as constants by cc-mode
  ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
  ;;  new C++11 keywords
  ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
  ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
  ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
  ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
  ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
  ;; hexadecimal numbers
  ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
  ;; integer/float/scientific numbers
  ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
  ;; c++11 string literals
  ;;       L"wide string"
  ;;       L"wide string with UNICODE codepoint: \u2018"
  ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
  ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
  ;;       R"(user-defined literal)"
  ;;       R"( a "quot'd" string )"
  ;;       R"delimiter(The String Data" )delimiter"
  ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
  ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
  (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
  (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

  ;; user-defined types (rather project-specific)
  ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
  ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
  ))
  )

(add-hook 'c++-mode-hook
          (lambda()
            (set-fill-column 100)
            ;;(column-marker-1 100)
            (c++-mode-hook-c++11)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)
            (google-set-c-style)
            (google-make-newline-indent)
            (setq c-basic-offset 4)
            ))

(setq js-indent-level 4)
(add-hook 'js-mode (lambda () (setq js-indent-level 4) (setq c-basic-offset 4)))

;; gtags
;; update gtags after save it
(require 'gtags)
(defun update-gtags (&optional prefix)
  (interactive "P")
  (let ((rootdir (gtags-get-rootpath))
        (args (if prefix "-v" "-iv")))
    (when rootdir
      (let* ((default-directory rootdir)
             (buffer (get-buffer-create "*update GTAGS*")))
        (save-excursion
          (set-buffer buffer)
          (erase-buffer)
          (let ((result (process-file "gtags" nil buffer nil args)))
            (if (= 0 result)
                (message "GTAGS successfully updated.")
              (message "update GTAGS error with exit status %d" result))))))))
;; it may be slow...
;;(add-hook 'after-save-hook 'update-gtags)

;;(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-pattern)
(global-set-key (kbd "M-,") 'helm-gtags-find-pattern)
(global-set-key (kbd "M-.") 'helm-gtags-find-tag-other-window)
;; (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;; (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;; (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))
(setq vc-handled-backends nil)

;; (require 'auto-complete-clang-async)
;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/modules/clang-auto-complete/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

;; (require 'gtags)
;; (global-set-key "\M-." 'gtags-find-tag)
;; (global-set-key "\M-r" 'gtags-find-rtag)
;; (global-set-key "\M-s" 'gtags-find-symbol)
;; (global-set-key "\M-," 'gtags-pop-stack)

(require 'clang-format-diff)
(global-set-key "\M-[" 'clang-format-diff-view)
(custom-set-variables '(ediff-split-window-function 'split-window-horizontally))

(require 'win-switch)
;; simple functions to change background color of selected buffer

(custom-set-variables
 '(win-switch-feedback-background-color "yellow")
 '(win-switch-feedback-foreground-color "black")
 '(win-switch-idle-time 1.5)
 '(win-switch-window-threshold 1)
 )

;; switching window
(win-switch-set-keys '("k") 'up)
(win-switch-set-keys '("j") 'down)
(win-switch-set-keys '("h") 'left)
(win-switch-set-keys '("l") 'right)
(win-switch-set-keys '("o") 'next-window)
(win-switch-set-keys '("p") 'previous-window)
;; resizing
(win-switch-set-keys '("K") 'enlarge-vertically)
(win-switch-set-keys '("J") 'shrink-vertically)
(win-switch-set-keys '("H") 'shrink-horizontally)
(win-switch-set-keys '("L") 'enlarge-horizontally)
;; split
(win-switch-set-keys '("3") 'split-horizontally)
(win-switch-set-keys '("2") 'split-vertically)
(win-switch-set-keys '("0") 'delete-window)
(win-switch-set-keys '(" ") 'other-frame)
(win-switch-set-keys '("u" [return]) 'exit)
(win-switch-set-keys '("\M-\C-g") 'emergency-exit)
;; replace C-x o
(global-set-key (kbd "C-x o") 'win-switch-dispatch)
;;(global-set-key (kbd "C-x p") (win-switch-dispatch-with 'win-switch-previous-window))

(require 'company)
(global-company-mode)
(setq company-idle-delay 0.1) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-h") 'backward-delete-char)
(define-key company-active-map (kbd "C-h") 'backward-delete-char)
(push (apply-partially #'cl-remove-if
                      (lambda (c)
                        (or (string-match-p "[^\x00-\x7F]+" c)
                            (string-match-p "[0-9]+" c)
                            (if (equal major-mode "org")
                                (>= (length c) 15)))))
             company-transformers)

(require 'irony)
;; run "M-x irony-install-server" the first time
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(setq irony-lang-compile-option-alist
      '((c++-mode . ("c++" "-std=c++11" "-lstdc++" "-lm"))
        (c-mode . ("c"))
        (objc-mode . '("objective-c"))))
(defun irony--lang-compile-option ()
  (irony--awhen (cdr-safe (assq major-mode irony-lang-compile-option-alist))
    (append '("-x") it)))
(add-to-list 'company-backends 'company-irony)

(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 300)
        (let ((split-num (/ (window-body-width) 100)))
          (split-window-horizontally-n split-num))
      (split-window-horizontally)))
  (win-switch-dispatch))
(global-set-key "\M-o" 'other-window-or-split)

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key "\M-r" 'revert-buffer-no-confirm)

;; for tmux integration
(defun open-current-file-in-tmux ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (let ((target-dir (if (file-directory-p file-path)
                          file-path
                          (file-name-directory file-path))))
      (message (format "Opening directory %s in tmux" target-dir))
      (call-process-shell-command
       "tmux" nil "*tmux-output*"
       nil
       (format "new-window -a -t $(tmux ls -F \"#S\") -c %s"
               target-dir))
    )))

(global-set-key "\M-t" 'open-current-file-in-tmux)

(require 'backup-each-save)
;;; バックアップ先
(setq backup-each-save-mirror-location "~/.emacs.d/backups")
;;; バックアップファイルにつけるsuffix
(setq backup-each-save-time-format "%y%m%d_%H%M%S")
;;; バックアップするファイルサイズの上限
(setq backup-each-save-size-limit 5000000)
;;; バックアップ作成するファイルを判定
;; (defun backup-each-save-backup-p (filename)
;;   (string-match
;;    ;; ファイル名に/sync/が含まれるときのみバックアップする
;;    (rx (or "/sync/"))
;;    (file-truename filename)))
;; (setq backup-each-save-filter-function 'backup-each-save-backup-p)
;;; すべてのファイルをバックアップする
(setq backup-each-save-filter-function 'identity)
;;; 有効化！
(add-hook 'after-save-hook 'backup-each-save)

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced-include-only-checkout-path t)
(auto-save-buffers-enhanced nil)

(require 'helm-etags+)
(global-set-key "\M-." 'helm-etags+-select)

(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(defun decrement-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
          (error "No number at point"))
      (replace-match (number-to-string (1- (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c C-+") 'increment-number-at-point)
(global-set-key (kbd "C-c C-;") 'increment-number-at-point)
(global-set-key (kbd "C-c C--") 'decrement-number-at-point)

(require 'rust-mode)

(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

(custom-set-variables
 '(sort-fold-case t t)
)

;; 作業時間終了後に開くファイル。デフォルトでは "~/.emacs.d/pomodoro.org"
(setq pomodoro:file "~/mywork.md")

;; 作業時間関連
(setq pomodoro:work-time 25
      pomodoro:rest-time 5
      pomodoro:long-rest-time 30)

;; hook関数関連
(require 'notifications) ;; Linuxで DBUSがある環境のみ
(defun* my/pomodoro-notification (&key (title "Pomodoro")
                                       body
                                       (urgency 'critical))
  (notifications-notify :title title :body body :urgency urgency))

;; 作業終了後の hook
(add-hook 'pomodoro:finish-work-hook
          (lambda ()
            (my/pomodoro-notification :body "Work is Finish")))

;; 休憩終了後の hook
(add-hook 'pomodoro:finish-rest-hook
          (lambda ()
            (my/pomodoro-notification :body "Break time is finished")))

;; 長期休憩後の hook
(add-hook 'pomodoro:long-rest-hook
          (lambda ()
            (my/pomodoro-notification :body "Long Break time now")))

(require 'calfw)

(add-to-list 'auto-mode-alist '("\\.subr$" . shell-script-mode))

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
     "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
     "* %?\nEntered on %U\n  %i\n  %a")))

(require 'ob-ipython)

;; コードを評価するとき尋ねない
(setq org-confirm-babel-evaluate nil)
;; ソースコードを書き出すコマンド

(defun random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun org-babel-tangle-and-execute ()
  (interactive)
  (org-babel-tangle)
  (org-babel-execute-buffer)
  (org-display-inline-images))

(defun org-ipython-insert-initial-setting ()
  (interactive)
  (insert "#+BEGIN_SRC ipython :session\n")
  (insert "%matplotlib inline\n")
  (insert "#+END_SRC\n")
  )

(defun org-ipython-insert-matplotlib-block ()
  (interactive)
  (let ((random-png-file (format "/tmp/%s%s%s%s%s.png"
                                 (random-alnum)
                                 (random-alnum)
                                 (random-alnum)
                                 (random-alnum)
                                 (random-alnum))))
    (if (not (file-exists-p random-png-file))
        (progn
          (insert (format "#+BEGIN_SRC ipython :session :file %s :exports both\n"
                          random-png-file))
          (insert "#+END_SRC\n")))
    ))

(define-key org-mode-map (kbd "C-c C-i") 'org-ipython-insert-matplotlib-block)
(define-key org-mode-map (kbd "C-x C-e") 'org-babel-tangle-and-execute)

;; sample to insert figure
;; #+BEGIN_SRC ipython :session :file /tmp/image.png :exports both
;; %matplotlib inline
;; import matplotlib.pyplot as plt
;; import numpy as np
;; fig, ax = plt.subplots(facecolor='white')
;; ax.hist(np.random.randn(20000), bins=200)
;; #+END_SRC

(provide 'garaemon-dot-emacs)
