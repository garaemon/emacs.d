;; -*- mode: emacs-lisp -*-

(require 'garaemon-util)
(setq gc-cons-threshold 134217728)

(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; anthy
(when (or (eq system-type 'cygwin)
      (eq system-type 'gnu/linux))
  (setq load-path (append '("/usr/share/emacs/site-lisp/anthy/")
              load-path))
  (load-library "anthy")
  (global-unset-key "\C-\\")
  (setq default-input-method "japanese-anthy")
  (global-set-key "\C-\\" 'anthy-mode))

;; ;; anthying-cheat-sheat-popup
;; (require 'anything-cheat-sheat-popup)

;; ;; (defanything-cheat-sheat emacs-lisp                ;name, must be unique
;; ;;   (emacs-lisp-mode lisp-interaction-mode)          ;major mode list
;; ;;   "~/org/cheat-sheat/emacs-lisp.org")              ;cheat file name

;; ;; (defanything-cheat-sheat slime
;; ;;   (slime-repl-mode)
;; ;;   "~/org/cheat-sheat/slime.org")

;; ;; (defanything-cheat-sheat org
;; ;;   (org-mode)
;; ;;   "~/org/cheat-sheat/org.org")

;; ;;;; bind to M-h
;; ;;(global-set-key "\M-h" 'anything-cheat-sheat)


(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

;; (require 'auto-complete-clang-async)

;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/modules/clang-complete-async/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )

(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (setq ac-auto-start 2)
  (setq ac-dwim t)
  (set-default 'ac-sources '(ac-source-yasnippet
                             ac-source-abbrev
                             ac-source-words-in-buffer))
  (setq ac-modes
    (append ac-modes
        '(emacs-lisp-mode
          lisp-mode
          euslisp-mode
          html-mode
          cmake-mode
          xhtml-mode
          ;;org-mode
          )))
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (add-hook 'emacs-lisp-mode-hook
        (lambda ()
          (setq ac-sources '(ac-source-yasnippet
                 ac-source-abbrev
                 ac-source-words-in-buffer
                 ac-source-symbols))))
  )

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-h" 'help-for-help)

;; (require 'bm)
;; (global-set-key [?\C-;] 'bm-previous)
;; (global-set-key [?\C-:] 'bm-next)
;; (global-set-key [?\C-\M- ] 'bm-toggle)



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
(dolist (mode '(c-mode-hook
                c++-mode-hook
                sh-mode-hook
                markdown-mode-hook
                python-mode-hook
                lisp-mode-hook euslisp-mode-hook
                cmake-mode-hook
                javascript-mode-hook js-mode-hook
                emacs-lisp-mode-hook))
  (add-hook mode (lambda () (interactive) (column-marker-1 80)))
  )

(global-set-key "\C-x;" 'comment-region)
;;(fset 'uncomment-region "\C-u\C-[xcomment-region\C-m")
(global-set-key "\C-x:" 'uncomment-region)

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
(column-number-mode 1)

(display-time)

;; (autoload 'svn-status "dsvn" "Run `svn status'." t)
;; (autoload 'svn-update "dsvn" "Run `svn update'." t)

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
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(define-key global-map [165] nil)
(define-key global-map [67109029] nil)
(define-key global-map [134217893] nil)
(define-key global-map [201326757] nil)
(define-key function-key-map [165] [?\\])
(define-key function-key-map [67109029] [?\C-\\])
(define-key function-key-map [134217893] [?\M-\\])
(define-key function-key-map [201326757] [?\C-\M-\\])

(autoload 'goby "goby" nil t)

(defun gcode-lookup ()
  "カーソル位置のシンボルをGoogle Codeで検索(lisp決め打ち)"
  (interactive)
  (browse-url
   (format
    "http://www.google.com/codesearch?q=%s+lang:%s+file:\\.%s$&hl=ja&num=20"
    (thing-at-point 'symbol) "lisp" "lisp")))
(global-set-key "\C-cs" 'gcode-lookup)

(global-set-key "\M-g" 'goto-line)


;; (when t
;;   (when-gui
;;    ;; 文字の色を設定します。
;;    (set-foreground-color "white")
;;    ;; 背景色を設定します。
;;    (set-background-color "black")
;;    ;; モードラインの文字の色を設定します。
;;    (set-face-foreground 'modeline "white")
;;    ;; モードラインの背景色を設定します。
;;    (set-face-background 'modeline "gray40")
;;    ;; カーソルの色を設定します。
;;    (set-cursor-color "yellow")
;;    ;; マウスポインタの色を設定します。
;;    (set-mouse-color  "yellow")
;;    )
;;   (when-gui
;;    ;; 透明に
;;    (set-frame-parameter nil 'alpha '(85 70)))
;;   )

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

(eval-after-load "ispell"
  '(setq ispell-skip-region-alist
     (cons '("[^¥000-¥377]")
           ispell-skip-region-alist)))

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
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(global-set-key "\C-cl" 'magit-status)

;; initial parameter for commit
;;(setq magit-custom-options '("--author" "\"Ryohei Ueda\" <garaemon@gmail.com>"))

(require 'markdown-mode)

(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
(define-key markdown-mode-map (kbd "M-p") nil)
(define-key markdown-mode-map (kbd "M-n") nil)

(require 'w3m)
(defun w3m-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url newwin)))

(defun markdown-render-w3m (n)
  (interactive "p")
  (call-process "/usr/local/bin/grip" nil nil nil
                "--gfm" "--export"
                (buffer-file-name)
                "/tmp/grip.html")
  (w3m-browse-url-other-window "file://tmp/grip.html")
  )
(define-key markdown-mode-map (kbd "C-c C-c") 'markdown-render-w3m)

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

(when nil                               ;disable migemo
(cond
 ((meadowp)
  (setq migemo-directory "c:/cygwin/usr/local/share/migemo")
  (load "migemo"))
 ((cygwinp)
  (setq migemo-directory "/usr/local/share/migemo")
  (load "migemo"))
 ((or (cocoa-emacs-p) (carbon-emacs-p))
  (setq load-path (cons "~/elisp/migemo" load-path))
  (load "migemo.el")
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-dictionary "/usr/local/share/migemo/euc-jp/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil))
 (t                     ;Linux
  ;;(setq migemo-directory "/usr/local/share/migemo")
  (setq migemo-directory "/usr/share/migemo")
  (load "migemo")
  (migemo-init))
 ))

;;=============================================
;;               mode-info
;;---------------------------------------------
;; http://www.namazu.org/~tsuchiya/elisp/mode-info.html
;; M-x mode-info-make-all-indicesでインデックス作成
;; M-x help-for-help f setqとかするらしいがうまくうごかん.
;; M-.でもつかえるとか
;;=============================================

;; (require 'mi-config)
;; (setq mode-info-index-directory "~/Documents/info/index")
;; (setq Info-directory-list
;;       (append
;;        Info-default-directory-list
;;        (list
;;      (expand-file-name "~/Documents/info")
;;      (expand-file-name "~/Documents/info/glibc-2.3.2"))))
;; (define-key global-map "\C-chf" 'mode-info-describe-function)
;; (define-key global-map "\C-chv" 'mode-info-describe-variable)
;; (define-key global-map "\M-." 'mode-info-find-tag)
;; (require 'mi-fontify)
;; (setq mode-info-class-alist
;;       '((elisp  emacs-lisp-mode lisp-interaction-mode)
;;      (libc   c-mode c++-mode)
;;      (make   makefile-mode)
;;      (perl   perl-mode cperl-mode eperl-mode)
;;      (ruby   ruby-mode)
;;      (gauche scheme-mode scheme-interaction-mode inferior-scheme-mode)))


(setq navi2ch-article-auto-range nil)
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
(setq navi2ch-list-bbstable-url
      "http://azlucky.s25.xrea.com/2chboard/bbsmenu2.html")

(setq visible-bell t)
(setq make-backup-files nil)

(unless-cygwin
 (scroll-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)

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

;; (cond
;;  ((meadowp)
;;   (setq shell-file-name "c:/cygwin/bin/zsh.exe")
;;   (modify-coding-system-alist 'process ".*sh\\.exe" 'undecided-unix))
;;  ((cygwinp)
;;   (setq shell-file-name "/bin/zsh"))
;;  (t
;;   (setq shell-file-name "zsh")))
(setq explicit-shell-file-name shell-file-name)
(setq shell-command-option "-c")
(setq system-uses-terminfo nil)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
;; C-lでシェルをクリアするようにする
(defun shell-clear()
  "In shell-mode,clear all display and move cursor top of the buffer."
  (interactive)
  (recenter 0))
(eval-after-load "shell"
  '(define-key shell-mode-map "\C-l" 'shell-clear))

;;shell-modeでlsしたとき色を表示
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(show-paren-mode t)
(setq show-paren-style 'mixed)


(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

;; (require 'tramp)
;; (setq tramp-default-method "sshx")

;; (setq truncate-lines nil)
;; (setq truncate-partial-width-windows nil)


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(fset 'yes-or-no-p 'y-or-n-p)

;; Yatex
(require 'yatex)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-kanji-code 4)
(setq YaTeX-latex-message-code 'utf-8)


(defun shell-command-sequence (cmd &rest others)
  (mapc 'shell-command `(,cmd ,@others)))

(defun yatex-typeset-and-preview ()
  (interactive)
  (let* ((f (buffer-file-name))
         (dir (file-name-directory f))
         (stem (file-name-sans-extension f)))
    (save-excursion
      (basic-save-buffer)
      (shell-command-sequence (concat "cd " dir)
                              (concat "platex --kanji=utf8 " f)
                              (concat "dvipdfmx " stem ".dvi")
                              (concat "rm " stem ".dvi")
                              (concat "rm " stem ".aux") ;; ここはコメントアウトすべきかも
                              (concat "rm " stem ".log"))
      (pop-to-buffer (find-file-noselect (concat stem ".pdf"))))))

(defun yatex-typeset-and-preview-region (beg end)
  (interactive "r")
  (let* ((f (file-name-nondirectory (buffer-file-name)))
         (tmpfile (concat "/tmp/" f))
         (stem (file-name-sans-extension tmpfile))
         (contents (buffer-substring-no-properties beg end))
         header)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\\\\begin{document}")
      (setq header (buffer-substring-no-properties (point-min) (1+ (match-end 0))))
      (set-buffer (find-file-noselect tmpfile))
      (insert header)
      (insert contents)
      (insert "\\end{document}")
      (unwind-protect
          (yatex-typeset-and-preview)
        ;;(shell-command (concat "rm " tmpfile))
        (kill-buffer (current-buffer))))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode nil)
             (local-set-key (kbd "C-c C-c") 'yatex-typeset-and-preview)
             (local-set-key (kbd "C-c C-r") 'yatex-typeset-and-preview-region)))
(setq dvi2-command "xdvi")
(setq YaTeX-inhibit-prefix-letter t)

;; for emacs24 x mac
(setq mac-command-modifier 'meta)

(require 'magit)
(prefer-coding-system 'utf-8)

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
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
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(setq system-uses-terminfo nil)

;;(require 'python-mode)

(load-library "php-mode")
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
(setq-default c-basic-offset 2)
(c-set-offset 'substatement-open 0)
;; (require 'judge-indent)
;; (global-judge-indent-mode t)
;; (setq judge-indent-major-modes '(c-mode python-mode sh-mode c++-mode lisp-mode euslisp-mode html-mode))


;; nyan-mode
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)

;; (require 'garaemon-objective-c)
;; (add-hook 'objc-mode-hook
;;           (function (lambda ()
;;                       (setq c-basic-offset 2))))


(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(require 'expand-region)
(defmacro save-mark-and-excursion (&rest body)
  `(save-excursion ,@body))

(require 'multiple-cursors)
(require 'smartrep)

(global-set-key (kbd "C-^") 'er/expand-region)
(global-set-key (kbd "C-M-^") 'er/contract-region)

(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(global-set-key (kbd "C-M-j") 'mc/edit-lines)

(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f4>") 'highlight-symbol-remove-all)

(require 'auto-highlight-symbol-config)

(require 'git-gutter)
(global-git-gutter-mode +1)
;;(git-gutter:linum-setup)


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


;; helm
(when (>= emacs-major-version 24)
  (require 'helm)
  (require 'helm-config)
  (require 'helm-swoop)
  (require 'helm-gtags)

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
  ;; does not activate helm for find-file
  ;; For find-file etc.
  ;;(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; For helm-find-files etc.
  ;;(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (require 'helm-replace-string)
  (require 'helm-regexp)
  (defadvice query-replace (before helm-replace-string-query-replace
                                   (from-string to-string &optional delimited start end backward) activate)
    (helm-replace-string-push-history from-string to-string 'query-string))

  (custom-set-variables
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-ls-git
                                 helm-c-source-replace-string
                                 helm-source-files-in-current-dir
                                 helm-source-recentf
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

  (require 'helm-ag)
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-thing-at-point 'symbol)
  (define-key global-map (kbd "C-x g")   'helm-ag)
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
  ;;helm-query-replace-regexp
  ;;(global-set-key (kbd "M-%") 'helm-replace-string)
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

;;(load-theme 'zenburn t)
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/modules/solarized/")
  (load-theme 'solarized-dark t)
  ;;(load-theme 'solarized-light t)
  )


(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))

;; undo tree
;; C-x u
(require 'undo-tree)
(global-undo-tree-mode)
(define-key undo-tree-visualizer-mode-map
  "\C-m" 'undo-tree-visualizer-quit)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\|rosinstall\\)$" . yaml-mode))

;; powerline
(when (>= emacs-major-version 24)
  (require 'powerline)
  ;;(powerline-default-theme)
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
                       '(:eval (list (nyan-create)))
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

  (make-face 'mode-line-color-2)
  (set-face-attribute 'mode-line-color-2 nil
                      :foreground "#fff"
                      :background color2)

  (set-face-attribute 'mode-line nil
                      :foreground "#fff"
                      :background color3
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#fff"
                      :background color4))
;; ignore case when find-file completion
(setq completion-ignore-case t)

(require 'trr)

;; (require 'sublimity)
;; (require 'sublimity-map)
;; (require 'sublimity-scroll)
;; (require 'sublimity-attractive)
;; (sublimity-mode 1)

;; (require 'indent-guide)
;; (indent-guide-global-mode nil)

(require 'volatile-highlights)
(volatile-highlights-mode)

(require 'anzu)
(global-anzu-mode +1)
(setq anzu-search-threshold 1000)
(setq anzu-minimum-input-length 3)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "M-&") 'anzu-query-replace) ;for mistype

;; (require 'yascroll)
;; (global-yascroll-bar-mode 0)

;; 除外したい拡張子
(setq delete-trailing-whitespace-exclude-patterns
      (list "\\.cpp$" "\\.h$" "\\.l" "\\.py" "\\.launch" "\\.test"
            "\\.cmake" "\\.xml"))

(require 'cl)
(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond ((equal nil (loop for pattern in delete-trailing-whitespace-exclude-patterns
                          thereis (string-match pattern buffer-file-name)))
         (delete-trailing-whitespace))))

(add-hook 'before-save-hook 'delete-trailing-whitespace-with-exclude-pattern)

(require 'emoji-cheat-sheet)

;; Force to load yasnippet/yasnippet.el in order to avoid
;; to use yasnippet.el under elpa packages.
(load "~/.emacs.d/modules/yasnippet/yasnippet.el")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/modules/yasnippet/snippets"))
(yas-global-mode 1)
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

(defun insert-date()
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
;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
(require 'popwin)
(push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'foreign-regexp)
(custom-set-variables
'(foreign-regexp/regexp-type 'perl) ;; Choose your taste of foreign regexp
                                    ;; from 'perl, 'ruby or 'javascript.
'(reb-re-syntax 'foreign-regexp))   ;; Tell re-builder to use foreign regex.

(require 'smart-cursor-color)
(smart-cursor-color-mode +1)

(require 'w3m)
(define-key w3m-mode-map (kbd "M-p") nil)
(define-key w3m-mode-map (kbd "M-n") nil)

(defun w3m-ros (pkg)
  (interactive (list (read-string "ros package: ")))
  (w3m-browse-url-other-window (format "http://wiki.ros.org/%s" pkg)))

(require 'dired-subtree)

(require 'dired+)

(global-hl-line-mode +1)

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

;; (global-linum-mode)

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
  (start-process "shiba" "*shiba*" "zsh" "-ic" (format "shiba --detach %s" buffer-file-name))
  )
(define-key markdown-mode-map (kbd "C-c C-c") 'open-with-shiba)
(define-key markdown-mode-map (kbd "C-c m") 'newline)
;; For emacs 24
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))

;; (require 'milkode)
;; Path-to-gmilk
;; (setq gmilk-command "~/.rvm/gems/ruby-2.2.0/bin/gmilk")
;; (global-set-key (kbd "C-c C-s") 'milkode:search)

;; (require 'c-eldoc)
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;; (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
;; (setq c-eldoc-buffer-regenerate-time 60)

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
(provide 'garaemon-dot-emacs)
