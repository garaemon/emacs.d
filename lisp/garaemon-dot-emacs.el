;;; garaemon-dot-emacs.el --- This is the emacs settings file for garaemon

;;; Commentary:

;;; Code:

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:


(require 'garaemon-util)
(require 'cl)

;; (setq file-name-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
(defun gde-basic-settings()
  "Basic settings.

All the setting is wrapped by function for imenu indexing."
  ;; C-o to expand completion
  (global-set-key "\C-o" 'dabbrev-expand)
  ;; Increase threshold to fire garbage collection
  (setq gc-cons-threshold 134217728)
  (global-set-key "\C-h" 'backward-delete-char)
  (global-set-key "\M-h" 'help-for-help)
  ;; C-x p to switch buffer with inverse manner.
  (global-set-key "\C-xp" (lambda ()
                            (interactive)
                            (other-window -1)))
  ;; Show line number
  (line-number-mode 1)
  ;; Show time
  (display-time)
  ;; Show active region
  (setq-default transient-mark-mode t)
  (global-set-key "\M-g" 'goto-line)
  ;; Indent when insert newline
  (global-set-key "\C-m" 'newline-and-indent)
  (global-set-key "\C-j" 'newline)
  ;; Do not concer about upper/lower case in completion.
  (setq completion-ignore-case t)
  (global-unset-key "\C-\\")
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  ;; no scroll bar
  (scroll-bar-mode -1)
  ;; no menu bar
  (menu-bar-mode -1)
  ;; no tool bar
  (tool-bar-mode -1)
  (setq scroll-conservatively 1)
  ;; Do not create ~ files
  (setq make-backup-files nil)
  ;; Disable bell
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  ;; Highlight parens
  (show-paren-mode t)
  (custom-set-variables
   '(show-paren-style 'mixed))
  ;; Disable hard tabs
  (setq-default indent-tabs-mode nil)
  ;; Disable startup image
  (setq inhibit-startup-message t)
  ;; Force to use y-or-n-p
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; ??
  (setq system-uses-terminfo nil)
  (setq-default tab-width 4)
  ;; scroll down compilation buffer when new output is available.
  (custom-set-variables
   '(compilation-scroll-output t))
  ;; sort with ignoring case.
  (custom-set-variables '(sort-fold-case t t)))
(gde-basic-settings)

(defun gde-gui-settings ()
  "Setup GUI configurations."
  (when-darwin
   ;; see http://d.hatena.ne.jp/kazu-yamamoto/20090122/1232589385
   (set-face-attribute 'default nil :family "monaco" :height 110)
   ;; Japanese font
   (set-fontset-font
    nil 'japanese-jisx0208
    (font-spec :family "Hiragino Kaku Gothic ProN")) ; font
   (setq ns-command-modifier (quote meta))
   (setq ns-alternate-modifier (quote super))
   ;; Do not pass control key to mac OS X
   (defvar mac-pass-control-to-system)
   (setq mac-pass-control-to-system nil)
   ;; for emacs24 x mac
   (setq mac-command-modifier 'meta))
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil
                        :height 120)    ;font size
    (set-frame-font "Ricty Diminished-12")))
(gde-gui-settings)

(defun gde-anthy-setting ()
  "Setup anty for no window environment."
  ;; anthy
  (when (or (eq system-type 'cygwin)
          (eq system-type 'gnu/linux))
  (setq load-path (append '("/usr/share/emacs/site-lisp/anthy/") load-path))
  (load-library "anthy")
  (global-unset-key "\C-\\")
  (setq default-input-method "japanese-anthy")
  (global-set-key "\C-\\" 'anthy-mode)))
(gde-anthy-setting)

(defun gde-column-marker-setting ()
  "Setup column-marker package."
  (require 'column-marker)
  ;; Hihghlight character exceeds 100 column
  (add-hook 'prog-mode-hook (lambda ()
                              (column-marker-1 100))))
(gde-column-marker-setting)

(defun gde-cuda-setting ()
  "Setup for nVIDIA CUDA."
  (setq auto-mode-alist (cons (cons "\\.cu?$" 'c-mode) auto-mode-alist))
  (setq auto-mode-alist (cons (cons "\\.cg?$" 'c-mode) auto-mode-alist)))
(gde-cuda-setting)

(defun gde-c++-setting ()
  "Setup c++ mode."
  (setq auto-mode-alist (cons (cons "\\.h?$" 'c++-mode) auto-mode-alist))

  ;; Quick & Dirty C++11 support
  (defun c++-mode-hook-c++11 ()
    (font-lock-add-keywords
     nil
     '( ;; complete some fundamental keywords
       ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" .
        font-lock-keyword-face)
       ;; namespace names and tags - these are rendered as constants by cc-mode
       ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
       ;;  new C++11 keywords
       ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>"
        . font-lock-keyword-face)
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
       (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1
           font-lock-string-face t)       ; actual string
       (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1
           font-lock-keyword-face t)      ; end delimiter

       ;; user-defined types (rather project-specific)
       ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
       ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face))))

  (add-hook 'c++-mode-hook (lambda()
                             (set-fill-column 100)
                             (c++-mode-hook-c++11)
                             (google-set-c-style)
                             (google-make-newline-indent)
                             (setq c-basic-offset 4))))
(gde-c++-setting)

(defun gde-euslisp-setting()
  "euslisp programing language settings."
  (require 'euslisp-mode)
  (setq auto-mode-alist (cons (cons "\\.l$" 'euslisp-mode) auto-mode-alist))

  (defvar inferior-euslisp-program)
  (defun lisp-other-window ()
    "Run Lisp on other window."
    (interactive)
    (if (not (string= (buffer-name) "*inferior-lisp*"))
        (switch-to-buffer-other-window (get-buffer-create "*inferior-lisp*")))
    (run-lisp inferior-euslisp-program))

  (set-variable 'inferior-euslisp-program "~/.emacs.d/roseus.sh")

  (global-set-key "\C-cE" 'lisp-other-window))
(gde-euslisp-setting)

(defun gde-javascript-setting ()
  "Setup javascript environment."
  ;;(autoload 'js-mode "js")
  (defun my-js2-indent-function ()
    "Hook function for indent in js2 mode."
    (interactive)
    (save-restriction (widen)
                      (let* ((inhibit-point-motion-hooks t)
                             (parse-status (save-excursion (syntax-ppss (point-at-bol))))
                             (offset (- (current-column)
                                        (current-indentation)))
                             (indentation (js--proper-indentation parse-status)) node)
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
                        (when (> offset 0)
                          (forward-char offset)))))

  (defun my-indent-sexp ()
    (interactive)
    (save-restriction (save-excursion (widen)
                                      (let* ((inhibit-point-motion-hooks t)
                                             (parse-status (syntax-ppss (point)))
                                             (beg (nth 1 parse-status))
                                             (end-marker (make-marker))
                                             (end (progn (goto-char beg)
                                                         (forward-list)
                                                         (point)))
                                             (ovl (make-overlay beg end)))
                                        (set-marker end-marker end)
                                        (overlay-put ovl 'face 'highlight)
                                        (goto-char beg)
                                        (while (< (point)
                                                  (marker-position end-marker))
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
    (setq js-indent-level 2 indent-tabs-mode nil c-basic-offset 2)
    ;; Disable some js2 features for eslint integration by flycheck
    (setq js2-include-browser-externs nil)
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-highlight-external-variables nil)
    (setq js2-include-jslint-globals nil)
    (c-toggle-auto-state 0)
    (c-toggle-hungry-state 1)
    (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
    ;;  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
    (define-key js2-mode-map [(meta control \;)]
      '(lambda()
         (interactive)
         (insert "/* -----[ ")
         (save-excursion (insert " ]----- */"))))
    (define-key js2-mode-map [(return)] 'newline-and-indent)
    (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
    (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
    (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
    (if
        (featurep 'js2-highlight-vars)
        (js2-highlight-vars-mode))

    (rainbow-delimiters-mode)
    (message "My JS2 hook"))

  (add-hook 'js2-mode-hook 'my-js2-mode-hook)
  (require 'js2-mode)
  (setq auto-mode-alist (cons (cons "\\.js$" 'js2-mode) auto-mode-alist))
  (setq auto-mode-alist (cons (cons "\\.jsx$" 'js2-jsx-mode) auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
  (setq js-indent-level 4)
  (add-hook 'js-mode (lambda ()
                       (setq js-indent-level 4)
                       (setq c-basic-offset 4)))
  )
(gde-javascript-setting)

(defun gde-goby-setting ()
  "Setup goby programming language."
  (autoload 'goby "goby" nil t))
(gde-goby-setting)

(defun gde-haskell-setting ()
  "Setup haskell programming language."
  (setq auto-mode-alist (append auto-mode-alist '(("\\.[hg]s$"  . haskell-mode)
                                                  ("\\.hi$"     . haskell-mode)
                                                  ("\\.l[hg]s$" . literate-haskell-mode))))
  (autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell scripts." t)
  (autoload 'literate-haskell-mode "haskell-mode" "Major mode for editing literate Haskell scripts."
    t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
  (defvar haskell-literate-default)
  (defvar haskell-doc-idle-delay)
  (setq haskell-literate-default 'latex)
  (setq haskell-doc-idle-delay 0)

  (global-set-key "\C-c(" 'hs-hide-block)
  (global-set-key "\C-c)" 'hs-show-block)
  (global-set-key "\C-c{" 'hs-hide-all)
  (global-set-key "\C-c}" 'hs-show-all))
(gde-haskell-setting)

(defun gde-html-setting ()
  "Setup HTML environment."
  (setq auto-mode-alist (cons (cons "\\.html$" 'html-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|kml\\|gpx\\)\\'" . html-mode)
                              auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.ejs$" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.xml$" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.launch$" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.test$" . html-mode))
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'web-mode-hook)
  )
(gde-html-setting)

(defun gde-c-setting ()
  "Setup 'c-mode'."
  (require 'google-c-style)
  (defvar c-default-style)
  (setq c-default-style "stroustrup")
  (setq-default c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
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
          (statement-cont . (,(when (fboundp 'c-no-indent-after-java-annotations)
                                'c-no-indent-after-java-annotations)
                             ,(when (fboundp 'c-lineup-assignments) 'c-lineup-assignments) ++))
          (label . /)
          (case-label . +)
          (statement-case-open . +)
          (statement-case-intro . +)      ; case w/o {
          ;;(access-label . /)
          (innamespace . 0)))
  )
(gde-c-setting)

(defun gde-lisp-mode-setting ()
  "Setup Lisp mode."
  (font-lock-add-keywords
   'lisp-mode
   (list
    ;; *hoge*に色を付ける
    (list "\\(\\*\\w\+\\*\\)\\>" '(1 font-lock-constant-face nil t))
    ;; +hoge+に色を付ける
    (list "\\(\\+\\w\+\\+\\)\\>" '(1 font-lock-constant-face nil t))
    ;; <hoge>に色を付ける
    (list "\\(<\\w\+>\\)\\>" '(1 font-lock-constant-face nil t))
    ;; defclass*に色を付ける
    (list "\\(defclass\\*\\)" '(1 font-lock-keyword-face nil t))))

  (defun cl-indent (sym indent)
    (put sym 'common-lisp-indent-function
         (if (symbolp indent)
             (get indent 'common-lisp-indent-function) indent)))
  (cl-indent 'iterate 'let)
  (cl-indent 'collect 'progn)
  (cl-indent 'mapping 'let)
  (cl-indent 'mapping 'let)
  (cl-indent 'define-test 'let))
(gde-lisp-mode-setting)

(defun gde-magit-setting ()
  "Setup magit-mode."
  ;; magit!
  (add-to-list 'exec-path "/opt/local/bin")
  (require 'magit)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  ;; Usage of gh-pulls
  ;;   - refresh list of pr: '# g'
  ;;   - creat a new pr:     '# b'
  ;;   - merge a pr:         '# m'
  (global-set-key "\C-cl" 'magit-status)
  (global-set-key "\C-cL" 'magit-status))
(gde-magit-setting)

(defun gde-markdown-setting ()
  "Setup 'markdown-mode'."
  (require 'markdown-mode)

  (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (defvar markdown-mode-map)
  (define-key markdown-mode-map (kbd "M-p") nil)
  (define-key markdown-mode-map (kbd "M-n") nil)
  (defun open-with-shiba ()
    "open a current markdown file with shiba"
    (interactive)
    (start-process "shiba" "*shiba*" "shiba" "--detach" buffer-file-name))
  (define-key markdown-mode-map (kbd "C-c C-c") 'open-with-shiba)
  (define-key markdown-mode-map (kbd "C-c m") 'newline)
  ;; For emacs 24
  (add-hook 'markdown-mode-hook '(lambda ()
                                   (electric-indent-local-mode -1))))
(gde-markdown-setting)

(defun gde-migemo-setting ()
  "Setup migemo."
  ;; sudo apt-get install cmigemo is required
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (custom-set-variables
     '(migemo-command "cmigemo")
     '(migemo-options '("-q" "--emacs"))
     '(migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
     '(migemo-user-dictionary nil)
     '(migemo-regex-dictionary nil)
     '(migemo-coding-system 'utf-8-unix))
    (load-library "migemo")
    (migemo-init)))
(gde-migemo-setting)

(defun gde-scroll-in-place-setting ()
  "Scroll buffer with keeping mouse position."
  (global-unset-key "\M-p")
  (global-unset-key "\M-n")

  (defun scroll-up-in-place (n)
    "Scroll up the current buffer with keeping cursor position.

- N the number of the lines to scroll up"
    (interactive "p")
    (forward-line (- n))
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
  (global-set-key [M-down] 'scroll-down-in-place))
(gde-scroll-in-place-setting)

(defun gde-scheme-setting ()
  "Setup scheme setting."
  (require 'cmuscheme)
  (setq process-coding-system-alist
        (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
  (custom-set-variables
   '(scheme-program-name "gosh")
   '(gosh-program-name "/usr/bin/env gosh -i")
   '(scheme-program-name "gosh -i"))

  (defun scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name))

  (define-key global-map "\C-cS" 'scheme-other-window)

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
  (font-lock-add-keywords
   'scheme-mode
   (list
    (list (concat "(" (regexp-opt '("use") t) "\\>")
          '(1 font-lock-keyword-face nil t))
    (list "\\(self\\)\\>" '(1 font-lock-constant-face nil t))
    (list "\\(\\*\\w\+\\*\\)\\>" '(1 font-lock-constant-face nil t))
    (list "\\(#\\(\\+\\|\\-\\)\.\*\\)" '(1 font-lock-variable-name-face))
    (cons "\\(dotimes\\|unless\\|when\\|dolist\\|while\\)\\>" 1)
    (cons
     "\\(let-\\(keywords\\|optionals\\|values\\|keywords\\*\\|optionals\\*\\|values\\*\\)\\)\\>"
     1)
    (list "\\(warn\\)\\>" '(1 font-lock-warning-face))
    (list "\\(#t\\|#f\\)\\>" '(1 font-lock-constant-face))
    (cons "\\(defclass\\|defmethod\\)\\>" 1)
    (cons "\\(define-\\(function\\*\\|class\\*\\|method\\*\\)\\)\\>" 1))))
(gde-scheme-setting)

(defun gde-sdic-setting ()
  "Setup sdic."
  (autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
  (global-set-key "\C-cW" 'sdic-describe-word-at-point))
(gde-sdic-setting)

(defun gde-tramp-setting ()
  "Setup tramp mode."

  (require 'tramp)
  (custom-set-variables
   '(tramp-verbose 10)
   '(tramp-default-method "ssh")))
(gde-tramp-setting)

(defun gde-uniquify-setting ()
  "Setup uniquify in order to distinguish the files which have the same names."
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
(gde-uniquify-setting)

;;(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

(defun gde-ruby-setting ()
  "Setup ruby programming language."
  (add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
  (defvar ruby-indent-level 2))

(defun gde-nxml-setting ()
  "Setup nxml for xml editing."
  (defvar nxml-child-indent 2)
;; nxml mode
  (custom-set-variables
   '(mumamo-background-colors nil)))
(gde-nxml-setting)

(defun gde-less-css-mode-setting ()
  "Setup 'less-css-mode' setting."
  (require 'less-css-mode))
(gde-less-css-mode-setting)

(defun gde-ucs-normalize-setting ()
  "Setup 'ucs-normalize'."
  (require 'ucs-normalize))
(gde-ucs-normalize-setting)

(defun gde-php-setting ()
  "Setup php programming language."
  (require 'php-mode))
(gde-php-setting)

(defun gde-visualize-abnormal-whitespaces ()
  "Visualize abnormal white spaces such as hard tab and japanese space."
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
  (set-face-background 'whitespace-tab "DarkSlateGray"))
(gde-visualize-abnormal-whitespaces)

(defun gde-cmake-setting ()
  "Setup cmake environment."
  (require 'cmake-mode)
  (setq auto-mode-alist (cons '("CMakeLists.txt" . cmake-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.cmake$" . cmake-mode) auto-mode-alist)))
(gde-cmake-setting)

(defun gde-objc-setting ()
  "Setup objective c encironment."
  ;; objective-c
  (add-to-list 'magic-mode-alist
               `(,(lambda ()
                    (and (string= (file-name-extension buffer-file-name) "h")
                         (re-search-forward "@\\<interface\\>"
                                            magic-mode-regexp-match-limit t))) . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.m$" . objc-mode)))
(gde-objc-setting)

(defun gde-puppet-setting ()
  "Setup puppet environment."
  (require 'puppet-mode)
  (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode)))
(gde-puppet-setting)

(defun gde-expand-region-setting ()
  "Setup 'expand-region'."
  (require 'expand-region)
  ;; why required...?
  (when (<= emacs-major-version 24)
    (defmacro save-mark-and-excursion
        (&rest
         body)
      `(save-excursion ,@body)))

  (global-set-key (kbd "C-^") 'er/expand-region)
  (global-set-key (kbd "C-M-^") 'er/contract-region))
(gde-expand-region-setting)

(defun gde-multiple-cursors-setting ()
  "Setup 'multiple-cursors'."
  (require 'multiple-cursors)

  (global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
  (global-set-key (kbd "C-M-j") 'mc/edit-lines)

  (global-set-key (kbd "<C-M-down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "<C-M-up>") 'mc/mark-previous-like-this))
(gde-multiple-cursors-setting)

(defun gde-smartrep-setting ()
  "Setup 'smartrep'."
  (require 'smartrep)

  (declare-function smartrep-define-key "smartrep")
  (global-unset-key "\C-q")
  (smartrep-define-key global-map "C-q" '(("C-t"      . 'mc/mark-next-like-this)
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
                                          ("O"        . 'mc/reverse-regions))))
(gde-smartrep-setting)

(defun gde-auto-highlight-symbol-setting ()
  "Setup 'auto-highlight-symbol'."
  (require 'auto-highlight-symbol)
  (global-auto-highlight-symbol-mode t))
(gde-auto-highlight-symbol-setting)

(defun gde-git-gutter-fringe+-setting ()
  "Setup 'git-gutter-fringe+'."
  (require 'git-gutter-fringe+)
  (global-git-gutter+-mode))
(gde-git-gutter-fringe+-setting)

(defun gde-nlinum-setting ()
  "Setup 'nlinum'."
  (require 'nlinum)
  (global-nlinum-mode)
  ;; these linum-delay and linum-schedule are required even if nlinum-mode is used?
  (setq linum-delay t)
  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current))
  )
(gde-nlinum-setting)

(defun gde-recentf-ext-setting ()
  "Setup 'recentf-ext'."
  (require 'recentf-ext))
(gde-recentf-ext-setting)

(defun gde-help-setting ()
  "Setup 'helm'."
  (when (>= emacs-major-version 24)
    (require 'helm)
    (require 'helm-config)
    (require 'helm-swoop)
    (require 'helm-gtags)
    (setq helm-gtags-auto-update t)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    (custom-set-variables '(helm-gtags-prefix-key "C-t")
                          '(helm-gtags-suggested-key-mapping t)
                          '(helm-gtags-ignore-case t)
                          '(helm-gtags-auto-update nil))
    (eval-after-load "helm-gtags"
      '(progn (define-key helm-gtags-mode-map (kbd "C-:")
                'helm-gtags-find-pattern)
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
     '(helm-mini-default-sources
       '(helm-source-buffers-list helm-source-ls-git
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
    (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
      "Emulate `kill-line' in helm minibuffer"
      (kill-new
       (buffer-substring
        (point)
        (field-end))))
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
    ))
(gde-help-setting)

(defun gde-minimap-setting ()
  "Setup minimap."
  (require 'minimap)
  (minimap-mode))

(defun gde-theme-setting ()
  "Setup theme."

  (when (and window-system (>= emacs-major-version 24))
    (load-theme 'solarized-dark t))
  ;;(require 'color-theme-solarized)
  ;;(color-theme-solarized)

  ;; Supporting terminal.
  ;; See https://github.com/bbatsov/solarized-emacs/issues/18#issuecomment-313521878
  (defun my-solarized-dark-workaround (frame)
    "Fix solarized-dark theme for terminal FRAME."
    (with-selected-frame frame
      (if (and (featurep 'color-theme)
               (not window-system))
          (set-face-background 'default "none" frame))))

  ;; first created emacsclient frame not fixed so use focus-in-hook
  (add-hook 'after-make-frame-functions #'my-solarized-dark-workaround)

  (defun my-solarized-dark-on-focus ()
    "Fix solarized-dark theme for terminal on focus."
    (my-solarized-dark-workaround (selected-frame)))

  (add-hook 'focus-in-hook #'my-solarized-dark-on-focus)
  ;; (seq-doseq (frame (frame-list)) (my-solarized-dark-workaround frame))
  )
(gde-theme-setting)

(defun gde-rainbow-delimiters-setting ()
  "Setup 'rainbow-delimiters'."
  (require 'rainbow-delimiters)
  (custom-set-faces '(rainbow-delimiters-depth-1-face
                      ((t
                        (:foreground "#7f8c8d")))))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(gde-rainbow-delimiters-setting)

(defun gde-undo-tree-setting ()
  "Visualize undo history as tree."
  ;; C-x u
  (require 'undo-tree)
  (global-undo-tree-mode)
  (define-key undo-tree-visualizer-mode-map "\C-m" 'undo-tree-visualizer-quit))
(gde-undo-tree-setting)

(defun gde-undohist-setting ()
  "Persistend undo history even though emacs process is closed."
  (require 'undohist)
  (undohist-initialize)
  (setq undohist-ignored-files '("/tmp/")))
(gde-undohist-setting)

(defun gde-yaml-setting ()
  "Setup yaml mode."
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\|rosinstall\\)$" . yaml-mode)))
(gde-yaml-setting)

(defun gde-powerline-setting ()
  "Setup powerline."
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
    (defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t
                                        :ascent 'center))
    (defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t
                                        :ascent 'center))
    (defvar arrow-left-1 (create-image (arrow-left-xpm color2 color1) 'xpm t
                                       :ascent 'center))
    (defvar arrow-left-2 (create-image (arrow-left-xpm "None" color2) 'xpm t
                                       :ascent 'center))
    (setq-default mode-line-format
                  (list
                   '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
                                   (propertize " " 'display arrow-right-1)))
                   '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
                                   (propertize " " 'display arrow-right-2)))
                   ;; Justify right by filling with spaces to right fringe - 16
                   ;; (16 should be computed rahter than hardcoded)
                   '(:eval (propertize " " 'display '((space :align-to (-
                                                                        right-fringe
                                                                        17)))))
                   '(:eval (concat (propertize " " 'display arrow-left-2)
                                   (propertize " %p " 'face 'mode-line-color-2)))
                   '(:eval (concat (propertize " " 'display arrow-left-1)
                                   (propertize "%4l:%2c  " 'face 'mode-line-color-1)))))
    (make-face 'mode-line-color-1)
    (set-face-attribute 'mode-line-color-1 nil
                        :foreground "#fff"
                        :background color1)
    (make-face 'mode-line-color-2)
    (set-face-attribute 'mode-line-color-2 nil
                        :foreground "#fff"
                        :background nil)
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
                        :inherit 'mode-line)))
(gde-powerline-setting)

(defun gde-trr-setting ()
  "Setup trr."
  (require 'trr))
(gde-trr-setting)

(defun gde-volatile-highlights-setting ()
  "Setup volatile-highlights to provide feedback visually."
  (require 'volatile-highlights)
  (volatile-highlights-mode))
(gde-volatile-highlights-setting)

(defun gde-anzu-setting ()
  "Setup anzu to visualize the number of match in status bar."
  (require 'anzu)
  (global-anzu-mode +1)
  (custom-set-variables
   '(anzu-search-threshold 1000)))
(gde-anzu-setting)

(defun gde-delete-trailing-whitespace ()
  "Delete trailing whitespaces when saving the file."
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
(gde-delete-trailing-whitespace)

(defun gde-emoji-cheat-sheet-setting ()
  "Setup emoji cheat sheet."
  (require 'emoji-cheat-sheet))
(gde-emoji-cheat-sheet-setting)

(defun gde-yasnippet-setting ()
  "Setup yasnippet."
  ;; Force to load yasnippet/yasnippet.el in order to avoid
  ;; to use yasnippet.el under elpa packages.
  (require 'yasnippet)
  (custom-set-variables
   '(yas-snippet-dirs ("~/.emacs.d/snippets"
                       "~/.emacs.d/el-get/yasnippet/snippets"))
   '(yas-trigger-key "Enter"))
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
    (custom-set-variables
     '(helm-c-yas-space-match-any-greedy t))
    (global-set-key (kbd "M--") 'helm-c-yas-complete)))
(gde-yasnippet-setting)

(defun gde-insert-date ()
  "Setup config to insert date at current cursor."
  (defun insert-date ()
    "Insert date at current cursor."
    (interactive)
    (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ\n" nil t))))
(gde-insert-date)

(defun gde-graphviz-setting ()
  "Setup graphviz environment."
  (require 'graphviz-dot-mode)
  (defun graphviz-compile-preview ()
    "Compile and preview graphviz dot file."
    (interactive)
    (compile compile-command)
    (sleep-for 1)
    (graphviz-dot-preview))
  (global-set-key [f5] 'graphviz-compile-preview))
(gde-graphviz-setting)

(defun gde-direx-setting ()
  "Setup direx."
  ;; direx
  (require 'direx)
  (require 'direx-project)
  (require 'popwin)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '(direx:direx-mode :position left
                           :width 50
                           :dedicated t) popwin:special-display-config)

  ;; re-redifine function in order to support .repo
  (defun direx-project:vc-repo-p (dirname)
    (cl-loop for vc-dir in '(".repo") thereis (file-exists-p (expand-file-name vc-dir dirname))))

  (defun direx-project:vc-root-p (dirname)
    (cl-loop for vc-dir in '(".git" ".hg" ".bzr") thereis (file-exists-p (expand-file-name vc-dir
                                                                                           dirname))))

  (defun direx-project:project-root-p (dirname)
    (cl-some (lambda (fun)
               (funcall fun dirname)) direx-project:project-root-predicate-functions))

  (defun direx-project:project-repo-root-p (dirname)
    (cl-some (lambda (fun)
               (funcall fun dirname))
             '(direx-project:vc-repo-p)))

  (defun direx-project:find-project-root-noselect (filename)
    (interactive)
    (or (cl-loop for parent-dirname in (if (file-directory-p filename)
                                           (cons filename (direx:directory-parents filename))
                                         (direx:directory-parents filename)) if
                                         (direx-project:project-repo-root-p parent-dirname) return
                                         (direx:find-directory-noselect parent-dirname))
        (cl-loop for parent-dirname in (if (file-directory-p filename)
                                           (cons filename (direx:directory-parents filename))
                                         (direx:directory-parents filename)) if
                                         (direx-project:project-root-p parent-dirname) return
                                         (direx:find-directory-noselect parent-dirname))))
  (global-set-key (kbd "C-x C-j") 'direx-project:jump-to-project-root-other-window))
(gde-direx-setting)


(defun gde-dockerfile-setting ()
  "Setup dockerfile mode."
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
(gde-dockerfile-setting)

(defun gde-smart-cursor-color-setting ()
  "Setup 'smart-cursor-color' to change cursor color according to background and foreground color."
  (require 'smart-cursor-color)
  (smart-cursor-color-mode +1))
(gde-smart-cursor-color-setting)

(defun gde-dired-setting ()
  "Setup dired more."
  (require 'dired-subtree)
  (require 'dired+))
(gde-dired-setting)

(defun gde-hl-line-setting ()
  "Setup hl-line to highlight current line."
  (require 'hl-line)
  ;; Small delay to update hl-line to reduce CPU load.
  (defun global-hl-line-timer-function ()
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight)))
  (custom-set-variables
   (list 'global-hl-line-timer
     (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))))
(gde-hl-line-setting)

(defun gde-qrr-setting ()
  "Setup query-replace-regexp."
  (defalias 'qrr 'query-replace-regexp)
  ;; for mistype :)
  (global-set-key "\M-%" 'query-replace))
(gde-qrr-setting)

(defun gde-symon-setting ()
  "Setup symon to visualize CPU, memory and so on."
  (require 'symon)
  (custom-set-variables '(symon-sparkline-type symon-sparkline-type-gridded)
                        '(symon-delay 5))
  (symon-mode))
(gde-symon-setting)

(defun gde-bm-setting ()
  "Setup bm to make bookmark."
  (require 'bm)
  (global-set-key [?\C-\M-\ ] 'bm-toggle)
  (global-set-key [?\C-\M-n] 'bm-next)
  (global-set-key [?\C-\M-p] 'bm-previous)
  (set-face-background bm-face "orange")
  (set-face-foreground bm-face "black"))
(gde-bm-setting)

(defun gde-scss-setting ()
  "Setup scss environment."
  (require 'scss-mode)
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode)))
(gde-scss-setting)

(defun gde-rosemacs-setting ()
  "Setup rosemacs."
  (when
      (require 'rosemacs nil t)
    (invoke-rosemacs)
    (global-set-key "\C-x\C-r" ros-keymap)))
(gde-rosemacs-setting)

(defun gde-gist-setting ()
  "Setup gist."
  (require 'gist))
(gde-gist-setting)

(defun gde-window-manage-utils ()
  "Setup useful functions to manage multiple windows."

  (defun split-window-vertically-n (num-wins)
    "Split window vertically into `num-wins' windows."
    (interactive "p")
    (if (= num-wins 2)
        (split-window-vertically)
      (progn (split-window-vertically (- (window-height)
                                         (/ (window-height) num-wins)))
             (split-window-vertically-n (- num-wins 1)))))

  (defun split-window-horizontally-n (num-wins)
    "Split window horizontally into `num-wins' windows."
    (interactive "p")
    (if (= num-wins 2)
        (split-window-horizontally)
      (progn (split-window-horizontally (- (window-width)
                                           (/ (window-width) num-wins)))
             (split-window-horizontally-n (- num-wins 1)))))

  (defun other-window-or-split ()
    "Split window if there is enough space and switch to next window."
    (interactive)
    (when (one-window-p)
      (if (>= (window-body-width) 300)
          (let ((split-num (/ (window-body-width) 100)))
            (split-window-horizontally-n split-num))
        (split-window-horizontally)))
    (win-switch-dispatch))

  (global-set-key "\M-o" 'other-window-or-split)
  (global-set-key "\C-x@" '(lambda ()
                             (interactive)
                             (split-window-vertically-n 3)))
  (global-set-key "\C-x#" '(lambda ()
                             (interactive)
                             (split-window-horizontally-n 3))))
(gde-window-manage-utils)

(defun gde-coffee-setting ()
  "Setup coffee programming language."
  (require 'coffee-mode)
  (add-hook 'coffee-mode-hook
            '(lambda()
               (set (make-local-variable 'tab-width) 2)
               (setq coffee-tab-width 2))))
(gde-coffee-setting)

(defun gde-flycheck-setting ()
  "Setup flycheck as linter."
  (require 'flycheck)
  (custom-set-variables '(flycheck-check-syntax-automatically (mode-enabled save)))
  (global-flycheck-mode t)
  ;; flycheck runs emacs with `-Q` option to lint emacs lisp codes. It means that
  ;; load-path is not taken into account in linting.
  ;; By assiging `flycheck-emacs-lisp-load-path` to 'inherit, flycheck runs emacs with
  ;; `load-path` inherited from the current emacs.
  (custom-set-variables
   '(flycheck-emacs-lisp-load-path 'inherit))

  (defun flycheck-exclude-tramp ()
    "Do not run flycheck for the file over tramp mode.

See http://fukuyama.co/tramp-flycheck"
    (unless (or (and (fboundp 'tramp-tramp-file-p)
                     (tramp-tramp-file-p buffer-file-name))
                (string-match "sudo:.*:" (buffer-file-name)))
      (flycheck-mode t)))

  ;; Do not use flycheck-pos-tip on cocoa emacs
  (if (not (eq system-type 'darwin))
      (progn
        (require 'flycheck-pos-tip)
        (eval-after-load 'flycheck
          '(custom-set-variables
            '(flycheck-display-errors-function
              #'flycheck-pos-tip-error-messages)
            '(flycheck-disable-checkers '(c/c++-clang
                                          c/c++-gcc
                                          javascript-jshint))))))
  ;; disable clang and gcc linter
  (custom-set-variables
   '(flycheck-disable-checkers '(flycheck-disable-checkers . (c/c++-clang
                                                              c/c++-gcc))))
  ;; disable jshint: append?
  (custom-set-variables
   '(flycheck-disable-checkers '(flycheck-disable-checkers . (javascript-jshint))))

  ;; googlelint for C++
  (eval-after-load 'flycheck
    '(progn
       (require 'flycheck-google-cpplint)
       (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint))))
  (custom-set-variables
   '(flycheck-googlelint-filter "-runtime/references,-readability/braces")
   '(flycheck-googlelint-verbose "3"))

  ;; python
  ;; check flake8 version.
  ;; If flake8 is newer than 2.0, it does not have --stdin-display-name.
  (if (executable-find "flake8")
      (let* ((version-command-output (with-temp-buffer (shell-command "flake8 --version"
                                                                      (current-buffer))
                                                       (buffer-substring-no-properties
                                                        (point-min)
                                                        (point-max))))
             (version-string (car (split-string version-command-output " ")))
             (major-version (read (car (split-string version-string "\\."))))
             (error-filter-func #'(lambda (errors)
                                    (let ((errors (flycheck-sanitize-errors errors)))
                                      (seq-do #'flycheck-flake8-fix-error-level errors)
                                      errors))))
        (if (>= major-version 2)
            (progn (message "flake8 vresion is larger than 2.0 %s" major-version)
                   ;; Use eval ` to evaluate error-filter before quoting.
                   (eval `(flycheck-define-checker python-flake8
                            "A Python syntax and style checker using Flake8.
This patch is depending on https://github.com/flycheck/flycheck/issues/1078.

Requires Flake8 3.0 or newer. See URL
`https://flake8.readthedocs.io/'."
                            :command ("flake8"
                                      ;; "--format=default"
                                      ;; (config-file "--config" flycheck-flake8rc)
                                      (option "--max-complexity"
                                              flycheck-flake8-maximum-complexity nil
                                              flycheck-option-int)
                                      (option "--max-line-length"
                                              flycheck-flake8-maximum-line-length nil
                                              flycheck-option-int) "--ignore=E901" "-")
                                        ;psource)
                            :standard-input t
                            :error-filter ,error-filter-func
                            :error-patterns
                            ((warning line-start "stdin:" line ":" (optional
                                                                    column ":")
                                      " " (id (one-or-more (any alpha))
                                              (one-or-more digit)) " " (message
                                                                        (one-or-more
                                                                         not-newline))
                                              line-end))
                            :modes python-mode)))
          (message "flake8 vresion is smaller than 2.0"))))
  )
(gde-flycheck-setting)

(defun gde-typescript-setting ()
  "Setup typescript."
  (require 'typescript)
  (setq auto-mode-alist (cons (cons "\\.ts?$" 'typescript-mode) auto-mode-alist)))
(gde-typescript-setting)

(defun gde-replace-utilities-for-japanese ()
  "Setup some utility functions for Japanese."

  (defun replace-punctuation (a1 a2 b1 b2)
    "Replace periods and commas"
    (let ((s1 (if mark-active "選択領域" "バッファ全体"))
          (s2 (concat a2 b2))
          (b (if mark-active (region-beginning)
               (point-min)))
          (e (if mark-active (region-end)
               (point-max))))
      (if (y-or-n-p (concat s1 "の句読点を「" s2 "」にしますがよろしいですか?"))
          (progn (replace-string a1 a2 nil b e)
                 (replace-string b1 b2 nil b e)))))

  (defun commaperiod ()
    "選択領域またはバッファ全体の句読点を「，．」にします"
    (interactive)
    (replace-punctuation "、" ", " "。" ". "))


  (defun replace-commaperiod-buffer ()
    "バッファ全体の句読点と読点をコンマとピリオドに変換"
    (interactive "r")
    (save-excursion (replace-string "、" ", " nil (point-min)
                                    (point-max))
                    (replace-string "。" ". " nil (point-min)
                                    (point-max))))

  (defun hankaku-eisuu-region (start end)
    "選択範囲内の全角英数字を半角英数字に変換"
    (interactive "r")
    (while (string-match "[０-９Ａ-Ｚａ-ｚ]+"
                         (buffer-substring
                          start
                          end))
      (save-excursion (japanese-hankaku-region (+ start (match-beginning 0))
                                               (+ start (match-end 0))))))

  (defun hankaku-eisuu-buffer ()
    "バッファ全体の全角英数字を半角英数字に変換"
    (interactive)
    (hankaku-eisuu-region (point-min)
                          (point-max)))

  (defun replace-commaperiod-before-save-if-needed ()
    (when (memq major-mode '(latex-mode))
      (replace-commaperiod-buffer)
      (hankaku-eisuu-buffer)))

  ;; register as function
  (add-hook 'before-save-hook 'replace-commaperiod-before-save-if-needed))
(gde-replace-utilities-for-japanese)

(defun gde-google-this-setting ()
  "Setup google."
 (require 'google-this)
 (global-set-key (kbd "C-x g") 'google-this-mode-submap)
 (global-set-key (kbd "C-c g") 'google-this))
(gde-google-this-setting)

(defun gde-lua-setting ()
  "Setup lua environment."
  (require 'lua-mode))
(gde-lua-setting)

(defun gde-protobuf-setting ()
  "Setup protobuf environment."
  ;; Colorize Protobuf
  (require 'protobuf-mode)
  (defconst my-protobuf-style '((c-basic-offset . 4)
                                (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook (lambda ()
                                  (c-add-style "my-style" my-protobuf-style t)))
  (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.pb.txt$" . protobuf-mode))
  )
(gde-protobuf-setting)

(global-set-key "\M-[" 'code-format-view)

(defun gde-ediff-setting ()
  "Show eidiff with horizontal split view."
  (custom-set-variables '(ediff-split-window-function 'split-window-horizontally)))
(gde-ediff-setting)


(defun gde-win-switch-setting ()
  "Pretermit prefix key to switch windows."
  (require 'win-switch)
  ;; simple functions to change background color of selected buffer

  (custom-set-variables '(win-switch-feedback-background-color "yellow")
                        '(win-switch-feedback-foreground-color "black")
                        '(win-switch-idle-time 1.5)
                        '(win-switch-window-threshold 1))

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
  )
(gde-win-switch-setting)

(defun gde-company-setting ()
  "Setup company for completion."
  (require 'company)
  (global-company-mode)
  (setq company-idle-delay 0.1)           ; デフォルトは0.5
  (setq company-minimum-prefix-length 2)  ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-h") 'backward-delete-char)
  (define-key company-active-map (kbd "C-h") 'backward-delete-char)
  (push (apply-partially #'cl-remove-if (lambda (c)
                                          (or (string-match-p "[^\x00-\x7F]+" c)
                                              (string-match-p "[0-9]+" c)
                                              (if (equal major-mode "org")
                                                  (>= (length c) 15))))) company-transformers))
(gde-company-setting)

(defun gde-irony-setting ()
  "Setup irony for c++ completion."
  (require 'irony)
  ;; run "M-x irony-install-server" the first time
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (setq irony-lang-compile-option-alist '((c++-mode . ("c++" "-std=c++11" "-lstdc++" "-lm"))
                                          (c-mode . ("c"))
                                          (objc-mode . '("objective-c"))))
  (defun irony--lang-compile-option ()
    (irony--awhen (cdr-safe (assq major-mode irony-lang-compile-option-alist))
      (append '("-x") it)))
  (add-to-list 'company-backends 'company-irony))
(gde-irony-setting)

(defun gde-jedi-setting ()
  "Setup jedi for python code completion."
  (require 'jedi-core)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'auto-mode-alist '("\\.cfg$" . python-mode))
  (add-to-list 'company-backends 'company-jedi))
(gde-jedi-setting)

(defun gde-reopen-file ()
  "Re-open current file with \M-r."
  (defun revert-buffer-no-confirm (&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    ;;(message "force-reverting value is %s" force-reverting)
    (if (or force-reverting
            (not (buffer-modified-p)))
        (revert-buffer
         :ignore-auto
         :noconfirm)
      (error
       "The buffer has been modified")))
  (global-set-key "\M-r" 'revert-buffer-no-confirm))
(gde-reopen-file)

(defun gde-tmux-integration ()
  "Setup tmux integration utilities."
  ;; for tmux integration
  (defun open-current-file-in-tmux ()
    (interactive)
    (let ((file-path (buffer-file-name)))
      (let ((target-dir (if (file-directory-p file-path)
                            file-path
                          (file-name-directory file-path))))
        (message (format "Opening directory %s in tmux" target-dir))
        (call-process-shell-command "tmux" nil "*tmux-output*" nil
                                    (format
                                     "new-window -a -t $(tmux ls -F \"#S\") -c %s"
                                     target-dir)))))

  (global-set-key "\M-t" 'open-current-file-in-tmux))
(gde-tmux-integration)

(defun gde-backup-each-save-setting ()
  "Setup backup all the history."
  (require 'backup-each-save)
  ;; where backuped file goes
  (setq backup-each-save-mirror-location "~/.emacs.d/backups")
  ;; suffix for backup file
  (setq backup-each-save-time-format "%y%m%d_%H%M%S")
  ;; the size limit of backup files
  (setq backup-each-save-size-limit 5000000)
  ;; backup all the files
  (setq backup-each-save-filter-function 'identity)
  ;; enable backup
  (add-hook 'after-save-hook 'backup-each-save))
(gde-backup-each-save-setting)

(defun gde-auto-save-buffers-enhanced-setting ()
  "Setup auto-save-buffers-enhanced mode."
  (require 'auto-save-buffers-enhanced)
  (auto-save-buffers-enhanced-include-only-checkout-path t)
  (auto-save-buffers-enhanced nil))
(gde-auto-save-buffers-enhanced-setting)

(defun gde-manipulate-number-at-point ()
  "Increase and decrease the number at point."
  (defun increment-number-at-point ()
    "Increase number at current cursor."
    (interactive)
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
        (error
         "No number at point"))
    (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
  (defun decrement-number-at-point ()
    "Decrease number at current cursor."
    (interactive)
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
        (error
         "No number at point"))
    (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

  (global-set-key (kbd "C-c C-+") 'increment-number-at-point)
  (global-set-key (kbd "C-c C-;") 'increment-number-at-point)
  (global-set-key (kbd "C-c C--") 'decrement-number-at-point))
(gde-manipulate-number-at-point)

(defun gde-rust-setting ()
  "Setup rust programming language."
  (require 'rust-mode))
(gde-rust-setting)

(defun gde-calfw-setting ()
  "Setup calfw mode."
  (require 'calfw))

(defun gde-shell-script-setting ()
  "Setup shell script environment."
  (add-to-list 'auto-mode-alist '("\\.subr$" . shell-script-mode)))
(gde-shell-script-setting)

(defun gde-org-setting ()
  "Setup `org-mode'."
  (require 'org)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  (setq org-capture-templates '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
                                 "* TODO %?\n  %i\n  %a")
                                ("j" "Journal" entry (file+datetree "~/org/journal.org")
                                 "* %?\nEntered on %U\n  %i\n  %a")))

  (require 'ob-ipython)

  ;; コードを評価するとき尋ねない
  (setq org-confirm-babel-evaluate nil)
  ;; ソースコードを書き出すコマンド

  (defun random-alnum ()
    (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
           (i (% (abs (random))
                 (length alnum))))
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
    (insert "#+END_SRC\n"))

  (defun org-ipython-insert-matplotlib-block ()
    (interactive)
    ;; create .image directory under current directory
    (if (not (file-exists-p ".images"))
        (make-directory ".images"))
    (let ((random-png-file (format ".images/%s%s%s%s%s.png" (random-alnum)
                                   (random-alnum)
                                   (random-alnum)
                                   (random-alnum)
                                   (random-alnum))))
      (if (not (file-exists-p random-png-file))
          (progn (insert (format "#+BEGIN_SRC ipython :session :file %s :exports both\n"
                                 random-png-file))
                 (insert "#+END_SRC\n")))))

  (define-key org-mode-map (kbd "C-c C-i") 'org-ipython-insert-matplotlib-block)
  (define-key org-mode-map (kbd "C-x C-e") 'org-babel-tangle-and-execute))
(gde-org-setting)

(defun gde-hyde-setting ()
  "Setup hyde-mode for jekyll."
  ;; sample to insert figure
  ;; #+BEGIN_SRC ipython :session :file /tmp/image.png :exports both
  ;; %matplotlib inline
  ;; import matplotlib.pyplot as plt
  ;; import numpy as np
  ;; fig, ax = plt.subplots(facecolor='white')
  ;; ax.hist(np.random.randn(20000), bins=200)
  ;; #+END_SRC

  (require 'hyde)
  (setq-default jekyll-root (expand-file-name "~/gprog/garaemon.github.io"))
  (setq-default hyde-home (expand-file-name "~/gprog/garaemon.github.io"))
  (defun ghyde ()
    (interactive)
    (hyde hyde-home))

  (defun jekyll-new-post (title)
    "Create a new post for jekyll with filling date."
    (interactive "MEnter post title: ")
    (let* ((YYYY-MM-DD (format-time-string "%Y-%m-%d" nil t))
           (file-name (format "%s/_posts/%s-%s.md" jekyll-root YYYY-MM-DD title)))
      ;; template header
      (save-excursion (find-file file-name)
                      (insert "---\n")
                      (insert "layout: post\n")
                      (insert (format "title: %s\n" title))
                      (insert (format "date: %s\n" (format-time-string "%Y-%m-%dT%H:%M:%SJST")))
                      (insert "descriptions:\n")
                      (insert "categories:\n")
                      (insert "- blog\n")
                      (insert "---\n")
                      (insert "\n"))
      (find-file file-name))))
(gde-hyde-setting)

(defun gde-string-inflection ()
  "Setup 'string-inflection' to change casing of words."
  (require 'string-inflection)
  (global-set-key (kbd "C-c i") 'string-inflection-cycle))
(gde-string-inflection)

(defun gde-elisp-format-setting ()
  "Setup 'elisp-format' to format elisp codes."
  (require 'elisp-format))
(gde-elisp-format-setting)

(defun gde-sr-speedbar-setting ()
  "Setup sr-speedbar."
  (require 'sr-speedbar)
  (setq sr-speedbar-right-side nil)
  ;; (sr-speedbar-open)
  )
(gde-sr-speedbar-setting)

(defun gde-imenus-setting ()
  "Setup imenus."
  (require 'imenus))
(gde-imenus-setting)

(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
;; (sr-speedbar-open)

(require 'imenus)

(add-to-list 'auto-mode-alist '("\\.cfg$" . python-mode))

(provide 'garaemon-dot-emacs)
