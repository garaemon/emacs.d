(deftheme garaemon-solarized-dark
  "Created 2019-03-09.")

(custom-theme-set-faces
 'garaemon-solarized-dark
 '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "black" :stipple nil :inherit nil))))
 '(cursor ((t (:background "red"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(highlight ((t (:background "color-23"))))
 '(region ((t (:foreground "black" :background "color-244"))))
 '(shadow ((t (:foreground "brightblack"))))
 '(secondary-selection ((t (:background "brightblack"))))
 '(trailing-whitespace ((t (:foreground "yellow" :background "cyan"))))
 '(font-lock-builtin-face ((t (:foreground "cyan"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "brightyellow"))))
 '(font-lock-comment-face ((t (:foreground "brightblack"))))
 '(font-lock-constant-face ((t (:foreground "brightred"))))
 '(font-lock-doc-face ((t (:foreground "brightblue"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "magenta"))))
 '(font-lock-negation-char-face ((t (:foreground "green"))))
 '(font-lock-preprocessor-face ((t (:foreground "blue"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "yellow"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "magenta"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "red"))))
 '(font-lock-warning-face ((t (:foreground "red"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "blue"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "magenta"))))
 '(fringe ((t (:background "black"))))
 '(header-line ((t (:foreground "magenta" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line ((t (:box nil :inverse-video nil :foreground "gray60" :background "color-16"))))
 '(mode-line-buffer-id ((t (:inherit (sml/filename)))))
 '(mode-line-emphasis ((t (:foreground "brightred" :slant italic))))
 '(mode-line-highlight ((t (:weight bold :box nil :foreground "magenta"))))
 '(mode-line-inactive ((t (:box nil :inverse-video nil :foreground "gray60" :background "#404045"))))
 '(isearch ((t (:inverse-video t :foreground "yellow" :background "black"))))
 '(isearch-fail ((t (:inverse-video t :background "black" :inherit (font-lock-warning-face)))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((t (:inverse-video t :foreground "blue" :background "black"))))
 '(next-error ((t (:foreground "black" :inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'garaemon-solarized-dark)
