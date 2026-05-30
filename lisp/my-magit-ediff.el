;;; my-magit-ediff.el --- Multi-file ediff for Magit -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides functions to ediff all changed files sequentially.
;; When you quit one ediff session, a navigation prompt lets you go
;; to the next/previous file or quit the review.

;;; Code:

(require 'magit)
(require 'ediff)

;;;; State variables

(defvar my-magit-ediff--files nil
  "All files to ediff.")

(defvar my-magit-ediff--current-index 0
  "Current file index (0-based).")

(defvar my-magit-ediff--rev-a nil
  "Revision A for comparison.")

(defvar my-magit-ediff--rev-b nil
  "Revision B for comparison. nil means working tree, `index' means staged.")

(defvar my-magit-ediff--total-count 0
  "Total number of files for progress display.")

(defvar my-magit-ediff--active nil
  "Non-nil when multi-file ediff session is running.")

(defvar my-magit-ediff--temp-buffers nil
  "Temporary revision buffers to kill on navigation or cleanup.")

(defvar my-magit-ediff--reviewed nil
  "Hash table mapping a file to t when it has been marked reviewed.")

(defvar my-magit-ediff--memos nil
  "Hash table mapping a file to its memo string.")

(defconst my-magit-ediff--sidebar-buffer-name "*Ediff Review*"
  "Name of the buffer that hosts the review file-list sidebar.")

(defconst my-magit-ediff--summary-buffer-name "*Ediff Review Summary*"
  "Name of the buffer that shows the review summary on quit.")

;;;; Entry point functions

;;;###autoload
(defun my-magit-ediff-all-unstaged ()
  "Ediff all unstaged files against HEAD sequentially."
  (interactive)
  (let ((files (magit-unstaged-files)))
    (if (null files)
        (message "No unstaged changes.")
      (my-magit-ediff--start files "HEAD" nil))))

;;;###autoload
(defun my-magit-ediff-all-staged ()
  "Ediff all staged files against HEAD sequentially."
  (interactive)
  (let ((files (magit-staged-files)))
    (if (null files)
        (message "No staged changes.")
      (my-magit-ediff--start files "HEAD" 'index))))

;;;###autoload
(defun my-magit-ediff-all-compare (rev-a rev-b)
  "Ediff all changed files between REV-A and REV-B sequentially."
  (interactive
   (let ((rev-a (magit-read-branch-or-commit "Revision A"))
         (rev-b (magit-read-branch-or-commit "Revision B")))
     (list rev-a rev-b)))
  (let ((files (magit-changed-files rev-a rev-b)))
    (if (null files)
        (message "No changes between %s and %s." rev-a rev-b)
      (my-magit-ediff--start files rev-a rev-b))))

;;;; Core engine functions

(defun my-magit-ediff--start (files rev-a rev-b)
  "Initialize state and begin ediff session for FILES between REV-A and REV-B."
  (setq my-magit-ediff--files files
        my-magit-ediff--current-index 0
        my-magit-ediff--rev-a rev-a
        my-magit-ediff--rev-b rev-b
        my-magit-ediff--total-count (length files)
        my-magit-ediff--active t
        my-magit-ediff--reviewed (make-hash-table :test 'equal)
        my-magit-ediff--memos (make-hash-table :test 'equal))
  (add-hook 'ediff-quit-hook #'my-magit-ediff--on-quit)
  (my-magit-ediff--show-sidebar)
  (my-magit-ediff--open-current))

(defun my-magit-ediff--open-current ()
  "Open ediff for the file at current index."
  (let ((file (nth my-magit-ediff--current-index my-magit-ediff--files)))
    (message "Ediff [%d/%d]: %s"
             (1+ my-magit-ediff--current-index)
             my-magit-ediff--total-count
             file)
    (my-magit-ediff--open-file file)))

(defun my-magit-ediff--open-file (file)
  "Open ediff for a single FILE using current rev-a/rev-b."
  (let ((buf-a (my-magit-ediff--create-revision-buffer
                file my-magit-ediff--rev-a))
        (buf-b (my-magit-ediff--create-revision-buffer
                file my-magit-ediff--rev-b)))
    (ediff-buffers buf-a buf-b)))

(defun my-magit-ediff--create-revision-buffer (file rev)
  "Create a buffer with FILE content at REV.
If REV is nil, return the working tree file buffer.
If REV is `index', return the staged content.
If REV is a string, return that revision's content."
  (let ((default-directory (magit-toplevel)))
    (cond
     ((null rev)
      (find-file-noselect (expand-file-name file)))
     ((eq rev 'index)
      (let ((buf (generate-new-buffer
                  (format "%s (index)" file))))
        (with-current-buffer buf
          (magit-git-insert "cat-file" "-p" (concat ":0:" file))
          (let ((buffer-file-name (expand-file-name file)))
            (set-auto-mode))
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (push buf my-magit-ediff--temp-buffers)
        buf))
     ((stringp rev)
      (let ((buf (generate-new-buffer
                  (format "%s (%s)" file (substring rev 0 (min 7 (length rev)))))))
        (with-current-buffer buf
          (magit-git-insert "cat-file" "-p" (concat rev ":" file))
          (let ((buffer-file-name (expand-file-name file)))
            (set-auto-mode))
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (push buf my-magit-ediff--temp-buffers)
        buf)))))

(defun my-magit-ediff--kill-temp-buffers ()
  "Kill all temporary revision buffers."
  (dolist (buf my-magit-ediff--temp-buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq my-magit-ediff--temp-buffers nil))

(defun my-magit-ediff--on-quit ()
  "Hook called when ediff session ends. Show navigation prompt if active."
  (when my-magit-ediff--active
    ;; Delay to let ediff finish cleanup before killing temp buffers.
    (run-at-time 0.1 nil #'my-magit-ediff--after-quit)))

(defun my-magit-ediff--after-quit ()
  "Kill temp buffers from previous session, then refresh the sidebar."
  (my-magit-ediff--kill-temp-buffers)
  (when my-magit-ediff--active
    (my-magit-ediff--render-sidebar)
    (let ((window (get-buffer-window my-magit-ediff--sidebar-buffer-name)))
      (when (window-live-p window)
        (select-window window)))))

(defun my-magit-ediff--cleanup ()
  "Reset state variables, remove hook and close the sidebar window."
  (my-magit-ediff--kill-temp-buffers)
  (let ((window (get-buffer-window my-magit-ediff--sidebar-buffer-name)))
    (when (window-live-p window)
      (ignore-errors (delete-window window))))
  (let ((buffer (get-buffer my-magit-ediff--sidebar-buffer-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setq my-magit-ediff--files nil
        my-magit-ediff--current-index 0
        my-magit-ediff--rev-a nil
        my-magit-ediff--rev-b nil
        my-magit-ediff--total-count 0
        my-magit-ediff--active nil
        my-magit-ediff--reviewed nil
        my-magit-ediff--memos nil)
  (remove-hook 'ediff-quit-hook #'my-magit-ediff--on-quit))

;;;; Data model helpers

(defun my-magit-ediff--file-reviewed-p (file)
  "Return non-nil when FILE has been marked reviewed."
  (and my-magit-ediff--reviewed
       (gethash file my-magit-ediff--reviewed)))

(defun my-magit-ediff--toggle-file-reviewed (file)
  "Toggle the reviewed flag for FILE."
  (if (my-magit-ediff--file-reviewed-p file)
      (remhash file my-magit-ediff--reviewed)
    (puthash file t my-magit-ediff--reviewed)))

(defun my-magit-ediff--file-memo (file)
  "Return the memo string stored for FILE, or nil when none."
  (and my-magit-ediff--memos
       (gethash file my-magit-ediff--memos)))

(defun my-magit-ediff--set-file-memo (file memo)
  "Store MEMO for FILE. An empty MEMO removes any existing memo."
  (if (string-empty-p memo)
      (remhash file my-magit-ediff--memos)
    (puthash file memo my-magit-ediff--memos)))

(defun my-magit-ediff--reviewed-count ()
  "Return the number of files marked reviewed."
  (if my-magit-ediff--reviewed
      (hash-table-count my-magit-ediff--reviewed)
    0))

(defun my-magit-ediff--format-file-line (file index current-index)
  "Return the sidebar display line for FILE at INDEX.
CURRENT-INDEX marks the file currently shown in ediff so it can be
highlighted.  The returned string carries FILE in the
`my-magit-ediff-file' text property for click and RET handling."
  (let* ((reviewed-marker (if (my-magit-ediff--file-reviewed-p file) "[✓]" "[ ]"))
         (memo-marker (if (my-magit-ediff--file-memo file) " ✎" "  "))
         (pointer (if (= index current-index) "▶ " "  "))
         (line (format "%s%s%s %s" pointer reviewed-marker memo-marker file)))
    (when (= index current-index)
      (setq line (propertize line 'face 'highlight)))
    (propertize line 'my-magit-ediff-file file)))

(defun my-magit-ediff--build-summary-text ()
  "Return a textual review summary covering reviewed state and memos."
  (let ((lines (list (format "Ediff review summary — reviewed %d/%d\n"
                             (my-magit-ediff--reviewed-count)
                             my-magit-ediff--total-count))))
    (dolist (file my-magit-ediff--files)
      (let ((status (if (my-magit-ediff--file-reviewed-p file) "DONE" "TODO"))
            (memo (my-magit-ediff--file-memo file)))
        (push (format "[%s] %s" status file) lines)
        (when memo
          (push (format "        memo: %s" memo) lines))))
    (concat (string-join (nreverse lines) "\n") "\n")))

;;;; Sidebar rendering and commands

(defun my-magit-ediff--render-sidebar ()
  "Rebuild the contents of the review sidebar buffer."
  (let ((buffer (get-buffer-create my-magit-ediff--sidebar-buffer-name))
        (saved-line nil))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my-magit-ediff-review-mode)
        (my-magit-ediff-review-mode))
      (setq saved-line (line-number-at-pos))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "Review  reviewed %d/%d\n\n"
                         (my-magit-ediff--reviewed-count)
                         my-magit-ediff--total-count)
                 'face 'bold))
        (let ((index 0))
          (dolist (file my-magit-ediff--files)
            (insert (my-magit-ediff--format-file-line
                     file index my-magit-ediff--current-index)
                    "\n")
            (setq index (1+ index)))))
      (goto-char (point-min))
      (forward-line (1- saved-line)))))

(defun my-magit-ediff--show-sidebar ()
  "Display the review sidebar in a left side window."
  (my-magit-ediff--render-sidebar)
  (display-buffer-in-side-window
   (get-buffer my-magit-ediff--sidebar-buffer-name)
   '((side . left)
     (window-width . 40)
     (window-parameters . ((no-delete-other-windows . t))))))

(defun my-magit-ediff--select-content-window ()
  "Select a non-side window so ediff does not take over the sidebar."
  (let ((window (or (window-with-parameter 'window-side nil)
                    (get-largest-window nil nil t))))
    (when (and window
               (not (window-parameter window 'window-side)))
      (select-window window))))

(defun my-magit-ediff-review-open-file-at-point ()
  "Open ediff for the file on the current sidebar line."
  (interactive)
  (let ((file (get-text-property (point) 'my-magit-ediff-file)))
    (if (null file)
        (message "No file on this line.")
      (let ((index (seq-position my-magit-ediff--files file #'string=)))
        (when index
          (setq my-magit-ediff--current-index index)
          (my-magit-ediff--select-content-window)
          (my-magit-ediff--open-current)
          (my-magit-ediff--render-sidebar))))))

(defun my-magit-ediff-review-toggle-reviewed ()
  "Toggle the reviewed flag for the file on the current sidebar line."
  (interactive)
  (let ((file (get-text-property (point) 'my-magit-ediff-file)))
    (if (null file)
        (message "No file on this line.")
      (my-magit-ediff--toggle-file-reviewed file)
      (my-magit-ediff--render-sidebar))))

(defun my-magit-ediff-review-edit-memo ()
  "Edit the memo for the file on the current sidebar line."
  (interactive)
  (let ((file (get-text-property (point) 'my-magit-ediff-file)))
    (if (null file)
        (message "No file on this line.")
      (let ((memo (read-string (format "Memo for %s: " file)
                               (my-magit-ediff--file-memo file))))
        (my-magit-ediff--set-file-memo file memo)
        (my-magit-ediff--render-sidebar)))))

(defun my-magit-ediff-review-refresh ()
  "Redraw the review sidebar."
  (interactive)
  (my-magit-ediff--render-sidebar))

(defun my-magit-ediff-review-quit ()
  "End the review, showing a summary buffer of reviewed state and memos."
  (interactive)
  (let ((summary (my-magit-ediff--build-summary-text))
        (reviewed (my-magit-ediff--reviewed-count))
        (total my-magit-ediff--total-count))
    (my-magit-ediff--cleanup)
    (with-current-buffer (get-buffer-create my-magit-ediff--summary-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert summary))
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer my-magit-ediff--summary-buffer-name)
    (message "Review ended. %d/%d files reviewed." reviewed total)))

(defvar my-magit-ediff-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my-magit-ediff-review-open-file-at-point)
    (define-key map (kbd "<mouse-1>") #'my-magit-ediff-review-open-file-at-point)
    (define-key map (kbd "d") #'my-magit-ediff-review-toggle-reviewed)
    (define-key map (kbd "m") #'my-magit-ediff-review-edit-memo)
    (define-key map (kbd "g") #'my-magit-ediff-review-refresh)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "q") #'my-magit-ediff-review-quit)
    map)
  "Keymap for `my-magit-ediff-review-mode'.")

(define-derived-mode my-magit-ediff-review-mode special-mode "EdiffReview"
  "Major mode for the multi-file ediff review sidebar."
  (setq truncate-lines t))

;;;; Keybindings

(with-eval-after-load 'magit-ediff
  (transient-append-suffix 'magit-ediff '(0 -1)
    ["All files"
     ("A u" "All unstaged" my-magit-ediff-all-unstaged)
     ("A s" "All staged" my-magit-ediff-all-staged)
     ("A r" "All revisions" my-magit-ediff-all-compare)]))

(provide 'my-magit-ediff)
;;; my-magit-ediff.el ends here
