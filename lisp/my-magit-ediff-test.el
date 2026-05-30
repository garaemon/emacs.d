;;; my-magit-ediff-test.el --- Tests for my-magit-ediff -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the pure data-model helpers of my-magit-ediff.
;; These tests avoid ediff/window side effects and only exercise the
;; reviewed-flag, memo, counting, line-formatting and summary helpers.

;;; Code:

(require 'ert)
(require 'my-magit-ediff)

(defun my-magit-ediff-test--reset-state ()
  "Reset session state to a clean slate for a single test."
  (setq my-magit-ediff--files '("a.el" "b.el" "c.el")
        my-magit-ediff--total-count 3
        my-magit-ediff--reviewed (make-hash-table :test 'equal)
        my-magit-ediff--memos (make-hash-table :test 'equal)))

(ert-deftest should_mark_file_reviewed_when_toggled ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--toggle-file-reviewed "a.el")
  (should (my-magit-ediff--file-reviewed-p "a.el")))

(ert-deftest should_unmark_file_when_toggled_twice ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--toggle-file-reviewed "a.el")
  (my-magit-ediff--toggle-file-reviewed "a.el")
  (should-not (my-magit-ediff--file-reviewed-p "a.el")))

(ert-deftest should_store_and_return_memo ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--set-file-memo "a.el" "needs refactor")
  (should (string= (my-magit-ediff--file-memo "a.el") "needs refactor")))

(ert-deftest should_clear_memo_when_set_empty ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--set-file-memo "a.el" "temp note")
  (my-magit-ediff--set-file-memo "a.el" "")
  (should-not (my-magit-ediff--file-memo "a.el")))

(ert-deftest should_count_reviewed_files ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--toggle-file-reviewed "a.el")
  (my-magit-ediff--toggle-file-reviewed "c.el")
  (should (= (my-magit-ediff--reviewed-count) 2)))

(ert-deftest should_render_check_marker_for_reviewed_file ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--toggle-file-reviewed "a.el")
  (should (string-match-p "\\[✓\\]"
                          (my-magit-ediff--format-file-line "a.el" 0 0))))

(ert-deftest should_render_empty_marker_for_unreviewed_file ()
  (my-magit-ediff-test--reset-state)
  (should (string-match-p "\\[ \\]"
                          (my-magit-ediff--format-file-line "b.el" 1 0))))

(ert-deftest should_render_memo_indicator_when_memo_present ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--set-file-memo "b.el" "note")
  (should (string-match-p "✎"
                          (my-magit-ediff--format-file-line "b.el" 1 0))))

(ert-deftest should_attach_file_property_on_line ()
  (my-magit-ediff-test--reset-state)
  (let ((line (my-magit-ediff--format-file-line "c.el" 2 0)))
    (should (string= (get-text-property 0 'my-magit-ediff-file line) "c.el"))))

(ert-deftest should_include_memo_in_summary ()
  (my-magit-ediff-test--reset-state)
  (my-magit-ediff--set-file-memo "a.el" "important note")
  (should (string-match-p "important note"
                          (my-magit-ediff--build-summary-text))))

(provide 'my-magit-ediff-test)
;;; my-magit-ediff-test.el ends here
