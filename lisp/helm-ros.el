;; helm-ros.el

;; written by garaemon
;; 
;; please use like:
;;  (require 'helm-ros)
;;  (global-set-key "\C-xb" 'helm-mini-with-ros)

(require 'helm)

(defvar helm-source-catkin-root "~/catkin_tmp/hydro")

(defun helm-catkin-packages-list ()
  (let ((string-output (shell-command-to-string (format "find %s -name package.xml -exec dirname {} \\\;"  helm-source-catkin-root))))
    (let ((dirs (split-string string-output "\n")))
      dirs)))

(defvar helm-source-catkin-packages
  `((name . "catkin packages")
    (inint . (setq helm-catkin-packages-list (helm-catkin-packages-list)))
    (candidates . helm-catkin-packages-list)
    (type . file)))

(defun helm-mini-with-ros ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-source-buffers-list
                         helm-source-recentf
                         helm-source-catkin-packages
                         helm-source-buffer-not-found)
                       "*helm mini*")))

(provide 'helm-ros)
