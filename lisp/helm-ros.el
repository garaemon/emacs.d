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

(defun helm-rospack-list ()
  (let ((string-output (shell-command-to-string "zsh -c 'source ~/ros/hydro/devel/setup.zsh >/dev/null 2>&1; rospack list'")))
    (let ((dir-and-packs (split-string string-output "\n")))
      (mapcar #'(lambda (line)
                  (cadr (split-string line " ")))
              dir-and-packs))))

(defvar helm-source-catkin-packages
  `((name . "catkin packages")
    (init . (lambda () (setq helm-catkin-packages-list (helm-catkin-packages-list))))
    (candidates . helm-catkin-packages-list)
    (type . file)))

(defvar helm-source-rospack-list
  `((name . "rospack list")
    (init . (lambda () (setq helm-rospack-list (helm-rospack-list))))
    (candidates . (lambda () helm-rospack-list))
    (type . file)))


(defun helm-mini-with-ros ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-source-buffers-list
                         helm-source-recentf
                         helm-source-catkin-packages
                         helm-source-rospack-list
                         helm-source-buffer-not-found)
                       "*helm mini*")))

(provide 'helm-ros)
