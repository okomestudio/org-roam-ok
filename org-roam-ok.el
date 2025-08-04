;;; org-roam-ok.el --- Org Roam Okome Studio Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-ok
;; Version: 0.9.1
;; Keywords: org-mode, roam, plug-in
;; Package-Requires: ((emacs "30.1") (org "9.7") (org-roam "2.3.1") (async "1.9.7") (dash "2.19.1") (marginalia "1.6") (ok "0.12.1") (org-ref "20250301.1918") (org-roam-timestamps "1.0.0") (s "1.13.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The `org-roam-ok' mode is a plugin to enhance `org-roam' in several
;; ways. See the repository README for detail.
;;
;;; Code:

(require 'ok)

(defgroup org-roam-ok nil
  "Group for `org-roam-ok'."
  :group 'org-roam-ok
  :prefix "org-roam-ok-")

(defvar org-roam-ok-version "0.9.1"
  "Package version.")

(defcustom org-roam-ok-on-idle-delay 60
  "Idle delay in seconds before running on-idle initializer.
When set to nil, the on-idle initializer will not run."
  :group 'org-roam-ok)

;; On-idle initialization

(defvar org-roam-ok--on-idle-timer nil
  "Timer object used by on-idle init scheduler.")

(defun org-roam-ok--init-on-idle ()
  "Initialize `org-roam' on idle."
  (when org-roam-ok--on-idle-timer
    (cancel-timer org-roam-ok--on-idle-timer))

  (remove-hook 'post-command-hook #'org-roam-ok--on-idle-init-scheduler)

  (when (not (and (boundp 'org-roam-ok-node--cache-in-memory)
                  (> (hash-table-count org-roam-ok-node--cache-in-memory) 0)))
    (org-roam-ok-node-fill-caches-async)))

(defun org-roam-ok--on-idle-init-scheduler ()
  "Schedule on-idle initializer."
  (when org-roam-ok--on-idle-timer
    (cancel-timer org-roam-ok--on-idle-timer))
  (unless (null org-roam-ok-on-idle-delay)
    (setq org-roam-ok--on-idle-timer
          (run-with-idle-timer org-roam-ok-on-idle-delay
                               nil
                               #'org-roam-ok--init-on-idle))))

;;;###autoload
(defun org-roam-ok-on-idle-init-setup ()
  "Set up on-idle initializer."
  (org-roam-ok--on-idle-init-scheduler)
  (add-hook 'post-command-hook #'org-roam-ok--on-idle-init-scheduler))

;;; Public functions

;;;###autoload
(defun org-roam-ok-enhance ()
  "Enhance `org-roam' with `org-roam-ok'."
  (setopt find-file-visit-truename t)  ; see "5.3 Setting up Org-roam"
  (require 'org-roam-dailies)
  (org-roam-ok-mode 1))

;;; Minor mode config

(defcustom org-roam-ok-node-display-title #'org-roam-ok-node--title
  "Node title getter function for display template."
  :group 'org-roam-ok)

(defun org-roam-ok-activate ()
  "Activate `org-roam-ok-mode'."
  (require 'org-roam-ok-utils)
  (require 'org-roam-ok-capture)
  (require 'org-roam-ok-mode)
  (require 'org-roam-ok-node)

  (defun org-roam-ok-node-display-template (node &optional total-width)
    "Render NODE using display template and cache.
When not given, TOTAL-WIDTH defaults to the current `frame-width'. Use
this as the default display template function."
    (let* ((total-width (or total-width (frame-width)))

           (title (funcall org-roam-ok-node-display-title node))
           (tags (string-join
                  (mapcar (lambda (s)
                            (let (r)
                              (setq r (concat ":" s ":"))
                              (put-text-property 0 1 'invisible t r)
                              (put-text-property (- (length r) 1) (length r)
                                                 'invisible t r)
                              (setq r (propertize r 'face 'highlight))))
                          (or (org-roam-ok-node--tags node) nil))
                  " "))
           (timestamp (org-roam-ok-node--timestamp node))

           (multibyte-scale 1.7)
           (tag-scale 1.0)

           (timestamp-width (1+ (length timestamp)))
           (title-and-tags-width (- total-width timestamp-width))
           (tags-width (1+ (round (* (length tags) tag-scale))))
           (max-title-width (- title-and-tags-width tags-width))
           (title-width (ok-string-multibyte-string-width title multibyte-scale)))
      (if (>= title-width max-title-width)
          (let ((thresh (length (ok-string-multibyte-substring
                                 title 0 max-title-width multibyte-scale))))
            (put-text-property 0 (1- thresh) 'invisible nil title)
            (put-text-property thresh (length title) 'invisible t title))
        (put-text-property 0 (length title) 'invisible nil title)
        (setq title (concat title
                            (make-string (- max-title-width title-width) ?\ ))))
      (concat title " " tags " " timestamp)))

  (setopt org-roam-node-display-template #'org-roam-ok-node-display-template)

  ;; Wrap these functions so that each will load the project
  ;; .dir-locals.el before its execution.
  (dolist (func '(org-roam-node-find
                  org-roam-node-insert
                  org-roam-dailies-capture-today))
    (advice-add func :around #'org-roam-ok-node-project-org-file--load)))

(defun org-roam-ok-deactivate ()
  "Deactivate `org-roam-ok-mode'."
  (remove-hook 'after-save-hook #'org-roam-ok-node-display--cache-maybe-remove))

;;;###autoload
(define-minor-mode org-roam-ok-mode
  "The `org-roam-ok-mode' minor mode."
  :global nil
  :group 'org-roam-ok-mode
  (if org-roam-ok-mode
      (org-roam-ok-activate)
    (org-roam-ok-deactivate)))

(provide 'org-roam-ok)
;;; org-roam-ok.el ends here
