;;; org-roam-plugin-ok.el --- Org Roam Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.4") (dash "2.13") (adaptive-wrap "0.8"))
;;
;;; Commentary:
;;
;; This is the top-level module.
;;
;;; Code:

(defgroup org-roam-plugin-ok nil
  "Group for `org-roam-plugin-ok'."
  :group 'org
  :prefix "org-roam-plugin-ok-")

(defvar org-roam-plugin-ok-version "0.1"
  "Package version.")

;; On-idle initialization

(defvar org-roam-plugin-ok--on-idle-timer nil
  "Timer object used by on-idle init scheduler.")

(defun org-roam-plugin-ok--init-on-idle ()
  "Initialize `org-roam' on idle."
  (when org-roam-plugin-ok--on-idle-timer
    (cancel-timer org-roam-plugin-ok--on-idle-timer))
  (remove-hook 'post-command-hook #'org-roam-plugin-ok--on-idle-init-scheduler)
  (when (not (and (boundp 'orp-ok-node--cache-in-memory)
                  (> (hash-table-count orp-ok-node--cache-in-memory) 0)))
    (message "Starting org-roam-init-on-idle...")
    (require 'org-roam)
    (org-roam-node-list)

    (org-roam-plugin-ok-mode 1)
    (orp-ok-node-fill-caches)
    (message "Finished org-roam-init-on-idle")))

(defun org-roam-plugin-ok--on-idle-init-scheduler ()
  "Schedule on-idle initializer."
  (when org-roam-plugin-ok--on-idle-timer
    (cancel-timer org-roam-plugin-ok--on-idle-timer))
  (setq org-roam-plugin-ok--on-idle-timer
        (run-with-idle-timer 10 nil #'org-roam-plugin-ok--init-on-idle)))

;;;###autoload
(defun org-roam-plugin-ok-on-idle-init-setup ()
  "Set up on-idle initializer."
  (org-roam-plugin-ok--on-idle-init-scheduler)
  (add-hook 'post-command-hook #'org-roam-plugin-ok--on-idle-init-scheduler))

;;;###autoload
(define-minor-mode org-roam-plugin-ok-mode
  "The `org-roam-plugin-ok-mode' minor mode."
  :global nil
  :group 'org-roam-plugin-ok-mode
  (when org-roam-plugin-ok-mode
    (require 'orp-ok-utils)
    (require 'orp-ok-capture)
    (require 'orp-ok-mode)
    (require 'orp-ok-node)
    (require 'orp-ok-ja)
    (require 'orp-ok-org)))

(provide 'org-roam-plugin-ok)
;;; org-roam-plugin-ok.el ends here
