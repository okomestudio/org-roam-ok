;;; org-roam-plugin-ok.el --- Org Roam Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.2
;; Keywords: org-mode, roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.7") (org-roam "2.2.2") (adaptive-wrap "0.8") (dash "2.13") (marginalia "1.6") (ok-plural "0.1") (org-roam-timestamps "1.0.0") (s "1.13.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The `org-roam-plugin-ok' mode is a plugin to enhance `org-roam' in
;; several ways. See the repository README for detail.
;;
;;; Code:

(defgroup org-roam-plugin-ok nil
  "Group for `org-roam-plugin-ok'."
  :group 'org
  :prefix "org-roam-plugin-ok-")

(defvar org-roam-plugin-ok-version "0.1"
  "Package version.")

(defcustom org-roam-plugin-ok-on-idle-delay 60
  "Idle delay before running on-idle initializer."
  :group 'org)

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
        (run-with-idle-timer org-roam-plugin-ok-on-idle-delay
                             nil
                             #'org-roam-plugin-ok--init-on-idle)))

;;;###autoload
(defun org-roam-plugin-ok-on-idle-init-setup ()
  "Set up on-idle initializer."
  (org-roam-plugin-ok--on-idle-init-scheduler)
  (add-hook 'post-command-hook #'org-roam-plugin-ok--on-idle-init-scheduler))

(defun org-roam-plugin-ok-activate ()
  "Activate `org-roam-plugin-ok-mode'."
  (require 'orp-ok-utils)
  (require 'orp-ok-capture)
  (require 'orp-ok-mode)
  (require 'orp-ok-node)
  (with-eval-after-load 'org-roam-gt
    (require 'orp-ok-node-gt))
  (require 'orp-ok-ja)

  (advice-add #'org-roam-node-find
              :around #'orp-ok-node-project-org-file--load)
  (advice-add #'org-roam-node-insert
              :around #'orp-ok-node-project-org-file--load))

(defun org-roam-plugin-ok-deactivate ()
  "Deactivate `org-roam-plugin-ok-mode'."
  nil)

;;;###autoload
(define-minor-mode org-roam-plugin-ok-mode
  "The `org-roam-plugin-ok-mode' minor mode."
  :global nil
  :group 'org-roam-plugin-ok-mode
  (if org-roam-plugin-ok-mode
      (org-roam-plugin-ok-activate)
    (org-roam-plugin-ok-deactivate)))

(provide 'org-roam-plugin-ok)
;;; org-roam-plugin-ok.el ends here
