;;; orp-ok-init.el --- Plugin for org-roam initialization  -*- lexical-binding: t -*-
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
;; This module provides a plugin for `org-roam' initialization.
;;
;;; Code:

(with-eval-after-load 'org-roam-mode
  (defun orp-ok-init--node-list-refresh ()
    "Refresh node list to speed up the initial node finding."
    (message "Running orp-ok-init--node-list-refresh...")
    (org-roam-node-list)
    ;; Commented out since title construction appears to break:
    ;; (org-roam-node-read--completions)
    )

  (run-with-idle-timer 15 nil #'orp-ok-init--node-list-refresh))

(provide 'orp-ok-init)
;;; orp-ok-init.el ends here
