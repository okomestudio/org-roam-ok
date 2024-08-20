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
;; This is the top-level module. Load all plugin modules here.
;;
;;; Code:

(require 'orp-ok-utils)
(require 'orp-ok-node)
(require 'orp-ok-ja)

;;;###autoload
(defun orp-ok-version ()
  (interactive)
  (message "0.1"))

(provide 'org-roam-plugin-ok)
;;; org-roam-plugin-ok.el ends here
