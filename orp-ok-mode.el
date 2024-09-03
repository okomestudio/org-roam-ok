;;; orp-ok-mode.el --- Plugin for org-roam-mode  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.4") (org-roam "2.2.2"))
;;
;;; Commentary:
;;
;; This module provides a plugin for `org-roam-mode'.
;;
;;; Code:

(defun orp-ok-create-missing-directories (file)
  "Create parent directories of FILE if missing."
  (let* ((file (expand-file-name file))
         (parent-directory (file-name-directory file)))
    (and (not (file-remote-p file))
         (string-prefix-p org-roam-directory file)
         (not (file-directory-p parent-directory))
         (make-directory parent-directory 'parents))))

(with-eval-after-load 'org-roam
  (defun orp-ok-mode--create-missing-directories ()
    "Automatically create missing directories when creating a file.

This hook removes the prompt for the directory creation during,
e.g., Org capture."
    (let ((file buffer-file-name))
      (orp-ok-create-missing-directories file)))

  (add-hook 'find-file-not-found-functions
            #'orp-ok-mode--create-missing-directories)

  (defun rename-file--ok-create-missing-directories
      (_ newname &optional _)
    "Create missing directories when creating NEWNAME on `rename-file'."
    (orp-ok-create-missing-directories newname))

  (advice-add 'rename-file
              :before #'rename-file--ok-create-missing-directories))

(provide 'orp-ok-mode)
;;; orp-ok-mode.el ends here
