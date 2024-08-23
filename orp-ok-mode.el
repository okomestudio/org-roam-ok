;;; orp-ok-mode.el --- Plugin for org-roam-mode  -*- lexical-binding: t -*-
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
;; This module provides a plugin for `org-roam-mode'.
;;
;;; Code:

(defun orp-ok-create-missing-directories (FILE)
  "Create parent directories of FILE if missing."
  (let ((parent-directory (file-name-directory FILE)))
    (and (not (file-directory-p parent-directory))
         (make-directory parent-directory 'parents))))

(with-eval-after-load 'org-roam
  (defun orp-ok-mode--create-missing-parent-directories ()
    "Automatically create missing directories when creating new files.
This hook removes the prompt for the directory creation during
the Org capture."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (string-prefix-p org-roam-directory buffer-file-name)
             (progn
               (make-directory parent-directory 'parents)
               t)))))

  (add-hook 'find-file-not-found-functions
            #'orp-ok-mode--create-missing-parent-directories)

  (defun rename-file--create-missing-directories-before
      (_ NEWNAME &optional _)
    "Create missing directories if missing before `rename-file'."
    (let ((newname (expand-file-name NEWNAME)))
      (unless (file-remote-p newname)
        (and (string-prefix-p org-roam-directory newname)
             (orp-ok-create-missing-directories newname)))))

  (advice-add 'rename-file
              :before #'rename-file--create-missing-directories-before))

(provide 'orp-ok-mode)
;;; orp-ok-mode.el ends here
