;;; orp-ok-mode.el --- Plugin for org-roam-mode  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
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
;; This module provides a plugin for `org-roam-mode'.
;;
;;; Code:

(require 'org-roam)

(defun orp-ok-create-missing-directories (file)
  "Create parent directories of FILE if missing."
  (let* ((file (expand-file-name file))
         (parent-directory (file-name-directory file)))
    (and (not (file-remote-p file))
         (string-prefix-p org-roam-directory file)
         (not (file-directory-p parent-directory))
         (make-directory parent-directory 'parents))))

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
            :before #'rename-file--ok-create-missing-directories)

(provide 'orp-ok-mode)
;;; orp-ok-mode.el ends here
