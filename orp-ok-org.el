;;; orp-ok-org.el --- Org mode  -*- lexical-binding: t -*-
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
;;; Code:

(require 'org)

(defun orp-ok-org-add-properties (props)
  "Add properties.
PROPS is a list of cons cells (keyword . values). When values is
a list, its values are used as fixed values for the preset
properties."
  (dolist (prop props)
    (let ((keyword (car prop))
          (values (cdr prop)))
      (when values
        (add-to-list 'org-global-properties-fixed
                     `(,(concat keyword "_ALL")
                       .
                       ,(mapconcat (lambda (s)
                                     (format "\"%s\"" s))
                                   values
                                   " "))))
      (add-to-list 'org-default-properties keyword))))

;;; org-src

(defun orp-ok-org-src-skip-noweb-refs-on-format (func &rest rest)
  "Comment out noweb references in Org source edit buffer."
  (interactive)
  (let ((noweb-ref-re "<<\\([A-Za-z0-9-_]+\\)>>"))
    (when (org-src-edit-buffer-p)
      (save-excursion
        (beginning-of-buffer)
        (while (re-search-forward (concat "^" noweb-ref-re) nil t)
          (replace-match (concat comment-start "<<\\1>>")))))

    (call-interactively func rest)

    (when (org-src-edit-buffer-p)
      (save-excursion
        (beginning-of-buffer)
        (while (re-search-forward (concat "^" comment-start noweb-ref-re) nil t)
          (replace-match "<<\\1>>"))))))

(with-eval-after-load 'ruff-format
  (advice-add #'ruff-format-buffer :around 'orp-ok-org-src-skip-noweb-refs-on-format))

(provide 'orp-ok-org)
;;; orp-ok-org.el ends here
