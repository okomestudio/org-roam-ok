;;; orp-ok-ox-substack.el --- Substack Export Plugin  -*- lexical-binding: t -*-
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
;; This module provides a plugin for Substack export.
;;
;;; Code:

(with-eval-after-load 'ox
  (defun ok-org--org-html-link (link desc info)
    (let* ((raw-link (org-element-property :raw-link link))
           (raw-path (org-element-property :path link))
           (type (org-element-property :type link))
           (link-is-url (member type '("http" "https" "ftp" "mailto")))
           (desc (org-string-nw-p desc)))
      (if link-is-url
          (format "<a href=\"%s\">%s</a>" raw-link (or desc raw-link))
        (if (string= (substring raw-link 0 3) "id:")
            desc
          (if (member (file-name-extension raw-link)
                      '("gif" "jpeg" "jpg" "png" "webp"))
              (format "<img src=\"%s\" />" raw-link)
            (format "<a href=\"%s\">%s</a>" raw-link desc))))))

  (org-export-define-derived-backend
      'substack 'html
    :menu-entry
    '(?S "Export to Substack article"
         ((?o "As HTML file and open"
	            (lambda (a s v b)
	              (if a
                    (org-export-to-buffer t s v b)
                  (let ((f (concat (file-name-sans-extension buffer-file-name)
                                   ".html")))
                    (org-open-file (org-export-to-file 'substack f nil s v b))))))))
    :translate-alist '((link . ok-org--org-html-link))))

(provide 'orp-ok-ox-substack)
;;; orp-ok-ox-substack.el ends here
