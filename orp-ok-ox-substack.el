;;; orp-ok-ox-substack.el --- Substack Export Plugin  -*- lexical-binding: t -*-
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
