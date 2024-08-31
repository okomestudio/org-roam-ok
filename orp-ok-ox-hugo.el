;;; orp-ok-ox-hugo.el --- ox-hugo plug-in  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.4") (dash "2.13") (adaptive-wrap "0.8") (s "1.13.1"))
;;
;;; Commentary:
;;
;; This `ox-hugo' plug-in is intended for Hugo articles written as Org
;; file. Articles written as Org subtrees are not supported.
;;
;;; Code:

(require 'orp-ok-utils)

(defcustom orp-ok-ox-hugo-content-tag "blog"
  "The file tag used for Hugo contents.")

(defcustom orp-ok-ox-hugo-draft-state-tags '((true . '("wip")) (false . '("posted")))
  "The file tags used for the HUGO_DRAFT keyword.")

(with-eval-after-load 'ox-hugo
  (defun orp-ok-ox-hugo-content-metadata-update (&optional subtreep)
    "Add keywords to prepare the current buffer for Hugo export."
    (interactive)
    (let ((filetags (cl-remove-if (lambda (a) (string= a ""))
                                  (split-string (org-roam-get-keyword "filetags")
                                                ":"))))
      (if (not (member orp-ok-ox-hugo-content-tag filetags))
          (error "A note without the '%s' tag is not Hugo-exportable."
                 orp-ok-ox-hugo-content-tag)
        (save-excursion
          (beginning-of-buffer)
          (let* ((node (org-roam-node-at-point))
                 (title (org-roam-node-title node))
                 (slug (orp-string-to-org-slug title))
                 (time (current-time))
                 (date (format-time-string "%Y-%m-%d" time))
                 (section (format-time-string "%Y/%m" time))
                 (tags (org-roam-node-tags node))
                 (non-hugo-tags (append
                                 `(,orp-ok-ox-hugo-content-tag)
                                 (flatten-list (mapcar 'cddr
                                                       orp-ok-ox-hugo-draft-state-tags))))
                 (hugo-tags (seq-filter (lambda (t)
                                          (not (member t non-hugo-tags)))
                                        tags)))
            (dolist (it `(("hugo_tags" . ,(string-join hugo-tags " "))
                          ("hugo_section" . ,section)
                          ("hugo_draft" . "true")
                          ("hugo_custom_front_matter" . ":archived false")
                          ("hugo_bundle" . ,slug)
                          ("hugo_base_dir" . ,org-hugo-base-dir)
                          ("hugo_auto_set_lastmod" . "t")
                          ("export_hugo_bundle" . ,slug)
                          ("export_file_name" . "index")
                          ("date" . ,date)
                          ("author" . ,user-login-name)))
              (let ((keyword (car it))
                    (value (cdr it)))
                (if (not (org-roam-get-keyword keyword))
                    (org-roam-set-keyword keyword value)))))))))

  (advice-add #'org-hugo--before-export-function :before #'orp-ok-ox-hugo-content-metadata-update)

  (defun org-hugo-link--strip-id-link (fun link desc info)
    "Render Org Roam links with their description."
    (let ((type (org-element-property :type link))
          (tags (org-roam-get-keyword "filetags")))
      (cond ((string= type "id")
             (or (and (cl-search (format ":%s:" orp-ok-ox-hugo-article-tag) tags)
                      desc)
                 (funcall fun link desc info)))
            (t (funcall fun link desc info)))))

  (advice-add #'org-hugo-link :around #'org-hugo-link--strip-id-link)

  (defun orp-ox-hugo-backend-menu-item (menu-item)
    "Add a MENU-ITEM to Hogo's backend."
    (dolist (backend org-export-registered-backends)
      (when (eq 'hugo (org-export-backend-name backend))
        (let ((menu-items (caddr (org-export-backend-menu backend))))
          (setq menu-items (append menu-items '(menu-item)))
          (setf (caddr (org-export-backend-menu backend)) menu-items))))))

(provide 'orp-ok-ox-hugo)
;;; orp-ok-ox-hugo.el ends here
