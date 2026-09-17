;;; org-roam-ok-ref.el --- References Utilities -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2026 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The enhancements for citation reference (via `org-ref', for now).
;;
;;; Code:

(require 'org-roam)
(require 'org-ref)

(defun org-roam-ok-ref-at-point ()
  "Return the reference key if point is on an `org-ref` cite link."
  (when (derived-mode-p 'org-mode)
    (when-let* ((link (org-element-lineage (org-element-context) '(link) t))
                (type (org-element-property :type link))
                (_ (assoc type org-ref-cite-types #'string=))
                (path (org-element-property :path link)))
      (save-match-data
        (when (string-match "&\\(\\S-+\\)" path)
          (match-string 1 path))))))

(defun org-roam-ok-ref--filter-find-args (args)
  "Filter ARGS for `org-roam-ref-find` to inject initial input from point."
  (cl-destructuring-bind (&optional initial-input filter-fn) args
    (list (or initial-input (org-roam-ok-ref-at-point))
          filter-fn)))

(advice-add #'org-roam-ref-find :filter-args #'org-roam-ok-ref--filter-find-args)

(defun org-roam-ok-ref-reflinks-get (key)
  "Return the reflinks for citation KEY."
  (let ((refs (org-roam-db-query
               [ :select :distinct [ citations:cite-key citations:node-id
                                     citations:pos citations:properties ]
                 :from citations
                 :where (= citations:cite-key $s1)
                 :union
                 :select :distinct [refs:ref refs:node-id 0 ""]
                 :from refs
                 :where (= refs:ref $s1) ]
               key))
        links)
    (pcase-dolist (`(,ref ,source-id ,pos ,properties) refs)
      (push (org-roam-populate
             (org-roam-reflink-create
              :source-node (org-roam-node-create :id source-id)
              :ref ref
              :point pos
              :properties properties))
            links))
    links))

(defun org-roam-ok-ref-search-buffer--render (key)
  "Render the ref search buffer for citation KEY."
  (when-let* ((reflinks (seq-sort #'org-roam-reflinks-sort
                                  (org-roam-ok-ref-reflinks-get key)))
              (inhibit-read-only t))
    (org-roam-mode)
    (org-roam-buffer-set-header-line-format key)
    (magit-insert-section (org-roam-reflink)
      (magit-insert-heading (format "Reflinks (%d):" (length reflinks)))
      (dolist (reflink reflinks)
        (org-roam-node-insert-section
         :source-node (org-roam-reflink-source-node reflink)
         :point (org-roam-reflink-point reflink)
         :properties (org-roam-reflink-properties reflink)))
      (insert ?\n))))

(defvar org-roam-ok-ref-search-buffer "*org-roam-ref-search*")

;;;###autoload
(defun org-roam-ok-ref-search-buffer (key)
  "Open the ref search buffer for citation KEY."
  (interactive (list (or (org-roam-ok-ref-at-point) (org-ref-read-key))))
  (let ((buffer (generate-new-buffer
                 (format "%s<%s>" org-roam-ok-ref-search-buffer key))))
    (switch-to-buffer buffer)
    (org-roam-ok-ref-search-buffer--render key)
    (beginning-of-buffer)))

(provide 'org-roam-ok-ref)
;;; org-roam-ok-ref.el ends here
