;;; org-roam-ok-ref.el --- References Utilities -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The enhancements for citation reference (via `org-ref', for now).
;;
;;; Code:

(require 'org-roam)

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
  (interactive (list (org-ref-read-key)))
  (let ((buffer (generate-new-buffer
                 (format "%s<%s>" org-roam-ok-ref-search-buffer key))))
    (switch-to-buffer buffer)
    (org-roam-ok-ref-search-buffer--render key)
    (beginning-of-buffer)))

(provide 'org-roam-ok-ref)
;;; org-roam-ok-ref.el ends here
