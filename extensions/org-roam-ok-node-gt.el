;;; org-roam-ok-node-gt.el --- Plugin for org-roam-gt  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
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
;; This module provides a customization/plugin for `org-roam-gt'.
;;
;;; Code:

(require 'ok)
(require 'org-roam)
(require 'org-roam-gt)
(require 'org-roam-ok-node)

(defcustom orong-use-cache-in-memory t
  "Set non-nil to use in-memory cache, set nil to disable it."
  :group 'org-roam-plugin-ok)

(defvar orong--cache-in-memory (make-hash-table :test 'equal)
  "In-memory cache.")

(defun orong--cache-in-memory-get (node)
  "Get the cached item for NODE from the in-memory cache."
  (when orong-use-cache-in-memory
    (let* ((node-id (org-roam-node-id node))
           (node-title (org-roam-node-title node))
           (entries (cadr (gethash node-id orong--cache-in-memory))))
      (cdr (assoc node-title entries)))))

(defun orong--cache-in-memory-save (node &rest rest)
  "Save the NODE item in REST to the in-memory cache."
  (when orong-use-cache-in-memory
    (let* ((node-id (org-roam-node-id node))
           (node-title (org-roam-node-title node))
           (entries (cadr (gethash node-id orong--cache-in-memory))))
      (push `(,node-title . ,rest) entries)
      (puthash node-id `(,(float-time) ,entries) orong--cache-in-memory)))
  rest)

(defun orong--cache-in-memory-remove (node)
  "Remove NODE item from the in-memory cache."
  (remhash (org-roam-node-id node) orong--cache-in-memory))

(defun orong--cache-in-memory-clear ()
  "Clear the in-memory cache."
  (interactive)
  (clrhash orong--cache-in-memory))

(defun orong--cache-in-memory-maybe-remove ()
  (when orong-use-cache-in-memory
    (let ((file buffer-file-name))
      (when (string= (file-name-extension file) "org")
        (let* ((node (org-roam-node-at-point t)))
          (orong--cache-in-memory-remove node))))))

(add-hook 'after-save-hook #'orong--cache-in-memory-maybe-remove)

(defun orong--recompute-display (total-width node)
  "Recompute display of NODE with TOTAL-WIDTH."
  (let* ((title (concat (and (featurep 'org-roam-fz) (org-roam-node-fid node))
                        (org-roam-ok-node--title node)))
         (tags (or (org-roam-ok-node--tags node) nil))
         (tags (string-join
                (mapcar (lambda (s)
                          (let (r)
                            (setq r (concat ":" s ":"))
                            (put-text-property 0 1 'invisible t r)
                            (put-text-property (- (length r) 1) (length r)
                                               'invisible t r)
                            (setq r (propertize r 'face 'highlight))))
                        tags)
                " "))
         (timestamp (org-roam-ok-node--timestamp node))

         (multibyte-scale 1.7)
         (tag-scale 1.0)

         (timestamp-width (1+ (length timestamp)))
         (title-and-tags-width (- total-width timestamp-width))
         (tags-width (1+ (round (* (length tags) tag-scale))))
         (max-title-width (- title-and-tags-width tags-width))
         (title-width (ok-string-multibyte-string-width title multibyte-scale)))
    (if (>= title-width max-title-width)
        (let ((thresh (length (ok-string-multibyte-substring
                               title 0 max-title-width multibyte-scale))))
          (put-text-property 0 (1- thresh) 'invisible nil title)
          (put-text-property thresh (length title) 'invisible t title))
      (put-text-property 0 (length title) 'invisible nil title)
      (setq title (concat title
                          (make-string (- max-title-width title-width) ?\ ))))
    (concat title
            " " tags
            " " timestamp)))

(defun orong-display-template (node)
  "Callback for `org-roam-gt-node-display-template' for NODE."
  (let* ((frame-width (frame-width))
         (cached (orong--cache-in-memory-get node))
         (total-width (or (car cached) -1)))
    (cons (cadr
           (if (= frame-width total-width)
               cached
             (orong--cache-in-memory-save
              node frame-width (orong--recompute-display frame-width node))))
          node)))

(setq org-roam-gt-node-display-template
      #'orong-display-template
      ;; Or use string template, e.g.,
      ;; (concat "​​​​​${ok-title:*} "
      ;;         (propertize "${ok-tags}" 'face 'org-tag)
      ;;         " ${ok-timestamp:10}")
      )

(provide 'org-roam-ok-node-gt)

;; Local Variables:
;; read-symbol-shorthands: (("orong" . "org-roam-ok-node-gt"))
;; End:
;;; org-roam-ok-node-gt.el ends here
