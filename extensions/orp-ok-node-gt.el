;;; orp-ok-node-gt.el --- Plugin for org-roam-gt  -*- lexical-binding: t -*-
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
;; This module provides a plugin for `org-roam-gt'.
;;
;;; Code:

(require 'ok)
(require 'org-roam)
(require 'org-roam-gt)
(require 'orp-ok-node)

(defcustom oong-use-cache-in-memory t
  "Set non-nil to use in-memory cache, set nil to disable it."
  :group 'org-roam-plugin-ok)

(defvar oong--cache-in-memory (make-hash-table :test 'equal)
  "In-memory cache.")

(defun oong--cache-in-memory-get (node)
  "Get the cached item for NODE from the in-memory cache."
  (when oong-use-cache-in-memory
    (let* ((node-id (org-roam-node-id node))
           (node-title (org-roam-node-title node))
           (entries (cadr (gethash node-id oong--cache-in-memory))))
      (cdr (assoc node-title entries)))))

(defun oong--cache-in-memory-save (node &rest rest)
  "Save the NODE item in REST to the in-memory cache."
  (when oong-use-cache-in-memory
    (let* ((node-id (org-roam-node-id node))
           (node-title (org-roam-node-title node))
           (entries (cadr (gethash node-id oong--cache-in-memory))))
      (push `(,node-title . ,rest) entries)
      (puthash node-id `(,(float-time) ,entries) oong--cache-in-memory)))
  rest)

(defun oong--cache-in-memory-remove (node)
  "Remove NODE item from the in-memory cache."
  (remhash (org-roam-node-id node) oong--cache-in-memory))

(defun oong--recompute-display (total-width node)
  "Recompute display of NODE with TOTAL-WIDTH."
  (let* ((title (orp-ok-node--title node))
         (tags (or (orp-ok-node--tags node) nil))
         (tags (string-join
                (mapcar (lambda (s)
                          (let (r)
                            (setq r (concat ":" s ":"))
                            (put-text-property 0 1 'invisible t r)
                            (put-text-property (- (length r) 1) (length r)
                                               'invisible t r)
                            (setq r (propertize r 'face 'org-modern-tag))))
                        tags)
                " "))
         (timestamp (orp-ok-node--timestamp node))

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

(defun oong-display-template (node)
  "Callback for `org-roam-gt-node-display-template' for NODE."
  (let* ((frame-width (frame-width))
         (cached (oong--cache-in-memory-get node))
         (total-width (or (car cached) -1)))
    (cons (cadr
           (if (= frame-width total-width)
               cached
             (oong--cache-in-memory-save
              node frame-width (oong--recompute-display frame-width node))))
          node)))

(setq org-roam-gt-node-display-template
      #'oong-display-template
      ;; Or use string template, e.g.,
      ;; (concat "​​​​​${orp-title:*} "
      ;;         (propertize "${orp-tags}" 'face 'org-tag)
      ;;         " ${orp-timestamp:10}")
      )

(provide 'orp-ok-node-gt)

;; Local Variables:
;; read-symbol-shorthands: (("oong" . "orp-ok-node-gt"))
;; End:
;;; orp-ok-node-gt.el ends here
