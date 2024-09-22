;;; orp-ok-node.el --- Plugin for org-roam-node  -*- lexical-binding: t -*-
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
;; This module provides a plugin for `org-roam-node'.
;;
;;; Code:

(require 'orp-ok-utils)
(require 'marginalia)
(require 'org-roam-node)
(require 'org-roam-timestamps)

(defcustom oon-use-cache-in-memory t
  "Set non-nil to use in-memory cache, set nil to disable it."
  :group 'org-roam-plugin-ok)

(defcustom oon-project-org-file nil
  "Org file visited to load directory local variables."
  :group 'org-roam-plugin-ok)

;;; The directory local variables loader
(defun oon-project-org-file--load (orig-func)
  "Load dir local variables before ORIG-FUNC.
Use as an around advice for `org-roam-node-find' to load relevant
project directory local variables prior to calling it
interactively. This is useful when the `org-roam-node-find'
function is run outside the context of `org-roam-directory'."
  (if (or (org-roam-file-p)
          (null oon-project-org-file))
      (call-interactively orig-func)
    (let* ((file oon-project-org-file)
           (find-file-hook (remq 'recentf-track-opened-file find-file-hook))
           (buffer-existed (get-file-buffer file))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (unwind-protect
            (call-interactively orig-func)
          (if (null buffer-existed)
              (kill-buffer buffer)))))))

;;; The parent property
(defun oon--get-parent-property (node)
  "Get the parent property of NODE."
  (let* ((prop (org-roam-node-properties node))
         (parent (cdr (assoc-string "PARENT" prop)))
         (extracted (and parent
                         (replace-regexp-in-string
                          "\\[\\[id:\\(.+\\)\\]\\[\\([^]]+\\)\\]\\]"
                          "\\1"
                          parent))))
    (cond ((and parent (equal parent extracted))
           (message "WARNING: Malformed parent (%s) in %s"
                    parent
                    (org-roam-node-file node))
           nil)
          (t extracted))))

(defun oon--get-parent (&optional node)
  "Get the parent of NODE if it exists."
  (cdr (assoc-string "PARENT"
                     (org-roam-node-properties
                      (or node
                          (org-roam-node-at-point))))))

(defun oon-show-parent (node)
  "Show the parent of NODE if exists."
  (interactive "P")
  (let* ((parent (oon--get-parent node)))
    (if parent
        (message "Parent: %s" parent)
      (message "No parent found"))))

(defun oon-visit-parent (node)
  "Visit parent of given NODE at point, if exists."
  (interactive "P")
  (let ((parent (oon--get-parent node)))
    (if parent
        (org-link-open-from-string parent)
      (message "No parent found"))))

;;; The org-roam cache layer (sqlite)
(defun oon--all-node-ids-within-file (file)
  "Get the IDs of all nodes within FILE."
  (mapcar 'car
          (org-roam-db-query `[:select :distinct nodes:id
                                       :from nodes
                                       :where (and (= nodes:file ,file))])))

(defun oon--file-node-id (node)
  "Get the ID of the top-level file node of NODE."
  (let ((file (org-roam-node-file node)))
    (caar (org-roam-db-query `[:select nodes:id :from nodes
                                       :where (and (= nodes:file ,file)
                                                   (= nodes:level 0))]))))

(defun oon--all-file-nodes-and-ids ()
  (org-roam-db-query '[:select [nodes:file nodes:id]
                               :from nodes
                               :where (= nodes:level 0)]))

(defun oon--all-nodes ()
  (org-roam-db-query '[:select nodes:id :from nodes]))

;;; In-memory cache
;;
;; This caching layer exists to speed up the interactive node query.
;;
;; TODO: Make the implementation memory efficient.

(defvar oon--cache-in-memory-file (make-hash-table :test 'equal)
  "In-memory cache, mapping a file to the ID of its top-level node.")

(defun oon--cache-in-memory-file-get (file)
  (when oon-use-cache-in-memory
    (gethash file oon--cache-in-memory-file)))

(defun oon--cache-in-memory-file-save (file node-id)
  (when oon-use-cache-in-memory
    (puthash file node-id oon--cache-in-memory-file)))

(defun oon--cache-in-memory-file-remove (file)
  (remhash file oon--cache-in-memory-file))

(defun oon--cache-in-memory-file-fill ()
  (dolist (row (oon--all-file-nodes-and-ids))
    (oon--cache-in-memory-file-save (car row) (cadr row))))

(defvar oon--cache-in-memory (make-hash-table :test 'equal)
  "In-memory cache, mapping a node ID to its node.")

(defun oon--cache-in-memory-get (node-id)
  "Get the node for NODE-ID from the in-memory cache."
  (when oon-use-cache-in-memory
    (cdr (gethash node-id oon--cache-in-memory))))

(defun oon--cache-in-memory-save (node)
  "Save NODE to the in-memory cache."
  (let* ((node-id (org-roam-node-id node))
         (level (org-roam-node-level node))
         (file-node-id (when (< 0 level)
                         (let* ((file (org-roam-node-file node))
                                (id (oon--cache-in-memory-file-get file)))
                           (if id
                               id
                             (setq id (oon--file-node-id node))
                             (oon--cache-in-memory-file-save file id)
                             id))))
         (file-parent-node-id (when (= 0 level)
                                (oon--get-parent-property node)))
         (item `(,node ,file-node-id ,file-parent-node-id)))
    (when oon-use-cache-in-memory
      (puthash node-id `(,(float-time) ,@item) oon--cache-in-memory))
    item))

(defun oon--cache-in-memory-remove (node-id)
  "Save NODE to the in-memory cache."
  (remhash node-id oon--cache-in-memory))

(defun oon--cache-in-memory-fill ()
  (dolist (row (oon--all-nodes))
    (oon--from-id (car row))))

(defun oon--cache-in-memory-maybe-remove ()
  (let ((file buffer-file-name))
    (when (string= (file-name-extension file) "org")
      (oon--cache-in-memory-file-remove file)
      (dolist (node-id (oon--all-node-ids-within-file file))
        (oon--cache-in-memory-remove node-id)))))

(add-hook 'after-save-hook #'oon--cache-in-memory-maybe-remove)

;;; Node utility functions

(defun oon--from-id (node-id)
  "Get the node with NODE-ID either from in-memory cache or Org cache."
  (let ((node (car (oon--cache-in-memory-get node-id))))
    (if node
        node
      (setq node (org-roam-node-from-id node-id))
      (oon--cache-in-memory-save node)
      node)))

(defun oon--file-node-from-id (node-id)
  (let ((node-and-parent-id (oon--cache-in-memory-get node-id)))
    (when (not node-and-parent-id)
      (setq node-and-parent-id (oon--cache-in-memory-save (org-roam-node-from-id node-id))))
    (let ((parent-node-id (cadr node-and-parent-id)))
      (when parent-node-id
        (oon--from-id parent-node-id)))))

(defun oon--parent-titles (node)
  "Get the title of the file NODE's parent."
  (let* ((parent-id (caddr (oon--cache-in-memory-get (org-roam-node-id node))))
         (visited nil)
         titles)
    (catch 'circular
      (while parent-id
        (if (member parent-id visited)
            (progn
              (message "orp-ok-node: Circular nodes detected!")
              (throw 'circular nil)))
        (setq node (oon--from-id parent-id))
        (setq titles (append titles `(,(org-roam-node-title node))))
        (setq visited (append visited `(,parent-id)))
        (setq parent-id (caddr (oon--cache-in-memory-get parent-id))))
      titles)))

(defun oon--title-aux-get (node)
  "Get the auxiliary title info for NODE."
  (let ((node-id (org-roam-node-id node))
        (title-or-alias (org-roam-node-title node))
        (alias-delimiter " = ")
        (section-delimiter " ❬ ")
        sections)
    (if (member title-or-alias (org-roam-node-aliases node))
        ;; `node' may have its title replaced with an alias, so pull
        ;; the title from the original node:
        (list alias-delimiter (org-roam-node-title (oon--from-id node-id)))
      (setq sections
            (pcase (org-roam-node-level node)  ; 0 for file-level node
              (0 (oon--parent-titles (oon--from-id node-id)))
              (_ (or (oon--parent-titles (oon--file-node-from-id node-id))
                     `(,(org-roam-node-file-title node))))))
      (flatten-list (mapcar (lambda (x) `(,section-delimiter ,x)) sections)))))

(defun oon--title-aux-render (title-aux)
  (if (not title-aux)
      ""
    (let ((face-sym `(:foreground ,(face-attribute 'completions-annotations
                                                   :foreground)))
          (face-aux `(:foreground ,(face-attribute 'completions-annotations
                                                   :foreground)
                                  :slant italic))
          rendered)
      (while title-aux
        (let ((sym (car title-aux))
              (aux (cadr title-aux)))
          (setq title-aux (cddr title-aux))
          (setq rendered (concat rendered
                                 (propertize sym 'face face-sym)
                                 (propertize aux 'face face-aux)))))
      rendered)))

;;; Public functions and methods

;; Node attribute accessors

(defun oon--title (node)
  (concat (org-roam-node-title node)
          (oon--title-aux-render (oon--title-aux-get node))))

(cl-defmethod org-roam-node-orp-title ((node org-roam-node))
  (oon--title node))

(defun oon--tags (node)
  (let ((tags (if (eq 0 (org-roam-node-level node))
                  ;; File-level node
                  (org-roam-node-tags node)
                ;; Non-file-level node
                (cl-set-difference (org-roam-node-tags node)
                                   org-tags-exclude-from-inheritance
                                   :test 'equal))))
    tags))

(cl-defmethod org-roam-node-orp-tags ((node org-roam-node))
  (format "#%s#" (string-join (oon--tags node) "#")))

(defun oon--timestamp (node)
  (let* ((inhibit-message t)
         (mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
         (mtime (if mtime
                    (org-roam-timestamps-encode (car (split-string mtime)))
                  (org-roam-node-file-mtime node))))
    (format-time-string "%Y-%m-%d" mtime)))

(cl-defmethod org-roam-node-orp-timestamp ((node org-roam-node))
  (oon--timestamp node))

(defun oon--timestamp-marginalia (node)
  (let* ((inhibit-message t)
         (mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
         (mtime (if mtime
                    (org-roam-timestamps-encode (car (split-string mtime)))
                  (org-roam-node-file-mtime node))))
    (marginalia--time mtime)))

(cl-defmethod org-roam-node-orp-timestamp-marginalia ((node org-roam-node))
  (oon--timestamp-marginalia node))

(defun oon--slug (node)
  (orp-ok-string-to-org-slug (org-roam-node-title node)))

(with-eval-after-load 'org-roam-node
  ;; NOTE: To ensure *override*, need to eval after `org-roam-node' gets loaded
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (oon--slug node)))

;;; Misc.

(defun oon-fill-caches ()
  "Fill all caches."
  (message "Running orp-ok-node-fill-caches...")
  (oon--cache-in-memory-fill)
  (oon--cache-in-memory-file-fill))

(provide 'orp-ok-node)

;; Local Variables:
;; read-symbol-shorthands: (("oon" . "orp-ok-node"))
;; End:
;;; orp-ok-node.el ends here
