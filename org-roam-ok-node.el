;;; org-roam-ok-node.el --- Plugin for org-roam-node  -*- lexical-binding: t -*-
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
;; This module provides a plugin for `org-roam-node'.
;;
;;; Code:

(require 'async)
(require 'dash)
(require 'org-roam)
(require 'org-roam-ok-utils)
(require 'marginalia)
(require 'org-roam-node)
(require 'org-roam-timestamps)

(defcustom org-roam-ok-node-use-cache-in-memory t
  "Set non-nil to use in-memory cache, set nil to disable it."
  :group 'org-roam-ok)

(defcustom org-roam-ok-node-project-org-file nil
  "Org file visited to load directory local variables."
  :group 'org-roam-ok)

;;; The directory local variables loader
(defun org-roam-ok-node-project-org-file--load (orig-func &rest rest)
  "Load directory local variables by visiting an Org file.
Use as the around advice for function ORIG-FUNC to load relevant
per-project directory local variables associated with
`org-roam-ok-node-project-org-file' prior to calling the function. The
REST arguments are used for non-interactive invocations. The
purpose is to make a function like `org-roam-node-find' aware of
`org-roam-directory' being set outside of the current context."
  (if (or (org-roam-file-p buffer-file-name)
          (minibufferp)
          (null org-roam-ok-node-project-org-file))
      (cond ((called-interactively-p 'any) (call-interactively orig-func))
            (t (apply orig-func rest)))
    (let* ((find-file-hook (remq 'recentf-track-opened-file find-file-hook))
           (enable-local-variables :all)
           (buffer-existed (get-file-buffer org-roam-ok-node-project-org-file))
           (buffer (find-file-noselect org-roam-ok-node-project-org-file)))
      (with-current-buffer buffer
        (unwind-protect
            (cond ((called-interactively-p 'any) (call-interactively orig-func))
                  (t (apply orig-func rest)))
          (if (null buffer-existed)
              (kill-buffer buffer)))))))

;;; The parent property
(defun org-roam-ok-node--get-parent-property (node)
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

(defun org-roam-ok-node--get-parent (&optional node)
  "Get the parent of NODE if it exists."
  (cdr (assoc-string "PARENT"
                     (org-roam-node-properties
                      (or node
                          (org-roam-node-at-point))))))

(defun org-roam-ok-node-show-parent (node)
  "Show the parent of (file) NODE if exists."
  (interactive "P")
  (save-mark-and-excursion
    (beginning-of-buffer)
    (let* ((parent (org-roam-ok-node--get-parent node)))
      (if parent
          (message "Parent: %s" parent)
        (message "No parent found")))))

(defun org-roam-ok-node-visit-parent (node)
  "Visit parent of given NODE at point, if exists."
  (interactive "P")
  (let ((parent (org-roam-ok-node--get-parent node)))
    (if parent
        (org-link-open-from-string parent)
      (message "No parent found"))))

;;; The org-roam cache layer (sqlite)
(defun org-roam-ok-node--all-node-ids-within-file (file)
  "Get the IDs of all nodes within FILE."
  (mapcar 'car
          (org-roam-db-query `[:select :distinct nodes:id
                                       :from nodes
                                       :where (and (= nodes:file ,file))])))

(defun org-roam-ok-node--file-node-id (node)
  "Get the ID of the top-level file node of NODE."
  (let ((file (org-roam-node-file node)))
    (caar (org-roam-db-query `[:select nodes:id :from nodes
                                       :where (and (= nodes:file ,file)
                                                   (= nodes:level 0))]))))

(defun org-roam-ok-node--all-file-nodes-and-ids ()
  (org-roam-db-query '[:select [nodes:file nodes:id]
                               :from nodes
                               :where (= nodes:level 0)]))

(defun org-roam-ok-node--all-nodes ()
  (org-roam-db-query '[:select nodes:id :from nodes]))

;;; In-memory cache
;;
;; This caching layer exists to speed up the interactive node query.
;;
;; TODO: Make the implementation memory efficient.

(defvar org-roam-ok-node--cache-in-memory-file (make-hash-table :test 'equal)
  "In-memory cache, mapping a file to the ID of its top-level node.")

(defun org-roam-ok-node--cache-in-memory-clear ()
  (interactive)
  (setq org-roam-ok-node--cache-in-memory (make-hash-table :test 'equal)
        org-roam-ok-node--cache-in-memory-file (make-hash-table :test 'equal)))

(defun org-roam-ok-node--cache-in-memory-file-get (file)
  (when org-roam-ok-node-use-cache-in-memory
    (gethash file org-roam-ok-node--cache-in-memory-file)))

(defun org-roam-ok-node--cache-in-memory-file-save (file node-id)
  (when org-roam-ok-node-use-cache-in-memory
    (puthash file node-id org-roam-ok-node--cache-in-memory-file)))

(defun org-roam-ok-node--cache-in-memory-file-remove (file)
  (remhash file org-roam-ok-node--cache-in-memory-file))

(defun org-roam-ok-node--cache-in-memory-file-fill ()
  (dolist (row (org-roam-ok-node--all-file-nodes-and-ids))
    (org-roam-ok-node--cache-in-memory-file-save (car row) (cadr row))))

(defvar org-roam-ok-node--cache-in-memory (make-hash-table :test 'equal)
  "In-memory cache, mapping a node ID to its node.")

(defun org-roam-ok-node--cache-in-memory-get (node-id)
  "Get the node for NODE-ID from the in-memory cache."
  (when org-roam-ok-node-use-cache-in-memory
    (cdr (gethash node-id org-roam-ok-node--cache-in-memory))))

(defun org-roam-ok-node--cache-in-memory-save (node)
  "Save NODE to the in-memory cache."
  (let* ((node-id (org-roam-node-id node))
         (level (org-roam-node-level node))
         (file-node-id (when (< 0 level)
                         (let* ((file (org-roam-node-file node))
                                (id (org-roam-ok-node--cache-in-memory-file-get file)))
                           (if id
                               id
                             (setq id (org-roam-ok-node--file-node-id node))
                             (org-roam-ok-node--cache-in-memory-file-save file id)
                             id))))
         (file-parent-node-id (when (= 0 level)
                                (org-roam-ok-node--get-parent-property node)))
         (item `(,node ,file-node-id ,file-parent-node-id)))
    (when org-roam-ok-node-use-cache-in-memory
      (puthash node-id `(,(float-time) ,@item) org-roam-ok-node--cache-in-memory))
    item))

(defun org-roam-ok-node--cache-in-memory-remove (node-id)
  "Save NODE to the in-memory cache."
  (remhash node-id org-roam-ok-node--cache-in-memory))

(defun org-roam-ok-node--cache-in-memory-fill ()
  (dolist (row (org-roam-ok-node--all-nodes))
    (org-roam-ok-node--from-id (car row))))

(defun org-roam-ok-node--cache-in-memory-maybe-remove ()
  (let ((file buffer-file-name))
    (when (string= (file-name-extension file) "org")
      (org-roam-ok-node--cache-in-memory-file-remove file)
      (dolist (node-id (org-roam-ok-node--all-node-ids-within-file file))
        (org-roam-ok-node--cache-in-memory-remove node-id)))))

(add-hook 'after-save-hook #'org-roam-ok-node--cache-in-memory-maybe-remove)

;;; Node utility functions

(defun org-roam-ok-node--from-id (node-id)
  "Get the node with NODE-ID either from in-memory cache or Org cache."
  (let ((node (car (org-roam-ok-node--cache-in-memory-get node-id))))
    (if node
        node
      (setq node (org-roam-node-from-id node-id))
      (org-roam-ok-node--cache-in-memory-save node)
      node)))

(defun org-roam-ok-node--file-node-from-id (node-id)
  (let ((node-and-parent-id (org-roam-ok-node--cache-in-memory-get node-id)))
    (when (not node-and-parent-id)
      (setq node-and-parent-id (org-roam-ok-node--cache-in-memory-save (org-roam-node-from-id node-id))))
    (let ((parent-node-id (cadr node-and-parent-id)))
      (when parent-node-id
        (org-roam-ok-node--from-id parent-node-id)))))

(defun org-roam-ok-node--parent-titles (node)
  "Get the title of the file NODE's parent."
  (let* ((parent-id (caddr (org-roam-ok-node--cache-in-memory-get (org-roam-node-id node))))
         (visited nil)
         titles)
    (catch 'circular
      (while parent-id
        (if (member parent-id visited)
            (progn
              (message "org-roam-ok-node: Circular nodes detected!")
              (throw 'circular nil)))
        (setq node (org-roam-ok-node--from-id parent-id))
        (setq titles (append titles `(,(org-roam-node-title node))))
        (setq visited (append visited `(,parent-id)))
        (setq parent-id (caddr (org-roam-ok-node--cache-in-memory-get parent-id))))
      titles)))

(defun org-roam-ok-node--title-aux-get (node)
  "Get the auxiliary title info for NODE."
  (let ((node-id (org-roam-node-id node))
        (title-or-alias (org-roam-node-title node))
        (alias-delimiter " = ")
        (section-delimiter " ❬ ")
        sections)
    (if (member title-or-alias (org-roam-node-aliases node))
        ;; `node' may have its title replaced with an alias, so pull
        ;; the title from the original node:
        (list alias-delimiter (org-roam-node-title (org-roam-ok-node--from-id node-id)))
      (setq sections
            (pcase (org-roam-node-level node)  ; 0 for file-level node
              (0 (org-roam-ok-node--parent-titles (org-roam-ok-node--from-id node-id)))
              (_ (or (org-roam-ok-node--parent-titles (org-roam-ok-node--file-node-from-id node-id))
                     `(,(org-roam-node-file-title node))))))
      (flatten-list (mapcar (lambda (x) `(,section-delimiter ,x)) sections)))))

(defun org-roam-ok-node--title-aux-render (title-aux)
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

(defun org-roam-ok-node--title (node)
  (concat (org-roam-node-title node)
          (org-roam-ok-node--title-aux-render (org-roam-ok-node--title-aux-get node))))

(cl-defmethod org-roam-node-ok-title ((node org-roam-node))
  (org-roam-ok-node--title node))

(defun org-roam-ok-node--tags (node)
  (let ((tags (if (eq 0 (org-roam-node-level node))
                  ;; File-level node
                  (org-roam-node-tags node)
                ;; Non-file-level node
                (cl-set-difference (org-roam-node-tags node)
                                   org-tags-exclude-from-inheritance
                                   :test 'equal))))
    tags))

(cl-defmethod org-roam-node-ok-tags ((node org-roam-node))
  (format "#%s#" (string-join (org-roam-ok-node--tags node) "#")))

(defun org-roam-ok-node--timestamp (node)
  (let* ((inhibit-message t)
         (mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
         (mtime (if mtime
                    (org-roam-timestamps-encode (car (split-string mtime)))
                  (org-roam-node-file-mtime node))))
    (format-time-string "%Y-%m-%d" mtime)))

(cl-defmethod org-roam-node-ok-timestamp ((node org-roam-node))
  (org-roam-ok-node--timestamp node))

(defun org-roam-ok-node--timestamp-marginalia (node)
  (let* ((inhibit-message t)
         (mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
         (mtime (if mtime
                    (org-roam-timestamps-encode (car (split-string mtime)))
                  (org-roam-node-file-mtime node))))
    (marginalia--time mtime)))

(cl-defmethod org-roam-node-ok-timestamp-marginalia ((node org-roam-node))
  (org-roam-ok-node--timestamp-marginalia node))

(defun org-roam-ok-node--slug (node)
  (org-roam-ok-string-to-org-slug (org-roam-node-title node)))

(with-eval-after-load 'org-roam-node
  ;; NOTE: To ensure *override*, need to eval after `org-roam-node' gets loaded
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (org-roam-ok-node--slug node)))

;;; Aliases

(defun org-roam-ok-node-alias-add-or-remove (&optional arg)
  "Add an Org Roam alias.
When called with the `\\[universal-argument]' prefix ARG, the alias
is removed instead of added."
  (interactive "P")
  (call-interactively
   (pcase arg
     ('(4) #'org-roam-alias-remove)
     (_ #'org-roam-alias-add))))

;;; Refs

(defun org-roam-ok-node-ref-find (&optional _)
  "Call the enhanced version of `org-roam-ref-find'.
If the point is on a link and it is a cite link, then
`org-roam-ref-find' is given the citekey as the initial string.
Otherwise, it is the same as the vanilla version of
`org-roam-ref-find'."
  (interactive)
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (ref (cond
               ((string= type "cite")
                (string-trim-left path "&"))
               (t ""))))
    (org-roam-ref-find ref)))

;;; Tags

(defun org-roam-ok-node-tag-add-or-remove (&optional arg)
  "Add an Org filetags or heading tag.
When called with the `\\[universal-argument]' `\\[universal-argument]'
`\\[universal-argument]' prefix ARG, the tag is removed instead of
added."
  (interactive "P")
  (call-interactively
   (if (not (org-before-first-heading-p))
       ;; For non-filetags tags, use `org-set-tags-command' for both
       ;; addition and removal:
       #'org-set-tags-command
     (pcase arg
       ('(64) #'org-roam-tag-remove)
       (_ #'org-roam-tag-add)))))

(cl-defun org-roam-ok-node-nodes-with-backlinks (node)
  "Get the nodes with backlinks to NODE."
  (--map (org-roam-backlink-source-node it) (org-roam-backlinks-get node)))

;;; Nodes selector

(cl-defun org-roam-ok-node-nodes-select (&key (days 7) (tags nil) (limit nil) (filter nil))
  "Select nodes with TAGS within the last DAYS.
When given, result will be truncated to LIMIT nodes."
  (require 'org-roam-ok-timestamps)
  (let* ((days-string (mapcar
                       (lambda (time)
                         (substring (org-roam-timestamps-decode time)
                                    0 8))
                       (org-roam-ok-timestamps--dates days)))

         ;; Construct SQL statement.
         (where-days (mapcar
                      (lambda (s)
                        `(like
                          nodes:properties
                          ',(format "%%(\"MTIME\"%%\"%s%%" s)))
                      days-string))
         (where-tags (mapcar
                      (lambda (s)
                        `(like
                          nodes:properties
                          ',(format '"%%(\"ALLTAGS\"%%\"%%:%s:%%\"%%" s)))
                      tags))
         (where (cond ((and where-days where-tags) `((and
                                                      (or ,@where-days)
                                                      (and ,@where-tags))))
                      ((and where-days (null where-tags)) `((or ,@where-days)))
                      ((and (null where-days) where-tags) `((and ,@where-tags)))))
         (sql `[:select [nodes:id] :from nodes
                        ,@(if where `(:where ,@where))
                        :order-by [(desc nodes:title)]
                        ,@(if limit `(:limit ,limit))])
         (rows (mapcar
                (lambda (row)
                  (let* ((node (org-roam-populate (org-roam-node-create :id (car row))))
                         (node (if (functionp filter) (apply filter `(,node)) node)))
                    (when node
                      `(,(org-roam-ok-node-mtime node) . ,node))))
                (org-roam-db-query sql))))
    (mapcar (lambda (e) (cdr e))
            (sort (--filter it rows)
                  :key (lambda (row) (car row))
                  :reverse t))))

(defun org-roam-ok-node-mtime (node)
  "Access mtime of NODE."
  (first
   (string-split
    (alist-get "MTIME" (org-roam-node-properties node)
               nil nil 'equal)
    " ")))

;;; Node cashing

(defvar org-roam-ok-node-fill-caches--lock nil
  "Lock to ensure only one fill-caches session becomes active.")

(defun org-roam-ok-node-fill-caches ()
  "Fill all caches."
  (if org-roam-ok-node-fill-caches--lock
      (message "Skipping as an active fill-caches session exists.")
    (setq org-roam-ok-node-fill-caches--lock t)
    (message "Filling caches from org-roam-directory (s)..."
             org-roam-directory)
    (org-roam-ok-node--cache-in-memory-fill)
    (org-roam-ok-node--cache-in-memory-file-fill)
    (setq org-roam-ok-node-fill-caches--lock nil)))

;;; Editing utilities

(cl-defun org-roam-ok-node-insert-backlinks ()
  "Insert a backlinks section for node at point."
  (interactive)
  (let ((nodes (org-roam-ok-node-nodes-with-backlinks (org-roam-node-at-point))))
    (save-excursion
      (insert "* Backlinks\n")
      (dolist (node nodes)
        (let ((id (org-roam-node-id node))
              (desc (org-roam-node-title node)))
          (insert (format "** [[id:%s][%s]]\n" id desc)))))))

;;;###autoload
(defun org-roam-ok-node-fill-caches-async ()
  "Run `org-roam-ok-node-fill-caches' in an async process."
  (interactive)
  (unless (and (boundp 'org-roam-ok-node-fill-caches--lock)
               org-roam-ok-node-fill-caches--lock)
    (let ((async-debug t)
          (lsp-log-io nil)
          (lsp-print-performance nil)
          (inhibit-message t))
      (setq org-roam-ok-node-fill-caches--lock t)
      (async-start
       `(lambda ()
          (let ((load-path '(,@load-path))
                (started-time (current-time)))
            (require 'org-ok)
            (require 'org-roam)
            (require 'org-roam-ok)
            (org-roam-ok-enhance)

            (let ((org-roam-ok-node-project-org-file ,org-roam-ok-node-project-org-file))
              (org-roam-ok-node-project-org-file--load #'org-roam-ok-node-fill-caches))

            (prin1 (list org-roam-ok-node--cache-in-memory
                         org-roam-ok-node--cache-in-memory-file
                         started-time))))

       (lambda (result)
         (pcase-let
             ((`(,cache-in-memory ,cache-in-memory-file ,started-time) result))
           (message
            (concat "org-roam-ok: "
                    "Filled in-memory cache (%d nodes; %d files; took %f sec)")
            (hash-table-size cache-in-memory)
            (hash-table-size cache-in-memory-file)
            (float-time (time-subtract (current-time) started-time)))
           (setq org-roam-ok-node--cache-in-memory cache-in-memory
                 org-roam-ok-node--cache-in-memory-file cache-in-memory-file
                 org-roam-ok-node-fill-caches--lock nil)))))
    ;; (switch-to-buffer "*emacs*")
    ))

(provide 'org-roam-ok-node)
;;; org-roam-ok-node.el ends here
