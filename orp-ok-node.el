;;; orp-ok-node.el --- Plugin for org-roam-node  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, org-roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.7") (org-roam "2.2.2") (marginalia "1.6"))
;;
;;; Commentary:
;;
;; This module provides a plugin for `org-roam-node'.
;;
;;; Code:

(require 'orp-ok-utils)
(require 'org-roam-timestamps)
(require 'marginalia)

(with-eval-after-load 'org-roam-node
  ;;; The org-roam cache layer (sqlite)
  (defun orp-ok-node--all-node-ids-within-file (file)
    "Get the IDs of all nodes within FILE."
    (mapcar 'car
            (org-roam-db-query `[:select :distinct nodes:id
                                         :from nodes
                                         :where (and (= nodes:file ,file))])))

  (defun orp-ok-node--file-node-id (node)
    "Get the ID of the top-level file node of NODE."
    (let ((file (org-roam-node-file node)))
      (caar (org-roam-db-query `[:select nodes:id :from nodes
                                         :where (and (= nodes:file ,file)
                                                     (= nodes:level 0))]))))

  (defun orp-ok-node--all-file-nodes-and-ids ()
    (org-roam-db-query '[:select [nodes:file nodes:id]
                                 :from nodes
                                 :where (= nodes:level 0)]))

  (defun orp-ok-node--all-nodes ()
    (org-roam-db-query '[:select nodes:id :from nodes]))

;;; In-memory cache
  ;;
  ;; This caching layer exists to speed up the interactive node query.
  ;;
  ;; TODO: Make the implementation memory efficient.

  (defvar orp-ok-node--cache-in-memory-file (make-hash-table :test 'equal)
    "In-memory cache, mapping a file to the ID of its top-level node.")

  (defun orp-ok-node--cache-in-memory-file-get (file)
    (gethash file orp-ok-node--cache-in-memory-file))

  (defun orp-ok-node--cache-in-memory-file-save (file node-id)
    (puthash file node-id orp-ok-node--cache-in-memory-file))

  (defun orp-ok-node--cache-in-memory-file-remove (file)
    (remhash file orp-ok-node--cache-in-memory-file))

  (defun orp-ok-node--cache-in-memory-file-fill ()
    (dolist (row (orp-ok-node--all-file-nodes-and-ids))
      (orp-ok-node--cache-in-memory-file-save (car row) (cadr row))))

  (defvar orp-ok-node--cache-in-memory (make-hash-table :test 'equal)
    "In-memory cache, mapping a node ID to its node.")

  (defun orp-ok-node--cache-in-memory-get (node-id)
    "Get the node for NODE-ID from the in-memory cache."
    (cdr (gethash node-id orp-ok-node--cache-in-memory)))

  (defun orp-ok-node--cache-in-memory-save (node)
    "Save NODE to the in-memory cache."
    (let* ((node-id (org-roam-node-id node))
           (level (org-roam-node-level node))
           (file-node-id (when (< 0 level)
                           (let* ((file (org-roam-node-file node))
                                  (id (orp-ok-node--cache-in-memory-file-get file)))
                             (if id
                                 id
                               (setq id (orp-ok-node--file-node-id node))
                               (orp-ok-node--cache-in-memory-file-save file id)
                               id))))
           (file-parent-node-id
            (when (= 0 level)
              (let* ((prop (org-roam-node-properties node))
                     (parent (cdr (assoc-string "PARENT" prop))))
                (when parent
                  (replace-regexp-in-string
                   "\\[\\[id:\\(.+\\)\\]\\[\\([^]]+\\)\\]\\]"
                   "\\1"
                   parent)))))
           (item `(,node ,file-node-id ,file-parent-node-id)))
      (puthash node-id `(,(float-time) ,@item) orp-ok-node--cache-in-memory)
      item))

  (defun orp-ok-node--cache-in-memory-remove (node-id)
    "Save NODE to the in-memory cache."
    (remhash node-id orp-ok-node--cache-in-memory))

  (defun orp-ok-node--cache-in-memory-fill ()
    (dolist (row (orp-ok-node--all-nodes))
      (orp-ok-node--from-id (car row))))

  (defun orp-ok-node--cache-in-memory-maybe-remove ()
    (let ((file buffer-file-name))
      (when (string= (file-name-extension file) "org")
        (orp-ok-node--cache-in-memory-file-remove file)
        (dolist (node-id (orp-ok-node--all-node-ids-within-file file))
          (orp-ok-node--cache-in-memory-remove node-id)))))

  (add-hook 'after-save-hook #'orp-ok-node--cache-in-memory-maybe-remove)

  ;;; Node utility functions

  (defun orp-ok-node--from-id (node-id)
    "Get the node with NODE-ID either from in-memory cache or Org cache."
    (let ((node (car (orp-ok-node--cache-in-memory-get node-id))))
      (if node
          node
        (setq node (org-roam-node-from-id node-id))
        (orp-ok-node--cache-in-memory-save node)
        node)))

  (defun orp-ok-node--file-node-from-id (node-id)
    (let ((node-and-parent-id (orp-ok-node--cache-in-memory-get node-id)))
      (when (not node-and-parent-id)
        (setq node-and-parent-id (orp-ok-node--cache-in-memory-save (org-roam-node-from-id node-id))))
      (let ((parent-node-id (cadr node-and-parent-id)))
        (when parent-node-id
          (orp-ok-node--from-id parent-node-id)))))

  (defun orp-ok-node--parent-titles (node)
    "Get the title of the file NODE's parent."
    (let* ((parent-id (caddr (orp-ok-node--cache-in-memory-get (org-roam-node-id node))))
           (visited nil)
           titles)
      (catch 'circular
        (while parent-id
          (if (member parent-id visited)
              (progn
                (message "orp-ok-node: Circular nodes detected!")
                (throw 'circular nil)))
          (setq node (orp-ok-node--from-id parent-id))
          (setq titles (append titles `(,(org-roam-node-title node))))
          (setq visited (append visited `(,parent-id)))
          (setq parent-id (caddr (orp-ok-node--cache-in-memory-get parent-id))))
        titles)))

  (defun orp-ok-node--title-aux-get (node)
    "Get the auxiliary title info for NODE."
    (let ((title-or-alias (org-roam-node-title node)))
      (if (member title-or-alias (org-roam-node-aliases node))
          ;; `node' may have its title replaced with an alias, so pull
          ;; the title from the original node:
          (list " = " (org-roam-node-title (orp-ok-node--from-id (org-roam-node-id node))))
        (let ((section-delimiter " ❬ ")
              sections)
          (setq sections
                (if (eq 0 (org-roam-node-level node))
                    ;; File-level node
                    (let ((parent-titles (orp-ok-node--parent-titles node)))
                      (if parent-titles
                          parent-titles))
                  ;; Non-file-level node
                  (let* ((p (orp-ok-node--file-node-from-id (org-roam-node-id node)))
                         (parent-titles (orp-ok-node--parent-titles p)))
                    (or parent-titles
                        `(,(org-roam-node-file-title node))))))
          (flatten-list (mapcar (lambda (x) `(,section-delimiter ,x))
                                sections))))))

  (defun orp-ok-node--title-aux-render (title-aux)
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

  ;; Node accessors

  (cl-defmethod org-roam-node-orp-title ((node org-roam-node))
    (concat (org-roam-node-title node)
            (orp-ok-node--title-aux-render (orp-ok-node--title-aux-get node))))

  (cl-defmethod org-roam-node-orp-tags ((node org-roam-node))
    (let ((tags (if (eq 0 (org-roam-node-level node))
                    ;; File-level node
                    (org-roam-node-tags node)
                  ;; Non-file-level node
                  (cl-set-difference (org-roam-node-tags node)
                                     org-tags-exclude-from-inheritance
                                     :test 'equal))))
      (when tags
        (format "#%s#" (string-join tags "#")))))

  (cl-defmethod org-roam-node-orp-timestamp ((node org-roam-node))
    (let* ((inhibit-message t)
           (mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
           (mtime (if mtime
                      (org-roam-timestamps-encode (car (split-string mtime)))
                    (org-roam-node-file-mtime node))))
      (format-time-string "%Y-%m-%d" mtime)))

  (cl-defmethod org-roam-node-orp-timestamp-marginalia ((node org-roam-node))
    (let* ((inhibit-message t)
           (mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
           (mtime (if mtime
                      (org-roam-timestamps-encode (car (split-string mtime)))
                    (org-roam-node-file-mtime node))))
      (marginalia--time mtime)))

  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (orp-string-to-org-slug (org-roam-node-title node)))

  ;; Interactive functions

  (defun orp-ok-node-visit-parent-of-node-at-point (node)
    "Visit parent of given NODE at point, if exists."
    (interactive "P")
    (let* ((node (or node (org-roam-node-at-point)))
           (parent (cdr (assoc-string "PARENT"
                                      (org-roam-node-properties node)))))
      (if parent
          (org-link-open-from-string parent)
        (message "No parent found for this node"))))

  ;;; Fill cache on idle for speeding up the first invocation

  (defun orp-ok-node--on-idle ()
    (message "On idle...")
    (orp-ok-node--cache-in-memory-fill)
    (orp-ok-node--cache-in-memory-file-fill))

  (run-with-idle-timer 10 nil #'orp-ok-node--on-idle))

(provide 'orp-ok-node)
;;; orp-ok-node.el ends here
