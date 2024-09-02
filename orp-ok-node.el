;;; orp-ok-node.el --- Plugin for org-roam-node  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, org-roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.7") (org-roam "2.2.2"))
;;
;;; Commentary:
;;
;; This module provides a plugin for `org-roam-node'.
;;
;;; Code:

(require 'orp-ok-utils)
(require 'org-roam-timestamps)

(with-eval-after-load 'org-roam-node
  (defun orp-visit-parent-of-node-at-point (node)
    "Visit parent of given NODE at point, if exists."
    (interactive "P")
    (let* ((node (or node (org-roam-node-at-point)))
           (parent (cdr (assoc-string "PARENT"
                                      (org-roam-node-properties node)))))
      (if parent
          (org-link-open-from-string parent)
        (message "No parent found for this node"))))

  ;; Node title rendering utility functions.
  (defvar orp--file-node-cache '()
    "Cache for file nodes.")

  (defun orp--file-node-cache-maybe-invalidate ()
    "Invalidate file node cache if the saved buffer is Org file."
    (let ((file buffer-file-name))
      (when (string= (file-name-extension file) "org")
        (setf orp--file-node-cache (assoc-delete-all file orp--file-node-cache)))))

  (add-hook 'after-save-hook #'orp--file-node-cache-maybe-invalidate)

  (defun orp--node-id-from-file (file)
    "Get the node ID for FILE."
    (caar (org-roam-db-query `[:select nodes:id :from nodes
                                       :where (and (= nodes:file ,file)
                                                   (= nodes:level 0))])))

  (defun orp--node-from-file (file)
    "Get the node for FILE. Use the cache whenever possible."
    (let ((cached (assoc file orp--file-node-cache)))
      (if cached
          (cdr (cdr cached))
        (let ((node (org-roam-node-from-id (orp--node-id-from-file file))))
          (push `(,file . (,(float-time) . ,node)) orp--file-node-cache)
          node))))

  (defun orp--parent-title-get (node)
    "Get the title of the NODE's parent."
    (let ((parent (cdr (assoc-string "PARENT" (org-roam-node-properties node)))))
      (when parent
        ;; NOTE: This replacement may not be necessary, but some links are not
        ;; rendered correctly in minibuffer without. For now, the slow down due
        ;; to parsing is not significant.
        (replace-regexp-in-string "\\[\\[\\(.+\\)\\]\\[\\([^]]+\\)\\]\\]"
                                  "\\2"
                                  parent))))

  (defun orp--title-aux-get (node)
    "Get the auxiliary title info for NODE."
    (let ((node-title (org-roam-node-title node))
          (node-file-title (or (if (not (s-blank? (org-roam-node-file-title node)))
                                   (org-roam-node-file-title node))
                               (file-name-nondirectory (org-roam-node-file node)))))
      (if (string= node-title node-file-title)
          ;; This is the file-level node.
          (let ((p (orp--parent-title-get node)))
            (if p
                (list " ❬ " p))) ;; add the parent title if a parent exists
        ;; This is a node within a file.
        (if (member node-title (org-roam-node-aliases node))
            (list " = " node-file-title) ;; show equality for the alias entry
          (let ((p (orp--parent-title-get (orp--node-from-file (org-roam-node-file node)))))
            (list " ❬ " (or p node-file-title)))))))

  (defun orp--title-aux-render (title-aux)
    (if (not title-aux)
        ""
      (let ((sym (nth 0 title-aux)) ;; symbol
            (face-sym `(:foreground ,(face-attribute 'completions-annotations
                                                     :foreground)))
            (aux (nth 1 title-aux)) ;; text
            (face-aux `(:foreground ,(face-attribute 'completions-annotations
                                                     :foreground)
                                    :slant italic)))
        (concat (propertize sym 'face face-sym)
                (propertize aux 'face face-aux)))))

  (cl-defmethod org-roam-node-orp-title ((node org-roam-node))
    (concat (org-roam-node-title node)
            (orp--title-aux-render (orp--title-aux-get node))))

  (cl-defmethod org-roam-node-orp-tags ((node org-roam-node))
    (let ((tags (org-roam-node-tags node)))
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
    (require 'marginalia)
    (let* ((inhibit-message t)
           (mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
           (mtime (if mtime
                      (org-roam-timestamps-encode (car (split-string mtime)))
                    (org-roam-node-file-mtime node))))
      (marginalia--time mtime)))

  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (orp-string-to-org-slug (org-roam-node-title node))))

(provide 'orp-ok-node)
;;; orp-ok-node.el ends here
