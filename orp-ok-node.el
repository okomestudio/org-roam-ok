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
  (defun orp-ok-node-visit-parent-of-node-at-point (node)
    "Visit parent of given NODE at point, if exists."
    (interactive "P")
    (let* ((node (or node (org-roam-node-at-point)))
           (parent (cdr (assoc-string "PARENT"
                                      (org-roam-node-properties node)))))
      (if parent
          (org-link-open-from-string parent)
        (message "No parent found for this node"))))

  ;; Node title rendering utility functions.
  (defvar orp-ok-node--file-node-cache '()
    "Cache for file nodes.")

  (defun orp-ok-node--file-node-cache-maybe-invalidate ()
    "Invalidate file node cache if the saved buffer is Org file."
    (let ((file buffer-file-name))
      (when (string= (file-name-extension file) "org")
        (setf orp-ok-node--file-node-cache (assoc-delete-all file orp-ok-node--file-node-cache)))))

  (add-hook 'after-save-hook #'orp-ok-node--file-node-cache-maybe-invalidate)

  (defun orp-ok-node--id-from-file (file)
    "Get the node ID for FILE."
    (caar (org-roam-db-query `[:select :distinct nodes:id :from nodes
                                       :where (and (= nodes:file ,file)
                                                   (= nodes:level 0))])))

  (defun orp-ok-node--from-file (file)
    "Get the node for FILE. Use the cache whenever possible."
    (let ((cached (assoc file orp-ok-node--file-node-cache)))
      (if cached
          (cdr (cdr cached))
        (let ((node (org-roam-node-from-id (orp-ok-node--id-from-file file))))
          (push `(,file . (,(float-time) . ,node)) orp-ok-node--file-node-cache)
          node))))

  (defun orp-ok-node--parent-title-get (node)
    "Get the title of the NODE's parent."
    (let ((parent (cdr (assoc-string "PARENT" (org-roam-node-properties node)))))
      (when parent
        ;; NOTE: This replacement may not be necessary, but some links are not
        ;; rendered correctly in minibuffer without. For now, the slow down due
        ;; to parsing is not significant.
        (replace-regexp-in-string "\\[\\[\\(.+\\)\\]\\[\\([^]]+\\)\\]\\]"
                                  "\\2"
                                  parent))))

  (defun orp-ok-node--title-aux-get (node)
    "Get the auxiliary title info for NODE."
    (let ((node-title (org-roam-node-title node)))
      (if (member node-title (org-roam-node-aliases node))
          (list " = " (org-roam-node-file-title node))
        (if (eq 0 (org-roam-node-level node))
            ;; File-level node
            (let ((pt (orp-ok-node--parent-title-get node)))
              (if pt
                  (list " ❬ " pt)))
          ;; Non-file-level node
          (let* ((p (orp-ok-node--from-file (org-roam-node-file node)))
                 (pt (orp-ok-node--parent-title-get p)))
            (list " ❬ " (or pt (org-roam-node-file-title node))))))))

  (defun orp-ok-node--title-aux-render (title-aux)
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

  ;; Public accessor methods

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
    (orp-string-to-org-slug (org-roam-node-title node))))

(provide 'orp-ok-node)
;;; orp-ok-node.el ends here
