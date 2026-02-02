;;; org-roam-ok-capture.el --- org-roam-capture Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2026 Taro Sato
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
;; This module provides a plugin for `org-roam-capture'.
;;
;;; Code:

(require 'dash)
(require 'ok)
(require 'org-ok)
(require 'org-ref)
(require 'org-roam-capture)
(require 's)

(defcustom org-roam-ok-capture-template-directory nil
  "Template body directory.
Each file in this directory should contain template body.")

;;;###autoload
(defun org-roam-ok-capture-templates-merge (templates &optional local)
  "Merge TEMPLATES to `org-roam-capture-templates'.
The TEMPLATES and `org-roam-capture-templates' are merged and sorted by the CAR
of each entry and then saved to `org-roam-capture-templates'.

When LOCAL is non-nil, a local copy of `org-roam-capture-templates' is created
before merge.

An element like `(KEY nil)' in TEMPLATES will remove the KEY entry.

See the `org-roam-capture-templates' documentation for template entry format."
  (let* ((capts (if (and local
                         (not (local-variable-p 'org-roam-capture-templates)))
                    (make-local-variable 'org-roam-capture-templates)
                  'org-roam-capture-templates))
         (old-items (sort (symbol-value capts)
                          :lessp (lambda (x y) (string< (car x) (car y)))))
         (new-items (sort templates
                          (lambda (x y) (string< (car x) (car y)))))
         result)
    (while (and old-items new-items)
      (setq result (if (string< (caar old-items) (caar new-items))
                       (append result (list (pop old-items)))
                     (if (string= (caar old-items) (caar new-items))
                         (progn
                           (pop old-items)
                           (append result (list (pop new-items))))
                       (append result (list (pop new-items)))))))
    (setq result (append result old-items new-items))
    (set capts (seq-filter (lambda (it) (cadr it)) result))))

(defun org-roam-ok-capture-template-as-string (template)
  "Read content of TEMPLATE as string.
The TEMPLATE file is looked for in `org-roam-ok-capture-template-directory'."
  (let ((file (expand-file-name template org-roam-ok-capture-template-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

;;; Capture templates with Bibtex

(defcustom org-roam-ok-capture-type-to-capture-keys '((book . "b"))
  "The mapping from Bibtex record type to capture keys.")

(defcustom org-roam-ok-capture-parent-from-citekey
  '(("SomeKeyPrefix" . "[[id:1234][Parent]]"))
  "Alist mapping refkey prefix to its parent node.")

(defun org-roam-ok-capture--prepare-capture ()
  "Prepare data for capture using a Bibtex item.
This function prompts user for a Bibtex item."
  (let* ((record (bibtex-completion-get-entry (org-ref-read-key)))
         (key (alist-get "=key=" record nil nil 'equal))
         (citekey (format "cite:&%s" key))
         (type (alist-get "=type=" record nil nil 'equal))
         (title (alist-get "title" record nil nil 'equal))
         (title (replace-regexp-in-string "[{}\\]" "" title))
         (output (shell-command-to-string
                  (format "rg \":ROAM_REFS:\s+.*cite:&%s\" -l %s"
                          key org-roam-directory)))
         info)
    (when (> (length (string-trim output)) 0)
      (error "Literature note already exists for %s" key))

    ;; TODO(2025-03-09): Format person name in the order of first and
    ;; last name? Also, should we have authors as org-roam nodes?
    (setq
     info
     (pcase type
       ("article"
        `(:article-author ,(org-ok-ref-format-author
                            (alist-get "author" record "" nil 'equal))))
       ("book"
        `(:book-author ,(org-ok-ref-format-author
                         (alist-get "author" record "" nil 'equal))))
       ("online"
        `(:article-author ,(org-ok-ref-format-author
                            (alist-get "author" record "" nil 'equal))))
       ("podcast"
        (let ((parent
               (let* ((pattern
                       (format "^\\(%s\\)[0-9]+"
                               (s-join "\\|"
                                       (--map (car it)
                                              org-roam-ok-capture-parent-from-citekey))))
                      (matched (if (string-match pattern key)
                                   (match-string 1 key)
                                 "")))
                 (alist-get matched org-roam-ok-capture-parent-from-citekey key nil 'equal))))
          `( :podcast-guest ,(org-ok-ref-format-author
                              (alist-get "guest" record "" nil 'equal))
             :parent ,parent )))))
    `( :type ,type
       :node ,(org-roam-node-create :title title)
       :info (:citekey ,citekey ,@info) )))

(defun org-roam-ok-capture-prompt-for-citekey-if-missing ()
  "If citekey is not set, this function will prompt user for a Bibtex item."
  (when (not (plist-member org-roam-capture--info :citekey))
    (let* ((result (org-roam-ok-capture--prepare-capture))
           (node (plist-get result :node))
           (info (plist-get result :info)))
      (setf (org-roam-node-title org-roam-capture--node) (org-roam-node-title node))
      (setq org-roam-capture--info info))))

(defun org-roam-ok-capture-create-from-ref ()
  "Capture from a Bibtex item."
  (interactive)
  (let* ((result (org-roam-ok-capture--prepare-capture))
         (type (intern (plist-get result :type)))
         (org-roam-capture--node (plist-get result :node))
         (org-roam-capture--info (plist-get result :info))
         (capture-keys (alist-get type org-roam-ok-capture-type-to-capture-keys)))
    (org-roam-capture- :keys capture-keys
                       :node org-roam-capture--node
                       :info org-roam-capture--info
                       :props '(:immediate-finish nil))))

(provide 'org-roam-ok-capture)
;;; org-roam-ok-capture.el ends here
