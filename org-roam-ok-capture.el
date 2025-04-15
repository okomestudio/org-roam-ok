;;; org-roam-ok-capture.el --- Plugin for org-roam-capture  -*- lexical-binding: t -*-
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
;; This module provides a plugin for `org-roam-capture'.
;;
;;; Code:

(require 'dash)
(require 'ok)
(require 'org-ref)
(require 'org-roam-capture)
(require 's)

(defcustom oroc-template-directory nil
  "Template body directory.
Each file in this directory should contain template body.")

(defun oroc-templates-merge (templates)
  "Merge TEMPLATES to `org-roam-capture-templates'.
The TEMPLATES and `org-roam-capture-templates' are sorted by their
keys (i.e., the CAR of each template in the list) separately, merged,
and then saved to `org-roam-capture-templates'.

See the documentation for `org-roam-capture-templates' for how to write
each template."
  (let ((old-items (sort org-roam-capture-templates
                         (lambda (x y) (s-less? (car x) (car y)))))
        (new-items (sort templates
                         (lambda (x y) (s-less? (car x) (car y)))))
        result)
    (while (and old-items new-items)
      (if (s-less? (caar old-items) (caar new-items))
          (setq result (append result (list (pop old-items))))
        (if (s-equals? (caar old-items) (caar new-items))
            (progn
              (setq result (append result (list (pop new-items))))
              (pop old-items))
          (setq result (append result (list (pop new-items)))))))
    (setq result (append result old-items new-items))
    (setopt org-roam-capture-templates result)))

(defun oroc-template-as-string (template)
  "Read content of TEMPLATE as string.
The TEMPLATE file is looked for in `oroc-template-directory'."
  (let ((file (expand-file-name template oroc-template-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

;;; Capture templates with Bibtex

(defcustom oroc-templates-bibtex
  '(("b" "Example" plain
     (function (lambda ()
                 (org-roam-ok-capture-prompt-for-citekey-if-missing)
                 "#+title: ${title}\n"))
     :target (file+head "literature/${id}/${slug}.org"
                        ":PROPERTIES:\\n:ROAM_REFS:%^{ref}\n:END:\n")
     :unnarrowed t))
  "The `org-roam' capture templates for use with Bibtex.")

(defcustom oroc-type-to-capture-keys '((book . "b"))
  "The mapping from Bibtex record type to capture keys.")

(defcustom oroc-parent-from-citekey
  '(("SomeKeyPrefix" . "[[id:1234][Parent]]"))
  "Alist mapping refkey prefix to its parent node.")

(defun oroc--format-author (authors)
  "Format BibTeX AUTHORS list for `org-roam'.
See `bibtex-completion-shorten-authors' for reference."
  (cl-loop for a in (s-split " and " authors)
           for p = (--map (s-trim it) (s-split "," a t))
           for sep = "" then ", "
           concat sep
           if (eq 1 (length p))
           concat (car p)
           else
           concat (if (ok-string-contains-ja-p (car p))
                      (concat (car p) (cadr p))
                    (concat (cadr p) " " (car p)))))

(defun oroc--prepare-capture ()
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
        `(:article-author ,(oroc--format-author
                            (alist-get "author" record  nil 'equal))))
       ("book"
        `(:book-author ,(oroc--format-author
                         (alist-get "author" record "" nil 'equal))))
       ("online"
        `(:article-author ,(oroc--format-author
                            (alist-get "author" record "" nil 'equal))))
       ("podcast"
        (let ((parent
               (let* ((pattern
                       (format "^\\(%s\\)[0-9]+"
                               (s-join "\\|"
                                       (--map (car it)
                                              oroc-parent-from-citekey))))
                      (matched (if (string-match pattern key)
                                   (match-string 1 key)
                                 "")))
                 (alist-get matched oroc-parent-from-citekey key nil 'equal))))
          `( :podcast-guest ,(oroc--format-author
                              (alist-get "guest" record "" nil 'equal))
             :parent ,parent )))))
    `( :type ,type
       :node ,(org-roam-node-create :title title)
       :info (:citekey ,citekey ,@info) )))

(defun oroc-prompt-for-citekey-if-missing ()
  "If citekey is not set, this function will prompt user for a Bibtex item."
  (when (not (plist-member org-roam-capture--info :citekey))
    (let* ((result (oroc--prepare-capture))
           (node (plist-get result :node))
           (info (plist-get result :info)))
      (setf (org-roam-node-title org-roam-capture--node) (org-roam-node-title node))
      (setq org-roam-capture--info info))))

(defun oroc-create-from-ref ()
  "Capture from a Bibtex item."
  (interactive)
  (let* ((result (oroc--prepare-capture))
         (type (intern (plist-get result :type)))
         (org-roam-capture--node (plist-get result :node))
         (org-roam-capture--info (plist-get result :info))
         (capture-keys (alist-get type oroc-type-to-capture-keys)))
    (org-roam-capture- :keys capture-keys
                       :node org-roam-capture--node
                       :info org-roam-capture--info
                       :props '(:immediate-finish nil)
                       :templates oroc-templates-bibtex)))

(provide 'org-roam-ok-capture)

;; Local Variables:
;; read-symbol-shorthands: (("oroc" . "org-roam-ok-capture"))
;; End:
;;; org-roam-ok-capture.el ends here
