;;; org-roam-ok-utils.el --- Org Roam Plugin Okome Studio Utilities  -*- lexical-binding: t -*-
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
;; This module provides a collection of utility functions.
;;
;;; Code:

(require 'dash)

(defun org-roam-ok-ensure-all-headings-with-ids ()
  "Ensure all headings have IDs."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (org-id-get-create))))

(defun org-roam-ok-extract-subtree-to-subdir ()
  "Extract the subtree to a new Org file within the current directory."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let* ((parent-node (org-roam-node-at-point))
           (parent-file (org-roam-node-file parent-node))

           ;; Make the parent directory path relative to `org-roam-directory'
           (directory (file-name-directory
                       (string-replace org-roam-directory
                                       ""
                                       parent-file))))
      (setq org-roam-extract-new-file-path (file-name-concat directory
                                                             "${slug}.org"))))
  (call-interactively #'org-roam-extract-subtree))

(defun org-roam-ok-filetags ()
  "Get filetags from the current node."
  (remove-if (lambda (x) (string= "" x))
             (string-split (cadar (org-collect-keywords '("filetags"))) ":")))

(defun org-roam-ok-interpolate-leaf-nodes-for-export ()
  "Extrapolate leaf heading nodes for export.
When invoked within an Org buffer, the headings are traversed in
its copy, each leaf heading expanded with the body of the target
node."
  (interactive)
  (let ((tmp-buffer (org-export-copy-buffer)))
    (with-current-buffer tmp-buffer
      (beginning-of-buffer)
      (while (outline-next-heading)
        (while (org-goto-first-child) t)
        (end-of-line)
        (backward-char)
        (when (link-hint--org-link-at-point-p)  ; missing function?
          (let ((has-content nil))
            (save-excursion
              (org-open-at-point +1)
              (beginning-of-line)
              (if (eq ?* (char-after))
                  (setq has-content t))
              (with-current-buffer (current-buffer)
                (org-preserve-local-variables
                 (let* ((end (org-end-of-subtree t t)))
                   (previous-line)
                   (org-back-to-heading)
                   (copy-region-as-kill (re-search-forward "^\\s-*$") end)))
                (kill-buffer)))
            (end-of-line)
            (org-return-and-maybe-indent)
            (when has-content
              (org-yank))))))
    (switch-to-buffer tmp-buffer)))

(defun org-roam-ok-link-get (&optional arg)
  "Extract URL from org-mode link and add it to kill ring.
See emacs.stackexchange.com/a/60555/599."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)))

(defun org-roam-ok-string-to-org-slug (title)
  "Turn TITLE into its '-'-delimited slug.

This function is used in place of `org-roam-node-slug'."
  (let (;; Combining Diacritical Marks
        ;; https://www.unicode.org/charts/PDF/U0300.pdf
        (slug-trim-chars '(768     ; U+0300 COMBINING GRAVE ACCENT
                           769     ; U+0301 COMBINING ACUTE ACCENT
                           770     ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771     ; U+0303 COMBINING TILDE
                           772     ; U+0304 COMBINING MACRON
                           774     ; U+0306 COMBINING BREVE
                           775     ; U+0307 COMBINING DOT ABOVE
                           776     ; U+0308 COMBINING DIAERESIS
                           777     ; U+0309 COMBINING HOOK ABOVE
                           778     ; U+030A COMBINING RING ABOVE
                           779     ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780     ; U+030C COMBINING CARON
                           795     ; U+031B COMBINING HORN
                           803     ; U+0323 COMBINING DOT BELOW
                           804     ; U+0324 COMBINING DIAERESIS BELOW
                           805     ; U+0325 COMBINING RING BELOW
                           807     ; U+0327 COMBINING CEDILLA
                           813     ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814     ; U+032E COMBINING BREVE BELOW
                           816     ; U+0330 COMBINING TILDE BELOW
                           817)))  ; U+0331 COMBINING MACRON BELOW
    (cl-flet* ((nonspacing-mark-p (char)
                 (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                 (string-glyph-compose
                  (apply #'string
                         (seq-remove #'nonspacing-mark-p
                                     (string-glyph-decompose s)))))
               (cl-replace (title pair)
                 (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(; convert anything not alphanumeric
                      ("[^[:alnum:][:digit:]]" . "-")

                      ("--*" . "-")  ; remove sequential underscores
                      ("^-" . "")    ; remove starting underscore
                      ("-$" . "")))  ; remove ending underscore
             (slug (-reduce-from #'cl-replace
                                 (strip-nonspacing-marks title)
                                 pairs)))
        (downcase slug)))))

(make-obsolete #'org-roam-ok-string-to-org-slug #'org-ok-text-to-slug "0.3")

;;;###autoload
(defun org-roam-ok-rename-visited-file-from-title ()
  "Rename the visited file using the slug from the document title."
  (interactive)
  (if-let* ((title (cadr (assoc "TITLE" (org-collect-keywords '("title")))))
            (slug (org-roam-ok-string-to-org-slug title)))
      (rename-visited-file (format "%s.org" slug))
    (warn "Cannot parse document title")))

;;;###autoload
(defun org-roam-ok-mv-cwd-to (dest)
  "Move the current directory under the directory DEST."
  ;; TODO(2025-07-21): Remove the mv-pwd-to dependency.
  (interactive "DPick the destination directory: ")
  (let* ((node (save-excursion (beginning-of-buffer) (org-roam-node-at-point)))
         (id (org-roam-node-id node))
         (cmd (format "mv-pwd-to %s" dest))
         (output (shell-command-to-string cmd)))
    (kill-buffer)
    (org-roam-db-sync)
    (org-roam-node-visit (org-roam-node-from-id id))
    (message "Moved %s to %s")))

;;;###autoload
(defun org-roam-ok-move-cwd (target)
  "Move the current directory for the node at point to directory under TARGET."
  (interactive "DPick the target parent directory: ")
  (if-let* ((node (save-excursion (beginning-of-buffer)
                                  (org-roam-node-at-point)))
            (cwd (file-name-as-directory (expand-file-name default-directory)))
            (dirname (file-name-nondirectory (directory-file-name cwd))))
      (progn
        (unless (file-exists-p target)
          (make-directory target t))
        (rename-file cwd (file-name-concat target dirname)))
    (warn "Not an `org-roam' node")))

(provide 'org-roam-ok-utils)
;;; org-roam-ok-utils.el ends here
