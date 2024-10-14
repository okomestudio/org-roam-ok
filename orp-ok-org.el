;;; orp-ok-org.el --- Org mode  -*- lexical-binding: t -*-
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
;;; Code:

(require 'org)

(defun orp-ok-org-add-properties (props)
  "Add properties.
PROPS is a list of cons cells (keyword . values). When values is
a list, its values are used as fixed values for the preset
properties."
  (dolist (prop props)
    (let ((keyword (car prop))
          (values (cdr prop)))
      (when values
        (add-to-list 'org-global-properties-fixed
                     `(,(concat keyword "_ALL")
                       .
                       ,(mapconcat (lambda (s)
                                     (format "\"%s\"" s))
                                   values
                                   " "))))
      (add-to-list 'org-default-properties keyword))))

;;; org-babel

(defun orp-ok-org-babel-run-pytest (&optional arg tangle-default)
  "Run ob-pytest using the current Python code block.
If tangle is missing, it will default to TANGLE-DEFAULT if given or
't.py' if not given."
  (interactive)
  (let* ((params (nth 2 (org-babel-get-src-block-info)))
         (tangle (alist-get :tangle params))
         (tangle (if (string= tangle "no")
                     (or tangle-default "t.py")
                   tangle))
         (src-ob-pytest
          (concat
           (format "#+begin_src shell :var in=\"%s\" :exports none :results output\n"
                   tangle)
           "  ob-pytest \"$in\"\n"
           "#+end_src\n")))
    (if (null params)
        (message "Org Babel source block not found")
      (org-babel-tangle '(4) tangle)
      (org-forward-element)
      (insert src-ob-pytest)
      (previous-line)
      (let ((org-confirm-babel-evaluate nil))
        (org-ctrl-c-ctrl-c))
      (re-search-forward "#\\+RESULTS:" nil nil 1)
      (recenter-top-bottom)
      (delete-file tangle))))

;;; org-src

(defcustom orp-ok-org-src-noweb-ref-re "<<\\([A-Za-z0-9_.-]+\\)>>"
  "Regexp for noweb-ref.")

(defun orp-ok-org-src--comment-out-noweb-ref (beg end)
  "Comment out noweb references in Org source edit buffer from BEG to END."
  (when (org-src-edit-buffer-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward (concat "^" orp-ok-org-src-noweb-ref-re)
                                end t 1)
        (replace-match (concat comment-start "<<\\1>>"))
        (setq end (+ end (length comment-start))))))
  end)

(defun orp-ok-org-src--uncomment-noweb-ref (beg end)
  "Uncomment noweb references in Org source edit buffer from BEG to END."
  (when (org-src-edit-buffer-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward (concat "^" comment-start
                                        orp-ok-org-src-noweb-ref-re)
                                end t 1)
        (replace-match "<<\\1>>")))))

(defun orp-ok-org-src-reformatter--do-region-ad (func name beg end &rest rest)
  "Advise reformatter--do-region to ignore noweb-refs."
  (let ((end (orp-ok-org-src--comment-out-noweb-ref beg end)))
    (apply func `(,name ,beg ,end ,@rest))
    (orp-ok-org-src--uncomment-noweb-ref beg end)))

(with-eval-after-load 'org-src
  (with-eval-after-load 'reformatter
    (advice-add #'reformatter--do-region :around 'orp-ok-org-src-reformatter--do-region-ad)))

(provide 'orp-ok-org)
;;; orp-ok-org.el ends here
