;;; orp-ok-ja-patch.el --- Japanese patch  -*- lexical-binding: t -*-
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
;; This module supplies a patch to enhance the unlinked references
;; section of Org Roam buffer for use with Japanese.
;;
;; The implementation within the following form has been submitted as
;; the following pull request:
;;
;;   - https://github.com/org-roam/org-roam/pull/2417/
;;
;; Use the `orp-ok-ja-unlinked-references-section' function in place
;; of `org-roam-unlinked-references-section'.
;;
;;; Code:

(require 'magit-section)

(with-eval-after-load 'org-roam-mode
  (defcustom org-roam-unlinked-references-word-boundary-re "|(\\b%1$s\\b)"
    "The word bounday regex used by ripgrep for unlinked references.
In such languages as CJK, the regex's word boundary (\b) does not
correctly determine how words and phrases should be tokenized.
This custom variable allows users to extend regex in those cases."
    :group 'org-roam
    :type 'string)

  (defcustom org-roam-unlinked-references-max-results-count 1000
    "The max number of items in the unlinked references section.
Rendering of the unlinked references section can appear to freeze
when the match count is very large. This number limits the
maximum number of matched unlinked references to show to prevent
the issue."
    :group 'org-roam
    :type 'integer)

  (defun org-roam-unlinked-references--result-filter-p (matched-text matched-file row col titles node)
    "Filter if the match is considered an unlinked reference.
Return non-nil if MATCHED-TEXT at ROW and COL in MATCHED-FILE is
an unlinked reference, or return nil. TITLES and NODE are
supplied for use in the conditional expression."
    (and (not (file-equal-p (org-roam-node-file node) matched-file))
         (member (downcase matched-text) (mapcar #'downcase titles))))

  (defun org-roam-unlinked-references-preview-line (file row col file-prev row-prev col-prev)
    "Return the preview line from FILE.
The line was matched with text at ROW and COL. FILE-PREV,
ROW-PREV, and COL-PREV points to the previous line and can be
used to control rendering."
    (with-temp-buffer
      (insert-file-contents file)
      (forward-line (1- row))
      (buffer-substring-no-properties
       (save-excursion
         (beginning-of-line)
         (point))
       (save-excursion
         (end-of-line)
         (point)))))

  (defun org-roam-unlinked-references--file-glob-args ()
    "Construct file glob arguments for ripgrep."
    (mapconcat (lambda (glob) (concat "-g " glob))
               (org-roam--list-files-search-globs org-roam-file-extensions)
               " "))

  (defun org-roam-unlinked-references--title-regex (titles)
    "Construct a ripgrep regex pattern from TITLES.
The output expression should be sanitized for the shell use."
    (format "'\\[([^[]]++|(?R))+\\]%s'"
            (mapconcat 'org-roam-unlinked-references--apply-word-boundary-re titles "")))

  (defun org-roam-unlinked-references--apply-word-boundary-re (title)
    "Wrap TITLE with word boundary regex."
    (format org-roam-unlinked-references-word-boundary-re
            (org-roam-unlinked-references--sanitize-title title)))

  (defun org-roam-unlinked-references--sanitize-title (title)
    "Sanitize TITLE for shell use."
    (mapconcat #'shell-quote-argument (split-string title "'") "'\"'\"'"))

  (defun org-roam-unlinked-references--rg-command (titles)
    "Return the ripgrep command searching for TITLES."
    (concat "rg --follow --only-matching --vimgrep --pcre2 --ignore-case "
            (org-roam-unlinked-references--file-glob-args) " "
            (org-roam-unlinked-references--title-regex titles) " "
            (shell-quote-argument org-roam-directory)))

  ;; NOTE: The following function replaces
  ;; `org-roam-unlinked-references-section' in the patch submitted as
  ;; a pull request. Here, a different function name is given in order
  ;; to allow use of both implementations side-by-side.

  (defun orp-ok-ja-unlinked-references-section (node)
    "The unlinked references section for NODE.
References from FILE are excluded."
    (when (and (executable-find "rg")
               (org-roam-node-title node)
               (not (string-match "PCRE2 is not available"
                                  (shell-command-to-string "rg --pcre2-version"))))
      (let* ((titles (cons (org-roam-node-title node)
                           (org-roam-node-aliases node)))
             (rg-command (org-roam-unlinked-references--rg-command titles))
             (results (split-string (shell-command-to-string rg-command) "\n"))
             (match_count 0)
             f f-prev row row-prev col col-prev matched-text)
        (magit-insert-section
            (unlinked-references)
          (magit-insert-heading "Unlinked References:")
          (catch 'limit-result
            (dolist (line results)
              (save-match-data
                (when (string-match org-roam-unlinked-references-result-re line)
                  (setq f (match-string 1 line)
                        row (string-to-number (match-string 2 line))
                        col (string-to-number (match-string 3 line))
                        matched-text (match-string 4 line))
                  (when (and matched-text
                             (org-roam-unlinked-references--result-filter-p
                              matched-text f row col titles node))
                    (magit-insert-section
                        section (org-roam-grep-section)
                        (oset section file f)
                        (oset section row row)
                        (oset section col col)
                        (insert (propertize
                                 (format "%s:%s:%s"
                                         (truncate-string-to-width (file-name-base f)
                                                                   15 nil nil t)
                                         row col)
                                 'font-lock-face 'org-roam-dim)
                                " "
                                (org-roam-fontify-like-in-org-mode
                                 (org-roam-unlinked-references-preview-line
                                  f row col f-prev row-prev col-prev))
                                "\n")
                        (setq f-prev f
                              row-prev row
                              col-prev col
                              match_count (+ match_count 1))
                        (if (= match_count org-roam-unlinked-references-max-results-count)
                            (insert (format "WARNING: Results truncated to %d items (%d potential matches)"
                                            match_count (length results))))))))
              (if (= match_count org-roam-unlinked-references-max-results-count)
                  ;; Throw outside of magit-insert-section to render correct item count.
                  (throw 'limit-result match_count))))
          (insert ?\n))))))

(provide 'orp-ok-ja-patch)
;;; orp-ok-ja-patch.el ends here
