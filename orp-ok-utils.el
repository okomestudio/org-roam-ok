;;; orp-ok-utils.el --- Org Roam Plugin Okome Studio Utilities  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.4") (dash "2.13") (adaptive-wrap "0.8"))
;;
;;; Commentary:
;;
;; This module provides a collection of utility functions.
;;
;;; Code:

(require 'dash)

(defun orp-extract-subtree-to-subdir ()
  "Extract the subtree to a new Org file within the current directory."
  (interactive)
  (let ((org-roam-extract-new-file-path
         (save-excursion
           (beginning-of-buffer)
           (let* ((parent-node (org-roam-node-at-point))
                  (parent-file (org-roam-node-file parent-node)))
             (file-name-concat
              (file-name-directory (string-replace org-roam-directory
                                                   ""
                                                   parent-file))
              "${slug}.org")))))
    (call-interactively 'org-roam-extract-subtree)))

(defun orp-string-to-org-slug (title)
  "Turn the TITLE string into a slug for Org file."
  (let (;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
        (slug-trim-chars '(768 ;; U+0300 COMBINING GRAVE ACCENT
                           769 ;; U+0301 COMBINING ACUTE ACCENT
                           770 ;; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ;; U+0303 COMBINING TILDE
                           772 ;; U+0304 COMBINING MACRON
                           774 ;; U+0306 COMBINING BREVE
                           775 ;; U+0307 COMBINING DOT ABOVE
                           776 ;; U+0308 COMBINING DIAERESIS
                           777 ;; U+0309 COMBINING HOOK ABOVE
                           778 ;; U+030A COMBINING RING ABOVE
                           779 ;; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780 ;; U+030C COMBINING CARON
                           795 ;; U+031B COMBINING HORN
                           803 ;; U+0323 COMBINING DOT BELOW
                           804 ;; U+0324 COMBINING DIAERESIS BELOW
                           805 ;; U+0325 COMBINING RING BELOW
                           807 ;; U+0327 COMBINING CEDILLA
                           813 ;; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ;; U+032E COMBINING BREVE BELOW
                           816 ;; U+0330 COMBINING TILDE BELOW
                           817))) ;; U+0331 COMBINING MACRON BELOW
    (cl-flet* ((nonspacing-mark-p (char)
                 (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                 (string-glyph-compose
                  (apply #'string
                         (seq-remove #'nonspacing-mark-p
                                     (string-glyph-decompose s)))))
               (cl-replace (title pair)
                 (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                      ("--*" . "-") ;; remove sequential underscores
                      ("^-" . "")   ;; remove starting underscore
                      ("-$" . ""))) ;; remove ending underscore
             (slug (-reduce-from #'cl-replace
                                 (strip-nonspacing-marks title)
                                 pairs)))
        (downcase slug)))))

(provide 'orp-ok-utils)
;;; orp-ok-utils.el ends here
