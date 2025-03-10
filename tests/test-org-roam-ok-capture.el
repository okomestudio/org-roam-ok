;;; test-org-roam-ok-capture.el --- test-org-roam-ok-capture  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'org-roam-ok-capture)

(describe
 "org-roam-ok-capture-templates-merge"
 :var ((templates '(("b" "desc" entry "%?")))
       org-roam-capture-templates)
 (it "merges TEMPLATES to `org-roam-capture-templates"
     (expect
      (progn
        (setopt org-roam-capture-templates
                '(("a" "desc" entry "%?") ("c" "desc" entry "%?")))
        (org-roam-ok-capture-templates-merge templates)
        org-roam-capture-templates)
      :to-equal '(("a" "desc" entry "%?")
                  ("b" "desc" entry "%?")
                  ("c" "desc" entry "%?")))))

(describe
 "org-roam-ok-capture--format-author"
 (it "format authors list correctly"
     (pcase-dolist
         (`(,src ,expected)
          '(("Doe, John and Doe, Jane" "John Doe, Jane Doe")
            ("van der Beek, Hans" "Hans van der Beek")
            ("山田, 太郎" "山田太郎")))
       (expect
        (org-roam-ok-capture--format-author src) :to-equal expected))))

;;; test-org-roam-ok-capture.el ends here
