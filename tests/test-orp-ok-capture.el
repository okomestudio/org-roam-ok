;;; test-orp-ok-capture.el --- test-orp-ok-capture  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'orp-ok-capture)

(describe
 "ooc-templates-merge"
 :var ((templates '(("b" "desc" entry "%?")))
       org-roam-capture-templates)
 (it "merges TEMPLATES to `org-roam-capture-templates"
     (expect
      (progn
        (setopt org-roam-capture-templates
                '(("a" "desc" entry "%?") ("c" "desc" entry "%?")))
        (ooc-templates-merge templates)
        org-roam-capture-templates)
      :to-equal '(("a" "desc" entry "%?")
                  ("b" "desc" entry "%?")
                  ("c" "desc" entry "%?")))))

;; Local Variables:
;; read-symbol-shorthands: (("ooc" . "orp-ok-capture"))
;; End:
;;; test-orp-ok-capture.el ends here
