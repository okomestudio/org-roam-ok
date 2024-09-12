;;; orp-ok-org.el --- Org mode  -*- lexical-binding: t -*-
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

(provide 'orp-ok-org)
;;; orp-ok-org.el ends here
