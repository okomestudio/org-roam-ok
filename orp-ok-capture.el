;;; orp-ok-capture.el --- Plugin for org-roam-capture  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, roam, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.4") (org-roam "2.2.2") (s "1.13.1"))
;;
;;; Commentary:
;;
;; This module provides a plugin for `org-roam-capture'.
;;
;;; Code:

(require 's)

(with-eval-after-load 'org-roam-capture
  (defun orp-ok-capture-templates-merge (templates)
    "Merge org roam capture TEMPLATES to `org-roam-capture-templates'.x"
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
      (setopt org-roam-capture-templates result))))

(provide 'orp-ok-capture)
;;; orp-ok-capture.el ends here
