;;; orp-ok-capture.el --- Plugin for org-roam-capture  -*- lexical-binding: t -*-
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

(require 's)
(require 'org-roam-capture)

(defun ooc-templates-merge (templates)
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

(provide 'orp-ok-capture)

;; Local Variables:
;; read-symbol-shorthands: (("ooc" . "orp-ok-capture"))
;; End:
;;; orp-ok-capture.el ends here
