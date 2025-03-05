;;; org-roam-ok-timestamps.el --- org-roam-timestamps extension  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
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
;; This package extends `org-roam-timestamps'.
;;
;;; Code:

(require 'dash)
(require 'org-roam-timestamps)

(defun org-roam-ok-timestamps--dates (n)
  "Get the list of times for the last N days."
  (let ((time (current-time))
        (result))
    (dotimes (_ n)
      (push time result)
      (setq time (time-add time (* -24 3600))))
    result))

;; NOTE(2025-03-05): The "mtime" is recorded as a property in
;; `org-roam-node', so SQL might get them faster than the following
;; method. Explore this avenue.
(defun org-roam-ok-timestamps--rg-args (days)
  "Get the ripgrep output for the files modified within the last DAYS."
  (let ((dates (format
                "(%s)"
                (string-join
                 (mapcar (lambda (time)
                           (substring (org-roam-timestamps-decode time)
                                      0 8))
                         (org-roam-ok-timestamps--dates days))
                 "|"))))
    (string-join
     (list "rg"
           (format "\":mtime:\s+%s\"" dates)
           "--context-separator '|'"
           "--field-context-separator '|'"
           "-b"
           org-roam-directory)
     " ")))

(defun org-roam-ok-timestamps-nodes (days)
  "Get a list of nodes modified within the last DAYS."
  (let* ((cmd (org-roam-ok-timestamps--rg-args days))
         (output (shell-command-to-string cmd))
         (items (mapcar
                 (lambda (line)
                   (string-match "\\(.+\\):\\([0-9]+\\):\\(.+\\)" line)
                   (list :file (match-string 1 line)
                         :line (string-to-number (match-string 2 line))
                         :match (match-string 3 line)))
                 (-filter (lambda (e) (not (string= e "")))
                          (string-split output "\n"))))
         nodes file result)
    (dolist (item items)
      (setq file (plist-get item :file)
            line (plist-get item :line))
      (push (car (org-roam-db-query
                  `[:select [nodes:id nodes:title nodes:properties] :from nodes
                            :where (and (= nodes:file ,file)
                                        (<= nodes:pos ,line))
                            :order-by [(desc nodes:pos)]
                            :limit 1]))
            result))
    result))

(defun org-roam-ok-timestamps--modified (days)
  "Get a list of nodes modified within the last DAYS."
  (let (result)
    (dolist (node (org-roam-ok-timestamps-nodes))
      (let* ((id (nth 0 node))
             (title (nth 1 node))
             (props (nth 2 node))
             (mtime (org-roam-timestamps-encode
                     (first (string-split
                             (alist-get "MTIME" props nil nil 'equal)
                             " ")))))
        (push `(:id ,id :title ,title :mtime ,mtime) result)))
    (sort result :key (lambda (item) (plist-get item :mtime)) :reverse t)))

(cl-defun org-roam-ok-timestamps-modified (&optional arg &key (days 7))
  "Get a list of nodes modified within the last DAYS.
When called with the `\\[universal-argument]' prefix ARG, the user will
be prompted for DAYS. When called non-interactively, DAYS can also be
set as a keyword argument, as in ':days DAYS'."
  (interactive "P")
  (let ((days (pcase arg
                ('(4) (string-to-number (read-string "Days: ")))
                (_ days))))
    (save-excursion
      (dolist (item (org-roam-ok-timestamps--modified days))
        (insert (format "- %s [[id:%s][%s]]\n"
                        (format-time-string "%Y-%m-%d" (plist-get item :mtime))
                        (plist-get item :id)
                        (plist-get item :title)))))))

(provide 'org-roam-ok-timestamps)
;;; org-roam-ok-timestamps.el ends here
