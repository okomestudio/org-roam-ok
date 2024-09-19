;;; orp-ok-ja.el --- Japanese Plugin  -*- lexical-binding: t -*-
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
;; Configure the patched `org-roam' for the Japanese usage.
;;
;;; Code:

(require 'orp-ok-ja-patch) ;; uncomment this line when using without patch
(require 'adaptive-wrap)
(require 'ok-plural)

(with-eval-after-load 'org-roam-mode
  ;; WORD BOUNDARY
  (let* (;; Strict version:
         ;;
         ;; Unlike English, Japanese words are not delimited by character
         ;; word boundary. In this stricter version, we rely on "joshi"
         ;; particles (助詞; ja.wikipedia.org/wiki/%E5%8A%A9%E8%A9%9E) as
         ;; boundary indicator. This implicitly assumes titles (words to be
         ;; delimited by the word boundary) to be "meishi" (名詞).

         ;; The collection of joshi that comes on the right side of word:
         (joshi_r '("か" "が" "かしら" "がてら" "から" "きり" "くらい" "ぐらい" "こそ"
                    "さ" "さえ" "しか" "ずつ"
                    "だけ" "だの" "で" "では" "でも" "と" "とは" "とも"
                    "ながら" "なぞ" "など" "なり" "なんぞ" "に" "ね" "の" "のみ"
                    "は" "ばかり" "へ" "ほど"
                    "まで" "も"
                    "や" "やら" "よ" "より"
                    "を"))
         ;; The collection of joshi that comes on the left side of word:
         (joshi_l (append joshi_r
                          '("かい" "かり" "けど" "けれど" "けれども"
                            "し" "ぜ" "ぞ"
                            "たり" "つつ" "ってば" "て" "ても" "ところで" "とも"
                            "な" "ので" "のに"
                            "ば"
                            "まま" "ものか" "ものの" "もん"
                            "わ")))
         ;; These are not joshi, but nouns that can come before
         ;; (proper) nouns for descriptions, e.g., "政治家田中角栄":
         (joshi_l (append joshi_l
                          '("家" "スト")))

         (word-boundary-re-strict
          (concat "|(\\b%1$s\\b"
                  "|(?<="
                  (s-join "|" joshi_l) ")%1$s(?=" (s-join "|" joshi_r) "))"))

         ;; Lenient version:
         ;;
         ;; Use any Japanese characters as word boundary:
         (word-boundary-re-lenient
          (concat "|(\\b%1$s\\b"
                  "|(\\b%1$s\\b"
                  "|(?<=[^\x20-\x7e\xff61-\xff9f])%1$s(?=[^\x20-\x7e\xff61-\xff9f]))")))
    (setopt org-roam-unlinked-references-word-boundary-re word-boundary-re-strict))

  ;; TITLE EXTRACTION REGEX
  (defun orp-ok-ja--pluralize (title)
    "Pluralize noun(s) in TITLE."
    (let* ((tokens (string-split title " "))
           (pluralized-tokens (mapcar (lambda (t)
                                        (ok-plural-pluralize t))
                                      tokens)))
      ;; TODO: Expand the action based on permutations, not just the last token
      (string-join (append (butlast tokens)
                           `(,(cond
                               ((car (last pluralized-tokens))
                                (car (last pluralized-tokens)))
                               (t (car (last tokens))))))
                   " ")))

  (advice-add
   #'org-roam-unlinked-references--title-regex
   :override
   (lambda (titles)
     (let* (;; Extend titles with their plurals
            (titles (flatten-list (mapcar (lambda (w)
                                            (let ((p (orp-ok-ja--pluralize w)))
                                              (if p `(,w ,p) `(,w))))
                                          titles)))

            ;; Apply word boundaries to each title
            (bounded-re (substring (mapconcat
                                    #'org-roam-unlinked-references--apply-word-boundary-re
                                    titles "")
                                   1))

            ;; For variable-lengths lookbehinds in PCRE, see:
            ;;
            ;;   - www.drregex.com/2019/02/variable-length-lookbehinds-actually.html
            ;;
            ;; `plb' is for positive lookbehind; `nlb' is for negative lookbehind.
            (plb "(?=(?'a'[\\s\\S]*))(?'b'(%s)(?=\\k'a'\\z)|(?<=(?=x^|(?&b))[\\s\\S]))")
            (nlb "(?!(?=(?<a>[\\s\\S]*))(?<b>(%s)(?=\\k<a>\\z)|(?<=(?=x^|(?&b))[\\s\\S])))")

            ;; The list of substrings for negative matching:
            (lines-to-ignore '("PYTHONDONTWRITEBYTECODE=1 "
                               "begin_src.+"
                               "export_hugo_bundle:.+"
                               "filetags:.+"
                               "hugo_bundle:.+"
                               "hugo_tags:.+"
                               "parent:.+"
                               "property:.+"
                               "transclude:.+")))
       (format "'\\[\\[id:[0-9a-f-]+\\]\\[[^][]*(%s)[^][]*\\]\\]|%s(%s)'"
               bounded-re
               (format nlb (string-join lines-to-ignore "|"))
               bounded-re))))

  ;; TITLE SANITIZATION
  (advice-add
   #'org-roam-unlinked-references--apply-word-boundary-re
   :around
   (lambda (orig-func title)
     (let* (;; Expand quote variants:
            (s (replace-regexp-in-string " ['\‘]\\(\\w\\)" " ['\‘]\\1" title))
            (s (replace-regexp-in-string "\\(\\w\\)['\’]" "\\1['\’]" s))
            (s (replace-regexp-in-string " [\"\“]\\(\\w\\)" " [\"\“]\\1" s))
            (s (replace-regexp-in-string "\\(\\w\\)[\"\”]" "\\1[\"\”]" s))

            (s (funcall orig-func s))

            ;; Some special chars needs unescaping after `shell-quotes':
            (s (replace-regexp-in-string "[\\][[]\\([^][]+\\)[\\][]]" "[\\1]" s)))
       s)))

  ;; PREDICATE FOR UNLINKED REFERENCE CHECK
  (advice-add
   #'org-roam-unlinked-references--result-filter-p
   :override
   (lambda (matched-text matched-file row col titles node)
     (let ((linked-re (format "\\[\\[id:%s\\]\\[.*\\]\\]" (org-roam-node-id node))))
       (if (not (file-equal-p (org-roam-node-file node) matched-file))
           ;; The matched text is in a different file from the current node.
           (not (string-match linked-re matched-text)) ;; Is this reference unlinked?
         ;; The matched text is in the same file of the current node.
         (let* ((other-node (save-match-data
                              (with-current-buffer (find-file-noselect matched-file)
                                (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- row))
                                  (move-to-column col)
                                  (org-roam-node-at-point))))))
           (if (not (string-equal (org-roam-node-id node)
                                  (org-roam-node-id other-node)))
               ;; The matched text is in a different node from the
               ;; current node within the same file.
               (not (string-match linked-re matched-text)) ;; Is this reference unlinked?
             ))))))

  ;; PREVIEW LINE RENDERING
  (advice-add
   #'org-roam-unlinked-references-preview-line
   :around
   (lambda (orig-fun file row col file-prev row-prev col-prev)
     (if (and (string= file file-prev) (= row row-prev))
         ;; Use a "ditto" mark if the current line is
         ;; similar to the previous line.
         "⎯〃⎯"
       (funcall orig-fun file row col file-prev row-prev col-prev))))

  ;; ORG ROAM BUFFER FORMATTING
  (defun orp-ok-ja--org-roam-buffer-format ()
    "Format org-roam buffer for ease of viewing multi-line items."
    (turn-on-visual-line-mode)
    (setq-local adaptive-wrap-extra-indent 4)
    (adaptive-wrap-prefix-mode +1))

  (add-hook 'org-roam-mode-hook #'orp-ok-ja--org-roam-buffer-format))

(provide 'orp-ok-ja)
;;; orp-ok-ja.el ends here
