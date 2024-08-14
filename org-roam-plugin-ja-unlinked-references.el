;;; org-roam-plugin-ja-unlinked-references.el --- Unlinked References Section  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customize the unlinked references section for use with Japanese.
;;
;;; Code:

(require 'org-roam-plugin-ja-unlinked-references-patch) ;; uncomment this line when using without patch

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
  (advice-add
   'org-roam-plugin-ja-unlinked-references-title-regex
   :override
   (lambda (titles)
     (let ((bounded-re (substring (mapconcat
                                   #'org-roam-plugin-ja-unlinked-references-apply-word-boundary-re
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
           (lines-to-ignore '("begin_src +"
                              "filetags: "
                              "header-args: "
                              "PYTHONDONTWRITEBYTECODE=1 "
                              "transclude: ")))
       (format "'\\[\\[id:[0-9a-f-]+\\]\\[[^][]*(%s)[^][]*\\]\\]|%s(%s)'"
               bounded-re
               (format nlb (string-join lines-to-ignore "|"))
               bounded-re))))

  ;; TITLE SANITIZATION
  (advice-add
   'org-roam-unlinked-references--apply-word-boundary-re
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
   'org-roam-unlinked-references--result-filter-p
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
   'org-roam-unlinked-references-preview-line
   :around
   (lambda (orig-fun file row col file-prev row-prev col-prev)
     (if (and (string= file file-prev) (= row row-prev))
         ;; Use a "ditto" mark if the current line is
         ;; similar to the previous line.
         "⎯〃⎯"
       (funcall orig-fun file row col file-prev row-prev col-prev)))))

(provide 'org-roam-plugin-ja-unlinked-references)
;;; org-roam-plugin-ja-unlinked-references.el ends here
