;;; helm-pinyin.el --- helm pinyin support.          -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <dingyi@dingyi>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defsubst helm--mapconcat-pattern (pattern)
    "Transform string PATTERN in regexp for further fuzzy matching.
e.g helm.el$
    => \"[^h哈]*[h哈][^e额]*[e额][^l]*l[^m]*m[^.]*[.][^e]*e[^l]*l$\"
    ^helm.el$
    => \"helm[.]el$\"."
    (let ((ls (split-string-and-unquote pattern "")))
      (if (string= "^" (car ls))
          ;; Exact match.
          (mapconcat (lambda (c)
                       (if (and (string= c "$")
                                (string-match "$\\'" pattern))
                           c (regexp-quote c)))
                     (cdr ls) "")
        ;; Fuzzy match.
        (mapconcat (lambda (c)
                     (if (and (string= c "$")
                              (string-match "$\\'" pattern))
                         c (let ((pinyin-pattern (pinyinlib-build-regexp-string c)))
                             (if (< (length pinyin-pattern) 3)
                                 c
                               (format "[^%s]*%s" (substring pinyin-pattern 1 -1) pinyin-pattern)))))
                   ls ""))))

(defun helm-ff--transform-pattern-for-completion (pattern)
    "Maybe return PATTERN with it's basename modified as a regexp.
This happen only when `helm-ff-fuzzy-matching' is enabled.
This provide a similar behavior as `ido-enable-flex-matching'.
See also `helm--mapconcat-pinyin-pattern'
If PATTERN is an url returns it unmodified.
When PATTERN contain a space fallback to multi-match.
If basename contain one or more space fallback to multi-match.
If PATTERN is a valid directory name,return PATTERN unchanged."
    ;; handle bad filenames containing a backslash.
    (setq pattern (helm-ff-handle-backslash pattern))
    (let ((bn      (helm-basename pattern))
          (bd      (or (helm-basedir pattern) ""))
          ;; Trigger tramp connection with file-directory-p.
          (dir-p   (file-directory-p pattern))
          (tramp-p (cl-loop for (m . f) in tramp-methods
                            thereis (string-match m pattern))))
      ;; Always regexp-quote base directory name to handle
      ;; crap dirnames such e.g bookmark+
      (cond
       ((or (and dir-p tramp-p (string-match ":\\'" pattern))
            (string= pattern "")
            (and dir-p (<= (length bn) 2))
            ;; Fix Issue #541 when BD have a subdir similar
            ;; to BN, don't switch to match plugin
            ;; which will match both.
            (and dir-p (string-match (regexp-quote bn) bd)))
        ;; Use full PATTERN on e.g "/ssh:host:".
        (regexp-quote pattern))
       ;; Prefixing BN with a space call multi-match completion.
       ;; This allow showing all files/dirs matching BN (Issue #518).
       ;; FIXME: some multi-match methods may not work here.
       (dir-p (concat (regexp-quote bd) " " (regexp-quote bn)))
       ((or (not (helm-ff-fuzzy-matching-p))
            (string-match "\\s-" bn))    ; Fall back to multi-match.
        (concat (regexp-quote bd) bn))
       ((or (string-match "[*][.]?.*" bn) ; Allow entering wilcard.
            (string-match "/$" pattern)     ; Allow mkdir.
            (string-match helm-ff-url-regexp pattern)
            (and (string= helm-ff-default-directory "/") tramp-p))
        ;; Don't treat wildcards ("*") as regexp char.
        ;; (e.g ./foo/*.el => ./foo/[*].el)
        (concat (regexp-quote bd)
                (replace-regexp-in-string "[*]" "[*]" bn)))
       (t (concat (regexp-quote bd)
                  (if (>= (length bn) 2) ; wait 2nd char before concating.
                      (progn
                        ;; (print (helm--mapconcat-pinyin-pattern bn))
                        (helm--mapconcat-pinyin-pattern bn))
                    (concat ".*" (regexp-quote bn))))))))

(provide 'helm-pinyin)
;;; helm-pinyin.el ends here
