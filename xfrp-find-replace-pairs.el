;;; xfrp-find-replace-pairs.el --- elisp utility for string replacement. -*- coding: utf-8 -*-

;; Copyright © 2010, 2011, 2012, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Created: 2010-08-17
;; Keywords: emacs lisp, string, find replace

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; DESCRIPTION

;; this package is a emacs lisp utility.
;; It provides the following functions:

;; replace-pairs-in-string
;; replace-regexp-pairs-in-string
;; replace-pairs-region
;; replace-regexp-pairs-region
;; replace-pairs-in-string-recursive

;; these are convenient functions that lets you do multiple find/replace pairs.

;; For explanation of the need for these functions, see:
;;  http://ergoemacs.org/emacs/elisp_replace_string_region.html

;; donate $3 please. Paypal to xah@xahlee.org , thanks.

;;; INSTALL

;; Place the file in your emacs load path. Then
;; (require 'xfrp-find-replace-pairs)

;;; HISTORY

;; version 1.5.1, 2013-02-22 • major rewrite. Last version 1.5.0 had a bug too. So, the algorithm is changed again. On testing, version 1.4.6 is 9 seconds, version 1.5.0 is 12 seconds, version 1.5.1 is 6 seconds.
;; version 1.5.0, 2013-02-17 • major rewrite. The algorithm has changed. The prev algo is O(n^2). The new algo is O(n). The prev algo works by replacing each string to unique string, then replace them by replacement. Also, the new algorithm fixed a bug in “replace-pairs-region” and “replace-pairs-in-string”, when you have a lot replacement pairs and many of the find string are single char. Example: (let ((case-fold-search nil)) (replace-pairs-in-string "For a little fun today, i wrote “xah-convert-latin-alphabet-gothic”. This will replace all English alphabet by Gothic version (aka Blackletter, Fraktur) that's available in Unicode as characters. Here's the code." [ ["A" "𝔄"] ["B" "𝔅"] ["C" "ℭ"] ["D" "𝔇"] ["E" "𝔈"] ["F" "𝔉"] ["G" "𝔊"] ["H" "ℌ"] ["I" "ℑ"] ["J" "𝔍"] ["K" "𝔎"] ["L" "𝔏"] ["M" "𝔐"] ["N" "𝔑"] ["O" "𝔒"] ["P" "𝔓"] ["Q" "𝔔"] ["R" "ℜ"] ["S" "𝔖"] ["T" "𝔗"] ["U" "𝔘"] ["V" "𝔙"] ["W" "𝔚"] ["X" "𝔛"] ["Y" "𝔜"] ["Z" "ℨ"] ["a" "𝔞"] ["b" "𝔟"] ["c" "𝔠"] ["d" "𝔡"] ["e" "𝔢"] ["f" "𝔣"] ["g" "𝔤"] ["h" "𝔥"] ["i" "𝔦"] ["j" "𝔧"] ["k" "𝔨"] ["l" "𝔩"] ["m" "𝔪"] ["n" "𝔫"] ["o" "𝔬"] ["p" "𝔭"] ["q" "𝔮"] ["r" "𝔯"] ["s" "𝔰"] ["t" "𝔱"] ["u" "𝔲"] ["v" "𝔳"] ["w" "𝔴"] ["x" "𝔵"] ["y" "𝔶"] ["z" "𝔷"] ])) The unique strings are generated as a combination of rare Unicode char plus hexadecimal. The new algo generate a map of replacement positions instead.
;; version 1.4.6, 2012-07-05 • fixed several documentation error: mismatched paren in doc.
;; version 1.4.5, 2011-11-12 • added a optional argument to replace-regexp-pairs-region.
;; version 1.4.4, 2011-10-30 • fix a important error on documentation of replace-regexp-pairs-in-string, about the reversal of its 3rd argument fixedcase.
;; version 1.4.3, 2011-10-29 • major update on the implementation of “replace-pairs-region”, and minor update on others. No user visible change.
;; version 1.3, 2011-09-28 • slight change to replace-pairs-in-string to improve speed. The function's user level behavior is the same.
;; version 1.2, 2011-08-31 • change made to replace-pairs-region so that inserting occurs only if there are changes made. The function's user level behavior is the same, except the function might be slower when the region's text is large.
;; version 1.1, 2011-03-14. • fixed a doc error in replace-pairs-region. • fixed a code error in replace-regexp-pairs-in-string (this fix has no change in behavior).
;; version 1.0, 2010-08-17. First version.


;;; Code:

(defun replace-pairs-region (p1 p2 pairs)
  "Replace multiple PAIRS of find/replace strings in region P1 P2.

PAIRS should be a sequence of pairs, ℯℊ [[findStr1 replaceStr1] [findStr2 replaceStr2] …] It can be list or vector, for the elements or the entire argument.  

The find strings are not case sensitive. If you want case sensitive, set `case-fold-search' to nil. Like this: (let ((case-fold-search nil)) (replace-pairs-region …))

The replacement are literal and case sensitive.

Once a subsring in the input string is replaced, that part is not changed again.  For example, if the input string is “abcd”, and the pairs are a → c and c → d, then, result is “cbdd”, not “dbdd”. If you simply want repeated replacements, use `replace-pairs-in-string-recursive'.

Same as `replace-pairs-in-string' except does on a region.

Note: the region's text or any string in pairs is assumed to NOT contain any character from Unicode Private Use Area A. That is, U+F0000 to U+FFFFD. And, there are no more than 65534 pairs."
(let (
(unicodePriveUseA #xf0000)
ξi (tempMapPoints '()))
    ;; generate a list of Unicode chars for intermediate replacement. These chars are in  Private Use Area.
    (setq ξi 0)
    (while (< ξi (length pairs))
      (setq tempMapPoints (cons (char-to-string (+ unicodePriveUseA ξi)) tempMapPoints ))
      (setq ξi (1+ ξi))
      )
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)

        ;; replace each find string by corresponding item in tempMapPoints
        (setq ξi 0)
        (while (< ξi (length pairs))
          (goto-char (point-min))
          (while (search-forward (elt (elt pairs ξi) 0) nil t)
            (replace-match (elt tempMapPoints ξi) t t) )
          (setq ξi (1+ ξi))
          )

        ;; replace each tempMapPoints by corresponding replacement string
        (setq ξi 0)
        (while (< ξi (length pairs))
          (goto-char (point-min))
          (while (search-forward (elt tempMapPoints ξi) nil t)
            (replace-match (elt (elt pairs ξi) 1) t t) )
          (setq ξi (1+ ξi)) ) ) ) ) )

(defun replace-pairs-in-string (ξstr ξpairs)
  "Replace string ξstr by find/replace ξpairs sequence.

Returns the new string.

Example:
 (replace-pairs-in-string \"abcdef\"
 '([\"a\" \"1\"] [\"b\" \"2\"] [\"c\" \"3\"]))  ⇒ “\"123def\"”.

This function calls `replace-pairs-region' to do its work."
  (let (outputStr)
    (setq outputStr
          (with-temp-buffer
            (insert ξstr)
            (replace-pairs-region 1 (point-max) ξpairs)
            (buffer-string)
            )
          )
    outputStr
    ))

(defun replace-regexp-pairs-in-string (str pairs &optional fixedcase)
  "Replace string STR recursively by regex find/replace pairs PAIRS sequence.

The second argument PAIRS should be a sequence of pairs, e.g.
 [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
 It can be list or vector.

If third arg FIXEDCASE is non-nil, do not alter case of replacement text.
 (same as in `replace-match')

If you want the regex to be case sensitive, set the global
variable `case-fold-search' to “nil”. Like this: (let ((case-fold-search nil)) (replace-regexp-pairs-in-string …))

See also `replace-pairs-in-string'."
  (let ((myStr str))
    (mapc
     (lambda (x) (setq myStr (replace-regexp-in-string (elt x 0) (elt x 1) myStr fixedcase)))
     pairs)
    myStr))

(defun replace-regexp-pairs-region (p1 p2 pairs &optional fixedcase literal)
  "Replace regex string find/replace PAIRS in region.

P1 P2 are the region boundaries.

PAIRS is
 [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
 It can be list or vector.

The optional arguments FIXEDCASE and LITERAL is the same as in `replace-match'.

If you want the regex to be case sensitive, set the global
variable `case-fold-search' to “nil”. Like this: (let ((case-fold-search nil)) (replace-regexp-pairs-region …))"
  (let ( ξi currentPair (pairLength (length pairs)))
    (save-restriction
      (narrow-to-region p1 p2)
      (setq ξi 0)
      (while (< ξi pairLength)
        (setq currentPair (elt pairs ξi))
        (goto-char (point-min))
        (while (search-forward-regexp (elt currentPair 0) (point-max) t)
          (replace-match (elt currentPair 1) fixedcase literal) )
        (setq ξi (1+ ξi) ) ) ) ) )

(defun replace-pairs-in-string-recursive (str pairs)
  "Replace string STR recursively by find/replace pairs PAIRS sequence.

This function is similar to `replace-pairs-in-string', except that the replacement is done recursively after each find/replace pair.  Earlier replaced value may be replaced again.

For example, if the input string is “abcd”, and the pairs are a → c and c → d, then, the result is “dbdd” (not “cbdd”).

See `replace-pairs-in-string' for full doc."
  (let (myStr)
    (setq myStr str)
    (mapc
     (lambda (x) (setq myStr (replace-regexp-in-string (regexp-quote (elt x 0)) (elt x 1) myStr t t)))
     pairs)
    myStr))

(provide 'xfrp-find-replace-pairs)
