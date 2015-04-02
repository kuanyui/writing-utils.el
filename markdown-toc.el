;;; markdown-toc.el ---                              -*- lexical-binding: t; -*-
;; Generate TOC (Table of Contents) with inline CSS styled for markdown file:

;;; Code:

(defun markdown-toc-remove ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "^<div id=[\"']text-table-of-contents[\"'].+$" "")
    (goto-char (point-min))
    (replace-regexp "<a id=[\"']sec-[0-9]+[\"'] name=[\"']sec-[0-9]+[\"']></a>$" "")
  ))

(defvar markdown-toc-header-and-footer-html
  ["<div id=\"text-table-of-contents\" style='background-color:#f0f0f7;border-left:5px solid #568DBE;padding:10px 20px;margin:1em 0;display:table;'><h3 style='color:#505050;margin-top:10px !important;'>Contents</h3>"
   "</div>"])

(defun markdown-toc-insert ()
  "Insert a table of contents in the position of cursor.
Note this is only for format."
  (interactive)
  (markdown-toc-remove)
  (let (toc fin (num 1))
    (save-excursion
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^# ?\\([^#] ?.+\\)" nil :no-error)
          (when (and (not (eq (face-at-point) 'markdown-inline-code-face))
                     (not (eq (face-at-point) 'markdown-pre-face)))
            (push (cons num (match-string 1)) toc)
            (end-of-line)
            (insert (format "<a id=\"sec-%s\" name=\"sec-%s\"></a>" num num))
            (incf num))))
    (insert
     (format "%s%s%s"
             (elt markdown-toc-header-and-footer-html 0)
             (mapconcat
              (lambda (x)
                (format "<a href=\"#sec-%s\" style=\"color:#505050\">%s. %s</a><br>"
                        (car x) (car x) (markdown-toc-string-replacer (cdr x))))
              (reverse toc)
              "")
             (elt markdown-toc-header-and-footer-html 1))))))

(defun markdown-toc-string-replacer (string)
  "Remove markdown markup symbols (e.g. `*_ )."
  (mapcar
   (lambda (x)
     (setq string (replace-regexp-in-string x "" string)))
   '("`" "\\*" "_"))
  string)

;; ======================================================
;; TOC for GitHub
;; ======================================================

(defun markdown-github-insert-toc ()
  (interactive)
  (let* ((fin "")
	 pos)
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "^>\\*\\*Table of Contents\\*\\*\n\\(?:.\\|\n\\)*?\n\n" "")
      (setq pos (point)))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\#+\\) \\(.+\\)" nil :no-error)
      (let* ((depth (length (match-string 1)))
	     (name (match-string 2)))
	(setq fin (concat fin (format "%s- [%s](#%s)\n"
				      (make-string (* 2 (1- depth)) 32)
				      name
				      (replace-regexp-in-string
				       " " "-"
				       (replace-regexp-in-string
					"[^A-z0-9- ]" "" (downcase name)))
				      )))))
    (goto-char pos)
    (insert (format ">**Table of Contents**\n%s\n" fin))))



(provide 'markdown-toc)
;;; markdown-toc.el ends here
