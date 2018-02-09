;; page-title : Auto retrieve web page's title and insert with various format.
;; Author: kuanyui (azazabc123@gmail.com)
;; ========================================================================
;; markdown-insert-link-with-title
;; twittering-share-link

(require 'url)
(require 'html-entities-convert)
(require 'org)
(require 'sgml-mode)
(require 'markdown-mode)
(require 'recentf)
(require 'avoid-url-el)

(add-to-list 'recentf-exclude "/tmp/url-retrieve-.+")

(defun page-title-retrieve (url)
  (let* ((temp-file (format "%s%s" "/tmp/url-retrieve-" (random))))
    (wget url temp-file)
    (find-file temp-file)
    (goto-char (point-min))
    (re-search-forward "\<title\>\\(\\(:?.\\|\n\\)*?\\)\</title\>" nil :no-error)
    (setq url-gotten-page-title (html-entities-convert (match-string 1)))
                                        ;convert html entities
    (setq url-gotten-page-title (replace-regexp-in-string "[\n]" "" url-gotten-page-title))
    (kill-buffer)
    (delete-file temp-file)
    (format "%s" url-gotten-page-title)
    (message (format "%s" url-gotten-page-title))))



(defun insert-link-with-title ()
  "Insert link along with title (grabbed via url).
Format is determined by what major mode is being used currently.
Currently support: Markdown/Org/HTML/Twittering
If in beginning of a line, insert a - prefix as a list. For example:

Markdown:
- [The GNU Operating System](http://www.gnu.org/)
Org:
- [[http://www.gnu.org/][The GNU Operating System]]
"
  (interactive)
  (let* ((LINK (if (boundp 'LINK)
                   LINK
                 (read-from-minibuffer "Page's URL: ")))
         (TITLE (if (boundp 'TITLE)
                    TITLE
                  (page-title-retrieve LINK))))
    (cond ((eq major-mode 'markdown-mode)
           (if (not (equal 0 (current-column))) ;if not in the beginning of line,
               (insert (format "[%s](%s)" TITLE LINK)) ;insert "[title](link)"
             (progn (insert (format "- [%s](%s)" TITLE LINK))
                    (newline))))  ;or insert "- [title](link)" and new line.
          ((eq major-mode 'org-mode)
           (if (not (equal 0 (current-column)))
               (insert (format "[[%s][%s]]" LINK TITLE))
             (progn (insert (format "- [[%s][%s]]" LINK TITLE))
                    (newline))))
          ((eq major-mode 'html-mode)
           (insert (format "<a href=\"%s\">%s</a>" LINK TITLE)))
          ((eq major-mode 'twittering-edit-mode)
           (insert (format "\"%s\"( %s ) // " TITLE LINK)))
          (t
           (let (major-mode)
             (setq major-mode (intern (concat
                                       (completing-read "Select a format: "
                                                        '(("org")
                                                          ("markdown" )
                                                          ("html")
                                                          ("twittering-edit"))
                                                        nil t "" )
                                       "-mode")))
             (insert-link-with-title)))
          )))

(define-key markdown-mode-map (kbd "C-c i l") 'insert-link-with-title)
(define-key org-mode-map (kbd "C-c i l") 'insert-link-with-title)
(define-key html-mode-map (kbd "C-c i l") 'insert-link-with-title)

;; If twittering-mode has been installed and loaded.
(when (require 'twittering-mode nil 'no-error)
  (defun twittering-share-link ()
    "Share link with twittering-edit-mode buffer,
and insert page's title automatically.

If major-mode is already twittering-edit-mode, insterting
directly without opening a new buffer."
    (interactive)
    (if (not (equal major-mode 'twittering-edit-mode))
        (twittering-update-status-interactive) nil)
      (insert-link-with-title))
  (define-key twittering-edit-mode-map (kbd "C-c i l") 'insert-link-with-title)
  (define-key twittering-mode-map (kbd "C-c i l") 'twittering-share-link))

(provide 'page-title)
