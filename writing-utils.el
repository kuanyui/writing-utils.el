;; writing-utils - Some utilities making writing articles more convenient.
;; Author: kuanyui (azazabc123@gmail.com)
;; ========================================================================
;; markdown-insert-link-with-title
;; twittering-share-link

(require 'url)
(require 'html-entities-convert)
(require 'org)

(defun url-get-page-title (url)
  (let* ((temp-file (format "%s%s" "/tmp/url-retrieve-" (random))))
    (url-copy-file url temp-file)
    (find-file temp-file)
    (goto-char (point-min))
    (re-search-forward "\<title\>\\(\\(:?.\\|\n\\)*?\\)\</title\>" nil :no-error)
    (setq url-gotten-page-title (html-entities-convert (match-string 1)))
                                        ;convert html entities
    (setq url-gotten-page-title (replace-regexp-in-string "\n" "" url-gotten-page-title))
                                       ; remove \n in title
    (kill-buffer)
    (delete-file temp-file)
    (format "%s" url-gotten-page-title)
    (message (format "%s" url-gotten-page-title))))

(defun markdown-insert-link-with-title ()
  "Insert markdown link along with title, which grabbed via url.
If in beginning of a line, insert a - prefix as a list. For example:
- [The GNU Operating System](http://www.gnu.org/)
"
  (interactive)
  (let* ((LINK (read-from-minibuffer "Page's URL: "))
         (TITLE (url-get-page-title LINK)))
    (if (not (equal 0 (current-column))) ;if not in the beginning of line,
        (insert (format "[%s](%s)" TITLE LINK)) ;insert "[title](link)"
      (progn (insert (format "- [%s](%s)" TITLE LINK))
             (newline)))))  ;or insert "- [title](link)" and new line.
(define-key markdown-mode-map (kbd "C-c i l") 'markdown-insert-link-with-title)

(defun org-insert-link-with-title ()
  "Insert org link along with title, which grabbed via url."
  (interactive)
  (let* ((LINK (read-from-minibuffer "Page's URL: "))
         (TITLE (url-get-page-title LINK)))
    (if (not (equal 0 (current-column))) ;if not in the beginning of line,
        (insert (format "[[%s][%s]]" LINK TITLE))
      (progn (insert (format "- [[%s][%s]]" LINK TITLE))
             (newline)))))
(define-key org-mode-map (kbd "C-c i l") 'org-insert-link-with-title)

;; If twittering-mode has been installed and loaded.
(when (boundp 'twittering-mode-hook)
  (require 'twittering-mode)
  (defun twittering-share-link ()
    "Share link with twittering-edit-mode buffer,
and insert page's title automatically.

If major-mode is already twittering-edit-mode, insterting
directly without opening a new buffer."
    (interactive)
    (let* ((LINK (read-from-minibuffer "Page's URL: "))
           (TITLE (url-get-page-title LINK)))
      (if (not (equal major-mode 'twittering-edit-mode))
          (twittering-update-status-interactive) nil)
      (insert (format "\"%s\"( %s ) // " TITLE LINK))))
  (define-key twittering-edit-mode-map (kbd "<f5>") 'twittering-share-link)
  (define-key twittering-mode-map (kbd "<f5>") 'twittering-share-link))
