;; writing-utils - Some utilities making writing articles more convenient.
;; Author: kuanyui (azazabc123@gmail.com)
;; ========================================================================
;; markdown-insert-link-with-title
;; twittering-share-link

(require 'url)
(require 'xfrp-find-replace-pairs)
(require 'html-entities-convert)

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
  "Insert markdown link along with title, which grabbed via url."
  (interactive)
  (let* ((LINK (read-from-minibuffer "Page's URL: "))
         (TITLE (url-get-page-title LINK)))
    (if (not (equal 0 (current-column))) ;if not in the beginning of line,
        (insert (format "[%s](%s)" TITLE LINK)) ;insert "[title](link)"
      (progn (insert (format "- [%s](%s)" TITLE LINK))
             (newline)))))  ;or insert "- [title](link)" and new line.
(define-key markdown-mode-map (kbd "C-c i l") 'markdown-insert-link-with-title)

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
(define-key twittering-mode-map (kbd "<f5>") 'twittering-share-link)



;; ========================================================================
;; Flickr
(defvar flickr-api-key nil
  "This variable is used by `flickr-insert-raw-link-with-html-tag'.
You can get a Flickr API:
http://www.flickr.com/services/apps/create/apply/
Then (setq flickr-api-key \"YOUR_API_KEY\")")

(defvar flickr-default-size nil
  "This variable is used by `flickr-insert-raw-link-with-html-tag'.

For example, you can set like this (setq flickr-default-size \"medium640\")
If the copy of \"medium640\" of the photo exists, apply it directly;
if not exist, it will ask for your size choice.

When nil, it ask you for raw image size everytime.
")

(defun flickr-set-default-size ()
  "Set variable `flickr-default-size' interactively."
  (interactive)
  (setq flickr-default-size
        (completing-read "Input size: "
                         '("nil" "large" "large1600" "large2048" "largesquare" "medium" "medium640" "medium800" "original" "small" "small320" "square" "thumbnail") nil nil nil)))

(defun flickr-insert-raw-link-with-html-tag ()
  "Insert the raw link of a Flickr page ,with HTML tags attached.
For example, enter:

     http://www.flickr.com/photos/41522078@N05/11529752404/

And select size you like (tab completion is available), it will insert:

     <a href=\"http://www.flickr.com/photos/41522078@N05/11529799266/\"><img src=\"https://farm8.staticflickr.com/7420/11529799266_4e391575b0_z.jpg\" alt=\"\" class=\"\"></img></a>

With one C-u prefix, ignore `flickr-default-size' and always ask for size.
With two C-u prefix, it only inserts raw link and always ask for size.

Variable `flickr-api-key' is required. Please get one first:
http://www.flickr.com/services/apps/create/apply/
And (setq flickr-api-key \"YOUR_API_KEY\")
"
  (interactive)
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (size-pattern "<size label=\"\\(.*?\\)\".*source=\"\\(.*?\\)\"")
         (url-pattern "https?://www.flickr.com/photos/[0-9A-z@]*/\\([0-9]+\\)/")
         (flickr-html-template "<a href=\"%s\"><img src=\"%s\" alt=\"\" class=\"\"></img></a>")
         (flickr-url (read-from-minibuffer "Flickr URL: "))
         (size-list '())               ; the list of all pic size.
         url-buffer
         flickr-photo-id)

    (if (string-match url-pattern flickr-url)
        (setq flickr-photo-id (match-string 1 flickr-url))
      (progn
        (message "This is not a valid Flickr photo page link. A valid example:
http://www.flickr.com/photos/12037949754@N01/155761353/")
        (sleep-for 3)
        (insert-flickr-raw-link-with-html-tag)))

    (setq url-buffer
          (url-retrieve-synchronously
     (format "https://www.flickr.com/services/rest/?method=flickr.photos.getSizes&photo_id=%s&api_key=%s" flickr-photo-id flickr-api-key)))
    (switch-to-buffer url-buffer)
    (goto-char (point-min))

      (if (not (string-match size-pattern (buffer-string))) ;if cannot find any size-pattern
          (progn (kill-buffer)
                 (message "Encounter problem (it may be a private photo?), please try again.")
                 (sleep-for 3)
                 (insert-flickr-raw-link-with-html-tag)))

    (while (re-search-forward size-pattern nil :no-error)
      (let ((size (match-string 1))
            (raw-link (match-string 2)))
        (setq size (downcase (replace-regexp-in-string " " "" size))) ;downcase `size' strings
        (push
         (list size raw-link)
         size-list)))
      (kill-buffer)

      ;; Universal Argument
      (let (raw-link)
        (cond
         ;; if no prefix
         ((equal current-prefix-arg nil)
          (if (assoc flickr-default-size size-list) ;if default-size exist in `size-list'
              (progn                    ;insert that without asking! (with html tags)
                (setq raw-link (cadr (assoc flickr-default-size size-list)))
                (insert (format flickr-html-template flickr-url raw-link)))
            (progn                      ;if default-size not exist in `size-list'
              (setq raw-link
                    (cadr (assoc        ;ask for user with minibuffer
                           (completing-read "Select size: " size-list nil t nil) size-list)))
              (insert (format flickr-html-template flickr-url raw-link)))))

         ;; if one C-u
         ((equal current-prefix-arg '(4)) ;ask for user with minibuffer
          (setq raw-link
                (cadr (assoc
                       (completing-read "Select size: " size-list nil t nil) size-list)))
          (insert (format flickr-html-template flickr-url raw-link))) ;with html tags
         ;; if two C-u
         ((equal current-prefix-arg '(16))
          (setq raw-link
                (cadr (assoc
                       (completing-read "Select size: " size-list nil t nil) size-list)))
          (insert (format "%s" raw-link)))))))

;; Avoid recentf add temporary files.
(add-to-list 'recentf-exclude "/tmp/url-retrieve-.+'")

(define-key markdown-mode-map (kbd "C-c i f") 'flickr-insert-raw-link-with-html-tag)
