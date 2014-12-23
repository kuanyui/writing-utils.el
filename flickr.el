;; Flickr
;;
(require 'xml)
(require 'url)

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

(defvar flickr-use-curl nil
  "If non-nil, call shell command `curl` to retrieve data.
If nil, use `url.el' to do this.

If `curl` installed on your system, it's recommended to setq this to t,
because `url-retrieve' occurs GnuTLS error very often in our some testing.")

(if (and (executable-find "curl")
         (not (equal system-type 'ms-dos))
         (not (equal system-type 'windows-nt)))
    (setq flickr-use-curl t))


(defun flickr-set-default-size ()
  "Set variable `flickr-default-size' interactively."
  (interactive)
  (setq flickr-default-size
         (completing-read "Input size: "
                          '(nil large large1600 large2048 largesquare medium medium640 medium800 original small small320 square thumbnail) nil nil nil)))

(defun flickr-insert-html ()
  "Insert the raw link of a Flickr page ,with HTML tags attached.
For example, enter:

     http://www.flickr.com/photos/41522078@N05/11529752404/

And select size you like (tab completion is available), it will insert:

     <a href=\"http://www.flickr.com/photos/41522078@N05/11529799266/\"><img src=\"https://farm8.staticflickr.com/7420/11529799266_4e391575b0_z.jpg\" alt=\"\" class=\"\"></img></a>

With one C-u prefix, ignore `flickr-default-size' and always ask for size.

Variable `flickr-api-key' is required. Please get one first:
http://www.flickr.com/services/apps/create/apply/
And (setq flickr-api-key \"YOUR_API_KEY\")"
  (interactive)
  (let* ((major-mode 'html-mode))
    (flickr-insert-auto-format)
    ))

(defun flickr-insert-auto-format ()
  "Insert the raw link of a Flickr page, formatted according to current mode.
Input like: http://www.flickr.com/photos/41522078@N05/11529752404/
For example:
- HTML: <a href=\"http://www.flickr.com/photos/41522078@N05/11529752404/\"><img src=\"https://farm8.staticflickr.com/7420/11529799266_4e391575b0_z.jpg\" alt=\"\" class=\"\"></img></a>
- Markdown: ![](https://farm8.staticflickr.com/7420/11529799266_4e391575b0_z.jpg)
- Org: [[https://farm8.staticflickr.com/7420/11529799266_4e391575b0_z.jpg]]"
  (interactive)
  (let* ((raw (flickr-get-raw-link-interactively))
         (major-mode (if (member major-mode '(markdown-mode org-mode html-mode))
                         major-mode
                       (intern (concat (completing-read "Select a format: " '("org" "markdown" "html") nil t "" ) "-mode")))))
    (save-excursion
      (cond ((eq major-mode 'markdown-mode)
             (insert (format "![](%s)" (cdr raw))))
            ((eq major-mode 'org-mode)
             (insert (format "[[%s]]" (cdr raw))))
            ((eq major-mode 'html-mode)
             (insert (format "<a href=\"%s\"><img src=\"%s\" alt=\"\" class=\"\"></img></a>" (car raw) (cdr raw))))))
    (when (eq (current-column) 0)
      (end-of-line)
      (newline))
    ))

(defun flickr-process-whole-buffer ()
  (interactive)

  )

(defun flickr-get-raw-link-interactively (&optional input)
  "This is NOT an interactive function. Instead, it will automatically
 decide if interactive interface needed.
Output is a list like (INPUT . RAWLINK)."
  (if (null flickr-api-key)
      (message "You have to set Flickr API key first. C-h v flickr-api-key for more details.")
    (let* ((input (if input
                      input
                    (read-from-minibuffer "Flickr url: ")))
           (size-list (flickr-parse-xml (flickr-retrieve-xml input)))
           (specify-size flickr-default-size))
      (when (or (equal current-prefix-arg '(4))
                (null specify-size)
                (not (assq specify-size size-list)))
        (setq specify-size (intern (completing-read "Select size: " size-list nil t nil))))
      (cons input (cdr (assq specify-size size-list))))))


(defun flickr-parse-xml (flickr-xml)
  "Input should be a sizes list parsed from XML. Like this:
((size ((label . \"Square\") (width . \"75\") (height . \"75\")
        (source . \"https://farm8.staticflickr.com/7440/13963740818_37ffef157b_s.jpg\")
        (url . \"https://www.flickr.com/photos/41522078@N05/13963740818/sizes/sq/\")
        (media . \"photo\")))...)
And output is a pairs list for sizes and raw-link:
((original . \"https://farm8.staticflickr.com/7440/13963740818_17c4821702_o.png\")
 (large2048 . \"https://farm8.staticflickr.com/7440/13963740818_16f04a43ef_k.jpg\")...)"
  (let* ((sizes (car flickr-xml))
         (attrs (xml-node-attributes sizes))
         (size (xml-get-children sizes 'size))
         fin)
    (mapcar (lambda (x)
              (push (cons (intern (replace-regexp-in-string " " "" (downcase (cdr (assq 'label (cadr x))))))
                          (cdr (assq 'source (cadr x)))) fin))
            size)
    fin))

(defun flickr-retrieve-xml (flickr-url)
  "Input should be a flickr url, output is a raw XML string retrieve with Flickr API"
  (interactive)
  (string-match "https?://www.flickr.com/photos/[0-9A-z@]*/\\([0-9]+\\)/" flickr-url)

  (if flickr-use-curl
      (progn
	(switch-to-buffer (generate-new-buffer "curl-"))
	(save-excursion
	  (insert (shell-command-to-string
		   (format "curl 'https://www.flickr.com/services/rest/?method=flickr.photos.getSizes&photo_id=%s&api_key=%s' 2>/dev/null"
			   (match-string 1 flickr-url)
			   flickr-api-key)))))

    (switch-to-buffer
     (url-retrieve-synchronously
      (format "https://www.flickr.com/services/rest/?method=flickr.photos.getSizes&photo_id=%s&api_key=%s"
	      (match-string 1 flickr-url)
	      flickr-api-key))))
  (goto-char (point-min))
  (re-search-forward "<sizes" nil :no-error)(left-char 6)
  (setq fin (xml-parse-region (point) (point-max)))
  (kill-buffer)
  fin)

(define-key markdown-mode-map (kbd "C-c i f") 'flickr-insert-html)
(define-key org-mode-map (kbd "C-c i f") 'flickr-insert-auto-format)
(define-key html-mode-map (kbd "C-c i f") 'flickr-insert-auto-format)

(provide 'flickr)
