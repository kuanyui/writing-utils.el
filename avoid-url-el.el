;;; try-curl.el --- Try to use `curl` instead url.el to retrieve data
;;; This package exists due to the suck url.el
;;; Author: kuanyui (azazabc123@gmail.com)
;;; Code:
(require 'url)

(defvar avoid-url-el nil
  "If non-nil, call shell command `curl` and `wget` to retrieve data.
If nil, use `url.el' to do this.

If `curl` installed on your system, it's recommended to setq this to t,
because `url-retrieve' occurs GnuTLS error very often in our some testing.")

(if (and (executable-find "curl")
	 (executable-find "wget")
         (not (equal system-type 'ms-dos))
         (not (equal system-type 'windows-nt)))
    (setq avoid-url-el t))

(defmacro curl-get (url)
  "Try to use `curl` command to retrieve url, and switch to the
buffer which contains the content. Remember to `kill-buffer'
after using."
  `(progn
     (if ,avoid-url-el
	 (progn
	   (switch-to-buffer (generate-new-buffer "curl-"))
	   (save-excursion
	     (insert (shell-command-to-string
		      (format "curl -L '%s' 2>/dev/null" ,url)))))

       (switch-to-buffer
	(url-retrieve-synchronously ,url)))
     (goto-char (point-min))))

(defmacro wget (url path)
  "If wget exist, use it. Otherwise, `url-copy-file' in url.el
Retrieve url as a file to path (include filename, please)"
  `(if ,avoid-url-el
       (shell-command
	(format "wget '%s' -O '%s' 1>/dev/null 2>/dev/null" ,url ,path))
     (url-copy-file ,url ,path)))

(provide 'avoid-url-el)
;;; try-curl.el ends here
