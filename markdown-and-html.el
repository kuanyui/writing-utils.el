;;插入blog的動態行間註解(需搭配CSS)
;; [FIXME] 請想個更好的function名...這個太容易忘記了
(defun html-insert-inline-note (begin end)
  (interactive "r")
  (let* ((title (if (region-active-p)
                    (buffer-substring-no-properties begin end)
                  (read-from-minibuffer "標題: ")))
         (content (read-from-minibuffer "內容: ")))
    (when (region-active-p)
      (delete-region begin end)
      (goto-char begin))
    (insert (format "<span class=\"note\">%s<span class=\"content\">%s</span></span>" title content))))
(define-key markdown-mode-map (kbd "C-c i n") 'html-insert-inline-note)


(defun html-insert-strike (begin end)
  (interactive "r")
  (let* ((text (if (region-active-p)
                    (buffer-substring-no-properties begin end)
                  (read-from-minibuffer "Content: "))))
    (when (region-active-p)
      (delete-region begin end)
      (goto-char begin))
    (insert (format "<strike>%s</strike>" text))))
(define-key markdown-mode-map (kbd "C-c i s") 'html-insert-strike)


(defface markdown-ruby-mark '((((class color) (background light)) (:foreground "#fff" :background "#6c0099"))
                              (((class color) (background dark)) (:foreground "#6c0099" :background "#d187ff"))) "" :group 'faces)
(defface markdown-ruby-title '((((class color) (background light)) (:foreground "#6c0099"))
                              (((class color) (background dark)) (:foreground "#d187ff"))) "" :group 'faces)
(defface markdown-ruby-content '((((class color) (background light)) (:foreground "#d187ff"))
                                 (((class color) (background dark)) (:foreground "#8700af"))) "" :group 'faces)
(font-lock-add-keywords 'markdown-mode
                          '(
                            ("\{\\(rb\\)\|.+?\|.+?\}" 1 'markdown-ruby-mark)
                            ("\{rb\|\\(.+?\\)\|.+?\}" 1 'markdown-ruby-title)
                            ("\{rb\|.+?\|\\(.+?\\)\}" 1 'markdown-ruby-content)
                            ))
