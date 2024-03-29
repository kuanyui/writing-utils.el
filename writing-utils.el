;; writing-utils - Some utilities making writing articles / editing more convenient.
;; Author: kuanyui (azazabc123@gmail.com)
;; ========================================================================

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<up>") 'org-metaup)
  (define-key org-mode-map (kbd "ESC <up>") 'org-metaup)
  (define-key org-mode-map (kbd "M-<down>") 'org-metadown)
  (define-key org-mode-map (kbd "ESC <down>") 'org-metadown)
  )

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (cur-col (length (buffer-substring-no-properties (point-at-bol) (point)))))
    ;; ^ Don't blame me for this way that looks ugly.  If I use only
    ;; (current-column) to record position, a problem will occur when
    ;; current line contains Chinese or Japanese, because they are
    ;; multi-byte characters.
    (end-of-line) (insert "\n" text)
    (beginning-of-line) (right-char cur-col)))
;; (global-set-key (kbd "C-c l") 'duplicate-line)  ;; VSCode style
;; (global-set-key (kbd "C-c d l") 'duplicate-line)

(defun move-current-line-up ()
  (interactive)
  (when (> (line-number-at-pos) 1)
    (let ((cur-col (length (buffer-substring-no-properties (point-at-bol) (point))))
	  (cur-line (delete-and-extract-region (point-at-bol) (point-at-eol)))
          )
      (delete-char -1)
      (beginning-of-line)
      (insert cur-line "\n")
      (forward-line -1)
      (goto-char (+ cur-col (point-at-bol))))))
;; (global-set-key (kbd "M-<up>") 'move-current-line-up)
;; (global-set-key (kbd "ESC <up>") 'move-current-line-up)
(defun move-current-line-down ()
  (interactive)
  (when (not (eq (line-number-at-pos)
                 (count-lines (point-min) (point-max))))
    (let ((cur-col (length (buffer-substring-no-properties (point-at-bol) (point))))
	  (cur-line (delete-and-extract-region (point-at-bol) (point-at-eol))))
      (delete-char 1)
      (forward-line 1)
      (insert cur-line "\n")
      (forward-line -1)
      (goto-char (+ cur-col (point-at-bol))))))
;; (global-set-key (kbd "M-<down>") 'move-current-line-down)
;; (global-set-key (kbd "ESC <down>") 'move-current-line-down)

(defun copy-current-line ()
  "Copy current line into kill-ring."
  (interactive)
  (kill-new (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (message "Line copied."))
(global-set-key (kbd "C-c M-w") 'copy-current-line)

(defun indent-buffer ()
  "Similiar to C-M-\\, but indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; [Known bug] when bottom of buffer lacks of a newline.  But I don't
;; want to deal with it because as you save-buffer, Emacs add a new
;; line in the bottom of buffer.
;; http://www.emacswiki.org/emacs/MoveLine
;; This one has the same bug.

;; Improved C-a
(defun beginning-of-line-or-indentation ()
  "If current line indented, go to indented position, or go to
  beginning-of-line. Press second time, go to beginning-of-line
  whatever. "
  (interactive)
  (let ((indent-pos (progn
                      (save-excursion
                        (back-to-indentation)
                        (point)))))
    (if (eq (point) indent-pos)
        (beginning-of-line)
      (back-to-indentation))))

;; ======================================================
;; Insert commented seperator like this line
;; ======================================================
(defun insert-commented-separator()
  "Insert a commented separator in your code. Like this in
ELisp:
;; ======================================================
;; Title
;; ======================================================
Which makes code easier to read.
"
  (interactive)
  (let* ((line (make-string 54 (string-to-char "=")))
	 (comment-start (if (member major-mode '(emacs-lisp-mode lisp-mode))
			    ";; " comment-start))
	 (seperator (concat comment-start line)))
    (when (> (current-column) 0) (end-of-line) (newline))
    (insert (format "%s\n%s\n%s"
		    seperator comment-start seperator))
    (previous-line)
    ))
(global-set-key (kbd "C-c i M-;") 'insert-commented-separator)
(global-set-key (kbd "C-c M-;") 'insert-commented-separator)

;; ======================================================
;; Insert file path
;; ======================================================

(defun insert-file-path()
  (interactive)
  (insert (read-file-name "File Path: ")))
(global-set-key (kbd "C-c i p") 'insert-file-path)

;; ======================================================
;; 統計中英日文字數
;; ======================================================
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun wc ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在內
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉org文件的OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數（不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))

(defun zh-simplified-to-tranditional ()
  "Use iconv to convert simplified Chinese to traditional Chinese"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end)
                           "iconv -f utf8 -t gb2312 | iconv -f gb2312 -t big5 | iconv -f big5 -t utf8"
                           nil
                           t
                           ))

(provide 'writing-utils)
