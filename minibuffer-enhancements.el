;;; minibuffer-enhancements.el --- Enhancements for minibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
;; [English] Enhanced minibuffer & find-file!  I cannot bear helm &
;; ido-mode's UI, but theirs some features are very juicy. So:

;;   1. If minibuffer contains a file path, M-[DEL] to delete back to
;;   parent dir
;;   2. Press C-a is the normal C-a, but when you press it 2-times:
;;       (a) if a file path, delete all except for ~/ or /
;;       (b) if not a file path, delete the whole line.
;;   3. Above actions won't pollute kill-ring. So feel free to use them!

;; [Chinese]
;; 加強minibuffer和find-file

;; 我實在無法忍受helm和ido-mode的find-file設計，但又覺得他們有部份功能
;; 實在很方便，所以：

;;   1. 如果minibuffer中是個目錄的樣式，按M-[DEL]就可以往前刪到parent dir
;;   2. 按一次C-a只是一般的beginning-of-line，但按第二次C-a的話：
;;       (a) 如果是個路徑，會把~/或/以後的東西刪掉。
;;       (b) 如果不是路徑，則整行刪掉。
;;   3. 以上行為都不會把刪過的東西存到 kill-ring，所以可以放心用力刪，
;;      而不用擔心會影響到目前的 kill-ring~

(defun minibuffer-beginning-of-line ()
  "Pressing C-a once, this's just a normal `beginning-of-line'.
When pressing second time, and the string in minibuffer looks
like a file path, it will *delete* whole minibuffer except for ~/
or / in the beginning of minibuffer."
  (interactive)
  (if (not (eq (minibuffer-prompt-end) (point)))
      (move-beginning-of-line 1)
    (if (or (equal "~/" (substring-no-properties (buffer-string) (1- (point)) (+ 1 (point))))
            (equal "/" (substring-no-properties (buffer-string) (1- (point)) (point))))
        (progn
          (re-search-forward "/" nil :no-error)
          (delete-region (point) (point-max)))
      (delete-region (point) (point-max)))))

(defun minibuffer-backward-delete-word (arg)
  "*Delete* word backward instead of kill it in minibuffer.
Besides, when the string in minibuffer looks like a file path, it will
delete backward until the parent directory."
  (interactive "p")
  (if (and (eq (point) (point-max))
           (string-match "~*/\\([^/\n]+/?\\)+$" (buffer-string)))
      (progn (re-search-backward "/." nil :no-error)
             (delete-region (1+ (point)) (point-max))
             (end-of-line))
    (delete-region (point) (progn (backward-word arg) (point)))))

(define-key minibuffer-local-completion-map (kbd "C-a") 'minibuffer-beginning-of-line)
(define-key minibuffer-local-completion-map (kbd "M-DEL") 'minibuffer-backward-delete-word)
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-a") 'minibuffer-beginning-of-line)
  (define-key vertico-map (kbd "M-DEL") 'minibuffer-backward-delete-word)
  )
;; ======================================================
;; Enhancement for *Completions*
;; ======================================================

;; Press Shift-Tab to jump into completions buffer! ｡:.ﾟヽ(*´∀`)ﾉﾟ.:｡

(define-key completion-list-mode-map (kbd "TAB") 'next-completion)
(define-key completion-list-mode-map (kbd "<backtab>") 'previous-completion)
(define-key minibuffer-local-map (kbd "<backtab>") 'goto-completions-buffer)

(defun goto-completions-buffer ()
  "This should only be called interactively in minibuffer"
  (interactive)
  (let ((name "*Completions*"))
    (if (not (get-buffer-window name))  ;if *Completions* window is hidden
                                        ;(remained by previous complete action)
        (progn (if (get-buffer name) (kill-buffer name)) ;kill *Completions* if it exists
               (with-current-buffer (window-buffer (minibuffer-window))
                 (minibuffer-completion-help (minibuffer-prompt-end) (point-max)))))
    (when (get-buffer-window name)
      (switch-to-buffer-other-window name)
      (next-completion 1))))

(provide 'minibuffer-enhancements)
;;; minibuffer-enhancements.el ends here
