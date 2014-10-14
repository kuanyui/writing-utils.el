# writing-utils
Some small tools making editing, writing articles (especially blog articles) more conveniently in Emacs.

# Packages list
## writing-utils.el
- `M-up/down` to move current line up/down.
- `C-c d l` to duplicate current line.
- `C-a` to back to indentation first.
```lisp
;; only in programming-related mode:
(define-key prog-mode-map (kbd "C-a") 'beginning-of-line-or-indentation)
;; or in a global way:
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
```
- `C-c M-w` to copy current line into kill-ring.
- `M-x wc` to count English/Japanese/Chinese characters and words, **except for comments, org document options/attributions**.
- Minibuffer enhancement:
    - `C-a` twice to erase minibuffer, without kill-ring pollution.
    - `M-<backspace>` to delete path back to parent directory, without kill-ring pollution.
- `M-x indent-buffer` to indent the whole buffer (according to current major-mode syntax).

## flickr.el
`C-c i f` in markdown/org/html-mode to retrieve, format and insert flickr photo raw link conveniently.

> Notice: This function currently cannot retrieve your private photos.

## page-title
- One key `C-c i l` to retrieve the title of HTML file via its URL, and insert to:
    - Markdown file.
    - Org file.
    - HTML file.
    - `twittering-mode` buffer.

## hexo.el
- `M-x hexo-new`Create new post anywhere.
- `M-x hexo-move-article` Move article between `_post/` and `_draft/`. 
- `M-x hexo-touch-files-in-dir-by-time` Sort articles by their timestamp.
- `M-x hexo-update-current-article-date` Update article's date stamp by current time. **This may change its permanent link!**

## html-entities-convert.el
library to convert html entities. e.g. `"&gt;" → ">"`

## markdown-toc.el
Generate TOC (Table of Contents) with inline CSS styled for markdown file:
- `M-x markdown-toc-insert` Generate and insert/update TOC for current article.
- `M-x markdown-toc-remove` Remove TOC.

## markdown-and-html.el
>The name and usage of this package are still not determined ˊ・ω・ˋ.

- `M-x html-insert-strike` (or `C-c i s` in markdown-mode) to insert `<strike>text</strike>`.

# License
WTFPL 2.0

kuanyui (ɔ) Copyleft 2014

This program is free software. It comes without any warranty, to the extent permitted by applicable law. You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2, as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.
