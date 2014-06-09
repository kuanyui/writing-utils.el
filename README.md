# writing-utils
Some small tools making editing, writing articles (especially blog articles) more conveniently in Emacs.
> This package is still under developing, do not use.

# Packages list
- `writing-utils.el`
    - `M-up/down` to move current line up/down.
    - `C-c d l` to duplicate current line.
    - `C-a` to back to indentation first.
    - `M-x wc` to count English/Japanese/Chinese characters and words, **except for comments, org document options/attributions**.
    - Minibuffer enhancement:
        - `C-a` twice to erase minibuffer, without kill-ring pollution.
        - `M-<backspace>` to delete path back to parent directory, without kill-ring pollution.

- `flickr.el` retrieve, format and insert flickr photo raw link conveniently.

- `page-title` one key to:
    - Retrieve the title of HTML file via its URL, and insert to:
        - Markdown file.
        - Org file.
        - HTML file.
    -Share link with `twittering-mode` easily.

- `hexo.el` one-key to:
    - Create new post anywhere.
    - Move article between `_post/` and `_draft/`. 
    - Sort articles by their timestamp.
    - Update article's date stamp by current time.

- `html-entities-convert.el` convert html entities. e.g. `"&gt;" → ">"`

## Unknown usage
- `markdown-and-html` The name and usage of this package are still not determined ˊ・ω・ˋ.

# License
WTFPL 1.0
