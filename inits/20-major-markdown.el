;;; 20-major-markdown.el --- 設定 - メジャーモード - Markdown ドキュメント

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-10T19:55:00+09:00>

;;; Commentary:

;;; Code:

(package-install 'markdown-mode)
(package-install 'markdown-mode+)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; FIXME: "marked" を使うと文字化けする
 ;;        1度コマンドラインを通してるので、エンコーディングを確認すること
 '(markdown-command (or (executable-find "marked")
                        (executable-find "md2html")
                        (executable-find "markdown")
                        "markdown"))
 '(markdown-coding-system 'utf-8-unix))


;; ----------------------------------------------------------------------------
;; PATCH: 見出しは行頭だけにする
;; ----------------------------------------------------------------------------
(eval-after-load 'markdown-mode
  '(progn
     (defun markdown-insert-header (&optional level text setext)
       "Insert or replace header markup.
The level of the header is specified by LEVEL and header text is
given by TEXT.  LEVEL must be an integer from 1 and 6, and the
default value is 1.
When TEXT is nil, the header text is obtained as follows.
If there is an active region, it is used as the header text.
Otherwise, the current line will be used as the header text.
If there is not an active region and the point is at a header,
remove the header markup and replace with level N header.
Otherwise, insert empty header markup and place the cursor in
between.
The style of the header will be atx (hash marks) unless
SETEXT is non-nil, in which case a setext-style (underlined)
header will be inserted."
       (interactive "p\nsHeader text: ")
       (setq level (min (max (or level 1) 1) (if setext 2 6)))
       ;; Determine header text if not given
       (when (null text)
         (if (markdown-use-region-p)
             ;; Active region
             (setq text (delete-and-extract-region (region-beginning) (region-end)))
           ;; No active region
           (markdown-remove-header)
           (setq text (delete-and-extract-region
                       (line-beginning-position) (line-end-position)))
           (when (and setext (string-match "^[ \t]*$" text))
             (setq text (read-string "Header text: "))))
         (setq text (markdown-compress-whitespace-string text)))
       ;; Insertion with given text
       (markdown-ensure-blank-line-before)
       (let (hdr)
         (cond (setext
                (setq hdr (make-string (string-width text) (if (= level 2) ?- ?=)))
                (insert text "\n" hdr))
               (t
                (setq hdr (make-string level ?#))
                ;; ここを変更し、行末には何も追加させないようにした
                (insert hdr " " text))))
       (markdown-ensure-blank-line-after)
       ;; Leave point at end of text
       (if setext
           (backward-char (1+ (string-width text)))
         ;; カーソル位置変更はしない
         ;; 行末には何も追加させないようにしたため、位置変更が不要となった
         ))))


;; ----------------------------------------------------------------------------
;; PATCH: HTML5 文書を作成させる
;; ----------------------------------------------------------------------------
;; TODO: オリジナルの関数への参照を
;;       `original-markdown-add-xhtml-header-and-footer' に残したい
(eval-after-load 'markdown-mode
  '(progn
     (defun markdown-add-xhtml-header-and-footer (title)
       "Customized wrap HTML header and footer with given TITLE around current buffer."
       (insert "<!DOCTYPE html>\n"
               "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"ja\" lang=\"ja\">\n"
               "<head>\n")
       (insert "<title>" title "</title>\n")

       (insert "<meta charset=\""
               (format "%s" (if (fboundp 'coding-system-get)
                                (coding-system-get (or markdown-coding-system
                                                       buffer-file-coding-system)
                                                   'mime-charset)
                              "iso-8859-1"))
               "\" />\n")

       (if (> (length markdown-css-path) 0)
           (insert "<link rel=\"stylesheet\" href=\"" markdown-css-path "\" />\n"))

       (if (> (length markdown-xhtml-header-content) 0)
           (insert markdown-xhtml-header-content "\n"))

       (insert "</head>\n"
               "<body>\n")

       (goto-char (point-max))

       (insert "\n"
               "</body>\n"
               "</html>\n"))))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-markdown-mode-initialize ()
  "Initialize `markdown-mode' before file load."
  (setq-local indent-tabs-mode nil))

(add-hook 'markdown-mode-hook #'my-markdown-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; プレーンテキストファイルは除外
(setq auto-mode-alist
      (delete '("\\.text\\'" . markdown-mode) auto-mode-alist))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-markdown.el ends here
