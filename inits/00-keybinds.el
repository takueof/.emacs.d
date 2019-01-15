;;; 00-keybinds.el --- 設定 - グローバル キーバインド -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-15T13:11:47+09:00>

;;; Commentary:

;;; Code:

;; Backspace と DEL を 交換
(keyboard-translate ?\C-h ?\C-?)

;; DEL を C-d にする
(keyboard-translate ?\C-? ?\C-d)

;; `ido-undo-merge-work-directory' 実行のため C-z を押しすぎた場合、
;; `suspend-frame' が起動しないよう配慮
(global-unset-key (kbd "C-z"))

;; ヘルプ表示を割り当てなおす
(if (fboundp 'help-command)
    (global-set-key (kbd "C-x ?") #'help-command))

;; ウインドウ中央表示はもっともシンプルなものを用いる
;; `recenter-top-bottom' は使わない
(if (fboundp 'recenter)
    (global-set-key (kbd "C-l") #'recenter))

;; リージョン範囲をソート
(if (fboundp 'sort-lines)
    (global-set-key (kbd "C-c C-c C-s") #'sort-lines))

;; 1つ前のエラーを表示
(if (fboundp 'previous-error)
    (global-set-key (kbd "C-x \\") #'previous-error))


;; ----------------------------------------------------------------------------
;; 行頭移動は物理行
;; ----------------------------------------------------------------------------
;; see also:
;; http://gifnksm.hatenablog.jp/entry/20100131/1264956220
;; ----------------------------------------------------------------------------
(defmacro visual-line-beginning-position (&optional n)
  "Get cursor point of visual line beginning position.
N is same meaning of `beginning-of-visual-line' argument."
  `(save-excursion
     (beginning-of-visual-line ,n)
     (point)))

(defun beginning-of-smart-indented-line ()
  "Move curosr to beginning of indent.
Move to the beginning of the line if the cursor is at the beginning or middle of the indent."
  (interactive)

  ;; テスト:
  ;; あああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああ

  ;; x 行目の先頭 (1 < x)
  (if (and (not (equal (visual-line-beginning-position) (line-beginning-position)))
           (equal (visual-line-beginning-position) (point)))
      ;; x - 1 行目の行末とみなして判定を再開
      (backward-char))

  (if (and (equal (visual-line-beginning-position) (line-beginning-position)) ; 1行目
           (not (string-match                                                 ; インデント途中でない
                 ;; Syntax Table で定義される空白文字 ([:space:]) だけでは
                 ;; インデント途中か否か判定できない場合もある
                 ;; 仕方ないので Emacs の正規表現が認識する全空白文字 (\s-) も包含
                 "^[[:space:]\s-]+$"
                 (buffer-substring-no-properties (visual-line-beginning-position)
                                                 (point)))))
      (back-to-indentation)
    (beginning-of-visual-line)))

(global-set-key (kbd "C-a") #'beginning-of-smart-indented-line)


;; ----------------------------------------------------------------------------
;; 前のウインドウに移動
;; ----------------------------------------------------------------------------
(defun other-window-reverse (count &optional all-frames)
  "Move before window, reverse behavior of `other-window'.
COUNT and ALL-FRAMES are same meaning of `other-window' arguments."
  (interactive "p")
  (other-window (- arg) all-frames))

(global-set-key (kbd "C-x p") #'other-window-reverse)


;; ----------------------------------------------------------------------------
;; 前のフレームに移動
;; ----------------------------------------------------------------------------
(defun other-frame-reverse (arg)
  "Move before frame, reverse behavior of `other-frame'.
ARG is same meaning of `other-frame' argument."
  (interactive "p")
  (other-frame (- arg)))

(global-set-key (kbd "C-x 5 p") #'other-frame-reverse)


;; ----------------------------------------------------------------------------
;; 折り返し表示を強制切替
;; ----------------------------------------------------------------------------
(defun toggle-truncate-lines-force (&optional arg)
  "Force switch the current buffer's display wrapping.
ARG is non-nil to wrapping, or nil to no wrapping.
It doesn't depend on `truncate-partial-width-windows' of `toggle-truncate-lines'."
  (interactive "P")
  (let ((after (if (null arg)
                   (not truncate-lines)
                 (> (prefix-numeric-value arg) 0))))
    ;; 物理行移動はバッファの折り返し表示が有効でなければ意味がない
    ;; ゆえに強制切替してムダを省く
    ;;
    ;; see also:
    ;; `fill-column-indicator.el'
    (setq-local line-move-visual (not after))
    ;; `toggle-truncate-lines' は `truncate-partial-width-windows' が non-nil だと
    ;; 何もしない
    ;; ゆえに `truncate-partial-width-windows' を `truncate-lines' の切替予定値と
    ;; 同値に変更し `toggle-truncate-lines' を強制的に機能させるように準備する
    (setq-local truncate-partial-width-windows after)
    ;; 残りは任せる
    (toggle-truncate-lines after)))

(global-set-key (kbd "C-x w") #'toggle-truncate-lines-force)


;; ----------------------------------------------------------------------------
;; カーソル位置にファイル名を挿入
;; ----------------------------------------------------------------------------
(defun insert-file-name (&optional name)
  "Insert current buffer's file name to cursor position.
If NAME is string, it is treated as a file path.
If NAME is buffer, it targets the file name of the buffer.
If NAME is other symbol, it targets file name of the `current-buffer'."
  (interactive)
  (insert (convert-standard-filename
           (file-name-nondirectory
            (cond
             ((stringp name)
              name)
             (t
              (buffer-file-name (if (bufferp name)
                                    name
                                  (current-buffer)))))))))

(global-set-key (kbd "C-c i f") #'insert-file-name)


;; ----------------------------------------------------------------------------
;; カーソル位置にファイルパスを挿入
;; ----------------------------------------------------------------------------
(defun insert-file-path (&optional name)
  "Insert current buffer's file path (full path) to cursor position.
If NAME is string, it is treated as a file path.
If NAME is buffer, it targets the file name of the buffer.
If NAME is other symbol, it targets file name of the `current-buffer'."
  (interactive)
  (insert (convert-standard-filename
           (cond
            ((stringp name)
             name)
            (t
             (buffer-file-name (if (bufferp name)
                                   name
                                 (current-buffer))))))))

(global-set-key (kbd "C-c i p") #'insert-file-path)


;; ----------------------------------------------------------------------------
;; 一括エンコーディング変換
;; ----------------------------------------------------------------------------
(defun my-change-files-coding-system (dir regexp coding-system recursive)
  "Convert CODING-SYSTEM for all files matched REGEXP in DIR.
If RECURSIVE is non-nil, find files for recursive.
Return file numbers of converted."
  (interactive
   (list (read-directory-name "Target directory: ")
         (read-regexp "File name (RegExp): ")
         (read-coding-system "Coding system: ")
         (y-or-n-p "Recursive search? ")))
  (let* ((target-files (if recursive
                           (progn
                             (require 'find-lisp nil :noerror)
                             (declare-function find-lisp-find-files "find-lisp")
                             (find-lisp-find-files dir regexp))
                         (directory-files dir t regexp t)))
         (target-files-length (safe-length target-files))
         (before-buffer (current-buffer)))
    (if (< 0 target-files-length)
        (save-excursion
          (with-temp-buffer
            ;; 最終行処理は絶対にやらせない
            (setq-local mode-require-final-newline nil)
            (setq-local require-final-newline nil)
            (dolist (path target-files)
              ;; 変換開始
              (set-visited-file-name path t)
              (insert-file-contents path nil nil nil t) ; `erase-buffer' と同等の処理も実施
              (set-buffer-file-coding-system coding-system)
              (save-buffer)))
          (set-buffer before-buffer)))
    (message "%d files converted to `%S'" target-files-length coding-system)
    target-files-length))

(global-set-key (kbd "C-c RET f") #'my-change-files-coding-system)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-keybinds.el ends here
