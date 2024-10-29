;;; my-utils.el --- 設定 - 独自ユーティリティ -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2024 Taku Watabe
;; Time-stamp: <2024-10-29T09:56:45+09:00>

;; Author: Taku Watabe <taku.eof@gmail.com>
;; Keywords: display, mule, i18n, fontset, extensions lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 独自定義した関数・マクロの集合
;; `feature' 名 `my-utils'

;; 疑似名前空間プレフィクスは `my-'

;;; Code:


;; ============================================================================
;; 依存解決
;; ============================================================================
(autoload 'create-fontset-from-fontset-spec "fontset")
(autoload 'x-compose-font-name "fontset")


;; ============================================================================
;; 行移動
;; ============================================================================
;; See also:
;; http://gifnksm.hatenablog.jp/entry/20100131/1264956220
;; ============================================================================
(defmacro my-visual-line-beginning-position (&optional n)
  "Get cursor point of visual line beginning position.

N is same meaning of `beginning-of-visual-line' argument."
  `(save-excursion
     (beginning-of-visual-line ,n)
     (point)))

(defun my-beginning-of-smart-indented-line ()
  "Move curosr to beginning of indent.

Move to the beginning of the line if the cursor is at the beginning or middle of the indent."
  (interactive)

  ;; テスト:
  ;; あああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああああ

  ;; x 行目の先頭 (1 < x)
  (if (and (not (equal (my-visual-line-beginning-position) (line-beginning-position)))
           (equal (my-visual-line-beginning-position) (point)))
      ;; x - 1 行目の行末とみなして判定を再開
      (backward-char))

  (if (and (equal (my-visual-line-beginning-position) (line-beginning-position)) ; 1行目
           (not (string-match                                                    ; インデント途中でない
                 ;; Syntax Table で定義される空白文字 ([:space:]) だけでは
                 ;; インデント途中か否か判定できない場合もある
                 ;; 仕方ないので Emacs の正規表現が認識する全空白文字 (\s-) も包含
                 "^[[:space:]\s-]+$"
                 (buffer-substring-no-properties (my-visual-line-beginning-position)
                                                 (point)))))
      (back-to-indentation)
    (beginning-of-visual-line)))


;; ============================================================================
;; ウインドウ移動
;; ============================================================================
(defun my-other-window-reverse (count &optional all-frames)
  "Move before window, reverse behavior of `other-window'.

COUNT and ALL-FRAMES are same arguments of `other-window'."
  (interactive "p")
  (other-window (- count) all-frames))


;; ============================================================================
;; フレーム移動
;; ============================================================================
(defun my-other-frame-reverse (arg)
  "Move before frame, reverse behavior of `other-frame'.

ARG is same argument of `other-frame'."
  (interactive "p")
  (other-frame (- arg)))


;; ============================================================================
;; 折り返し表示
;; ============================================================================
(defun my-toggle-truncate-lines-force (&optional arg)
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
    ;; See also:
    ;; `fill-column-indicator.el'
    (setq-local line-move-visual (not after))
    ;; `toggle-truncate-lines' は `truncate-partial-width-windows' が
    ;; non-nil だと何もしない
    ;; ゆえに `truncate-partial-width-windows' を `truncate-lines' の
    ;; 切替予定値と同値に変更して `toggle-truncate-lines' を強制的に
    ;; 機能させるよう準備する
    (setq-local truncate-partial-width-windows after)
    ;; 残りは任せる
    (toggle-truncate-lines after)))


;; ============================================================================
;; バッファ
;; ============================================================================
(defun my-revert-buffer-quick-with-normal-mode (&optional auto-save)
  "Run `normal-mode' after `revert-buffer-quick'.

This function is force avoid the problem that `font-lock' could become invalid
 after running `revert-buffer-quick'.

AUTO-SAVE is same as 1st argument of `revert-buffer-quick'"
  (interactive "P")
  (revert-buffer-quick auto-save)
  (normal-mode))


;; ============================================================================
;; 挿入
;; ============================================================================
(defun my-insert-yen-sign ()
  "Insert YEN SIGN (U+00A5) character to cursor position."
  (interactive)
  (insert "¥"))

(defun my-insert-file-name (&optional name)
  "Insert current buffer's file name to cursor position.

NAME is string to target the string.
NAME is buffer to target the buffer file name.
NAME is other symbol to target the `current-buffer' file name.

Return string of file name."
  (interactive)
  (insert (convert-standard-filename
           (file-name-nondirectory
            (cond ((stringp name)
                   name)
                  (t
                   (buffer-file-name (if (bufferp name)
                                         name
                                       (current-buffer)))))))))

(defun my-insert-file-path (&optional name)
  "Insert current buffer's file path (full path) to cursor position.

NAME is string to target the string.
NAME is buffer to target the buffer file path.
NAME is other symbol to target the `current-buffer' file path.

Return string of file path."
  (interactive)
  (insert (convert-standard-filename
           (cond ((stringp name)
                  name)
                 (t
                  (buffer-file-name (if (bufferp name)
                                        name
                                      (current-buffer))))))))


;; ============================================================================
;; Input Method (IM)
;; ============================================================================
(defface my-cursor-default nil
  "`cursor' face for `current-input-method' is nil."
  :group 'customize)
(copy-face 'cursor 'my-cursor-default)

(defface my-cursor-input-method-activated '((t
                                             :background "gold"))
  "`cursor' face for `current-input-method' is non-nil."
  :group 'customize)

(defun my-change-cursor-faces-by-current-input-method ()
  "Change cursor color with `current-input-method'."
  (let* ((current-input-method (if (fboundp #'mac-input-source)
                                   (let ((input-source (mac-input-source)))
                                     (if (numberp (string-match "\\.US\\'" input-source))
                                         nil
                                       input-source))
                                 current-input-method))
         (cursor-face (if current-input-method
                          'my-cursor-input-method-activated
                        'my-cursor-default)))
    (set-cursor-color (face-attribute cursor-face :background))))


;; ============================================================================
;; フォントセット
;; ============================================================================
(defmacro my-fallback-font-family (&rest families)
  "Return a first matched avaliable font-family in FAMILIES.

Return nil to all FAMILIES are unavailable."
  (declare (indent 0)
           (debug t))
  (let ((founded (make-symbol "founded"))
        (family (make-symbol "family")))
    `(catch ',founded
       (dolist (,family ',families)
         (if (find-font (font-spec :family ,family))
             (throw ',founded ,family))))))

(defmacro my-set-fontset-font-safe (&rest args)
  "Return the result of `set-fontset-font' if don't cause error, or else nil.

ARGS are same arguments of `set-fontset-font'.

This feature seems to `car-safe' and `cdr-safe'."
  (declare (indent 0)
           (debug t))
  ;; 例外を無視
  `(ignore-errors (set-fontset-font ,@args)))


;; ============================================================================
;; 一括エンコーディング変換
;; ============================================================================
(defun my-change-files-coding-system (dir regexp coding-system recursive)
  "Convert CODING-SYSTEM for all files matched REGEXP in DIR.

RECURSIVE is non-nil to find files for recursive.

Return converted file numbers."
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


;; ============================================================================
;; フレーム
;; ============================================================================
(defun my-toggle-frame-transparency ()
  "Switch frame background transparency between translucent and opaque."
  (interactive)
  (set-frame-parameter nil
                       'alpha
                       (let ((alpha (frame-parameter nil 'alpha)))
                         (if (or (null alpha)
                                 (equal alpha '(100 . 100)))
                             '(80 . 50)
                           '(100 . 100)))))

(provide 'my-utils)


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; my-utils.el ends here
