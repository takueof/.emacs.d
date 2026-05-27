;;; my-utils.el --- 設定 - 独自ユーティリティ -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2026 Taku WATABE
;; Time-stamp: <2026-05-27T16:26:04+09:00>

;; Author: Taku WATABE <taku.eof@gmail.com>
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
  ;; x - 1 行目の行末とみなし、本判定前にポイント位置を調整
  (if (and (not (equal (my-visual-line-beginning-position) (line-beginning-position)))
           (equal (my-visual-line-beginning-position) (point)))
      (backward-char))
  ;; 本判定
  (if (and (;; 1行目
            equal (my-visual-line-beginning-position) (line-beginning-position))
           (;; インデント途中ではない
            not (string-match
                 ;; HACK: Unicode 空白文字 [:space:] だけでは
                 ;;       インデント途中かどうか判定しきれない
                 ;;       そこで GNU Emacs の正規表現における空白文字 [\s-] も
                 ;;       追加することで補完する
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
;; バッファ
;; ============================================================================
(defun my-revert-buffer (&optional auto-save)
  "Run `revert-buffer-quick' and `normal-mode'.

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

(provide 'my-utils)


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; my-utils.el ends here
