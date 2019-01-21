;;; 00-00-utils.el --- 設定 - 独自ユーティリティ -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-21T23:01:53+09:00>

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 独自定義した関数・マクロの集合
;; `feature' 名 `my-utils'
;;
;; 疑似名前空間プレフィクスは `my-'

;;; Code:


;; ----------------------------------------------------------------------------
;; 依存解決
;; ----------------------------------------------------------------------------
(require 'fontset nil :noerror)


;; ----------------------------------------------------------------------------
;; 行移動
;; ----------------------------------------------------------------------------
;; see also:
;; http://gifnksm.hatenablog.jp/entry/20100131/1264956220
;; ----------------------------------------------------------------------------
;;;###autoload
(defmacro my-visual-line-beginning-position (&optional n)
  "Get cursor point of visual line beginning position.

N is same meaning of `beginning-of-visual-line' argument."
  `(save-excursion
     (beginning-of-visual-line ,n)
     (point)))

;;;###autoload
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


;; ----------------------------------------------------------------------------
;; ウインドウ移動
;; ----------------------------------------------------------------------------
;;;###autoload
(defun my-other-window-reverse (count &optional all-frames)
  "Move before window, reverse behavior of `other-window'.

COUNT and ALL-FRAMES are same arguments of `other-window'."
  (interactive "p")
  (other-window (- arg) all-frames))


;; ----------------------------------------------------------------------------
;; フレーム移動
;; ----------------------------------------------------------------------------
;;;###autoload
(defun my-other-frame-reverse (arg)
  "Move before frame, reverse behavior of `other-frame'.

ARG is same argument of `other-frame'."
  (interactive "p")
  (other-frame (- arg)))


;; ----------------------------------------------------------------------------
;; 折り返し表示
;; ----------------------------------------------------------------------------
;;;###autoload
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
    ;; see also:
    ;; `fill-column-indicator.el'
    (setq-local line-move-visual (not after))
    ;; `toggle-truncate-lines' は `truncate-partial-width-windows' が
    ;; non-nil だと何もしない
    ;; ゆえに `truncate-partial-width-windows' を `truncate-lines' の切替予定値と
    ;; 同値に変更し `toggle-truncate-lines' を強制的に機能させるように準備する
    (setq-local truncate-partial-width-windows after)
    ;; 残りは任せる
    (toggle-truncate-lines after)))


;; ----------------------------------------------------------------------------
;; 挿入
;; ----------------------------------------------------------------------------
;;;###autoload
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

;;;###autoload
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


;; ----------------------------------------------------------------------------
;; ディスプレイ
;; ----------------------------------------------------------------------------
;;;###autoload
(defmacro my-real-display-pixels-per-inch ()
  "Calculate real pixels per inch (ppi) by real display.

Return cons of (WIDTH-DPI . HEIGHT-DPI).

`display-pixels-per-inch' has invalid value in high resolution display."
  (declare (indent 0)
           (debug t))
  (let ((mm-to-inch-multiple 25.4))
    ;; PPI = px / (mm / 25.4)
    `(cons (/ (display-pixel-width)
              (/ (display-mm-width) ,mm-to-inch-multiple))
           (/ (display-pixel-height)
              (/ (display-mm-height) ,mm-to-inch-multiple)))))


;; ----------------------------------------------------------------------------
;; フォントセット
;; ----------------------------------------------------------------------------
;;;###autoload
(defmacro my-fallback-font-family (&rest families)
  "Return a first matched avaliable font-family in FAMILIES.

Return nil to all FAMILIES are unavaliabled."
  (declare (indent 0)
           (debug t))
  (let ((founded (make-symbol "founded"))
        (family (make-symbol "family")))
    `(catch ',founded
       (dolist (,family ',families)
         (if (find-font (font-spec :family ,family))
             (throw ',founded ,family))))))

;;;###autoload
(defmacro my-fallback-font-xlfd (&rest xlfds)
  "Return a first matched avaliable font XLFD in XLFDS.

Return nil to all XLFDS are unavaliabled."
  (declare (indent 0)
           (debug t))
  (let ((founded (make-symbol "founded"))
        (xlfd (make-symbol "xlfd")))
    `(catch ',founded
       (dolist (,xlfd ',xlfds)
         (if (find-font (font-spec :name ,xlfd))
             (throw ',founded ,xlfd))))))

;;;###autoload
(defun my-create-fontset-spec-string (name spec)
  "Return string for 1st argument of `create-fontset-from-fontset-spec'.

NAME is fontset name without \"fontset-\" prefix (example: \"fontset-NAME\").
SPEC is font object (`font-spec' return value)."
  (let ((fontset-spec (x-decompose-font-name
                       (font-xlfd-name (find-font spec)))))
    ;; XLFD のエンコーディング部をフォントセット名に置換
    (aset fontset-spec
          xlfd-regexp-registry-subnum
          (downcase (format "fontset-%s" name)))
    ;; フォントセット名を含んだ XLFD 文字列を生成
    (x-compose-font-name fontset-spec)))

;;;###autoload
(defmacro my-create-fontset-from-spec (name spec &rest options)
  "Create a fontset from \"fontset-NAME\" and font object SPEC.

NAME is fontset name (without \"fontset-\" prefix).
SPEC is font object (ex. `font-spec' function's return value).

OPTIONS are same arguments of `create-fontset-from-fontset-spec' after the 2nd.

Return the result of `create-fontset-from-fontset-spec'."
  (declare (indent 0)
           (debug t))
  `(create-fontset-from-fontset-spec (my-create-fontset-spec-string ,name ,spec)
                                     ,@options))

;;;###autoload
(defmacro my-set-fontset-font-safe (&rest args)
  "Return the result of `set-fontset-font' if don't cause error, or else nil.

ARGS are same arguments of `set-fontset-font'.

This feature seems to `car-safe' and `cdr-safe'."
  (declare (indent 0)
           (debug t))
  ;; 例外を無視
  `(ignore-errors (set-fontset-font ,@args)))


;; ----------------------------------------------------------------------------
;; lighter
;; ----------------------------------------------------------------------------
;;;###autoload
(defmacro my-change-lighter (minor-mode value)
  "Change MINOR-MODE's lighter to VALUE.

Return object of lignter."
  (declare (indent 0)
           (debug t))
  (let ((lighter (make-symbol "lighter")))
    `(let ((,lighter (cdr-safe (assoc ',minor-mode minor-mode-alist))))
       (setcar ,lighter ,value)
       ,lighter)))


;; ----------------------------------------------------------------------------
;; 一括エンコーディング変換
;; ----------------------------------------------------------------------------
;;;###autoload
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


(provide 'my-utils)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-00-utils.el ends here
