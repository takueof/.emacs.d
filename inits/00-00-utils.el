;;; 00-00-utils.el --- 設定 - 独自ユーティリティ -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:24:57+09:00>

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

;; 独自定義した関数・マクロの集合。
;; `feature' 名 `my-utils'。
;;
;; 疑似名前空間プレフィクスは `my-'。

;;; Code:


;; ----------------------------------------------------------------------------
;; 依存解決
;; ----------------------------------------------------------------------------
(require 'fontset nil :noerror)


;; ----------------------------------------------------------------------------
;; Display
;; ----------------------------------------------------------------------------
;;;###autoload
(defmacro my-real-display-pixels-per-inch ()
  "Calculate real pixels per inch (ppi) by real display.
Value is a cons: (WIDTH-DPI . HEIGHT-DPI).

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
;; Fontsets
;; ----------------------------------------------------------------------------
;;;###autoload
(defmacro my-fallback-font-family (&rest families)
  "Return a first matched avaliabled font-family in FAMILIES.
If all FAMILIES are unavaliabled, return nil."
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
  "Return a first matched avaliabled font XLFD in XLFDS.
If all XLFDS are unavaliabled, return nil."
  (declare (indent 0)
           (debug t))
  (let ((founded (make-symbol "founded"))
        (xlfd (make-symbol "xlfd")))
    `(catch ',founded
       (dolist (,xlfd ',xlfds)
         (if (find-font (font-spec :name ,xlfd))
             (throw ',founded ,xlfd))))))

(defun my-create-fontset-spec-string (name spec)
  "Create string for 1st argument of `create-fontset-from-fontset-spec'.

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
SPEC is font object (ex. `font-spec' return value).

OPTIONS are same arguments of `create-fontset-from-fontset-spec' after the 2nd."
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
  "Change MINOR-MODE's lighter to VALUE."
  (declare (indent 0)
           (debug t))
  (let ((lighter (make-symbol "lighter")))
    `(let ((,lighter (cdr-safe (assoc ',minor-mode minor-mode-alist))))
       (setcar ,lighter ,value)
       ,lighter)))


(provide 'my-utils)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-00-utils.el ends here
