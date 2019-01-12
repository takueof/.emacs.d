;;; 10-minor-enriched.el --- 設定 - マイナーモード - `text/enriched' フォーマットファイル -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; PATCH: 25.3 未満に存在するセキュリティホールのフィックス
;;
;; see also:
;; https://lists.gnu.org/archive/html/emacs-devel/2017-09/msg00211.html
;; ----------------------------------------------------------------------------
(if (or (< emacs-major-version 25)
        (and (= emacs-major-version 25)
             (< emacs-minor-version 3)))
    (eval-after-load 'enriched
      '(defun enriched-decode-display-prop (start end &optional param)
         (list start end))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-enriched.el ends here
