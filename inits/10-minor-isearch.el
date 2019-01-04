;;; 10-minor-isearch.el --- 設定 - マイナーモード - インクリメンタル検索

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T13:57:28+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; インクリメンタル検索時に大小文字を区別しない
 '(isearch-case-fold-search t)
 ;; 逆インクリメンタル検索時に大小文字を区別しない
 '(isearch-last-case-fold-search t))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-isearch.el ends here
