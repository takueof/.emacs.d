;;; 10-minor-anzu.el --- 設定 - マイナーモード - 各種検索・置換強化 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;;; Code:

(package-install 'anzu)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(anzu-mode-lighter nil)
 '(anzu-minimum-input-length 3)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " -> "))

;; migemo 利用可能時
(eval-after-load 'migemo
  '(custom-set-variables
    '(anzu-use-migemo t)))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'anzu-query-replace)
    (global-set-key (kbd "M-%") #'anzu-query-replace))

(if (fboundp 'anzu-query-replace-regexp)
    (global-set-key (kbd "C-M-%") #'anzu-query-replace-regexp))

(if (fboundp 'global-anzu-mode)
    (global-anzu-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-anzu.el ends here
