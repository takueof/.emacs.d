;;; 10-minor-menu-bar.el --- 設定 - マイナーモード - メニューバー -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-27T23:29:58+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(eval-after-load 'menu-bar
  '(if (fboundp 'menu-bar-mode)
       (menu-bar-mode -1)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-menu-bar.el ends here
