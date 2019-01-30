;;; 10-minor-emmet.el --- 設定 - マイナーモード - Emmet サポート -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:22:26+09:00>

;;; Commentary:

;; see also:
;; http://docs.emmet.io/

;;; Code:

(if (not (package-installed-p 'emmet-mode))
    (package-install 'emmet-mode))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(emmet-indentation 2)
 '(emmet-move-cursor-between-quotes t))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-emmet.el ends here
