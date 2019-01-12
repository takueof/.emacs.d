;;; 20-major-indium.el --- 設定 - メジャーモード - JavaScript 開発環境 -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

(package-install 'indium)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(eval-after-load 'indium
  '(when (fboundp 'indium-interaction-mode)
     (add-hook 'js-mode-hook #'indium-interaction-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-indium.el ends here
