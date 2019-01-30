;;; 20-major-indium.el --- 設定 - メジャーモード - JavaScript 開発環境 -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:32:00+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'indium))
    (package-install 'indium))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(eval-after-load 'my-utils
  '(when (and (require 'indium nil :noerror)
              (fboundp 'indium-interaction-mode))
     (my-change-lighter indium-interaction-mode nil)
     (add-hook 'js-mode-hook #'indium-interaction-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-indium.el ends here
