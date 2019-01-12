;;; 20-major-text.el --- 設定 - メジャーモード - プレーンテキスト -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-text-mode-initialize ()
  "Initialize `text-mode' before file load."
  (setq-local truncate-lines nil))

(add-hook 'text-mode-hook #'my-text-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-text.el ends here
