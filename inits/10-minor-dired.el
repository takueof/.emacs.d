;;; 10-minor-dired.el --- 設定 - マイナーモード - `dired' 拡張 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:21:39+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'dired+))
    (package-install 'dired+))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(diredp-hide-details-initially-flag nil)
 '(diredp-hide-details-propagate-flag nil))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-dired-mode-initialize ()
  "Initialize `dired-mode'."
  ;; 常にすべての情報を表示（簡易モードにしない）
  (if (fboundp 'dired-hide-details-mode)
      (dired-hide-details-mode -1)))

(add-hook 'dired-mode-hook #'my-dired-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(require 'dired+ nil :no-error)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-dired.el ends here
