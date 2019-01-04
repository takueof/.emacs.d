;;; 10-minor-dired.el --- 設定 - マイナーモード - `dired' 拡張

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:56:10+09:00>

;;; Commentary:

;;; Code:

(package-install 'dired+)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; 常にすべての情報を表示（簡易モードにしない）
 '(dired-hide-details-mode -1)
 '(diredp-hide-details-initially-flag nil)
 '(diredp-hide-details-propagate-flag nil))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-dired-mode-initialize ()
  "Initialize `dired-mode'."
  ;; EMPTY
  )

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
