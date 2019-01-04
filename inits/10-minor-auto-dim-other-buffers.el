;;; 10-minor-auto-dim-other-buffers.el --- 設定 - マイナーモード - 他ウインドウ弱調化

;; Copyright (C) 2015-2018 Taku Watabe
;; Time-stamp: <2018-12-08T05:11:23+09:00>

;;; Commentary:

;; テーマ設定は `my-default-theme.el' に記述してある。

;;; Code:

(package-install 'auto-dim-other-buffers)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'my-utils
  '(eval-after-load 'auto-dim-other-buffers
     '(my-change-lighter auto-dim-other-buffers-mode nil)))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(defun my-auto-dim-other-buffers-mode-initialize ()
  "Initialize `auto-dim-other-buffers-mode'."
  (if (fboundp 'auto-dim-other-buffers-mode)
      (auto-dim-other-buffers-mode +1)))

(add-hook 'after-init-hook #'my-auto-dim-other-buffers-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-auto-dim-other-buffers.el ends here
