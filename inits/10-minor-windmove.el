;;; 10-minor-windmove.el --- 設定 - マイナーモード - ウインドウ移動キーを直感的にする

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T14:06:36+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
;; フレーム端のウインドウでは無限スクロールするようにふるまう
;; 「マリオブラザーズ」左右画面端におけるループのような動き
(custom-set-variables
 '(windmove-wrap-around t))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'windmove-left)
    (global-set-key (kbd "C-S-b") #'windmove-left))

(if (fboundp 'windmove-right)
    (global-set-key (kbd "C-S-f") #'windmove-right))

(if (fboundp 'windmove-up)
    (global-set-key (kbd "C-S-p") #'windmove-up))

(if (fboundp 'windmove-down)
    (global-set-key (kbd "C-S-n") #'windmove-down))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-windmove.el ends here
