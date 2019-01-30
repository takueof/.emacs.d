;;; 10-minor-magit.el --- 設定 - マイナーモード - Git インターフェース -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:26:58+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'magit))
    (package-install 'magit))



;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; カレントバッファを表示しているウインドウに表示させる
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'magit-status)
    (global-set-key (kbd "C-x g") 'magit-status))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-magit.el ends here
