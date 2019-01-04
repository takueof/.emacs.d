;;; 10-minor-yasnippet.el --- 設定 - マイナーモード - スニペット挿入

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-15T20:05:55+09:00>

;;; Commentary:

;;; Code:

(package-install 'yasnippet)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'my-utils
  '(eval-after-load 'yasnippet
     '(my-change-lighter yas-minor-mode nil)))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'yas-global-mode)
    (yas-global-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-yasnippet.el ends here
