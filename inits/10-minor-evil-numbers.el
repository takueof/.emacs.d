;;; 10-minor-evil-numbers.el --- 設定 - マイナーモード - カーソル下の数値を増減 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;;; Code:

(package-install 'evil-numbers)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'evil-numbers/inc-at-pt)
    (global-set-key (kbd "C-2") #'evil-numbers/inc-at-pt))

(if (fboundp 'evil-numbers/dec-at-pt)
    (global-set-key (kbd "C-1") #'evil-numbers/dec-at-pt))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-evil-numbers.el ends here
