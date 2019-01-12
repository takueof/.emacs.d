;;; 10-minor-magit.el --- 設定 - マイナーモード - Git インターフェース -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

(package-install 'magit)


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
