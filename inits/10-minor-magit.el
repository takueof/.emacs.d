;;; 10-minor-magit.el --- 設定 - マイナーモード - Git インターフェース

;; Copyright (C) 2018 Taku Watabe
;; Time-stamp: <2018-11-29T17:35:20+09:00>

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
