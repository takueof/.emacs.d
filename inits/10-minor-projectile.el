;;; 10-minor-projectile.el --- 設定 - マイナーモード - 汎用プロジェクト管理

;; Copyright (C) 2018 Taku Watabe
;; Time-stamp: <2018-12-08T02:00:08+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(projectile-enable-caching t)
 '(projectile-completion-system 'ido)
 '(projectile-mode-line-prefix " P"))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(when (and (fboundp 'projectile-mode)
           (projectile-mode +1)
           (boundp 'projectile-command-map))
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-projectile.el ends here
