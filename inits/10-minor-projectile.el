;;; 10-minor-projectile.el --- 設定 - マイナーモード - 汎用プロジェクト管理 -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:15:10+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(projectile-enable-caching t)
 '(projectile-completion-system (cond ((featurep 'ido) 'ido)
                                      ((featurep 'ivy) 'ivy)
                                      ((featurep 'helm) 'helm)
                                      (t 'default)))
 '(projectile-mode-line-prefix "")
 ;; ローカル環境にのみ保存
 '(projectile-cache-file (convert-standard-filename "~/.emacs.projectile.cache"))
 '(projectile-known-projects-file (convert-standard-filename "~/.emacs.projectile-bookmarks.eld")))


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
