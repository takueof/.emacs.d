;;; 10-minor-desktop.el --- 設定 - マイナーモード - デスクトップ環境保存・復旧 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-22T17:30:18+09:00>

;;; Commentary:

;; see also:
;; http://www.emacswiki.org/emacs/DeskTop

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; -----------------------------------------------------------------------------
(custom-set-variables
 '(desktop-restore-frames t)
 '(desktop-load-locked-desktop t)
 ;; 必要最小限の情報のみ保存させる
 '(desktop-locals-to-save '(desktop-locals-to-save
                            truncate-lines
                            case-fold-search
                            case-replace
                            fill-column)))

(eval-after-load 'desktop
  '(when (and (boundp 'desktop-path)
              (boundp 'desktop-modes-not-to-save))
     ;; ローカル環境にのみ保存
     ;;
     ;; ホームディレクトリを最優先にすることで達成
     (add-to-list 'desktop-path "~")

     ;; 除外リスト
     (dolist (mode '(Info-mode
                     comint-mode
                     compilation-mode
                     completion-list-mode
                     dired-mode
                     fundamental-mode
                     ibuffer-mode
                     info-lookup-mode
                     lisp-interaction-mode
                     shell-mode))
       (add-to-list 'desktop-modes-not-to-save mode))

     ;; キーバインド
     (global-set-key (kbd "C-c d c") #'desktop-clear)
     (global-set-key (kbd "C-c d C-s") #'desktop-save)
     (global-set-key (kbd "C-c d s") #'desktop-save-in-desktop-dir)
     (global-set-key (kbd "C-c d d") #'desktop-remove)
     (global-set-key (kbd "C-c d f") #'desktop-change-dir)
     (global-set-key (kbd "C-c d r") #'desktop-revert)))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'desktop-save-mode)
    (desktop-save-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-desktop.el ends here
