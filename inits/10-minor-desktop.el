;;; 10-minor-desktop.el --- 設定 - マイナーモード - デスクトップ環境保存・復旧

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T13:53:03+09:00>

;;; Commentary:

;; see also:
;; http://www.emacswiki.org/emacs/DeskTop

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; -----------------------------------------------------------------------------
(custom-set-variables
 '(desktop-restore-frames t)
 ;; 必要最小限の情報のみ保存させる
 '(desktop-locals-to-save '(desktop-locals-to-save
                            truncate-lines
                            case-fold-search
                            case-replace
                            fill-column)))

(eval-after-load 'desktop
  '(progn
     (when (and (boundp 'desktop-path)
                (boundp 'desktop-modes-not-to-save))
       ;; ローカル環境にのみ保存
       ;;
       ;; ホームディレクトリを最優先にすることで達成
       (setq desktop-path (delete "~" desktop-path))
       (push "~" desktop-path)

       ;; 除外リスト
       (add-to-list 'desktop-modes-not-to-save 'Info-mode)
       (add-to-list 'desktop-modes-not-to-save 'comint-mode)
       (add-to-list 'desktop-modes-not-to-save 'compilation-mode)
       (add-to-list 'desktop-modes-not-to-save 'completion-list-mode)
       (add-to-list 'desktop-modes-not-to-save 'dired-mode)
       (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
       (add-to-list 'desktop-modes-not-to-save 'ibuffer-mode)
       (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
       (add-to-list 'desktop-modes-not-to-save 'lisp-interaction-mode)
       (add-to-list 'desktop-modes-not-to-save 'shell-mode)

       ;; キーバインド
       (global-set-key (kbd "C-c d c") #'desktop-clear)
       (global-set-key (kbd "C-c d C-s") #'desktop-save)
       (global-set-key (kbd "C-c d s") #'desktop-save-in-desktop-dir)
       (global-set-key (kbd "C-c d d") #'desktop-remove)
       (global-set-key (kbd "C-c d f") #'desktop-change-dir)
       (global-set-key (kbd "C-c d r") #'desktop-revert))))


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
