;;; 10-minor_OBSOLETE-maxframe.el --- 設定 - マイナーモード - 環境に依存しないフレーム状態復元 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:30:30+09:00>

;;; Commentary:

;; 24.4 (r113242) 以降では `desktop' に同一機能 `desktop-restore-frames' が
;; 実装されている。
;;
;; 条件分岐による起動制御を実施する。
;;
;; see also:
;; `10-minor_OBSOLETE-frame-restore.el'

;;; Code:


(unless (and (require 'desktop nil :no-error)
             (boundp 'desktop-restore-frames))
  (if (not (package-installed-p 'maxframe))
      (package-install 'maxframe))


  ;; --------------------------------------------------------------------------
  ;; 起動
  ;; --------------------------------------------------------------------------
  (when (and (not noninteractive)
             (fboundp 'maximize-frame)
             (fboundp 'restore-frame))
    ;; 最大化時は次の条件をすべて満たす必要がある:
    ;;   * 必ずフレーム構築の最後であること
    ;;   * 他のフレームサイズ状態変更機能よりもあとで実行されること
    (add-hook 'window-setup-hook #'maximize-frame)
    ;; FIXME: (w32-send-sys-command 61728) が async なので
    ;;        `restore-frame' → `frame-restore-save-parameters' の順に
    ;;        実行されるよう配慮してもムダ
    ;;        現状 `frame-restore-save-parameters' 実行時のフレームサイズが
    ;;        最大化時のものになってしまっている
    (add-hook 'kill-emacs-hook #'restore-frame)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor_OBSOLETE-maxframe.el ends here
