;;; 10-minor_OBSOLETE-frame-restore.el --- 設定 - マイナーモード - フレーム位置・サイズ復元

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-10T19:55:54+09:00>

;;; Commentary:

;; 24.4 (r113242) 以降では `desktop' に同一機能 `desktop-restore-frames' が
;; 実装されている。
;;
;; 条件分岐による起動制御を実施する。
;; `frame-restore-mode' は自動検出して停止してくれるが、warning を吐くため。
;;
;; see also:
;; http://bzr.savannah.gnu.org/lh/emacs/trunk/revision/113242

;;; Code:

(unless (and (require 'desktop nil :no-error)
             (boundp 'desktop-restore-frames))
  (package-install 'frame-restore)


  ;; --------------------------------------------------------------------------
  ;; デフォルト値
  ;; --------------------------------------------------------------------------
  (custom-set-variables
   ;; ローカル環境にのみ保存
   '(frame-restore-parameters-file
     (convert-standard-filename "~/.emacs.frame-restore-parameters.el")))


  ;; --------------------------------------------------------------------------
  ;; 起動
  ;; --------------------------------------------------------------------------
  (when (fboundp 'frame-restore-mode)
    (frame-restore-mode +1)

    (unless noninteractive
      ;; なるべく「最後」に実行されるよう調整
      ;; なるべく他のフレーム状態変更機能と衝突させないため
      (when (fboundp 'frame-restore-save-parameters)
        (remove-hook 'kill-emacs-hook #'frame-restore-save-parameters)
        (add-hook 'kill-emacs-hook #'frame-restore-save-parameters t)))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor_OBSOLETE-frame-restore.el ends here
