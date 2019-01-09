;;; 10-minor-ido.el --- 設定 - マイナーモード - ファイル操作の簡略化

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-09T11:30:05+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(ido-enable-flex-matching t)
 '(ido-create-new-buffer 'always)
 '(ido-use-virtual-buffers t)
 '(ido-max-file-prompt-width 0)
 '(ido-use-filename-at-point 'guess)
 '(ido-unc-hosts t)
 ;; ローカル環境にのみ保存
 '(ido-save-directory-list-file
   (convert-standard-filename "~/.emacs.ido-save-directory-list.el")))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'ido-mode)
    (ido-mode +1))

(eval-after-load 'ido
  '(if (fboundp 'ido-everywhere)
      (ido-everywhere +1)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-ido.el ends here
