;;; 10-minor-undohist.el --- 設定 - マイナーモード - `undo' 履歴の記憶

;; Copyright (C) 2017-2019 Taku Watabe
;; Time-stamp: <2019-01-09T11:41:25+09:00>

;;; Commentary:

;;; Code:

(package-install 'undohist)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
  ;; ローカル環境にのみ保存
 '(undohist-directory (convert-standard-filename "~/.emacs.undohist")))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; `require' 必須（初期化が必要なため）
(if (and (require 'undohist nil :noerror)
         (fboundp 'undohist-initialize))
    (undohist-initialize))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-undohist.el ends here
