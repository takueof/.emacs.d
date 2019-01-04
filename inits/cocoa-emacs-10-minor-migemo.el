;;; cocoa-emacs-10-minor-migemo.el --- 設定 - macOS (Cocoa) - マイナーモード - ローマ字入力から日本語をインクリメンタル検索

;; Copyright (C) 2018 Taku Watabe
;; Time-stamp: <2018-11-26T18:42:42+09:00>

;;; Commentary:

;; TODO: 未設定。

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(let ((cmd "cmigemo")
      (dict "/usr/local/share/migemo/utf-8/migemo-dict"))
  (if (and (executable-find cmd)
           (file-exists-p dict))
      (custom-set-variables
       ;; コマンド設定
       '(migemo-command cmd)
       '(migemo-options '("-q" "--emacs"))
       ;; 辞書ファイルはデフォルトのものを利用
       '(migemo-dictionary (convert-standard-filename dict))
       '(migemo-user-dictionary nil)
       '(migemo-regex-dictionary nil))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; cocoa-emacs-10-minor-migemo.el ends here
