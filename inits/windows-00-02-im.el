;;; windows-00-02-im.el --- 設定 - Windows - インプットメソッド -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:20:56+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
;; `w32-ime' がないと例外を吐きうる設定を行う必要があるため、明示的に切り分ける
(if (featurep 'w32-ime)
    (custom-set-variables
     ;; この識別名は、IME パッチ環境が有効でなければ使えない
     '(default-input-method "W32-IME")
     ;; バッファ毎の IME 状態モードライン表示
     '(w32-ime-mode-line-state-indicator "[Aa]")
     ;; IME 状態モードライン表示の一覧
     ;; 順に「IME OFF」「IME ON: 日本語入力」「IME ON: 英字入力」
     '(w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[Aa]"))
     ;; バッファ切替時には IME 状態を引き継がない
     '(w32-ime-buffer-switch-p t)))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (and (require 'w32-ime nil :noerror)
         (fboundp 'w32-ime-initialize))
    (w32-ime-initialize))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-00-02-im.el ends here
