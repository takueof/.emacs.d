;;; 10-minor-timestamp.el --- 設定 - マイナーモード - タイムスタンプ記述 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; ISO 8601 (JIS X 0301) 形式にする
 ;;
 ;; see also:
 ;; http://ja.wikipedia.org/wiki/ISO_8601
 ;;
 ;; Warning:
 ;; time-stamp-time-zone を "+09:00" にしても、コロン以降が無視されてしまう
 '(time-stamp-format
   (concat "%:y-%02m-%02dT%02H:%02M:%02S"
           ;; タイムゾーンは別途指定
           ;;
           ;; time-stamp-string の %Z は (format-time-string "%Z") と同義
           ;; この値をそのまま扱うので、環境の差異が出やすくマトモに使えない
           ;;
           ;; time-stamp-string の %z は (format-time-string "%#Z") と同義
           ;; (format-time-string "%z") ではない点に注意
           ;; この値をそのまま扱うので、環境の差異が出やすくマトモに使えない
           ;; また format-time-string 側のバグにより、環境次第で文字化けする
           ;;
           ;; M$ Windows 環境・環境変数 %TZ% 未指定・+09:00 (JST) では
           ;; 次の値が用いられてしまう：
           ;;
           ;;   %Z (≒ %Z):  #("東京 (標準時)" 0 8 (charset cp932-2-byte))
           ;;   %z (≒ %#Z): #("東京 (婦準時)" 0 8 (charset cp932-2-byte))
           ;;
           ;; 「標」→「婦」に文字化けしているのがわかる
           ;; また、propertize されている（プロパティが付与されている）
           ;;
           ;; FIXME: 現状では、OS 側の動的なタイムゾーン変更に追従不能
           ;;        都度評価にしたい
           (replace-regexp-in-string
            ;; コロンがない形式を返されるため、強制的にコロンを付与
            ;; 厳密なチェックにより "±1259" 形式のみ対象にする（他は無視）
            "\\`\\([\\+\\-]\\(?:0[0-9]\\|1[0-2]\\)\\)\\([0-5][0-9]\\)\\'"
            "\\1:\\2"
            ;; タイムゾーンが UTC でも "Z" でなく "+0000" を返してくる
            ;; 今のところ、あえて "Z" への変換はしないでおく
            (format-time-string "%z")))))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'time-stamp)
    (add-hook 'before-save-hook #'time-stamp))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-timestamp.el ends here
