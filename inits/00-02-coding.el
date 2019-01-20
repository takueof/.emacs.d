;;; 00-02-coding.el --- 設定 - 文字セット・コーディングシステム -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:11:29+09:00>

;;; Commentary:

;; WARNING: `set-language-environment' 実行よりも後で設定すること。

;;; Code:


;; ----------------------------------------------------------------------------
;; 「UTF-8（BOM 有）」のエイリアスを作成
;; ----------------------------------------------------------------------------
;; デフォルト名は長いため
;;
;; see also:
;; `mule-conf.el'
(define-coding-system-alias 'utf-8-bom 'utf-8-with-signature)


;; ----------------------------------------------------------------------------
;; コーディングシステムの CP932 を Shift_JIS として強制認識
;; ----------------------------------------------------------------------------
;; MIME を用いた自動エンコーディング判定を行うコード（`sgml-mode' など）でも
;; 例外が出ないようにする
(coding-system-put 'japanese-cp932
                   :mime-charset 'shift_jis)

;; `japanese-shift-jis' を Microsoft Code Page 932 相当にする
;;
;; GNU Emacs における Shift_JIS 実装 `japanese-shift-jis' は、
;; JIS X 0208 の附属書1にある定義を厳格に実装したもの
;; ゆえに、一部文字（例：「～」）が未定義であったりするなど、実用上の問題が発生
;;
;; そこで、タイムスタンプ時点で最も普及している Microsoft の Shift_JIS 実装
;;  `japanese-cp932' を、デフォルトの Shift_JIS として認識させる
;;
;; see also:
;; `japanese.el'
(define-coding-system-alias 'japanese-shift-jis 'japanese-cp932)
(define-coding-system-alias 'shift_jis 'japanese-cp932)
(define-coding-system-alias 'sjis 'japanese-cp932)


;; ----------------------------------------------------------------------------
;; 「〜」(U+301C) → 「～」(U+FF5E) 自動変換
;; ----------------------------------------------------------------------------
;; see also:
;; http://nijino.homelinux.net/emacs/emacs23-ja.html
(coding-system-put 'japanese-cp932 ; Shift_JIS
                   :encode-translation-table (get 'japanese-ucs-jis-to-cp932-map 'translation-table))


;; ----------------------------------------------------------------------------
;; 「～」(U+FF5E) → 「〜」(U+301C) 自動変換
;; ----------------------------------------------------------------------------
(coding-system-put 'japanese-iso-8bit ; EUC-JP
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'iso-2022-jp ; JIS
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-02-coding.el ends here
