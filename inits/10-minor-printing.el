;;; 10-minor-printing.el --- 設定 - マイナーモード - 印刷 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:15:03+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(ps-multibyte-buffer 'non-latin-printer)
 '(ps-lpr-switches '("-dBATCH" "-dNOPAUSE" "-dWINKANJI"))
 '(ps-paper-type 'a4)
 ;;
 ;; 本文フォント
 ;;
 '(ps-font-size '(10 . 10))
 ;;
 ;; 色
 ;;
 '(ps-print-color-p t)
 '(ps-default-fg t)
 '(ps-default-bg t)
 '(ps-use-face-background t)
 ;;
 ;; 行調整
 ;;
 '(ps-line-spacing 2)
 ;;
 ;; 行番号
 ;;
 '(ps-line-number t)
 '(ps-line-number-font "Times-Italic") ; TODO: Courier にしたい
 ;;
 ;; 水平レイアウト
 ;;
 '(ps-left-margin (/ (* 72 1.4) 2.54)) ; 14mm（行番号が切れないようにする）
 '(ps-inter-column (/ (* 72 1.0) 2.54)) ; 10mm
 '(ps-right-margin (/ (* 72 0.54) 2.54)) ; 5.4mm（ヘッダ・フッタの box 右端が切れないようにする）
 ;;
 ;; 垂直レイアウト
 ;;
 '(ps-top-margin (/ (* 72 0.9) 2.54)) ; 9mm（ヘッダ box 上端が切れないようにする）
 '(ps-header-offset (/ (* 72 0.1) 2.54)) ; 1mm
 '(ps-footer-offset (/ (* 72 0.37) 2.54)) ; 3.7mm（フッタ box 上端へカブらないようにする）
 '(ps-bottom-margin (/ (* 72 0.55) 2.54)) ; 5.5mm（フッタ box 下端が切れないようにする）
 ;;
 ;; ヘッダ
 ;;
 '(ps-print-header t)
 '(ps-header-lines 2)
 '(ps-print-header-frame t)
 '(ps-left-header (list 'ps-get-buffer-name
                        'ps-header-dirpart))
 '(ps-right-header (list 'ps-time-stamp-yyyy-mm-dd
                         'ps-time-stamp-hh:mm:ss))
 '(ps-header-line-pad 0.15)
 '(ps-header-font-family 'Courier)
 '(ps-header-font-size '(10 . 10))
 '(ps-header-title-font-size '(10 . 10))
 ;;
 ;; フッタ
 ;;
 '(ps-print-footer t)
 '(ps-footer-lines 1)
 '(ps-print-footer-frame t)
 '(ps-left-footer nil)
 '(ps-right-footer (list "/pagenumberstring load"))
 '(ps-footer-line-pad 0.15)
 '(ps-footer-font-family 'Courier)
 '(ps-footer-font-size '(10 . 10))
 '(ps-footer-title-font-size '(10 . 10)))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-ps-print-hook-listener ()
  "Initialize `ps-print' when load hook `ps-print-hook'."

  ;; FIXME: 長い行の右端が切れてしまう問題を解決しなければならない
  ;;        いちいち改行を（カレントバッファへ）明示的に入れる方法はナシで
  ;;        プリント前処理 temp バッファを作ればいいかもしれないが……？

  ;; 極限まで細くする
  (if (boundp 'ps-header-frame-alist)
      (setcdr (assoc 'border-width ps-header-frame-alist) 0.1)))

(add-hook 'ps-print-hook #'my-ps-print-hook-listener)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'ps-print-buffer)
    (global-set-key (kbd "C-c p p") #'ps-print-buffer))

(if (fboundp 'ps-print-buffer-with-faces)
    (global-set-key (kbd "C-c p c") #'ps-print-buffer-with-faces))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-printing.el ends here
