;;; windows-10-minor-printing.el --- 設定 - Windows - 印刷 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
;; TODO: Consolas + IPAmj明朝 の組み合わせを実現したい
;;       %GS_DIR%/lib/cidfmap を生成する %GS_DIR%/lib/mkcidfm.ps を
;;       編集しないとダメか？
;;
;; mkcidfm.ps の実行方法:
;; gs -q -dNOPAUSE -dBATCH -sFONTDIR=c:/windows/fonts -sCIDFMAP=%GS_DIR%/lib/cidfmap %GS_DIR%/lib/mkcidfm.ps
(let ((gsprint (executable-find "gsprint"))
      (gswinc (executable-find "gswinc")))
  (cond
   ;; GSView
   (gsprint
    (custom-set-variables
     `(ps-lpr-command ,(convert-standard-filename gsprint))
     '(ps-printer-name-option "-noprinter")
     '(ps-printer-name nil)
     '(ps-lpr-switches '("-color"
                         "-noquery"
                         "-all"
                         "-papersize A4"
                         ;; TODO: オプションつけるとうまくいかない
                         ;;       原因を探ること
                         ;;       縦書き印刷はしていないので、現状は
                         ;;       オプションなしでも問題は出ていない
                         ;; "-option \"-dNOPAUSE -dBATCH -dWINKANJI\""
                         ))))
   ;; Windows 用 Ghostscript
   (gswinc
    (custom-set-variables
     `(ps-lpr-command ,(convert-standard-filename gswinc))))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-10-minor-printing.el ends here
