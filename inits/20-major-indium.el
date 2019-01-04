;;; 20-major-indium.el --- 設定 - メジャーモード - JavaScript 開発環境

;; Copyright (C) 2018 Taku Watabe
;; Time-stamp: <2018-11-29T12:01:45+09:00>

;;; Commentary:

;;; Code:

(package-install 'indium)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(eval-after-load 'indium
  '(when (fboundp 'indium-interaction-mode)
     (add-hook 'js-mode-hook #'indium-interaction-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-indium.el ends here
