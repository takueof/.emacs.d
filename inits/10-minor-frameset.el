;;; 10-minor-frameset.el --- 設定 - マイナーモード - フレームセット -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-22T16:43:02+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-frameset-initialize ()
  "Initialize `frameset' when `after-init-hook' running."
  (eval-after-load 'frameset
    '(when (listp frameset-filter-alist)
       ;; `desktop' で保存不要な項目はすべて :never にする
       (dolist (key '(background-color
                      foreground-color
                      font
                      frameset--text-pixel-height
                      frameset--text-pixel-width
                      GUI:font))
         (setcdr (assoc key frameset-filter-alist) :never)))))

;; 全設定が完了してから実行しなければならない
;; 途中で追加される項目がありうるため
(add-hook 'after-init-hook #'my-frameset-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-frameset.el ends here
