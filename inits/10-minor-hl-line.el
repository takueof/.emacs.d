;;; 10-minor-hl-line.el --- 設定 - マイナーモード - カレントカーソル行強調

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-19T22:35:11+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-hl-line-initialize ()
  "Initialize `hl-line' settings."
  (when (and (require 'color nil :noerror) ; 未ロードの場合がありうるため必須
             (fboundp 'color-rgb-to-hsl)
             (fboundp 'color-name-to-rgb)
             (fboundp 'color-darken-name)
             (fboundp 'color-lighten-name))
    (let* ((L-diff 20)
           (background-color (face-attribute 'default :background))
           (L (nth 2 (apply 'color-rgb-to-hsl
                            (color-name-to-rgb background-color))))
           (line-background-color (funcall (if (< L 0.5)
                                               'color-lighten-name
                                             'color-darken-name)
                                           background-color
                                           L-diff)))
      (custom-set-faces
       `(hl-line ((((class color))
                   (:inherit nil :background ,line-background-color)))))
      nil)))

;; TODO: `after-init-hook' 後に実行した `load-theme' に対応したい
;;       `defadvice' で `enable-theme' の after 実行してもうまくいかない
(add-hook 'after-init-hook #'my-hl-line-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'global-hl-line-mode)
    (global-hl-line-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-hl-line.el ends here
