;;; 10-minor-close-comment-tag.el --- 設定 - マイナーモード - (X)HTML 終了タグ前コメント挿入

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-04T06:47:45+09:00>

;;; Commentary:

;;; Code:

;; ;; FIXME: MELPA などに存在しないパッケージ
;; (package-install 'close-comment-tag)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'close-comment-tag
  '(progn
     (defun close-comment-tag--construct-comment (id classes)
       (documentation 'close-comment-tag--construct-comment)
       (if (or id classes)
           (concat "<!-- /"
                   (and id (concat "#" id))
                   ;; class 属性値では常に /.foo となるように調整
                   (and classes (concat "." (mapconcat 'identity classes ".")))
                   " -->")
         ;; 属性値がなければ、何も出力させない
         ""))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-close-comment-tag.el ends here
