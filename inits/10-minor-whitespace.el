;;; 10-minor-whitespace.el --- 設定 - マイナーモード - 空白文字強調 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:16:27+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; 「不正」位置の空白文字のみ強調
 '(whitespace-style '(face
                      trailing
                      tabs
                      newline
                      empty
                      space-after-tab
                      space-before-tab
                      tab-mark
                      newline-mark))
 ;; 行カラム最大値は fill-column を参照させる
 '(whitespace-line-column nil))

(custom-set-faces
 ;; フェイス強調しないようにする
 '(whitespace-space ((t
                      (:background nil)))))

(eval-after-load 'my-utils
  '(eval-after-load 'whitespace
     '(progn
        (if (fboundp 'whitespace-mode)
            (my-change-lighter whitespace-mode nil))
        (if (fboundp 'whitespace-newline-mode)
            (my-change-lighter whitespace-newline-mode nil))
        (if (fboundp 'global-whitespace-mode)
            (my-change-lighter global-whitespace-mode nil))
        (if (fboundp 'global-whitespace-newline-mode)
            (my-change-lighter global-whitespace-newline-mode nil)))))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'global-whitespace-mode)
    (global-whitespace-mode +1))


;; ----------------------------------------------------------------------------
;; HACK: 全角空白 (U+3000) を HARD SPACE とみなして強調表示
;; ----------------------------------------------------------------------------
;; 表示テスト:
;;   U+0009: 「	」
;;   U+00A0: 「 」
;;   U+3000: 「　」
;; ----------------------------------------------------------------------------
(eval-after-load 'whitespace
  '(when (and (boundp 'whitespace-style)
              (boundp 'whitespace-display-mappings))
     (custom-set-variables
      ;; 空白の強調を明示
      `(whitespace-style ',(let ((styles (copy-tree whitespace-style)))
                             ;; HARD SPACE の ON/OFF も含んでいる
                             (add-to-list 'styles 'spaces)
                             (add-to-list 'styles 'space-mark)))
      ;; 検索条件を追加
      '(whitespace-hspace-regexp "\\(\\(\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\|\x3000\\)+\\)")
      '(whitespace-trailing-regexp "\\([\t \u00A0\u3000]+\\)$"))

     ;; 表示置換条件を追加
     (add-to-list 'whitespace-display-mappings
                  '(space-mark ?\u3000 [?\u25a1] [?_ ?_]))))


;; ----------------------------------------------------------------------------
;; HACK: 半角空白 (U+0020) を強調しないようにする
;; ----------------------------------------------------------------------------
;; 表示テスト:
;;   U+0020: 「 」
;; ----------------------------------------------------------------------------
(eval-after-load 'whitespace
  '(if (boundp 'whitespace-display-mappings)
       ;; 表示置換しないようにする
       (custom-set-variables
        `(whitespace-display-mappings ',(delete '(space-mark ?\  [?\u00B7] [?.])
                                                whitespace-display-mappings)))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-whitespace.el ends here
