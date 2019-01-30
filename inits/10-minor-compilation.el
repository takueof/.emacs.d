;;; 10-minor-compilation.el --- 設定 - マイナーモード - コンパイル -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:12:27+09:00>

;;; Commentary:

;;; Code:

(require 'subr-x nil :noerror)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(compilation-window-height 15)
 '(compile-command (purecopy "")) ; ビルドツール・タスクランナーに依存させない
 '(compilation-scroll-output t)
 '(compilation-always-kill t)
 '(compilation-context-lines t))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-compilation-mode-initialize ()
  "Initialize `compilation-mode' before buffer load."
  ;; EMPTY
  )

(add-hook 'compilation-mode-hook #'my-compilation-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'compile)
    (global-set-key (kbd "C-c C-l") #'compile))


;; ----------------------------------------------------------------------------
;; HACK: ウインドウの状態を問わず、常にリサイズをかける
;; ----------------------------------------------------------------------------
(eval-after-load 'compile
  ;; オーバーライド
  '(defun compilation-set-window-height (window)
     "Set the height of WINDOW according to `compilation-window-height'."
     (let ((height (buffer-local-value 'compilation-window-height
                                       (window-buffer window))))
       (and height
            ;; `window-full-width-p' は用いない
            ;;
            ;; If window is alone in its frame, aside from a minibuffer,
            ;; don't change its height.
            (not (eq window (frame-root-window (window-frame window))))
            ;; Stef said that doing the saves in this order is safer:
            (save-excursion
              (save-selected-window
                (select-window window)
                (enlarge-window (- height (window-height)))))))))


;; ----------------------------------------------------------------------------
;; HACK: コンパイル完了後、モードラインにも状態を簡易表示
;; ----------------------------------------------------------------------------
(eval-after-load 'compile
  '(progn
     (defun my-compilation-message (cur-buffer msg)
       "Show status messages when compile done in `compilation-mode'."
       (let ((msg-text (string-trim msg)) ; 改行文字が含まれうる問題を回避
             (msg-title (buffer-name))
             (msg-face 'compilation-mode-line-fail))
         (message "%s: %s"
                  msg-title
                  (propertize msg-text
                              'face
                              (if (string-equal "finished" msg-text)
                                  'compilation-mode-line-exit
                                'compilation-mode-line-fail)))))

     (add-hook 'compilation-finish-functions #'my-compilation-message)))


;; ----------------------------------------------------------------------------
;; HACK: コンパイル完了後、ステータスに異常がなければ自動でウインドウを閉じる
;; ----------------------------------------------------------------------------
(eval-after-load 'compile
  '(progn
     (defcustom my-compilation-auto-quit-window-enable-buffer-name '("*compilation*")
       "TODO: ドキュメント書く"
       :group 'compilation
       :type '(list (repeat string)))

     ;; `process-status' と `exit-status' の値も得たいので、アドバイスを利用
     ;; `compilation-finish-functions' にフックした関数では `msg' しか
     ;; 参照できないため
     (defun my-compilation-auto-quit-window (process-status exit-status msg)
       "Run `quit-window' when `compile' successed."
       (if (and (member (buffer-name)
                        my-compilation-auto-quit-window-enable-buffer-name)
                (or (and (equal process-status 'exit)
                         (zerop exit-status))
                    ;; 改行文字が含まれうる問題を回避
                    (string-equal "finished" (string-trim msg))))
           (quit-window nil (get-buffer-window))))

     (if (fboundp 'compilation-handle-exit)
         (advice-add 'compilation-handle-exit
                     :after
                     #'my-compilation-auto-quit-window))))


;; ----------------------------------------------------------------------------
;; HACK: ANSI エスケープシーケンスが正しく解釈されない問題を回避
;; ----------------------------------------------------------------------------
(defun my-ansi-color-apply-on-compilation ()
  "Recognize ASCII color escape sequences for `compilation-mode' buffer."
  (if (and (require 'ansi-color nil :noerror)
           (fboundp 'ansi-color-apply-on-region))
      (let ((start-marker (make-marker))
            (end-marker (process-mark (get-buffer-process (current-buffer)))))
        (set-marker start-marker (point-min))
        (ansi-color-apply-on-region start-marker end-marker))))

(add-hook 'compilation-filter-hook #'my-ansi-color-apply-on-compilation)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-compilation.el ends here
