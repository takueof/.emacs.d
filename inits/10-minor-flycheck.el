;;; 10-minor-flycheck.el --- 設定 - マイナーモード - 自動静的解析ランナー -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;; `flycheck-color-mode-line' の face 設定は `my-default-theme.el' 参照。

;;; Code:

(package-install 'flycheck)
(package-install 'flycheck-color-mode-line)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; 無制限化
 '(flycheck-checker-error-threshold nil)
 ;; 表示タイマー
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 0.25)
 ;; 補完
 '(flycheck-completion-system (cond
                               ((featurep 'grizzl) 'grizzl)
                               ((featurep 'ido) 'ido)
                               (t nil)))
 ;; 省スペース化
 '(flycheck-mode-line " FC")
 ;; 無効化
 '(flycheck-disabled-checkers '(javascript-jscs)))

(eval-after-load 'flycheck
  '(eval-after-load 'warnings
     '(progn
        (if (boundp 'warning-suppress-log-types)
            ;; HACK: `flycheck-checker-error-threshold' 以上の項目が出現すると
            ;;       生成されうる警告バッファの出現を抑制
            (add-to-list 'warning-suppress-log-types
                         '(flycheck syntax-checker))))))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(when (fboundp 'flycheck-mode)
  (global-set-key (kbd "C-c f") #'flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(if (fboundp 'flycheck-color-mode-line-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))


;; ----------------------------------------------------------------------------
;; PATCH: ESLint 優先利用
;; ----------------------------------------------------------------------------
(eval-after-load 'flycheck
  '(progn
     ;; JSHint -> ESLint を ESLint -> JSHint 順に変更
     (if (boundp 'flycheck-checkers)
         (let* ((target-and-other-checkers (member 'javascript-eslint
                                                   flycheck-checkers)))
           (delete 'javascript-jshint flycheck-checkers)
           (setcdr target-and-other-checkers
                   (cons 'javascript-jshint
                         (cdr-safe target-and-other-checkers)))
           flycheck-checkers))))


;; ----------------------------------------------------------------------------
;; PATCH: v.Nu サポート
;; ----------------------------------------------------------------------------
(eval-after-load 'flycheck
  ;; FIXME: 標準出力が UTF-8 なので、環境によって文字化けする問題をどうにかする
  '(progn
     (unless (flycheck-registered-checker-p 'vnu)
       (flycheck-define-checker vnu
         "A (X)HTML syntax and style checker using v.NU.

See URL `https://github.com/validator/validator'."
         :command ("vnu" "--format" "gnu" "--verbose" source)
         :error-patterns
         ;; ファイル名はフルパスで入ってくるため、チェックしない
         ((error line-start (minimal-match (one-or-more not-newline)) ":"
                 line "." column "-"
                 ;; `flycheck' は範囲指定（複数の line, column 指定）を
                 ;; サポートしていない
                 ;; ゆえに範囲の終了地点は無視
                 (one-or-more digit) "." (one-or-more digit) ": "
                 "error: " (message)
                 line-end)
          (warning line-start (minimal-match (one-or-more not-newline)) ":"
                   line "." column "-"
                   (one-or-more digit) "." (one-or-more digit) ": "
                   "info warning: " (message)
                   line-end)
          (info line-start (minimal-match (one-or-more not-newline)) ":"
                line "." column "-"
                (one-or-more digit) "." (one-or-more digit) ": "
                "info: " (message)
                line-end))
         :modes (html-mode nxhtml-mode web-mode))

       ;; 有効化
       (let ((target-and-other-checkers (member 'html-tidy flycheck-checkers)))
         (cond
          ;; デフォルトの (X)HTML チェッカ `html-tidy' と入れ替える
          (target-and-other-checkers
           (setcar target-and-other-checkers 'vnu))
          ;; 未追加ならリスト先頭に追加
          (t
           (add-to-list 'flycheck-checkers 'vnu)))))))


;; ----------------------------------------------------------------------------
;; PATCH: Sass（.scss/.sass 両形式）チェック時にキャッシュを使わせない
;; ----------------------------------------------------------------------------
(eval-after-load 'flycheck
  '(progn
     (dolist (checker '(scss sass))
       (if (and (flycheck-registered-checker-p checker)
                (not (member "-C" (flycheck-checker-arguments checker))))
           ;; あえて破壊的に変更（元のリストに追加したい）
           (nconc (get checker 'flycheck-command) '("-C"))))))


;; ----------------------------------------------------------------------------
;; PATCH: temp ファイルのデフォルトコーディングシステムを、
;;        強制的に UTF-8 (LF) とする
;; ----------------------------------------------------------------------------
(eval-after-load 'flycheck
  '(progn
     ;; 他の部分は同名の関数と一致させる
     (defun flycheck-save-buffer-to-file (file-name)
       "Save the contents of the current buffer to FILE-NAME."
       (make-directory (file-name-directory file-name) t)
       ;; TODO: もっと柔軟に設定できるようにならないか？
       (let ((coding-system-for-write 'utf-8-unix) ; ここだけ変更・決め打ち
             (jka-compr-inhibit t))
         (write-region nil nil file-name nil 0)))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-flycheck.el ends here
