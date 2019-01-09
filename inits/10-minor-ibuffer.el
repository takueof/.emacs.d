;;; 10-minor-ibuffer.el --- 設定 - マイナーモード - 拡張バッファ一覧

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-09T11:29:35+09:00>

;;; Commentary:

;;; Code:

(package-install 'ibuffer-vc)
(package-install 'ibuffer-projectile)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(ibuffer-expert t))

(eval-after-load 'ibuffer
  '(when (and (boundp 'ibuffer-formats)
              (boundp 'ibuffer-sorting-mode)
              (boundp 'ibuffer-last-sorting-mode)
              (boundp 'ibuffer-sorting-reversep)
              (boundp 'ibuffer-sorting-functions-alist))
     ;; バッファ名の表示を30文字に拡張
     ;; カラム幅が揃わなくなるため、-1 にはできない
     (let* (;; `customize-mark-to-save' の評価を t にするため、明示的にコピー
            (formats (copy-tree ibuffer-formats))
            (settings (assoc 'name (assoc 'mark formats))))
       ;; 該当する設定項目がなければ何もしない
       ;; 将来的に項目が変更された場合でも、例外を出さないための対策
       (when settings
         (setcdr settings '(30 30 :left :elide))
         (custom-set-variables
          '(ibuffer-formats formats))))

     ;; メジャーモード名 + ファイルパスでソート
     ;;
     ;; see also:
     ;; http://www.emacswiki.org/emacs/IbufferMode#toc10
     (define-ibuffer-sorter mode-name-and-path-alphabetic
       "Sort the buffers by their mode and paths.
Ordering is lexicographic."
       (:description "major mode + alphabetic")
       (string-lessp
        (with-current-buffer (car a)
          (let* ((file-path (buffer-file-name))
                 (buffer-name (buffer-name))
                 (path file-path)
                 (mode-name (format-mode-line mode-name))
                 (prefix "999"))
            (when (and (not (stringp file-path))
                       (stringp buffer-name))
              (setq prefix "000")
              (setq path buffer-name))
            (concat prefix ": " mode-name ": " path)))
        (with-current-buffer (car b)
          (let* ((file-path (buffer-file-name))
                 (buffer-name (buffer-name))
                 (path file-path)
                 (mode-name (format-mode-line mode-name))
                 (prefix "999"))
            (when (and (not (stringp file-path))
                       (stringp buffer-name))
              (setq prefix "000")
              (setq path buffer-name))
            (concat prefix ": " mode-name ": " path)))))

     (custom-set-variables
      '(ibuffer-default-sorting-mode 'mode-name-and-path-alphabetic))))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-ibuffer-initialize ()
  "Initialize `ibuffer' settings."
  (if (fboundp 'ibuffer-projectile-set-filter-groups)
      (ibuffer-projectile-set-filter-groups)))

(add-hook 'ibuffer-hook #'my-ibuffer-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'ibuffer)
    (global-set-key (kbd "C-x C-b") #'ibuffer))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-ibuffer.el ends here
