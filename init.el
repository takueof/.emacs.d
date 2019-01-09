;;; init.el --- "GNU Emacs" main config file

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-09T11:17:33+09:00>

;;; Commentary:

;; This config file can use "GNU Emacs" ONLY,
;; not compatible with "XEmacs" and other emacsens.

;;; Code:

(set-language-environment "Japanese")


;; ----------------------------------------------------------------------------
;; コーディングシステム
;; ----------------------------------------------------------------------------
;; ファイルとバッファのデフォルトだけ決める（他は変えない）
;;
;; (prefer-coding-system 'utf-8-unix) は使わない
;; システムごとに最適化された、デフォルト定義自動設定の結果を破壊するため
;;
;; 他の設定は `00-02-coding.el' を参照
(set-coding-system-priority 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)

;; macOS ONLY
(when (equal system-type 'darwin)
  (set-terminal-coding-system 'utf-8)
  (setq-default default-process-coding-system '(utf-8 . utf-8)))


;; ----------------------------------------------------------------------------
;; ロードパス
;; ----------------------------------------------------------------------------
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; `custom-set-variables' と `custom-set-faces' に `user-init-file' への
 ;; 追記を許さない
 ;; 自動保存は別ファイルに行わせる
 '(custom-file (locate-user-emacs-file "custom.el")))


;; ----------------------------------------------------------------------------
;; パッケージマネージャ `package'
;; ----------------------------------------------------------------------------
;; `defcustom' によって定義されたリストヘシンボルを追加したいため、
;; あえて明示的にロード
(when (and (require 'package nil :noerror)
           (fboundp 'package-initialize))
  ;; ----------------------------------
  ;; 初期化
  ;; ----------------------------------
  ;; 確実に定義された後で追加
  (add-to-list 'package-archives
               '("MELPA" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "https://marmalade-repo.org/packages/"))

  ;; あらゆるパッケージロードに先んじて記述しなければならない
  (package-initialize)

  ;; `list-packages' のような短縮版を用意
  (defalias 'list-packages-no-fetch 'package-list-packages-no-fetch))


;; ----------------------------------------------------------------------------
;; 詳細設定ロード (by `init-loader')
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(init-loader-directory (locate-user-emacs-file "inits"))
 '(init-loader-show-log-after-init 'error-only)
 ;; 設定ファイルのバイトコンパイルは非推奨
 ;;
 ;; see also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
 '(init-loader-byte-compile nil))

(eval-after-load 'package
  ;; 全てのパッケージマネージャ関連機能が、必ず使える状況を前提とする
  '(progn
     (package-install 'init-loader)

     ;; 起動
     (if (and (require 'init-loader nil :noerror)
              (fboundp 'init-loader-load))
         (init-loader-load))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; init.el ends here
