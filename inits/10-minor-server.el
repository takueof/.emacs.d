;;; 10-minor-server.el --- 設定 - マイナーモード - サーバ化

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-09T11:37:42+09:00>

;;; Commentary:

;; Windows 環境では `server-auth-dir' の「所有者」が：
;;   * Administrator (RID=500)
;;   * Administrators (RID=544)
;; である場合、`server-ensure-safe-dir' の評価が `nil' になる。
;;
;; `server-auth-dir' で指定したフォルダの「プロパティ」→「セキュリティ」→
;; 「詳細設定」→「所有者」→「編集」から、所有者をログオンユーザ自身に
;; 変更すること。

;; Windows 環境では emacsclientw.exe 実行時に環境変数 EMACS_SERVER_FILE で
;; サーバファイルのパスを明示する必要がある。
;;
;; なぜ必要かは不明。
;;
;; この欠点をある程度回避した wemacs.cmd を用いること。

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; ローカル環境にのみ保存
 '(server-auth-dir (convert-standard-filename "~/.emacs.server")))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
;; サーバ上のバッファは黙って kill
(remove-hook 'kill-buffer-query-functions #'server-kill-buffer-query-function)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (and (require 'server nil :noerror)
         (fboundp 'server-force-delete)
         (fboundp 'server-start))
    (server-force-delete)
    (server-start))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-server.el ends here
