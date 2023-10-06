;;; init.el --- "GNU Emacs" main config file -*- mode: Emacs-Lisp; coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2013-2023 Taku Watabe
;; Time-stamp: <2023-10-06T12:57:52+09:00>

;; Author: Taku Watabe <taku.eof@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This config file can use "GNU Emacs" ONLY.
;; Unsupported other "emacsen" ("XEmacs" and others).

;; This file is VERY LONG.
;; So, I DARE USE file local variables in the FIRST LINE.

;; Show initialization time:
;; (emacs-init-time)

;;; Code:


;; ============================================================================
;; Use "Japanese" environment
;; ============================================================================
(set-language-environment "Japanese")


;; ============================================================================
;; コーディングシステム
;; ============================================================================
;; いくつかのデフォルトだけ決める（他は変えない）
;;
;; `prefer-coding-system' は絶対に使わないこと！
;; 例：(prefer-coding-system 'utf-8-unix)
;; システムごとに最適化された、自動設定のデフォルト定義を破壊するため
(set-coding-system-priority 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; macOS ONLY
(when (member system-type '(darwin))
  (set-keyboard-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq-default default-process-coding-system '(utf-8 . utf-8)))

;; 「UTF-8（BOM 有）」のエイリアスを作成
;;
;; デフォルト名は長いため
;;
;; See also:
;; `mule-conf.el'
(define-coding-system-alias 'utf-8-bom 'utf-8-with-signature)

;; `japanese-cp932' を `shift_jis' として強制認識
;;
;; MIME を用いた自動エンコーディング判定を行うコード（`sgml-mode' など）でも
;; 例外が出ないようにする
(coding-system-put 'japanese-cp932
                   :mime-charset 'shift_jis)

;; `japanese-shift-jis' を Microsoft Code Page 932 (`japanese-cp932') に変更
;;
;; GNU Emacs における Shift_JIS 実装 `japanese-shift-jis' は、
;; JIS X 0208 附属書1にある定義を厳格に実装したもの
;; ゆえに、一部文字（例：「～」(U+FF5E)）が未定義であるなどし、
;; 実用上問題が発生しやすい
;;
;; そこで、タイムスタンプ時点で最も普及している Microsoft の Shift_JIS 実装
;; Microsoft Code Page 932 (`japanese-cp932') を、
;; デフォルトの Shift_JIS 実装として認識させる
;;
;; See also:
;; `japanese.el'
(define-coding-system-alias 'japanese-shift-jis 'japanese-cp932)
(define-coding-system-alias 'shift_jis 'japanese-cp932)
(define-coding-system-alias 'sjis 'japanese-cp932)

;; 「〜」(U+301C) → 「～」(U+FF5E) 自動変換
(coding-system-put 'japanese-cp932 ; Shift_JIS
                   :encode-translation-table (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

;; 「～」(U+FF5E) → 「〜」(U+301C) 自動変換
(coding-system-put 'japanese-iso-8bit ; EUC-JP
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'iso-2022-jp ; JIS
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))


;; ============================================================================
;; デフォルト値
;; ============================================================================
(custom-set-variables
 ;;
 ;; フレームタイトルはカレントバッファ名を基準にする
 ;;
 '(frame-title-format (format "%%b - emacs %s" emacs-version))
 ;;
 ;; スタートアップ表示は一切させない
 ;;
 ;; See also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
 ;;
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message t)
 ;;
 ;; *scratch* バッファのデフォルトメッセージは表示しない
 ;;
 '(initial-scratch-message nil)
 ;;
 ;; ベルは視覚のみ・音なし
 ;;
 '(visible-bell t)
 '(ring-bell-function 'ignore)
 ;;
 ;; タイプ時にマウスポインタを自動で隠す
 ;;
 '(make-pointer-invisible t)
 ;;
 ;; 空行をフリンジに表示
 ;;
 '(indicate-empty-lines t)
 ;;
 ;; ファイル先頭・末尾の状態表示をフリンジに表示
 ;;
 '(indicate-buffer-boundaries 'right)
 ;;
 ;; `kill-line' で改行も含めて削除
 ;;
 '(kill-whole-line t)
 ;;
 ;; 読取専用バッファにおける `kill-line' 実行時、
 ;; エコーエリアに関連メッセージが表示されるようにする
 ;;
 '(kill-read-only-ok t)
 ;;
 ;; 同一（重複）文字列は `kill-ring' に保存しない
 ;;
 '(kill-do-not-save-duplicates t)
 ;;
 ;; `undo' 時に `redo' 履歴は無視する
 ;;
 '(undo-no-redo t)
 ;;
 ;; クリップボードと `kill-ring' を同期させる
 ;;
 '(select-enable-clipboard t)
 ;;
 ;; Yank (Paste) 時にプロパティを全破棄し、プレーンテキストを利用
 ;;
 '(yank-excluded-properties t)
 ;;
 ;; プレビューウインドウの表示を即時にする
 ;;
 '(register-preview-delay nil)
 ;;
 ;; スクロール時、自動スクロールをアグレッシブにする
 ;;
 ;; See also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
 '(scroll-conservatively 0) ; default
 '(scroll-step 0) ; default
 '(scroll-up-aggressively nil) ; default
 '(scroll-down-aggressively nil) ; default
 ;;
 ;; なるべくウインドウ上下から2行目でスクロール開始
 ;;
 '(scroll-margin 2)
 '(maximum-scroll-margin 2)
 ;;
 ;; ページ単位スクロール時に行を重複させる
 ;;
 '(next-screen-context-lines 2)
 ;;
 ;; スクロール時、なるべく先頭ないし最後の文字にポイントを移動させる
 ;;
 '(scroll-error-top-bottom t)
 ;;
 ;; スクロール時、ポイントを同一スクリーン位置に留まらせなくてもよい
 ;; non-nil にするとスクロールが不安定になりがちなため、nil とする
 ;;
 '(scroll-preserve-screen-position nil)
 ;;
 ;; 行間調整はしない
 ;;
 '(line-spacing nil)
 ;;
 ;; 行間移動に論理行を用いる
 ;;
 '(line-move-visual t)
 ;;
 ;; デフォルトの行表示は折り返し「なし」
 ;;
 '(truncate-lines t)
 '(truncate-partial-width-windows t)
 '(default-truncate-lines t)
 ;;
 ;; デフォルトの行文字数を、端末エミュレータのデファクトスタンダードにあわせる
 ;;
 '(fill-column 80)
 ;;
 ;; デフォルトのインデント利用文字は、常に半角空白 (U+0020) のみ
 ;; 必要なら各メジャーモードごとに設定しなおす
 ;;
 '(indent-tabs-mode nil)
 ;;
 ;; タブは常にインデントのみ実施
 ;;
 '(tab-always-indent t)
 ;;
 ;; 自分用デフォルトタブ文字表示幅
 ;; 必要なら各メジャーモードごとに設定しなおす
 ;;
 '(tab-width 4)
 ;;
 ;; 大文字・小文字は区別しない
 ;;
 '(case-fold-search t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 ;;
 ;; 新規ファイル・バッファ作成時の確認は省略
 ;;
 '(confirm-nonexistent-file-or-buffer nil)
 ;;
 ;; 最終行への改行（空行）挿入を強制
 ;;
 ;; 不要なら各メジャーモードごとに設定させる
 ;;
 '(require-final-newline t)
 '(mode-require-final-newline t)
 ;;
 ;; `undo' 上限を引き上げ
 ;;
 '(undo-limit 600000)
 '(undo-strong-limit 900000) ; (= 1.5 (/ undo-strong-limit undo-limit)) を踏襲
 ;;
 ;; 自動バックアップは不要
 ;;
 '(auto-save-default nil)
 '(make-backup-files nil)
 `(auto-save-list-file-prefix ,(convert-standard-filename "~/.emacs.auto-save-list/.saves-")) ; ローカル環境化
 ;;
 ;; ロックファイル不要
 ;;
 '(create-lockfiles nil)
 ;;
 ;; `eval-expression' 時の出力を省略させない
 ;;
 '(eval-expression-print-level nil)
 '(eval-expression-print-length nil)
 ;;
 ;; 補完表示は循環させる
 ;;
 '(completion-cycle-threshold t)
 ;;
 ;; 補完表示は縦にする
 ;;
 '(completions-format 'vertical)
 ;;
 ;; エコーエリアの最大行数を増やす
 ;;
 '(message-log-max 2000)
 ;;
 ;; ミニバッファで各種コマンドを利用できるようにする
 ;;
 '(enable-recursive-minibuffers t)
 ;;
 ;; Trash（「ごみ箱」など）が使える場合はそちらへ廃棄
 ;;
 '(delete-by-moving-to-trash t)
 ;;
 ;; YES/NO 選択を簡略化
 ;;
 '(use-short-answers t)
 ;;
 ;; 人為的に italic/bold フォントを選択 (Windows ONLY)
 ;;
 '(w32-enable-synthesized-fonts t)
 ;;
 ;; インプットメソッドを明示 (Windows ONLY)
 ;;
 ;; 識別名 "W32-IME"' は、IME パッチが適用されていなければ使えない
 ;; 他環境ではデフォルトのままとする
 ;;
 `(default-input-method ,(cond ((member system-type '(ms-dos windows-nt))
                                "W32-IME")
                               (t
                                default-input-method)))
 ;;
 ;; バッファごとのインプットメソッド状態モードライン表示 (Windows ONLY)
 ;;
 '(w32-ime-mode-line-state-indicator "[Aa]")
 ;;
 ;; インプットメソッド状態モードライン表示の一覧 (Windows ONLY)
 ;;
 ;; 順に「IM OFF」「IM ON: 日本語入力」「IM ON: 英字入力」
 ;;
 '(w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[Aa]"))
 ;;
 ;; バッファ切替時にはインプットメソッド状態を引き継がない (Windows ONLY)
 ;;
 '(w32-ime-buffer-switch-p t)
 ;;
 ;; 右 <Alt> + 左 <Ctrl> で <AltGr> が発送されないようにする (Windows ONLY)
 ;; <AltGr> は独自のキーコードであり、<C-M-> であるとみなされない
 ;;
 ;; See also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Keyboard.html
 ;;
 '(w32-recognize-altgr nil)
 ;;
 ;; Web ブラウザ
 ;;
 `(browse-url-browser-function ',(cond ((equal window-system 'w32)
                                        'browse-url-default-windows-browser)
                                       ((equal window-system 'mac)
                                        'browse-url-default-macosx-browser)
                                       (t
                                        'browse-url-default-browser)))
 ;;
 ;; 証明書
 ;;
 `(gnutls-trustfiles ',(mapcar 'convert-standard-filename
                               (if (member system-type '(ms-dos windows-nt))
                                   '("C:/programs/cygwin/usr/ssl/certs/ca-bundle.crt")
                                 '("/usr/local/etc/libressl/cert.pem"
                                   "/usr/local/etc/openssl/cert.pem"
                                   "/private/etc/ssl/cert.pem"
                                   "/etc/ssl/cert.pem")))))


;; ============================================================================
;; リージョンの大文字・小文字変換で、実行の是非を問わせない
;; ============================================================================
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ============================================================================
;; ベル音 (Windows ONLY)
;; ============================================================================
(if (fboundp #'set-message-beep) ; Windows 環境でのみ存在
    ;; なし
    (set-message-beep 'silent))


;; ============================================================================
;; ロードパス追加
;; ============================================================================
(add-to-list 'load-path (locate-user-emacs-file "utils"))


;; ============================================================================
;; ユーティリティロード
;; ============================================================================
(require 'my-utils nil :noerror)


;; ============================================================================
;; 環境変数 (Windows ONLY)
;; ============================================================================
(when (member system-type '(ms-dos windows-nt))
  ;; 環境変数 %PATH% では不足している分の追加
  (let* ((program-files-dir-x86 (or (getenv "PROGRAMFILES\(X86\)")
                                    (getenv "PROGRAMFILES")
                                    "C:/programs"))
         (paths `(,(concat program-files-dir-x86 "/Aspell/bin")
                  "C:/programs/cmigemo/bin"
                  "C:/programs/Ghostgum/gsview"
                  "C:/programs/cygwin/bin")))
    (dolist (path paths)
      (setq path (convert-standard-filename path))
      (if (and (file-exists-p path)
               (file-directory-p path))
          (add-to-list 'exec-path path)))))


;; ============================================================================
;; NSM (Network Security Manager)
;; ============================================================================
;; WARNING: `package' といったネットワークセキュリティを利用するパッケージの
;;          利用前に設定を実施しなければならない
;;          後のほうで `nsm-settings-file' を設定してしまうと意味がない
;; ============================================================================
;; HACK: `require' しておかないと、
;;       なぜか `custom-set-variables' が効かない
;; ============================================================================
(when (require 'nsm nil :noerror)
  (custom-set-variables
   ;; ローカル環境にのみ保存
   '(nsm-settings-file "~/.emacs.network-security.data")))


;; ============================================================================
;; パッケージマネージャ (by `package')
;; ============================================================================
;; `defcustom' によって定義されたリストヘシンボルを追加したいため、
;; あえて明示的にロード
(when (require 'package nil :noerror)
  ;; 確実に定義された後で追加
  (add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/"))

  ;; あらゆるパッケージロードに先んじての記述が必須
  (package-initialize)

  ;; `list-packages' のような短縮版を用意
  (defalias 'list-packages-no-fetch 'package-list-packages-no-fetch))


;; ============================================================================
;; 詳細設定補助 (by `leaf')
;;
;; `package' が必ず使える状況を前提とする
;; ============================================================================
;; WARNING: `package' が必ず使える状況を前提とする
;;          `package' の初期化より後に設定しなければならない
;; ============================================================================
(with-eval-after-load 'package
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))


;; ============================================================================
;; 詳細設定
;;
;; See also:
;; https://github.com/conao3/leaf.el
;; https://qiita.com/conao3/items/dc88bdadb0523ef95878
;; ============================================================================
;; WARNING: `leaf' が必ず使える状況を前提とする
;;          `leaf' のインストールより後に設定しなければならない
;; ============================================================================
;; `leaf' キーワード群
;; ============================================================================
(leaf leaf-keywords
  :package t
  :config
  (leaf-keywords-init))


;; ============================================================================
;; パッケージマネージャ (by `el-get')
;; ============================================================================
;; WARNING: 全てのパッケージに影響するため、
;;          なるべく早いタイミングでインストールするようにしてある
;; ============================================================================
(leaf el-get
  :package t
  :custom ((el-get-git-shallow-clone . t)))


;; ============================================================================
;; IME patch (Windows ONLY)
;; ============================================================================
;; WARNING: 全てのパッケージに影響するため、
;;          なるべく早いタイミングでインストールするようにしてある
;; ============================================================================
(leaf tr-ime
  :when (member system-type '(ms-dos windows-nt))
  :package t
  :config
  (tr-ime-advanced-install)
  (w32-ime-initialize))


;; ============================================================================
;; サーバ化
;; ============================================================================
;; WARNING: 起動を前提としたパッケージが存在するため、
;;          なるべく早いタイミングで開始するようにしてある
;; ============================================================================
;; Windows 環境では `server-auth-dir' の「所有者」が：
;;   * Administrator (RID=500)
;;   * Administrators (RID=544)
;; である場合、`server-ensure-safe-dir' の評価が `nil' になる
;;
;; `server-auth-dir' で指定したフォルダの
;; 「プロパティ」→「セキュリティ」→「詳細設定」→「所有者」→「編集」
;; から、所有者をログオンユーザ自身に変更すること
;; ============================================================================
;; Windows 環境は emacsclientw.exe 実行時に環境変数 %EMACS_SERVER_FILE% へ
;; サーバファイルのパスを明示する必要がある
;; なぜ必要かは不明
;;
;; この欠点をある程度回避した wemacs.cmd を用いること
;; ============================================================================
(leaf server
  :custom (;; ローカル環境にのみ保存
           (server-auth-dir . "~/.emacs.server"))
  :config
  (server-start t))


;; ============================================================================
;; グローバルキーバインド
;; ============================================================================
(leaf *global-keybind
  :leaf-defer nil
  :after my-utils
  :bind (;; ヘルプ表示を割り当てなおす
         ("C-x ?" . help-command)
         ;; ウインドウ中央表示はもっともシンプルなものを用いる
         ;; `recenter-top-bottom' は使わない
         ("C-l" . recenter)
         ;; リージョン範囲をソート
         ("C-c s" . sort-lines)
         ;; 1つ前のエラーを表示
         ("C-x \\" . previous-error)
         ;; `revert-buffer-quick' ショートカット
         ("C-c r" . revert-buffer-quick)
         ;; 行頭移動は物理行
         ("C-a" . my-beginning-of-smart-indented-line)
         ;; 前のウインドウに移動
         ("C-x p" . my-other-window-reverse)
         ;; 前のフレームに移動
         ("C-x 5 p" . my-other-frame-reverse)
         ;; 折り返し表示を強制切替
         ("C-x w" . my-toggle-truncate-lines-force)
         ;; カーソル位置に YEN SIGN (U+00A5) を挿入
         ("C-c i \\" . my-insert-yen-sign)
         ;; カーソル位置にファイル名を挿入
         ("C-c i f" . my-insert-file-name)
         ;; カーソル位置にファイルパスを挿入
         ("C-c i p" . my-insert-file-path)
         ;; 一括エンコーディング変換
         ("C-c RET f" . my-change-files-coding-system)
         ;; 一括ファイル通知ウォッチ削除
         ("C-c q" . my-file-notify-rm-all-watches))
  :config
  ;; <Backspace> と <DEL> を 交換
  (keyboard-translate ?\C-h ?\C-?)
  ;; <DEL> を <C-d> にする
  (keyboard-translate ?\C-? ?\C-d)
  ;; `ido-undo-merge-work-directory' 実行のため <C-z> を押しすぎた場合、
  ;; `suspend-frame' が起動しないよう配慮
  (global-unset-key (kbd "C-z"))
  ;; タッチパッドによる各種操作を無効化 (macOS ONLY)
  (when (member system-type '(darwin))
    (global-unset-key [magnify-up])
    (global-unset-key [magnify-down])
    (global-unset-key [S-magnify-up])
    (global-unset-key [S-magnify-down])))


;; ============================================================================
;; カラーテーマ
;; ============================================================================
(leaf *themes
  :custom (;; For "Modus"
           (modus-themes-bold-constructs . t)
           (modus-themes-syntax . '(faint alt-syntax green-strings yellow-comments))
           (modus-themes-mode-line . '(moody accented borderless))
           (modus-themes-paren-match . '(bold intense))
           (modus-themes-region . '(accented bg-only no-extend)))
  :config
  ;; 利用可能なカラーテーマを設定
  (let ((required-themes '(;; 利用したいカラーテーマの一覧
                           ;; 優先度が高い順に降順ソートしておくこと
                           modus-vivendi
                           wheatgrass))
        (availabled-themes (custom-available-themes)))
    ;; 利用したいカラーテーマが見つからなければ何もしない
    (catch 'required-theme-found
      (dolist (theme required-themes)
        (when (member theme availabled-themes)
          (load-theme theme t)
          (throw 'required-theme-found theme))))))


;; ============================================================================
;; モードライン
;; ============================================================================
(leaf *modeline
  :after my-utils
  :custom (;; ニーモニックを改行コードにちなんだ表現にする
           (eol-mnemonic-dos . "[CRLF]")
           (eol-mnemonic-mac . "[CR]")
           (eol-mnemonic-unix . "[LF]")
           (eol-mnemonic-undecided . ""))
  :config
  ;; --------------------------------------------------------------------------
  ;; モードライン表示領域をスリムにする
  ;; --------------------------------------------------------------------------
  (let ((fmt (copy-sequence mode-line-format))
        (item)
        (result '()))
    (while (setq item (car-safe fmt))
      (setq fmt (cdr-safe fmt))
      (push
       ;; 3以上の空白文字のみで構成された項目は、すべて U+0020 2文字に置換
       (if (and (stringp item)
                (numberp (string-match "\\`\\s-\\{3,\\}\\'" item)))
           "  "
         item)
       result))
    (setq mode-line-format (reverse result)))

  ;; 各種情報の非表示化
  (delete 'mode-line-client mode-line-format) ; Emacs Client
  (delete 'mode-line-remote mode-line-format) ; リモートバッファ
  (delete 'mode-line-frame-identification mode-line-format) ; フレーム名
  (delete 'mode-line-misc-info mode-line-format) ;その他

  ;; --------------------------------------------------------------------------
  ;; `mode-line-mule-info' 差替
  ;;
  ;; See also:
  ;; https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
  ;; --------------------------------------------------------------------------
  (let ((mode-line-coding-system-mnemonic (member "%z" mode-line-mule-info)))
    (if (listp mode-line-coding-system-mnemonic)
        (setcar mode-line-coding-system-mnemonic
                '(:eval (my-buffer-coding-system-mnemonic)))))

  ;; --------------------------------------------------------------------------
  ;; 各種位置情報
  ;;
  ;; すべて独自定義にする
  ;; (line-number-mode t)
  ;; (column-number-mode t)
  ;; (size-indication-mode -1)
  ;; --------------------------------------------------------------------------
  ;; `defcustom' 定義ではないため `setq' を利用
  (setq mode-line-position
        `(;; カーソル位置情報
          (:eval (if (use-region-p)
                     ;; リージョン選択時
                     (propertize
                      (format "%d" (abs (- (region-end)
                                           (region-beginning))))
                      'local-map mode-line-column-line-number-mode-map
                      'mouse-face 'mode-line-highlight
                      'help-echo "リージョン文字数")
                   ;; 通常
                   ;;
                   ;; 行先頭にあるときは0になる（`current-column' と同仕様）
                   ;; 現状では %c + 1 にはしない
                   ,(propertize
                     "%c"
                     'local-map mode-line-column-line-number-mode-map
                     'mouse-face 'mode-line-highlight
                     'help-echo "行内カーソル位置")))
          ":"
          ;; FIXME: リージョン選択時に前・次行の同一カラムヘ直接移動すると、
          ;;        `region-beginning' と `region-end' 値が
          ;;        更新されない場合がある（視覚表現と一致しなくなる）
          (:eval (if (use-region-p)
                     ;; リージョン選択時
                     (propertize
                      (format "%d" (count-lines (region-beginning)
                                                (region-end)))
                      'local-map mode-line-column-line-number-mode-map
                      'mouse-face 'mode-line-highlight
                      'help-echo "リージョン行数")
                   ;; 通常
                   ,(propertize
                     "%l"
                     'local-map mode-line-column-line-number-mode-map
                     'mouse-face 'mode-line-highlight
                     'help-echo "現在行")))
          ;; セパレータ
          " "
          ;; バッファ情報
          "["
          (:eval (propertize
                  (format "%d"
                          (abs (- (line-end-position)
                                  (line-beginning-position))))
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "全文字数（現在行）"))
          ":"
          (:eval (propertize
                  (format "%d" (line-number-at-pos (point-max)))
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "全行数"))
          "]"
          ;; セパレータ
          " "
          ;; バッファサイズ
          ,(concat
            (propertize
             "%ic"
             'local-map mode-line-column-line-number-mode-map
             'mouse-face 'mode-line-highlight
             'help-echo "全文字数")
            "/"
            (propertize
             "%I"
             'local-map mode-line-column-line-number-mode-map
             'mouse-face 'mode-line-highlight
             'help-echo "ファイルサイズ"))
          ))
  ) ; End of *modeline


;; ============================================================================
;; Input Method (IM)
;; ============================================================================
(leaf *input-method
  ;; WARNING: `window-system' 外の環境（例：ターミナル）では例外発生
  :when window-system
  :after my-utils
  :hook (;; -------------------------------------------------------------------
         ;; ウインドウ選択後、IM の状態に応じてフェイス `cursor' を変更
         ;;
         ;; `cursor' はフレーム単位
         ;; しかし、`current-input-method' はバッファローカル変数
         ;; よって、バッファ間で `current-input-method' 値が異なれば、
         ;; `cursor' が意図せぬ状態になる
         ;;
         ;; ゆえに、ウインドウ切替のタイミングでの `cursor' 明示変更が必要
         ;;
         ;; バッファ切替時は、特に何もしない
         ;; -------------------------------------------------------------------
         ;; `select-window' 実行後に起動するフックを利用
         (buffer-list-update-hook . my-change-cursor-faces-by-current-input-method)
         ;; IM の activate/deactivate と連動させる
         (input-method-activate-hook . my-change-cursor-faces-by-current-input-method)
         (input-method-deactivate-hook . my-change-cursor-faces-by-current-input-method)
         ;; macOS ONLY
         (mac-selected-keyboard-input-source-change-hook . my-change-cursor-faces-by-current-input-method)
         (mac-enabled-keyboard-input-sources-change-hook . my-change-cursor-faces-by-current-input-method))
  :init
  (defface my-cursor-default nil
    "`cursor' face for `current-input-method' is nil."
    :group 'customize)
  (copy-face 'cursor 'my-cursor-default)

  (defface my-cursor-input-method-activated '((t
                                               :background "gold"))
    "`cursor' face for `current-input-method' is non-nil."
    :group 'customize))


;; ============================================================================
;; 未分類パッケージ（非メジャー／マイナーモード）
;; ============================================================================
(leaf *packages
  :config
  ;; --------------------------------------------------------------------------
  ;; Node.js モジュールパス解決
  ;; --------------------------------------------------------------------------
  (leaf add-node-modules-path
    :package t
    :hook ((prog-mode-hook . add-node-modules-path)))


  ;; --------------------------------------------------------------------------
  ;; ANSI エスケープシーケンス
  ;; --------------------------------------------------------------------------
  (leaf ansi-color
    :config
    ;; `comint-mode' および派生モードで、ANSI エスケープシーケンスの解釈を開始
    (ansi-color-for-comint-mode-on))


  ;; --------------------------------------------------------------------------
  ;; 未コミット diff
  ;; --------------------------------------------------------------------------
  (leaf diff-hl
    :package t
    :global-minor-mode global-diff-hl-mode)


  ;; --------------------------------------------------------------------------
  ;; EWW (Emacs Web Wowser, Web Browser)
  ;; --------------------------------------------------------------------------
  (leaf eww
    :bind (("C-c C-e" . eww))
    :custom ((eww-search-prefix . "https://www.google.co.jp/search?&q=")
             (eww-history-limit . 100)))


  ;; --------------------------------------------------------------------------
  ;; GNU/Linux, UNIX, macOS 環境変数 $PATH 自動取得・設定
  ;; --------------------------------------------------------------------------
  (leaf exec-path-from-shell
    :unless (member system-type '(ms-dos windows-nt))
    :package t
    :require t
    :config
    (exec-path-from-shell-initialize))


  ;; --------------------------------------------------------------------------
  ;; スペルチェッカ
  ;; --------------------------------------------------------------------------
  (leaf ispell
    :custom ((ispell-dictionary . "english")
             (ispell-extra-args . '("--sug-mode=fast"
                                    "--run-together"
                                    "--run-together-limit=5"
                                    "--run-together-min=2"))))


  ;; --------------------------------------------------------------------------
  ;; Git インターフェース
  ;; --------------------------------------------------------------------------
  (leaf magit
    :package t
    :bind (("C-x g" . magit-status)))


  ;; --------------------------------------------------------------------------
  ;; nvm 経由での Node.js 利用をサポート
  ;; --------------------------------------------------------------------------
  (leaf nvm
    :package t
    :config
    ;; `~/.nvmrc' がなければ何もしない
    (ignore-errors (nvm-use-for)))


  ;; --------------------------------------------------------------------------
  ;; タイムスタンプ記述
  ;; --------------------------------------------------------------------------
  (leaf time-stamp
    :hook ((before-save-hook . time-stamp))
    :custom `(;; ISO 8601 (JIS X 0301) 形式にする
              ;;
              ;; See also:
              ;; https://ja.wikipedia.org/wiki/ISO_8601
              ;;
              ;; WARNING: `time-stamp-time-zone' を "+09:00" にしても、
              ;;          コロン以降が無視される
              ;;
              ;; タイムゾーンは別途指定、以下理由：
              ;;
              ;; `time-stamp-string' の "%Z" は
              ;; (format-time-string "%Z") と同義
              ;; この値をそのまま扱うため、
              ;; 環境の差異が出やすくマトモに使えない
              ;;
              ;; `time-stamp-string' の "%z" は
              ;; (format-time-string "%#Z") と同義
              ;; (format-time-string "%z") ではない点に注意
              ;; この値をそのまま扱うため、
              ;; 環境の差異が出やすくマトモに使えない
              ;; また `format-time-string' 側のバグにより、
              ;; 環境次第で文字化けする
              ;;
              ;; Windows 環境（環境変数 %TZ% 未指定・+09:00 ゾーン）では
              ;; 次の値が用いられてしまう
              ;; （どちらもエンコーディングは `cp932-2-byte'）：
              ;;
              ;; "%Z" (≒ "%Z"):  #("東京 (標準時)" 0 8
              ;; "%z" (≒ "%#Z"): #("東京 (婦準時)" 0 8
              ;;
              ;; 「標」→「婦」に文字化けしているのがわかる
              ;; また、`propertize' されている
              ;;
              ;; FIXME: 現状、OS 側の動的なタイムゾーン変更に追従不能
              ;;        都度評価にしたい
              (time-stamp-format . ,(concat "%:y-%02m-%02dT%02H:%02M:%02S"
                                            (replace-regexp-in-string
                                             ;; コロンがない形式を返されるため、強制的にコロンを付与
                                             ;; 厳密なチェックにより "±1259" 形式のみ対象にする
                                             ;;   → 他は無視
                                             "\\`\\([\\+\\-]\\(?:0[0-9]\\|1[0-2]\\)\\)\\([0-5][0-9]\\)\\'"
                                             "\\1:\\2"
                                             ;; タイムゾーンが UTC でも "Z" でなく "+0000" を返す
                                             ;; 今のところ、あえて "Z" への変換はしないでおく
                                             (format-time-string "%z"))))))


  ;; --------------------------------------------------------------------------
  ;; TRAMP (Transparent Remote Access, Multiple Protocols)
  ;; --------------------------------------------------------------------------
  (leaf tramp
    :require t
    :custom (;; WARNING: `load' か `autoload' 後に実行しないと適用されない
             ;; ローカル環境にのみ保存
             (tramp-persistency-file-name . "~/.emacs.tramp")))


  ;; --------------------------------------------------------------------------
  ;; ファイル名を元に、より唯一性の高いバッファ名を生成
  ;; --------------------------------------------------------------------------
  (leaf uniquify
    :require t
    :custom ((uniquify-buffer-name-style . 'forward)
             (uniquify-ignore-buffers-re . "^*[^*]+*\\-")))


  ;; --------------------------------------------------------------------------
  ;; URL ツール
  ;; --------------------------------------------------------------------------
  (leaf url
    :custom ((url-using-proxy . t)))


  ;; --------------------------------------------------------------------------
  ;; ターミナルエミュレータ
  ;; --------------------------------------------------------------------------
  (leaf vterm
    :package t
    :custom ((vterm-shell . "bash")
             (vterm-max-scrollback . 100000)
             (vterm-clear-scrollback-when-clearing . t)
             (vterm-enable-manipulate-selection-data-by-osc52 . t)
             (vterm-copy-exclude-prompt . nil))
    :init
    (leaf vterm
      :after vterm
      :init
      ;; WARNING: 確実に `vterm-keymap-exceptions' が存在する状態で、
      ;;          「定義」ではなく「追加」しないと
      ;;          他のキーバインドに影響が出てしまう
      (add-to-list 'vterm-keymap-exceptions "C-S-b") ; for `windmove'
      (add-to-list 'vterm-keymap-exceptions "C-S-f") ; for `windmove'
      (add-to-list 'vterm-keymap-exceptions "C-S-n") ; for `windmove'
      (add-to-list 'vterm-keymap-exceptions "C-S-p"))) ; for `windmove'


  ;; --------------------------------------------------------------------------
  ;; ウインドウ移動キーを直感的にする
  ;; --------------------------------------------------------------------------
  (leaf windmove
    :bind (("C-S-b" . windmove-left)
           ("C-S-f" . windmove-right)
           ("C-S-n" . windmove-down)
           ("C-S-p" . windmove-up))
    :custom (;; フレーム端のウインドウでは無限スクロールするようにふるまう
             ;; 「マリオブラザーズ」左右画面端におけるループのような動き
             (windmove-wrap-around . t)))
  ) ; End of *packages


;; ============================================================================
;; マイナーモード
;; ============================================================================
(leaf *minor-mode
  :config
  ;; --------------------------------------------------------------------------
  ;; スペース区切りによる複数キーワードを使った絞り込み
  ;; --------------------------------------------------------------------------
  (leaf affe
    :package t
    :after (consult orderless)
    :custom ((affe-regexp-function . #'orderless-pattern-compiler)
             (affe-highlight-function . #'orderless--highlight))
    :config
    (consult-customize affe-grep
                       :preview-key (kbd "M-.")))


  ;; --------------------------------------------------------------------------
  ;; 各種検索・置換強化
  ;; --------------------------------------------------------------------------
  (leaf anzu
    :package t
    :bind (("M-%" . anzu-query-replace)
           ("C-M-%" . anzu-query-replace-regexp))
    :custom ((anzu-minimum-input-length . 3)
             (anzu-search-threshold . 1000)
             (anzu-replace-to-string-separator . " -> "))
    :init
    ;; `migemo' 利用可能時
    (leaf anzu
      :after migemo
      :custom ((anzu-use-migemo . t)))
    :global-minor-mode global-anzu-mode)


  ;; --------------------------------------------------------------------------
  ;; 他ウインドウ弱調化
  ;; --------------------------------------------------------------------------
  (leaf auto-dim-other-buffers
    :package t
    :hook ((after-init-hook . my-auto-dim-other-buffers-mode-initialize))
    :init
    (defun my-auto-dim-other-buffers-mode-initialize ()
      "Initialize `auto-dim-other-buffers-mode'."
      (auto-dim-other-buffers-mode +1)))


  ;; --------------------------------------------------------------------------
  ;; 自動バッファ再読込
  ;; --------------------------------------------------------------------------
  (leaf autorevert
    :custom (;; ファイル監視（通知）関数を使わない
             ;;
             ;; GNU Emacs の仕様では 1024 - 50 = 974 個以上のファイル監視を
             ;; 登録できない
             ;; 少しでもファイル監視を減らすため無効化
             ;;
             ;; See also:
             ;; https://www.reddit.com/r/emacs/comments/mq2znn/comment/gugo0n4/?context=3
             (auto-revert-use-notify . nil)
             (auto-revert-check-vc-info . t))
    :global-minor-mode global-auto-revert-mode)


  ;; --------------------------------------------------------------------------
  ;; ブックマーク
  ;; --------------------------------------------------------------------------
  (leaf bookmark
    :require t
    :custom ((bookmark-version-control . t)
             ;; ローカル環境にのみ保存
             (bookmark-default-file . "~/.emacs.bookmark.el")))


  ;; --------------------------------------------------------------------------
  ;; ブックマーク (`bookmark') 拡張
  ;; --------------------------------------------------------------------------
  (leaf bookmark+
    :el-get (bookmark+
             :type github
             :pkgname "emacsmirror/bookmark-plus")
    :after bookmark
    :require t)


  ;; --------------------------------------------------------------------------
  ;; プログラマ向けネーミング辞書
  ;; --------------------------------------------------------------------------
  (leaf codic
    :package t
    :bind (("C-c C-d" . codic))
    :config
    ;; --------------------------------
    ;; HACK: 専用バッファをコマンドで `quit-window' させる
    ;; --------------------------------
    (unless (fboundp #'codic-view-kill)
      ;; 専用ウインドウを `quit-window' する関数が
      ;; 定義されていないなら追加
      (defun my-codic-view-kill ()
        "Quit `codic' window and bury its buffer."
        (interactive)
        (with-current-buffer (current-buffer)
          (quit-window t)))

      ;; 専用バッファでキーバインドを有効にするため、アドバイスを利用
      ;; 専用 hook がないため
      (defun my-codic-local-set-key (items)
        "Set `local-set-key' for `codic' result buffer."
        (with-current-buffer "*Codic Result*"
          (local-set-key (kbd "q") #'my-codic-view-kill)))

      (advice-add #'codic--view
                  :after
                  #'my-codic-local-set-key)))


  ;; --------------------------------------------------------------------------
  ;; 共通コマンドインタプリタ (Windows ONLY)
  ;; --------------------------------------------------------------------------
  (leaf comint
    :when (member system-type '(ms-dos windows-nt))
    :hook ((comint-mode-hook . my-comint-mode-initialize))
    :custom ((comint-scroll-to-bottom-on-input . 'all)
             (comint-move-point-for-output . 'all)
             (comint-buffer-maximum-size . 5000)
             (comint-process-echoes . t)
             (comint-eol-on-send . t))
    :init
    (defun my-comint-mode-initialize ()
      "Initialize `comint-mode'."
      (setq-local comint-input-sender-no-newline t))

    ;; --------------------------------
    ;; プロセスごとのコーディングシステム変換表
    ;;
    ;; See also:
    ;; https://www.emacswiki.org/emacs/ShellMode#toc1
    ;; --------------------------------
    (add-to-list 'process-coding-system-alist
                 '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix))))


  ;; --------------------------------------------------------------------------
  ;; 補完フレームワーク
  ;; --------------------------------------------------------------------------
  (leaf company
    :package t
    :hook ((after-init-hook . global-company-mode))
    :custom (;; `company'
             (company-tooltip-limit . 20)
             (company-tooltip-minimum . 10)
             (company-tooltip-offset-display . 'lines)
             (company-tooltip-align-annotations . t)
             (company-tooltip-flip-when-above . t)
             (company-transformers . '(company-sort-by-occurrence))
             (company-minimum-prefix-length . 1)
             (company-abort-manual-when-too-short . t)
             (company-idle-delay . 0.25)
             (company-selection-wrap-around . t)
             ;; `company-dabbrev'
             (company-dabbrev-other-buffers . t)
             (company-dabbrev-downcase . nil)
             ;; `company-dabbrev-code'
             (company-dabbrev-code-modes . '(batch-file-mode
                                             csharp-mode
                                             css-mode
                                             erlang-mode
                                             haskell-mode
                                             html-mode
                                             jde-mode
                                             js-mode
                                             js2-mode
                                             lua-mode
                                             prog-mode
                                             python-mode
                                             scss-mode
                                             typescript-mode))
             (company-dabbrev-code-other-buffers . t)
             (company-dabbrev-code-everywhere . t)
             (company-dabbrev-code-ignore-case . t)))


  ;; --------------------------------------------------------------------------
  ;; 補完フレームワーク (`company') 拡張（補完候補のソート）
  ;; --------------------------------------------------------------------------
  (leaf company-statistics
    :after company
    :package t
    :custom ((company-statistics-size . 500)
             ;; ローカル環境にのみ保存
             (company-statistics-file . "~/.emacs.company-statistics-cache.el"))
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; 補完フレームワーク (`company') 拡張（ポップアップドキュメント）
  ;; --------------------------------------------------------------------------
  (leaf company-quickhelp
    :after company
    :package t
    :custom ((company-quickhelp-delay . 0.25))
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; コンパイル
  ;; --------------------------------------------------------------------------
  (leaf compile
    :after (nvm exec-path-from-shell)
    :bind (("C-c x" . compile))
    :hook ((compilation-filter-hook . ansi-color-compilation-filter))
    :custom ((compilation-window-height . 15)
             ;; ビルドツール・タスクランナーに依存させない
             (compile-command . "")
             (compilation-scroll-output . t)
             (compilation-always-kill . t)
             (compilation-context-lines . t))
    :init
    ;; --------------------------------
    ;; HACK: コンパイル完了後、モードラインにも状態を簡易表示
    ;; --------------------------------
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

    (add-to-list 'compilation-finish-functions 'my-compilation-message)
    :config
    ;; --------------------------------
    ;; HACK: コンパイル完了後、正常に終了していれば自動でウインドウを閉じる
    ;; --------------------------------
    (defcustom my-compilation-auto-quit-window-enable-buffer-names '("*compilation*")
      "Created buffer names by `compile' command."
      :group 'compilation
      :type '(list (repeat string)))

    ;; `process-status' と `exit-status' の値も得たいので、アドバイスを利用
    ;; `compilation-finish-functions' にフックした関数では `msg' しか
    ;; 参照できないため
    (defun my-compilation-auto-quit-window (process-status exit-status msg)
      "Run `quit-window' when `compile' successed."
      (if (and (member (buffer-name)
                       my-compilation-auto-quit-window-enable-buffer-names)
               (or (and (equal process-status 'exit)
                        (zerop exit-status))
                   ;; 改行文字が含まれうる問題を回避
                   (string-equal "finished" (string-trim msg))))
          (quit-window nil (get-buffer-window))))

    (advice-add #'compilation-handle-exit
                :after
                #'my-compilation-auto-quit-window))


  ;; --------------------------------------------------------------------------
  ;; 補完
  ;; --------------------------------------------------------------------------
  (leaf consult
    :package t
    :bind (;; 上書き
           ("C-s" . my-consult-line)
           ("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
           ;; コマンド群
           ;; `C-c c' プレフィクスを用いる
           ("C-c c h" . consult-history)
           ("C-c c m" . consult-mode-command)
           ("C-c c b" . consult-bookmark)
           ("C-c c k" . consult-kmacro)
           ("C-c c e" . consult-compile-error)
           ("C-c c g" . consult-goto-line)
           ([remap goto-line] . consult-goto-line)
           ("C-c c o" . consult-outline)
           ("C-c c m" . consult-mark)
           ("C-c c M" . consult-global-mark)
           ("C-c c i" . consult-imenu)
           ("C-c c I" . consult-project-imenu)
           ("C-c c f" . consult-focus-lines)
           ;; コマンド群（検索）
           ;; "C-c c s" プレフィクスを用いる
           ("C-c c s f" . consult-find)
           ("C-c c s L" . consult-locate)
           ("C-c c s g" . consult-grep)
           ("C-c c s G" . consult-git-grep)
           ("C-c c s r" . consult-ripgrep)
           ("C-c c s l" . consult-line))
    :hook ((completion-list-mode . consult-preview-at-point-mode))
    :custom ((register-preview-function . #'consult-register-format)
             (xref-show-xrefs-function . #'consult-xref)
             (xref-show-definitions-function . #'consult-xref)
             (consult-project-root-function . #'my-consult-project-root-function))
    :advice ((:override register-preview consult-register-window)
             (:override completing-read-multiple consult-completing-read-multiple))
    :config
    (defun my-consult-line (&optional at-point)
      "Consult-line uses things-at-point if set C-u prefix."
      (interactive "P")
      (if at-point
          (consult-line (thing-at-point 'symbol))
        (consult-line)))

    (defun my-consult-project-root-function ()
      "Function which returns project root directory."
      (if-let (project (project-current))
          (car (project-roots project))))

    (consult-customize consult-theme
                       :preview-key '(:debounce 0.2 any)
                       consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark consult-recent-file consult-xref
                       :preview-key (kbd "M-.")))


  ;; --------------------------------------------------------------------------
  ;; 補完 - LSP (Language Server Protocol) サポート
  ;; --------------------------------------------------------------------------
  (leaf consult-lsp
    :package t
    :bind (("C-c c ." . consult-lsp-diagnostics)))


  ;; --------------------------------------------------------------------------
  ;; 矩形選択
  ;; --------------------------------------------------------------------------
  (leaf cua-base
    ;; 特殊キーバインド無効
    :global-minor-mode cua-selection-mode)


  ;; --------------------------------------------------------------------------
  ;; バッファ内マッチ補完
  ;; --------------------------------------------------------------------------
  (leaf dabbrev
    :custom (;; 補完時に大小文字を区別しない
             (dabbrev-case-fold-search . t)))


  ;; --------------------------------------------------------------------------
  ;; Debug Adapter Protocol
  ;; --------------------------------------------------------------------------
  (leaf dap-mode
    :package t
    :after dap-mode
    :config
    (dap-ui-mode +1))


  ;; --------------------------------------------------------------------------
  ;; モードラインからモードの表示を消す
  ;; --------------------------------------------------------------------------
  (leaf delight
    :package t
    :config
    (delight '(;; 降順ソート
               (anzu-mode nil "anzu")
               (auto-dim-other-buffers-mode nil "auto-dim-other-buffers")
               (company-mode nil "company")
               (editorconfig-mode nil "editorconfig")
               (eldoc-mode nil "eldoc")
               (flycheck-mode nil "flycheck")
               (flymake-mode nil "flymake")
               (flyspell-mode nil "flyspell")
               (flyspell-prog-mode nil "flyspell")
               (global-anzu-mode nil "anzu")
               (global-company-mode nil "company")
               (global-flycheck-mode nil "flycheck")
               (global-prettier-mode nil "prettier")
               (global-whitespace-mode nil "whitespace")
               (lsp-mode nil "lsp-mode")
               (prettier-mode nil "prettier")
               (projectile-mode nil "projectile")
               (show-smartparens-global-mode nil "smartparens")
               (show-smartparens-mode nil "smartparens")
               (smartparens-global-mode nil "smartparens")
               (smartparens-mode nil "smartparens")
               (text-scale-mode nil "face-remap")
               (whitespace-mode nil "whitespace")
               (yas-global-mode nil "yasnippet")
               (yas-minor-mode nil "yasnippet"))))


  ;; --------------------------------------------------------------------------
  ;; デスクトップ環境保存・復旧
  ;; --------------------------------------------------------------------------
  (leaf desktop
    :bind (;; "C-c d" プレフィクスを用いる
           ("C-c d c" . desktop-clear)
           ("C-c d C-s" . desktop-save)
           ("C-c d s" . desktop-save-in-desktop-dir)
           ("C-c d d" . desktop-remove)
           ("C-c d f" . desktop-change-dir)
           ("C-c d r" . desktop-revert))
    :custom ((desktop-save . 'ask-if-new)
             (desktop-load-locked-desktop . t)
             (desktop-missing-file-warning . t)
             ;; 必要最小限の情報のみ保存させる
             (desktop-locals-to-save . '(case-fold-search
                                         case-replace
                                         desktop-locals-to-save
                                         fill-column
                                         truncate-lines))
             (desktop-restore-frames . t)
             (desktop-restore-in-current-display . t)
             (desktop-restore-forces-onscreen . t)
             (desktop-restore-reuses-frames . t)
             (desktop-file-name-format . 'absolute)
             (desktop-restore-eager . t)
             (desktop-lazy-verbose . t)
             (desktop-lazy-idle-delay . 5))
    :global-minor-mode desktop-save-mode)


  ;; --------------------------------------------------------------------------
  ;; 行番号表示
  ;; --------------------------------------------------------------------------
  (leaf display-line-numbers
    :bind (("C-c l" . display-line-numbers-mode)))


  ;; --------------------------------------------------------------------------
  ;; ディレクトリブラウジング
  ;; --------------------------------------------------------------------------
  (leaf dired
    :hook ((dired-mode-hook . my-dired-mode-initialize))
    :init
    (defun my-dired-mode-initialize ()
      "Initialize `dired-mode'."
      ;; 常にすべての情報を表示（簡易モードにしない）
      (dired-hide-details-mode -1)))


  ;; --------------------------------------------------------------------------
  ;; ディレクトリブラウジング (`dired') 拡張
  ;; --------------------------------------------------------------------------
  (leaf dired+
    :el-get (dired+
             :type github
             :pkgname "emacsmirror/dired-plus")
    :after dired
    :require t
    :custom ((diredp-hide-details-initially-flag . nil)
             (diredp-hide-details-propagate-flag . nil)))


  ;; --------------------------------------------------------------------------
  ;; EditorConfig
  ;; --------------------------------------------------------------------------
  (leaf editorconfig
    :package t
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; GNU Emacs Lisp ドキュメント表示
  ;; --------------------------------------------------------------------------
  (leaf eldoc
    :hook ((emacs-lisp-mode-hook . eldoc-mode)
           (ielm-mode-hook . eldoc-mode)
           (lisp-interaction-mode-hook . eldoc-mode)
           (lisp-mode-hook . eldoc-mode))
    :custom ((eldoc-minor-mode-string . nil)
             (eldoc-idle-delay . 0.2)
             (eldoc-echo-area-use-multiline-p . 'truncate-sym-name-if-fit)))


  ;; --------------------------------------------------------------------------
  ;; コンテキストメニュー
  ;; --------------------------------------------------------------------------
  (leaf embark
    :package t
    :bind (("C-." . embark-act)
           ("C-;" . embark-dwim)
           ("C-x ? b" . embark-bindings))
    :custom ((prefix-help-command . #'embark-prefix-help-command))
    :config
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))


  ;; --------------------------------------------------------------------------
  ;; Embark ⇔ Consult 連携
  ;; --------------------------------------------------------------------------
  (leaf embark-consult
    :package t
    :after (embark consult)
    :hook ((embark-collect-mode . consult-preview-at-point-mode)))


  ;; --------------------------------------------------------------------------
  ;; カーソル下の数値を増減
  ;; --------------------------------------------------------------------------
  (leaf evil-numbers
    :package t
    :custom ((evil-numbers-pad-default . t))
    :bind (("C-3" . evil-numbers/dec-at-pt)
           ("C-4" . evil-numbers/inc-at-pt)))


  ;; --------------------------------------------------------------------------
  ;; `dired' における `find' コマンド実行 (Windows ONLY)
  ;;
  ;; See also:
  ;; `dired'
  ;; --------------------------------------------------------------------------
  (leaf find-dired
    :when (member system-type '(ms-dos windows-nt))
    :after find-dired
    :config
    ;; --------------------------------
    ;; HACK: `:custom' で設定すると `find-exec-terminator' の展開が
    ;;       `find-dired' の `eval-after-load' より先になりエラーとなる
    ;;       仕方なく `:config' で泥臭く設定しなければならない
    ;; --------------------------------
    (custom-set-variables
     `(find-ls-option ,(cons (format "-exec %s -ld {} %s"
                                     (executable-find "ls")
                                     find-exec-terminator)
                             "-ld"))))


  ;; --------------------------------------------------------------------------
  ;; 自動静的解析
  ;; --------------------------------------------------------------------------
  (leaf flycheck
    :package t
    :bind (("C-c f" . flycheck-mode))
    :hook ((after-init-hook . global-flycheck-mode))
    :custom ((flycheck-checker-error-threshold . nil)
             (flycheck-display-errors-delay . 0.5)
             (flycheck-idle-change-delay . 0.25)
             (flycheck-disabled-checkers . '(javascript-jscs)))
    :config
    ;; --------------------------------
    ;; HACK: `flycheck-checker-error-threshold' 以上の項目が出現すると
    ;;       生成されうる警告バッファの出現を抑制
    ;; --------------------------------
    (with-eval-after-load 'warnings
      (add-to-list 'warning-suppress-log-types '(flycheck syntax-checker)))

    ;; --------------------------------
    ;; PATCH: v.Nu サポート
    ;; --------------------------------
    (unless (flycheck-registered-checker-p 'vnu)
      ;; FIXME: v.Nu の標準出力が UTF-8 なので、環境によっては文字化けする
      (flycheck-define-checker vnu
        "A (X)HTML syntax and style checker using v.NU.

See also: `https://github.com/validator/validator'."
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
        :modes (html-mode nxhtml-mode))

      ;; 有効化
      (let ((target-and-other-checkers (member 'html-tidy flycheck-checkers)))
        (if target-and-other-checkers
            ;; デフォルトの (X)HTML チェッカ `html-tidy' と入れ替える
            (setcar target-and-other-checkers 'vnu)
          ;; 未追加ならリスト先頭に追加
          (add-to-list 'flycheck-checkers 'vnu))))

    ;; --------------------------------
    ;; PATCH: Sass（.scss/.sass 両形式）チェック時にキャッシュを使わせない
    ;; --------------------------------
    (dolist (checker '(scss sass))
      (if (and (flycheck-registered-checker-p checker)
               (not (member "-C" (flycheck-checker-arguments checker))))
          ;; あえて破壊的に変更（元のリストに追加したい）
          (nconc (get checker 'flycheck-command) '("-C"))))

    ;; --------------------------------
    ;; PATCH: temp ファイルのデフォルトコーディングシステムを、
    ;;        強制的に UTF-8 (LF) とする
    ;; --------------------------------
    ;; オーバーライド
    (defun flycheck-save-buffer-to-file (file-name)
      "Save the contents of the current buffer to FILE-NAME."
      ;; 他の部分は元定義と一致させる
      (make-directory (file-name-directory file-name) t)
      ;; FIXME: もっと柔軟に設定できるようにならないか？
      (let ((coding-system-for-write 'utf-8-unix) ; ここだけ変更・決め打ち
            (jka-compr-inhibit t))
        (write-region nil nil file-name nil 0))))


  ;; --------------------------------------------------------------------------
  ;; 自動静的解析拡張（モードライン変更）
  ;; --------------------------------------------------------------------------
  (leaf flycheck-color-mode-line
    :after flycheck
    :package t
    :hook ((flycheck-mode-hook . flycheck-color-mode-line-mode)))


  ;; --------------------------------------------------------------------------
  ;; 自動静的解析 (OLD)
  ;; --------------------------------------------------------------------------
  (leaf flymake
    :custom ((flymake-run-in-place . nil)))


  ;; --------------------------------------------------------------------------
  ;; 自動スペルチェッカ
  ;; --------------------------------------------------------------------------
  (leaf flyspell
    :hook (;; Full
           (markdown-mode-hook . flyspell-mode)
           (org-mode-hook . flyspell-mode)
           (text-mode-hook . flyspell-mode)
           ;; Comments ONLY
           (css-mode-hook . flyspell-prog-mode)
           (emacs-lisp-mode-hook . flyspell-prog-mode)
           (html-mode-hook . flyspell-prog-mode)
           (ielm-mode-hook . flyspell-prog-mode)
           (js-mode-hook . flyspell-prog-mode)
           (js2-mode-hook . flyspell-prog-mode)
           (lisp-interaction-mode-hook . flyspell-prog-mode)
           (lisp-mode-hook . flyspell-prog-mode)
           (php-mode-hook . flyspell-prog-mode)
           (scss-mode-hook . flyspell-prog-mode)
           (typescript-mode-hook . flyspell-prog-mode)
           (web-mode-hook . flyspell-prog-mode))
    :custom ((flyspell-delay . 1.0)))


  ;; --------------------------------------------------------------------------
  ;; フレーム
  ;; --------------------------------------------------------------------------
  (leaf frame
    :custom (;; フレームサイズ変更を px 単位で実行できるようにする
             (frame-resize-pixelwise . t))
    :config
    ;; カーソルは点滅させない
    (blink-cursor-mode -1)

    ;; 半透明化
    (if window-system
        (set-frame-parameter nil 'alpha '(90 . 80))))


  ;; --------------------------------------------------------------------------
  ;; フレームセット
  ;; --------------------------------------------------------------------------
  (leaf frameset
    ;; 全設定が完了してから実行しなければならない
    ;; 途中で追加される項目がありうるため
    :hook ((after-init-hook . my-frameset-initialize))
    :init
    (defun my-frameset-initialize ()
      "Initialize `frameset' when `after-init-hook' running."
      (when (listp frameset-filter-alist)
        ;; `desktop' で保存不要な項目はすべて `:never' にする
        (dolist (key '(background-color
                       foreground-color
                       font
                       frameset--text-pixel-height
                       frameset--text-pixel-width
                       GUI:font))
          (setcdr (assoc key frameset-filter-alist) :never)))))


  ;; --------------------------------------------------------------------------
  ;; `grep'
  ;; --------------------------------------------------------------------------
  (leaf grep
    :bind (("C-M-g" . rgrep))
    :init
    ;; --------------------------------
    ;; Windows ONLY
    ;; --------------------------------
    (leaf grep
      :when (member system-type '(ms-dos windows-nt))
      ;; ------------------------------
      ;; HACK: `autoload' 未対応変数を変更する必要があるため、
      ;;       明示的にロードさせる必要がある
      ;; ------------------------------
      :require t
      :custom (;; 例外が出るため NUL デバイスは使わせない
               (grep-use-null-device . nil))
      :config
      ;; PATH は通っていないが、`exec-path' は通っている場合を想定
      ;;
      ;; すべて `defvar' 定義なので、 `autoload' 前後での
      ;; `custom-set-variables' による設定は不可能
      ;; 明示的ロード後～関数実行前までに設定しなければならない
      (setq grep-program
            (purecopy (or (executable-find "grep")
                          "grep")))
      (setq find-program
            (purecopy (or (executable-find "find")
                          "find")))
      (setq xargs-program
            (purecopy (or (executable-find "xargs")
                          "xargs")))))


  ;; --------------------------------------------------------------------------
  ;; カレントカーソル行強調
  ;; --------------------------------------------------------------------------
  (leaf hl-line
    :custom ((global-hl-line-sticky-flag . t))
    :global-minor-mode global-hl-line-mode)


  ;; --------------------------------------------------------------------------
  ;; 特殊コメント強調
  ;; --------------------------------------------------------------------------
  (leaf hl-todo
    :package t
    :custom ((hl-todo-keyword-faces . '(("HOLD" . "#d0bf8f")
                                        ("TODO" . "#cc9393")
                                        ("NEXT" . "#dca3a3")
                                        ("THEM" . "#dc8cc3")
                                        ("PROG" . "#7cb8bb")
                                        ("OKAY" . "#7cb8bb")
                                        ("DONT" . "#5f7f5f")
                                        ("FAIL" . "#8c5353")
                                        ("DONE" . "#afd8af")
                                        ("NOTE"   . "#d0bf8f")
                                        ("KLUDGE" . "#d0bf8f")
                                        ("HACK"   . "#d0bf8f")
                                        ("TEMP"   . "#d0bf8f")
                                        ("FIXME"  . "#cc9393")
                                        ("XXX+"   . "#cc9393")
                                        ("CAUTION" . "#ffff66")
                                        ("WARNING" . "#ff6666"))))
    :global-minor-mode global-hl-todo-mode)


  ;; --------------------------------------------------------------------------
  ;; 強化バッファ一覧
  ;; --------------------------------------------------------------------------
  (leaf ibuffer
    :bind (("C-x C-b" . ibuffer))
    :custom ((ibuffer-default-sorting-mode . 'filename/process)
             (ibuffer-expert . t))
    :config
    ;; --------------------------------
    ;; 機能拡張
    ;; --------------------------------
    ;; バッファ名の表示を30文字に拡張
    ;; カラム幅が揃わなくなるため、-1 にはできない
    (let* (;; `customize-mark-to-save' の評価を t にするため、
           ;; 明示的にコピー
           (formats (copy-tree ibuffer-formats))
           (settings (assoc 'name (assoc 'mark formats))))
      ;; 該当する設定項目がなければ何もしない
      ;; 将来的に項目が変更された場合でも、例外を出さないための対策
      (when settings
        (setcdr settings '(30 30 :left :elide))
        ;; WARNING: この `custom-set-variables' は `:custom' に移動できない
        ;;          変数 `settings' で加工を行った結果が入るため
        (custom-set-variables
         `(ibuffer-formats ',formats)))))


  ;; --------------------------------------------------------------------------
  ;; 強化バッファ一覧 (`ibuffer') 拡張（`projectile' サポート）
  ;; --------------------------------------------------------------------------
  (leaf ibuffer-projectile
    :after (ibuffer projectile)
    :package t
    :hook ((ibuffer-hook . ibuffer-projectile-set-filter-groups)))


  ;; --------------------------------------------------------------------------
  ;; ファイル操作の簡略化
  ;; --------------------------------------------------------------------------
  (leaf ido
    :custom ((ido-enable-flex-matching . t)
             (ido-create-new-buffer . 'always)
             (ido-use-virtual-buffers . t)
             (ido-max-file-prompt-width . 0)
             (ido-use-filename-at-point . 'guess)
             (ido-unc-hosts . t)
             ;; ローカル環境にのみ保存
             (ido-save-directory-list-file . "~/.emacs.ido-save-directory-list.el"))
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; ファイル操作の簡略化（全環境に適用）
  ;; --------------------------------------------------------------------------
  (leaf ido-everywhere
    :after ido
    :config
    (ido-everywhere +1))


  ;; --------------------------------------------------------------------------
  ;; 画像の直接表示
  ;; --------------------------------------------------------------------------
  (leaf image-file
    :global-minor-mode auto-image-file-mode)


  ;; --------------------------------------------------------------------------
  ;; インクリメンタル検索
  ;; --------------------------------------------------------------------------
  (leaf isearch
    :custom ((isearch-case-fold-search . t)
             (isearch-last-case-fold-search . t)))


  ;; --------------------------------------------------------------------------
  ;; アーカイブファイルを直接編集
  ;; --------------------------------------------------------------------------
  (leaf jka-cmpr-hook
    :global-minor-mode auto-compression-mode)


  ;; --------------------------------------------------------------------------
  ;; JavaScript リファクタリング補助
  ;; --------------------------------------------------------------------------
  (leaf js2-refactor
    :after js2-mode
    :package t
    :require t)


  ;; --------------------------------------------------------------------------
  ;; LSP (Language Server Protocol) クライアント拡張 (UI)
  ;; --------------------------------------------------------------------------
  ;; WARNING: `lsp-mode' が自動ロードする
  ;;          念のため `lsp-mode' より前にインストール
  ;; --------------------------------------------------------------------------
  (leaf lsp-ui
    :package t
    :custom ((lsp-ui-sideline-show-hover . t)
             (lsp-ui-sideline-show-code-actions . t)
             (lsp-ui-sideline-diagnostic-max-lines . 2)
             (lsp-ui-sideline-diagnostic-max-line-length . 150)))


  ;; --------------------------------------------------------------------------
  ;; LSP (Language Server Protocol) クライアント拡張 (Java)
  ;; --------------------------------------------------------------------------
  ;; WARNING: `lsp-mode' が自動ロードする
  ;;          念のため `lsp-mode' より前にインストール
  ;; --------------------------------------------------------------------------
  (leaf lsp-java
    :package t
    :custom (;; 旧バージョン（11.x 系）を利用するため旧い `jdtls' を指定
             (lsp-java-jdt-download-url . "https://download.eclipse.org/jdtls/milestones/1.12.0/jdt-language-server-1.12.0-202206011637.tar.gz"))
    :init
    (leaf lsp-java
      :after lsp-java
      :init
      ;; For "DAP"
      (leaf dap-java
        :require t)
      ;; WARNING: 確実に `defcustom' 定義済変数が存在する状態で、
      ;;          「定義」ではなく「追加」
      (add-to-list 'lsp-java-vmargs "-Djsse.enableSNIExtension=false")
      (add-to-list 'lsp-java-9-args "-Djsse.enableSNIExtension=false"))
    ;; For "SpringBoot"
    (leaf lsp-java-boot
      :require t
      :hook ((lsp-mode-hook . lsp-lens-mode)
             (java-mode-hook . lsp-java-boot-lens-mode))))


  ;; --------------------------------------------------------------------------
  ;; LSP (Language Server Protocol) クライアント拡張 (Tailwind CSS)
  ;; --------------------------------------------------------------------------
  ;; WARNING: `lsp-mode' が自動ロードする
  ;;          念のため `lsp-mode' より前にインストール
  ;; --------------------------------------------------------------------------
  (leaf lsp-tailwindcss
    :package t
    :custom ((lsp-tailwindcss-add-on-mode . t)
             (lsp-tailwindcss-server-version . "0.8.7")))


  ;; --------------------------------------------------------------------------
  ;; LSP (Language Server Protocol) クライアント
  ;;
  ;; See also:
  ;; https://microsoft.github.io/language-server-protocol/
  ;; https://langserver.org/
  ;; --------------------------------------------------------------------------
  (leaf lsp-mode
    :package t
    :hook (;; 有効化は必要最小限にとどめる
           (css-mode-hook . lsp)
           (java-mode-hook . lsp)
           (js-mode-hook . lsp)
           (js2-mode-hook . lsp)
           (json-mode-hook . lsp)
           (php-mode-hook . lsp)
           (scss-mode-hook . lsp)
           (typescript-mode-hook . lsp)
           (web-mode-hook . lsp)
           (yaml-mode-hook . lsp))
    :custom (;; ローカル環境にのみ保存
             (lsp-session-file . "~/.emacs.lsp-session")
             ;; LSP サーバからのファイル監視要求を無視
             ;;
             ;; GNU Emacs の仕様では 1024 - 50 = 974 個以上のファイル監視を
             ;; 登録できない
             ;; LSP サーバによっては大量のファイル監視要求を行うため、意図的に無視
             ;;
             ;; See also:
             ;; https://www.reddit.com/r/emacs/comments/mq2znn/no_file_descriptors_left/
             ;; https://apple.stackexchange.com/a/418699
             ;; https://github.com/emacs-mirror/emacs/blob/0008003c3e466269074001d637cda872d6fee9be/src/kqueue.c#L387-L401
             (lsp-enable-file-watchers . nil)
             (lsp-eldoc-render-all . t)
             (lsp-headerline-breadcrumb-enable . nil)
             (lsp-signature-doc-lines . t)
             (lsp-progress-function . 'ignore)
             (lsp-warn-no-matched-clients . nil)))


  ;; --------------------------------------------------------------------------
  ;; 補完候補一覧の側に項目情報を表示
  ;; --------------------------------------------------------------------------
  (leaf marginalia
    :package t
    :custom ((marginalia-field-width . 200)
             (marginalia-max-relative-age . most-positive-fixnum))
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; ローマ字入力から日本語をインクリメンタル検索
  ;; --------------------------------------------------------------------------
  (leaf migemo
    :leaf-defer nil
    :after exec-path-from-shell
    :package t
    :require t
    :custom `(;; C/Migemo 利用設定
              (migemo-command . ,(executable-find "cmigemo"))
              (migemo-options . '("-q" "--emacs"))
              ;; 空白文字と認識させる対象を広げる
              (migemo-white-space-regexp . "[[:space:]\s-]*")
              ;; ユーザ別基礎ディレクトリは設定ディレクトリ内にまとめる
              (migemo-directory . ,(convert-standard-filename "~"))
              ;; `migemo' 側で定義されている `isearch' 関連キーバインドを使わせない
              ;; ミニバッファ内で `yank' できない現象が発生する問題の対策
              (migemo-use-default-isearch-keybinding . nil)
              ;; 辞書ファイルはデフォルトのものを利用
              (migemo-dictionary . ,(convert-standard-filename
                                     (if (member system-type '(ms-dos windows-nt))
                                         "C:/programs/cmigemo/share/migemo/utf-8/migemo-dict"
                                       "/usr/local/share/migemo/utf-8/migemo-dict")))
              (migemo-user-dictionary . nil)
              (migemo-regex-dictionary . nil)
              ;; 辞書エンコーディングを明示
              (migemo-coding-system . 'utf-8-unix)
              ;; キャッシュを使わせる
              (migemo-use-pattern-alist . t)
              (migemo-use-frequent-pattern-alist . t)
              (migemo-pattern-alist-length . 1024)
              ;; ローカル環境にのみ保存
              (migemo-pattern-alist-file . "~/.emacs.migemo-pattern")
              (migemo-frequent-pattern-alist-file . "~/.emacs.migemo-frequent"))
    :config
    (migemo-init))


  ;; --------------------------------------------------------------------------
  ;; 無順序スペース区切り補完
  ;; --------------------------------------------------------------------------
  (leaf orderless
    :package t
    :require t
    :custom ((completion-styles . '(orderless)))
    :config
    ;; `migemo' 利用可能時
    ;;
    ;; See also:
    ;; https://nyoho.jp/diary/?date=20210615
    (leaf orderless
      :after migemo
      :config
      (defun my-orderless-migemo (component)
        "Match COMPONENT as `migemo'."
        (let ((pattern (migemo-get-pattern component)))
          (condition-case nil
              (progn (string-match-p pattern "") pattern)
            (invalid-regexp nil))))

      (add-to-list 'orderless-matching-styles #'my-orderless-migemo t)))


  ;; --------------------------------------------------------------------------
  ;; 汎用フォーマッタ
  ;; --------------------------------------------------------------------------
  (leaf prettier
    :package t
    :hook ((after-init-hook . global-prettier-mode)))


  ;; --------------------------------------------------------------------------
  ;; 汎用プロジェクト管理
  ;; --------------------------------------------------------------------------
  (leaf projectile
    :package t
    :require t
    :custom `((projectile-enable-caching . t)
              (projectile-completion-system . ',(cond ((featurep 'ido) 'ido)
                                                      (t 'default)))
              (projectile-keymap-prefix . ,(kbd "C-c C-p"))
              ;; ローカル環境にのみ保存
              (projectile-cache-file . "~/.emacs.projectile.cache")
              (projectile-known-projects-file . "~/.emacs.projectile-bookmarks.eld"))
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; 自動カラー表示
  ;; --------------------------------------------------------------------------
  (leaf rainbow-mode
    :after rainbow-mode
    :config
    (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode))


  ;; --------------------------------------------------------------------------
  ;; ファイル履歴保存
  ;; --------------------------------------------------------------------------
  (leaf recentf
    :custom (;; 履歴保存数は絞る
             (recentf-max-saved-items . 20)
             ;; ローカル環境にのみ保存
             (recentf-save-file . "~/.emacs.recentf.el")))


  ;; --------------------------------------------------------------------------
  ;; ミニバッファの履歴を残す
  ;; --------------------------------------------------------------------------
  (leaf savehist
    :custom (;; 履歴保存数は絞る
             (history-length . 100)
             ;; ローカル環境にのみ保存
             (savehist-file . "~/.emacs.savehist.el"))
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; ファイルごとにカーソル位置を保存
  ;; --------------------------------------------------------------------------
  (leaf saveplace
    :require t
    :custom (;; ローカル環境にのみ保存
             (save-place-file . "~/.emacs.saveplace.el"))
    :global-minor-mode save-place-mode)


  ;; --------------------------------------------------------------------------
  ;; スクロールバー
  ;; --------------------------------------------------------------------------
  (leaf scroll-bar
    ;; `after-init-hook' で実行しないと適用されない問題がある
    :hook ((after-init-hook . my-scroll-bar-initilalize))
    :init
    ;; ウインドウシステム上では、あらゆるスクロールバーを非表示化
    (defun my-scroll-bar-initilalize ()
      "Initialize `scroll-bar'."
      (with-eval-after-load 'scroll-bar
        (when window-system
          (scroll-bar-mode -1)
          (horizontal-scroll-bar-mode -1)))))


  ;; --------------------------------------------------------------------------
  ;; 基礎編集コマンド集
  ;; --------------------------------------------------------------------------
  (leaf simple
    ;; 暫定マークを使用
    :global-minor-mode transient-mark-mode)


  ;; --------------------------------------------------------------------------
  ;; 各種カッコ関連機能拡張
  ;; --------------------------------------------------------------------------
  (leaf smartparens
    :package t
    :require t
    :custom ((sp-show-pair-from-inside . t)
             (sp-undo-pairs-separately . t))
    :config
    ;; 公式デフォルト設定
    (leaf smartparens-config
      :after smartparens
      :require t)
    :global-minor-mode (show-smartparens-global-mode smartparens-global-mode))


  ;; --------------------------------------------------------------------------
  ;; 同時置換
  ;; --------------------------------------------------------------------------
  (leaf substitute
    :package t
    :bind (("C-M-b" . substitute-target-in-buffer))
    :custom ((substitute-highlight . t)))


  ;; --------------------------------------------------------------------------
  ;; ツールチップ
  ;; --------------------------------------------------------------------------
  (leaf tooltip
    :config
    ;; 非表示
    (tooltip-mode -1))


  ;; --------------------------------------------------------------------------
  ;; `redo' 追加
  ;; --------------------------------------------------------------------------
  (leaf undo-fu
    :package t
    :require t
    :bind (("C-/" . undo-fu-only-undo)
           ("C-?" . undo-fu-only-redo)))


  ;; --------------------------------------------------------------------------
  ;; 垂直インタラクティブ補完
  ;; --------------------------------------------------------------------------
  (leaf vertico
    :package t
    :custom ((vertico-count . 20)
             (vertico-cycle . t)
             (vertico-sort-function . #'vertico-sort-history-alpha))
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; 巨大ファイル表示
  ;; --------------------------------------------------------------------------
  (leaf vlf
    :package t
    :require vlf-setup
    :bind ((:vlf-mode-map
            :package vlf
            ("C-c C-v" . vlf-prefix-map)))
    :custom `((vlf-batch-size . ,(* 1 1024 1024)) ;; 1MB
              (vlf-application . 'dont-ask)))


  ;; --------------------------------------------------------------------------
  ;; 空白文字強調
  ;; --------------------------------------------------------------------------
  (leaf whitespace
    :hook ((after-change-major-mode-hook . my-whitespace-mode-initialize))
    :custom (;; 「不正」位置の空白文字のみ強調
             (whitespace-style . '(empty
                                   face
                                   newline
                                   newline-mark
                                   space-after-tab
                                   space-before-tab
                                   space-mark ; HARD SPACE の ON/OFF も含む
                                   spaces ; HARD SPACE の ON/OFF も含む
                                   tab-mark
                                   tabs
                                   trailing))
             ;; ----------------------
             ;; HACK: 全角空白 (U+3000) を HARD SPACE とみなして強調表示
             ;;
             ;; 表示テスト:
             ;;   U+0009: 「	」
             ;;   U+00A0: 「 」
             ;;   U+3000: 「　」
             ;; ----------------------
             (whitespace-hspace-regexp . "\\(\u00A0\\|\u08A0\\|\u0920\\|\u0E20\\|\u0F20\\|\u3000\\)+")
             (whitespace-trailing-regexp . "\\([\t \u00A0\u3000]+\\)$")
             ;; 行カラム最大値は `fill-column' を参照させる
             (whitespace-line-column . nil)
             ;; ----------------------
             ;; HACK: 半角空白 (U+0020) を強調しないようにする
             ;;
             ;; 表示テスト:
             ;;   U+0020: 「 」
             ;; ----------------------
             (whitespace-display-mappings . '(;; EOL -> DOLLAR SIGN
                                              (newline-mark ?\n [?$ ?\n])
                                              ;; TAB -> CURRENCY SIGN
                                              (space-mark ?\u00A0 [?¤] [?_])
                                              ;; IDEOGRAPHIC SPACE -> WHITE SQUARE
                                              (space-mark ?\u3000 [?\u25a1] [?_ ?_])
                                              ;; Tab -> RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
                                              (tab-mark ?\t [?» ?\t] [?\\ ?\t]))))
    :custom-face ((whitespace-space . '((t
                                         (:background nil)))))
    :init
    (defun my-whitespace-mode-initialize ()
      "Initialize `whitespace'."
      ;; ------------------------------
      ;; HACK: 一部メジャーモードでは無効化
      ;; ------------------------------
      (with-eval-after-load 'whitespace
        (if (member major-mode '(;; 降順ソート
                                 lisp-interaction-mode
                                 ))
            (whitespace-mode -1))))
    :global-minor-mode global-whitespace-mode)


  ;; --------------------------------------------------------------------------
  ;; ウインドウの状態履歴を undo/redo
  ;; --------------------------------------------------------------------------
  (leaf winner
    :global-minor-mode t)


  ;; --------------------------------------------------------------------------
  ;; スニペット挿入
  ;; --------------------------------------------------------------------------
  (leaf yasnippet
    :package t
    :global-minor-mode yas-global-mode)


  ;; --------------------------------------------------------------------------
  ;; `yasnippet' 公式コレクション
  ;; --------------------------------------------------------------------------
  (leaf yasnippet-snippets
    :package t
    :after yasnippet
    :require t)
  ) ; End of *minor-mode


;; ============================================================================
;; メジャーモード
;; ============================================================================
(leaf *major-mode
  :config
  ;; --------------------------------------------------------------------------
  ;; Apache
  ;; --------------------------------------------------------------------------
  (leaf apache-mode
    :package t
    :mode (("\\.conf\\'" . apache-mode))
    :config
    (setq-local apache-indent-level 4))


  ;; --------------------------------------------------------------------------
  ;; CSS
  ;; --------------------------------------------------------------------------
  (leaf css-mode
    :hook ((css-mode-hook . my-css-mode-initialize))
    :custom ((css-indent-offset . 2))
    :init
    (defun my-css-mode-initialize ()
      "Initialize `css-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; Docker's Dockerfile
  ;; --------------------------------------------------------------------------
  (leaf dockerfile-mode
    :package t
    :hook ((dockerfile-mode-hook . my-dockerfile-mode-initialize))
    :init
    (defun my-dockerfile-mode-initialize ()
      "Initialize `dockerfile-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; GNU Emacs Lisp
  ;; --------------------------------------------------------------------------
  (leaf elisp-mode
    :hook ((emacs-lisp-mode-hook . my-emacs-lisp-mode-initialize)
           (lisp-interaction-mode-hook . my-emacs-lisp-mode-initialize)
           (lisp-interaction-mode-hook . my-lisp-interaction-mode-initialize))
    :init
    ;; 共通
    (defun my-emacs-lisp-mode-initialize ()
      "Initialize `emacs-lisp-mode' and `lisp-interaction-mode' before file load."
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 8)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab"))
                   (tab-width-number-data (gethash 'tab_width editorconfig-properties-hash))
                   (tab-width-number (if (and tab-width-number-data
                                              (stringp tab-width-number-data))
                                         (string-to-number tab-width-number-data)
                                       tab-width)))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style))
              (if (not (equal tab-width tab-width-number))
                  (setq-local tab-width tab-width-number))))))

    ;; `lisp-interaction-mode' ONLY
    (defun my-lisp-interaction-mode-initialize ()
      "Initialize `lisp-interaction-mode-initialize' before file load."
      ;; EMPTY
      ))


  ;; ----------------------------------------------------------------------------
  ;; GraphQL
  ;; ----------------------------------------------------------------------------
  (leaf graphql-mode
    :package t
    :hook ((graphql-mode-hook . my-haml-mode-initialize))
    :init
    (defun my-graphql-mode-initialize ()
      "Initialize `graphql-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; ----------------------------------------------------------------------------
  ;; Haml
  ;; ----------------------------------------------------------------------------
  (leaf haml-mode
    :package t
    :hook ((haml-mode-hook . my-haml-mode-initialize))
    :init
    (defun my-haml-mode-initialize ()
      "Initialize `haml-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; Emacs Lisp インタラクション
  ;; --------------------------------------------------------------------------
  (leaf ielm
    :hook ((ielm-mode-hook . my-ielm-mode-initialize))
    :init
    (defun my-ielm-mode-initialize ()
      "Initialize `ielm' major mode before file load."
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 8)))


  ;; --------------------------------------------------------------------------
  ;; Java
  ;; --------------------------------------------------------------------------
  (leaf java-mode
    :hook ((java-mode-hook . my-java-mode-initialize))
    :init
    (defun my-java-mode-initialize ()
      "Initialize `java-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; JavaScript (Basic)
  ;; --------------------------------------------------------------------------
  (leaf js-mode
    :hook ((js-mode-hook . my-js-mode-initialize))
    :custom ((js-indent-level . 4)
             (js-expr-indent-offset . 0)
             (js-paren-indent-offset . 0)
             (js-square-indent-offset . 0)
             (js-curly-indent-offset . 0)
             (js-switch-indent-offset . 0)
             (js-flat-functions . nil)
             (js-indent-align-list-continuation . t)
             (js-js-switch-tabs . t)
             (js-indent-first-init . 'dynamic)
             (js-chain-indent . t))
    :init
    (defun my-js-mode-initialize ()
      "Initialize `js-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; JavaScript (Expert)
  ;; --------------------------------------------------------------------------
  (leaf js2-mode
    :package t
    :mode (("\\.es[0-9]\\'" . js2-mode)
           ("\\.js\\'" . js2-mode)
           ("\\.pac\\'" . js2-mode))
    :hook ((js2-mode-hook . my-js2-mode-initialize))
    :custom ((js2-highlight-level . 3) ; すべての構文強調を有効化
             (js2-bounce-indent-p . nil)
             (js2-idle-timer-delay . 0.25)
             (js2-dynamic-idle-timer-adjust . 0)
             (js2-concat-multiline-strings . t)
             ;; 文法チェック関連
             ;;
             ;; 他ツールに任せるため、すべて無効化
             (js2-mode-show-parse-errors . nil)
             (js2-mode-assume-strict . nil)
             (js2-mode-show-strict-warnings . nil)
             (js2-strict-trailing-comma-warning . nil)
             (js2-strict-missing-semi-warning . nil)
             (js2-missing-semi-one-line-override . nil)
             (js2-strict-inconsistent-return-warning . nil)
             (js2-strict-cond-assign-warning . nil)
             (js2-strict-var-redeclaration-warning . nil)
             (js2-strict-var-hides-function-arg-warning . nil)
             ;; その他
             (js2-skip-preprocessor-directives . t)
             (js2-language-version . 200)
             (js2-instanceof-has-side-effects . nil)
             (js2-move-point-on-right-click . nil) ; 使わない
             (js2-allow-rhino-new-expr-initializer . nil) ; 使わない
             (js2-allow-member-expr-as-function-name . nil)
             (js2-include-browser-externs . t)
             (js2-include-rhino-externs . nil)
             (js2-include-node-externs . nil)
             (js2-mode-indent-inhibit-undo . nil)
             (js2-mode-indent-ignore-first-tab . nil)
             (js2-highlight-external-variables . t)
             (js2-warn-about-unused-function-arguments . nil)
             ;; JSLint
             ;;
             ;; 他ツールに任せるため、すべて無効化
             (js2-include-jslint-globals . nil)
             (js2-include-jslint-declaration-externs . nil))
    :init
    (defun my-js2-mode-initialize ()
      "Initialize `js2-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; JSON
  ;; --------------------------------------------------------------------------
  (leaf json-mode
    :package t
    :mode (("\\.json\\'" . json-mode))
    :hook ((json-mode-hook . my-json-mode-initialize))
    :init
    (defun my-json-mode-initialize ()
      "Initialize `json-mode' before file load."
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 2)
      (setq-local js-indent-level tab-width)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab"))
                   (tab-width-number-data (gethash 'tab_width editorconfig-properties-hash))
                   (tab-width-number (if (and tab-width-number-data
                                              (stringp tab-width-number-data))
                                         (string-to-number tab-width-number-data)
                                       tab-width)))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style))
              (if (not (equal tab-width tab-width-number))
                  (setq-local tab-width tab-width-number))
              (if (not (equal js-indent-level tab-width))
                  (setq-local js-indent-level tab-width)))))))


  ;; --------------------------------------------------------------------------
  ;; Lisp
  ;; --------------------------------------------------------------------------
  (leaf lisp-mode
    :hook ((lisp-mode-hook . my-lisp-mode-initialize))
    :init
    (defun my-lisp-mode-initialize ()
      "Itialize `lisp-mode' before file load."
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 8)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab"))
                   (tab-width-number-data (gethash 'tab_width editorconfig-properties-hash))
                   (tab-width-number (if (and tab-width-number-data
                                              (stringp tab-width-number-data))
                                         (string-to-number tab-width-number-data)
                                       tab-width)))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style))
              (if (not (equal tab-width tab-width-number))
                  (setq-local tab-width tab-width-number)))))))


  ;; --------------------------------------------------------------------------
  ;; Markdown
  ;; --------------------------------------------------------------------------
  (leaf markdown-mode
    :package t
    :hook ((markdown-mode-hook . my-markdown-mode-initialize))
    :custom `((markdown-command . ,(or (executable-find "github-markup")
                                       (executable-find "markdown")
                                       "markdown"))
              (markdown-command-needs-filename . ,(executable-find "github-markup"))
              (markdown-coding-system . 'utf-8-unix)
              ;; プレビュー用に生成した実 HTML ファイルの残存を防ぐ
              (markdown-live-preview-delete-export . 'delete-on-export))
    :init
    (defun my-markdown-mode-initialize ()
      "Initialize `markdown-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style))))))
    :config
    ;; プレーンテキストファイルは除外
    (setq auto-mode-alist
          (delete '("\\.text\\'" . markdown-mode) auto-mode-alist)))


  ;; --------------------------------------------------------------------------
  ;; Mustache
  ;;
  ;; See also:
  ;; https://mustache.github.io/
  ;; --------------------------------------------------------------------------
  (leaf mustache-mode
    :package t
    :hook ((mustache-mode-hook . my-mustache-mode-initialize))
    :init
    (defun my-mustache-mode-initialize ()
      "Initialize `mustache-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; PHP
  ;; --------------------------------------------------------------------------
  (leaf php-mode
    :package t
    :hook ((php-mode-hook . my-php-mode-initialize))
    :init
    (defun my-php-mode-initialize ()
      "Initialize `php-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; TypeScript
  ;; --------------------------------------------------------------------------
  (leaf typescript-mode
    :package t
    :mode (("\\.tsx?\\'" . typescript-mode))
    :hook ((typescript-mode-hook . my-typescript-mode-initialize))
    :init
    (defun my-typescript-mode-initialize ()
      "Initialize `typescript-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; Sass (extension: ".scss")
  ;; --------------------------------------------------------------------------
  (leaf scss-mode
    :package t
    :hook ((scss-mode-hook . my-scss-mode-initialize))
    :custom (;; コンパイルは常に手動（保存時は何もしない）
             ;; 各種ツール経由でコンパイルされうるため
             (scss-compile-at-save . nil))
    :init
    (defun my-scss-mode-initialize ()
      "Initialize `scss-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; SGML, (X)HTML
  ;; --------------------------------------------------------------------------
  (leaf sgml-mode
    :mode (("\\.[sx]?html?\\'" . html-mode))
    :hook ((html-mode-hook . my-html-mode-initialize)
           (sgml-mode-hook . my-sgml-mode-initialize))
    :init
    ;; SGML
    (defun my-sgml-mode-initialize ()
      "Initialize `sgml-mode' before file load."
      (setq-local indent-tabs-mode nil)

      (when (featurep 'sgml-electric-tag-pair-mode)
        (declare-function sgml-electric-tag-pair-mode "sgml-mode")
        (sgml-electric-tag-pair-mode +1))

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style))))))

    ;; (X)HTML
    (defun my-html-mode-initialize ()
      "Initialize `html-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; Terraform
  ;; --------------------------------------------------------------------------
  (leaf terraform-mode
    :package t
    :hook ((terraform-mode-hook . my-terraform-mode-initialize))
    :init
    (defun my-terraform-mode-initialize ()
      "Initialize `terraform-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; TeX
  ;; --------------------------------------------------------------------------
  (leaf tex-mode
    :hook ((tex-mode-hook . my-tex-mode-initialize))
    :init
    (defun my-tex-mode-initialize ()
      "Initialize `tex-mode' before file load."
      (setq-local truncate-lines nil)))


  ;; --------------------------------------------------------------------------
  ;; Text
  ;; --------------------------------------------------------------------------
  (leaf text-mode
    :hook ((text-mode-hook . my-text-mode-initialize))
    :init
    (defun my-text-mode-initialize ()
      "Initialize `text-mode' before file load."
      (setq-local truncate-lines nil)))


  ;; --------------------------------------------------------------------------
  ;; Template Toolkit (tt, written by Perl)
  ;;
  ;; See also:
  ;; http://tt2.org/
  ;; --------------------------------------------------------------------------
  (leaf tt-mode
    :package t)


  ;; --------------------------------------------------------------------------
  ;; Web
  ;; --------------------------------------------------------------------------
  (leaf web-mode
    :package t
    :mode (("\\.njk\\'" . web-mode)
           ("\\.vue\\'" . web-mode))
    :hook ((web-mode-hook . my-web-mode-initialize))
    :custom ((web-mode-block-padding . nil)
             (web-mode-part-padding . nil)
             (web-mode-script-padding . nil)
             (web-mode-style-padding . nil)
             (web-mode-attr-indent-offset . nil)
             (web-mode-attr-value-indent-offset . nil)
             (web-mode-markup-indent-offset . 0)
             (web-mode-css-indent-offset . 0)
             (web-mode-code-indent-offset . 0)
             (web-mode-sql-indent-offset . 0)
             (web-mode-enable-css-colorization . ,(display-graphic-p))
             (web-mode-enable-comment-interpolation . nil)
             (web-mode-enable-comment-annotation . nil)
             (web-mode-enable-auto-indentation . nil)
             (web-mode-enable-auto-closing . nil)
             (web-mode-enable-auto-pairing . nil)
             (web-mode-enable-auto-opening . nil)
             (web-mode-enable-auto-quoting . nil)
             (web-mode-enable-auto-expanding . nil)
             (web-mode-enable-curly-brace-indentation . nil)
             (web-mode-enable-control-block-indentation . nil)
             (web-mode-enable-current-element-highlight . t)
             (web-mode-enable-current-column-highlight . t)
             (web-mode-enable-whitespace-fontification . nil)
             (web-mode-enable-html-entities-fontification . t)
             (web-mode-enable-block-face . t)
             (web-mode-enable-part-face . t)
             (web-mode-enable-inlays . t)
             (web-mode-enable-sexp-functions . t)
             (web-mode-enable-string-interpolation . t)
             (web-mode-enable-literal-interpolation . t)
             (web-mode-enable-sql-detection . t)
             (web-mode-enable-heredoc-fontification . t)
             (web-mode-enable-element-content-fontification . t)
             (web-mode-enable-element-tag-fontification . t)
             (web-mode-enable-front-matter-block . t)
             (web-mode-enable-engine-detection . t)
             (web-mode-enable-optional-tags . t)
             (web-mode-comment-style . 1)
             (web-mode-indent-style . 1)
             (web-mode-auto-close-style . 1)
             (web-mode-auto-quote-style . 1))
    :init
    (defun my-web-mode-initialize ()
      "Initialize `web-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; XML
  ;; --------------------------------------------------------------------------
  (leaf nxml-mode
    :mode (("\\.xml\\'" . nxml-mode)
           ("\\.plist\\'" . nxml-mode))
    :hook ((nxml-mode-hook . my-nxml-mode-initialize))
    :custom ((nxml-child-indent . 2)
             (nxml-attribute-indent . 0)
             (nxml-slash-auto-complete-flag . t)
             (nxml-bind-meta-tab-to-complete-flag . t)
             (nxml-sexp-element-flag . t)
             (nxml-char-ref-display-glyph-flag . t))
    :init
    (defun my-nxml-mode-initialize ()
      "Initialize `nxml-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))


  ;; --------------------------------------------------------------------------
  ;; YAML
  ;; --------------------------------------------------------------------------
  (leaf yaml-mode
    :package t
    :hook ((yaml-mode-hook . my-yaml-mode-initialize))
    :custom ((yaml-indent-offset . 2))
    :init
    (defun my-yaml-mode-initialize ()
      "Initialize `yaml-mode' before file load."
      (setq-local indent-tabs-mode nil)

      ;; EditorConfig 対応
      (with-eval-after-load 'editorconfig
        (if (hash-table-p editorconfig-properties-hash)
            (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                   (indent-style (equal indent-style-data "tab")))
              (if (not (equal indent-tabs-mode indent-style))
                  (setq-local indent-tabs-mode indent-style)))))))
  ) ; End of *major-mode


;; ============================================================================
;; フォント
;;
;; 独自定義したフォント設定
;; ============================================================================
;; 文字幅調整テスト：
;;   aa| アルファベット
;;   ıı| ラテン文字
;;   あ| ひらがな（日本語）
;;   简| 簡体字
;;   粵| 繁体字
;;   한| ハングル
;;   ไไ| タイ文字
;;   ░▓| 記号
;;   😊| 絵文字
;; ============================================================================
;; 波ダッシュ字形テスト：
;;   「〜」(U+301C: WAVE DASH)
;;   「～」(U+FF5E: FULLWIDTH TILDE)
;; ============================================================================
;; 文字拡大・縮小モード：
;;   C-x C-0
;;
;; カーソルがポイントしている文字の「簡易」情報を表示：
;;   C-x =
;;
;; カーソルがポイントしている文字の「詳細」情報を表示：
;;   C-u C-x =
;;
;; 各種フォントセットの詳細を、別バッファに表示：
;;   M-x describe-fontset
;;
;; 定義されているフォントセットの一覧を、別バッファに表示：
;;   M-x list-fontsets
;;
;; 利用可能なフォントの一覧：
;;   (dolist (xlfd (x-list-fonts "*")) (insert (format "%S" xlfd) "\n"))
;;
;; 該当ファミリフォントの一覧：
;;   (list-fonts (font-spec :family "ファミリ名"))
;;
;; 定義されているフォントセットの一覧：
;;   (fontset-list)
;;
;; 定義されているフォントセットと、別名（短縮名、エイリアス）の alist：
;;   fontset-alias-alist
;;
;; フレームが使用中のフォントを表示：
;;   (frame-parameter nil 'font)
;; ============================================================================
;; Microsoft Code page 858 (`cp858')
;;
;; 概要：
;;   * ラテン文字
;;   * 基底である ISO/IEC 8859-1 にはない文字を補完するときに利用する
;;   * `cp858' は `cp850' の "ı" (U+0131) を "€" (U+20AC) に置換したもの
;;   * `cp858' は `cp585' に "€" (U+20AC) を追加したもの
;;
;; 追加文字:
;;   ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿƒ‗€─│┌┐└┘├┤┬┴┼═║╔╗╚╝╠╣╦╩╬▀▄█░▒▓■
;;
;; 半角フォントで表示されてほしいもの：
;;   ¡¢£¤¥¦©ª«¬®¯²³µ·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿƒ‗€═║╔╗╚╝╠╣╦╩╬▀▄█░▒▓
;;
;; 全角フォントで表示されてほしいもの：
;;   §¨°±´¶×÷─│┌┐└┘├┤┬┴┼■
;;
;; フォントによっては、他の文字と判別しにくいもの：
;;   "Ø" (U+00d8: LATIN CAPITAL LETTER O WITH STROKE)
;;
;; See also:
;; https://en.wikipedia.org/wiki/Code_page_858
;; https://en.wikipedia.org/wiki/Code_page_850
;; ============================================================================
;; Microsoft Code page 932 (`cp932')
;;
;; 概要：
;;   * 日本語
;;   * 俗称「Microsoft Shift_JIS」
;;   * `cp932' にはあるが、JIS X 0213:2004 にはない文字群がある
;;     例：「カナダ漢字」
;;
;; 特殊文字（マップ順でソート済）：
;;   纊褜鍈銈蓜俉炻昱棈鋹曻彅丨仡仼伀伃伹佖侒侊侚侔俍偀倢俿倞偆偰偂傔僴僘兊兤冝
;;   冾凬刕劜劦勀勛匀匇匤卲厓厲叝﨎咜咊咩哿喆坙坥垬埈埇﨏塚增墲夋奓奛奝奣妤妺孖
;;   寀甯寘寬尞岦岺峵崧嵓﨑嵂嵭嶸嶹巐弡弴彧德忞恝悅悊惞惕愠惲愑愷愰憘戓抦揵摠撝
;;   擎敎昀昕昻昉昮昞昤晥晗晙晴晳暙暠暲暿曺朎朗杦枻桒柀栁桄棏﨓楨﨔榘槢樰橫橆橳
;;   橾櫢櫤毖氿汜沆汯泚洄涇浯涖涬淏淸淲淼渹湜渧渼溿澈澵濵瀅瀇瀨炅炫焏焄煜煆煇凞
;;   燁燾犱犾猤猪獷玽珉珖珣珒琇珵琦琪琩琮瑢璉璟甁畯皂皜皞皛皦益睆劯砡硎硤硺礰礼
;;   神祥禔福禛竑竧靖竫箞精絈絜綷綠緖繒罇羡羽茁荢荿菇菶葈蒴蕓蕙蕫﨟薰蘒﨡蠇裵訒
;;   訷詹誧誾諟諸諶譓譿賰賴贒赶﨣軏﨤逸遧郞都鄕鄧釚釗釞釭釮釤釥鈆鈐鈊鈺鉀鈼鉎鉙
;;   鉑鈹鉧銧鉷鉸鋧鋗鋙鋐﨧鋕鋠鋓錥錡鋻﨨錞鋿錝錂鍰鍗鎤鏆鏞鏸鐱鑅鑈閒隆﨩隝隯霳
;;   霻靃靍靏靑靕顗顥飯飼餧館馞驎髙髜魵魲鮏鮱鮻鰀鵰鵫鶴鸙黑ⅰⅱⅲⅳⅴⅵⅶⅷⅸⅹ
;;   ￢￤＇＂
;;
;; See also:
;; https://ja.wikipedia.org/wiki/Cp932
;; https://internet.watch.impress.co.jp/www/column/ogata/news4.htm
;; http://charset.7jp.net/sjis.html
;; ============================================================================
;; JIS X 0213:2004 (`japanese-jisx0213.2004-1' and `japanese-jisx0213-2')
;;
;; 字形変更：
;;   逢芦飴溢茨鰯淫迂厩噂餌襖迦牙廻恢晦蟹葛鞄釜翰翫徽
;;   祇汲灸笈卿饗僅喰櫛屑粂祁隙倦捲牽鍵諺巷梗膏鵠甑叉
;;   榊薩鯖錆鮫餐杓灼酋楯薯藷哨鞘杖蝕訊逗摺撰煎煽穿箭
;;   詮噌遡揃遜腿蛸辿樽歎註瀦捗槌鎚辻挺鄭擢溺兎堵屠賭
;;   瀞遁謎灘楢禰牌這秤駁箸叛挽誹樋稗逼謬豹廟瀕斧蔽瞥
;;   蔑篇娩鞭庖蓬鱒迄儲餅籾爺鑓愈猷漣煉簾榔屢冤叟咬嘲
;;   囀徘扁棘橙狡甕甦疼祟竈筵篝腱艘芒虔蜃蠅訝靄靱騙鴉
;;
;; 平仮名・片仮名・記号など：
;;   ゔヿヷヸヹヺㇰㇱㇲㇳㇴㇵㇶㇷㇸㇹㇺㇻㇼㇽ
;;
;; 第3水準（追加・1面）：
;;   旧：倶剥叱呑嘘妍屏并痩繋
;;   新：俱剝𠮟吞噓姸屛幷瘦繫
;;
;; 第4水準（一部・2面）：
;;   𠂉𪚲
;; ============================================================================
;; アラビア文字
;;
;; 表示例（一部）：
;; ء آ أ ؤ إ ئ ا ب ة ت ث ج ح خ د
;; ============================================================================
;; See also:
;; `my-utils.el': 独自サポート関数・マクロ定義
;; `mule-conf.el': 文字セット定義（`set-fontset-font' 第2引数の定義一覧）
;; `mule-diag.el': 文字セット・コーディングシステム用ツール定義
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lookup.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Selection.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Alternate-character-sets.html
;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Fonts-and-text-translation.html
;; https://www.emacswiki.org/emacs/DisplayingNonAsciiCharacters
;; https://www.emacswiki.org/emacs/FontSets
;; http://d.hatena.ne.jp/setoryohei/20110117
;; http://d.hatena.ne.jp/tomoya/20090519/1242687354
;; http://d.hatena.ne.jp/tomoya/20090807/1249641049
;; https://emacs.g.hatena.ne.jp/sakito/20100127
;; http://macemacsjp.osdn.jp/matsuan/FontSettingJp.html
;; http://www.shuiren.org/chuden/teach/code/main5.htm
;; http://lioon.net/emacs-change-font-size-quickly
;; ============================================================================
(leaf *font
  ;; ターミナルエミュレータ上では何もしない（設定する意味がないため）
  :when window-system
  :after my-utils
  :custom (;; シンボルや句読点などを表示するフォントを、設定に応じて選択
           ;;   → GNU Emacs 25 より前のふるまいに戻す
           (use-default-font-for-symbols . nil))
  :config
  ;; --------------------------------------------------------------------------
  ;; スケール変換
  ;; --------------------------------------------------------------------------
  ;; 多バイト文字の認識に支障がある場合の書法：
  ;; (add-to-list 'face-font-rescale-alist `(,(encode-coding-string "-フォント名-" 'emacs-mule) . 倍率))
  ;; --------------------------------------------------------------------------
  (cond
   (;; Windows (96dpi) ONLY
    (and (equal window-system 'w32)
         (< (car (my-real-display-pixels-per-inch)) 97.0) ; for 96dpi
         (my-fallback-font-family "ProFontWindows"))
    (add-to-list 'face-font-rescale-alist '("-Osaka-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-MS Gothic-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Courier-" . 0.900))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 0.900))
    (add-to-list 'face-font-rescale-alist '("-Microsoft YaHei-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Microsoft JhengHei-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Malgun Gothic-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Tahoma-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Emoji-" . 0.800))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Symbol-" . 1.100)))
   (;; macOS & Windows
    (and (my-fallback-font-family "Inconsolata")
         (my-fallback-font-family "Migu 1M"))
    (add-to-list 'face-font-rescale-alist '("-Menlo-" . 0.850))
    (add-to-list 'face-font-rescale-alist '("-Courier-" . 0.850))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-PingFang SC-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Microsoft YaHei-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-PingFang HK-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-MingLiU-ExtB-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-PingFang TC-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Microsoft JhengHei-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Apple SD Gothic Neo-" . 1.150))
    (add-to-list 'face-font-rescale-alist '("-Malgun Gothic-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Ayuthaya-" . 0.850))
    (add-to-list 'face-font-rescale-alist '("-Tahoma-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.785))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Emoji-" . 0.800))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Symbol-" . 1.100)))
   (;; macOS pre-install fonts ONLY
    (and (equal window-system 'mac)
         (my-fallback-font-family "Menlo"))
    (add-to-list 'face-font-rescale-alist '("-Hiragino Sans-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-Courier-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-PingFang SC-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-PingFang HK-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-PingFang TC-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-Ayuthaya-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.950)))
   (;; Windows pre-install fonts ONLY
    (and (equal window-system 'w32)
         (my-fallback-font-family "Consolas"))
    (add-to-list 'face-font-rescale-alist '("-Meiryo-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-MS Gothic-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Courier-" . 0.900))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 0.910))
    (add-to-list 'face-font-rescale-alist '("-Microsoft YaHei-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Microsoft JhengHei-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Malgun Gothic-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Tahoma-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Emoji-" . 0.800))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Symbol-" . 1.100))))


  ;; --------------------------------------------------------------------------
  ;; フォントセット：プログラミング用（高 dpi 環境向け）
  ;; --------------------------------------------------------------------------
  (my-create-fontset-from-spec "programming"
                               (font-spec :size 14.0 ; デフォルトフォントサイズ (pt)
                                          :family (my-fallback-font-family "Inconsolata"
                                                                           "Menlo"
                                                                           "Consolas"
                                                                           "Monospace")))
  ;; Emoji
  (my-set-fontset-font-safe "fontset-programming"
                            nil
                            (font-spec :family (my-fallback-font-family "Apple Color Emoji"
                                                                        "Segoe UI Emoji"
                                                                        "Segoe UI Symbol"
                                                                        "Symbola"
                                                                        "Monospace")))
  ;; 簡体字：GB 18030
  (my-set-fontset-font-safe "fontset-programming"
                            'gb18030
                            (font-spec :family (my-fallback-font-family "PingFang SC"
                                                                        "Microsoft YaHei"
                                                                        "Monospace")))
  ;; 繁体字（香港・マカオ）：HKSCS-2016
  (my-set-fontset-font-safe "fontset-programming"
                            'big5-hkscs
                            (font-spec :family (my-fallback-font-family "PingFang HK"
                                                                        "MingLiU-ExtB"
                                                                        "Monospace")))
  ;; 繁体字：Big5
  (my-set-fontset-font-safe "fontset-programming"
                            'big5
                            (font-spec :family (my-fallback-font-family "PingFang TC"
                                                                        "Microsoft JhengHei"
                                                                        "Monospace")))
  ;; ハングル：KS C 5601-1987 (a.k.a. KS X 1001:1998)
  (my-set-fontset-font-safe "fontset-programming"
                            'korean-ksc5601
                            (font-spec :family (my-fallback-font-family "Apple SD Gothic Neo"
                                                                        "Malgun Gothic"
                                                                        "Monospace")))
  ;; タイ文字：Thai Industrial Standard 620-2533 (TIS-620)
  (my-set-fontset-font-safe "fontset-programming"
                            'thai-tis620
                            (font-spec :family (my-fallback-font-family "Ayuthaya"
                                                                        "Tahoma"
                                                                        "Monospace")))
  ;; アラビア文字：Unicode 直接指定
  ;;               `cp858' との重複を避けるため、`cp1256' による指定はしない
  (dolist (range '((cons #x00600 #x006FF) ; U+0600-U+06FF (Arabic)
                   (cons #x00750 #x0077F) ; U+0750–U+077F (Arabic Supplement)
                   (cons #x008A0 #x008FF) ; U+08A0–U+08FF (Arabic Extended-A)
                   (cons #x0FB50 #x0FDFF) ; U+FB50–U+FDFF (Arabic Presentation Forms-A)
                   (cons #x0FE70 #X0FEFF) ; U+FE70–U+FEFF (Arabic Presentation Forms-B)
                   (cons #x10E60 #x10E7F) ; U+10E60–U+10E7F (Rumi Numeral Symbols)
                   (cons #x1EC70 #x1ECBF) ; U+1EC70–U+1ECBF (Indic Siyaq Numbers)
                   (cons #x1EE00 #x1EEFF))) ; U+1EE00-U+1EEFF (Arabic Mathematical Alphabetic Symbols)
    (my-set-fontset-font-safe "fontset-programming"
                              range
                              (font-spec :family (my-fallback-font-family "Baghdad"
                                                                          "Microsoft Sans Serif"
                                                                          "Monospace"))))
  ;; 日本語：JIS X 0213:2004
  (my-set-fontset-font-safe "fontset-programming"
                            'japanese-jisx0213.2004-1
                            (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                        "ヒラギノ角ゴシック"
                                                                        "メイリオ"
                                                                        "Monospace")))
  (my-set-fontset-font-safe "fontset-programming"
                            'japanese-jisx0213-2
                            (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                        "ヒラギノ角ゴシック"
                                                                        "メイリオ"
                                                                        "Monospace")))
  ;; ラテン文字：Code page 858 (`cp858')
  (my-set-fontset-font-safe "fontset-programming"
                            'cp858
                            (font-spec :family (my-fallback-font-family "Inconsolata"
                                                                        "Menlo"
                                                                        "Consolas"
                                                                        "Courier New"
                                                                        "Monospace")))
  (my-set-fontset-font-safe "fontset-programming"
                            (cons (string-to-char "░") (string-to-char "▓"))
                            ;; 次のフォントは U+2591, U+2592, U+2593 未実装：
                            ;;
                            ;;   * "Inconsolata"
                            ;;   * "Consolas"
                            ;;
                            ;; ゆえに、他フォントによるフォールバックが必要
                            (font-spec :family (my-fallback-font-family "Menlo"
                                                                        "Courier New"
                                                                        "Monospace")))
  ;; "ı" (U+0131: LATIN SMALL LETTER DOTLESS I) フォント明示
  (my-set-fontset-font-safe "fontset-programming"
                            (cons (string-to-char "ı") (string-to-char "ı"))
                            ;; 次のフォントは "ı" (U+0131) 未実装：
                            ;;
                            ;;   * "Inconsolata"
                            ;;
                            ;; ゆえに、他フォントによるフォールバックが必要
                            (font-spec :family (my-fallback-font-family "Menlo"
                                                                        "Consolas"
                                                                        "Courier New"
                                                                        "Monospace")))
  ;; "§" (U+00A7: SECTION SIGN)
  ;; "¨" (U+00A8: DIAERESIS)
  ;; "°" (U+00B0: DEGREE SIGN)
  ;; "±" (U+00B1: PLUS-MINUS SIGN)
  ;; "´" (U+00B4: ACUTE ACCENT)
  ;; "¶" (U+00B6: PILCROW SIGN)
  ;; "×" (U+00D7: LATIN CAPITAL LETTER O WITH STROKE)
  ;; "÷" (U+00F7: DIVISION SIGN)
  ;; "─" (U+2500: BOX DRAWINGS LIGHT HORIZONTAL)
  ;; "│" (U+2502: BOX DRAWINGS LIGHT VERTICAL)
  ;; "┌" (U+250c: BOX DRAWINGS LIGHT DOWN AND RIGHT)
  ;; "┐" (U+2510: BOX DRAWINGS LIGHT DOWN AND LEFT)
  ;; "└" (U+2514: BOX DRAWINGS LIGHT UP AND RIGHT)
  ;; "┘" (U+2518: BOX DRAWINGS LIGHT UP AND LEFT)
  ;; "├" (U+251c: BOX DRAWINGS LIGHT VERTICAL AND RIGHT)
  ;; "┤" (U+2524: BOX DRAWINGS LIGHT VERTICAL AND LEFT)
  ;; "┬" (U+252c: BOX DRAWINGS LIGHT DOWN AND HORIZONTAL)
  ;; "┴" (U+2534: BOX DRAWINGS LIGHT UP AND HORIZONTAL)
  ;; "┼" (U+253c: BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL)
  ;; "■" (U+25A0: BLACK SQUARE)
  ;;
  ;; `cp858' に含まれているため、半角フォントが利用されてしまう問題を回避
  (dolist (code (mapcar 'string-to-char
                        (split-string "§¨°±´¶×÷─│┌┐└┘├┤┬┴┼■" "" t)))
    (my-set-fontset-font-safe "fontset-programming"
                              (cons code code)
                              ;; 次のフォントは一部記号が全角にならない：
                              ;;
                              ;;   * "ヒラギノ角ゴシック"
                              ;;
                              ;; ゆえに、明示的な除外が必要
                              (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                          "メイリオ"
                                                                          "Monospace"))))
  ;; ASCII
  (my-set-fontset-font-safe "fontset-programming"
                            'ascii
                            (font-spec :size 14.0 ; デフォルトフォントサイズ (pt)
                                       :family (my-fallback-font-family "Inconsolata"
                                                                        "Menlo"
                                                                        "Consolas"
                                                                        "Courier New"
                                                                        "Monospace")))


  ;; --------------------------------------------------------------------------
  ;; フォントセット：プログラミング用（低 dpi 環境向け）
  ;;                 ビットマップフォント主体・96dpi 環境で設定済
  ;; --------------------------------------------------------------------------
  ;; See also:
  ;; https://github.com/chrissimpkins/codeface/tree/master/fonts/pro-font-windows
  ;; http://osaka.is.land.to/
  ;; http://emk.name/2003/12/osakattf.html
  ;; --------------------------------------------------------------------------
  (my-create-fontset-from-spec "programmingBMP"
                               (font-spec :size 12 ; デフォルトフォントサイズ (px)
                                          :family (my-fallback-font-family "ProFontWindows"
                                                                           "Courier New"
                                                                           "Monospace")))
  ;; Emoji
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            nil
                            (font-spec :family (my-fallback-font-family "Apple Color Emoji"
                                                                        "Segoe UI Emoji"
                                                                        "Segoe UI Symbol"
                                                                        "Symbola"
                                                                        "Monospace")))
  ;; 簡体字：GB 18030
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'gb18030
                            (font-spec :family (my-fallback-font-family "PingFang SC"
                                                                        "Microsoft YaHei"
                                                                        "Monospace")))
  ;; 繁体字（香港・マカオ）：HKSCS-2016
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'big5-hkscs
                            (font-spec :family (my-fallback-font-family "PingFang HK"
                                                                        "MingLiU-ExtB"
                                                                        "Monospace")))
  ;; 繁体字：Big5
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'big5
                            (font-spec :family (my-fallback-font-family "PingFang TC"
                                                                        "Microsoft JhengHei"
                                                                        "Monospace")))
  ;; ハングル：KS C 5601-1987 (a.k.a. KS X 1001:1998)
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'korean-ksc5601
                            (font-spec :family (my-fallback-font-family "Apple SD Gothic Neo"
                                                                        "Malgun Gothic"
                                                                        "Monospace")))
  ;; タイ文字：Thai Industrial Standard 620-2533 (TIS-620)
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'thai-tis620
                            (font-spec :family (my-fallback-font-family "Ayuthaya"
                                                                        "Tahoma"
                                                                        "Monospace")))
  ;; アラビア文字：Unicode 直接指定
  ;;               `cp858' との重複を避けるため、`cp1256' による指定はしない
  (dolist (range '((cons #x00600 #x006FF) ; U+0600-U+06FF (Arabic)
                   (cons #x00750 #x0077F) ; U+0750–U+077F (Arabic Supplement)
                   (cons #x008A0 #x008FF) ; U+08A0–U+08FF (Arabic Extended-A)
                   (cons #x0FB50 #x0FDFF) ; U+FB50–U+FDFF (Arabic Presentation Forms-A)
                   (cons #x0FE70 #X0FEFF) ; U+FE70–U+FEFF (Arabic Presentation Forms-B)
                   (cons #x10E60 #x10E7F) ; U+10E60–U+10E7F (Rumi Numeral Symbols)
                   (cons #x1EC70 #x1ECBF) ; U+1EC70–U+1ECBF (Indic Siyaq Numbers)
                   (cons #x1EE00 #x1EEFF))) ; U+1EE00-U+1EEFF (Arabic Mathematical Alphabetic Symbols)
    (my-set-fontset-font-safe "fontset-programmingBMP"
                              range
                              (font-spec :family (my-fallback-font-family "Baghdad"
                                                                          "Microsoft Sans Serif"
                                                                          "Monospace"))))
  ;; 日本語：JIS X 0213:2004
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'japanese-jisx0213.2004-1
                            (font-spec :family (my-fallback-font-family "ＭＳ ゴシック"
                                                                        "さざなみフォント"
                                                                        "東雲フォント"
                                                                        "Monospace")))
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'japanese-jisx0213-2
                            (font-spec :family (my-fallback-font-family "ＭＳ ゴシック"
                                                                        "さざなみフォント"
                                                                        "東雲フォント"
                                                                        "Monospace")))
  ;; 日本語：JIS X 0208
  ;;
  ;; "Osaka－等幅" で対応している文字はできるだけ利用
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'japanese-jisx0208
                            (font-spec :family (my-fallback-font-family "Osaka－等幅"
                                                                        "ＭＳ ゴシック"
                                                                        "さざなみフォント"
                                                                        "東雲フォント"
                                                                        "Monospace")))
  ;; ラテン文字：Code page 858 (`cp858')
  ;;
  ;; "ProFontWindows" の readme.txt には次の記述がある:
  ;;
  ;;   * "€" (U+20AC) サポートを投入
  ;;   * `cp585' フルサポート
  ;;
  ;; ゆえに、明示はないものの "ProFontWindows" の範囲は
  ;; `cp858' と同一であると仮定・設定する
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'cp858
                            (font-spec :family (my-fallback-font-family "ProFontWindows"
                                                                        "Courier New"
                                                                        "Monospace")))
  ;; "§" (U+00A7: SECTION SIGN)
  ;; "¨" (U+00A8: DIAERESIS)
  ;; "°" (U+00B0: DEGREE SIGN)
  ;; "±" (U+00B1: PLUS-MINUS SIGN)
  ;; "´" (U+00B4: ACUTE ACCENT)
  ;; "¶" (U+00B6: PILCROW SIGN)
  ;; "×" (U+00D7: LATIN CAPITAL LETTER O WITH STROKE)
  ;; "÷" (U+00F7: DIVISION SIGN)
  ;; "─" (U+2500: BOX DRAWINGS LIGHT HORIZONTAL)
  ;; "│" (U+2502: BOX DRAWINGS LIGHT VERTICAL)
  ;; "┌" (U+250c: BOX DRAWINGS LIGHT DOWN AND RIGHT)
  ;; "┐" (U+2510: BOX DRAWINGS LIGHT DOWN AND LEFT)
  ;; "└" (U+2514: BOX DRAWINGS LIGHT UP AND RIGHT)
  ;; "┘" (U+2518: BOX DRAWINGS LIGHT UP AND LEFT)
  ;; "├" (U+251c: BOX DRAWINGS LIGHT VERTICAL AND RIGHT)
  ;; "┤" (U+2524: BOX DRAWINGS LIGHT VERTICAL AND LEFT)
  ;; "┬" (U+252c: BOX DRAWINGS LIGHT DOWN AND HORIZONTAL)
  ;; "┴" (U+2534: BOX DRAWINGS LIGHT UP AND HORIZONTAL)
  ;; "┼" (U+253c: BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL)
  ;; "■" (U+25A0: BLACK SQUARE)
  ;;
  ;; `cp858' に含まれているため、半角フォントが利用されてしまう問題を回避
  (dolist (code (mapcar 'string-to-char
                        (split-string "§¨°±´¶×÷─│┌┐└┘├┤┬┴┼■" "" t)))
    (my-set-fontset-font-safe "fontset-programmingBMP"
                              (cons code code)
                              (font-spec :family (my-fallback-font-family "Osaka－等幅"
                                                                          "Osaka"
                                                                          "ＭＳ ゴシック"
                                                                          "さざなみフォント"
                                                                          "東雲フォント"
                                                                          "Monospace"))))
  ;; "Ø" (U+00D8: LATIN CAPITAL LETTER O WITH STROKE)
  ;;
  ;; 次のフォントは "Ø" と "0" (U+0030: DIGIT ZERO) が判別しにくい：
  ;;
  ;;     * "ProFontWindows"
  ;;
  ;; ゆえに、他フォントで表示させる
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            (cons (string-to-char "Ø") (string-to-char "Ø"))
                            (font-spec :family (my-fallback-font-family "ＭＳ ゴシック"
                                                                        "さざなみフォント"
                                                                        "東雲フォント"
                                                                        "Courier New"
                                                                        "Monospace")))
  ;; ASCII
  (my-set-fontset-font-safe "fontset-programmingBMP"
                            'ascii
                            (font-spec :size 12 ; デフォルトフォントサイズ (px)
                                       :family (my-fallback-font-family "ProFontWindows"
                                                                        "Courier New"
                                                                        "Monospace")))


  ;; --------------------------------------------------------------------------
  ;; フォントセット設定
  ;; --------------------------------------------------------------------------
  (let ((fontset (if (< (car (my-real-display-pixels-per-inch)) 97.0)
                     "fontset-programmingBMP" ; for 96dpi
                   "fontset-programming")))
    (modify-all-frames-parameters `((font . ,fontset)))

    ;; TODO: ダイアログの face も変えたい
    ;;       シンボル名不明
    ;;       `face-list' で一覧を出しても、それらしきものがなかった
    (custom-set-faces
     `(tooltip ((t
                 (:font ,fontset))))))
  ) ; END of *font


;; ============================================================================
;; `early-init.el' で設定した項目の変更
;; ============================================================================
(leaf *early-init-el-restore
  :custom `(;; ガベージコレクション閾値を現実的な値に戻す
            (gc-cons-threshold . ,(* 128 1024 1024)) ; 128MB
            ;; 黙らせていた余分なメッセージ I/O を復活
            (inhibit-message . nil)))


;; ============================================================================
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here
