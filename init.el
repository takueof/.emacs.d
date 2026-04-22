;;; init.el --- "GNU Emacs" main config file -*- mode: Emacs-Lisp; coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2013-2026 Taku WATABE
;; Time-stamp: <2026-04-23T05:26:23+09:00>

;; Author: Taku WATABE <taku.eof@gmail.com>

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

;; This config file is for "GNU Emacs" ONLY.
;; Unsupported other "emacsen" ("XEmacs" and others).

;; This file is VERY LONG.
;; So, I DARE USE file local variables in the FIRST LINE.

;; Show initialization time:
;; (emacs-init-time)

;;; Code:
(set-language-environment "Japanese")


;; ============================================================================
;; コーディングシステム
;; いくつかのデフォルトだけ決める（他は変えない）
;;
;; WARNING: `prefer-coding-system' は絶対に使わないこと！
;;          例：(prefer-coding-system 'utf-8-unix)
;;          システムごとに最適化された、自動設定のデフォルト定義を破壊するため
;; ============================================================================
;; macOS ONLY
(when (member system-type '(darwin))
  (set-keyboard-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq-default default-process-coding-system '(utf-8 . utf-8)))

;; Windows ONLY
(when (member system-type '(ms-dos windows-nt))
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos)))


;; ------------------------------------
;; 「UTF-8（BOM 有）」のエイリアスを作成
;; デフォルト名は長いため
;;
;; See: `mule-conf.el'
;; ------------------------------------
(define-coding-system-alias 'utf-8-bom 'utf-8-with-signature)


;; ------------------------------------
;; HACK: `japanese-cp932' を `shift_jis' として強制認識
;;       MIME を使用した自動判定を行うコード（`sgml-mode' など）でも
;;       例外が出ないようにする
;; ------------------------------------
(coding-system-put 'japanese-cp932
                   :mime-charset 'shift_jis)


;; ------------------------------------
;; HACK: `japanese-shift-jis' を `japanese-cp932' のエイリアスに変更する
;;       GNU Emacs における Shift_JIS 定義 `japanese-shift-jis' は、
;;       「JIS X 0208 附属書1」を厳格に実装したものである
;;       そのため一部文字（例：「～」(U+FF5E)）が未定義であるなどし、
;;       実用するうえで問題が出やすい
;;       そこで Microsoft の Shift_JIS 実装 `japanese-cp932' を、
;;       デフォルトの Shift_JIS 実装として認識させる
;;
;; See: `japanese.el'
;; ------------------------------------
(define-coding-system-alias 'japanese-shift-jis 'japanese-cp932)
(define-coding-system-alias 'shift_jis 'japanese-cp932)
(define-coding-system-alias 'sjis 'japanese-cp932)


;; ------------------------------------
;; 「〜」(U+301C) → 「～」(U+FF5E) 自動変換
;; ------------------------------------
(coding-system-put 'japanese-cp932 ; Shift_JIS
                   :encode-translation-table (get 'japanese-ucs-jis-to-cp932-map 'translation-table))


;; ------------------------------------
;; 「～」(U+FF5E) → 「〜」(U+301C) 自動変換
;; ------------------------------------
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
 '(frame-title-format (format "%%b - GNU Emacs v%s" emacs-version))
 ;;
 ;; スタートアップ表示は一切させない
 ;;
 ;; See:
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
 ;; ベルは視覚のみ、音なし
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
 ;; ファイル先頭＆末尾の状態表示をフリンジに表示
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
 ;; プレビューウインドウの表示を即時にする
 ;;
 '(register-preview-delay nil)
 ;;
 ;; スクロール時、自動スクロールをアグレッシブにしない
 ;;
 ;; See:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
 '(scroll-conservatively 0) ; default
 '(scroll-step 0) ; default
 '(scroll-up-aggressively nil) ; default
 '(scroll-down-aggressively nil) ; default
 ;;
 ;; なるべくウインドウ上下から2行目でスクロールを開始させる
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
 ;; 行間移動に論理行を使用する
 ;;
 '(line-move-visual t)
 ;;
 ;; 行表示は折り返さない
 ;;
 '(truncate-lines t)
 '(truncate-partial-width-windows t)
 '(default-truncate-lines t)
 ;;
 ;; 行文字数を、端末エミュレータのデファクトスタンダードにあわせる
 ;;
 '(fill-column 80)
 ;;
 ;; インデント利用文字は、常に半角空白 (U+0020) のみとする
 ;; 必要なら各メジャーモードごとに設定しなおす
 ;;
 '(indent-tabs-mode nil)
 ;;
 ;; タブは常にインデントのみ実施する
 ;;
 '(tab-always-indent t)
 ;;
 ;; 自分用デフォルトタブ文字表示幅を設定する
 ;; 必要なら各メジャーモードごとに設定しなおす
 ;;
 '(tab-width 4)
 ;;
 ;; 大文字／小文字を区別しない
 ;;
 '(case-fold-search t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 ;;
 ;; 新規ファイル／バッファ作成時の確認を省略する
 ;;
 '(confirm-nonexistent-file-or-buffer nil)
 ;;
 ;; 最終行への改行（空行）挿入を強制させる
 ;; 不要なら各メジャーモードごとに設定させる
 ;;
 '(require-final-newline t)
 '(mode-require-final-newline t)
 ;;
 ;; `undo' 上限を引き上げる
 ;;
 '(undo-limit 600000)
 '(undo-strong-limit 900000) ; (= 1.5 (/ undo-strong-limit undo-limit)) を踏襲
 ;;
 ;; 自動バックアップを無効にする
 ;;
 '(auto-save-default nil)
 '(make-backup-files nil)
 `(auto-save-list-file-prefix ,(convert-standard-filename "~/.emacs.auto-save-list/.saves-")) ; ローカル環境化
 ;;
 ;; ロックファイルを無効にする
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
 ;; ニーモニックを改行コードにちなんだ表現にする
 ;;
 '(eol-mnemonic-dos "[CRLF]")
 '(eol-mnemonic-mac "[CR]")
 '(eol-mnemonic-unix "[LF]")
 '(eol-mnemonic-undecided "")
 ;;
 ;; カーソルの行列表記を好みに変更する
 ;;
 '(mode-line-position-column-line-format '(" (%l:%c)"))
 ;;
 ;; Trash（「ごみ箱」など）が使える場合はそちらへ廃棄させる
 ;;
 '(delete-by-moving-to-trash t)
 ;;
 ;; YES/NO 選択を簡略化する
 ;;
 '(use-short-answers t)
 ;;
 ;; Option キーを `meta' とみなす (macOS GUI ONLY)
 ;;
 `(mac-option-modifier ,(if (display-graphic-p)
                            ''meta
                          ''(:function alt :mouse alt)))
 ;;
 ;; Command キーは何もしない (macOS GUI ONLY)
 ;;
 `(mac-command-modifier ,(if (display-graphic-p)
                             nil
                           ''meta))
 ;;
 ;; 右 <Alt> + 左 <Ctrl> で <AltGr> が発送されないようにする (Windows ONLY)
 ;; <AltGr> は独自のキーコードであり、<C-M-> であるとみなされない
 ;;
 ;; See:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Keyboard.html
 ;;
 '(w32-recognize-altgr nil)
 ;;
 ;; キャレットの視認性が悪くなる問題を回避する (Windows ONLY)
 ;;
 ;; See: https://mementomori.social/@tml/116416045226298692
 ;;
 '(w32-use-visible-system-caret nil)
 ;;
 ;; 証明書を明示的に設定する
 ;;
 `(gnutls-trustfiles ',(mapcar 'convert-standard-filename
                               (if (member system-type '(ms-dos windows-nt))
                                   '("C:/programs/cygwin/etc/pki/tls/certs/ca-bundle.trust.crt"
                                     "C:/programs/cygwin/etc/pki/tls/certs/ca-bundle.crt")
                                 '("/private/etc/ssl/cert.pem"
                                   "/etc/ssl/cert.pem")))))


;; ------------------------------------
;; リージョンの大文字／小文字変換で、実行の是非を問わせない
;; ------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ------------------------------------
;; モードラインを好みにする
;; ------------------------------------
(line-number-mode +1)
(column-number-mode +1)
(size-indication-mode +1)


;; ------------------------------------
;; ベル音を鳴らさない (Windows ONLY)
;; ------------------------------------
(if (fboundp #'set-message-beep)
    (set-message-beep 'silent))


;; ============================================================================
;; パッケージマネージャー `package'
;;
;; WARNING: `package' といったネットワークセキュリティを利用するパッケージの
;;          利用前に `nsm-settings-file' を設定しなければならない
;; ============================================================================
(when (require 'nsm nil :noerror) ; Network Security Manager
  ;; HACK: 未 `require' だと `custom-set-variables' が効かない問題を回避する
  (custom-set-variables
   ;; ローカル環境にのみ保存させる
   '(nsm-settings-file "~/.emacs.network-security.data")))


;; ------------------------------------
;; ロード
;; ------------------------------------
(when (require 'package nil :noerror)
  ;; 確実に `package-archives' が定義済でなければならない
  (add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/"))
  ;; あらゆるパッケージロードに先んじて初期化する
  (package-initialize))


;; ============================================================================
;; 詳細設定補助 `leaf'
;; ============================================================================
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))


;; ------------------------------------
;; キーワード定義
;; ------------------------------------
(leaf leaf-keywords
  :ensure t
  :config
  (leaf-keywords-init))


;; ============================================================================
;; 組み込みパッケージ
;; ============================================================================
;; ------------------------------------
;; サーバー化
;;
;; WARNING: サーバー化に依存するパッケージがあるため、なるはやで開始しておく
;; ------------------------------------
(leaf server
  :custom (;; ローカル環境にのみ保存させる
           (server-auth-dir . "~/.emacs.server"))
  :config
  (server-start t))


;; ------------------------------------
;; 認証 (macOS ONLY)
;;
;; WARNING: 認証を使うパッケージがあるため、なるはやで設定しておく
;; ------------------------------------
(leaf auth-source
  :when (member system-type '(darwin))
  :defer-config
  (add-to-list 'auth-sources 'macos-keychain-internet)
  (add-to-list 'auth-sources 'macos-keychain-generic))


;; ------------------------------------
;; `comint-mode' と派生モードで ANSI エスケープシーケンスの解釈を開始する
;; ------------------------------------
(leaf ansi-color
  :config
  (ansi-color-for-comint-mode-on))


;; ------------------------------------
;; 自動バッファ再読込
;; ------------------------------------
(leaf autorevert
  :custom (;; ファイル監視（通知）を使わない
           ;;
           ;; GNU Emacs の仕様では 1024 - 50 = 974 個以上の
           ;; ファイル監視を登録できない
           ;; 少しでもファイル監視を減らすため無効化する
           ;;
           ;; See:
           ;; https://www.reddit.com/r/emacs/comments/mq2znn/comment/gugo0n4/?context=3
           (auto-revert-use-notify . nil)
           (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)


;; ------------------------------------
;; ブックマーク
;; ------------------------------------
(leaf bookmark
  :custom ((bookmark-version-control . t)
           ;; ローカル環境にのみ保存させる
           (bookmark-default-file . "~/.emacs.bookmark.el")))


;; ------------------------------------
;; 共通コマンドインタプリタ (Windows ONLY)
;; ------------------------------------
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
    "Initialize `comint-mode' before load."
    (setq-local comint-input-sender-no-newline t))
  ;; プロセスごとのコーディングシステム変換表を追加する
  ;;
  ;; See:
  ;; https://www.emacswiki.org/emacs/ShellMode#toc1
  (add-to-list 'process-coding-system-alist
               '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix))))


;; ------------------------------------
;; コンパイル
;; ------------------------------------
(leaf compile
  :bind (("C-c x" . compile))
  :hook ((compilation-filter-hook . ansi-color-compilation-filter))
  :custom ((compilation-window-height . 15)
           ;; ビルドツール／タスクランナーに依存させない
           (compile-command . "")
           (compilation-scroll-output . t)
           (compilation-always-kill . t)
           (compilation-context-lines . t))
  :init
  ;; ----------------------------------
  ;; HACK: コンパイル完了後、モードラインにも状態を簡易表示
  ;; ----------------------------------
  (defun my-compilation-message (cur-buffer msg)
    "Show status messages when compile done in `compilation-mode'."
    (let ((msg-text (string-trim msg)) ; 改行文字が含まれうる問題を回避する
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
  ;; ----------------------------------
  ;; HACK: コンパイル完了後、正常に終了していれば自動でウインドウを閉じる
  ;; ----------------------------------
  (defcustom my-compilation-auto-quit-window-enable-buffer-names '("*compilation*")
    "Created buffer names by `compile' command."
    :group 'compilation
    :type '(list (repeat string)))
  ;; NOTE: `compilation-finish-functions' にフックするだけでは、
  ;;       `msg' しか参照できない
  (defun my-compilation-auto-quit-window (process-status exit-status msg)
    "Run `quit-window' when `compile' successed."
    (if (and (member (buffer-name)
                     my-compilation-auto-quit-window-enable-buffer-names)
             (or (and (equal process-status 'exit)
                      (zerop exit-status))
                 ;; 改行文字が含まれうる問題を回避する
                 (string-equal "finished" (string-trim msg))))
        (quit-window nil (get-buffer-window))))
  :advice (;; HACK: アドバイス経由で `process-status' と `exit-status' を得る
           (:after compilation-handle-exit my-compilation-auto-quit-window)))


;; ------------------------------------
;; CUA 標準キー（と矩形選択サポート）
;; ------------------------------------
(leaf cua-mode
  :custom ((cua-enable-cua-keys . nil)
           (cua-remap-control-v . nil)
           (cua-remap-control-z . nil)
           (cua-prefix-override-inhibit-delay . nil)
           (cua-enable-register-prefix . nil)
           (cua-check-pending-input . nil))
  :global-minor-mode cua-selection-mode)


;; ------------------------------------
;; バッファ内マッチ補完
;; ------------------------------------
(leaf dabbrev
  :custom (;; 補完時に大小文字を区別しない
           (dabbrev-case-fold-search . t)))


;; ------------------------------------
;; 行番号表示
;; ------------------------------------
(leaf display-line-numbers
  :bind (("C-c l" . display-line-numbers-mode)))


;; ------------------------------------
;; ディレクトリブラウジング
;; ------------------------------------
(leaf dired
  :hook ((dired-mode-hook . my-dired-mode-initialize))
  :bind ((:dired-mode-map
          ("r" . #'wdired-change-to-wdired-mode)))
  :custom ((dired-kill-when-opening-new-dired-buffer . t)
           (wdired-allow-to-change-permissions . 'advanced))
  :init
  (defun my-dired-mode-initialize ()
    "Initialize `dired-mode' before load."
    ;; 常にすべての情報を表示する（簡易モードにしない）
    (dired-hide-details-mode -1)
    ;; `windmove' を機能させる
    (keymap-local-unset "C-S-f" t)
    (keymap-local-unset "C-S-b" t)
    (keymap-local-unset "C-S-n" t)
    (keymap-local-unset "C-S-p" t)))


;; ------------------------------------
;; EditorConfig
;;
;; See:
;; https://editorconfig.org/
;; ------------------------------------
(leaf editorconfig
  :global-minor-mode t)


;; ------------------------------------
;; GNU Emacs Lisp ドキュメント表示
;; ------------------------------------
(leaf eldoc
  :hook ((emacs-lisp-mode-hook . eldoc-mode)
         (ielm-mode-hook . eldoc-mode)
         (lisp-interaction-mode-hook . eldoc-mode)
         (lisp-mode-hook . eldoc-mode))
  :custom ((eldoc-minor-mode-string . nil)
           (eldoc-idle-delay . 0.25)
           (eldoc-echo-area-use-multiline-p . 'truncate-sym-name-if-fit)))


;; ------------------------------------
;; 区切り自動挿入
;; ------------------------------------
(leaf elec-pair
  :custom ((electric-pair-preserve-balance . t)
           (electric-pair-delete-adjacent-pairs . t)
           (electric-pair-open-newline-between-pairs . t)
           (electric-pair-skip-whitespace . t))
  :global-minor-mode electric-pair-mode)


;; ------------------------------------
;; Emoji😊 挿入
;; ------------------------------------
(leaf emoji
  :bind (("C-c e i" . emoji-insert)
         ("C-c e r" . emoji-recent)
         ("C-c e s" . emoji-search)
         ("C-c e l" . emoji-list)
         ("C-c e d" . emoji-describe)))


;; ------------------------------------
;; EWW (Emacs Web Wowser, Web Browser)
;; ------------------------------------
(leaf eww
  :bind (("C-c C-e" . eww))
  :custom ((eww-search-prefix . "https://www.google.com/search?&q=")
           (eww-history-limit . nil)
           (eww-auto-rename-buffer . 'title)))


;; ------------------------------------
;; テキスト＆コード静的解析 (OLD)
;; ------------------------------------
(leaf flymake
  :custom ((flymake-run-in-place . nil)))


;; ------------------------------------
;; 自動スペルチェッカ
;; ------------------------------------
(leaf flyspell
  :bind (;; HACK: `flyspell' がキーバインドを横取りする問題を回避する
         (:flyspell-mode-map
          ("M-TAB" . nil)
          ("C-;" . nil)
          ("C-," . nil)
          ("C-." . nil)
          ("C-c $" . nil)))
  :hook ((text-mode-hook . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode))
  :custom ((flyspell-delay . 1.0)))


;; ------------------------------------
;; 正規表現検索
;; ------------------------------------
(leaf grep
  :bind (("C-M-g" . rgrep)))


;; ------------------------------------
;; カレントカーソル行強調
;; ------------------------------------
(leaf hl-line
  :custom ((global-hl-line-sticky-flag . t))
  :global-minor-mode global-hl-line-mode)


;; ------------------------------------
;; 強化バッファ一覧
;; ------------------------------------
(leaf ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom ((ibuffer-default-sorting-mode . 'filename/process)
           (ibuffer-expert . t))
  :config
  ;; バッファ名の表示を30文字に変更する
  ;;   -> カラム幅が揃わなくなるため `-1' は不可
  (let* (;; `customize-mark-to-save' の評価を t にするため明示的にコピーする
         (formats (copy-tree ibuffer-formats))
         (settings (assoc 'name (assoc 'mark formats))))
    ;; 該当する設定項目がなければ何もしない
    ;; 将来的に項目が変更された場合でも、例外を出さないため
    (when settings
      (setcdr settings '(30 30 :left :elide))
      ;; WARNING: この `custom-set-variables' は `:custom' に移動できない
      ;;          変数 `settings' で加工を行った結果が入るため
      (custom-set-variables
       `(ibuffer-formats ',formats)))))


;; ------------------------------------
;; ファイル操作の簡略化
;; ------------------------------------
(leaf ido
  ;; HACK: デフォルト OFF だが、他機能から切り替え可能にしておく
  :custom ((ido-enable-flex-matching . t)
           (ido-create-new-buffer . 'always)
           (ido-use-virtual-buffers . t)
           (ido-max-file-prompt-width . 0)
           (ido-use-filename-at-point . 'guess)
           (ido-unc-hosts . t)
           ;; ローカル環境にのみ保存させる
           (ido-save-directory-list-file . "~/.emacs.ido-save-directory-list.el")))


;; ------------------------------------
;; 画像の直接表示
;; ------------------------------------
(leaf image-file
  :global-minor-mode auto-image-file-mode)


;; ------------------------------------
;; インクリメンタル検索
;; ------------------------------------
(leaf isearch
  :custom ((isearch-case-fold-search . t)
           (isearch-last-case-fold-search . t)))


;; ------------------------------------
;; スペルチェッカー
;; ------------------------------------
(leaf ispell
  :when (or (executable-find "aspell")
            (executable-find "ispell"))
  :custom ((ispell-dictionary . "english")
           (ispell-extra-args . '("--sug-mode=fast"
                                  "--run-together"
                                  "--run-together-limit=5"
                                  "--run-together-min=2"))))


;; ------------------------------------
;; アーカイブファイルを直接編集
;; ------------------------------------
(leaf jka-cmpr-hook
  :global-minor-mode auto-compression-mode)


;; ------------------------------------
;; 自動カラー表示
;; ------------------------------------
(leaf rainbow-mode
  :defer-config
  (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode))


;; ------------------------------------
;; ファイル履歴保存
;; ------------------------------------
(leaf recentf
  :custom (;; 履歴保存数は絞る
           (recentf-max-saved-items . 20)
           ;; ローカル環境にのみ保存させる
           (recentf-save-file . "~/.emacs.recentf.el")))


;; ------------------------------------
;; ミニバッファの履歴を残す
;; ------------------------------------
(leaf savehist
  :custom (;; 履歴保存数は絞る
           (history-length . 100)
           ;; ローカル環境にのみ保存させる
           (savehist-file . "~/.emacs.savehist.el"))
  :global-minor-mode t)


;; ------------------------------------
;; ファイルごとにカーソル位置を保存
;; ------------------------------------
(leaf saveplace
  :custom (;; ローカル環境にのみ保存させる
           (save-place-file . "~/.emacs.saveplace.el"))
  :global-minor-mode save-place-mode)


;; ------------------------------------
;; 基礎編集コマンド集
;; ------------------------------------
(leaf simple
  ;; 暫定マークを使用する
  :global-minor-mode transient-mark-mode)


;; ------------------------------------
;; タイムスタンプ
;; ------------------------------------
(leaf time-stamp
  :hook ((before-save-hook . time-stamp))
  :custom `(;; ISO 8601 (JIS X 0301) 形式にする
            ;;
            ;; See:
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
            ;; Windows 環境（環境変数 %TZ% 未指定かつ +09:00 ゾーン）では
            ;; 次の値が使用されてしまう
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
                                           ;; 強制的にコロンを付与する
                                           ;; コロンなし形式を返されるため
                                           ;; 厳密チェックで "±1259" のみ利用する
                                           ;;   → 他は無視する
                                           "\\`\\([\\+\\-]\\(?:0[0-9]\\|1[0-2]\\)\\)\\([0-5][0-9]\\)\\'"
                                           "\\1:\\2"
                                           ;; タイムゾーンが "+0000" を返す
                                           ;; あえて "Z" への変換はしない
                                           (format-time-string "%z"))))))


;; ------------------------------------
;; 自動生成されるバッファ名をユニーク性が高いものにする
;; ------------------------------------
(leaf uniquify
  :require t
  :custom ((uniquify-buffer-name-style . 'forward)
           (uniquify-ignore-buffers-re . "^*[^*]+*\\-")))


;; ------------------------------------
;; 空白文字強調
;; ------------------------------------
(leaf whitespace
  :hook ((after-change-major-mode-hook . my-whitespace-mode-initialize))
  :custom (;; 「不正」位置の空白文字のみ強調させる
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
           ;; -------------------------
           ;; HACK: 全角空白 (U+3000) を HARD SPACE とみなして強調表示する
           ;;
           ;; 表示テスト:
           ;;   U+0009: 「	」
           ;;   U+00A0: 「 」
           ;;   U+3000: 「　」
           ;; -------------------------
           (whitespace-hspace-regexp . "\\(\u00A0\\|\u08A0\\|\u0920\\|\u0E20\\|\u0F20\\|\u3000\\)+")
           (whitespace-trailing-regexp . "\\([\t \u00A0\u3000]+\\)$")
           ;; 行カラム最大値は `fill-column' を参照させる
           (whitespace-line-column . nil)
           ;; -------------------------
           ;; HACK: 半角空白 (U+0020) を強調しないようにする
           ;;
           ;; 表示テスト:
           ;;   U+0020: 「 」
           ;; -------------------------
           (whitespace-display-mappings . '(;; EOL -> DOLLAR SIGN
                                            (newline-mark ?\n [?$ ?\n])
                                            ;; TAB -> CURRENCY SIGN
                                            (space-mark ?\u00A0 [?¤] [?_])
                                            ;; IDEOGRAPHIC SPACE -> WHITE SQUARE
                                            (space-mark ?\u3000 [?\u25a1] [?_ ?_])
                                            ;; Tab -> RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
                                            (tab-mark ?\t [?» ?\t] [?\\ ?\t]))))
  :init
  (defun my-whitespace-mode-initialize ()
    "Initialize `whitespace' before load."
    ;; --------------------------------
    ;; HACK: 一部メジャーモードでは無効にする
    ;; --------------------------------
    (with-eval-after-load 'whitespace
      (if (member major-mode '(;; 降順ソート
                               lisp-interaction-mode
                               ))
          (whitespace-mode -1))))
  :global-minor-mode global-whitespace-mode)


;; ------------------------------------
;; ウインドウ移動キーを直感的にする
;; ------------------------------------
(leaf windmove
  :bind (("C-S-b" . windmove-left)
         ("C-S-f" . windmove-right)
         ("C-S-n" . windmove-down)
         ("C-S-p" . windmove-up))
  :custom (;; フレーム端のウインドウでは無限スクロールするようにふるまう
           ;; 「マリオブラザーズ」左右画面端におけるループのような動き
           (windmove-wrap-around . t)))


;; ------------------------------------
;; ウインドウの状態履歴を undo/redo
;; ------------------------------------
(leaf winner
  :global-minor-mode t)


;; ============================================================================
;; 自作ユーティリティをロード
;; ============================================================================
(leaf my-utils
  :load-path* "utils"
  :require t)


;; ============================================================================
;; グローバルキーバインド
;; ============================================================================
(leaf *global-keybind
  :leaf-defer nil
  :after my-utils
  :bind (;; ヘルプ表示を割り当てなおす
         ("C-x ?" . help-command)
         ;; ウインドウ中央表示はもっともシンプルなものを使用する
         ;; `recenter-top-bottom' は使わない
         ("C-l" . recenter)
         ;; リージョン範囲をソート
         ("C-c s" . sort-lines)
         ;; 1つ前のエラーを表示する
         ("C-x \\" . previous-error)
         ;; メジャーモードを再適用後に `revert-buffer-quick' 実行させる
         ("C-c r" . my-revert-buffer-quick-with-normal-mode)
         ;; 行頭移動は物理行とする
         ("C-a" . my-beginning-of-smart-indented-line)
         ;; 前のウインドウに移動する
         ("C-x p" . my-other-window-reverse)
         ;; 前のフレームに移動する
         ("C-x 5 p" . my-other-frame-reverse)
         ;; 折り返し表示を強制的に切り替える
         ("C-x w" . my-toggle-truncate-lines-force)
         ;; カーソル位置に YEN SIGN (U+00A5) を挿入する
         ("C-c i \\" . my-insert-yen-sign)
         ;; カーソル位置にファイル名を挿入する
         ("C-c i f" . my-insert-file-name)
         ;; カーソル位置にファイルパスを挿入する
         ("C-c i p" . my-insert-file-path)
         ;; 一括でエンコーディング変換させる
         ("C-c RET f" . my-change-files-coding-system)
         ;; フレーム背景の透明度を切り替える
         ("C-c w t" . my-toggle-frame-transparency))
  :config
  ;; C-h を backspace とみなす
  (keyboard-translate ?\C-h ?\C-?)
  ;; 誤字の元になる `transpose-chars' キーバインドを明示的に解除しておく
  (global-unset-key (kbd "C-t"))
  ;; 誤字の元になる `transpose-lines' キーバインドを明示的に解除しておく
  (global-unset-key (kbd "C-M-t"))
  ;; `ido-undo-merge-work-directory' 実行のため <C-z> を押しすぎた場合、
  ;; `suspend-frame' が起動しないよう配慮する
  (global-unset-key (kbd "C-z")))


;; ============================================================================
;; カラーテーマ
;; ============================================================================
(leaf modus-themes
  :ensure t
  :require t ; NOTE: 最新 MELPA 版を利用
  :custom ((modus-themes-bold-constructs . t)
           (modus-themes-common-palette-overrides . '((comment yellow-faint)
                                                      (string green-faint)
                                                      (border-mode-line-active unspecified)
                                                      (border-mode-line-inactive unspecified)
                                                      (bg-mode-line-active bg-green-subtle))))
  :config
  (modus-themes-load-theme 'modus-vivendi))


;; ============================================================================
;; 動的 IME パッチ (Windows ONLY)
;;
;; WARNING: 全 IM に影響するため、なるはやでインストールしておく
;; ============================================================================
(leaf tr-ime
  :when (member system-type '(ms-dos windows-nt))
  :ensure t
  :custom '(;; 識別名 "W32-IME" は `tr-ime' 未適用だと使えない
            (default-input-method . "W32-IME")
            (w32-ime-buffer-switch-p . t)
            (w32-ime-mode-line-state-indicator . "[Aa]")
            (w32-ime-mode-line-state-indicator-list . '("[--]" "[あ]" "[Aa]")))
  :config
  (tr-ime-advanced-install t)
  (w32-ime-initialize))


;; ============================================================================
;; Input Method (IM)
;; ============================================================================
(leaf *input-method
  :when window-system ; CUI などは例外になるので、何もさせない
  :after my-utils
  :hook (;; ウインドウ選択後、IM の状態に応じてフェイス `cursor' を変更する
         ;;
         ;; `cursor' はフレーム単位だが、`current-input-method' はバッファ単位
         ;; そのためバッファ間で `current-input-method' 値が異なると、
         ;; `cursor' が意図せぬ状態になる
         ;;
         ;; ゆえに、ウインドウ切替のタイミングで `cursor' を変更する必要がある
         ;; バッファ切替のタイミングでは何もしない
         ;;
         ;; `select-window' 実行後に起動するフックを利用する
         (buffer-list-update-hook . my-change-cursor-faces-by-current-input-method)
         ;; IM と連動させる
         (input-method-activate-hook . my-change-cursor-faces-by-current-input-method)
         (input-method-deactivate-hook . my-change-cursor-faces-by-current-input-method)
         ;; macOS ONLY
         (mac-selected-keyboard-input-source-change-hook . my-change-cursor-faces-by-current-input-method)
         (mac-enabled-keyboard-input-sources-change-hook . my-change-cursor-faces-by-current-input-method)))


;; ============================================================================
;; マイナーモード：外部パッケージ
;; ============================================================================
;; ------------------------------------
;; GNU/Linux, UNIX, macOS 環境変数 $PATH 自動取得＆設定
;;
;; WARNING: 環境変数を使うパッケージがあるため、なるはやでインストールしておく
;; ------------------------------------
(leaf exec-path-from-shell
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; ------------------------------------
;; Node.js モジュールパス解決
;; ------------------------------------
(leaf add-node-modules-path
  :ensure t
  :hook ((prog-mode-hook . add-node-modules-path)
         (text-mode-hook . add-node-modules-path)))


;; ------------------------------------
;; 非同期あいまい検索
;; ------------------------------------
(leaf affe
  :ensure t
  :bind (("C-M-S-f" . affe-find)
         ("C-M-S-g" . affe-grep)))


;; ------------------------------------
;; LLM エージェントと ACP (Agent Client Protocol) で対話
;; ------------------------------------
(leaf agent-shell
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :bind ((:agent-shell-mode-map
          ("RET" . newline)
          ("M-RET" . shell-maker-submit)))
  :custom `((agent-shell-show-welcome-message . nil)
            (agent-shell-mcp-servers . '(((name . "mcp-platform")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_OLD_REGISTRY")
                                                   ,(getenv "AI_MCP_OLD_PLATFORM"))))
                                         ((name . "mcp-service")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_OLD_REGISTRY")
                                                   ,(getenv "AI_MCP_OLD_SERVICE"))))
                                         ((name . "mcp-techportal")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_OLD_REGISTRY")
                                                   ,(getenv "AI_MCP_OLD_TECHPORTAL"))))
                                         ((name . "mcp-bts")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_REGISTRY")
                                                   ,(getenv "AI_MCP_PACKAGE")
                                                   ,(getenv "AI_MCP_BTS"))))
                                         ((name . "mcp-calendar")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_REGISTRY")
                                                   ,(getenv "AI_MCP_PACKAGE")
                                                   ,(getenv "AI_MCP_CALENDAR"))))
                                         ((name . "mcp-drive")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_REGISTRY")
                                                   ,(getenv "AI_MCP_PACKAGE")
                                                   ,(getenv "AI_MCP_DRIVE"))))
                                         ((name . "mcp-mail")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_REGISTRY")
                                                   ,(getenv "AI_MCP_PACKAGE")
                                                   ,(getenv "AI_MCP_MAIL"))))
                                         ((name . "mcp-slack")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_REGISTRY")
                                                   ,(getenv "AI_MCP_PACKAGE")
                                                   ,(getenv "AI_MCP_SLACK"))))
                                         ((name . "mcp-wiki")
                                          (type . "stdio")
                                          (command . "npx")
                                          (args . ("-y"
                                                   "--registry"
                                                   ,(getenv "AI_MCP_REGISTRY")
                                                   ,(getenv "AI_MCP_PACKAGE")
                                                   ,(getenv "AI_MCP_WIKI")))))))
  :init
  ;; FIXME: まだエラーになる！
  ;;        もしかしたら、社内専用 AWS 互換サーバーエンドポイントと繋がっていないとダメかも？
  ;;        VPN OFF すなわち HTTP 404 の場合はどうするか考えないとダメ？
  (defun my-agent-shell-initialize (f &rest args)
    "Initialize `agent-shell' between from package load to call `agent-shell' function.
F is inner function in `agent-shell', ARGS are F arguments."
    ;; 確実に `agent-shell-make-environment-variables' を定義させる
    (require 'agent-shell nil :noerror)
    ;; `agent-shell-make-environment-variables' 定義後に環境変数を渡す
    (customize-set-variable 'agent-shell-anthropic-claude-environment
                            (agent-shell-make-environment-variables :inherit-env t))
    (apply f args))
  :advice (;; HACK: 適切な hook がないため `:advice' でしのぐ
           (:around agent-shell my-agent-shell-initialize)))


;; ------------------------------------
;; 各種検索／置換強化
;; ------------------------------------
(leaf anzu
  :ensure t
  :bind (("C-r" . anzu-query-replace-regexp))
  :custom ((anzu-minimum-input-length . 3)
           (anzu-search-threshold . 1000)
           (anzu-replace-to-string-separator . " -> "))
  :global-minor-mode global-anzu-mode)
;;
;; `migemo' が準備できたら使いはじめる
;;
(leaf anzu-migemo
  :after migemo
  :custom ((anzu-use-migemo . t)))


;; ------------------------------------
;; コードフォーマッター
;; ------------------------------------
(leaf apheleia
  :ensure t
  :hook ((apheleia-mode-on-hook . my-apheleia-initialize))
  :custom ((apheleia-mode-lighter . ""))
  :init
  (defun my-apheleia-initialize ()
    "Initialize `apheleia' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/ruff を自動検出
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (ruff-path (expand-file-name ".venv/bin/ruff" venv-dir)))
      (when (file-executable-p ruff-path)
        (with-eval-after-load 'apheleia-formatters
          (let ((local-apheleia-formatters (copy-tree apheleia-formatters)))
            (setcar (member "ruff" (assoc 'ruff local-apheleia-formatters)) ruff-path)
            (setcar (member "ruff" (assoc 'ruff-isort local-apheleia-formatters)) ruff-path)
            (setq-local apheleia-formatters local-apheleia-formatters)
            (message "Using project ruff: %s" ruff-path))))))
  :defer-config
  ;;
  ;; JavaScript
  ;;
  ;; パーサーを "babel-flow" から "typescript" に変更する
  ;;
  ;; See:
  ;; https://prettier.io/docs/options#parser
  (ignore-errors ; "--parser=babel-flow" がなければ何もしない
    (setcar (member "--parser=babel-flow" (assoc 'prettier-javascript apheleia-formatters)) "--parser=typescript"))
  ;;
  ;; Python
  ;;
  ;; フォーマッターを `black' から `ruff-isort' → `ruff' 実行に変更する
  ;;
  ;; See:
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
  (setcdr (assoc 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setcdr (assoc 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  :global-minor-mode apheleia-global-mode)


;; ------------------------------------
;; Markdown リーダー (macOS ONLY)
;; ------------------------------------
(leaf arto
  :when (executable-find "arto")
  ;; NOTE: 起動には `markdown-open' のキーバインドを使うため、何もしない
  :vc (:url "https://github.com/arto-app/arto.el" :rev :newest))


;; ------------------------------------
;; 他ウインドウ弱調化
;; ------------------------------------
(leaf auto-dim-other-buffers
  :ensure t
  :hook ((window-setup-hook . auto-dim-other-buffers-mode)))


;; ------------------------------------
;; 補完フレームワーク
;; ------------------------------------
(leaf company
  :ensure t
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
                                           scss-mode))
           (company-dabbrev-code-other-buffers . t)
           (company-dabbrev-code-everywhere . t)
           (company-dabbrev-code-ignore-case . t))
  :global-minor-mode global-company-mode)


;; ------------------------------------
;; 補完フレームワーク：拡張（ポップアップ）
;; ------------------------------------
(leaf company-box
  :ensure t
  :hook ((company-mode-hook . company-box-mode)))


;; ------------------------------------
;; 補完フレームワーク：拡張（補完候補のソート）
;; ------------------------------------
(leaf company-statistics
  :ensure t
  :custom ((company-statistics-size . 500)
           ;; ローカル環境にのみ保存させる
           (company-statistics-file . "~/.emacs.company-statistics-cache.el"))
  :global-minor-mode t)


;; ------------------------------------
;; 補完
;; ------------------------------------
(leaf consult
  :ensure t
  :bind (;; リマップ
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap switch-to-buffer] . consult-buffer)
         ;; 上書き
         ("C-s" . consult-line)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; ミニバッファ
         (minibuffer-local-map
          :package emacs
          ("C-r" . consult-history)))
  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))
  :custom ((register-preview-function . #'consult-register-format)
           (xref-show-xrefs-function . #'consult-xref)
           (xref-show-definitions-function . #'consult-xref))
  :advice ((:override register-preview consult-register-window)))


;; ------------------------------------
;; GitHub Copilot (AI)
;; ------------------------------------
(leaf copilot
  :ensure t
  :bind (("C-c c c" . copilot-mode)
         ("C-TAB" . copilot-complete)
         ("C-<tab>" . copilot-complete)
         (:copilot-completion-map
          ("TAB" . copilot-accept-completion)
          ("<tab>" . copilot-accept-completion)))
  :hook ((post-command-hook . copilot-clear-overlay))
  :custom ((copilot-max-char . -1) ; No limit
           (copilot-indent-offset-warning-disable . t))
  :config
  (add-to-list 'copilot-major-mode-alist '("web" . "html")))


;; ------------------------------------
;; モードラインからモードの表示を消す
;; ------------------------------------
(leaf delight
  :ensure t
  :config
  (delight '(;; 降順ソート
             (anzu-mode nil "anzu")
             (auto-dim-other-buffers-mode nil "auto-dim-other-buffers")
             (company-mode nil "company")
             (company-box-mode nil "company-box")
             (easysession-save-mode nil "easysession")
             (editorconfig-mode nil "editorconfig")
             (eldoc-mode nil "eldoc")
             (flycheck-mode nil "flycheck")
             (flymake-mode nil "flymake")
             (flyspell-mode nil "flyspell")
             (flyspell-prog-mode nil "flyspell")
             (global-anzu-mode nil "anzu")
             (global-company-mode nil "company")
             (global-flycheck-mode nil "flycheck")
             (global-whitespace-mode nil "whitespace")
             (inhibit-mouse-mode nil "inhibit-mouse")
             (js2-minor-mode nil "js2-mode")
             (lsp-mode nil "lsp-mode")
             (projectile-mode nil "projectile")
             (text-scale-mode nil "face-remap")
             (whitespace-mode nil "whitespace")
             (yas-global-mode nil "yasnippet")
             (yas-minor-mode nil "yasnippet"))))


;; ------------------------------------
;; 未コミット diff 表示
;; ------------------------------------
(leaf diff-hl
  :ensure t
  :bind (("C-c v g" . diff-hl-diff-goto-hunk)
         ("C-c v n" . diff-hl-next-hunk)
         ("C-c v p" . diff-hl-previous-hunk)
         ("C-c v r" . diff-hl-revert-hunk)
         ("C-c v v" . diff-hl-show-hunk))
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom `((diff-hl-update-async . 'thread)
            (diff-hl-command-prefix . ,(kbd "C-c v")))
  :config
  (diff-hl-dired-mode +1)
  (diff-hl-margin-mode +1)
  :global-minor-mode (global-diff-hl-mode diff-hl-flydiff-mode))


;; ------------------------------------
;; ディレクトリブラウジング：拡張
;; ------------------------------------
(leaf dired+
  :after dired
  :vc (:url "https://github.com/emacsmirror/dired-plus" :rev :newest)
  :require t
  :custom ((diredp-hide-details-initially-flag . nil)
           (diredp-hide-details-propagate-flag . nil))
  :config
  (diredp-toggle-find-file-reuse-dir +1))


;; ------------------------------------
;; コードナビゲーション
;; ------------------------------------
(leaf dumber-jump
  :when (executable-find "rg")
  :ensure t
  :hook (xref-backend-functions . dumber-jump-xref-activate))


;; ------------------------------------
;; セッション管理
;; ------------------------------------
(leaf easysession
  :ensure t
  :leaf-defer nil
  :bind (("C-c d l" . easysession-switch-to)
         ("C-c d L" . easysession-switch-to-and-restore-geometry)
         ("C-c d s" . easysession-save)
         ("C-c d r" . easysession-rename)
         ("C-c d R" . easysession-reset)
         ("C-c d u" . easysession-unload)
         ("C-c d d" . easysession-delete))
  :config
  (easysession-setup))


;; ------------------------------------
;; 補完候補へのアクション提供
;; ------------------------------------
(leaf embark-consult
  :ensure t
  :bind ((:minibuffer-mode-map
          :package emacs
          ("M-." . embark-dwim)
          ("C-." . embark-act)))
  :hook ((embark-collect-mode . consult-preview-at-point-mode))
  :custom ((prefix-help-command . #'embark-prefix-help-command)))


;; ------------------------------------
;; テキスト＆コード静的解析
;; ------------------------------------
(leaf flycheck
  :ensure t
  :bind (("C-c f" . flycheck-mode))
  :hook ((after-init-hook . global-flycheck-mode))
  :custom ((flycheck-checker-error-threshold . nil)
           (flycheck-display-errors-delay . 0.5)
           (flycheck-idle-change-delay . 0.25)
           (flycheck-disabled-checkers . '(javascript-jscs)))
  :config
  ;; ----------------------------------
  ;; HACK: `flycheck-checker-error-threshold' 以上の項目が出現すると
  ;;       生成されうる警告バッファの出現を抑制する
  ;; ----------------------------------
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-log-types '(flycheck syntax-checker)))
  ;; ----------------------------------
  ;; PATCH: Sass（.scss/.sass 両形式）チェック時にキャッシュを使わせない
  ;; ----------------------------------
  (dolist (checker '(scss sass))
    (if (and (flycheck-registered-checker-p checker)
             (not (member "-C" (flycheck-checker-arguments checker))))
        ;; あえて破壊的に変更（元のリストに追加したい）
        (nconc (get checker 'flycheck-command) '("-C"))))
  ;; ----------------------------------
  ;; PATCH: temp ファイルのデフォルトコーディングシステムを、
  ;;        強制的に UTF-8 (LF) とする
  ;; ----------------------------------
  ;; オーバーライド
  (defun flycheck-save-buffer-to-file (file-name)
    "Save the contents of the current buffer to FILE-NAME."
    ;; 他の部分は元定義と一致させる
    (make-directory (file-name-directory file-name) t)
    ;; FIXME: もっと柔軟に設定できるようにならないか？
    (let ((coding-system-for-write 'utf-8-unix) ; ここだけ変更決め打ちで変更する
          (jka-compr-inhibit t))
      (write-region nil nil file-name nil 0))))


;; ------------------------------------
;; テキスト＆コード静的解析：拡張（モードライン変更）
;; ------------------------------------
(leaf flycheck-color-mode-line
  :ensure t
  :hook ((flycheck-mode-hook . flycheck-color-mode-line-mode)))


;; ------------------------------------
;; Google 翻訳
;; ------------------------------------
(leaf google-translate
  :ensure t
  :bind (("C-c t t" . google-translate-at-point)
         ("C-c t RET" . google-translate-smooth-translate))
  :custom ((google-translate-output-destination . 'help)
           (google-translate-display-translation-phonetic . nil)
           (google-translate-translation-to-kill-ring . t)
           (google-translate-default-source-language . "en")
           (google-translate-default-target-language . "ja")
           (google-translate-translation-directions-alist . '(("ja" . "en")
                                                              ("en" . "ja")
                                                              ("ko" . "ja")
                                                              ("zh-TW" . "ja")
                                                              ("zh-CN" . "ja")))))


;; ------------------------------------
;; 特殊コメント強調
;; ------------------------------------
(leaf hl-todo
  :ensure t
  :custom ((hl-todo-keyword-faces . '(;; 既存
                                      ("HOLD" . "#99ff99")
                                      ("TODO" . "#99ff99")
                                      ("NEXT" . "#99ff99")
                                      ("THEM" . "#99ff99")
                                      ("PROG" . "#00ffff")
                                      ("OKAY" . "#00ffff")
                                      ("DONT" . "#ffffcc")
                                      ("FAIL" . "#ff0000")
                                      ("DONE" . "#00ff00")
                                      ("NOTE"   . "#ffccff")
                                      ("KLUDGE" . "#ffccff")
                                      ("HACK"   . "#ffccff")
                                      ("TEMP"   . "#ffccff")
                                      ("FIXME"  . "#ff0000")
                                      ("XXX"   . "#ffccff")
                                      ("CAUTION" . "#ffff00")
                                      ("WARNING" . "#ff0000")
                                      ;; 追加
                                      ("PATCH" . "#ffcc00"))))
  :global-minor-mode global-hl-todo-mode)


;; ------------------------------------
;; 強化バッファ一覧：拡張（`projectile' サポート）
;; ------------------------------------
(leaf ibuffer-projectile
  :ensure t
  :hook ((ibuffer-hook . ibuffer-projectile-set-filter-groups)))


;; ------------------------------------
;; 各種マウス操作無効化
;; ------------------------------------
(leaf inhibit-mouse
  :ensure t
  :global-minor-mode t)


;; ------------------------------------
;; Language Server Protocol (LSP) クライアント
;;
;; See:
;; https://emacs-lsp.github.io/lsp-mode/page/languages/
;; ------------------------------------
(leaf lsp-mode
  :ensure t
  :hook (;; 有効化は必要最小限にとどめる
         (css-mode-hook . lsp-deferred)
         (js-mode-hook . lsp-deferred)
         (json-mode-hook . lsp-deferred)
         (markdown-mode-hook . lsp-deferred)
         (python-mode-hook . lsp-deferred)
         (scss-mode-hook . lsp-deferred)
         (web-mode-hook . lsp-deferred)
         (nxml-mode-hook . lsp-deferred)
         (yaml-mode-hook . lsp-deferred))
  :custom (;;
           ;; `lsp-mode'
           ;;
           (lsp-semantic-tokens-enable . t)
           (lsp-restart . 'auto-restart)
           ;; ローカル環境にのみ保存させる
           (lsp-session-file . "~/.emacs.lsp-session")
           ;; LSP サーバからのファイル監視要求を無視する
           ;;
           ;; GNU Emacs の仕様で 1024 - 50 = 974 個以上のファイル監視が登録不可
           ;; LSP サーバによっては大量のファイル監視要求を行う → 意図的に無視する
           ;;
           ;; See:
           ;; https://www.reddit.com/r/emacs/comments/mq2znn/no_file_descriptors_left/
           ;; https://apple.stackexchange.com/a/418699
           ;; https://github.com/emacs-mirror/emacs/blob/0008003c3e466269074001d637cda872d6fee9be/src/kqueue.c#L387-L401
           (lsp-enable-file-watchers . nil)
           (lsp-eldoc-enable-hover . nil) ; Use `lsp-ui'
           (lsp-enable-indentation . nil) ; Use another formatter and each major-mode
           (lsp-enable-text-document-color . nil) ; Use major-mode
           (lsp-before-save-edits . nil) ; Use another formatter and each major-mode
           (lsp-modeline-diagnostics-enable . nil)
           (lsp-headerline-breadcrumb-enable . nil)
           (lsp-progress-function . 'ignore)
           (lsp-inlay-hint-enable . t)
           (lsp-trim-trailing-whitespace . nil) ; Use `whitespace'
           (lsp-insert-final-newline . nil) ; Use `editorconfig'
           (lsp-trim-final-newlines . nil) ; Use `editorconfig'
           (lsp-rename-use-prepare . nil)
           ;;
           ;; `lsp-javascript' (with TypeScript)
           ;;
           (lsp-javascript-format-enable . nil) ; Use another formatter and each major-mode
           (lsp-typescript-format-enable . nil) ; Use another formatter and each major-mode
           (lsp-typescript-surveys-enabled . nil)
           ;;
           ;; `lsp-html'
           ;;
           (lsp-html-format-enable . nil) ; Use another formatter and each major-mode
           (lsp-html-auto-closing-tags . nil) ; Use `web-mode'
           ;;
           ;; `lsp-eslint'
           ;;
           (lsp-eslint-experimental . '(;; Enable ESLint flat config
                                        ;;
                                        ;; See:
                                        ;; https://discord.com/channels/789885435026604033/1167077517157470278/1174364060712714310
                                        ;; https://github.com/microsoft/vscode-eslint/issues/1518#issuecomment-1319753092
                                        (useFlatConfig . true)))))


;; ------------------------------------
;; LSP: 拡張 (UI)
;;
;; `lsp-mode' が自動ロードする
;; ------------------------------------
(leaf lsp-ui
  :ensure t
  :bind ((:lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references)))
  :custom ((lsp-ui-doc-show-with-mouse . nil)
           (lsp-ui-doc-show-with-cursor . t)
           (lsp-ui-doc-header . t)
           (lsp-ui-doc-include-signature . t)
           (lsp-ui-doc-position . 'at-point)
           (lsp-ui-doc-alignment . 'window)
           (lsp-ui-doc-max-width . 200)
           (lsp-ui-doc-max-height . 50)
           (lsp-ui-doc-delay . 0.25)
           ;; `lsp-ui-doc' を有効にしているため、他は不要になる
           (lsp-ui-sideline-enable . nil)))


;; ------------------------------------
;; LSP: 拡張 (Tailwind CSS)
;;
;; `lsp-mode' が自動ロードする
;; ------------------------------------
(leaf lsp-tailwindcss
  :ensure t
  :custom ((lsp-tailwindcss-add-on-mode . t)))


;; ------------------------------------
;; LSP: 拡張 (ty)
;;
;; `lsp-mode' が自動ロードする
;; ------------------------------------
(leaf lsp-python-ty
  :hook ((python-mode-hook . my-lsp-python-ty-initialize))
  :init
  (defun my-lsp-python-ty-initialize ()
    "Initialize `lsp-python-ty' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/ty を自動検出
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (ty-path (expand-file-name ".venv/bin/ty" venv-dir)))
      (when (file-executable-p ty-path)
        (setq-local lsp-python-ty-clients-server-command `(,ty-path "server"))
        (message "Using project ty: %s" ty-path)))))


;; ------------------------------------
;; Git インターフェース
;; ------------------------------------
(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom ((auto-revert-buffer-list-filter . #'magit-auto-revert-buffer-p)))


;; ------------------------------------
;; Git インターフェース：拡張（GitHub CLI 対応）
;; ------------------------------------
(leaf magit-gh
  :ensure t)


;; ------------------------------------
;; 補完候補に拡張情報を追加
;; ------------------------------------
(leaf marginalia
  :ensure t
  :custom ((marginalia-field-width . 200)
           (marginalia-max-relative-age . 0))
  :global-minor-mode t)


;; ------------------------------------
;; 日本語インクリメンタル検索
;; ------------------------------------
(leaf migemo
  :when (executable-find "cmigemo")
  :leaf-defer nil
  :ensure t
  :require t
  :custom `(;; C/Migemo を利用する
            (migemo-command . ,(executable-find "cmigemo"))
            (migemo-options . '("-q" "--emacs"))
            ;; 空白文字の対象を拡大する
            (migemo-white-space-regexp . "[[:space:]\s-]*")
            ;; ユーザ別基礎ディレクトリは設定ディレクトリ内にまとめる
            (migemo-directory . ,(convert-standard-filename "~"))
            ;; `migemo' 側で定義済の `isearch' 関連キーバインドを使わせない
            ;; ミニバッファ内で `yank' できない現象が発生する問題の対策
            (migemo-use-default-isearch-keybinding . nil)
            ;; 辞書ファイルはデフォルトを利用する
            (migemo-dictionary . ,(catch 'founded
                                    (dolist (path '("/opt/homebrew/share/migemo/utf-8/migemo-dict"
                                                    "C:/programs/cmigemo/dict/utf-8/migemo-dic"))
                                      (if (file-readable-p path)
                                          (throw 'founded path)))))
            (migemo-user-dictionary . nil)
            (migemo-regex-dictionary . nil)
            ;; 辞書エンコーディングを明示する
            (migemo-coding-system . 'utf-8-unix)
            ;; キャッシュを利用する
            (migemo-use-pattern-alist . t)
            (migemo-use-frequent-pattern-alist . t)
            (migemo-pattern-alist-length . 1024)
            ;; ローカル環境にのみ保存させる
            (migemo-pattern-alist-file . "~/.emacs.migemo-pattern")
            (migemo-frequent-pattern-alist-file . "~/.emacs.migemo-frequent"))
  :config
  (migemo-init))


;; ------------------------------------
;; nvm 経由で Node.js を利用する
;; ------------------------------------
(leaf nvm
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :hook ((change-major-mode-after-body-hook . my-nvm-use-for-buffer))
  :init
  (defun my-nvm-use-for-buffer ()
    "Run `nvm-use-for-buffer', but crush the error."
    (ignore-errors (nvm-use-for-buffer)))
  :config
  ;; `~/.nvmrc' がなければ何もしない
  (ignore-errors (nvm-use-for)))


;; ------------------------------------
;; 正規表現を任意順でマッチさせる補完スタイル
;; ------------------------------------
(leaf orderless
  :ensure t
  :custom ((completion-styles . '(orderless))
           ;; `affe' と連携する
           (affe-regexp-function . #'orderless-pattern-compiler)
           (affe-highlight-function . #'orderless--highlight)))
;;
;; `migemo' が準備できたら使いはじめる
;;
;; See:
;; https://nyoho.jp/diary/?date=20210615
;;
(leaf orderless-migemo
  :after migemo
  :config
  (defun my-orderless-migemo (component)
    "Match COMPONENT as `migemo'."
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  (add-to-list 'orderless-matching-styles #'my-orderless-migemo))


;; ------------------------------------
;; 汎用プロジェクト管理
;; ------------------------------------
(leaf projectile
  :ensure t
  :bind ((:projectile-mode-map
          ("C-;" . projectile-next-project-buffer)
          ("C-:" . projectile-previous-project-buffer)))
  :custom `((projectile-enable-caching . t)
            (projectile-completion-system . ',(cond ((featurep 'ido) 'ido)
                                                    (t 'default)))
            ;; ローカル環境にのみ保存させる
            (projectile-cache-file . "~/.emacs.projectile.cache")
            (projectile-known-projects-file . "~/.emacs.projectile-bookmarks.eld"))
  :global-minor-mode t)


;; ------------------------------------
;; 同時置換
;; ------------------------------------
(leaf substitute
  :ensure t
  :bind (("C-M-r" . substitute-target-in-buffer))
  :custom ((substitute-highlight . t)))


;; ------------------------------------
;; `redo' を追加する
;; ------------------------------------
(leaf undo-fu
  :ensure t
  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))


;; ------------------------------------
;; 垂直インタラクティブ補完
;; ------------------------------------
(leaf vertico
  :ensure t
  :bind ((:vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("<DEL>" . vertico-directory-delete-char)
          ("<escape>" . minibuffer-keyboard-quit)
          ("RET" . vertico-directory-enter)))
  :hook ((minibuffer-setup . vertico-repeat-save))
  :custom ((vertico-count . 20)
           (vertico-cycle . t)
           (vertico-resize . t)
           (vertico-sort-function . #'vertico-sort-history-alpha))
  :global-minor-mode t)


;; ============================================================================
;; メジャーモード
;; ============================================================================
;; ------------------------------------
;; CSS
;; ------------------------------------
(leaf css-mode
  :custom ((css-indent-offset . 2)))


;; ------------------------------------
;; JavaScript (Main)
;; ------------------------------------
(leaf js-mode ; `js2-minor-mode' で拡張する
  :mode (("\\.pac\\'" . js-mode)
         ("\\.[cm]?js\\'" . js-mode)
         ("\\.es[0-9]\\'" . js-mode)
         ("\\.[jt]s\\'" . js-mode)
         ("\\.[jt]sx\\'" . js-jsx-mode))
  :custom ((js-indent-level . 2)
           (js-expr-indent-offset . 0)
           (js-paren-indent-offset . 0)
           (js-square-indent-offset . 0)
           (js-curly-indent-offset . 0)
           (js-switch-indent-offset . 0)
           (js-flat-functions . nil)
           (js-indent-align-list-continuation . t)
           (js-js-switch-tabs . t)
           (js-indent-first-init . 'dynamic)
           (js-chain-indent . t)))


;; ------------------------------------
;; JavaScript (Extend)
;; ------------------------------------
(leaf js2-mode
  :ensure t
  ;; HACK: `js2-minor-mode' で `js-mode' を拡張し、`js2-mode' は使わない
  :hook ((js-mode-hook . js2-minor-mode)
         (js-jsx-mode-hook . js2-minor-mode))
  :custom ((js2-highlight-level . 3) ; すべての構文強調を有効にする
           (js2-bounce-indent-p . t)
           (js2-idle-timer-delay . 0.25)
           (js2-dynamic-idle-timer-adjust . 0)
           (js2-concat-multiline-strings . t)
           ;; 文法チェック関連
           ;;
           ;; 他ツールに任せるため、すべて無効にする
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
           (js2-move-point-on-right-click . nil)
           (js2-allow-rhino-new-expr-initializer . nil)
           (js2-include-node-externs . t)
           ;; JSLint
           ;;
           ;; 他ツールに任せるため、すべて無効にする
           (js2-include-jslint-globals . nil)
           (js2-include-jslint-declaration-externs . nil)))


;; ------------------------------------
;; JSON
;; ------------------------------------
(leaf json-mode
  :ensure t
  :mode (("\\.ipynb\\'" . json-mode)))


;; ------------------------------------
;; Jupyter
;;
;; See:
;; https://docs.jupyter.org/en/latest/
;; https://github.com/emacs-jupyter/jupyter
;; https://docs.astral.sh/uv/guides/integration/jupyter/
;; ------------------------------------
(leaf jupyter
  :ensure t
  :hook ((python-mode-hook . my-jupyter-initialize)
         (jupyter-repl-mode-hook . my-jupyter-initialize))
  :custom (;; HACK: WebSocket を使う（ZMQ が不安定）
           (jupyter-use-zmq . nil))
  :init
  (defun my-jupyter-initialize ()
    "Initialize `jupyter' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/jupyter を自動検出する
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (jupyter-path (expand-file-name ".venv/bin/jupyter" venv-dir)))
      (when (file-executable-p jupyter-path)
        (setq-local jupyter-command jupyter-path)
        (message "Using project jupyter: %s" jupyter-path)))))


;; ------------------------------------
;; Markdown
;; ------------------------------------
(leaf markdown-mode
  :ensure t
  :custom `((markdown-coding-system . 'utf-8-unix)
            (markdown-enable-highlighting-syntax . t)
            (markdown-fontify-code-blocks-natively . t)
            (markdown-fontify-whole-heading-line . t)
            (markdown-open-command . ,(if (functionp 'arto-open)
                                          ''arto-open
                                        nil)))
  :config
  (require 'lsp-marksman nil :noerror))


;; ------------------------------------
;; Org
;; ------------------------------------
(leaf org
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o r" . org-capture))
  :custom ((org-use-speed-commands . t))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t))))


;; ------------------------------------
;; Python
;; ------------------------------------
(leaf python-mode
  :hook ((python-mode-hook . my-python-mode-initialize)
         (jupyter-repl-mode-hook . my-python-mode-initialize))
  :init
  (defun my-python-mode-initialize ()
    "Initialize `python-mode' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/python を自動検出する
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (python-path (expand-file-name ".venv/bin/python" venv-dir)))
      (when (file-executable-p python-path)
        (setq-local python-shell-interpreter python-path)
        (message "Using project python: %s" python-path)))))


;; ------------------------------------
;; Sass
;; ------------------------------------
(leaf scss-mode
  :ensure t
  :custom (;; 保存時コンパイルを無効にする（他ツールに任せる）
           (scss-compile-at-save . nil)))


;; ------------------------------------
;; Text
;; ------------------------------------
(leaf text-mode
  :hook ((text-mode-hook . my-text-mode-initialize))
  :init
  (defun my-text-mode-initialize ()
    "Initialize `text-mode' before load."
    (setq-local truncate-lines nil)))


;; ------------------------------------
;; Web
;; ------------------------------------
(leaf web-mode
  :ensure t
  :mode (("\\.[sx]?html?\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :custom ((web-mode-enable-css-colorization . t)
           (web-mode-enable-auto-indentation . nil)
           (web-mode-enable-auto-closing . t)
           (web-mode-enable-auto-pairing . nil) ; Use `electric-pair-mode'
           (web-mode-enable-auto-opening . t)
           (web-mode-enable-auto-quoting . nil) ; Use `electric-pair-mode'
           (web-mode-enable-auto-expanding . t)
           (web-mode-enable-curly-brace-indentation . t)
           (web-mode-enable-current-element-highlight . t)
           (web-mode-enable-current-column-highlight . t)
           (web-mode-enable-html-entities-fontification . t)
           (web-mode-enable-block-face . t)
           (web-mode-enable-part-face . t)
           (web-mode-enable-inlays . t)
           (web-mode-enable-sql-detection . t)
           (web-mode-enable-element-content-fontification . t)
           (web-mode-enable-element-tag-fontification . t))
  :defer-config
  (add-to-list 'web-mode-comment-formats '("php" . "//"))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//")))


;; ------------------------------------
;; XML
;; ------------------------------------
(leaf nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.plist\\'" . nxml-mode))
  :custom ((nxml-child-indent . 2)
           (nxml-attribute-indent . 0)
           (nxml-slash-auto-complete-flag . t)
           (nxml-bind-meta-tab-to-complete-flag . t)
           (nxml-sexp-element-flag . t)
           (nxml-char-ref-display-glyph-flag . t)))


;; ------------------------------------
;; YAML
;; ------------------------------------
(leaf yaml-mode
  :ensure t
  :custom ((yaml-indent-offset . 2)))


;; ============================================================================
;; ターミナルエミュレーター
;; ============================================================================
(leaf vterm
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :bind (("C-`" . vterm))
  :hook ((vterm-mode-hook . my-vterm-initialize))
  :custom ((vterm-buffer-name-string . "%s")
           (vterm-copy-mode-remove-fake-newlines . t)
           (vterm-max-scrollback . 100000)
           (vterm-shell . "bash"))
  :init
  (defun my-vterm-initialize ()
    "Initialize `vterm' before load."
    ;; 干渉するマイナーモードを無効にする
    (setq-local cua-mode nil)
    (setq-local undo-fu-mode nil))
  :defer-config
  ;; WARNING: 確実に `vterm-keymap-exceptions' が存在する状態で
  ;;          リストを操作しないと他のキーバインドに影響が出る
  ;;
  ;; `windmove' 用の設定をする
  ;; 設定はリストの末尾に追加せねばならない
  (add-to-list 'vterm-keymap-exceptions "C-S-b" t)
  (add-to-list 'vterm-keymap-exceptions "C-S-f" t)
  (add-to-list 'vterm-keymap-exceptions "C-S-n" t)
  (add-to-list 'vterm-keymap-exceptions "C-S-p" t))


;; ============================================================================
;; フォント (GUI ONLY)
;;
;; 独自定義したフォント設定
;;
;; WARNING: CSS と同じで、後に書いた定義のほうが強制利用される
;;          そのため、「上書きしたい定義は後に書く」よう注意すること
;;
;; 文字幅調整テスト
;;   az| アルファベット
;;   ¡©| ラテン文字 (`iso-8859-1')
;;   αß| ラテン文字 (`cp437')
;;   €ı| ラテン文字 (`cp858')
;;   ⌐░| 半角記号
;;   ×| 全角記号
;;   あ| ひらがな（日本語）
;;   简| 簡体字
;;   粵| 繁体字
;;   한| ハングル
;;   ไไ| タイ文字
;;   😊| 絵文字
;;
;; アラビア文字テスト
;;   ء آ أ ؤ إ ئ ا ب ة ت ث ج ح خ د
;;
;; 波ダッシュ字形テスト
;;   「〜」(U+301C: WAVE DASH)
;;   「～」(U+FF5E: FULLWIDTH TILDE)
;;
;; 関連コマンド一覧
;;   文字拡大／縮小モード：
;;     C-x C-0
;;   カーソルがポイントしている文字の「簡易」情報を表示する：
;;     C-x =
;;   カーソルがポイントしている文字の「詳細」情報を表示する：
;;     C-u C-x =
;;   フォントセットの詳細を別バッファに表示する：
;;     M-x describe-fontset
;;   定義済フォントセット一覧を別バッファに表示する：
;;     M-x list-fontsets
;;   利用可能フォントを一覧表示する：
;;     (dolist (xlfd (x-list-fonts "*")) (insert (format "%S" xlfd) "\n"))
;;   該当ファミリフォントを一覧表示する：
;;     (list-fonts (font-spec :family "ファミリ名"))
;;   定義済フォントセットを一覧表示する：
;;     (fontset-list)
;;   定義済フォントセットと別名（短縮名、エイリアス）の `alist'：
;;     `fontset-alias-alist'
;;   フレーム使用中フォントを表示する：
;;     (frame-parameter nil 'font)
;;
;; 関連ファイル
;;   `my-utils.el': 独自サポート関数＆マクロ定義
;;   `mule-conf.el': 文字セット定義（`set-fontset-font' 第2引数の定義一覧）
;;   `mule-diag.el': 文字セット／コーディングシステム用ツール定義
;;
;; エンコーディング解説
;;   ISO/IEC 8859-1 (`iso-8859-1')
;;     概要：
;;       * 国際標準のラテン文字セット
;;       * ASCII (`ascii') を基底に、更なる文字が追加されたもの
;;     視覚可能な追加文字：
;;       ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ
;;     全角フォントで表示されてほしいもの：
;;       ±×÷
;;     フォントによっては、他の文字と判別しにくいもの：
;;       "Ø" (U+00d8: LATIN CAPITAL LETTER O WITH STROKE)
;;     See:
;;       https://en.wikipedia.org/wiki/ISO/IEC_8859-1
;;
;;   Code page 437 (`cp437')
;;     概要：
;;       * ISO/IEC 8859-1 (`iso-8859-1') を基底に、更なる文字が追加されたもの
;;     視覚可能な追加文字：
;;       ₧ƒ⌐¬░▒▓│┤╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀αßΓπΣσµτΦΘΩδ∞φε∩≡≥≤⌠⌡≈∙√ⁿ■
;;     全角フォントで表示されてほしいもの：
;;       ∞∩≡■
;;     See:
;;       https://en.wikipedia.org/wiki/Code_page_437
;;
;;   Code page 858 (`cp858')
;;     概要：
;;       * Code Page 437 (`cp437') を基底に、更なる文字が追加されたもの
;;       * `cp850' の "ı" (U+0131) が "€" (U+20AC) に置換されている
;;     視覚可能な追加文字：
;;       €ıþ‗
;;     See:
;;       https://en.wikipedia.org/wiki/Code_page_850 (include `cp858' document)
;;
;;   JIS X 0213:2004 (`japanese-jisx0213.2004-1' and `japanese-jisx0213-2')
;;     字形変更：
;;       逢芦飴溢茨鰯淫迂厩噂餌襖迦牙廻恢晦蟹葛鞄釜翰翫徽祇汲灸笈卿饗僅喰櫛屑粂
;;       祁隙倦捲牽鍵諺巷梗膏鵠甑叉榊薩鯖錆鮫餐杓灼酋楯薯藷哨鞘杖蝕訊逗摺撰煎煽
;;       穿箭詮噌遡揃遜腿蛸辿樽歎註瀦捗槌鎚辻挺鄭擢溺兎堵屠賭瀞遁謎灘楢禰牌這秤
;;       駁箸叛挽誹樋稗逼謬豹廟瀕斧蔽瞥蔑篇娩鞭庖蓬鱒迄儲餅籾爺鑓愈猷漣煉簾榔屢
;;       冤叟咬嘲囀徘扁棘橙狡甕甦疼祟竈筵篝腱艘芒虔蜃蠅訝靄靱騙鴉
;;     平仮名、片仮名、記号など：
;;       ゔヿヷヸヹヺㇰㇱㇲㇳㇴㇵㇶㇷㇸㇹㇺㇻㇼㇽ
;;     第3水準（追加、1面）：
;;       旧：倶剥叱呑嘘妍屏并痩繋
;;       新：俱剝𠮟吞噓姸屛幷瘦繫
;;     第4水準（一部、2面）：
;;       𠂉𪚲
;;
;;   Microsoft Code page 932 (`cp932')
;;     概要：
;;       * 日本語
;;       * 俗称「Microsoft Shift_JIS」
;;       * JIS X 0213:2004 にはない文字が定義されている
;;     `cp932' にはあるが、JIS X 0213:2004 にはない文字：
;;       仼伹俍僴僘兤冾凬劜勀卲叝﨎坙坥墲奓奣妺峵巐弡恝悅惞愠愰戓敎昻昮晴朎櫢汯
;;       浯涖淸淲渹猤玽珒珵琩皂益硺礼靖精羡羽菶蕫蠇譓赶﨣軏逸遧釞鈆鉷﨧鋕﨨鎤鏸
;;       鐱鑈閒﨩靃靑飯飼餧館髙鮻鶴￢￤
;;     See:
;;       https://ja.wikipedia.org/wiki/Cp932
;;       https://internet.watch.impress.co.jp/www/column/ogata/news4.htm
;;       https://seiai.ed.jp/sys/text/csd/cf14/c14b050.html
;;
;; 参考文献
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lookup.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Selection.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html
;;   https://www.gnu.org/software/emacs/manual/html_node/efaq/Alternate-character-sets.html
;;   https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Fonts-and-text-translation.html
;;   https://www.emacswiki.org/emacs/DisplayingNonAsciiCharacters
;;   https://www.emacswiki.org/emacs/FontSets
;;   https://emacs.g.hatena.ne.jp/sakito/20100127
;; ============================================================================
(leaf *font
  ;; GUI 上でのみ設定する（設定する意味がないため、ターミナル上では何もしない）
  :when window-system
  :after my-utils
  :custom (;; シンボルや句読点などを表示するフォントを、設定に応じて選択する
           ;;   → GNU Emacs 25 より前のふるまいに戻す
           (use-default-font-for-symbols . nil)
           ;; 人為的に italic/bold フォントを選択する (Windows ONLY)
           (w32-enable-synthesized-fonts . t))
  :config
  ;; ----------------------------------
  ;; スケール変換
  ;; ----------------------------------
  (cond
   (;; Custom font usage:
    (and (my-fallback-font-family "Inconsolata")
         (my-fallback-font-family "VL Gothic")
         (my-fallback-font-family "Migu 1M"))
    (add-to-list 'face-font-rescale-alist '("-Menlo-" . 0.850))
    (add-to-list 'face-font-rescale-alist '("-Consolas-" . 0.950))
    (add-to-list 'face-font-rescale-alist '("-Courier-" . 0.850))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 0.850))
    (add-to-list 'face-font-rescale-alist '("-PingFang SC-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Microsoft YaHei-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-PingFang HK-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-MingLiU-ExtB-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-PingFang TC-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Microsoft JhengHei-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Apple SD Gothic Neo-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-Malgun Gothic-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Ayuthaya-" . 0.850))
    (add-to-list 'face-font-rescale-alist '("-Tahoma-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.785))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Emoji-" . 0.785))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Symbol-" . 1.000)))
   (;; "macOS" pre-install fonts ONLY:
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
   (;; "Windows" pre-install fonts ONLY:
    (and (equal window-system 'w32)
         (my-fallback-font-family "Consolas"))
    (add-to-list 'face-font-rescale-alist '("-メイリオ-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-ＭＳ ゴシック-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-Courier-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Microsoft YaHei-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-Microsoft JhengHei-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-Malgun Gothic-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-Tahoma-" . 1.100))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Emoji-" . 0.900))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Symbol-" . 1.200))))


  ;; ----------------------------------
  ;; フォントセット定義
  ;; ----------------------------------
  (let* (;; デフォルトフォントサイズ (pt)
         ;;
         ;; NOTE: 浮動小数点型 → pt
         ;;       整数型 → px
         (font-size (if (equal window-system 'w32) 12.0 14.0))
         ;; フォントセット ID
         (fontset "programming")
         ;; 基礎フォント
         (base-font-family (my-fallback-font-family "Inconsolata"
                                                    "Menlo"
                                                    "Consolas"
                                                    "Courier New"
                                                    "Courier"))
         ;; フォントセット生成
         (fontset-name (create-fontset-from-ascii-font base-font-family)))
    ;; 簡体字：GB 18030
    (my-set-fontset-font-safe fontset-name
                              'gb18030
                              (font-spec :family (my-fallback-font-family "PingFang SC"
                                                                          "Microsoft YaHei")))
    ;; 繁体字（香港／マカオ）：HKSCS-2016
    (my-set-fontset-font-safe fontset-name
                              'big5-hkscs
                              (font-spec :family (my-fallback-font-family "PingFang HK"
                                                                          "MingLiU-ExtB")))
    ;; 繁体字：Big5
    (my-set-fontset-font-safe fontset-name
                              'big5
                              (font-spec :family (my-fallback-font-family "PingFang TC"
                                                                          "Microsoft JhengHei")))
    ;; ハングル：KS C 5601-1987 (a.k.a. KS X 1001:1998)
    (my-set-fontset-font-safe fontset-name
                              'korean-ksc5601
                              (font-spec :family (my-fallback-font-family "Apple SD Gothic Neo"
                                                                          "Malgun Gothic")))
    ;; タイ文字：Thai Industrial Standard 620-2533 (TIS-620)
    (my-set-fontset-font-safe fontset-name
                              'thai-tis620
                              (font-spec :family (my-fallback-font-family "Ayuthaya"
                                                                          "Tahoma")))
    ;; アラビア文字
    ;;
    ;; HACK: Unicode 直接指定（`cp858' との重複を避け、`cp1256' は利用禁止）
    (dolist (range '((cons #x00600 #x006FF) ; U+0600-U+06FF (Arabic)
                     (cons #x00750 #x0077F) ; U+0750–U+077F (Arabic Supplement)
                     (cons #x008A0 #x008FF) ; U+08A0–U+08FF (Arabic Extended-A)
                     (cons #x0FB50 #x0FDFF) ; U+FB50–U+FDFF (Arabic Presentation Forms-A)
                     (cons #x0FE70 #X0FEFF) ; U+FE70–U+FEFF (Arabic Presentation Forms-B)
                     (cons #x10E60 #x10E7F) ; U+10E60–U+10E7F (Rumi Numeral Symbols)
                     (cons #x1EC70 #x1ECBF) ; U+1EC70–U+1ECBF (Indic Siyaq Numbers)
                     (cons #x1EE00 #x1EEFF))) ; U+1EE00-U+1EEFF (Arabic Mathematical Alphabetic Symbols)
      (my-set-fontset-font-safe fontset-name
                                range
                                (font-spec :family (my-fallback-font-family "Baghdad"
                                                                            "Microsoft Sans Serif"))))
    ;; 日本語：JIS X 0213:2004
    (my-set-fontset-font-safe fontset-name
                              'japanese-jisx0213.2004-1
                              (font-spec :family (my-fallback-font-family "VL Gothic"
                                                                          "Hiragino Sans"
                                                                          "メイリオ"
                                                                          "ＭＳ ゴシック")))
    (my-set-fontset-font-safe fontset-name
                              'japanese-jisx0213-2
                              (font-spec :family (my-fallback-font-family "VL Gothic"
                                                                          "Hiragino Sans"
                                                                          "メイリオ"
                                                                          "ＭＳ ゴシック")))
    ;; 日本語：Code page 932 (`cp932')
    (my-set-fontset-font-safe fontset-name
                              'cp932
                              (font-spec :family (my-fallback-font-family "VL Gothic"
                                                                          "Hiragino Sans"
                                                                          "メイリオ"
                                                                          "ＭＳ ゴシック")))
    ;; 「〜」(U+301C: WAVE DASH) と「～」(U+FF5E: FULLWIDTH TILDE) の字形を変更する
    ;; 視覚的な区別を可能にする
    ;;
    ;; 対応フォント：
    ;;   * "Migu 1M"
    ;;   * "ＭＳ ゴシック"
    (my-set-fontset-font-safe fontset-name
                              ;; 「〜」(U+301C: WAVE DASH)
                              (cons (string-to-char "〜") (string-to-char "〜"))
                              (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                          "Hiragino Sans"
                                                                          "ＭＳ ゴシック")))
    (my-set-fontset-font-safe fontset-name
                              ;; 「～」(U+FF5E: FULLWIDTH TILDE)
                              (cons (string-to-char "～") (string-to-char "～"))
                              (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                          "Hiragino Sans"
                                                                          "ＭＳ ゴシック")))
    ;; ラテン文字：Code page 858 (`cp858')
    (my-set-fontset-font-safe fontset-name
                              'cp858
                              (font-spec :family (my-fallback-font-family "Inconsolata"
                                                                          "Menlo"
                                                                          "Consolas"
                                                                          "Courier New"
                                                                          "Courier")))
    ;; 未実装グリフのフォールバック
    ;;
    ;; 未対応フォント：
    ;;   * "Inconsolata"
    (dolist (code (mapcar 'string-to-char
                          (split-string "ı░▒▓╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪▌αßΓπΣσµτΦΘΩδφε≥≤ⁿ≈∙√" "" t)))
      (my-set-fontset-font-safe fontset-name
                                (cons code code)
                                (font-spec :family (my-fallback-font-family "Menlo"
                                                                            "Consolas"
                                                                            "Courier New"
                                                                            "Courier"))))
    ;; 未実装グリフのフォールバック
    ;;
    ;; 未対応フォント：
    ;;   * "Consolas"
    (my-set-fontset-font-safe fontset-name
                              (cons (string-to-char "₧") (string-to-char "₧"))
                              (font-spec :family (my-fallback-font-family "Inconsolata"
                                                                          "Menlo"
                                                                          "Courier New"
                                                                          "Courier")))
    ;; 未実装グリフのフォールバック
    ;;
    ;; 未対応フォント：
    ;;   * "Inconsolata"
    ;;   * "Menlo"
    ;;   * "Consolas"
    (dolist (code (mapcar 'string-to-char
                          (split-string "⌐‗" "" t)))
      (my-set-fontset-font-safe fontset-name
                                (cons code code)
                                (font-spec :family (my-fallback-font-family "Courier New"
                                                                            "Courier"))))
    ;; 未実装グリフのフォールバック
    ;;
    ;; 未対応フォント対象：
    ;;   * "VL Gothic"
    ;;   * "Migu 1M"
    (my-set-fontset-font-safe fontset-name
                              (cons (string-to-char "￤") (string-to-char "￤"))
                              (font-spec :family (my-fallback-font-family "Hiragino Sans"
                                                                          "ＭＳ ゴシック")))
    ;; 一部グリフが `cp858' に含まれているため半角になる状態を回避する
    (dolist (code (mapcar 'string-to-char
                          (split-string "∞∩≡■" "" t)))
      (my-set-fontset-font-safe fontset-name
                                (cons code code)
                                (font-spec :family (my-fallback-font-family "VL Gothic"
                                                                            "Hiragino Sans"
                                                                            "ＭＳ ゴシック"))))
    ;; 一部グリフが半角になる状態を回避する
    ;;
    ;; 一部グリフが半角で定義されているフォント：
    ;;   * "VL Gothic"
    ;;   * "Hiragino Sans"
    ;;   * "メイリオ"
    (dolist (code (mapcar 'string-to-char
                          (split-string "±×÷" "" t)))
      (my-set-fontset-font-safe fontset-name
                                (cons code code)
                                (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                            "ＭＳ ゴシック"))))
    ;; 絵文字 (Emoji)
    (my-set-fontset-font-safe fontset-name
                              'emoji
                              (font-spec :family (my-fallback-font-family "Apple Color Emoji"
                                                                          "Symbola"
                                                                          "Segoe UI Emoji"
                                                                          "Segoe UI Symbol")))
    ;; WARNING: フォントサイズ変更「専用」の設定を行う
    ;;          他の `my-set-fontset-font-safe' で `:size' は設定しないこと！
    ;;          `C-x C-0' によるズームが効かなくなるため
    (my-set-fontset-font-safe fontset-name
                              'ascii
                              (font-spec :size font-size
                                         :family base-font-family))
    ;; 適用
    (modify-all-frames-parameters `((font . ,fontset-name)))
    (custom-set-faces `(tooltip ((t (:font ,fontset-name)))))
    )
  ) ; END of *font


;; ============================================================================
;; `early-init.el' での一時的な設定をリセット
;; ============================================================================
(leaf *early-init-el-restore
  :custom (;; ガベージコレクト閾値を既定値に戻す
           (gc-cons-threshold . 800000)))


;; ============================================================================
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here
