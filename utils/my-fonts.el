;;; my-fonts.el --- 設定 - フォント -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2022 Taku Watabe
;; Time-stamp: <2022-08-20T17:35:15+09:00>

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

;; WARNING: 「コーディングシステム」設定より後に実行すること
;;          「ロードパス追加」設定より後に実行すること

;; 独自定義したフォント設定の集合
;; `feature' 名 `my-fonts'
;;
;; 疑似名前空間プレフィクスは `my-'

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

;; 波ダッシュ字形テスト：
;;   「〜」(U+301C: WAVE DASH)
;;   「～」(U+FF5E: FULLWIDTH TILDE)

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

;; Microsoft Code page 858 (`cp858')
;;
;; 概要：
;;   * ラテン文字
;;   * 基底である ISO/IEC 8859-1 にはない文字を補完するときに利用する
;;   * `cp858' は `cp850' の "ı" (U+0131) を "€" (U+20AC) に置換したもの
;;   * `cp858' は `cp585' に "€" (U+20AC) を追加したもの
;;
;; 追加文字:
;;   ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜø£Ø×ƒáíóúñÑªº¿®¬½¼¡«»░▒▓│┤ÁÂÀ©╣║╗╝¢¥┐└┴┬├─┼ãÃ╚╔╩╦╠═╬¤ðÐÊËÈ€ÍÎÏ┘┌█▄¦Ì▀ÓßÔÒõÕµþÞÚÛÙýÝ¯´±‗¾¶§÷¸°¨·¹³²■
;;
;; 英字フォント（半角）で表示されてほしいもの：
;;   ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜø£ØƒáíóúñÑªº¿®¬½¼¡«»░▒▓ÁÂÀ©╣║╗╝¢¥ãÃ╚╔╩╦╠═╬¤ðÐÊËÈ€ÍÎÏ█▄¦Ì▀ÓßÔÒõÕµþÞÚÛÙýÝ¯‗¾¸·¹³²
;;
;; フォントによっては、他の文字と判別しにくいもの：
;;   "Ø" (U+00d8: LATIN CAPITAL LETTER O WITH STROKE)
;;
;; 全角フォントで表示されてほしいもの：
;;   ×│┤┐└┴┬├─┼┘┌´±¶§÷°¨■
;;
;; See also:
;; https://en.wikipedia.org/wiki/Code_page_858
;; https://en.wikipedia.org/wiki/Code_page_850

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

;; アラビア文字
;;
;; 表示例（一部）：
;; ء آ أ ؤ إ ئ ا ب ة ت ث ج ح خ د

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

;;; Code:


;; ============================================================================
;; ウインドウシステム上でのみ動作させる
;;   → ターミナルエミュレータ上では何もしない（設定する意味がないため）
;; ============================================================================
(when window-system
  ;; --------------------------------------------------------------------------
  ;; デフォルト値
  ;; --------------------------------------------------------------------------
  (custom-set-variables
   ;;
   ;; シンボルや句読点などを表示するフォントを、フォント設定に応じて選択させる
   ;;   → GNU Emacs 25 より前のふるまいに戻す
   ;;
   '(use-default-font-for-symbols nil))


  ;; --------------------------------------------------------------------------
  ;; スケール変換
  ;; --------------------------------------------------------------------------
  ;; 多バイト文字の認識に支障がある場合の書法：
  ;; (add-to-list 'face-font-rescale-alist `(,(encode-coding-string "-フォント名-" 'emacs-mule) . 倍率))
  ;; --------------------------------------------------------------------------
  (cond
   (;; Windows (96dpi) ONLY
    (and (< (car (my-real-display-pixels-per-inch)) 97.0) ; for 96dpi
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
    (my-fallback-font-family "Ricty Discord"
                             "Ricty")
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
    (my-fallback-font-family "Menlo")
    (add-to-list 'face-font-rescale-alist '("-Hiragino Sans-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-Courier-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-PingFang SC-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-PingFang HK-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-PingFang TC-" . 1.300))
    (add-to-list 'face-font-rescale-alist '("-Ayuthaya-" . 1.000))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.950)))
   (;; Windows pre-install fonts ONLY
    (my-fallback-font-family "Consolas")
    (add-to-list 'face-font-rescale-alist '("-Meiryo-" . 1.000))
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
  ;; フォントサイズは 1.5 の倍数（例：9, 10.5, 12, 13.5, 15, 16.5 etc.）を
  ;; 指定すると半角：全角 = 1:2 で表示されやすくなる
  ;;
  ;; See also:
  ;; https://rictyfonts.github.io/
  ;; --------------------------------------------------------------------------
  (my-create-fontset-from-spec "programming"
                               (font-spec :size 16.5 ; デフォルトフォントサイズ (pt)
                                          :family (my-fallback-font-family "Ricty Discord"
                                                                           "Ricty"
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
                            (font-spec :family (my-fallback-font-family "Ricty Discord"
                                                                        "Ricty"
                                                                        "ヒラギノ角ゴシック"
                                                                        "メイリオ"
                                                                        "Monospace")))
  (my-set-fontset-font-safe "fontset-programming"
                            'japanese-jisx0213-2
                            (font-spec :family (my-fallback-font-family "Ricty Discord"
                                                                        "Ricty"
                                                                        "ヒラギノ角ゴシック"
                                                                        "メイリオ"
                                                                        "Monospace")))
  ;; ラテン文字：Code page 858 (`cp858')
  (my-set-fontset-font-safe "fontset-programming"
                            'cp858
                            (font-spec :family (my-fallback-font-family "Ricty Discord"
                                                                        "Ricty"
                                                                        "Menlo"
                                                                        "Consolas"
                                                                        "Courier New"
                                                                        "Monospace")))
  (my-set-fontset-font-safe "fontset-programming"
                            (cons (string-to-char "░") (string-to-char "▓"))
                            ;; 次のフォントは U+2591, U+2592, U+2593 未実装：
                            ;;
                            ;;   * "Ricty"
                            ;;   * "Ricty Discord"
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
                            ;;   * "Ricty"
                            ;;   * "Ricty Discord"
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
                              (font-spec :family (my-fallback-font-family "Ricty Discord"
                                                                          "Ricty"
                                                                          "メイリオ"
                                                                          "Monospace"))))
  ;; ASCII
  (my-set-fontset-font-safe "fontset-programming"
                            'ascii
                            (font-spec :size 16.5 ; デフォルトフォントサイズ (pt)
                                       :family (my-fallback-font-family "Ricty Discord"
                                                                        "Ricty"
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
                 (:font ,fontset)))))))


(provide 'my-fonts)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; my-fonts.el ends here
