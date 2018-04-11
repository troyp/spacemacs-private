;;; autolookup.el --- On-the-fly lookup/sdic
;; Copyright (C) 2000 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Masatake YAMATO <jet@gyve.org>

;; This file will be part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; 説明:
;;
;; * autolookup.elとは
;; このプログラムは，特別な操作を必要とすることなく(Emacsの)ポイント下にある単語の訳を
;; エコーエリアに表示するプログラムです．ユーザが1秒間何もエディタ操作をしなけれ単語の訳が
;; 表示されます．エコーエリアに訳を表示するので，訳の表示によってウィンドウのレイアウトなどを
;; 変更してしまう恐れはありません．
;; 
;; * 利用に必要なもの
;; 単語の訳の取得するために，lookup(http://openlab.ring.gr.jp/lookup)か
;; sdic(http://www-nagao.kuee.kyoto-u.ac.jp/member/tsuchiya/sdic/)が
;; 必要です．
;; 
;; 開発には，
;; emacsには．XEmacs21.1を
;; lookupは1.3を，sdicは2.1.2を
;; 辞書は，gene95のみを用いました．それ以外の辞書を用いた場合や，gene95と併用した場合，
;; について調べていません．
;;
;; * 使い方
;; まずロードパスの通ったディレクトリにこのファイルを"autolookup.el"の名前で保存します．
;; lookupを使う場合には，
;;
;; (setq auto-lookup-backend 'lookup)
;;
;; sdicを使う場合には，
;;
;; (setq auto-lookup-backend 'sdic)
;;
;; を.emacsに追加します．さらに.emacsに次の4行を追加して，評価します．
;;
;; (autoload 'auto-lookup-mode "autolookup" 
;;           "ポイント下の単語の訳を自動的に表示するマイナーモード" t)
;; (autoload 'global-auto-lookup-mode "autolookup" 
;;           "ポイント下の単語の訳を自動的に表示するマイナーモードをすべてのバッファで起動する．" t)
;;
;; 次のどちらかのコマンドを実行して起動します．
;; M-x auto-lookup-mode
;; によって単語の訳を自動的に表示するマイナーモードをon/offすることができます．
;; マイナーモードなのでコマンドを実行したバッファでのみ単語の訳を自動的に表示します．
;;
;; M-x global-auto-lookup-mode
;; すべてのバッファで同時にauto-lookup-modeをon/offすることができます．
;;
;; * カスタマイズ
;; sdicを使うか,lookupを使うかは，`auto-lookup-backend'という
;; 変数で制御します．
;; 単語の訳を表示するまでの
;; 時間は`auto-lookup-interval'という変数により調整することができます．
;; sdicをバックエンドに使う場合，利用可能な(複数の)全ての辞書を用いると検索
;; に時間がかかる場合があります．Auto lookupで使用したい辞書を
;; `auto-lookup-backend:sdic-eiwa-dictionary-list'
;; や`auto-lookup-backend:sdic-waei-dictionary-list'
;; に指定して，検索時間を短縮できます．
;;
;; 変更履歴:
;;
;; Release 9
;; * Hideyuki SHIRAI (白井秀行)さん<shirai at rdmg.mgcs.mei.co.jp>の
;;   patchをあてた．
;;   - 検索単語中から助詞を切捨てる.
;;   - 日本語の単語は２文字以上を検索する.
;;   - 検索単語をdownwardする.
;;
;; Release 8
;; * 変数名を変更した．
;;   auto-lookup-mode-backend->auto-lookup-backend.
;; * 一文字の単語は検索しない(sdic)．
;; * sdic backendでsyntax tableを変更するのをやめた．
;; * シンボル名中のbackend-lookup，backend-sdic
;;   それぞれbackend:lookup, backend:sdicに変更した．
;; * sdicで複数行分の結果を表示するようにした．
;; * okada at opaopa.org (岡田 健一 /Kenichi OKADA)さんの和英辞書を引く
;;   ためのパッチを当てた．
;; * Hideyuki SHIRAI (白井秀行)さん<shirai at rdmg.mgcs.mei.co.jp>の
;;   sdic backendに対するパッチをあてた．
;;   - auto-lookup-backend:sdic-word-inputで取り出した単語に日本語文字が
;;     まじっていないかチェックする．
;;   - 検索先の英和辞書/英和辞書を限定する機能を追加した．
;; * Added etags backend (experimental).
;;
;; Release 7
;; * sdicとlookupの両方を使えるようにした．
;; * GNU Emacsでも動作を確認した．
;;
;; Release 6
;; * Authorのフィールドを変更した．
;;
;; Release 5
;; * global-auto-lookup-modeに入っていれば，新しいファイルを開いたときに，
;;   自動的にglobal-auto-lookup-modeに入るようにした．
;;
;; Release 4
;; * global modeに入るとすべてのバッファにAutoLookupと表示するようにした．
;; * lookupの結果を取得する関数と表示する関数を分離した．
;; * customizeできるようにした．
;; * ドキュメントを書いた．
;;
;; Release 3
;; * Added some documents.
;; * Avoid lookup if the cursor is in minibuffer.
;; * Avoid lookup if isearch is active.
;;
;; Release 2
;; * Added global-auto-lookup-mode

;; 謝辞:
;; autolookup.elは下記に掲載された，Keisuke Nishida<kxn30@po.cwru.edu> さん
;; のコードから派生したものです．
;; http://news.ring.gr.jp/news/openlab.lookup-ja/1012

;; TODO:
;; * 1行表示以外もサポートする．
;; * 他の辞書でもテストする.
;; * 複数のエントリの結果をかきあつめる．
;; * fsetの撤廃する．
;; * defcustom

;;; Code:
(require 'timer)

;;
;; Customizable variables
;; 
(defgroup auto-lookup nil
  "Lookup自動呼び出し"
  :group 'lookup)

(defcustom auto-lookup-interval 1 
  "*自動的にlookupを呼び出す時間間隔(秒)"
  :type  'number
  :group 'auto-lookup)

(defcustom auto-lookup-mode-string " AutoLookup"
  "Auto lookupモードのモード文字列"
  :type 'string
  :group 'auto-lookup)

(defcustom auto-lookup-backend 'lookup
  "Auto lookupモードで使う検索インターフェースの種類. lookupかsdic"
  :type '(choice (const lookup)
                 (const sdic)
                 (const etags))
  :group 'auto-lookup)

;;
;; Backend own custumize
;;
(defvar auto-lookup-backend:sdic-eiwa-dictionary-list 
  nil
  "Auto lookupのバックエンドとしてsdicを使う場合，どの英和辞書を使うか指定する．
指定の方法は，`sdic-eiwa-dictionary-list'と同じ形式である．nilの場合
`sdic-eiwa-dictionary-list'の値をそのまま使う．
例:
(setq auto-lookup-backend:sdic-eiwa-dictionary-list
      '((sdicf-client \"/usr/local/dict/gene.sdic\")))")

(defvar auto-lookup-backend:sdic-waei-dictionary-list 
  nil
  "Auto lookupのバックエンドとしてsdicを使う場合，どの和英辞書を使うか指定する．
指定の方法は，`sdic-waei-dictionary-list'と同じ形式である．nilの場合
`sdic-waei-dictionary-list'の値をそのまま使う．
例:
(setq auto-lookup-backend:sdic-waei-dictionary-list
      '((sdicf-client \"/usr/local/dict/edict.sdic\" 
        (add-keys-to-headword t))))")

;;
;; Core
;;
(defvar auto-lookup-history nil 
"最後にautolookup経由で検索された単語
同じ単語を繰り返し検索することを避ける．")
(defvar auto-lookup-function (lambda () (auto-lookup)))

(defun auto-lookup ()
  "タイマーから呼び出されてlookupを実行する関数"
  (let (word)
  (if (and 
       ;; 以下の条件を見たさない限り検索を行なわない．
       ;; サーチ中でないこと
       (not isearch-mode)
       ;; マクロ実行中でないこと
       (not executing-kbd-macro)
       ;; autolookupモードに入っていること
       auto-lookup-mode
       ;; 現在のバッファがmini bufferでないこと
       ;; Stolen from skk-in-minibuffer-p in skk-foreword of skk-10.59.
       (not (window-minibuffer-p (selected-window)))
       ;; 現在のバッファがlookupの監督するバッファないこと
       (auto-lookup-do-backend 'check-buffer (current-buffer))
       ;; 今から検索しようとする単語があること
       (progn 
         (setq word (auto-lookup-do-backend 'input))
         (and
          word
          (stringp word)
          (< 0 (length word))           ; 単語の長さを調べる．
          (or (not auto-lookup-history)
              (not (string= word auto-lookup-history)))
          (progn (setq auto-lookup-history word) t))))
      ;; 検索
      (let ((result
             (if (auto-lookup-describe-word word)
                 (auto-lookup-do-backend 'get-result))))
        ;; 結果表示
        (if result
            (auto-lookup-show-result (car result) (cdr result))
        )))))

(defun auto-lookup-describe-word (word)
  "lookupあるいは，sdicを用いてWORDを検索する．"
  ;; messageの定義を一時的に置き換えて検索中の出力を抑制する．
  (let ((message-backup (symbol-function 'message)))
    (unwind-protect
        (progn
          (defun message (fmt &rest args))
          (auto-lookup-do-backend 'describe word))
      (fset 'message message-backup))))


(defun auto-lookup-do-backend (task &optional arg)
  "lookupあるいはsdicの関数を呼び出す
TASK
  describe: 検索. 検索に成功した場合nil以外の値を返す．
  get-result: 検索結果の取得. (単語 . 意味)のペアを返す．
  input: ポイント位置から単語の取得, 
         単語を文字列で返す．検索対象が見つからない場合nilを返しても良い．
  check-buffer: バックエンドで管理しているバッファに対してnilを返す．"
  (cond 
   ((eq auto-lookup-backend 'lookup)
    (require 'lookup)
    (cond 
     ((eq task 'describe)
      (auto-lookup-backend:lookup-describe-word arg))
     ((eq task 'get-result)
      (auto-lookup-backend:lookup-get-result))
     ((eq task 'input)
      (auto-lookup-backend:lookup-word-input))
     ((eq task 'check-buffer)
      (auto-lookup-backend:lookup-check-buffer arg))))
   ((eq auto-lookup-backend 'sdic)
    (require 'sdic)
    (cond
     ((eq task 'describe)
      (auto-lookup-backend:sdic-describe-word arg))
     ((eq task 'get-result)
      (auto-lookup-backend:sdic-get-result))
     ((eq task 'input)
      (auto-lookup-backend:sdic-word-input))
     ((eq task 'check-buffer)
      (auto-lookup-backend:sdic-check-buffer arg))
     ))
   ((eq auto-lookup-backend 'etags)
    (require 'etags)
    (cond
     ((eq task 'describe)
      (auto-lookup-backend:etags-describe-word arg))
     ((eq task 'get-result)
      (auto-lookup-backend:etags-get-result))
     ((eq task 'input)
      (auto-lookup-backend:etags-word-input))
     ((eq task 'check-buffer)
      (auto-lookup-backend:etags-check-buffer arg))
     )) 
   (t
    nil)))

(defun auto-lookup-show-result (word line)
  "lookupの結果をエコーエリアに表示する．"
  (message "[%s] %s: %s" auto-lookup-backend word line))

;;
;; Sdic backend
;;
(defvar auto-lookup-backend:sdic-buffer-name " *Autolookup-sdic")
(defvar auto-lookup-backend:sdic-eiwa-symbol-list nil)
(defvar auto-lookup-backend:sdic-waei-symbol-list nil)

(defun auto-lookup-backend:sdic-word-input ()
  "sdicで検索する単語を取り出す．"
  ;; 短かい単語を検索しようとすると，検索に時間がかかるので
  ;; 短かい単語は検索しないようにする．
  (let ((word (sdic-word-at-point)))
    ;; wordがnilでないこと
    (if word
        ;; アルファベットのみからなる単語か日本語文字を含む単語か
        (if (not (string-match "\\cj" word))
            ;; アルファベットのみ
 	    ;; 3文字以上の場合のみwordを小文字にして返却する
	    (if (< 2 (length word)) (downcase word) nil)
          ;; 日本語文字を含む単語
          ;; 漢字を含むか含まないか
          (if (string-match "\\cC+" word)
              ;; 漢字を含む場合漢字部分を抽出して返却
              (save-match-data (match-string 0 word))
	    ;; カタカナを含む場合カタカナ部分を抽出
	    (if (string-match "\\cK+" word)
		(setq word (match-string 0 word)))
	    ;; ひらがな/カタカナ一文字は検索しない
	    (if (> (length word) 1) word nil)))
      nil)))

(defun auto-lookup-backend:sdic-describe-word (word)
  "sdicでWORDを引く"
  (save-excursion
    (save-match-data
      (set-buffer (get-buffer-create auto-lookup-backend:sdic-buffer-name))
      ;; 辞書の数を絞る．
      (let (;;; 英和系
            (sdic-eiwa-dictionary-list 
             ;; auto-lookupの設定があれば使う．
             (or auto-lookup-backend:sdic-eiwa-dictionary-list
                 ;; なければ，置き換えは諦める．
                 sdic-eiwa-dictionary-list))
            ;; sdicの内部変数も置き換える．
            (sdic-eiwa-symbol-list auto-lookup-backend:sdic-eiwa-symbol-list)
            ;;; 和英系
            (sdic-waei-dictionary-list 
             (or auto-lookup-backend:sdic-waei-dictionary-list
                 sdic-waei-dictionary-list))
            (sdic-waei-symbol-list auto-lookup-backend:sdic-waei-symbol-list)
            ;;; 
            (case-fold-search t))
        ;; mode-nameを調べて繰り返しsdic-modeに入るのを防ぐ．
        ;; ただし，(sdic-mode)が呼び出されたときだけ
        ;; sdic-eiwa-dictionary-listや
        ;; sdic-eiwa-symbol-listの置き換えが作用する.
        (or (string= mode-name sdic-mode-name) (sdic-mode))
        (setq buffer-read-only nil)
        (erase-buffer)
        (prog1
            (if (string-match "\\cj" word)
                (funcall 'sdic-search-waei-dictionary word)
              (funcall 'sdic-search-eiwa-dictionary word))
          ;; 置き換わった内部変数をキャッシュして次回の(auto-lookupによる)
          ;; 検索時に使う．
          (setq auto-lookup-backend:sdic-eiwa-symbol-list sdic-eiwa-symbol-list
                auto-lookup-backend:sdic-waei-symbol-list sdic-waei-symbol-list))))))

(defun auto-lookup-backend:sdic-get-result ()
  "sdicの検索結果を取り出す．
結果は，(単語 . 訳)というペアである．"
  (with-current-buffer  (get-buffer-create auto-lookup-backend:sdic-buffer-name)
    (goto-char sdic-buffer-start-point)
    (let (b e b0 word (line "") (flag t))
      ;;
      (setq b (point))
      (end-of-line)
      (setq e (point))
      (setq word (buffer-substring b e))
      (beginning-of-line)
      ;;
      (while flag
        (forward-line 1)
        (setq b0 (point))
        (back-to-indentation)
        (setq b (point))
        (end-of-line)
        (setq e (point))
        (if (not (eq b b0))
            (setq line (concat line (buffer-substring b e)))
          (setq flag nil)))
      (cons word line))))

(defun auto-lookup-backend:sdic-check-buffer (buffer)
"BUFFERがsdicによって管理されているバッファかどうか調べる．
NILの場合管理されている．"
  (not (eq buffer
           (get-buffer auto-lookup-backend:sdic-buffer-name))))

;;
;; Lookup backend
;;
;; current-prefix-argがnil以外の値を持つとき，
;; lookup-word-inputがmodule名を尋ねてくるので
;; わずらわしい．current-prefix-argを一時的にnilとする．
(defun auto-lookup-backend:lookup-word-input ()
  "lookupで検索する単語を取り出す．"
  (let* ((current-prefix-arg nil)
         (input (lookup-word-input)))
    (if (and input (car input) (stringp (car input)))
        (car input)
      nil)))

(defun auto-lookup-backend:lookup-describe-word (word)
  "lookupで単語を引く．"
  ;; lookup-wordの内部で呼び出して欲しくない関数を置き換える前に
  ;; バックアップアップを作成する．
  (let ((current-prefix-arg nil)
        (lookup-display-buffer-backup 
         (symbol-function 'lookup-display-buffer))
        (lookup-pop-to-buffer-backup
         (symbol-function 'lookup-pop-to-buffer))
        (result nil))
    (unwind-protect
        (condition-case nil
            (progn
              ;;
              ;; 関数を置き換える．
              ;;
              (defun lookup-display-buffer (buffer))
              (defun lookup-pop-to-buffer (buffer))
              ;;
              ;; lookupの呼び出し．
              ;;
              (call-interactively 'lookup-word)
              ;; 成功
              (setq result t))
          (error nil))
      ;;
      ;; 置き換えた関数を復元する．
      ;;
      (fset 'lookup-display-buffer lookup-display-buffer-backup)
      (fset 'lookup-pop-to-buffer lookup-pop-to-buffer-backup))
    result))

;; この関数を変更すれば任意の辞書に対応できるはず．
(defun auto-lookup-backend:lookup-get-result ()
  "lookupのの検索結果を取り出す．
結果は，(単語 . 訳)というペアである．"
  (let (b e word (line ""))
    (with-current-buffer  (lookup-content-buffer)
      (goto-char (point-min))
      ;; 単語を取り出す．
      (setq b (point-min))
      (end-of-line)
      (setq e (point))
      (setq word (buffer-substring b e))
      (beginning-of-line)

      ;; 訳を取り出す．
      (while (not (eobp))
        (forward-line 1)
        (back-to-indentation)
        (setq b (point))
        (end-of-line)
        (setq e (point))
        (setq line (concat line (buffer-substring b e)))))
    (cons word line)))

(defun auto-lookup-backend:lookup-check-buffer (buffer)
  "BUFFERがlookupによって管理されているバッファかどうか調べる．
NILの場合管理されている．"
  (not (memq buffer lookup-buffer-list)))

;;
;; Etags backend
;;
;;
;;; eltags.el --- electric tag search
;;
;; Copyright (C) 1999  Masatake YAMATO, Takaaki MORIYAMA
;;
;; Author: Masatake YAMATO<masata-y@is.aist-nara.ac.jp>
;;         Takaaki MORIYAMA<taka@airlab.cs.ritsumei.ac.jp>
;; Keywords: etags
;; Version: v0
;; Revision: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defvar auto-lookup-backend:etags-data-cache nil)
(defvar auto-lookup-backend:etags-type-cache nil)
(defun auto-lookup-backend:etags-describe-word (symbol)
  (let (result b e)
    (setq result (condition-case ()
                     (find-tag-internal symbol)
                   (error nil)))
    (setq tags-loop-scan '(display-tag-info nil)
          tags-loop-operate nil)
    (if result
        (save-excursion
          (set-buffer (car result))
          (goto-char (cdr result))
          (beginning-of-line)
          ;;
          (setq auto-lookup-backend:etags-type-cache
                (cond
                 ((equal ?# (char-after))
                  ;;
                  ;; Macro
                  ;;
                  (setq b (point)
                        e (progn (next-line 1) (end-of-line) (point)))
                  "M")
                 ((save-match-data
                    (let ((colon -1)
                          (paren -1))
                      (save-excursion 
                        (if (re-search-forward ";\\|{" nil t)
                            (setq colon (point))))
                      (save-excursion 
                        (if (re-search-forward "(" nil t)
                            (setq paren (point))))
                      (if (or (eq -1 colon) (eq -1 paren))
                          nil
                        (if (< paren colon)
                            t
                          nil))))
                  ;;
                  ;; Function
                  ;;
                  (setq b (save-excursion (previous-line 1) (beginning-of-line) (point))
                        e (if (re-search-forward ")" nil t)
                              (point)
                            (next-line 1) (end-of-line) (point)))
                  "F")
                 (t
                  ;;
                  ;; typedef, struct, enum, variable...
                  ;;
                  (setq b (point)
                        e (progn (end-of-line) (point)))
                  "E"
                  )))
          (setq auto-lookup-backend:etags-data-cache
                (replace-in-string 
                 (replace-in-string (buffer-substring b e) "\n\\|\t" " ")
                 "  +" " "))
          t)
      nil)))

(defun auto-lookup-backend:etags-get-result ()
  (cons auto-lookup-backend:etags-type-cache
        auto-lookup-backend:etags-data-cache))

(defun auto-lookup-backend:etags-word-input ()
  (find-tag-default))

(defun auto-lookup-backend:etags-check-buffer (buffer)
  (not (string= "TAGS" (buffer-name buffer))))

;;
;; Minor mode
;;
(defvar auto-lookup-mode nil)
(make-variable-buffer-local 'auto-lookup-mode)

(defun auto-lookup-mode (&optional arg)
  "現在のバッファでAuto lookupを実行する. "
  (interactive "P")
  (setq auto-lookup-history nil)
  (setq auto-lookup-mode (if arg
                             (> (prefix-numeric-value arg) 0)
                           (not auto-lookup-mode)))
  (if auto-lookup-mode
      (auto-lookup-try-timer-start)
    (auto-lookup-try-timer-end)))

(if (fboundp 'add-minor-mode)
    (add-minor-mode 'auto-lookup-mode auto-lookup-mode-string)
  ;; Put this minor mode on the global minor-mode-alist.
  (or (assq 'auto-lookup-mode (default-value 'minor-mode-alist))
      (setq-default minor-mode-alist
                    (append (default-value 'minor-mode-alist)
                            '((auto-lookup-mode auto-lookup-mode-string))))))

;;
;; Global mode
;;
(defvar global-auto-lookup-mode nil)
(defun global-auto-lookup-mode (&optional arg)
  "すべてのバッファでAuto lookupを実行する. "
  (interactive "P")
  (setq auto-lookup-history nil)
  (let ((on-p (if arg
                  (> (prefix-numeric-value arg) 0)
                (not global-auto-lookup-mode))))
    (setq global-auto-lookup-mode on-p)
    ;; From flyspell.el
    ;; 将来作られるバッファでauto-lookup-modeを起動, 停止する．
    (cond
     (on-p
      (add-hook 'find-file-hooks 'turn-on-auto-lookup-if-enabled)
      (add-hook 'first-change-hook 'turn-on-auto-lookup-if-enabled))
     (t
      (remove-hook 'find-file-hooks 'turn-on-auto-lookup-if-enabled)
      (remove-hook 'first-change-hook 'turn-on-auto-lookup-if-enabled)))
    ;; 現在存在する，すべてのバッファでauto-lookup-modeを起動, 停止する．
    (mapcar
     (function 
      (lambda (buffer)
        (if (auto-lookup-do-backend 'check-buffer buffer)
            (with-current-buffer buffer
              (auto-lookup-mode (if on-p 1 -1))))))
     (buffer-list))))

(defun turn-on-auto-lookup-if-enabled ()
  (and global-auto-lookup-mode
       (not auto-lookup-mode)
       (auto-lookup-do-backend 'check-buffer (current-buffer))
       (auto-lookup-mode 1)))

;;
;; Timer
;;
(defvar auto-lookup-timer nil "Auto lookupのタイマを保持する変数")
(defun auto-lookup-try-timer-start ()
  "Auto lookupのタイマをスタートする．
複数のAuto lookupのタイマが設定しないように`auto-lookup-timer'の値を参照する．"
  (when (not auto-lookup-timer)
    (setq auto-lookup-timer (run-with-idle-timer 
                             auto-lookup-interval
                             t 
                             auto-lookup-function))))

(defun auto-lookup-try-timer-end ()
  "Auto lookupのタイマを停止する．
ただし，すべてのバッファがAuto lookupから抜けたときのみ停止する，"
  (if auto-lookup-timer
      (let ((autolookuped-buffer nil))
        ;; Auto lookup modeに入っているバッファが
        ;; 1つもないか調べる．
        (mapcar
         (function 
          (lambda (buffer)
            (with-current-buffer buffer
              (if auto-lookup-mode
                  (setq autolookuped-buffer t)))))
         (buffer-list))
        ;; Auto lookup modeに入っているバッファが1つもなけ
        ;; ればタイマを停止する．
        (when (and autolookuped-buffer)
          (cancel-timer auto-lookup-timer)
          (setq auto-lookup-timer nil)))))

(provide 'autolookup)
;;; autolookup.el ends here
