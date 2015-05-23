; githubからpullしemacsを設定する際は、~/.emacs.d/conf ~/.emacs.d/public_repos の2つのディレクトリの作成が必要

; 言語を日本語にする
(set-language-environment 'Japanese)
; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
;; (add-to-load-path "elisp" "conf" "public_repos")
(add-to-load-path "elisp")

;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
;; (require 'init-loader)
;; (init-loader-load "~/.emacs.d/conf") ; 設定ファイルがあるディレクトリを指定

;; 画面移動を↓で可能に
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; 入力されるキーシーケンスを置き換える(<backspace>をC-hで実行できるようにした)
(keyboard-translate ?\C-h ?\C-?)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; "C-t" でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; 文字コードを指定する
(set-language-environment "Japanese")

;; nilが返却された(ロードされていないかも)
(prefer-coding-system 'utf-8)

;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
(global-linum-mode t)

;; php-modeのみタブを利用しない
(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; テーマの設定
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; paren-mode : 対応するカッコを強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は、0.125
(show-paren-mode t) ; 有効化
(setq show-paren-style 'parenthesis) ; 対応する括弧だけをハイライト

;; ファイルが #! から始まる場合、+xを付けて保存する
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールするディレクトリを設定する 初期値は ~/emacs.d/auto-install
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup)

  (require 'anything-auto-install nil t))

;; anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補ば多い時に体感温度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
	     (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))

;; M-yにanything-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; M-fにanything-for-filesを割り当てる
(define-key global-map (kbd "M-f") 'anything-for-files)

;; パッケージに依存しているパッケージを同時にインストールするための設定
;; (参照)http://rubikitch.com/package-initialize/
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; auto-completeの設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start t)

;; php-mode
(require 'php-mode)
(setq php-mode-force-pear t) ;PEAR規約のインデント設定にする
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode)) ;*.phpのファイルのときにphp-modeを自動起動する

;; php-mode-hook
(add-hook 'php-mode-hook
          (lambda ()
            (require 'php-completion)
            (php-completion-mode t)
            (define-key php-mode-map (kbd "C-o") 'phpcmp-complete) ;php-completionの補完実行キーバインドの設定
            (make-local-variable 'ac-sources)
            (setq ac-sources '(
                               ac-source-words-in-same-mode-buffers
                               ac-source-php-completion
                               ac-source-filename
                               ))))

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hock 'js-indent-hock)

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; メニューバーを非表示
(menu-bar-mode 0)
;; ツールバーを非表示
(tool-bar-mode 0)

;; 画面3分割
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(global-set-key "\C-x@" '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(global-set-key "\C-x4" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))

;; color-moccurの設定
(when (require 'color-moccur nil t)
  ;; M-oにoccur-by-moccurを割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ;; Migemoを利用できる環境であればMigemoを使う
  (when (and (executable-find "cmigemo")
	     (requie 'migemo nil t))
    (setq moccur-use-migemo t)))

;; moccur-editの設定
(require 'moccur-edit nil t)

;; wgrepの設定
(require 'wgrep nil t)

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-treeの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; point-undoの設定
(when (require 'point-undo nil t)
  ;; (define-key global-map [f5] 'point-undo)
  ;; (define-key global-map [f6] 'point-redo)
  ;; 筆者のオススメのキーバインド
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo))

;; howmメモ保存の場所
(setq howm-directory (concat user-emacs-directory "howm"))
;; howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
;; howmメモを1日1ファイルにあるう
;; (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")
;; howm-modeを読み込む
(when (require 'howm-mode nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;; hownメモを保存と同時に閉じる
(defun howm-save-buffer-and-kill ()
  "hownメモを保存と同時に閉じます。"
  (interactive)
  (when (and (buffer-file-name)
	     (string-match "\\.hown" (buffer-file-name)))
    (save-buffer)
    (kill-buffer nill)))

;; C-c C-cでメモの保存と同時にバッファを閉じる
(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;; cssm-modeの設定
(defun css-mode-hooks ()
  "css-mode hooks"
  ;; インデントをCスタイルにする
  (setq cssm-indent-function #'cssm-c-style-indenter)
  ;; インデント幅を2にする
  (setq cssm-indent-level 2)
  ;; インデントにタブ文字を使わない
  (setq-default indent-tabs-mode nil)
  ;; 閉じカッコの前に改行を挿入する
  (setq cssm-newline-before-closing-bracket t))

(add-hook 'css-mode-hook 'css-mode-hooks)

;; 指定行数へのジャンプに関するエイリアスの設定 M-x gで行数指定が可能に
(defalias 'g 'goto-line)

;; yaml-modeの設定
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


;; 括弧自動補完機能
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))

(add-hook 'php-mode-hook
              (lambda ()
                ;; (define-key php-mode-map "\"" 'electric-pair)
                ;; (define-key php-mode-map "\'" 'electric-pair)
                (define-key php-mode-map "(" 'electric-pair)
                (define-key php-mode-map "[" 'electric-pair)
                (define-key php-mode-map "{" 'electric-pair)))

;; flymakeの設定
(require 'flymake)
(add-hook 'php-mode-hook
          '(lambda ()
             (flymake-mode t)))

;; HTML用のFlymakeの設定
(defun flymake-html-init ()
  (list "tidy" (list (flymake-init-create-temp-buffer-copy
		      'flymake-create-temp-inplace))))

(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.html\\'" flymake-html-init))

;; tidy error patttern
(add-to-list 'flymake-err-line-patterns
	 '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\); \\(.*\\)"
	   nil 1 2 4))

;; JS用Flymakeの初期化関数の定義
(defun flymake-jsl-init ()
  (list "jsl" (list "-process" (flymake-init-create-temp-buffer-copy
				'flymake-create-temp-inplace))))

;; Javascript編集でFlymakeを起動する
(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.js\\'" flymake-jsl-init))

(add-to-list 'flymake-err-line-patterns
	     '("^\\(.+\\)(\\([0-9]+\\)): \\(.*warning\\|SyntaxError\\): \\(.*\\)"
	       1 2 nil 4))

;; anything-for-tagsコマンドを作成
(defun anything-for-tags ()
  "Preconfigured `anything' for anything-for-tags."
  (interactive)
  (anything anything-for-tags
	    (thing-at-point 'symbol)
	    nil nil nil "*anything for tags*"))

;; M-tにanything-for-tagsを割り当て
(define-key global-map (kbd "M-t") 'anything-tags-for)

(require 'grep-edit)

(require 'helm-config)
(require 'helm-gtags)

(add-hook 'php-mode-hook 'helm-gtags-mode)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
