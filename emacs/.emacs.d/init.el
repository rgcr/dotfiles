;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => initialize package 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => custom definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'rgcr/help-map)

(add-hook 'eshell-mode 'rgcr/hide-fringes)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Use always y-or-n instead yes-or-not
(defalias 'yes-or-no-p 'y-or-n-p)

;; ;; Disable flycheck for emacs

;; ;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
;; (when (eq system-type 'windows-nt)
;;   (set-clipboard-coding-system 'utf-16le-dos))

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(show-paren-mode 1)
(column-number-mode t)

;; ;; Somewhat better formatting when showing line numbers, but still...
(setq linum-format "%4d \u2502 ")

;; (global-hl-line-mode 1)
;; (set-face-background ‘highlight “#222222”)

;; ;; Hide the startup message
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;; ;; Font and Frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))
;; (add-to-list 'default-frame-alist '(font . "mononoki-12"))
;; (add-to-list 'default-frame-alist '(height . 24))
;; (add-to-list 'default-frame-alist '(width . 80))
;; ;; Enable line numbers globally
(global-linum-mode t)

;; ;; Turn off autosave and backup files
(setq backup-inhibited t)
(setq auto-save-default nil)

;; ;; Spaces, not tabs
(setq-default indent-tabs-mode nil)

;; ;; Follow symlinks
(setq vc-follow-symlinks t)

;; ;; Bell
;; (setq visible-bell t)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)

;; ;; No wrap lines
(set-default 'truncate-lines t)

;; ;; Recent files
(setq recentf-max-saved-items 200)
(setq recentf-max-menu-items 200)
(recentf-mode 1)

;; (load-theme 'wombat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rgcr/reload-init-file ()
  (interactive)
  (load-file user-init-file))


(defun rgcr/comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

;; enable/disable tabs
(defun rgcr/disable-tab () (setq indent-tabs-mode nil))
(defun rgcr/enable-tab  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
   In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun rgcr/hide-fringes ()
  (set-window-fringes (selected-window) 0 0))

(defun rgcr/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun rgcr/close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => packages 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq use-package-always-ensure t)

;; Utilities

;; Use double key combination
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

;; ;; Emacs package from source
(use-package quelpa
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; better search
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy)
	  (t . ivy--regex-plus)))
  ;; Skip errors
  (setq ivy-on-del-error-function nil)
  ;; Delete symbol ^ 
  (setq ivy-initial-inputs-alist nil)
  ;; no regexp by default
  ;; (setq ivy-initial-inputs-alist nil)
  :config
  (ivy-mode t)
  )

(use-package swiper
  :ensure t
  :bind
  ([remap isearch-forward]  . swiper)
  ([remap isearch-backward] . swiper))

(use-package counsel
  :ensure t
  :bind
  ([remap execute-extended-command] . counsel-M-x)
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s .")
)

(use-package ace-window
  :ensure t
  :config
  (setq aw-scope 'frame)
  ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  )

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-popup-type 'minibuffer)
  ;; (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )


;; ------------------------------------
;; VIM Mode, welcome to the dark side

(use-package evil
  :ensure t
  :init
  ;; (setq evil-emacs-state-cursor '("red" box))
  ;; (setq evil-normal-state-cursor '("green" box))
  ;; (setq evil-visual-state-cursor '("orange" box))
  ;; (setq evil-insert-state-cursor '("red" bar))
  ;; (setq evil-replace-state-cursor '("red" bar))
  ;; setq evil-operator-state-cursor '("red" hollow))
  (evil-mode t)
  :config
  ;; Keybindings for neotree
  ;; (evil-define-key 'insert evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-change-root)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  )

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :init
  :config
  (progn
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "b"  'ivy-switch-buffer            ;; Switch to buffer
      "ci" 'rgcr/comment-dwim            ;; Comment/Uncomment
      "e"  'find-file  
      "i"  'highlight-indent-guides-mode  
      "l"  'whitespace-mode              ;; Toggle invisible characters
      "n"  'neotree-toggle               ;; Open file explorer (sidebar)
      "p"  'counsel-yank-pop             ;; fzf - paste
      "q"  'rgcr/kill-this-buffer        ;; Close current buffer
      "Q" 'delete-other-windows          ;; Close all other windows
      "R" 'rgcr/reload-init-file         ;; Reload emacs config
      "S"  'delete-trailing-whitespace
      "w"  'save-buffer
      "x"  'rgcr/close-and-kill-this-pane ;; I hate M-x
      "/"  'counsel-rg                    ;; fzf - ripgrep
      ","  'other-window                  ;; Switch to other window
      )
  )
  (global-evil-leader-mode)
)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; ;; Copy/paste
(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'js-mode-hook 'add-node-modules-path))

;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode))

;; Vimrc
(use-package vimrc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

;; ------------------------------------
;; Appearances


;; ;; Icons
;; (use-package all-the-icons
;;   :ensure t)

;; (use-package all-the-icons-dired
;;   :hook
;;   (dired-mode . all-the-icons-dired-mode))

;; Highlight the current line
(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode 1)
  (set-face-background hl-line-face "color-235")
  )

;; ;; Theme
(use-package doom-themes
  :after (spaceline)
  :init
  (setq
      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-one-brighter-comments t
      ;; doom-neotree-file-icons t)
      )
  (load-theme 'doom-Iosvkem t)
  :config
  (doom-themes-neotree-config))

;; (use-package monokai-theme
;;   :defer t
;;   :config
;;   (setq monokai-line-number "#444444")
;;   (setq monokai-background  "#444444")
;;   )

(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow)
  ;; (set-face-background 'spaceline-highlight-face "#005fff")
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-version-control-on)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-id-on)
  (spaceline-toggle-evil-state-on)
  (spaceline-toggle-selection-info-on)
  (spaceline-toggle-which-function-on)
  )

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  ;; :init
  ;; (progn
    ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    ;; )
  )

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    )
  )

;; ------------------------------------

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.33)
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (dimmer-mode))


;; ------------------------------------
;; Git

(use-package magit
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
  (add-hook 'python-mode-hook #'lsp)
  )

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

;; Show flake8 errors in lsp-ui
(defun lsp-set-cfg ()
  (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
    (lsp--set-configuration lsp-cfg)))

;; Activate that after lsp has started
(add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)

(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Custom keybinding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "-"   '(ace-window :which-key "switch window")
   "/"   '(counsel-rg :which-key "ripgrep")
   "SPC" '(execute-extended-command :which-key "M-x")
   ;; Buffers
   "b"   '(nil :which-key "buffer prefix")
   "bb"  '(switch-to-buffer :which-key "switch buffer")
   "bk"  '(rgcr/kill-this-buffer :which-key "kill buffer")
   ;; Git
   "g"   '(magit :which-key "magit")
   ;; Neotree
   "n"  '(neotree-toggle :which-key "neotree toggle")
   ;; Ace Windows 
   ;; "o"   '(ace-window :which-key "switch window")
   ;; Search
   "s"   '(nil :which-key "search prefix")
   "ss"  '(swiper-isearch :which-key "swipper search")
   ;; Window
   "w"   '(nil :which-key "window prefix")
   ;; "wl"  '(windmove-right :which-key "move right")
   ;; "wh"  '(windmove-left :which-key "move left")
   ;; "wk"  '(windmove-up :which-key "move up")
   ;; "wj"  '(windmove-down :which-key "move bottom")
   ;; "w|"  '(split-window-right :which-key "split right")
   ;; "w-"  '(split-window-below :which-key "split bottom")
   "wx"  '(delete-window :which-key "delete window")
   )

  (general-define-key
   :states '(normal emacs)
   :prefix "TAB"
   "TAB"     '(ivy-switch-buffer :which-key "Switch buffer")
   )

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "C-c"
   ;; Windows movemetn with C-c
   "<up>"    '(windmove-up :which-key "move up")
   "<down>"  '(windmove-down :which-key "move down")
   "<left>"  '(windmove-left :which-key "move left")
   "<right>" '(windmove-right :which-key "move right")
   ;; Split windows
   "|"       '(split-window-right :which-key "split right")
   "-"       '(split-window-below :which-key "split bottom")
   )
  )


(global-set-key (kbd "C-c p") 'counsel-fzf)
(global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "C-w k") 'rgcr/kill-this-buffer)
(global-set-key (kbd "C-x k") 'rgcr/kill-this-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => custom faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 107 :width normal))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "427fa665823299f8258d8e27c80a1481edbb8f5463a6fb2665261e9076626710" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "db10381a554231a40b7474eaac28bd58f05067faacce3b25d294bb179a3511a1" "868abc288f3afe212a70d24de2e156180e97c67ca2e86ba0f2bf9a18c9672f07" default)))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (dracula-theme zenburn-theme vimrc-mode use-package-chords spaceline simpleclip rainbow-delimiters projectile popup monokai-theme molokai-theme magit ibuffer-vc highlight-indent-guides git-gutter-fringe fzf flycheck exec-path-from-shell evil-nerd-commenter evil-leader evil-goggles evil-commentary doom-themes dimmer counsel atom-one-dark-theme all-the-icons-dired add-node-modules-path)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
