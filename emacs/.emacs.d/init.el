;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; => start as server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => custom definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'rgcr/help-map)

(add-hook 'eshell-mode 'rgcr/hide-fringes)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(normal-erase-is-backspace-mode 0)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Esc to quit from prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Font and Frame size
(add-to-list 'default-frame-alist '(font . "Hack-9"))

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Use always y-or-n instead yes-or-not
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
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
(electric-pair-mode 1)
(column-number-mode t)

;(global-linum-mode 1)
(winner-mode t)

;; Somewhat better formatting when showing line numbers, but still...
(setq linum-format "%4d \u2502 ")

;; (global-hl-line-mode 1)
;; (set-face-background ‘highlight “#222222”)

;; Hide the startup message
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Turn off autosave and backup files
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Bell
;; (setq visible-bell t)
(set-cursor-color "#ffffff")

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)

;; No wrap lines
(set-default 'truncate-lines t)

;; keep the list of recent files
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 100)
(recentf-mode 1)

;; remember cursor position, for emacs 25.1 or later
;; by default, the cursor position info is saved at ~/.emacs.d/places
(save-place-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => global hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => file type overrides (modes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.restc$" . restclient-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rgcr/reload-init-file ()
  (interactive)
  (load-file user-init-file))


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

;; -------------------------
;; >>>> UI
;; -------------------------
(use-package recentf
  ;; Loads after 1 second of idle time.
  :defer 1)

;; Highlight the current line
(use-package hl-line
  :ensure t
  :delight
  :config
  (global-hl-line-mode 1)
  (set-face-background hl-line-face "color-235")
  )

(use-package hl-todo
  :ensure t
  :delight
  :init
  (setq hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode t))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol ">>")
  ;; (setq linum-format "%4d \u2502 ")
  (setq linum-relative-format "%4s \u2502")
  (linum-relative-on))

;; ;; Theme
(use-package doom-themes
  :ensure t
  :after (spaceline)
  :init
  (setq
      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-one-brighter-comments t
      ;; doom-neotree-file-icons t)
      )
  (load-theme 'doom-Iosvkem t)
  ;; (load-theme 'doom-molokai t)
  )

;; (use-package kaolin-themes
;;   :config
;;   ;; (load-theme 'kaolin-dark t)
;;   (kaolin-treemacs-theme))

(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow)
  ;; (set-face-background 'spaceline-highlight-face "#005fff")
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-on)
  (spaceline-toggle-version-control-on)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-id-on)
  (spaceline-toggle-evil-state-on)
  (spaceline-toggle-selection-info-on)
  (spaceline-toggle-which-function-on)
  )

(use-package color-identifiers-mode
    :ensure t
    :config
    (global-color-identifiers-mode)
    )

(use-package centaur-tabs
  :ensure t
  ;; :demand t
  :init
  (centaur-tabs-mode)
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar t)
  (setq centaur-tabs-set-modified-marker t)
  ;; (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-close-button "X")
  (setq centaur-tabs-modified-marker "*")
  (set-face-foreground 'centaur-tabs-unselected "brightblack")
  (set-face-foreground 'centaur-tabs-selected "cyan")
  )

;; -------------------------
;; >>>> General utils
;; -------------------------

;; Emacs packages from source
(use-package quelpa
  :ensure t)


;; Make sure that delight is available as soon as any package triggers it.
(use-package delight
  :commands delight)

(use-package undo-tree
  :ensure t
  :delight
  :init
  (undo-tree-mode))

(use-package eldoc
  :delight
  :config (add-hook 'prog-mode-hook 'eldoc-mode))

;; org
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-cycle-separator-lines 2
        org-deadline-warning-days 30)
  (setq org-icalendar-timezone "America/Monterrey")
  ;; When a TODO item enters DONE, add a CLOSED: property with current date-time stamp and into drawer
  (setq org-log-done 'time)
  (setq org-log-into-drawer "LOGBOOK")

  (setq org-agenda-files
	'("~/.onedrive/org/personal.org"
          "~/.onedrive/org/work.org"
	  "~/.onedrive/org/diary.org"))
   ;; I use my own diary file
   (setq org-agenda-include-diary nil)
   (setq org-archive-location "~/.onedrive/archive/%s_archive.org::datetree/")
   (setq org-todo-keywords
         '((sequence "TODO(t!)"
                     "IN PROCESS(i!)"
                     "BLOCKED(b!)"
                     "|" "DONE(d!)" "ARCHIVE(a!)")))
   (setq org-todo-keyword-faces
         '(("TODO" . "orange")
           ("IN PROCESS" . "yellow")
           ("BLOCKED" . "red")
           ("DONE" . "green")
           ("ARCHIVE" .  "blue")))

   ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  )

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-autolist
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode 1))))

(use-package ivy
  :ensure t
  :delight
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
  :delight
  :bind
  ([remap isearch-forward]  . swiper)
  ([remap isearch-backward] . swiper))

(use-package counsel
  :ensure t
  :delight
  :bind
  ([remap execute-extended-command] . counsel-M-x)
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s .")
)

(use-package ivy-rich
  :after ivy
  :demand t
  :config
  (ivy-rich-mode 1)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package hydra
  :ensure t)

(use-package ace-window
  :ensure t
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

;; custom keybindings
(use-package general
  :ensure t
  :delight)

;; Which Key
(use-package which-key
  :ensure t
  :delight
  :init
  (setq which-key-popup-type 'minibuffer)
  ;; (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-sort-uppercase-first nil)
  (setq which-key-min-display-lines 6)
  :config
  (which-key-mode))

(use-package projectile
  :ensure t
  :delight "☷"
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-require-project-root nil)
  (setq projectile-project-search-path '("~/workspace/"))
  :config
  (projectile-mode t))

(use-package counsel-projectile
  :ensure t
  :delight
  :config
  (counsel-projectile-mode))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

(use-package eyebrowse
  :ensure t
  :delight "¬¬"
  :diminish eyebrowse-mode
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode t))

;; Copy/paste
;; (use-package simpleclip
;;   :ensure t
;;   :config
;;   (simpleclip-mode))

(use-package zoom-window
  :ensure t
  :config
  (setq zoom-window-mode-line-color "DarkGreen"))

;; -------------------------
;; >>>> for os
;; -------------------------
(use-package daemons :ensure t)

;; -------------------------
;; >>>> Evil mode (vim)
;; -------------------------
(use-package evil
  :ensure t
  :delight "♉"
  :init
  ;; fix for <tab> in terminal (-nw)
  (setq evil-want-C-i-jump nil)
  ;(setq evil-undo-system 'undo-redo)
  (global-undo-tree-mode)
  (setq evil-undo-system 'undo-tree)
  (evil-mode t))

(use-package evil-nerd-commenter
  :after evil
  :init
  (evilnc-default-hotkeys))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

; XXX: Takes too much time to load on other envs like WSL
;(use-package exec-path-from-shell
;  :ensure t
;  :init
;  (setq exec-path-from-shell-arguments '("-l"))
;  :config
;  (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'js-mode-hook 'add-node-modules-path))

;; -------------------------
;; >>>> Dev utils
;; -------------------------

(use-package diff-hl
  :commands global-diff-hl-mode
  :config (global-diff-hl-mode))

(use-package company
  :ensure t
  :after lsp-mode
  :delight
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations 't)
  (company--show-numbers t)
  (add-to-list 'company-backends '(company-tabnine company-capf company-dabbrev company-yasnippet))
  (setq company-files-exclusions '(".git/"))
  (global-company-mode t))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1))

(use-package company-tabnine
  :ensure t
  :config
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t))

(use-package flycheck
  :ensure t
  :delight
  :init
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :delight yas-minor-mode "ψ"
  :config
  ;(add-to-list 'company-backends '(company-yasnippet))
  (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package ivy-yasnippet
  :ensure t
  :after yasnippet)

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package magit
  :ensure t
  :config
  (setq magit-commit-show-diff nil
        magit-revert-buffers 1))

(use-package smartparens
  :ensure t
  :delight
  :hook
  (after-init . smartparens-global-mode))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.33)
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (dimmer-mode))

(use-package org-mime
  :ensure t
  :config
  (setq org-mime-library 'mml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :hook ((python-mode php-mode) . lsp)
  :custom
  (lsp-prefer-flymake nil)
  :init
  (setq-default lsp-pyls-configuration-sources ["flake8"]))

(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-include-signature t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (setq lsp-ui-sideline-enable nil)
;; lsp-ui-flycheck-live-reporting t
)

;; (use-package company-lsp
  ;; :ensure t
  ;; :config
  ;; (setq company-lsp-enable-snippet t
        ;; company-lsp-cache-candidates t)
  ;; (push 'company-lsp company-backends))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; python
(use-package python-mode
  :ensure t
  :delight "∫"
  :commands python-mode
  :mode ("\\.py\\'" . python-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; (use-package lsp-python-ms
;;   :ensure t
;;   :defer 0.3
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))
(use-package pipenv
  :ensure t
  :diminish 'pipenv-mode
  :hook (python-mode . pipenv-mode)
  :init
  (setenv "WORKON_HOME" "~/.venvs/")
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(use-package json-mode
  :ensure t
  :mode
  ("\\.json$"))

(use-package yaml-mode
  :delight "γ"
  :mode ("\\.yml$" . yaml-mode))

(use-package vimrc-mode
  :ensure t
  :mode ("^\\.vimrc\\'"))


(use-package ledger-mode
  :ensure t
  :defer t
  :mode ("\\.ledger$"))

(use-package flycheck-ledger
  :ensure t
  :after (flycheck ledger-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$"))

;; (use-package markdown-mode+
  ;; :ensure t
  ;; :after markdown-mode
  ;; :defer t)

;; php
(use-package php-mode
  :ensure t
  ;; :delight "php"
  :mode ("\\.php\\'" . php-mode))

(use-package web-mode
  :ensure t
  :commands web-mode
  :mode ("\\.html$"
         "\\.hthml?\\'"
         "\\/css\\'"
         "\\.js\\'"
         "\\.php\\'"
         "\\.hbs\\'"
         "\\.jsx\\'"
         "\\.vue\\'"
         "/\\([Vv]iews\\|[Hh]tml\\|[Tt]emplates\\)/.*\\.php\\'"
         "\\.blade\\.php\\'"))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Custom keybinding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global maps
(global-set-key (kbd "C-s") 'swiper-isearch)
;; create files easily in dired
(define-key dired-mode-map (kbd "a") 'find-file)
(put 'dired-find-alternate-file 'disabled nil) ; disables warning
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "DEL") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

(general-evil-setup)
(general-create-definer my-evil-leader-def
  :prefix ",")

(my-evil-leader-def
 :states '(normal visual emacs)
  "b"  'ivy-switch-buffer            ;; switch to buffer
  "e"  'counsel-find-file
  "i"  'highlight-indent-guides-mode
  "k"  'kill-buffer
  "n"  'linum-relative-toggle
  "p"  'projectile-find-file         ;; like ctrl-p
  "P"  'counsel-yank-pop             ;; fzf - paste
  "q"  'rgcr/kill-this-buffer        ;; close current buffer
  "Q"  'delete-other-windows         ;; close all other windows
  "r"  'rgcr/reload-init-file        ;; reload emacs config
  "s"  'whitespace-mode              ;; toggle invisible characters
  "S"  'delete-trailing-whitespace
  "u"  'undo-tree-visualize
  "w"  'save-buffer
  "x"  'rgcr/close-and-kill-this-pane ;; close and kill buffer
  "/"  'counsel-rg                    ;; fzf - ripgrep
  ","  'other-window                  ;; Switch to other window
  ;; centaur-tabs
  "1"  'awt-goto-1
  "2"  'awt-goto-2
  "3"  'awt-goto-3
  "4"  'awt-goto-4
  "5"  'awt-goto-5
  "6"  'awt-goto-6
  "7"  'awt-goto-7
  "8"  'awt-goto-8
  "9"  'awt-goto-9
  "$"  'eyebrowse-rename-windows-config
  )

;; XXX: I need a better approach for this
(defun awt-goto-1 () (interactive) (centaur-tabs-select-visible-nth-tab 1))
(defun awt-goto-2 () (interactive) (centaur-tabs-select-visible-nth-tab 2))
(defun awt-goto-3 () (interactive) (centaur-tabs-select-visible-nth-tab 3))
(defun awt-goto-4 () (interactive) (centaur-tabs-select-visible-nth-tab 4))
(defun awt-goto-5 () (interactive) (centaur-tabs-select-visible-nth-tab 5))
(defun awt-goto-6 () (interactive) (centaur-tabs-select-visible-nth-tab 6))
(defun awt-goto-7 () (interactive) (centaur-tabs-select-visible-nth-tab 7))
(defun awt-goto-8 () (interactive) (centaur-tabs-select-visible-nth-tab 8))
(defun awt-goto-9 () (interactive) (centaur-tabs-select-visible-nth-tab 9))


;; global general keys
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "SPC" '(execute-extended-command :which-key "M-x")
 "."   '(ivy-resume :which-key "Ivy Resume")
 ;; ripgrep
 "/"   '(counsel-rg :which-key "Ripgrep")
 ;; org-agenda
 "a"  '(org-agenda :which-key "ORG Agenda")'
 ;; dired
 "d"   '(counsel-dired :which-key "Dired")
 ;; eyebrowse
 "e"   '(hydra-eyebrowse/body :which-key "Eyebrowse")
 ;; flycheck
 "f"   '(hydra-flycheck/body :which-key "Flycheck")
 ;; maGit
 "g"   '(hydra-magit/body :which-key "Magit")
 ;; lsp
 "l"  '(hydra-lsp/body :which-key "LSP")
 ;; Neotree
 "n"  '(neotree-toggle :which-key "Neotree")
 ;; projectile
 "p"  '(hydra-projectile/body :which-key "Projectile")
 ;; yasnippet
 "y"   '(hydra-yasnippet/body :which-key "Yasnippet")
 ;; perspective
    ;; "x"  '(hydra-perspective/body :which-key "Perspective")
 ;; Window
 ;; "wx"  '(delete-window :which-key "delete window")
 ;; zoom
 ;; "z"   '(hydra-zoom/body :which-key "Zoom")
 )

;; fix for evil and neotree
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; evil maps
(with-eval-after-load 'evil-maps
  ;; windows
  (define-prefix-command 'evil-window-map)
  (define-key evil-window-map (kbd "C-w") 'other-window)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map "-" 'split-window-below)
  (define-key evil-window-map "|" 'split-window-right)
  ;; Ex commands
  (evil-ex-define-cmd "ls" #'ivy-switch-buffer)
  ;;; git
  (evil-ex-define-cmd "git" #'magit-status)
  (evil-ex-define-cmd "gstage" #'magit-stage)
  (evil-ex-define-cmd "gunstage" #'magit-unstage)
  (evil-ex-define-cmd "gblame" #'magit-blame)
  )

;; -------------------------
;; >>>> Hydras
;; -------------------------

;; magit
(defhydra hydra-magit (:color blue :hint nil)
  "
      Magit: %(magit-get \"remote\" \"origin\" \"url\")

^Magit^
^─────^───────────────────────────
_b_ blame   _i_ init   _s_ status
_c_ clone   _p_ pull
"
  ("s" magit-status)
  ("b" magit-blame)
  ("c" magit-clone)
  ("i" magit-init)
  ("p" magit-pull)
  ("q" nil))

;; yasnippet
(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
 ^^        _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas-global-mode)
  ("m" yas-minor-mode)
  ("a" yas-reload-all)
  ("q" nil))

;; flycheck
(defhydra hydra-flycheck
    (:pre (flycheck-list-errors)
     :post (quit-windows-on "*Flycheck errors*")
     :hint nil)
    "
  ^Flycheck^     ^Move^
  ^─────────^────^──^────────
  _f_ filter     _n_  next
  _m_ mode       _p_  previous
  _d_ disable    _gg_ first
  _?_ decribe    _G_  last

  _q_ quit
  "
  ("?" flycheck-describe-checker)
  ("d" flycheck-disable-checker)
  ("m" flycheck-mode)
  ("f" flycheck-error-list-set-filter)
  ("n" flycheck-next-error)
  ("p" flycheck-previous-error)
  ("gg" flycheck-first-error)
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)))
  ("q" nil))


;; lsp
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^              Symbol
------------------------------------------------------------------------------------
_f_ format             _d_ declaration    _i_ implementationa    _h_ documentation
_x_ execute action     _D_ definition     _t_ type               _n_ rename
^^                     _r_ references     _s_ signature

LSP Actions:
    [_R_] Restart    [_S_] Shutdown     [_I_] Session info
"
  ("f" lsp-format-buffer)
  ("x" lsp-execute-code-action)
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("r" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("h" lsp-describe-thing-at-point)
  ("n" lsp-rename)
  ("I" lsp-describe-session)
  ("R" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))

;; projectile
(defhydra hydra-projectile (:color teal :hint nil :exit t)
"
         PROJECTILE: %(projectile-project-root)

  ^Search^                ^Buffers^             ^Cache^
  ^^───────────────────────^^───────────────────^^─────────────────────
  _f_ Find file           _b_ Switch buffer     _z_ Cache current File
  _p_ Fzf in projects     _k_ Kill buffers      _c_ Clear cache
  _r_ Recent file
  _d_ Find directory
  _o_ Multi ocur

  Actions for Projects:
      [_s_] Switch    [_a_] Add    [_x_] Remove    [_X_] Cleanup

  [_q_]: Cancel
"
  ("s"   projectile-switch-project)
  ("a"   projectile-add-known-project)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("f"   projectile-find-file)
  ("p"   projectile-find-file-in-known-projects)
  ("r"   projectile-recentf)
  ("d"   projectile-find-dir)
  ("o"   projectile-multi-occur)
  ("b"   projectile-switch-to-buffer)
  ("k"   projectile-kill-buffers)
  ("z"   projectile-cache-current-file)
  ("c"   projectile-invalidate-cache)
  ("q"   nil :color red)
  )

(defhydra hydra-eyebrowse (:color teal :hint nil :exit t)
  ("1" eyebrowse-switch-to-window-config-1 "ws1")
  ("2" eyebrowse-switch-to-window-config-2 "ws2")
  ("3" eyebrowse-switch-to-window-config-3 "ws3")
  ("4" eyebrowse-switch-to-window-config-4 "ws4")
  ("5" eyebrowse-switch-to-window-config-5 "ws5")
  ("6" eyebrowse-switch-to-window-config-6 "ws6")
  ("7" eyebrowse-switch-to-window-config-7 "ws7")
  ("8" eyebrowse-switch-to-window-config-8 "ws8")
  ("9" eyebrowse-switch-to-window-config-9 "ws9")
  )
