;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => PLUGINS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; -------------------------
;; >>>> IU
;; -------------------------

(use-package recentf
  :ensure nil
  :delight
  :init
  (recentf-mode t)
  :config
  (setq recentf-max-saved-items 100 ;; Keep 100 recent files
      recentf-auto-cleanup 'never   ;; Don’t auto-cleanup on exit
      recentf-exclude
      '("\\.git/.*"
        "/tmp/"
        "\\.cache"
        "\\.ido\\.last"
        "\\.revive"
        "\\.gz$"
        "/ssh:"                     ;; Exclude remote files
        ))
  )

;; Themes
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
  :config
  (load-theme 'doom-Iosvkem t)
  (doom-themes-neotree-config))


;; Status line
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow)
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-version-control-on)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-id-on)
  (spaceline-toggle-evil-state-on)
  (spaceline-toggle-selection-info-on)
  (spaceline-toggle-which-function-on)
  )

;; Icons
(use-package nerd-icons
  :ensure t)

;; Hide minor modes
(use-package minions
  :ensure t
  :hook (after-init . minions-mode)
  :config
  (setq minions-mode-line-lighter "☰")
  (minions-mode t))


(use-package color-identifiers-mode
    :ensure t
    :delight
    :config
    :hook (prog-mode . color-identifiers-mode))


;; Tabs
(use-package centaur-tabs
  :ensure t
  :delight
  :demand
  :config
  ;; (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-bar t
        centaur-tabs-set-modified-marker t
        centaur-tabs-height 32
        centaur-tabs-set-bar 'over
        centaur-tabs-modified-marker "*"
        centaur-tabs-set-icons t
        centaur-tabs-show-count t
        centaur-tabs-icon-type 'nerd-icons)

  (set-face-foreground 'centaur-tabs-unselected "brightblack")
  (set-face-foreground 'centaur-tabs-selected "cyan")
  (centaur-tabs-mode)
  )


;; Highlight TODOs
(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)  ; enable in all programming modes
         (text-mode . hl-todo-mode)) ; or in text modes too
  :init
  (setq hl-todo-highlight-punctuation ":")
  ;; :config
  ;; (global-set-key (kbd "C-c t") 'hl-todo-occur)) ;; list all todos in buffer
  )

(use-package hide-mode-line
  :ensure t
  :delight
  :hook ((neo-tree-mode . hide-mode-line-mode)
         (dashboard-mode . hide-mode-line-mode)
         (helpful-mode . hide-mode-line-mode))
  )


;; -------------------------
;; >>>> Utils
;; -------------------------


;; Customize the mode line
(use-package delight
  :commands delight
  :config
    (delight '((eldoc-mode "⌘" t)
                 (auto-revert-mode "⟳" t)
                 (abbrev-mode "⍟" t)
                 (undo-tree-mode "↺" t)
                 (visual-line-mode "↵" t)
                 (yas-minor-mode "✎" t)
                 ;; (company-mode "☁" t)
                 (rainbow-delimiters-mode "◉" t)
                 (highlight-indent-guides-mode "≡" t)
                 ;; (ivy-mode "🔍" t)
                 (irg-bullets-mode "•" t)))
  )


;; Undo tree mode
(use-package undo-tree
  :ensure t
  :delight
  :init
  (undo-tree-mode))


;; eldoc mode
(use-package eldoc
  :config (add-hook 'prog-mode-hook 'eldoc-mode))

;; clipboard integration
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))


;; -------------------------
;; >>>> Completion
;; -------------------------


;; Completion styles
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion))
     (snippets (styles basic)))
   ))


;; Completion UI
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom (vertico-cycle t))


;; Annotate minibuffer entries
(use-package marginalia
  :ensure t
  :init (marginalia-mode))


;; In-buffer completion UI
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)
  :bind
  (:map corfu-map
        ;; ("C-n" . corfu-next)
        ;; ("C-p" . corfu-previous)
        ;; ("C-d" . corfu-show-documentation)
        ;; ("C-l" . corfu-complete)
        ("TAB" . corfu-next)
        ("S-TAB" . corfu-previous))
  :init
  (global-corfu-mode))


;; Icons for Corfu completions
(use-package kind-icon
  :ensure t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;; Navigation and Search
(use-package consult
  :ensure t
  :after evil
  :custom
  (consult-buffer-filter '("\\*Buffer List\\*" "\\*Ibuffer\\*" "\\*Buffer Menu\\*"))
  :config
  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-buffer
          consult--source-recent-file
          consult--source-project-buffer))
  )

;; Embark - act on minibuffer items
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))


;; Embark integration with Consult
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; Cape for integrating yasnippet into CAPF
(use-package cape
  :ensure t
  :init
  ;; Add cape-yasnippet to capf
  (setq completion-at-point-functions
        (list
         #'cape-yasnippet           ;; 1. Snippets
         #'cape-dabbrev             ;; 2. Buffer words
         #'cape-keyword             ;; 3. Language keywords
         #'cape-file                ;; 4. File paths
         #'cape-symbol              ;; 5. Symbols
         #'lsp-completion-at-point  ;; 6. LSP
         ))
  )



;; Snippets
(use-package yasnippet
  :ensure t
  :delight ""
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))


(use-package yasnippet-snippets
  :ensure t
  :delight ""
  :after yasnippet)


;; LSP
(use-package lsp-mode
  :ensure t
  :delight ""
  :commands lsp
  :hook (prog-mode . lsp)
  :init
  (setq lsp-completion-provider :none ;;:capf
        lsp-enable-snippet t
        lsp-enable-symbolic-highlighting t))


;; lsp-ui for better visuals
(use-package lsp-ui
  :ensure t
  :delight
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :requires lsp-mode flycheck
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-include-signature t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
        lsp-ui-sideline-enable t
        )
  )

;; Copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook ((prog-mode . copilot-mode))
  )

(defun my/copilot-tab ()
  "Smart fallback: yasnippet > cape > LSP > Copilot > indent."
  (interactive)
  (unless (or (copilot-accept-completion)
              (completion-at-point))
    (indent-for-tab-command)))

;; (with-eval-after-load 'copilot
  ;; (define-key copilot-mode-map (kbd "TAB") #'my/copilot-tab)
  ;; (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab))


;; -------------------------
;; >>>> Org
;; -------------------------


;; org
(use-package org
  :ensure nil
  :delight
  :mode ("\\.org\\'" . org-mode)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)

  :config
  (setq org-icalendar-timezone "America/Monterrey")
  (setq org-startup-indented t          ;; indent text and headings
        org-hide-leading-stars t        ;; hide leading stars for clean look
        org-cycle-separator-lines 1     ;; number of lines to show when cycling
        org-deadline-warning-days 30)
  ;; When a TODO item enters DONE, add a CLOSED: property with current date-time stamp and into drawer
  (setq org-log-done 'time)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-directory "~/org"                ;; default org files directory)
        org-agenda-files '("~/org/agenda"))  ;; files for agenda

  ;; (setq org-agenda-files
  ;;       '("~/.onedrive/org/personal.org"
  ;;         "~/.onedrive/org/work.org"
  ;;         "~/.onedrive/org/diary.org"))

   ;; I use my own diary file
   ;; (setq org-agenda-include-diary nil)
   ;; (setq org-archive-location "~/.onedrive/archive/%s_archive.org::datetree/")

   (setq org-todo-keywords
         '((sequence "TODO(t!)"
                     "IN-PROGRESS(p!)"
                     "BLOCKED(b!)"
                     "|" "DONE(d!)" "CANCELLED(c!)" "ARCHIVE(a!)")))

   (setq org-todo-keyword-faces
         '(("TODO" . "orange")
           ("IN PROCESS" . "yellow")
           ("BLOCKED" . "red")
           ("DONE" . "green")
           ("CANCELLED" .  "red")
           ("ARCHIVE" .  "blue")))

   ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  )

;; Org modern style
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :init
  (setq
   ;; Heading bullets, similar to org-bullets
   org-modern-star '("◉" "○" "✿" "✸" "✦")

   ;; Other optional visuals
   org-modern-hide-stars t
   org-modern-table t
   org-modern-list '((43 . "➤") (45 . "–") (42 . "•")) ;; + - * symbols
   org-modern-block-fringe nil
   org-modern-label-border 1)
  :config
  ;; Also make sure indentation looks good
  (add-hook 'org-mode-hook #'org-indent-mode))

;; Autolist for org-mode
(use-package org-autolist
  :ensure t
  :hook (org-mode . org-autolist-mode))


;; -------------------------
;; >>>> Navigation and Keybindings
;; -------------------------
(use-package dired
  :ensure nil  ;; built-in package
  :commands (dired dired-jump)
  :hook (dired-mode . dired-omit-mode)
  :config
  (require 'dired-x)
)

(use-package dired-narrow
  :ensure t)

;; Hydra
(use-package hydra
  :ensure t
  :delight)


;; Custom keybindings
(use-package general
  :ensure t
  :delight)


;; Switch to window with ace-window
(use-package ace-window
  :ensure t
  :defer t
  :delight
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "red"
                      :height 2.0
                      :weight bold)))))
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
         aw-scope 'frame
         aw-background t)
   )


;; Which Key
(use-package which-key
  :ensure t
  :delight
  :init
  ;; (setq which-key-popup-type 'minibuffer)
  ;; (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-sort-uppercase-first nil)
  (setq which-key-min-display-lines 6)
  :config
  (which-key-mode))


;; Project management
(use-package projectile
  :ensure t
  :delight
  :init
  (setq projectile-completion-system 'default)
  (setq projectile-require-project-root nil)
  (setq projectile-project-search-path '("~/workspace/"))
  :config
  (projectile-mode t))


(use-package neotree
  :ensure t
  :delight
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-follow-symlink t
        neo-show-hidden-files t
        neo-smart-open t)
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
  (setq evil-want-C-i-jump nil) ;; fix for <tab> in terminal (-nw)

  (setq evil-undo-system 'undo-tree)
  (global-undo-tree-mode)

  (setq select-enable-clipboard t)
  (setq save-interprogram-paste-before-kill t)
  :config
  (evil-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :delight
  :config
  (setq evilnc-use-comment-style-indicator-flag nil)
  (evilnc-default-hotkeys))

(use-package evil-goggles
  :ensure t
  :delight
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;  (evil-collection-init))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'js-mode-hook 'add-node-modules-path))


;; -------------------------
;; >>>> Dev utils
;; -------------------------


(use-package git-gutter
  :ensure t
  :delight
  :diminish
  :hook (prog-mode . git-gutter-mode)  ;; enable git-gutter in programming modes
  :config
  ;; Customize signs (optional)
  (setq git-gutter:modified-sign "~")   ;; sign for modified lines
  (setq git-gutter:added-sign "+")      ;; sign for added lines
  (setq git-gutter:deleted-sign "-")    ;; sign for deleted lines

  ;; Set faces for the signs (optional)
  (set-face-foreground 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:added    "green")
  (set-face-foreground 'git-gutter:deleted  "red")

  ;; Update signs automatically
  (global-git-gutter-mode +1))


(use-package flycheck
  :ensure t
  :delight
  :init
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode))


(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :delight
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|) ;; terminal fallback
  (setq highlight-indent-guides-responsive 'top)

  (setq highlight-indent-guides-auto-enabled nil)      ;; Don't auto-switch method
  (set-face-foreground 'highlight-indent-guides-character-face "brightblack")
  (set-face-background 'highlight-indent-guides-character-face nil)
  )

(use-package rainbow-delimiters
  :ensure t
  :delight
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package magit
  :ensure t
  :delight
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
  :delight
  :config
  (setq dimmer-fraction 0.33)
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (dimmer-mode))

(use-package org-mime
  :ensure t
  :delight
  :config
  (setq org-mime-library 'mml))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; python
;; (use-package python-mode
;;   :ensure t
;;   :commands python-mode
;;   :mode ("\\.py\\'" . python-mode))
;;
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred
;;
;; (use-package lsp-python-ms
;;   :ensure t
;;   :defer 0.3
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))

;; (use-package pipenv
;;   :ensure t
;;   :diminish 'pipenv-mode
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setenv "WORKON_HOME" "~/.venvs/")
;;   (setq pipenv-projectile-after-switch-function
;;         #'pipenv-projectile-after-switch-extended))

(use-package json-mode
  :ensure t
  :mode
  ("\\.json$"))

(use-package yaml-mode
  :ensure t
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
