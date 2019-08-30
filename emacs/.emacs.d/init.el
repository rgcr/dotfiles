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
(electric-pair-mode 1)
(column-number-mode t)


(global-linum-mode t)

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

;; ;; keep the list of recent files
(setq recentf-max-saved-items 200)
(setq recentf-max-menu-items 200)
(recentf-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => global hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => file type overrides (modes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.restc$" . restclient-mode))


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

;; -------------------------
;; >>>> UI
;; -------------------------

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

(use-package hl-todo
  :ensure t
  :init
  (setq hl-todo-highlight-punctuation ":")
  )

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
  ;; :config
  ;; (doom-themes-neotree-config)
  )

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

;; -------------------------
;; >>>> General utils
;; -------------------------

;; Use double key combination
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

;; Make sure that delight is available as soon as any package triggers it.
(use-package delight
  :commands delight)


;; org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure t)
(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Emacs package from source
(use-package quelpa
  :ensure t)

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
  :ensure t)

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
  :delight "pj"
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
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

;; Copy/paste
;; (use-package simpleclip
;;   :ensure t
;;   :config
;;   (simpleclip-mode))


;; -------------------------
;; >>>> Evil mode (vim)
;; -------------------------

(use-package evil
  :ensure t
  :init
  ;; fix for <tab> in terminal (-nw)
  (setq evil-want-C-i-jump nil)
  (evil-mode t))

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :init
  :config
  (progn
    (evil-leader/set-leader ","))
  (global-evil-leader-mode)
)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
  :delight
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

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
  :delight yas-minor-mode "ys"
  :diminish yas-minor-mode)
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-global-mode t))
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
  :ensure t)


(use-package smartparens
  :ensure t
  :hook
  (after-init . smartparens-global-mode))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.33)
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (dimmer-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Langs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; python
(use-package python-mode
  :ensure t
  :delight "π "
  :commands python-mode
  :mode ("\\.py\\'" . python-mode))

(use-package lsp-python-ms
  :ensure t
  :defer 0.3
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

;; (use-package pipenv
;;   :ensure t
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))


;; php
(use-package phpactor :ensure t)
(use-package company-phpactor :ensure t)
(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :hook ((php-mode . (lambda () (set (make-local-variable 'company-backends)
       '(;; list of backends
         company-phpactor
         company-files
         )))))
  :init
  (add-hook 'php-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function
                  'phpactor-hover))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :hook ((python-mode) . lsp)
  :custom
  (lsp-prefer-flymake nil)
  ;; (setq-default lsp-pyls-configuration-sources ["flake8"]))
  )

(use-package lsp-ui)
(use-package company-lsp)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => langs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Custom keybinding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global maps
(global-set-key (kbd "C-s") 'swiper-isearch)

;; global evil maps, leader => ","
(evil-leader/set-key
  "b"  'ivy-switch-buffer            ;; Switch to buffer
  "ci" 'rgcr/comment-dwim            ;; Comment/Uncomment
  "e"  'find-file
  "i"  'highlight-indent-guides-mode
  "n"  'neotree-toggle               ;; Open file explorer (sidebar)
  "p"  'counsel-yank-pop             ;; fzf - paste
  "q"  'rgcr/kill-this-buffer        ;; Close current buffer
  "Q"  'delete-other-windows         ;; Close all other windows
  "R"  'rgcr/reload-init-file        ;; Reload emacs config
  "s"  'whitespace-mode              ;; Toggle invisible characters
  "S"  'delete-trailing-whitespace
  "u"  'undo-tree-visualize
  "w"  'save-buffer
  "x"  'rgcr/close-and-kill-this-pane ;; close and kill buffer
  "/"  'counsel-rg                    ;; fzf - ripgrep
  ","  'other-window                  ;; Switch to other window
  )

;; evil leader for mode
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode
  ;; "b" 'byte-compile-file)


;; global general keys
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "SPC" '(execute-extended-command :which-key "M-x")
 "/"   '(counsel-rg :which-key "Ripgrep")
 ;; flycheck
 "f"   '(hydra-flycheck/body :which-key "Flycheck")
 ;; Git
 "g"   '(hydra-magit/body :which-key "Magit")
 ;; lsp
 "l"  '(hydra-lsp/body :which-key "LSP")
 ;; Neotree
 "n"  '(neotree-toggle :which-key "Neotree")
 ;; projectile
 "p"  '(hydra-projectile/body :which-key "Projectile")
 ;; yasnippet
 "y"   '(hydra-yasnippet/body :which-key "Yasnippet")
 ;; Window
 ;; "wx"  '(delete-window :which-key "delete window")
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
  ;;; general
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

;; yasnippet
(defhydra hydra-magit (:color blue)
  "
^
^Magit^             ^Do^
^─────^─────────────^──^────────────────
_q_ quit            _b_ blame
^^                  _c_ clone
^^                  _i_ init
^^                  _s_ status
^^                  ^^
"
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-clone)
  ("i" magit-init)
  ("s" magit-status))


;; yasnippet
(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
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
  ^
  ^Flycheck^          ^Do^
  ^─────────^──────────^──^────────
  _q_ quit             _?_ describe
  ^^                   _d_ disable
  ^^                   _m_ mode
  ^^                   _f_ filter
  ^^                   _n_ next
  ^^                   _p_ previous
  ^^                   _gg_ first
  ^^                   _G_ last
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
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_h_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("h" lsp-describe-thing-at-point)
  ("r" lsp-rename)
  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

;; projectile
(defhydra hydra-projectile (:color teal :hint nil
			    :columns 4)
  "
PROJECTILE: %(projectile-project-root)
"
  ("f"   projectile-find-file                "Find File")
  ("r"   projectile-recentf                  "Recent Files")
  ("z"   projectile-cache-current-file       "Cache Current File")
  ("x"   projectile-remove-known-project     "Remove Known Project")

  ("d"   projectile-find-dir                 "Find Directory")
  ("b"   projectile-switch-to-buffer         "Switch to Buffer")
  ("c"   projectile-invalidate-cache         "Clear Cache")
  ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

  ("o"   projectile-multi-occur              "Multi Occur")
  ("s"   projectile-switch-project           "Switch Project")
  ("k"   projectile-kill-buffers             "Kill Buffers")
  ("q"   nil "Cancel" :color blue))

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
    (ivy-yasnippet yasnippet-snippets php-mode switch-window org-bullets dracula-theme zenburn-theme vimrc-mode use-package-chords spaceline simpleclip rainbow-delimiters projectile popup monokai-theme molokai-theme magit ibuffer-vc highlight-indent-guides git-gutter-fringe fzf flycheck exec-path-from-shell evil-nerd-commenter evil-leader evil-goggles evil-commentary doom-themes dimmer counsel atom-one-dark-theme add-node-modules-path)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
