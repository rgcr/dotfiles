;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HOME and END keys
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; Dired keybindings
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "a") 'find-file)
  (put 'dired-find-alternate-file 'disabled nil) ; disables warning
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "DEL") (lambda () (interactive) (find-alternate-file "..")))  ;dired-up-directory
  (define-key dired-mode-map (kbd "-") (lambda () (interactive) (find-alternate-file "..")))  ; dired-up-directory
  (define-key dired-mode-map (kbd "m") #'dired-mark)
  (define-key dired-mode-map (kbd "M") #'dired-mark-all-files)
  (define-key dired-mode-map (kbd "u") #'dired-unmark)
  (define-key dired-mode-map (kbd "U") #'dired-unmark-all-marks)
  (define-key dired-mode-map (kbd "D") #'dired-do-delete) ; delete all marked files
  (define-key dired-mode-map (kbd "Y") #'dired-do-copy)
  (define-key dired-mode-map (kbd "R") #'dired-do-rename)
  (define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "!") #'dired-do-shell-command)
  (define-key dired-mode-map (kbd "/") #'dired-narrow)

  (define-key dired-mode-map (kbd "S")
  (lambda () (interactive)
    (select-window (split-window-right))
    (find-file (dired-get-file-for-visit))))

  (define-key dired-mode-map (kbd "C-t")
   (lambda ()
     (interactive)
     (select-window (split-window-below))
     (if (fboundp 'vterm)
         (vterm)
       (term (getenv "SHELL")))))
)

;; General keybindings
(general-evil-setup)

;; Evil leader ','
(general-create-definer my-evil-leader-def
  :prefix ",")

;; General leader 'SPC'
(general-create-definer my-general-leader-def
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(my-evil-leader-def
 :states '(normal visual emacs)
  "b"  'consult-buffer
  "e"  'consult-find
  "i"  'highlight-indent-guides-mode
  "k"  'kill-buffer
  "n"  'global-display-line-numbers-mode
  ;; "p"  'projectile-find-file         ;; like ctrl-p
  "P"  'consult-yank-pop             ;; fzf - paste
  "q"  'rgcr/kill-this-buffer        ;; close current buffer
  ;; "Q"  'delete-other-windows         ;; close all other windows
  "Q"   'kill-emacs
  "r"  'rgcr/reload-init-file        ;; reload emacs config
  "s"  'whitespace-mode              ;; toggle invisible characters
  "S"  'delete-trailing-whitespace
  "u"  'undo-tree-visualize
  "w"  'save-buffer
  "x"  'rgcr/close-and-kill-this-pane ;; close and kill buffer
  "/"  'consult-ripgrep
  ","  'other-window                  ;; Switch to other window
  "z"  'rgcr/toggle-zoom-window               ;; zoom window
  ;; centaur-tabs
  "1" '(lambda () (interactive) (awt-goto-tab 1))
  "2" '(lambda () (interactive) (awt-goto-tab 2))
  "3" '(lambda () (interactive) (awt-goto-tab 3))
  "4" '(lambda () (interactive) (awt-goto-tab 4))
  "5" '(lambda () (interactive) (awt-goto-tab 5))
  "6" '(lambda () (interactive) (awt-goto-tab 6))
  "7" '(lambda () (interactive) (awt-goto-tab 7))
  "8" '(lambda () (interactive) (awt-goto-tab 8))
  "9" '(lambda () (interactive) (awt-goto-tab 9))
  "$"  'eyebrowse-rename-windows-config
  )

(defun awt-goto-tab (n)
  "Switch to the nth centaur tab."
  (interactive)
  (centaur-tabs-select-visible-nth-tab n))


;; General leader 'SPC'
(my-general-leader-def
 :states '(normal visual insert emacs)
 "SPC" '(execute-extended-command :which-key "M-x")
 "."   '(ivy-resume :which-key "Ivy Resume")
 ;; ripgrep
 "/"   '(counsel-rg :which-key "Ripgrep")
 ;; org-capture
 "c"  '(org-capture :which-key "ORG Capture")
 ;; close active call
 "x"  '(my/close-active-call :which-key "Close Active Call")
 ;; dired
 "d"   '(dired-jump   :which-key "Dired")
 ;; eyebrowse
 ;; "e"   '(hydra-eyebrowse/body :which-key "Eyebrowse")
 "e"   '(hydra-eyebrowse/body :which-key "Eyebrowse")
 ;; flycheck
 "f"   '(hydra-flycheck/body :which-key "Flycheck")
 ;; maGit
 "g"   '(rgcr/magit-hydra :which-key "Magit")
 ;; lsp
 "l"  '(hydra-lsp/body :which-key "LSP")
 ;; Neotree
 "n"  '(neotree-toggle :which-key "Neotree")
 ;; projectile
 "p"  '(rgcr/projectile-hydra :which-key "Projectile")
 ;; yasnippet
 "y"   '(hydra-yasnippet/body :which-key "Yasnippet")
 )


;; Evil Maps
;; fix for evil and neotree
(with-eval-after-load 'evil-maps
  (evil-define-key 'normal neotree-mode-map
    (kbd "RET") 'neotree-enter
    (kbd "TAB") 'neotree-quick-look
    (kbd "S-TAB") 'neotree-collapse-all
    (kbd "DEL") 'neotree-select-up-node
    (kbd ".")   'neotree-change-root
    (kbd "q")   'neotree-hide
    (kbd "R")   'neotree-refresh
    (kbd "a")   'neotree-create-node
    (kbd "d")   'neotree-delete-node
    (kbd "r")   'neotree-rename-node
    (kbd "H")   'neotree-hidden-file-toggle
    (kbd "n")   'neotree-next-line
    (kbd "p")   'neotree-previous-line)

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
  (evil-ex-define-cmd "ls" #'consult-buffer)

  ;;; git
  (evil-ex-define-cmd "gstatus" #'magit-status)
  (evil-ex-define-cmd "gst" #'magit-status)
  (evil-ex-define-cmd "gstage" #'magit-stage)
  (evil-ex-define-cmd "gunstage" #'magit-unstage)
  (evil-ex-define-cmd "gblame" #'magit-blame)

  ;; consult
  (evil-global-set-key 'normal (kbd "C-s") #'consult-line)
  (evil-global-set-key 'normal (kbd "C-f") #'consult-line)

  (evil-global-set-key 'normal (kbd "C-t") (lambda ()
    (interactive)
    (select-window (split-window-below))
    (if (fboundp 'vterm)
        (vterm)
      (term (getenv "SHELL")))))
)

;; copilot
(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "M-RET") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "M-l") 'copilot-accept-completion-by-line)
  (define-key copilot-completion-map (kbd "M-q") 'copilot-clear-overlay)
)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd ",ci") #'comment-line) ;; normal mode
  (define-key evil-visual-state-map (kbd ",ci") #'comment-or-uncomment-region) ;; visual mode

;; ctrl + p -> projectile-find-file
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-insert-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-visual-state-map (kbd "C-p") 'projectile-find-file)
)

;; ctrl-p global binding as fallback
(global-set-key (kbd "C-p") 'projectile-find-file)


;; -------------------------
;; >>>> Hydras
;; -------------------------

;; Wrapper functions to load hydras
;; Because magit/projectile are being loaded lazily
(defun rgcr/projectile-hydra ()
  "Load projectile and show hydra."
  (interactive)
  (require 'projectile)
  (projectile-mode 1)
  ;; Force projectile to detect current project
  (when (projectile-project-p)
    (projectile-project-root))
  (hydra-projectile/body))

(defun rgcr/magit-hydra ()
  "Load magit and show hydra."
  (interactive)
  (require 'magit)
  (hydra-magit/body))

;; Magit
(with-eval-after-load 'magit
  (defhydra hydra-magit (:color blue :hint nil)
    "
 Repo:  %(if (fboundp 'magit-get) (magit-get \"remote\" \"origin\" \"url\") \"<Not in git repo>\")
^─────^───────────────────────────────────────────^
^
^Magit^
^
_s_tatus   _b_ranch  _f_etch  _p_ull
^
_c_ommit   _d_iff     _l_og    _P_ush
^
^
_q_: Quit
"
  ("s" magit-status)
  ("b" magit-branch)
  ("f" magit-fetch)
  ("p" magit-pull)
  ("c" magit-commit)
  ("d" magit-diff)
  ("l" magit-log)
  ("t" magit-show-commit-tree)
  ("P" magit-push)
  ("t" magit-tree)
  ("q" nil)
  ))


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


;; lsp hydra - works whether LSP is loaded or not
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^              Symbol
------------------------------------------------------------------------------------
_f_ format             _d_ declaration    _i_ implementationa    _h_ documentation
_c_ code action        _D_ definition     _t_ type               _n_ rename
_e_ List errors        _r_ references     _s_ signature

^^


LSP Actions:
    [_S_] Start LSP  [_R_] Restart    [_X_] Shutdown     [_I_] Session info
"
  ("f" lsp-format-buffer)
  ("c" lsp-execute-code-action)
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("e" flycheck-list-errors)
  ("r" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("h" lsp-describe-thing-at-point)
  ("n" lsp-rename)
  ("S" lsp) ;; START LSP
  ("I" lsp-describe-session)
  ("R" lsp-restart-workspace)
  ("X" lsp-workspace-shutdown)
  )


;; projectile
(with-eval-after-load 'projectile
  (defhydra hydra-projectile (:color teal :hint nil :exit t)
"
         PROJECTILE

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
  ))

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
