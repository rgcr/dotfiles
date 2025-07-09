;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => OPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-if-not-loaded "functions.el")

;; Fixes backendspace in terminal
(normal-erase-is-backspace-mode 0)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; Maximize on start
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Use always y-or-n instead yes-or-not
(defalias 'yes-or-no-p 'y-or-n-p)


;; Escape key to quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Font and Frame size
(if (display-graphic-p)
    (add-to-list 'default-frame-alist '(font . "Hack-10"))
  (add-to-list 'default-frame-alist '(font . "Hack-8")))


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
(setq select-enable-clipboard t)


;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(show-paren-mode 1)
(electric-pair-mode 1)
(column-number-mode t)


;; When open a file follow symlinks
(setq vc-follow-symlinks t)

;; Single space after period
(setq sentence-end-double-space nil)

;; Human readable file sizes in dired buffers
(setq-default dired-listing-switches "-alh")

;; Delete selection mode, if some text is selected, it will be replaced
(delete-selection-mode t)

;; Turn on syntax highlighting
(global-font-lock-mode t)

;; Refresh the buffer when the file changes on disk
(global-auto-revert-mode t)

;; Visually indicate matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; Flash the screen instead of beeping
(setq visible-bell t)

;; Set the default line length
;; (setq-default fill-column 80)

;; Somewhat better formatting when showing line numbers, but still...
;; (setq linum-format "%4d \u2502 ")
;; display-line-numbers-mode only for programming modes
(setq display-line-numbers-type 'relative) ; relative line numbers
(setq display-line-numbers-width 3) ; width of line numbers
(setq display-line-numbers-widen t) ; widen line numbers to fill the window
;; Enable line numbers in all programming modes
(dolist (mode '(prog-mode-hook
                 text-mode-hook
                 conf-mode-hook
                 org-mode-hook))
  (add-hook mode 'display-line-numbers-mode))
;; Enable line numbers globally
;; (global-display-line-numbers-mode t)

;; (global-hl-line-mode 1)
;; (set-face-background ‘highlight “#222222”)


;; Quiet startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      inhibit-startup-echo-area-message t)


;; Turn off autosave and backup files
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Dont resize frame
(setq frame-inhibit-implied-resize t)

;;  Disable scroll bars in minibuffer
(set-window-scroll-bars (minibuffer-window) nil nil)

;;
(when (display-graphic-p)
  (set-fringe-style '(8 . 8)))  ;; 8px left and right fringe
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


;; Highlight the current line
(global-hl-line-mode)

;; Cursor color
(set-cursor-color "#ffffff")

(set-face-background hl-line-face "color-235")

;; Call DELETE-TRAILING-WHITESPACE every time a buffer is saved."
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Treat CamelCase as separate words
(add-hook 'prog-mode-hook 'subword-mode)

;; display 'warnings' as a file
;; (add-to-list 'display-buffer-alist
             ;; '("\\*Warnings\\*"
               ;; (display-buffer-same-window)))

;; remove buffer list and warnings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (dolist (win (window-list))
              (let ((name (buffer-name (window-buffer win))))
                (when (or (string= name "*Buffer List*")
                          (string= name "*Warnings*"))
                  (delete-window win))))))
