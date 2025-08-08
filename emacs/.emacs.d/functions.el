
;; => FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ruff fix functions
(defun rgcr/ruff-check-buffer ()
  "Run ruff check on current buffer and show results in a split buffer."
  (interactive)
  (if (not (buffer-file-name))
      (message "Buffer is not visiting a file")
    (save-buffer)
    (message "Running ruff check on %s..." (buffer-file-name))
    ;; Use shell-command which automatically shows output in a split buffer
    (let ((exit-code (shell-command
                      (format "ruff check %s 2>&1"
                             (shell-quote-argument (buffer-file-name))))))
      (if (= exit-code 0)
          (message "Ruff: No issues found")
        (progn
          (message "Ruff: Issues found - see output buffer")
          ;; Set up Evil keybindings in the shell command output buffer
          (when (get-buffer "*Shell Command Output*")
            (with-current-buffer "*Shell Command Output*"
              (evil-local-set-key 'normal (kbd "<escape>")
                                (lambda ()
                                  (interactive)
                                  (quit-window t)))
              (evil-local-set-key 'normal (kbd "q")
                                (lambda ()
                                  (interactive)
                                  (quit-window t))))))))))

(defun rgcr/ruff-format-buffer ()
  "Run ruff format on current buffer."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)
    ;; Temporarily disable auto-save and LSP timers
    (let ((auto-save-default nil)
          (before-save-hook nil)
          (after-save-hook nil))
      (with-temp-message "Running ruff format..."
        (let ((exit-code (call-process "ruff" nil nil nil "format" (buffer-file-name))))
          (revert-buffer t t t)  ; quiet revert
          (if (= exit-code 0)
              (message "Ruff formatting applied")
            (message "Ruff formatting failed")))))))

;; Startup performance measurement
(defun rgcr/display-startup-time ()
  "Display startup time in echo area."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'rgcr/display-startup-time)

;; Copilot status command
(defun rgcr/copilot-status ()
  "Display current Copilot status and provide quick actions."
  (interactive)
  (let ((status (if (bound-and-true-p copilot-mode) "enabled" "disabled"))
        (buffer-name (buffer-name)))
    (message "Copilot is %s in buffer '%s'" status buffer-name)
    ;; Show additional info if available
    (when (bound-and-true-p copilot-mode)
      (if (fboundp 'copilot--overlay-visible)
          (message "Copilot: %s (suggestions: %s)"
                   status
                   (if (copilot--overlay-visible) "active" "none"))
        (message "Copilot: %s" status)))))

;;(define-prefix-command 'rgcr/help-map)
;; (add-hook 'eshell-mode 'rgcr/hide-fringes)

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

;; xclip functions
(defun rgcr/copy-to-clipboard-xclip (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xclip" "*Messages*" "xclip" "-selection" "clipboard")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun rgcr/paste-from-clipboard-xclip ()
  (shell-command-to-string "xclip -selection clipboard -o"))

;; xsel functions
(defun rgcr/copy-to-clipboard-xsel (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel" "*Messages*" "xsel" "--clipboard" "--input")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun rgcr/paste-from-clipboard-xsel ()
  (shell-command-to-string "xsel --clipboard --output"))

;; cut/paste integration
(cond
 ((and (not (display-graphic-p))
       (executable-find "xclip"))
  (setq interprogram-cut-function #'rgcr/copy-to-clipboard-xclip
        interprogram-paste-function #'rgcr/paste-from-clipboard-xclip))
 ((and (not (display-graphic-p))
       (executable-find "xsel"))
  (setq interprogram-cut-function #'rgcr/copy-to-clipboard-xsel
        interprogram-paste-function #'rgcr/paste-from-clipboard-xsel)))

;; toogle window zoom
(defvar rgcr/zoomed-window-config nil
  "Stores the window configuration before zooming.")

(defun rgcr/toggle-zoom-window ()
  "Toggle between zoomed and normal window layout.
When zoomed, maximize current window. When unzoomed, restore previous layout."
  (interactive)
  (if rgcr/zoomed-window-config
      ;; Currently zoomed - restore layout
      (progn
        (set-window-configuration rgcr/zoomed-window-config)
        (setq rgcr/zoomed-window-config nil)
        (message "Window unzoomed"))
    ;; Not zoomed - save layout and zoom
    (progn
      (setq rgcr/zoomed-window-config (current-window-configuration))
      (delete-other-windows)
      (message "Window zoomed"))))
