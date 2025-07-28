;; Performance optimizations
(setq gc-cons-threshold (* 50 1000 1000)) ;; 50mb
(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Define base directory for config files
(defconst my-config-dir (file-name-as-directory user-emacs-directory))

;; Helper to load file only once
(defun load-if-not-loaded (filename)
  "Load FILENAME from my-config-dir if not loaded yet."
  (let ((file (expand-file-name filename my-config-dir)))
    (unless (assoc file load-history)
      (load file))))


;; Package management - defer until needed
;; (package-initialize)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Always ensure packages are installed
(setq use-package-always-ensure t)

;; Always defer loading of packages
(setq use-package-always-defer t)


;; Load config modules
(load-if-not-loaded "functions.el")
(load-if-not-loaded "options.el")
(load-if-not-loaded "plugins.el")
(load-if-not-loaded "keybindings.el")

;; Load local config if it exists (private configurations)
(when (file-exists-p (expand-file-name "local.el" my-config-dir))
  (load-if-not-loaded "local.el"))

;; Reset GC settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))  ;; 2mb
            (setq gc-cons-percentage 0.1)))
