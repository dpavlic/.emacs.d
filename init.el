;; Functions
(defun my/join-path (&rest path-vals)
  (let ((map-to-dirs
	 (mapcar #'file-name-as-directory path-vals)))
    (mapconcat 'identity map-to-dirs "")))

;; M-x elpamr-create-mirror-for-installed to install / update local mirror
(add-to-list 'load-path (my/join-path user-emacs-directory "pkg" "elpa-mirror"))
(require 'elpa-mirror)
(setq package-archives `(("myelpa" . ,(my/join-path user-emacs-directory "pkg" "my-elpa"))))
(require 'use-package)
;;ser-emacs-directory(setq use-package-compute-statistics t)

(defun my-switch-archive(arg)
  "Switch between MELPA and personal MELPA mirror"
  (interactive "MELPA (M) or my elpa (any other key): ")
  (if (string-equal arg "M")
      (setq package-archives `(("MELPA" . "https://melpa.org/packages/")))
      (setq package-archives `(("myelpa" . ,(my/join-path u "pkg" "my-elpa"))))
      )
  (message "%s" package-archives))

;; Basic Configuration
;; 1. Display Line Numbers
;; 2. Remove Bells
;; 3. Remove menu bar
;; 4. Remove tool bar
;; 5. Remove startup splash screen
(add-hook
 'prog-mode-hook
 'display-line-numbers-mode)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)

;; Scrollings, vim style
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Backups
(setq version-control
      t
      kept-new-versions
      10
      kept-old-versions
      0
      delete-old-versions
      t
      backup-by-copying
      t)
(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist
      `(("" . ,(my/join-path
		user-emacs-directory
		"backup"
		"per-save"))))

(defun force-backup-of-buffer ()
  (when (not buffer-backed-up)
    (let ((backup-directory-alist `(("" . ,(my/join-path
					    user-emacs-directory
					    "backup"
					    "per-session"))))
	  (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook
 'before-save-hook
 'force-backup-of-buffer)

;; Fix up the isearch jumping to the END of the search
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; Source local.el if it exists
;; local.el does not live in git :: the idea is that we store only things
;;                                  here which are SPECIFIC to the local
;;                                  configuration (i.e. paths).
;; For Example:
;;   (add-to-list 'exec-path "some/location/to/bin/dir")
(let ((local-file (expand-file-name
		   "local.el"
		   user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

;; Themeing Packages
;; Set to Atom One Dark Theme
;; Set default fonts
(use-package
  atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(setq column-number-mode t)
(cond ((find-font
	(font-spec
	 :name "FuraCode Nerd Font"))
       (set-frame-font
	"FuraCode Nerd Font-14"))
      ((find-font
	(font-spec :name "Menlo"))
       (set-frame-font "Menlo-12"))
      ((find-font
	(font-spec
	 :name "DejaVu Sans Mono"))
       (set-frame-font
	"DejaVu Sans Mono-12"))
      ((find-font
	(font-spec :name "Inconsolata"))
       (set-frame-font
	"Inconsolata-12")))

;; Packages
(use-package org :defer)
(use-package god-mode
  :ensure t
  :init
  :config (god-mode))
(use-package key-chord :ensure t :config (key-chord-mode 1))
(use-package helm :ensure t)
(use-package avy :ensure t :config (global-set-key (kbd "C-/") 'avy-goto-char-timer))
(use-package magit :ensure t)

(use-package
  rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  projectile
  :ensure t)

(use-package
  undo-fu
  :ensure t
  :config (global-unset-key (kbd "C-z"))
          (global-set-key (kbd "C-z") 'undo-fu-only-undo)
	  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package
  which-key
  :ensure t
  :init ;;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom)
  (which-key-enable-god-mode-support)
  (which-key-mode)
  (define-key which-key-mode-map (kbd "C-x \\") 'which-key-C-h-dispatch))

(use-package general :ensure t)

(use-package
  flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init (add-hook
	 'sh-mode-hook
	 'flymake-shellcheck-load))

;; Completion Packages
(use-package eglot :ensure t)
(use-package
  company
  :ensure t
  :config (add-hook
	   'after-init-hook
	   'global-company-mode)
  ;; Start completing right away, prefix of 2 and wrap selections around
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length
	2)
  (setq company-selection-wrap-around
	't)
  ;; Use tab completion a la youcompleteme
  (company-tng-configure-default))

;; Dashboard
(use-package
  dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
	"Dashboard")
  (setq dashboard-set-footer nil))

;; Keybindings
(general-define-key "M-x" 'helm-M-x)
(general-define-key "<escape>" 'god-mode-all)
(general-define-key
 :prefix "C-x"
 "b" 'helm-buffers-list
 "C-f" 'helm-find-files)
(general-define-key
 :prefix "C-c"
 "e" 'eshell
 "l" 'avy-goto-line
 "s" 'helm-occur)

;; God Mode Settings and Extensions
(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(defun god-mod-toggle ()
  (interactive)
  (if (string-equal god-mod-current-mode "C")
      (progn
	(setq god-mod-current-mode "M")
	(setq god-mod-alist '((nil . "M-"))))
    (progn
      (setq god-mod-current-mode "C")
      (setq god-mod-alist '((nil . "C-"))))))

(defun my-god-mode-update-mode-line ()
  (cond
   ((and god-local-mode (string-equal god-mod-current-mode "C"))
    (set-face-attribute 'mode-line nil
                        :foreground "#604000"
                        :background "#98C379")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#3f3000"
                        :background "#98C379"))
   ((and god-local-mode (string-equal god-mod-current-mode "M"))
    (set-face-attribute 'mode-line nil
                        :foreground "#604000"
                        :background "#E06C75")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#3f3000"
                        :background "#E06C75"))
   (t
    (set-face-attribute 'mode-line nil
			:foreground "#0a0a0a"
			:background "#d7d7d7")
    (set-face-attribute 'mode-line-inactive nil
			:foreground "#404148"
			:background "#efefef"))))

(setq god-mod-current-mode "C")

(define-key god-local-mode-map (kbd ".") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-local-mode)
(define-key god-local-mode-map (kbd ",") #'god-mod-toggle)
(key-chord-define-global "kj" 'god-mode-all)

(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
(add-hook 'post-command-hook 'my-god-mode-update-mode-line)

;; Set custom variables in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

