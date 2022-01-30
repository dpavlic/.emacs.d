(require 'cl-lib)
(require 'org)

;; Functions
(defun my/join-path (&rest path-vals)
  (let ((map-to-dirs (cl-map
		      'vector
		      #'file-name-as-directory
		      path-vals)))
    (mapconcat 'identity map-to-dirs "")))

;; Package bootstrap and initialize
(eval-when-compile
  (add-to-list
   'load-path
   (my/join-path
    user-emacs-directory
    "pkg"
    "use-package"))
  (require 'use-package))
(add-to-list
 'package-archives
 '("MELPA" . "http://melpa.org/packages/"))
(package-initialize)

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

;; (setq inhibit-startup-message t)

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
;; Set to doom status line with column number
;; Set default fonts
(use-package
  atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))
(use-package
  doom-modeline
  :ensure t
  :config (doom-modeline-mode 1)
  (setq column-number-mode t))
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
(use-package
  rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  projectile
  :ensure t)

(use-package undo-fu :ensure t)
(use-package
  evil
  :ensure t
  :init (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system
	'undo-fu)
  (setq evil-want-fine-undo t)
  :config (advice-add
	   'undo-auto--last-boundary-amalgamating-number
	   :override #'ignore)
  (evil-ex-define-cmd "q[uit]" nil)
  (evil-ex-define-cmd "wq" nil)
  (evil-mode 1))

(use-package
  lispy
  :ensure t)
(use-package
  lispyville
  :ensure t
  :hook (lispy-mode . lispyville-mode))
(use-package
  evil-collection
  :ensure t
  :config (evil-collection-init)
  (with-eval-after-load
      "evil-collection-magit-state"
    ((evil-define-key*
       evil-collection-magit-state
       magit-mode-map
       [escape]
       nil))))

(use-package
  which-key
  :ensure t
  :init (which-key-setup-minibuffer)
  (which-key-mode))

(use-package general :ensure t)

(use-package helm :ensure t)
(use-package
  helm-projectile
  :ensure t)
(use-package
  helm-swoop
  :ensure t)

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
  (setq dashboard-set-init-info
	nil)
  (setq dashboard-set-footer nil))

;; Keybindings
(general-evil-setup t)
(general-define-key
 ;; Define escape as a transient quit (for magit)
 :keymaps 'transient-base-map
 "<escape>"
 'transient-quit-one
 :keymaps 'magit-status-mode-map
 "<escape>"
 'magit-mode-bury-buffer)
(nmap "<up>" 'evil-scroll-up "<down>" 'evil-scroll-down)
(nmap
  :prefix "SPC"
  "b"
  '(:ignore t
	    :which-key "buffers")
  "b s"
  'save-buffer
  "b d"
  'kill-this-buffer
  "b e"
  'eval-buffer
  "b l"
  'helm-buffers-list
  "b s"
  'save-buffer
  "b w"
  'helm-swoop
  "f r"
  'helm-recentf
  "g"
  '(:ignore t :which-key "git")
  "g s"
  'magit-status
  "e"
  'eshell
  "w"
  '(:ignore t
	    :which-key "window")
  "w -"
  'evil-window-split
  "w \\"
  'evil-window-vsplit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(lispyville which-key undo-fu srefactor rainbow-delimiters magit lispy helm-swoop helm-projectile general format-all flymake-shellcheck evil-collection eglot doom-modeline company atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
