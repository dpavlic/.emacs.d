;; Functions
(defun my/join-path (&rest path-vals)
  (let ((map-to-dirs
	 (mapcar #'file-name-as-directory path-vals)))
    (mapconcat 'identity map-to-dirs "")))

;; Bootsrap straight.el and use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-compute-statistics t)

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
;;(setq scroll-step 2)
;;(setq scroll-conservatively 1000)
;;(setq auto-window-vscroll nil)

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

;; Remove lockfile creation
(setq create-lockfiles nil)
(setq auto-save-default nil)

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

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Themeing Packages
;; Set to Atom One Dark Theme
;; Set default fonts
(use-package
  atom-one-dark-theme
  :straight t
  :config (load-theme 'atom-one-dark t))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package all-the-icons :straight t)

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

;; Builtins
(use-package emacs
  :custom
  (redisplay-dont-pause            t)
  (next-screen-context-lines       2)
  (scroll-conservatively       10000)
  (scroll-step                     1)
  (fast-but-imprecise-scrolling    t)
  (auto-window-vscroll           nil))
(use-package org :defer 1)

;; If we are on the Mac, use exec-path-from-shell to pickk
;; up all the extras from the path.
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package treemacs :straight t)
(use-package treemacs-all-the-icons
  :straight t
  :config (treemacs-load-theme "all-the-icons"))
(use-package avy
  :straight t
  :config (global-set-key (kbd "C-/") 'avy-goto-char-timer))
(use-package magit :straight t :defer 1)
(use-package yaml-mode
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package smartparens :straight t)

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config (evil-mode 1)) 
(use-package evil-collection
  :straight t
  :after evil
  :config (evil-collection-init))
(use-package undo-fu
  :straight t
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo))

(use-package counsel
  :straight t
  :config (ivy-mode 1))
(use-package ivy-posframe
  :straight t
  :config (ivy-posframe-mode 1))
(use-package ivy-rich
  :straight t
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package projectile :straight t)

(use-package
  which-key
  :straight t
  :config 
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (define-key which-key-mode-map (kbd "C-x \\") 'which-key-C-h-dispatch)
  :init
  (setq which-key-idle-delay 0.25)
  (setq which-key-idle-secondary-delay 0.05))
(use-package general :straight t)

(use-package
  flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :init (add-hook
	 'sh-mode-hook
	 'flymake-shellcheck-load))

;; Completion Packages
;;(use-package eglot :straight t)
(use-package lsp-mode :straight t :defer 1)
(use-package
  company
  :straight t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Disable company in eshell
  (setq company-global-modes '(not emacs-mode))
  ;; Start completing right away, prefix of 2 and wrap selections around
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length
	2)
  (setq company-selection-wrap-around
	't)
  ;; Use tab completion a la youcompleteme
  (company-tng-configure-default))

(use-package elpy
  :straight t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq
   python-shell-interpreter "ipython3"
   python-shell-interpreter-args "--simple-prompt --pprint"))

;; Dashboard
(use-package
  dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
	"Dashboard")
  (setq dashboard-set-footer nil))

;; Keybinding
(general-define-key
 :state
 'insert "k" (general-key-dispatch 'self-insert-command "j" 'evil-normal-state))

(general-define-key
  :states 'normal
  :prefix "SPC"
  "s" '(:ignore t :which-key "System")
  "b" '(ivy-switch-buffer :which-key "Buffers list")
  "x" '(counsel-M-x :which-key "m-X")
  "r" '(swiper :which-key "swiper")
  "g" '(magit-status :which-key "Git status")
  "E" '(eshell :which-key "eshell")
  "s s" '(save-buffer :which-key "Save buffer")
  "s k" '(kill-this-buffer :which-key "buffer Kill")
  "s r" '(counsel-recentf :which-key "recent files")
  "/" 'avy-goto-char-timer "avy")

(general-define-key
  :states 'normal
  :keymaps 'emacs-lisp-mode-map
  :prefix "SPC"
  "e" '(eval-buffer :which-key "Eval buffer"))

(general-define-key
 :states 'visual
 :prefix "SPC"
 "/" 'avy-goto-char-timer "avy")


(general-define-key
  :states 'normal
  :keymaps 'python-mode-map
  "<return>" 'elpy-shell-send-statement-and-step)
(general-define-key
  :states 'visual
  :keymaps 'python-mode-map
  "<return>" 'elpy-shell-send-region-or-buffer)

;; Set custom variables in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
