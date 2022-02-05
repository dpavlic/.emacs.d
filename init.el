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
(setq use-package-compute-statistics t)

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
(use-package hydra :ensure t)
(use-package major-mode-hydra
  :ensure t
  :bind
  ("M-SPC" . major-mode-hydra))
(use-package pretty-hydra :ensure t)
(use-package fontawesome :ensure t)
(use-package key-chord :ensure t :config (key-chord-mode 1))
(use-package helm :ensure t)
(use-package swiper-helm :ensure t)
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

(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defvar hydra-hydra--title (with-faicon "map" "Hydra" 1 -0.05))
(defvar hydra-system--title (with-faicon "map" "System" 1 -0.05))
(defvar hydra-nav--title (with-faicon "map" "Nav" 1 -0.05))

(pretty-hydra-define hydra-hydra (:foreign-keys warn :title hydra-hydra--title :quit-key "q" :exit t)
  ("Hydras"
   (("s" hydra-system/body "system")
    ("k" hydra-nav/body "nav"))))

(pretty-hydra-define hydra-system (:foreign-keys warn :title hydra-system--title :quit-key "q")
  ("System"
   (("b" helm-buffers-list "buffer list")
    ("s" save-buffer "buffer save")
    ("g" magit-status "git status")
    ("." hydra-hydra/body "hydras" :exit t))))

(pretty-hydra-define hydra-nav (:foreign-keys warn :title hydra-nav--title :quit-key "q")
  ("Basic"
   (("f" forward-char "forward")
    ("b" backward-char "backward")
    ("F" forward-word "fast forward")
    ("B" backward-word "fast backward")
    ("p" previous-line "prev line")
    ("n" next-line "next line"))

   "Extended"
   (("a" beginning-of-line "begin of line")
    ("e" end-of-line "end of line")
    ("v" scroll-up "pg down")
    ("V" scroll-down "pg up")
    ("<" beginning-of-buffer "begin of buffer")
    (">" end-of-buffer "end buffer"))

   "Search"
   (("s" swiper-helm "search")
    ("g" goto-line "to line")
    ("/" avy-goto-char-timer "avy search"))
   
   "Select / Delete"
   (("k" kill-line "kill line")
    ("d" delete-char "del char")
    ("D" kill-word "kill word")
    ("<SPC>" (cond ((not mark-active) (call-interactively 'set-mark-command))
                   (t (deactivate-mark))) "mark")
    ("w" kill-region "kill region")
    ("W" kill-ring-save "save region")
    ("<backspace>" delete-backward-char nil)
    ("<return>" newline nil))

   "Other"
   (("y" yank "yank")
    ("Y" yank-pop "yank pop")
    ("z" undo-fu-only-undo "undo")
    ("Z" undo-fu-only-redo "redo")
    ("i" (lambda (txt)
	 (interactive "sQuick insertion:")
	 (insert txt)) "insert")
    ("." hydra-hydra/body "hydras" :exit t))))

;; Keybindings
(general-define-key "M-x" 'helm-M-x)

(setq key-chord-two-keys-delay 0.4)
(general-define-key
 :prefix "C-x"
 "b" 'helm-buffers-list
 "C-f" 'helm-find-files)
(general-define-key
 :prefix "C-c"
 "e" 'eshell
 "s" 'helm-occur)
(general-define-key
 (general-chord "kj") 'hydra-nav/body
 (general-chord "\\s") 'hydra-system/body
 (general-chord "\\x") 'helm-M-x)

;; Set custom variables in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; (defvar jp-window--title (with-faicon "windows" "Window Management" 1 -0.05))

;; (pretty-hydra-define jp-window (:foreign-keys warn :title jp-window--title :quit-key "q")
;;   ("Actions"
;;    (("TAB" other-window "switch")
;;     ("x" ace-delete-window "delete")
;;     ("m" ace-delete-other-windows "maximize")
;;     ("s" ace-swap-window "swap")
;;     ("a" ace-select-window "select"))

;;    "Resize"
;;    (("h" move-border-left "←")
;;     ("j" move-border-down "↓")
;;     ("k" move-border-up "↑")
;;     ("l" move-border-right "→")
;;     ("n" balance-windows "balance")
;;     ("f" toggle-frame-fullscreen "toggle fullscreen"))

;;    "Split"
;;    (("b" split-window-right "horizontally")
;;     ("B" split-window-horizontally-instead "horizontally instead")
;;     ("v" split-window-below "vertically")
;;     ("V" split-window-vertically-instead "vertically instead"))

;;    "Zoom"
;;    (("+" zoom-in "in")
;;     ("=" zoom-in)
;;     ("-" zoom-out "out")
;;     ("0" jp-zoom-default "reset"))))
