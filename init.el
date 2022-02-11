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
(use-package hydra :straight t)
(use-package major-mode-hydra :straight t)
(use-package pretty-hydra :straight t)
(use-package key-chord :straight t :config (key-chord-mode 1))
(use-package all-the-icons :straight t)
(use-package helm :straight (helm :commit "08c4ad8e80394c8bc2c0d50429bce765f03826db"))
(use-package swiper-helm :straight t :after helm)
(use-package avy :straight t :config (global-set-key (kbd "C-/") 'avy-goto-char-timer))
(use-package magit :straight t)
(use-package yaml-mode :straight t :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package
  rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  projectile
  :straight t)

(use-package undo-fu
  :straight t
  :config (global-unset-key (kbd "C-z"))
          (global-set-key (kbd "C-z") 'undo-fu-only-undo)
	  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package
  which-key
  :straight t
  :init ;;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (define-key which-key-mode-map (kbd "C-x \\") 'which-key-C-h-dispatch))
(use-package general :straight t)

(use-package
  flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :init (add-hook
	 'sh-mode-hook
	 'flymake-shellcheck-load))

;; Completion Packages
(use-package eglot :straight t)
(use-package
  company
  :straight t
  :config (add-hook
	   'after-init-hook
	   'global-company-mode)
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

;; Dashboard
(use-package
  dashboard
  :straight t
  :config (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
	"Dashboard")
  (setq dashboard-set-footer nil))

;; Hydra functions
(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0)
  :height (or height 1)) " " str))

(defmacro my-pretty-hydra-define (name body heads)
  (let* ((used-keys (->> (--remove (stringp it) heads)
			 (-flatten-n 1)
		         (--map (car it))))
	 (lower-keys '("q" "w" "e" "r" "t" "y" "i" "o" "p" "a" "s" "d" "h" "f" "g" "h"
		       "j" "k" "l" "z" "x" "c" "v" "n" "m"))
	 (all-common-keys (append lower-keys (--map (upcase it) lower-keys)))
	 (all-keys
	  (append all-common-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
				    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"
				    "`" "~" "{" "}" "|" "[" "]" "\\" ";" ":" "'"
				    "<" ">" "," "." "/" "?")))
	 (unused-keys (-difference all-keys used-keys))
	 (unused-definitions (-map (lambda (x) `(,x ignore nil)) unused-keys))
	 (final-heads (append heads `("" ,unused-definitions))))
    `(pretty-hydra-define ,name ,body ,final-heads)))

;; Hydra definitions
(defvar hydra-hydra--title (with-faicon "map" "Hydra" 1 -0.05))
(my-pretty-hydra-define hydra-hydra (:color pink :title hydra-hydra--title :quit-key "q")
  ("Hydras"
   (("s" hydra-system/body "system" :exit t)
    ("k" hydra-nav/body "nav" :exit t)
    ("w" hydra-window/body "window" :exit t))))

(defvar hydra-system--title (with-faicon "map" "System" 1 -0.05))
(my-pretty-hydra-define hydra-system (:color pink :title hydra-system--title :quit-key "q")
  ("System"
   (("b" helm-buffers-list "buffer list")
    ("e" eshell "eshell")
    ("s" save-buffer "buffer save")
    ("g" magit-status "git status")
    ("." hydra-hydra/body "hydras" :exit t))))

(defvar hydra-nav--title (with-faicon "map" "Nav" 1 -0.05))
(my-pretty-hydra-define hydra-nav (:color pink :title hydra-nav--title :quit-key "q")
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
    ("<SPC>" (
	      cond ((not mark-active) (call-interactively 'set-mark-command)
		    (t (deactivate-mark))) "mark"))
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

(defvar hydra-window--title (with-faicon "windows" "Window Management" 1 -0.05))
(pretty-hydra-define hydra-window (:color pink :title hydra-window--title :quit-key "q")
  ("Actions"
   (("TAB" other-window "switch")
    ("d" delete-window "delete")
    ("m" delete-other-windows "maximize")
    ("s" window-swap-states "swap")
    ("a" ace-select-window "select"))

   "Resize"
   (("<left>" shrink-window-horizontally "←")
    ("<right>" enlarge-window-horizontally "→")
    ("k" move-border-up "↑")
    ("l" move-border-right "→")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("\\" split-window-right "split right")
    ("-" split-window-below  "split below"))))

;; Keybindings
(setq key-chord-two-keys-delay 0.4)
(general-define-key
 :prefix "C-c"
 "e" 'eshell
 "s" 'helm-occur)
(general-define-key
 (general-chord "kj") 'hydra-nav/body
 (general-chord "\\s") 'hydra-system/body
 (general-chord "\\x") 'helm-M-x
 (general-chord "\\w") 'hydra-window/body
 (general-chord "\\b") 'helm-buffers-list)

;; Set custom variables in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

