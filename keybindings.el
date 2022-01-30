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

