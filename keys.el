(general-define-key "ESC" #'evil-force-normal-state)
(general-define-key :keymaps 'evil-insert-state-map (general-chord "jk") #'evil-force-normal-state)
(general-define-key :keymaps 'evil-insert-state-map (general-chord "kj") #'evil-force-normal-state)
(general-define-key :states 'normal
		    "ESC" #'keyboard-quit)

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

(general-create-definer my-leader-def
                        :states 'normal
                        :keymaps 'override
                        :prefix "SPC")

(my-leader-def "<" #'consult-buffer)
(my-leader-def ":" #'execute-extended-command)
(my-leader-def ";" #'eval-expression)
(my-leader-def "SPC" #'find-file)
(my-leader-def "." #'find-file)
(my-leader-def "x" #'org-capture)
(my-leader-def "fP" #'my/open-private-configuration)
(my-leader-def
  "TAB TAB" #'persp-switch
  "TAB d" #'persp-kill
  )
(my-leader-def
  "oE" #'eshell
  "oe" #'my/eshell-other-window
  "oa" #'org-agenda
  "oA" #'org-agenda-list
  "of" #'make-frame
  )
(my-leader-def
  "pp" #'projectile-switch-project
  "pc" #'projectile-command-map
  "pd" #'projectile-remove-known-project
  "pa" #'projectile-add-known-project
  )
(my-leader-def
  "bd" #'kill-this-buffer
  )
(my-leader-def
  "ha" #'consult-apropos
  "hk" #'describe-key
  )
(my-leader-def
  "gg" #'magit
  "gi" #'magit-init
  )
(my-leader-def
  "sd" #'consult-ripgrep
  "sl" #'consult-line
  "so" #'consult-outline
  )
(my-leader-def
  "ll" #'consult-locate
  "ls" #'consult-lsp-symbols
  )
(my-leader-def
  "nF" #'my/open-org-folder
  )
(my-leader-def
  "wv" #'split-window-right
  "ws" #'split-window-below
  "wq" #'delete-window
  "wh" #'evil-window-left
  "wl" #'evil-window-right
  "wj" #'evil-window-down
  "wk" #'evil-window-up
  )

(general-define-key "<f13>" #'embark-act) ;; F3
(general-define-key "<f14>" #'embark-become) ;; F22

(general-define-key :keymaps 'embark-symbol-map "r" #'lsp-find-references)
(general-define-key "C-x C-e" #'eval-defun)
(general-define-key :keymaps 'emacs-lisp-mode-map "C-c C-c" #'eval-buffer)
