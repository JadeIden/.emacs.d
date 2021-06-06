;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)) (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(load (expand-file-name "~/.emacs.d/options.el"))

(straight-use-package 'evil)
(straight-use-package 'evil-args)

(straight-use-package 'general)

(straight-use-package 'key-chord)

(straight-use-package 'no-littering)

(straight-use-package 'which-key)

(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'company)
(straight-use-package 'company-lsp)
(straight-use-package 'consult-lsp)
(straight-use-package 'undo-fu)

(straight-use-package 'selectrum)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)

(straight-use-package 'perspective)
(straight-use-package 'persp-projectile)

(straight-use-package 'page-break-lines)
(straight-use-package 'dashboard)
(straight-use-package 'all-the-icons)

(straight-use-package 'magit)

(require 'no-littering)

(require 'company)
(require 'company-lsp)
(push 'company-lsp company-backends)
(add-hook 'after-init-hook 'global-company-mode)

(straight-use-package 'doom-themes)
(load-theme 'doom-one t)

(when my/use-haskell
  (straight-use-package 'haskell-mode)
  (add-hook 'haskell-mode-hook #'lsp))

(when my/use-python
  (straight-use-package 'ob-ipython)
  (org-babel-do-load-languages 'org-babel-load-languages
    '((ipython . t)
    ;; other languages..
    ))
  (add-hook 'python-mode-hook #'lsp)
  (setq org-confirm-babel-evaluate nil)
  )

(when my/use-lsp
  (straight-use-package 'lsp-mode))

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; bind evil-args text objects
(setq evil-undo-system 'undo-fu)
(require 'evil)

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

(setq completion-styles '(orderless))

;; Persist history over Emacs restarts
(savehist-mode)

;; Optional performance optimization
;; by highlighting only the visible candidates.
(setq orderless-skip-highlighting (lambda () selectrum-is-active))
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

(defun my/open-private-configuration ()
  (interactive)
  (let ((default-directory "~/.emacs.d/"))
    (call-interactively #'find-file)))

(general-create-definer my-leader-def
                        :states 'normal
                        :keymaps 'override
                        :prefix "SPC")

(my-leader-def "<" #'consult-buffer)
(my-leader-def ":" #'execute-extended-command)
(my-leader-def ";" #'eval-expression)
(my-leader-def "SPC" #'find-file)
(my-leader-def "." #'find-file)
(my-leader-def "fP" #'my/open-private-configuration)
(my-leader-def
  "TAB TAB" #'persp-switch
  "TAB d" #'persp-kill
  )
(my-leader-def
  "of" #'make-frame
  )
(my-leader-def
  "pp" #'projectile-persp-switch-project
  "pc" #'projectile-command-map
  "pa" #'projectile-add-known-project
  )
(my-leader-def
  "bd" #'kill-this-buffer
  )
(my-leader-def
  "gg" #'magit
  "gi" #'magit-init
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

(selectrum-mode +1)
(projectile-mode +1)
(key-chord-mode 1)
(which-key-mode 1)
(global-tree-sitter-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(evil-mode +1)
(persp-mode +1)
(global-display-line-numbers-mode 1)

(setq org-directory (expand-file-name "~/org"))

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq dashboard-set-init-info t)
(setq dashboard-set-footer nil)
(setq dashboard-week-agenda t)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(push 'evil-overriding-maps dashboard-mode-map)

(fset 'yes-or-no-p 'y-or-n-p)
