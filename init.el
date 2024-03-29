;;; -*- lexical-binding: t; -*-

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

(setq ediff-diff-options "")
(setq ediff-custom-diff-options "-u")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)

(setq evil-undo-system 'undo-fu)
(setq evil-want-keybinding nil)
(setq evil-collection-calendar-want-org-bindings t)
(setq evil-collection-outline-bind-tab-p t)
(setq evil-collection-setup-minibuffer t)
(setq-default evil-symbol-word-search t)

(setq js-indent-level 4)

(setq org-directory (expand-file-name "~/Notes/"))
(setq org-agenda-diary-file nil)

(straight-use-package 'org)
(straight-use-package 'evil)
(straight-use-package 'evil-plugins)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-args)
(straight-use-package 'evil-mc)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'evil-textobj-tree-sitter)
(straight-use-package 
 '(evil-repeat-motion :host github
                      :repo "yyoncho/evil-repeat-motion"))

(straight-use-package 'avy)
(straight-use-package 'vterm)

(straight-use-package 'perspective)

(straight-use-package 'protobuf-mode)
(straight-use-package 'json-mode)
(straight-use-package 'sqlformat)
(straight-use-package 'yaml-mode)

(straight-use-package 's)
(straight-use-package 'f)
(straight-use-package 'hl-todo)
(straight-use-package 'web-beautify)
(straight-use-package 'svg-tag-mode)

(straight-use-package 'general)

(straight-use-package 'projectile)
(straight-use-package 'direnv)

(straight-use-package 'csv-mode)
(straight-use-package 'go-mode)
(straight-use-package 'nim-mode)
(straight-use-package 'slime)
(straight-use-package 'sly)
(straight-use-package 'mixed-pitch)
(straight-use-package 'org-bullets)

(straight-use-package 'git-gutter)
(require 'org-protocol)
(require 'svg-tag-mode)

(straight-use-package 'yasnippet)

(straight-use-package 'key-chord)

(straight-use-package 'no-littering)

(straight-use-package 'which-key)

(straight-use-package 'web-mode)
(straight-use-package 'skewer-mode)

(straight-use-package 'jq-mode)

(straight-use-package 'doom-modeline)

(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'bookmark+)
(straight-use-package 'eshell-bookmark)
(require 'bookmark+)

(add-hook 'eshell-mode-hook 'eshell-bookmark-setup)

(unless (eq system-type 'darwin)
	(straight-use-package 'tree-sitter)
	(straight-use-package 'tree-sitter-langs))


(straight-use-package 'tide)

(straight-use-package 'corfu)
(straight-use-package 'consult-lsp)
(straight-use-package 'undo-fu)
(straight-use-package 'help-fns-plus)
(straight-use-package 'scratch)

(straight-use-package 'vertico)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)

(straight-use-package 'perspective)
(straight-use-package 'hydra)

(straight-use-package 'page-break-lines)
(straight-use-package 'all-the-icons)

(straight-use-package 'magit)
(straight-use-package 'popwin)

(straight-use-package 'org)
(straight-use-package 'org-journal)
(straight-use-package 'org-wild-notifier)

(require 'evil)

(require 'evil-little-word)
(define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-normal-state-map (kbd "e") 'evil-forward-little-word-end)
(define-key evil-operator-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-operator-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-operator-state-map (kbd "e") 'evil-forward-little-word-end)
(define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-visual-state-map (kbd "e") 'evil-forward-little-word-end)
(define-key evil-visual-state-map (kbd "i w") 'evil-inner-little-word)

(load (expand-file-name "~/.emacs.d/options.el"))
(load (expand-file-name "~/.emacs.d/functions.el"))
(load (expand-file-name "~/.emacs.d/keys.el"))

(add-hook 'after-init-hook 'global-hl-todo-mode)

(straight-use-package 'olivetti)

(straight-use-package 'doom-themes)
(straight-use-package 'modus-themes)
(load-theme 'modus-vivendi t)

(straight-use-package 'kubernetes)
(straight-use-package 'kubernetes-evil)

(setq org-confirm-babel-evaluate nil)

(when my/use-haskell
  (straight-use-package 'haskell-mode)
  (straight-use-package 'lsp-haskell)
  (add-hook 'haskell-mode-hook #'lsp))

(when my/use-python
  (straight-use-package 'ob-ipython)
  ;; (org-babel-do-load-languages 'org-babel-load-languages
  ;;                              '((ipython . t)
  ;;                                (shell . t)
  ;;   ;; other languages..
  ;;   ))
  (require 'lsp-pyright)
  (add-hook 'python-mode-hook #'lsp)
  (setq org-confirm-babel-evaluate nil)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  )

(unless my/use-python
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 )
                               ))

(add-hook 'nim-mode-hook #'lsp)

(when my/use-lsp
  (straight-use-package 'lsp-mode)
  (setq lsp-completion-enable t)

  (defun org-babel-edit-prep:javascript (babel-info)
    (setq-local buffer-file-name (->> babel-info (alist-get :tangle)))
    (lsp))

  ;; (setq lsp-prefer-capf t)
  )

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  )

;; aligns annotation to the right hand side

(add-hook 'js2-mode-hook #'setup-tide-mode)


(require 'tree-sitter)
(require 'tree-sitter-langs)

;; bind evil-args text objects
(evil-collection-init)


(setq completion-styles '(orderless initials))

;; Persist history over Emacs restarts
(savehist-mode)
(winner-mode +1)

(setq embark-action-indicator (lambda (map __target)
                                (which-key--show-keymap "Embark" map nil nil 'no-paging)
                                #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

;; No more squiggles! Thanks to https://stackoverflow.com/a/18330742
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(setq-default tab-always-indent 'complete)
(setq corfu-auto t)
(vertico-mode +1)
(global-git-gutter-mode +1)
(marginalia-mode +1)
(projectile-mode +1)
(direnv-mode +1)
(key-chord-mode 1)
(which-key-mode 1)
(show-paren-mode 1)
(evil-repeat-motion-mode 1)
(global-evil-mc-mode +1)
(electric-pair-mode 0)
(global-corfu-mode +1)
(global-tree-sitter-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(evil-mode +1)
(persp-mode +1)
(evil-commentary-mode 1)
(global-evil-surround-mode 1)
(global-flycheck-mode 1)
(global-display-line-numbers-mode 1)
(yas-global-mode 1)
(popwin-mode 1)
(display-battery-mode 1)
(projectile-mode +1)
(toggle-frame-maximized)
(add-hook 'org-mode-hook #'org-indent-mode)
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))

(add-hook 'after-init-hook #'doom-modeline-mode)

;; Mark org-capture windows as popups
(push '("*Org Select*" :height 15) popwin:special-display-config)
(push '("^CAPTURE-.+\*.org$" :regexp t) popwin:special-display-config)
(push '("*Flycheck errors*" :height 15) popwin:special-display-config)

(with-eval-after-load 'org
    (defun org-switch-to-buffer-other-window (&rest args)
      "Same as the original, but lacking the wrapping call to `org-no-popups'"
      (apply 'switch-to-buffer-other-window args)))

(defun my/lazy--find-file ()
  (interactive)
  (call-interactively #'find-file))

(setq projectile-switch-project-action #'my/lazy--find-file)
(setq org-hide-emphasis-markers t)
(setq org-agenda-diary-file nil)

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq lsp-clients-clangd-executable "clangd")
(setq lsp-clients-clangd-args '("--query-driver=/**/bin/xtensa-esp32-elf-*" "--ba"))
(add-hook 'c-mode 'lsp)
(add-hook 'lsp-mode 'lsp-enable-which-key-integration)

(add-hook 'eshell-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'comint-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

(fset 'yes-or-no-p 'y-or-n-p)
(setq password-cache t)
(setq password-cache-expiry 3600)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
   that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


(set-exec-path-from-shell-PATH)
(setenv "PATH" (concat (getenv "PATH") ":/Users/jeremyiden/go/bin"))
(add-to-list 'exec-path "/Users/jeremyiden/go/bin")

(when (eq system-type 'darwin)
    (setq indium-chrome-executable "~/.emacs.d/mac-launch-chrome.sh"))
(when (eq system-type 'gnu/linux)
  (setq indium-chrome-executable "chromium-browser"))

(setq-default evil-surround-pairs-alist
              (push '(?| . ("(" . " || '')")) evil-surround-pairs-alist))

(setq ring-bell-function nil)

(add-hook 'org-mode 'mixed-pitch-mode)
(add-hook 'org-mode 'org-bullets-mode)

(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

(when (file-exists-p "~/.emacs.d/init-local.el")
  (load "~/.emacs.d/init-local.el"))

(setq svg-tag-tags '(
                     ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'hl-todo :inverse t :margin 0))))
                     ))
(global-svg-tag-mode 0)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(put 'narrow-to-region 'disabled nil)

(require 'hydra)

(straight-use-package 'smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
)

(defun org-mode-<>-syntax-fix (start end)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "<\\|>" end t)
    (when (get-text-property (point) 'src-block)
      ;; This is a < or > in an org-src block
      (put-text-property (point) (1- (point))
                 'syntax-table (string-to-syntax "_")))))))

(add-hook 'org-mode-hook
      (lambda ()
        (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
        (syntax-propertize (point-max))))

(add-hook 'minibuffer-setup-hook (lambda ()
                                   (when (memq #'completion-at-point
                                               (flatten-tree
                                                (current-local-map)))
                                     (corfu-mode))))

(general-add-advice 'unpackaged/smerge-hydra/body :after 'magit-diff-visit-file)

(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)

(defun my/run-staticcheck ()
  "If the magit directory is a Go project, run staticcheck ./... and fail if necessary."
  (with-temp-buffer
      (call-process "staticcheck" nil (current-buffer) nil "./...")))
