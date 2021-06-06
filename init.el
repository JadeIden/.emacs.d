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


(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-args)

(straight-use-package 'general)

(straight-use-package 'key-chord)

(straight-use-package 'no-littering)

(straight-use-package 'which-key)

(straight-use-package 'web-mode)
(straight-use-package 'skewer-mode)

(straight-use-package 'doom-modeline)

(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'company)
(straight-use-package 'company-lsp)
(straight-use-package 'consult-lsp)
(straight-use-package 'undo-fu)
(straight-use-package 'help-fns-plus)

(straight-use-package 'selectrum)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)

(straight-use-package 'perspective)
;;(straight-use-package 'persp-projectile)

(straight-use-package 'page-break-lines)
(straight-use-package 'all-the-icons)

(straight-use-package 'magit)

(require 'evil)
(load (expand-file-name "~/.emacs.d/options.el"))
(load (expand-file-name "~/.emacs.d/functions.el"))
(load (expand-file-name "~/.emacs.d/keys.el"))

(require 'company)
(require 'company-lsp)
(push 'company-lsp company-backends)
(add-hook 'after-init-hook 'global-company-mode)

(straight-use-package 'doom-themes)
(load-theme 'doom-one t)

(when my/use-haskell
  (straight-use-package 'haskell-mode)
  (straight-use-package 'lsp-haskell)
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
(setq evil-want-keybinding nil)
(evil-collection-init)

(setq completion-styles '(orderless))

;; Persist history over Emacs restarts
(savehist-mode)

;; Optional performance optimization
;; by highlighting only the visible candidates.
(setq orderless-skip-highlighting (lambda () selectrum-is-active))
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

(setq org-agenda-files '("~/org/todo.org" "~/org/todo-archive.org"))
(setq org-agenda-start-on-weekday 0)
(setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
(setq org-capture-templates '(
			      ("t" "TODO" entry (file+headline "~/org/todo.org" "Tasks")
			       "* TODO %?\n  %i\n  %a")
			      ("a" "Appointment" entry (file+headline "~/org/todo.org" "Appointments")
			       "* %? \n%^T")))

;; Credit to https://protesilaos.com/dotemacs/#h:d67ed8d0-d711-48b0-9f40-f88ae2e5c984
(defvar embark-action-indicator)
(defvar embark-become-indicator) (declare-function which-key--show-keymap "which-key")
(declare-function which-key--hide-popup-ignore-command "which-key")

(defvar prot-embark--which-key-state nil
  "Store state of Embark's `which-key' hints.")

;;;###autoload
(defun prot-embark-toggle-which-key ()
  "Toggle `which-key' hints for Embark actions."
  (interactive)
  (if prot-embark--which-key-state
      (progn
        (setq embark-action-indicator
                   (let ((act (propertize "Act" 'face 'highlight)))
                     (cons act (concat act " on '%s'"))))
        (setq prot-embark--which-key-state nil))
    (setq embark-action-indicator
          (lambda (map _target)
            (which-key--show-keymap "Embark" map nil nil 'no-paging)
            #'which-key--hide-popup-ignore-command)
          embark-become-indicator embark-action-indicator)
    (setq prot-embark--which-key-state t)))

(prot-embark-toggle-which-key)

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


(selectrum-mode +1)
(marginalia-mode +1)
(projectile-mode +1)
(key-chord-mode 1)
(which-key-mode 1)
(global-tree-sitter-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(evil-mode +1)
(persp-mode +1)
(evil-commentary-mode 1)
(global-display-line-numbers-mode 1)

(add-hook 'after-init-hook #'doom-modeline-mode)

(setq org-directory (expand-file-name "~/org"))

(defun my/lazy--find-file ()
  (interactive)
  (call-interactively #'find-file))

(setq projectile-switch-project-action #'my/lazy--find-file)

(fset 'yes-or-no-p 'y-or-n-p)
