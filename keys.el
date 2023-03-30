(general-define-key "ESC" #'evil-force-normal-state)
(general-define-key :keymaps 'evil-insert-state-map (general-chord "jk") #'my/jaykay)
(general-define-key :keymaps 'evil-insert-state-map (general-chord "kj") #'my/jaykay)
(general-define-key :states 'normal
		    "ESC" #'my/jaykay)

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-symbol)
(define-key evil-normal-state-map "H" 'evil-backward-symbol)
(define-key evil-motion-state-map "L" 'evil-forward-symbol)
(define-key evil-motion-state-map "H" 'evil-backward-symbol)

(define-key evil-normal-state-map "gr" 'lsp-rename)

(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "^" 'evil-first-non-blank-of-visual-line)
(define-key evil-normal-state-map "$" 'evil-end-of-visual-line)
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)
(define-key evil-visual-state-map "^" 'evil-first-non-blank-of-visual-line)
(define-key evil-visual-state-map "$" 'evil-end-of-visual-line)

(general-create-definer my-leader-def
                        :states '(normal visual)
                        :keymaps 'override
                        :global-prefix "s-SPC"
                        :prefix "SPC"
                        )

(general-create-definer my-local-leader-def
  :states 'normal
  :keymaps 'override
  :prefix "SPC m")

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
  "cc" #'evil-mc-make-all-cursors
  "cn" #'evil-mc-skip-and-goto-next-match
  "cq" #'evil-mc-undo-all-cursors
  )
(my-leader-def
  "is" #'yas-insert-snippet
  )
(my-leader-def
  "bd" #'kill-this-buffer
  "br" #'revert-buffer
  )
(my-leader-def
  "ha" #'consult-apropos
  "hf" #'describe-function
  "hk" #'describe-key
  )
(my-leader-def
  "gg" #'magit
  "gb" #'magit-blame
  "gi" #'magit-init
  )
(my-leader-def
  "sd" #'consult-ripgrep
  "sl" #'consult-line
  "so" #'consult-outline
  "sk" #'consult-yank-from-kill-ring
  "ss" #'consult-imenu
  )
(my-leader-def
  "ll" #'consult-locate
  "ls" #'consult-lsp-symbols
  )
(my-leader-def
  "nF" #'my/open-org-folder
  "ns" #'my/search-notes
  "njj" #'org-journal-new-entry
  "njn" #'org-journal-new-entry
  "njv" #'org-journal-open-current-journal-file
  )
(my-leader-def
  "wv" #'my/split-n-swap-right
  "ws" #'my/split-n-swap-below
  "wq" #'delete-window
  "wh" #'evil-window-left
  "w <left>" #'evil-window-left
  "wl" #'evil-window-right
  "w <right>" #'evil-window-right
  "wj" #'evil-window-down
  "w <down>" #'evil-window-down
  "wk" #'evil-window-up
  "w <up>" #'evil-window-up
  )
(my-leader-def
  "yy" #'my/yank-buffer
  )

(my-leader-def
  :keymaps 'js2-mode-map
  "rs" #'tide-rename-symbol
  )

(require 'yasnippet)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(my-leader-def
  "<XF86Tools>" #'my/flycheck-hydra/body)

(my-local-leader-def
 :keymaps 'org-mode-map
 "t" #'org-todo)

(my-local-leader-def
  :keymaps 'json-mode
  "q" #'jq-interactively)

(my-local-leader-def
 :keymaps 'lsp-mode-map
 "a" #'lsp-execute-code-action)

(require 'embark)

(general-define-key "C-SPC" #'embark-act)

(general-define-key :keymaps 'embark-symbol-map "r" #'lsp-find-references)
(general-define-key "C-x C-e" #'eval-defun)
(general-define-key :keymaps 'emacs-lisp-mode-map "C-c C-c" #'eval-buffer)
(general-define-key :keymaps 'lsp-mode-map "C-c ." #'lsp-describe-thing-at-point)

(general-define-key "C-SPC" 'embark-act)
(general-define-key :states 'normal "gD" #'my/goto-definition-in-other-window)
(general-define-key :states 'normal "*" #'my/consult-line-at-point)
(general-define-key :states '(normal visual) ";" #'evil-repeat-motion)

(general-define-key :states 'normal :keymaps 'js2-mode-map "C" #'my/smart-change-rest-of-line)
(define-key isearch-mode-map (kbd "S-<return>") 'avy-isearch)
(global-unset-key (kbd "C-x C-x"))
(general-define-key "C-x C-x C-f" #'consult-projectile)
(general-define-key "C-x C-x C-c" #'save-buffers-kill-emacs)

(defun my/goto-definition-in-other-window ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (evil-goto-definition))

(defun my/consult-line-at-point ()
  (interactive)
  (consult-line (symbol-name (symbol-at-point))))

;; Credit to /u/yankfade on reddit!
(my-local-leader-def
  :mode 'smerge-mode
  "M" #'hydra-smerge/body)

(defun js/smerge-first ()
  (interactive)
  (progn
    (goto-char (point-min))
    (smerge-next)))

(defhydra hydra-smerge
  (:quit-key "q")
  "smerge"
  ("f" js/smerge-first "First")
  ("n" smerge-next "Next")
  ("p" smerge-prev "Previous")
  ("u" smerge-keep-upper "Keep Upper")
  ("l" smerge-keep-lower "Keep Lower")
  ("b" smerge-keep-all "Keep Both"))

(defun wrap-region-with (left right)
  "Wraps region with LEFT and RIGHT."
  (let ((beg (region-beginning))
        (end (region-end))
        (pos (point))
        (deactivate-mark nil))
    (save-excursion
      (goto-char beg)
      (insert left)
      (goto-char (+ end (length left)))
      (insert right))
    (if (= pos end) (forward-char 1))))

(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

(defun my/jaykay ()
  (interactive)
  (evil-force-normal-state)
  (evil-mc-undo-all-cursors))
