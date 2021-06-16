;;; -*- lexical-binding: t; -*-

(defun my/open-private-configuration ()
  (interactive)
  (let ((default-directory "~/.emacs.d/"))
    (call-interactively #'find-file)))

(defun my/split-n-swap-right ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun my/split-n-swap-below ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun my/open-org-folder ()
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively #'find-file)))

(defun my/eshell-other-window ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (eshell))

(defun my/search-notes ()
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively #'consult-ripgrep)))

(defun my/org-file-by-date ()
  "Create an Org file with current time as name. Credit
https://emacs.stackexchange.com/questions/14673/emacs-function-to-make-a-file-with-date-time-as-filename-and-a-shortcut-for-it"
  (find-file (format-time-string "~/org/notes-%Y-%m-%d--%H-%M-%S.org")))

(defhydra my/flycheck-hydra ()
    "
    ^Clock^             ^Do^
    ^─────^─────────────^──^─────────
    _q_ quit            _p_ previous error
    ^^                  _n_ next error
    ^^                  _l_ list errors
    ^^                  _f_ fix error (if LSP)
    "
    ("q" nil)
    ("p" #'flycheck-previous-error)
    ("n" #'flycheck-next-error)
    ("l" #'flycheck-list-errors)
    ("f" #'lsp-execute-code-action)
    )
