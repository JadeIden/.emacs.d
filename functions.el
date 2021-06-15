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

(defhydra my/flycheck-hydra ()
    "
    ^Clock^             ^Do^
    ^─────^─────────────^──^─────────
    _q_ quit            _p_ previous error
    ^^                  _n_ next error
    ^^                  _f_ fix error (if LSP)
    "
    ("q" nil)
    ("p" #'flycheck-previous-error)
    ("n" #'flycheck-next-error)
    ("f" #'lsp-execute-code-action)
    )
