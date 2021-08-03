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

(defun make-toggle-function (buffer-name buffer-create-fn &optional switch-cont)
  "Makes a toggle-function to have raise-or-create behaviour.

Creates a toggle-function that executes BUFFER-CREATE-FN if a
buffer named BUFFER-NAME doesn't exist, switches to the buffer
named BUFFER-NAME if it exists, and switches to the previous
buffer if we are currently visiting buffer BUFFER-NAME.

The SWITCH-CONT argument is a function which, if given, is called
after the buffer has been created or switched to.  This allows
running further actions that setup the state of the buffer or
modify it."
  (lambda ()
    (interactive)
    (let ((target-buf (get-buffer buffer-name)))
     (if target-buf
     (if (eq (current-buffer) target-buf)
         (progn
           (message "switching to other buffer")
           (switch-to-buffer nil))
         (progn
           (message "switching back...")
           (switch-to-buffer buffer-name)
           (when switch-cont (funcall switch-cont))))
       (message "creating buffer...")
       (funcall buffer-create-fn)
       (when switch-cont (funcall switch-cont))))))

(defun my/get-git-branch-name ()
  (magit-get-current-branch)
)

(defun my/wrike--extract-wrike-number ()
  (car (cdr (s-match "WRIKE-\\([[:digit:]]+\\)-\\(.+\\)" (magit-get-current-branch)))))

(defun my/wrike--extract-client-name ()
  (s-with (car (last (s-split "-" (magit-get-current-branch))))
    s-titleize
    (s-replace "_" " ")
    ))

(defun my/wrike-init-commit ()
  (interactive)
  (insert (concat
   "WRIKE-"
   (my/wrike--extract-wrike-number)
   "\n - "
   (my/wrike--extract-client-name)
   "\n - https://www.wrike.com/open.htm?id="
   (my/wrike--extract-wrike-number))
))

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

(defun my/sample (lst)
  "Return a random element of LST"
  (nth (random (length lst)) lst))

(defun parse-csv-to-array (contents)
  (parse-csv-string-rows contents ?\, ?\" "\n"))

(defun my/mac-javascript-exec (js &optional tab-descriptor)
  (do-applescript (concat "tell application \"Google Chrome\" to execute " (if tab-descriptor tab-descriptor "front window's active tab") " javascript \"" js "\"")))
