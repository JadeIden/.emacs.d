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

(defun my/org-file-by-date ()
  "Create an Org file with current time as name. Credit
https://emacs.stackexchange.com/questions/14673/emacs-function-to-make-a-file-with-date-time-as-filename-and-a-shortcut-for-it"
  (find-file (format-time-string "~/org/notes-%Y-%m-%d--%H-%M-%S.org")))

(defun parse-csv-to-array (contents)
  (parse-csv-string-rows contents ?\, ?\" "\n"))

(defun my/get-current-line ()
  "Return the line at point."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun my/smart-change-rest-of-line ()
  "Change until the symbol at end of line if it's `,', delete the symbol if cursor is at it, and delete the line otherwise."
  (interactive)
  (cond
   ((looking-at ",$") (replace-match "") (evil-insert-state))
   ((looking-at ".+\\(,\\)$") (save-excursion (replace-match "\\1") (evil-insert-state)))
   (t . ((call-interactively #'evil-change-line)))))

(defvar current-dt-format "%a %b %d %H:%M:%S %Z %Y")
(defun my/current-datetime ()
  "Get current time. Thanks to StackOverflow!"
  (format-time-string current-dt-format (current-time)))

(defun my/yank-updated-buffer ()
  "Update DEBUG_TIMESTAMP with the current time, then yank."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "DEBUG_TIMESTAMP = \".*\"" nil t)
      (replace-match (concat "DEBUG_TIMESTAMP = \"" (my/current-datetime) "\""))))
   (kill-new (buffer-string)))

(defun my/yank-buffer ()
  "Yanks the buffer to the clipboard."
  (interactive)
  (kill-new (buffer-string)))
