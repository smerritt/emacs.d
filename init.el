;; install extra packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages
  '(starter-kit
    starter-kit-ruby
    starter-kit-lisp
    starter-kit-bindings
    slime
    slime-repl
    haml-mode
    sass-mode
    color-theme
    color-theme-twilight)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'color-theme)
(dolist (theme-dir (directory-files package-user-dir t "color-theme-\[a-z\]"))
  (mapcar 'load-file (directory-files theme-dir t "\\.el$")))

;;;; Basic setup: colors, modifier keys, builtin behaviors, etc.
(setq kill-whole-line t)
(setq confirm-kill-emacs 'yes-or-no-p)
(delete-selection-mode)
(random t)                              ; reseed
(server-start)
(global-auto-revert-mode 1)

(global-set-key [(f6)] 'next-error)
(global-set-key [(shift f6)] 'previous-error)
(global-set-key [(control \;) ?f ?f] 'ffip)

;; on kinesis freestyle, 'Delete' sends kp-delete
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(control kp-delete)] 'kill-word)

;; Mac-isms. They do no harm on non-macs.
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; don't iconify on C-z when running in X
;; or exit emacs (!) when running in Emacs.app
(when window-system (global-unset-key "\C-z"))

;; while I <square box> Unicode as much as the next guy,
;; I want my lambdas left alone.
(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)

;; just nice to have everywhere; my screen is only so wide
(add-hook 'prog-mode-hook (lambda () (setq tab-width 2)))
