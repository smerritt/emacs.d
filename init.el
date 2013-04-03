;; install extra packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(starter-kit
    starter-kit-ruby
    starter-kit-lisp
    starter-kit-bindings
    slime
    slime-repl
    haml-mode
    sass-mode
    clojure-mode
    markdown-mode
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
(global-linum-mode 1)

(global-set-key [(f6)] 'next-error)
(global-set-key [(shift f6)] 'previous-error)
(global-set-key [(control \;) ?f ?f] 'ffip)
(global-set-key [(control tab)] 'hippie-expand)
(global-set-key [(shift tab)] 'hippie-expand)

;; C-' to start/end macro, C-M-' to run it
(defun start-or-end-kmacro ()
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))

(global-set-key [(control \')] 'start-or-end-kmacro)
(global-set-key [(control meta \')] 'kmacro-end-and-call-macro)

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

(defun random-ip ()
  (concat "10."
          (mapconcat
           (lambda (_) (format "%d" (random 256)))
           (make-list 3 'dontcare)
           ".")))

(defun insert-random-ip ()
  (interactive)
  (insert (random-ip)))

(defun random-mac ()
  (mapconcat
   (lambda (_) (format "%02X" (random 256)))
   (make-list 6 'dontcare)
   ":"))

(defun insert-random-mac ()
  (interactive)
  (insert (random-mac)))

(global-set-key [(control \;) ?r ?i] 'insert-random-ip)
(global-set-key [(control \;) ?r ?m] 'insert-random-mac)


(add-hook
 'html-mode-hook
 (lambda ()
   (set (make-local-variable 'sgml-basic-offset) 2)))
;; there ought to be a python starter kit
(defun python-insert-pdb-breakpoint ()
  (interactive)
  (indent-for-tab-command)
  (insert "import pdb; pdb.set_trace()  ### XXXXXXXXXXXXXXXXXXXXXXXX\n")
  (indent-for-tab-command))

(defun python-insert-dprint ()
  (interactive)
  (indent-for-tab-command)
  (insert "from common.pp import dprint\n")
  (indent-for-tab-command))

(add-hook 'python-mode-hook
          (lambda ()
            (setq fill-column 78)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (local-set-key [(control \;) ?b ?p] 'python-insert-pdb-breakpoint)
            (local-set-key [(control \;) ?d ?p] 'python-insert-dprint)))

;; Take all the windows in the current frame and shift them over one.
;;
;; With 2 windows, effectively switches their positions.
;;
;; With 1 window, this is a no-op.
(defun rotate-windows ()
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (mapcar* 'set-window-buffer
             (window-list)
             (append (cdr buffers) (list (car buffers))))))

(global-set-key [(control \;) ?r ?w] 'rotate-windows)

;; Kill region if active (for Darrell)
(defun kill-region-or-line ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line)))

(global-set-key [(control k)] 'kill-region-or-line)

;; Find everything, dammit (except stuff in .git)
(require 'find-file-in-project)
(add-to-list 'ffip-patterns "*.conf")
(add-to-list 'ffip-patterns "*.conf-sample")
(add-to-list 'ffip-patterns "*.css")
(add-to-list 'ffip-patterns "*.gitignore")
(add-to-list 'ffip-patterns "*.json")
(add-to-list 'ffip-patterns "*.list")
(add-to-list 'ffip-patterns "*.erb")
(add-to-list 'ffip-patterns "*.tmpl")
(add-to-list 'ffip-patterns "*.rst")
(add-to-list 'ffip-patterns "*.java")
(setq ffip-limit 2048)

;; Don't try to connect to something just because its name is under
;; point. It just locks up Emacs for a while and pisses me off.
(setq ido-use-filename-at-point nil)

(setq initial-scratch-message nil)
