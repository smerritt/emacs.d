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
    twilight-theme
    color-theme
    unicode-fonts)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'twilight-theme)

;;;; Basic setup: colors, modifier keys, builtin behaviors, etc.
(setq kill-whole-line t)
(setq confirm-kill-emacs 'yes-or-no-p)
(delete-selection-mode)
(random t)                              ; reseed
(server-start)
(global-auto-revert-mode 1)
(global-linum-mode 1)
(unicode-fonts-setup)

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

(defun insert-random-number ()
  (interactive)
  (insert (number-to-string (random 9999999))))

(defun insert-time ()
  (interactive)
  (insert (number-to-string (float-time))))

(defun insert-random-hash ()
  ;; 128 random bits; looks sort of MD5-ish
  (interactive)
  (let (_dummy)  ;; appease dotimes
    (dotimes (_dummy2 16 _dummy)
      (insert (format "%02x" (random 255))))))

(defun insert-random-string ()
  (interactive)
  (insert (shell-command-to-string
           (concat (getenv "HOME") "/bin/random-string -n"))))


(global-set-key [(control \;) ?r ?i] 'insert-random-ip)
(global-set-key [(control \;) ?r ?h] 'insert-random-hash)
(global-set-key [(control \;) ?r ?m] 'insert-random-mac)
(global-set-key [(control \;) ?r ?n] 'insert-random-number)
(global-set-key [(control \;) ?r ?s] 'insert-random-string)
(global-set-key [(control \;) ?r ?t] 'insert-time)


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

(defun python-insert-q-q ()
  (interactive)
  (indent-for-tab-command)
  (insert "import q; q.q()")
  (backward-char))

(add-hook 'python-mode-hook
          (lambda ()
            (setq fill-column 76)
            (setq python-fill-docstring-style 'django)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (local-set-key [(control \;) ?b ?p] 'python-insert-pdb-breakpoint)
            (local-set-key [(control \;) ?q ?q] 'python-insert-q-q)
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

(setq lua-indent-level 2)  ;; default is 3; who uses 3 spaces?

(require 'grep)
(add-to-list 'grep-find-ignored-directories ".venv")
(add-to-list 'grep-find-ignored-directories "migrations")


;; the default C indentation is pretty horrible
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq fill-column 78)
            (c-set-style "linux")
            (setq c-basic-offset 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8) (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
