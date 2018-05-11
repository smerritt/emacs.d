;; install extra packages
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(go-mode
		paredit
    clojure-mode
    markdown-mode
    twilight-theme
    color-theme
		smex
    unicode-fonts)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'twilight-theme)

;;;; Basic setup: colors, modifier keys, builtin behaviors, etc.
;; Emacs for OSX has this toolbar with stuff like save, open,
;; copy, paste... I'd rather have 2 more lines of text.
(tool-bar-mode 0)

(setq
 kill-whole-line t
 inhibit-startup-message t
 sentence-end-double-space nil
 initial-scratch-message nil
 confirm-kill-emacs 'y-or-n-p
 split-width-threshold 200
 )

(delete-selection-mode)
(random t)                              ; reseed
(server-start)
(global-auto-revert-mode 1)
(global-linum-mode 1)
(unicode-fonts-setup)
(windmove-default-keybindings)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key [(f6)] 'next-error)
(global-set-key [(shift f6)] 'previous-error)
(global-set-key [(control \;) ?f ?f] 'ffip)
(global-set-key [(control tab)] 'hippie-expand)
(global-set-key [(shift tab)] 'hippie-expand)
(global-set-key [(control c) ?r] 'revert-buffer)

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

;; just nice to have when writing code
(defun my-prog-mode-hook ()
	(column-number-mode)
	(setq tab-width 4)
	(show-paren-mode 1))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; visible bell on El Capitan is crap
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))


(defun random-ipv4-address ()
  (concat "10."
          (mapconcat
           (lambda (_) (format "%d" (random 256)))
           (make-list 3 'dontcare)
           ".")))

(defun insert-random-ipv4-address ()
  (interactive)
  (insert (random-ipv4-address)))


(defun random-ipv6-address ()
  (concat "fc00:"
          (mapconcat
           (lambda (_) (format "%x" (random 65536)))
           (make-list 7 'dontcare)
           ":")))

(defun insert-random-ipv6-address ()
  (interactive)
  (insert (random-ipv6-address)))

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

(defun insert-unix-time ()
  (interactive)
  (insert (number-to-string (float-time))))

(defun insert-nano-time ()
  (interactive)
  (insert (format-time-string "%s%N")))

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

(defun insert-random-uuid ()
	(interactive)
	(insert (format "%08x-%04x-%04x-%04x-%012x"
									(random (expt 16 8))
									(random (expt 16 4))
									(random (expt 16 4))
									(random (expt 16 4))
									(random (expt 16 12)))))


(global-set-key [(control \;) ?r ?i ?4] 'insert-random-ipv4-address)
(global-set-key [(control \;) ?r ?i ?6] 'insert-random-ipv6-address)
(global-set-key [(control \;) ?r ?h] 'insert-random-hash)
(global-set-key [(control \;) ?r ?m] 'insert-random-mac)
(global-set-key [(control \;) ?r ?n] 'insert-random-number)
(global-set-key [(control \;) ?r ?s] 'insert-random-string)
(global-set-key [(control \;) ?r ?u] 'insert-random-uuid)
(global-set-key [(control \;) ?u ?t] 'insert-unix-time)
(global-set-key [(control \;) ?n ?t] 'insert-nano-time)


(add-hook
 'html-mode-hook
 (lambda ()
   (set (make-local-variable 'sgml-basic-offset) 2)))

;;;; python stuff
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

(defun python-insert-pprint ()
  (interactive)
  (indent-for-tab-command)
  (insert "from pprint import pprint; pprint()")
  (backward-char))

(defun python-auto-constructor ()
	(interactive)
	(search-backward "self, ")
	(forward-char 6)
	(let ((start (point)))
		(search-forward ")")
		(backward-char 1)
		(let ((end (point)))
			(move-end-of-line nil)
			(insert "\n")
			(let (pyvar)
				(dolist (pyvar (split-string (buffer-substring start end) "[ \t\n]*,[ \t\n]*"))
					;; Handle keyword args; otherwise you get stuff like "self.x=None = x=None"
					(let ((equals-sign-idx (string-match-p "=" pyvar))
								(star-idx (string-match (regexp-quote "*") pyvar)))
						(cond ((numberp star-idx)) ;; do nothing for *args or **kwargs
									((null equals-sign-idx)  ;; no default for arg
									 (insert "self." pyvar " = " pyvar)
									 (indent-for-tab-command)
									 (insert "\n"))
									(t (let ((actual-pyvar (substring pyvar 0 equals-sign-idx)))
											 (insert "self." actual-pyvar " = " actual-pyvar)
											 (indent-for-tab-command)
											 (insert "\n"))))))))))

(defun my-python-mode-hook ()
	(setq fill-column 76)
	(setq python-fill-docstring-style 'django)
	(local-set-key (kbd "RET") 'newline-and-indent)
	(local-set-key [(control \;) ?a ?c] 'python-auto-constructor)
	(local-set-key [(control \;) ?b ?p] 'python-insert-pdb-breakpoint)
	(local-set-key [(control \;) ?q ?q] 'python-insert-q-q)
	(local-set-key [(control \;) ?p ?p] 'python-insert-pprint)
	(local-set-key [(control \;) ?d ?p] 'python-insert-dprint))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;;;; Elisp stuff
(defun my-emacs-lisp-mode-hook ()
	(paredit-mode)
	(setq tab-width 2))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; Take all the windows in the current frame and shift them over one.
;;
;; With 2 windows, effectively switches their positions.
;;
;; With 1 window, this is a no-op.
;;
;; With 3 or more windows, this confuses the user.
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
(add-to-list 'ffip-patterns "*.py")
(add-to-list 'ffip-patterns "*.go")
(setq ffip-limit 2048)

;; Don't look in .tox; it's never what I want
(add-to-list 'ffip-prune-patterns "*/.tox/*") ;; Python: tox virtualenvs
(add-to-list 'ffip-prune-patterns "*/*.egg-info/*") ;; Python: setup.py droppings
(add-to-list 'ffip-prune-patterns "*/doc/build/*") ;; Sphinx: built docs
(add-to-list 'ffip-prune-patterns "*/build/*") ;; Python: leftovers from "setup.py install"
(add-to-list 'ffip-prune-patterns "*/.eggs/*") ;; Python: more leftovers from "setup.py install"

(setq ffip-find-options (concat ffip-find-options " -not -regex \".*/.tox/.*\""))

;;;; ido-mode is basically magic
(ido-mode t)

(setq
 ;; Don't try to connect to something just because its name is under
 ;; point. It just locks up Emacs for a while and pisses me off.
 ido-use-filename-at-point nil

 ido-enable-flex-matching t
 )

(setq lua-indent-level 2)  ;; default is 3; who uses 3 spaces?

(require 'grep)
(add-to-list 'grep-find-ignored-directories ".venv")
(add-to-list 'grep-find-ignored-directories ".tox")
(add-to-list 'grep-find-ignored-directories "migrations")
(add-to-list 'grep-find-ignored-directories "build")


;; the default C indentation is pretty horrible
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq fill-column 78)
            (c-set-style "linux")
            (setq c-basic-offset 4)))

;;;; golang stuff
;; If you install your own Go environment, it typically ends up in
;; /usr/local/bin.
(setq exec-path (cons "/usr/local/bin" exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; goimports uses this to find my imports
(setenv "GOPATH" (concat (getenv "HOME") "/go"))

(defun go-if-err (preerr posterr)
  (let ((action (format "%serr%s" preerr posterr)))
    (dolist (line `("if err != nil {"
                    ,action
                    "}"))
      (insert line)
      (indent-for-tab-command)
      (newline))
    (delete-backward-char 1))) ; one too many newlines

(defun go-return-if-err ()
  (interactive)
  (go-if-err "return " ""))

(defun go-panic-if-err ()
  (interactive)
  (go-if-err "panic(" ")"))

(defun go-assert-return-if-err ()
  (interactive)
	(dolist (line `("if !assert.Nil(err) {"
									"return"
									"}"))
		(insert line)
		(indent-for-tab-command)
		(newline))
	(delete-backward-char 1)) ; one too many newlines

(defun my-go-mode-hook ()
  (setq tab-width 4)
	(setq fill-column 120)
  (setq gofmt-command "~/go/bin/goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-=") (lambda () (interactive) (insert ":=")))
	(local-set-key [(control \;) ?r ?e] 'go-return-if-err)
	(local-set-key [(control \;) ?p ?e] 'go-panic-if-err)
	(local-set-key [(control \;) ?a ?e] 'go-assert-return-if-err))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (whitespace-line-column . 80)
		 (ido-use-virtual-buffers . t)
     (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
