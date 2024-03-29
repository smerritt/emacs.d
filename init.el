;; MELPA for packages. The GNU repos are enabled by default.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(clang-format capnp-mode markdown-mode bazel exwm-mff yaml-mode typescript-mode find-file-in-project eglot pytest pytest-pdb-break python-black solarized-theme rust-mode))
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(package-install-selected-packages)

;; SSH on Windows requires plink
(require 'tramp)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

;; Solarized is a nice theme, but the grayish background on an LCD
;; monitor is off-putting.
(require 'solarized-theme)
(load-theme 'solarized-dark t)
(set-background-color "black")

;; iswitchb is dead; long live ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq
 ;; Don't try to connect to something just because its name is under
 ;; point. It just locks up Emacs for a while and pisses me off.
 ido-use-filename-at-point nil
 ido-enable-flex-matching t
 )

;; GUI Emacs has this toolbar with stuff like save, open,
;; copy, paste... I'd rather have 2 more lines of text.
(tool-bar-mode 0)

;; Miscellaneous
(server-start)

(global-auto-revert-mode)

;; Don't litter backup files everywhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq
 kill-whole-line t
 inhibit-startup-message t
 sentence-end-double-space nil
 initial-scratch-message nil
 confirm-kill-emacs 'y-or-n-p
 split-width-threshold 200
 )

;; Show line numbers.
(global-display-line-numbers-mode)

;; Don't minimized on Ctrl-z (C-x C-z is fine, though)
(global-unset-key "\C-z")

;; Let me type over things
(delete-selection-mode)

;; Switch between windows with shift-arrow
(windmove-default-keybindings)

;; I never really want zap-to-char. I pretty much always want the
;; character that I'm zapping to.
(global-set-key [(meta z)] 'zap-up-to-char)


;; C-' to start/end macro, C-M-' to run it
(defun start-or-end-kmacro ()
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))
(global-set-key [(control \')] 'start-or-end-kmacro)
(global-set-key [(control meta \')] 'kmacro-end-and-call-macro)


;; Mac-isms. They do no harm on non-macs.
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; Utility functions for generating random data


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

;;
;; Python stuff
;;
(defun python-insert-pdb-breakpoint ()
  (interactive)
  (indent-for-tab-command)
  (insert "import pdb; pdb.set_trace()  ### XXXXXXXXXXXXXXXXXXXXXXXX\n")
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
  "
    Turns
      def __init__(self, a, b, c):

    into
      def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c
  "
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
    (setq python-fill-docstring-style 'pep-257-nn)
    (local-set-key (kbd "RET") 'newline-and-indent)
    (local-set-key [(control \;) ?a ?c] 'python-auto-constructor)
    (local-set-key [(control \;) ?b ?p] 'python-insert-pdb-breakpoint)
    (local-set-key [(control \;) ?q ?q] 'python-insert-q-q)
    (local-set-key [(control \;) ?p ?p] 'python-insert-pprint))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;;
;; Typescript and Javascript configuration
;;
(require 'typescript-mode)
(defun my-typescript-mode-hook ()
  (setq tab-width 2)
  (setq typescript-indent-level 2)
  (setq indent-tabs-mode nil))

(add-hook 'typescript-mode-hook 'my-typescript-mode-hook)

; no require needed, apparently
(defun my-js-mode-hook ()
  ;; semistandard JS style rejects tabs, so let's just use spaces.
  (setq indent-tabs-mode nil)
  
  (setq tab-width 2)
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;;
;; C++ mode
;;
(defun my-c++-mode-hook ()
  (column-number-mode)

  ;; Try to match KJ style
  (setq fill-column 115)
  (setq c-basic-offset 2)
  (c-set-offset 'innamespace [0])
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-close 0)
  (eglot-ensure))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-17" "--background-index" "--header-insertion=never" "--query-driver=**"))))

;;
;; Rust mode
;;
(defun my-rust-mode-hook ()
  (setq indent-tabs-mode nil)
  (eglot-ensure))

(add-hook 'rust-mode-hook 'my-rust-mode-hook)


;;
;; ffip configuration
;;

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
(add-to-list 'ffip-patterns "*.h")
(add-to-list 'ffip-patterns "*.c")
(add-to-list 'ffip-patterns "*.cpp")
(add-to-list 'ffip-patterns "*.c++")
(add-to-list 'ffip-patterns "*.bazel")
(setq ffip-limit 2048)

;; Don't look in .tox; it's never what I want
(add-to-list 'ffip-prune-patterns "*/.tox/*") ;; Python: tox virtualenvs
(add-to-list 'ffip-prune-patterns "*/*.egg-info/*") ;; Python: setup.py droppings
(add-to-list 'ffip-prune-patterns "*/doc/build/*") ;; Sphinx: built docs
(add-to-list 'ffip-prune-patterns "*/build/*") ;; Python: leftovers from "setup.py install"
(add-to-list 'ffip-prune-patterns "*/.eggs/*") ;; Python: more leftovers from "setup.py install"

;; Don't even generate filenames in .tox only to ignore them later; it's slow
(setq ffip-find-options (concat ffip-find-options " -not -regex \".*/.tox/.*\""))

;; CapnProto
(require 'capnp-mode)

;; clang-format
(require 'clang-format)

; Try to find the latest and greatest clang-format if the default is
; not there. This happens on Ubuntu if you install, say,
; clang-format-17 and remove clang-format. /usr/bin/clang-format is
; clang-format-14, and if you want anything newer, you have to name it
; explicitly.
(if (not (file-executable-p clang-format-executable))
    (let ((clang-formats (seq-filter (lambda (fname) (string-match "\\`clang-format-[0-9]\\{1,\\}" fname))
				     (sort (directory-files "/usr/bin") 'string>))))
      (if (not (null clang-formats))
	  (setq clang-format-executable (concat "/usr/bin/" (car clang-formats))))))
