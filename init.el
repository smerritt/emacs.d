;; MELPA for packages. The GNU repos are enabled by default.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(terraform-mode protobuf-mode clang-format capnp-mode markdown-mode bazel exwm-mff yaml-mode typescript-mode find-file-in-project eglot pytest pytest-pdb-break python-black solarized-theme rust-mode))
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight regular :height 120 :width normal)))))
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
(add-to-list 'solarized-dark-color-palette-alist
	     '(base03 . "#000000")) ; shadow the grayish value for
				    ; base03
(add-to-list 'solarized-dark-high-contrast-palette-alist
	     '(base03 . "#000000"))

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
(menu-bar-mode -1)
(column-number-mode 1)

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

;; Don't minimize on anything; I pretty much never want to.
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

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
    (insert (format "%08x-%04x-4%03x-%04x-%012x"
                                    (random (expt 16 8))
                                    (random (expt 16 4))
                                    (random (expt 16 3))
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


(defun my/url-at-point (scheme)
  ;; Returns (start, end) buffer positions for the URL under point.
  ;; search-backward requires the whole match to be before point
  (let ((url-prefix (concat scheme "://")))
    (save-excursion
      (dotimes
	  (_ (length url-prefix))
	     (if (< (point) (point-max))
		   (forward-char)))
      (search-backward url-prefix)
      (let ((url-start (point)))
	(while (and (< (point) (point-max))
		    (not (string-match-p "[[:space:]]" (buffer-substring (point) (+ 1 (point))))))
	  (forward-char))
	(list url-start (point))))))

(defun jira-ticket-link ()
  (interactive)
  ;; Turns a Jira ticket URL at point into a Markdown-formatted link.
  ;;
  ;; Jira ticket URLs look like
  ;; https://jira.example.com/browse/PROJ-1234. The link text is
  ;; PROJ-1234.
  (let* ((pos (my/url-at-point "https"))
	 (url-start (car pos))
	 (url-end (cadr pos)))
    (goto-char url-end)
    (if (= (point) (point-max))
	(backward-char))
    (while (not (string= (buffer-substring (point) (+ 1 (point))) "/"))
      (backward-char))
    (let ((ticket-id (buffer-substring (+ 1 (point)) url-end)))
      ;; Edit in the markdown, working backwards so as not to perturb buffer offsets
      (goto-char url-end)
      (insert ")")
      (goto-char url-start)
      (insert "(")
      (goto-char url-start)
      (insert (concat "[" ticket-id "]")))))

(global-set-key [(control \;) ?j ?t] 'jira-ticket-link)

(defun gerrit-cr-link ()
  (interactive)
  ;; Turns a Gerrit URL at point into a Markdown-formatted link,
  ;; complete with ":gerrit:" Slack emoji.
  ;;
  ;; Gerrit CR URLs look like
  ;; https://gerrit.example.com/r/c/project/+/12345. The link text is
  ;; ":gerrit: 12345".
  (let* ((pos (my/url-at-point "https"))
	 (url-start (car pos))
	 (url-end (cadr pos)))
    (goto-char url-end)
    (if (= (point) (point-max))
	(backward-char))
    (while (not (string= (buffer-substring (point) (+ 1 (point))) "/"))
      (backward-char))
    (let ((cr-id (buffer-substring (+ 1 (point)) url-end)))
      ;; Edit in the markdown, working backwards so as not to perturb buffer offsets
      (goto-char url-end)
      (insert ")")
      (goto-char url-start)
      (insert "(")
      (goto-char url-start)
      (insert (concat "[:gerrit: " cr-id "]")))))

(global-set-key [(control \;) ?g ?l] 'gerrit-cr-link)


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

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

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

;; no require needed, apparently
(defun my-js-mode-hook ()
  ;; semistandard JS style rejects tabs, so let's just use spaces.
  (setq indent-tabs-mode nil)

  (setq tab-width 2)
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;;
;; C++ mode
;;

;; c-or-c++-mode tries its best, but when faced with an empty buffer,
;; it chooses C. This results in confusion, but then that confusion
;; goes away when you reopen the buffer, leaving you wondering if you
;; imagined things.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

(add-to-list 'auto-mode-alist '("\\.c\\+\\+\\'" . c++-ts-mode))


(defun my-c++-mode-hook ()
  (column-number-mode)

  ;; Try to match KJ style
  (setq fill-column 100)
  (setq c-basic-offset 2)
  (c-set-offset 'innamespace [0])
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-close 0)

  ;; I use this a lot, and "M-x co-at" is too long to type
  (keymap-local-set "<backtab>" 'completion-at-point)  ;; it's shift-tab

  (eglot-ensure)

  ;; If there's a .clang-format, format the buffer.
  (add-hook 'before-save-hook
	    (lambda ()
	      (when (locate-dominating-file "." ".clang-format")
		(clang-format-buffer)))
	    nil ; add in default position (head)
	    t ; buffer-local
	    ))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-ts-mode-hook 'my-c++-mode-hook)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(protobuf-mode . ("protobuf-language-server")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-17" "--background-index" "--header-insertion=never" "--query-driver=**")))
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd-17" "--background-index" "--header-insertion=never" "--query-driver=**")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp"))))

;; Define a C++ indentation style that more or less matches KJ style.
;;
;; You'll need this reference in order to understand any of this:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
;;
;; Also helpful: run M-x treesit-explore-mode so you can see what the node names are.
(defun kj-c++-ts-style ()
  ;; https://github.com/capnproto/capnproto/blob/v2/style-guide.md
  `(
    ;; Namespaces don't get any additional indenting. Why grandparent and not parent? The parse tree
    ;; is apparently:
    ;;
    ;; namespace_definition { declaration_list { ... declarations (classes, variables, etc.) ... }
    ;;
    ;; declaration_list is just a container for the declarations and has no actual
    ;; associated text. All the text is in one or more elements of the declaration_list,
    ;; even any comments. We could probably use the parent indentation here, but :shrug:
    ((n-p-gp nil "declaration_list" "namespace_definition") grand-parent 0)

    ;; Don't line up arguments with the open paren. The default has this:
    ;;
    ;; longFunc(foo, bar,
    ;;          baz, quux);
    ;;
    ;; The desired style is
    ;;
    ;; longFunc(foo, bar,
    ;;     baz, quux);
    ((parent-is "argument_list") parent-bol 4)

    ;; Same as above but for function params.
    ((parent-is "parameter_list") parent-bol 4)

    ;; Prepend all this to the GNU C++ style.
    ,@(alist-get 'gnu (c-ts-mode--indent-styles 'cpp))))



(add-hook
 'c++-ts-mode-hook
 (lambda ()
   ;; This sets up some treesitter variables that are needed for the
   ;; custom indentation to work at all.
   (c-ts-mode-set-style #'kj-c++-ts-style)))


;;
;; Rust mode
;;
(defun my-rust-mode-hook ()
  (setq indent-tabs-mode nil)
  (eglot-ensure))

(add-hook 'rust-mode-hook 'my-rust-mode-hook)


;;
;; Protobuf mode
;;
(require 'protobuf-mode)

(defun my-protobuf-mode-hook ()
  (eglot-ensure))

(add-hook 'protobuf-mode-hook 'my-protobuf-mode-hook)

;;
;; Go mode
;;

(require 'go-ts-mode)  ; puts entries for *.go *.go.mod in auto-mode-alist
(require 'eglot)  ; do this before messing with eglot-server-programs
(require 'bazel)  ; for workspace detection

;; gopls needs some options configured. Adapted from https://github.com/bazelbuild/rules_go/wiki/Editor-setup#neovim
(defun maybe-bazel-wrapped-gopls (unused--requested-interactively)
  "Returns a wrapped gopls that knows how to find packages
   and built files (say, protobuf generated .go files) where
   Bazel puts them.

   If the file is not in a Bazel workspace or the Bazel workspace
   doesn't have a dependency named @io_bazel_rules_go, the
   wrapper script does nothing.
  "
  ;; Experimentally, this function runs in the context of the buffer
  ;; with the Go code in it. That's not documented anywhere, but it's
  ;; what happens on Emacs 29.4, so I assume it's always true
  ;; (*sigh*). This means we can call buffer-file-name and figure out
  ;; if that's in a Bazel workspace or not.
  ;;
  ;; current-bazel-project is either an opaque struct or nil.
  (let ((current-bazel-project (bazel-find-project (directory-file-name (buffer-file-name)))))
    (if (not current-bazel-project)
	;; no bazel here, so no need for the wrapper or filters
	'("gopls")
      ;; current-project-dir is a path to the workspace root (e.g. the
      ;; directory with the "WORKSPACE" file in it).
      (let* ((current-project-dir (bazel-workspace-root current-bazel-project))
	     ;; Is current-project-dir an absolute or relative path?
	     ;; Does it have a trailing slash? We have no idea, so we
	     ;; have to do this ridiculous thing instead of just using
	     ;; file-name-nondirectory.
	     ;;
	     ;; This gets us the name, not path, of the directory
	     ;; containing the Bazel workspace.
	     (current-project-name (car (seq-filter
					 (lambda (s) (not (string-empty-p s)))
					 (reverse (file-name-split current-project-dir)))))
	     ;; Full path to gopls-wrapper, which is known to live in .emacs.d
	     (gopls-wrapper (file-name-concat (file-name-directory user-init-file) "gopls-wrapper")))
	(list gopls-wrapper
	      ;; By using a separate output base, gopls can run Bazel
	      ;; for its queries without blocking user-initiated
	      ;; builds, test runs, etc.
	      ;;
	      ;; This argument is just something that gopls-wrapper
	      ;; knows how to consume; it's not actually in Bazel.
	      "--gopls-wrapper-output-base"
	      (concat (getenv "HOME") "/tmp/bazel-gopls-" (replace-regexp-in-string "/" "_" (expand-file-name current-project-dir)))
	      :initializationOptions
	      ;; These options filter out a bunch of generated files
	      ;; that we don't care about, plus a bunch of symlinks
	      ;; back to the main repo (in bazel-${PROJECT}) that
	      ;; result in duplicate indexing.
	      (list :directoryFilters
		    (vector "-bazel-bin" "-bazel-out" "-bazel-testlogs"
			    (concat "-bazel-" current-project-name))))))))

(defun my--eq-or-contains (haystack-or-value needle)
  (if (listp haystack-or-value)
      (member needle haystack-or-value)
    (eq needle haystack-or-value)))

(setcdr (assoc 'go-ts-mode eglot-server-programs 'my--eq-or-contains)
	'maybe-bazel-wrapped-gopls)


(defun go-if-err-return-err (nparams)
  (interactive "p")
  (let ((start (point)))
    (insert "if err != nil {\n return ")
    (dotimes (_ (- nparams 1)) (insert "nil, "))
    (insert "err\n}")
    (indent-region-line-by-line start (point))))

(defun my-go-mode-hook ()
  (eglot-ensure)

  ;; I use this a lot, and "M-x co-at" is too long to type
  (keymap-local-set "<backtab>" 'completion-at-point)  ; it's shift-tab

  ;; This comes up a lot in Go
  (keymap-local-set "C-; r e" 'go-if-err-return-err)

  ;; Display tabs as two spaces
  (setq-local tab-width 2)

  ;; Wrap comments somewhere reasonable (the default of 70 is not reasonable).
  (setq-local fill-column 120)

  ;; When editing, use two spaces for indentation. This gets
  ;; overwritten with tabs by eglot-format-buffer, but at least it's
  ;; not visually jarring while I'm typing.
  (setq-local go-ts-mode-indent-offset 2)

  ;; Good grief, gopls is really slow in this huge repo at $JOB.
  (setq-local jsonrpc-default-request-timeout 300)

  )

(add-hook 'go-ts-mode-hook 'my-go-mode-hook)

;; Taken from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-eglot
;;
;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-before-save)
(add-hook 'go-ts-mode-hook #'eglot-format-buffer-before-save)

;;
;; Bazel configuration
;;
(defun my-bazel-mode-hook ()
  (setq-local bazel-buildifier-before-save t))
(add-hook 'bazel-mode-hook 'my-bazel-mode-hook)


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

;; Try to find the latest and greatest clang-format.
(defun find-newest-clang-format-in-dir (dir)
  (let ((clang-formats (seq-filter (lambda (fname) (or
						    (string-equal "clang-format" fname)
						    (string-match "\\`clang-format-[0-9]\\{1,\\}" fname)))
					; sort in reverse order so newest clang is on top
				   (if (file-directory-p dir)
				       (sort (directory-files dir) 'string>)
				     (list)))))
    (if (null clang-formats)
	nil
      (car clang-formats))))

(defun find-newest-clang-format ()
  (car
   (mapcar
    ;; turn ("/usr/bin" "clang-format-NN") into "/usr/bin/clang-format-NN"
    (lambda (l) (concat (car l) "/" (cadr l)))
    (sort
     (seq-filter

      ;; filter out entries not containing a clang-format
      (lambda (l) (not (null (cadr l))))

      ;; newest clang-format in each directory in $PATH, or nil
      (mapcar (lambda (dir) (list dir (find-newest-clang-format-in-dir dir))) exec-path))
     ;; sort the newest (i.e. lexically greatest) clang-format first, regardless of containing directory
     (lambda (l1 l2) (string> (cadr l1) (cadr l2)))))))


(let ((newest-clang-format (find-newest-clang-format)))
  ;; If we don't find anything, just leave clang-format-executable alone, I guess.
  (if (not (null newest-clang-format))
      (setq clang-format-executable newest-clang-format)))

;; treesit
(require 'treesit)

;; There are others out there, but these are some of the official ones,
;; where "official" means "under github.com/tree-sitter".
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	(c . ("https://github.com/tree-sitter/tree-sitter-c"))
	(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	(css . ("https://github.com/tree-sitter/tree-sitter-css"))
	(go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.21.2"))
	(gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))  ;; not official
	(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
	(html . ("https://github.com/tree-sitter/tree-sitter-html"))
	(java . ("https://github.com/tree-sitter/tree-sitter-java"))
	(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	(jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json"))
	(julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
	(python . ("https://github.com/tree-sitter/tree-sitter-python"))
	(regex . ("https://github.com/tree-sitter/tree-sitter-regex"))
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
	(rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	(scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
	(swift . ("https://github.com/tree-sitter/tree-sitter-swift"))
	(toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"))))
