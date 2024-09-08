;; hide startup message
(setq inhibit-startup-message t)

;; disable tool bar
(tool-bar-mode -1)

;; enable line numbers globally
(when (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode 1))

;; nicer theme
(use-package sweet-theme
  :ensure t
  :init
  (load-theme 'sweet t))



(defun joindirs (root &rest dirs)
  "Joins a series of directories together,
               like Python's os.path.join
               (joindirs \"/a\" \"b\" \"c\") => /a/b/c"
  (if (not dirs)
      root
    (apply 'joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

;; add .bin/local to PATH variable the current
;; this is because I start emacs with
;; env HOME=$HOME/somefolder

(setenv "PATH" (concat (getenv "PATH") ":"
                       (joindirs (getenv "ORIG_HOME") ".bin" "local")))

;; get conda environment
(require 'json)

(defun get-conda-envs-dir ()
  "Get the primary directory where Conda environments are stored."
  (let* ((output (process-lines "conda" "info" "--json"))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (info (json-read-from-string (mapconcat 'identity output "\n")))
         (envs-dirs (gethash "envs_dirs" info)))
    (if envs-dirs
        (car envs-dirs)
      (error "Could not determine Conda environments directory"))))

;; set conda env as workon
(defun set-conda-envs-dir-as-workon ()
  "Set the Conda environments directory as the WORKON environment variable."
  (let ((conda-envs-dir (get-conda-envs-dir)))
    (setenv "WORKON_HOME" conda-envs-dir)
    (message "WORKON_HOME set to %s" conda-envs-dir)))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install and configure Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-indexing-method 'native)
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t))

;; Optional: Install and configure Helm for better interface
(use-package helm
  :ensure t
  :init
  (helm-mode 1))
(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on))

;; Install and configure Ivy, Counsel, and Swiper
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)))

;; Install and configure LSP Mode
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (js-mode . lsp)
         (go-mode . lsp))
  :commands lsp
  :config
  ;; Define the custom function
  (defun my/lsp-python-send-line ()
    "Send the current line to the inferior Python process."
    (interactive)
    (let ((current-line (thing-at-point 'line t)))
      (python-shell-send-string current-line)))

  ;; Bind the custom function to a key combination
  (global-set-key (kbd "C-l") 'my/lsp-python-send-line)

  ;; Use flycheck instead of flymake
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable t))

;; Install and configure Company for completions
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  (global-company-mode t))

;; Install and configure Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Install and configure Magit for Git integration
(use-package magit
  :ensure t
  :commands magit-status)

;; Install and configure Python-specific LSP server (pyright)
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; Install and configure pyvenv for virtual environment management
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)
  (setenv "WORKON_HOME" (get-conda-envs-dir)))


;; enable quelpa

;; Install quelpa if not already installed
(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)



(use-package dape
  :ensure t
  :quelpa (dape :fetcher github :repo "svaante/dape")
  :preface
  ;; Optionally, set the keybinding prefix. If you do not want any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints when quitting and load them when starting up
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)

  :init
  ;; Set the window configuration for dape (choose between 'gud' or 'right')
  ;; For window arrangement similar to gdb-mi, use 'gud'
  (setq dape-buffer-window-arrangement 'right)

  :config
  ;; Enable global mouse-based breakpoints (optional)
  (dape-breakpoint-global-mode)

  ;; Optionally, highlight the source line being executed (may affect performance)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Display additional info and/or REPL buffers when a stop event occurs
  (add-hook 'dape-stopped-hook 'dape-info)
  (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill the compile buffer when a build succeeds
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers when starting a debugging session
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; For Projectile users, set the working directory to the project root
  (setq dape-cwd-fn 'projectile-project-root)
)

;; Python-specific configuration

;; Set Conda environments directory
(set-conda-envs-dir-as-workon)

;; Configure Python shell to use IPython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Increase garbage collection threshold for performance
(setq gc-cons-threshold 80000000)

;; Adjust read-process-output-max for larger data from processes
(setq read-process-output-max (* 1024 1024))  ;; Set to 1MB

;; Make sure to enable eldoc-mode for hover information on variables 
(eldoc-mode 1)
;; repeat-mode for ergonomic keybindings
(repeat-mode 1)
