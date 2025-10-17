;;; init.el --- Modern Python Development Configuration -*- lexical-binding: t; -*-

;; Package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Backup files configuration
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto-save files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-saves/") t)))

;; Create backup directories if they don't exist
(let ((backup-dir (expand-file-name "~/.emacs.d/backups"))
      (auto-save-dir (expand-file-name "~/.emacs.d/auto-saves")))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t)))

;; Tree-sitter setup for modern syntax highlighting
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;; Python mode with tree-sitter
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . tree-sitter-mode)
  :config
  (setq python-indent-offset 4)
  
  ;; Auto-detect and activate .venv
  (defun my/python-activate-venv ()
    "Automatically activate virtualenv in .venv directory."
    (interactive)
    (let* ((project-root (or (locate-dominating-file default-directory ".venv")
                            (locate-dominating-file default-directory "pyproject.toml")
                            (locate-dominating-file default-directory "setup.py")))
           (venv-path (when project-root
                       (expand-file-name ".venv" project-root))))
      (when (and venv-path (file-directory-p venv-path))
        (setq-local pyvenv-activate venv-path)
        (pyvenv-activate venv-path)
        (message "Activated virtualenv: %s" venv-path))))
  
  (add-hook 'python-mode-hook #'my/python-activate-venv))

;; pyvenv for virtualenv management
(use-package pyvenv
  :config
  (pyvenv-mode 1))

;; Eglot - Modern LSP client (built-in from Emacs 29)
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  ;; Try multiple Python LSP servers in order of preference
  ;; Pyright (most feature-rich), pylsp (good alternative), jedi-language-server (lightweight)
  (setq eglot-server-programs
        (assoc-delete-all 'python-mode eglot-server-programs))
  
  (add-to-list 'eglot-server-programs
               '(python-mode . ,(cond
                                 ((executable-find "pyright-langserver")
                                  '("pyright-langserver" "--stdio"))
                                 ((executable-find "pylsp")
                                  '("pylsp"))
                                 ((executable-find "jedi-language-server")
                                  '("jedi-language-server"))
                                 (t
                                  (message "No Python LSP server found. Install: npm install -g pyright OR pip install python-lsp-server")
                                  nil))))
  
  ;; Performance tuning
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)
  
  ;; Only enable eglot if an LSP server is available
  (defun my/eglot-ensure-with-check ()
    "Enable eglot only if a Python LSP server is available."
    (when (and (derived-mode-p 'python-mode)
               (or (executable-find "pyright-langserver")
                   (executable-find "pylsp")
                   (executable-find "jedi-language-server")))
      (eglot-ensure)))
  
  (add-hook 'python-mode-hook #'my/eglot-ensure-with-check)
  
  ;; Keybindings for common LSP operations
  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)))

;; Company for autocompletion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-tooltip-align-annotations t))

;; Flycheck for syntax checking with Ruff integration
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  
  ;; Define custom Ruff checker
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" "check"
              "--output-format=concise"
              "--stdin-filename" source-original
              "-")
    :standard-input t
    :error-patterns
    ((error line-start
            (file-name) ":" line ":" column ": " (one-or-more any) " "
            (message (one-or-more not-newline))
            line-end)
     (warning line-start
              (file-name) ":" line ":" column ": " (one-or-more any) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)
  
  (add-to-list 'flycheck-checkers 'python-ruff)
  
  ;; Use Ruff as the primary checker for Python
  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-checker 'python-ruff))))

;; Format buffer with ruff on save
(use-package reformatter
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args (list "format" "--stdin-filename" (buffer-file-name))
    :lighter " RuffFmt")
  
  (defun my/enable-ruff-format-on-save ()
    "Enable ruff formatting on save for Python buffers."
    (ruff-format-on-save-mode 1))
  
  (add-hook 'python-mode-hook #'my/enable-ruff-format-on-save))

;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Enhanced navigation and search
(use-package xref
  :ensure nil
  :config
  ;; Keybindings for jump-to-definition and find-references
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-?") 'xref-find-references)
  (global-set-key (kbd "M-,") 'xref-go-back))

;; Which-key for discoverability
(use-package which-key
  :config
  (which-key-mode))

;; Magit for git integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Modern UI improvements
(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)))

;; Rainbow delimiters for better readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight current line
(global-hl-line-mode 1)

;; Show line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Additional Python-specific keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-f") 'ruff-format-buffer)
  (define-key python-mode-map (kbd "C-c C-v") 'my/python-activate-venv))

;;; init.el ends here
