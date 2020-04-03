(require 'package)

;; archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			                   ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;;-------------------------------------------------------------------------------
;;; DEFINITIONS
;;-------------------------------------------------------------------------------

;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'find-user-init-file)

;; http://emacsredux.com/blog/2013/04/09/kill-whole-line/
(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

;;-------------------------------------------------------------------------------
;;; ENVIROMENT CONFIGURATION
;;-------------------------------------------------------------------------------

(setq gc-cons-threshold 20000000)

;; use-package
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(setq-default use-package-always-ensure t
              use-package-always-defer t)

(use-package ace-window
  :bind
  ("M-j" . ace-window))
(use-package avy
  :bind
  ("M-g w" . avy-goto-word-1))
(use-package company
  :hook (after-init . global-company-mode)
  :init
  ;; decrease delay before autocompletion popup shows
  (setq company-idle-delay .3)
  (setq company-tooltip-align-annotations t)
  ;; remove annoying blinking
  (setq company-echo-delay 0)
  :config)
(use-package company-go)
(use-package docker-tramp)
(use-package dired
  :ensure nil
  :init
  (defun ora-dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name
        "Rsync to:"
        (dired-dwim-target-directory)))))
    ;; store all selected files into "files" list
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          ;; the rsync command
          (tmtxt/rsync-command
           "rsync -arvz --progress "))
      ;; add all selected file names as arguments
      ;; to the rsync command
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      ;; append the destination
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      ;; run the async shell command
      (async-shell-command tmtxt/rsync-command "*rsync*")
      ;; finally, switch to that window
      (other-window 1))))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package eww
  :bind ("C-c e" . eww))
(use-package multiple-cursors
  :bind("C--" . mc/mark-next-like-this))
(use-package projectile
  :demand t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))
(use-package rainbow-delimiters
  :hook ((prog-mode latex-mode) . rainbow-delimiters-mode))
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled idle-change))
  (setq flycheck-highlighting-mode 'lines))
(use-package cc-mode)
(use-package web-mode
  :mode
  "\\.html?\\'"
  "\\.phtml\\'"
  "\\.tpl\\.php\\'"
  "\\.jsp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.vm\\'"
  "\\.jsx\\'"
  :init
  (setq web-mode-script-padding 2)
  )
(use-package clojure-mode)
(use-package aggressive-indent
  :hook (clojure-mode . aggressive-indent-mode))
(use-package clj-refactor
  :init
  (setq cljr-warn-on-eval nil)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook (clojure-mode . clj-refactor-mode))
(use-package cider
  :init
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (setq cider-prompt-for-symbol nil))
(use-package helpful
  :config
  (defalias 'describe-key 'helpful-key)
  (defalias 'describe-function 'helpful-callable)
  (defalias 'describe-variable 'helpful-variable)
  (defalias 'describe-symbol 'helpful-symbol))
(use-package paredit
  :hook ((emacs-lisp-mode clojure-mode eval-expression-minibuffer-setup) . paredit-mode))
;; The package is "python" but the mode is "python-mode"
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (add-hook 'python-mode-hook (lambda()
                                      (run-python "python"))))
(use-package magit
  :bind (("C-x m" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :init
  (setq vc-handled-backends nil)
  (setq magit-refresh-status-buffer nil)
  (when (eq system-type 'windows-nt)
    ;; https://github.com/magit/magit/wiki/Pushing-with-Magit-from-Windows
    (setenv "GIT_ASKPASS" "git-gui--askpass")))
;; (use-package automargin
;;   :init
;;   (setq automargin-target-width 100)
;;   :config
;;   (automargin-mode 1))
(use-package haskell-mode
  :init
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
(use-package go-mode
  :init
  (defun my-go-mode-hook ()
    ;; Use goimports instead of go-fmt
    (setq gofmt-command "goimports")
    ;; Call Gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    ;; Godef jump key binding
    (local-set-key (kbd "M-.") 'godef-jump))
  (add-hook 'go-mode-hook 'my-go-mode-hook)

  ;; autocompletion
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  )
(use-package exec-path-from-shell
  :defer nil
  :if (not (eq system-type 'windows-nt))
  :init
  (setq exec-path-from-shell-variables '("HOME" "GOPATH" "PATH" "MANPATH"))
  :config
  (exec-path-from-shell-initialize))
(use-package js2-mode
  :mode "\\.js\\'")
(use-package typescript-mode
  :mode "\\.ts\\'"
  :init
  (setq typescript-indent-level 2))
(use-package lsp-mode
  :hook ((js2-mode . lsp)
         (typescript-mode . lsp))
  :commands lsp
  :bind (("M-<RET>" . lsp-execute-code-action)))
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)
(use-package flx)
(use-package json-mode)
(use-package yaml-mode
  :mode "\\.yaml\\'")
(use-package ivy
  :hook (after-init . ivy-mode)
  :init
  ;; ivy claims to do this for you but I've had no such luck.
  (setq completing-read-function 'ivy-completing-read)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-extra-directories '("./"))
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-ignore-order)
          (t . ivy--regex-plus)))
  :config
  (ivy-add-actions
   'counsel-find-file
   '(("e" eww-open-file "eww"))))
(use-package ivy-rich
  :hook (ivy-mode . (lambda () (ivy-rich-mode 1)))
  :init (setq ivy-rich-path-style 'abbrev
              ivy-virtual-abbreviate 'abbreviate
              ivy-rich-switch-buffer-align-virtual-buffer t
              ivy-rich-switch-buffer-mode-max-length 10
              ivy-rich-switch-buffer-mode-max-length 50))
(use-package ivy-posframe
  :disabled
  :hook (ivy-mode . ivy-posframe-enable)
  :init
  ;; Different command can use different display function.
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
          (t               . ivy-posframe-display))))
(use-package counsel
  :init
  (setq counsel-find-file-at-point t)
  :config
  (setq ivy-initial-inputs-alist nil)
  :bind (("M-x" . counsel-M-x)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         :map counsel-find-file-map
         ("C-l" . counsel-up-directory)))
(use-package counsel-projectile
  :after projectile
  :demand t
  :config
  (counsel-projectile-mode))
(use-package counsel-tramp
  :bind (("C-c C-s . counsel-tramp")))
(use-package which-key
  :config (which-key-mode))
(use-package yasnippet
  :config
  (yas-global-mode 1))
;; (use-package auctex
;;   :init
;;   (setq TeX-PDF-mode t))
(use-package comment-dwim-2
  :config
  (define-key global-map [remap comment-dwim] 'comment-dwim-2))
(use-package eshell
  :demand t
  :bind (:map eshell-mode-map
              ([remap eshell-pcomplete] . completion-at-point))
  :init
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-history-size 100000)
  :config
  ;; allow . expansion for executing programs
  (defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
    (setq command (file-truename command))))
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))
(use-package aweshell
  :demand t
  :load-path "vendor/aweshell"
  :after (eshell)
  :init
  (setq-local company-auto-complete nil)
  (setq-local company-idle-delay 10)
  (setq aweshell-clear-buffer-key "M-c")
  :bind (:map eshell-mode-map
              ("M-e" . aweshell-new)
              ("M-}" . aweshell-next)
              ("M-{" . aweshell-prev)))
(use-package eshell-prompt-extras
  :after (aweshell)
  :init
  (setq eshell-highlight-prompt nil)
  (defun epe-theme-lambda-gitless ()
    "A eshell-prompt lambda theme.  Copied from the builtin one, but without git."
    (setq eshell-prompt-regexp "^[^#\nλ]*[#λ] ")
    (concat
     (when (epe-remote-p)
       (epe-colorize-with-face
        (concat (epe-remote-user) "@" (epe-remote-host) " ")
        'epe-remote-face))
     (when (and epe-show-python-info (bound-and-true-p venv-current-name))
       (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))
     (let ((f (cond ((eq epe-path-style 'fish) 'epe-fish-path)
                    ((eq epe-path-style 'single) 'epe-abbrev-dir-name)
                    ((eq epe-path-style 'full) 'abbreviate-file-name))))
       (epe-colorize-with-face (funcall f (eshell/pwd)) 'epe-dir-face))
     (when (epe-git-p)
       (concat
        (epe-colorize-with-face ":" 'epe-dir-face)
        (epe-colorize-with-face (epe-git-branch) 'epe-git-face)))
     (epe-colorize-with-face " λ" 'epe-symbol-face)
     (epe-colorize-with-face (if (= (user-uid) 0) "#" "") 'epe-sudo-symbol-face)
     " "))
  (setq eshell-prompt-function 'epe-theme-lambda-gitless))

(use-package solarized-theme
  :init
  (setq x-underline-at-descent-line t)
  ;; Use less bolding
  (setq solarized-use-less-bold t)
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-dark t))

(use-package ws-butler
  :init (ws-butler-global-mode 1))

(use-package org
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (set-fill-column 90)))
  (setq org-special-ctrl-a/e t)
  ;:bind
  ;("M-q" . toggle-truncate-lines)
  )

(use-package dumb-jump
  :init
  (dumb-jump-mode)
  (setq dumb-jump-force-searcher 'rg))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (setq rust-format-on-save t))

(use-package racer
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))
(use-package ediff
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))
(use-package multi-term
  :init
  (setq term-buffer-maximum-size 25000)
  ;; Most of these multi-term funs stolen from
  ;; https://github.com/aborn/multi-term-plus/blob/master/multi-term-plus.el
  ;; I've added more myself and made the preexisting ones less bad
  (defun multi-term-is-at-end-line ()
    (>= (line-number-at-pos) (count-lines (point-min) (point-max))))
  (defun multi-term-backward-char ()
    "Backward-char in term-mode. "
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (backward-char)
      (term-send-left)))
  (defun multi-term-forward-char ()
    "Forward-char in term-mode."
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (forward-char)
      (term-send-right)))
  (defun multi-term-backward-word ()
    "Backward-word in term-mode. "
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (backward-word)
      (term-send-backward-word)))
  (defun multi-term-forward-word ()
    "Forward-word in term-mode."
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (forward-word)
      (term-send-forward-word)))
  (defun multi-term-forward-kill-word ()
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (call-interactively 'kill-word)
      (term-send-forward-kill-word)))
  (defun multi-term-backward-kill-word ()
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (call-interactively 'backward-kill-word)
      (term-send-backward-kill-word)))
  (defun multi-term-move-beginning-of-line ()
    "Smart version of move-beginning-of-line in term-mode."
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (beginning-of-line)
      (term-send-raw)))
  (defun multi-term-move-end-of-line ()
    "Smart version of move-end-of-line in term-mode."
    (interactive)
    (if (not (multi-term-is-at-end-line))
        (move-end-of-line nil)
      (term-send-raw-string "\C-e")))
  (defun multi-term-kill-line ()
    "Smart kill-line in multi-term mode."
    (interactive)
    (kill-line)
    (if (multi-term-is-at-end-line)
        (term-send-raw-string "\C-k")))
  (defun term-send-yank ()
    "Yank in term mode."
    (interactive)
    (term-send-raw-string (current-kill 0))
    (yank))
  (defun multi-term-expand-region ()
	  "Wrap er/expand-region function in term-mode."
	  (interactive)
	  (er/expand-region 1))

  :config
  (setq term-char-mode-buffer-read-only nil)
  (setq term-char-mode-point-at-process-mark nil)
  (add-hook 'term-mode-hook
          (lambda ()
            (setq term-bind-key-alist
                  '(("M-f" . multi-term-forward-word)
                    ("M-b" . multi-term-backward-word)
                    ("<C-backspace>" . multi-term-backward-kill-word)
                    ("M-d" . multi-term-forward-kill-word)
                    ("C-f" . multi-term-forward-char)
                    ("C-b" . multi-term-backward-char)
                    ("C-d" . multi-term-delete-char)
                    ("C-k" . multi-term-kill-line)
                    ("C-y" . term-send-yank)
                    ("C-e" . multi-term-move-end-of-line)
                    ("C-a" . multi-term-move-beginning-of-line)
                    ("M-}" . multi-term-next)
                    ("M-{" . multi-term-prev)
                    ("C-c C-c" . term-interrupt-subjob)
                    ("C-c C-e" . term-send-esc)
                    ("C-p" . previous-line)
                    ("C-n" . next-line)
                    ("C-s" . isearch-forward)
                    ("C-r" . isearch-backward)
                    ("C-m" . term-send-return)
                    ("M-o" . term-send-backspace)
                    ("M-p" . term-send-up)
                    ("M-n" . term-send-down)
                    ("M-r" . term-send-reverse-search-history)
                    ("M-," . term-send-raw)
                    ("M-." . comint-dynamic-complete)
                    ("C-=" . multi-term-expand-region)))
            (yas-minor-mode -1))))
;; movement
(windmove-default-keybindings)

;; full screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; make splits horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
;;(setq tab-stop-list (number-sequence 4 200 4))

(setq x-select-enable-clipboard t)

;; backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; disable startup gnu emacs buffer
(setq inhibit-startup-message t)

;; scroll with cursor on edge, not shift page to center on cursor
(setq scroll-step 1)

;; delete current selection when characters typed
(delete-selection-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq mouse-yank-at-point t)

;;-------------------------------------------------------------------------------
;;; COSMETICS
;;-------------------------------------------------------------------------------

;; Font
(add-to-list 'default-frame-alist `(font . , "PragmataPro"))

;; Get rid of menu bars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Oh god the noise
(setq ring-bell-function 'ignore)

;; show line number in mode line
(line-number-mode t)

;; show column number in mode-line
(column-number-mode t)

;; disable menu bar
(menu-bar-mode -1)

(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

(setq fill-column 80)
(setq custom-safe-themes t)

(setq standard-indent 2)

(setq custom-file null-device)

(setq shell-command-dont-erase-buffer t)

(defun shell-command-print-separator ()
  (overlay-put (make-overlay (point-max) (point-max))
               'before-string
               (propertize "!" 'display
                           (list 'left-fringe
                                 'right-triangle))))
(advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)

(use-package smart-mode-line
  :config
  (sml/apply-theme 'respectful)
  :init
  (sml/setup)
  (setq sml/name-width 40)
  (setq sml/mode-width 'full))
