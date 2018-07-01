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

(use-package ace-window
  :ensure t
  :bind
  ("M-j" . ace-window))
;; (use-package auto-indent-mode
;;   :ensure t)
(use-package avy
  :ensure t
  :bind
  ("M-g w" . avy-goto-word-1))
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  ;; decrease delay before autocompletion popup shows
  (setq company-idle-delay .3)
  (setq company-tooltip-align-annotations t)
  ;; remove annoying blinking
  (setq company-echo-delay 0)
  :config
  ;; (push 'company-jedi company-backends)
  )
(use-package company-go
  :ensure t)
;; (use-package company-jedi
;;   :ensure t)
(use-package dired
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
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package multiple-cursors
  :ensure t
  :bind("C--" . mc/mark-next-like-this))
(use-package projectile
  :ensure t
  :init 
  (projectile-mode))
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode latex-mode) . rainbow-delimiters-mode))
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled idle-change))
  (setq flycheck-highlighting-mode 'lines))
(use-package cc-mode
  :ensure t)
(use-package web-mode
  :ensure t
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
(use-package clojure-mode
  :ensure t)
(use-package aggressive-indent
  :ensure t
  :hook (clojure-mode . aggressive-indent-mode))
(use-package clj-refactor
  :ensure t
  :init
  (setq cljr-warn-on-eval nil)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook (clojure-mode . clj-refactor-mode))
(use-package cider
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (setq cider-prompt-for-symbol nil))
(use-package helpful
  :ensure t
  :config
  (defalias 'describe-key 'helpful-key)
  (defalias 'describe-function 'helpful-callable)
  (defalias 'describe-variable 'helpful-variable)
  (defalias 'describe-symbol 'helpful-symbol))
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode eval-expression-minibuffer-setup) . paredit-mode))
;; The package is "python" but the mode is "python-mode"
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (add-hook 'python-mode-hook (lambda()
                                      (run-python "python"))))
(use-package magit
  :ensure t
  :bind ("C-x m" . magit-status)
  :init
  (setq vc-handled-backends nil)
  (setq magit-refresh-status-buffer nil))
;; (use-package automargin
;;   :ensure t
;;   :init
;;   (setq automargin-target-width 100)
;;   :config
;;   (automargin-mode 1))
(use-package haskell-mode
  :ensure t
  :init
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
(use-package go-mode
  :ensure t
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
  :ensure t
  :init
  (setq exec-path-from-shell-variables '("HOME" "GOPATH" "PATH" "MANPATH"))
  :config
  (exec-path-from-shell-initialize)
  )
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")
(use-package json-mode
  :ensure t)
(use-package flx :ensure t)
(use-package ivy
  :ensure t
  :init
  (setq ivy-mode 1)
  ;; ivy claims to do this for you but I've had no such luck.
  (setq completing-read-function 'ivy-completing-read)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-extra-directories nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          (t . ivy--regex-plus))))
(use-package counsel
  :ensure t
  :init
  (setq counsel-find-file-at-point t)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map counsel-find-file-map
         ("C-l" . counsel-up-directory)))
(use-package which-key
  :ensure t
  :config (which-key-mode))
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
;; (use-package auctex
;;   :ensure t
;;   :init
;;   (setq TeX-PDF-mode t))
(use-package comment-dwim-2
  :ensure t
  :config
  (define-key global-map [remap comment-dwim] 'comment-dwim-2))
(use-package eshell
  :bind
  ("M-e" . eshell)
  :config
  
  ;; allow . expansion for executing programs
  (defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
    (setq command (file-truename command)))

  (add-hook 'eshell-mode-hook (lambda()
                                (yas-minor-mode -1)))
  (add-hook 'eshell-mode-hook (lambda()
                                (company-mode -1)))

  ;; make eshell autocompletion like bash
  ;;(add-hook
  ;; 'eshell-mode-hook
  ;; (lambda ()
  ;;   (setq pcomplete-cycle-completions nil)))

  ;; eshell beginning of line is end of prompt
  (add-hook
   'eshell-mode-hook
   '(lambda ()
      (local-set-key (kbd "C-a") 
                     '(lambda ()
                        (interactive)
                        (beginning-of-line)
                        (search-forward-regexp eshell-prompt-regexp))))))

(use-package color-theme-solarized
  :ensure t
  :init
  (setq color-themes `())
  (setq frame-background-mode 'dark)
  (set-terminal-parameter nil 'background-mode 'dark)
  (load-theme 'solarized t))

(use-package ws-butler
  :ensure t)

(use-package org
  :ensure t
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (set-fill-column 90)))
  ;:bind
  ;("M-q" . toggle-truncate-lines)
  )

(use-package dumb-jump
  :ensure t
  :init
  (dumb-jump-mode)
  (setq dumb-jump-force-searcher 'rg))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (setq rust-format-on-save t))

(use-package racer
  :ensure t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))
(use-package ediff
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))
(use-package multi-term
  :ensure t
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

;; --- auto refresh buffers mode ---
(global-auto-revert-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq mouse-yank-at-point t)

;;-------------------------------------------------------------------------------
;;; COSMETICS
;;-------------------------------------------------------------------------------

;; Font
(add-to-list 'default-frame-alist `(font . , "Essential PragmataPro"))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-global-modes (quote (not eshell-mode)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fill-column 80)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/org/clubs/upe.org")))
 '(org-special-ctrl-a/e t)
 '(package-selected-packages
   (quote
    (helpful json-mode multi-term cargo flycheck-rust racer rust-mode yasnippet ws-butler web-mode use-package smart-mode-line rainbow-delimiters projectile paredit multiple-cursors magit js2-mode haskell-mode flycheck expand-region exec-path-from-shell company-jedi company-go comment-dwim-2 color-theme-solarized clojure-mode automargin ace-window)))
 '(standard-indent 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Put this after custom-safe-themes so that it stops complaining about
;; untrusted color themes.
(use-package smart-mode-line
  :ensure t
  :config
  (sml/apply-theme 'respectful)
  :init
  (sml/setup)
  (setq sml/name-width 40)
  (setq sml/mode-width 'full))
