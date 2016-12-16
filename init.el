(require 'package)

;; archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
			 ("marmalade" . "http://marmalade-repo.org/packages/")      
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
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

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
  ;; remove annoying blinking
  (setq company-echo-delay 0)
  :config
  ;; (push 'company-jedi company-backends)
  )
(use-package company-go
  :ensure t)
;; (use-package company-jedi
;;   :ensure t)
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package multiple-cursors
  :ensure t
  :bind("C--" . mc/mark-next-like-this))
(use-package projectile
  :ensure t
  :init 
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  ;; (helm-projectile-on)
  )
(use-package rainbow-delimiters
  :ensure t
  :init  
  ;;(global-rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'latex-mode-hook 'rainbow-delimiters-mode))
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
(use-package paredit
  :ensure t
  :init)
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
(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  
  ;; Allow helm-man-or-woman to use the symbol at point for man pages
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :bind  
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  ("C-x C-f" . helm-find-files)
  ;; TODO: Why doesn't this work?
  ;;("C-c h o" . helm-occur)
  )
;; (use-package helm-config		
;;   :ensure t)
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"                                      ;; personal snippets
          "~/.emacs.d/elpa/yasnippet-20150405.1526/snippets"         ;; the default collection
          "~/.emacs.d/snippets/yasnippet-go"
          ))
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

  (autoload 'helm-eshell-history "helm-eshell"    t)
  (autoload 'helm-esh-pcomplete  "helm-eshell"    t)
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map
                  [remap eshell-pcomplete]
                  'helm-esh-pcomplete)))
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
  (load-theme 'solarized t)
  (setq frame-background-mode 'dark)
  (set-terminal-parameter nil 'background-mode 'dark)
  (setq solarized-termcolors 256)
  :config
  )

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

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  )

(use-package racer
  :ensure t
  :init
  (setq racer-rust-src-path "/usr/local/src/rustc-1.10.0/src")
  (setq racer-cmd "/home/ian/.cargo/bin/racer")
  ;; (add-hook 'racer-mode-hook #'eldoc-mode)
  )

(use-package company-racer
  :ensure t
  :init
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  )

;; movement
;;(global-set-key (kbd "C-m") 'back-to-indentation)
(windmove-default-keybindings)

;; full screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; make splits horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 800)

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
 '(helm-split-window-in-side-p t)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/org/clubs/upe.org")))
 '(org-special-ctrl-a/e t)
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
