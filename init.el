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

;; use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

(use-package ace-window
  :ensure t)
(use-package ack-and-a-half
  :ensure t)
(use-package auto-indent-mode
  :ensure t)
(use-package avy
  :ensure t)
(use-package company-go
  :ensure t)
(use-package company
  :ensure t)
(use-package company-jedi
  :ensure t)
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package flx-ido
  :ensure t)
(use-package highlight-indentation
  :ensure t)
(use-package move-text
  :ensure t)
(use-package multiple-cursors
  :ensure t)
(use-package paren
  :ensure t)
(use-package projectile
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package cc-mode
  :ensure t)
(use-package web-mode
  :ensure t)
(use-package clojure-mode
  :ensure t)
(use-package paredit
  :ensure t)
(use-package python-mode
  :ensure t)
(use-package magit
  :ensure t
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-x m" . magit-status))
(use-package automargin
  :ensure t)
(use-package haskell-mode
  :ensure t)
;; (use-package haskell-interactive-mode
;;   :ensure t)
;; (use-package haskell-process		
;;   :ensure t)
(use-package go-mode
  :ensure t)
(use-package js2-mode
  :ensure t)
(use-package smart-mode-line
  :ensure t)
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
  ("C-c h o" . helm-occur))
;; (use-package helm-config		
;;   :ensure t)
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"                                      ;; personal snippets
          "~/.emacs.d/elpa/yasnippet-20150405.1526/snippets"         ;; the default collection
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

(use-package solarized-theme
  :init
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)
  :config
  ;; solarized
  (load-theme 'solarized-dark t)
)

;; movement
;;(global-set-key (kbd "C-m") 'back-to-indentation)

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

;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(push 'company-jedi company-backends)
(setq company-idle-delay .3)   ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)    ; remove annoying blinking

;; avy
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; ace-window
(global-set-key (kbd "M-p") 'ace-window)

;; --- ido mode ---
(ido-mode t)
(setq ido-everywhere t)

;; --- flx mode ---
;; flx ido fuzzy matching
(flx-ido-mode t)

;; disable ido faces to use flx highlighting
(setq ido-use-faces nil)

;; --- projectile mode ---
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; --- auto refresh buffers mode ---
(global-auto-revert-mode t)

;; --- rainbow-delimiters mode ---
;;(global-rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'latex-mode-hook 'rainbow-delimiters-mode)

;; --- flycheck mode ---
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled idle-change))
(setq flycheck-highlighting-mode 'lines)

;; --- haskell-mode ---
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


;; --- go mode ---
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; js2-mode
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; web mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))              
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; erc
;; Autojoin settings
;;(setq erc-autojoin-channels-alist
;;      '(("irc.zulusquad.org" "#zulu")
;;        ("irc.rpis.ec" "#rpisec")))
;;(erc :server "irc.zulusquad.org" :port 6667 :nick "canned[laptop]")
;;(erc :server "irc.rpis.ec" :port 6667 :nick "canned[laptop]")

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

;; automargin
(automargin-mode 1)
(setq automargin-target-width 100)


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
 '(background-color "#002b36")
 '(background-mode dark)
 '(column-number-mode t)
 '(company-global-modes (quote (not eshell-mode)))
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fill-column 80)
 '(foreground-color "#839496")
 '(helm-split-window-in-side-p t)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/org/clubs/upe.org")))
 '(standard-indent 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Mode line
(sml/setup)
(sml/apply-theme 'respectful)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
