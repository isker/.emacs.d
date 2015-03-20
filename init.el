(require 'package)
;; M-x package-list-packages
;; i:install
;; d:delete
;; u:update
;; U:update all
;; x:execute actions

;; archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
			 ("marmalade" . "http://marmalade-repo.org/packages/")      
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


(setq required-packages 
      (list 
       'ack-and-a-half
       'auto-indent-mode
       'company
			 'company-go
       'expand-region
       'flx-ido
       'highlight-indentation
       'move-text
       'multiple-cursors
       'projectile
       'rainbow-delimiters
       'flycheck
       'web-mode
       'clojure-mode
       'paredit
       'python-mode
       'magit
			 'solarized-theme 
			 'automargin
			 'haskell-mode
			 'auctex
			 'go-mode
			 'js2-mode
       'smart-mode-line
       ))

;; install all required packages
(dolist (package required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

(require 'ack-and-a-half)
(require 'auto-indent-mode)
(require 'company-go) 
(require 'company)
(require 'expand-region)
(require 'flx-ido)
(require 'highlight-indentation)
(require 'move-text)
(require 'multiple-cursors)
(require 'paren)
(require 'projectile)
(require 'rainbow-delimiters)
(require 'cc-mode)
(require 'web-mode)
(require 'clojure-mode)
(require 'paredit)
;; (require 'python-mode)
(require 'magit)
(require 'automargin)
(require 'haskell-mode)
(require 'go-mode)
(require 'js2-mode)
(require 'smart-mode-line)

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

(setq x-select-enable-clipboard t)
(setq TeX-PDF-mode t)

;; backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list (number-sequence 4 200 4))   

;; eshell
(global-set-key (kbd "C-c C-s") 'eshell)
;; allow . expansion for executing programs
(defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
  (setq command (file-truename command)))

;; make eshell autocompletion like bash
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

;; disable startup gnu emacs buffer
(setq inhibit-startup-message t)  

;; scroll with cursor on edge, not shift page to center on cursor
(setq scroll-step 1)

;; delete current selection when characters typed
(delete-selection-mode t)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay .3)   ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)    ; remove annoying blinking

;; ace-jump-mode
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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

;; --- auto refresh buffers mode ---
;; auto refresh buffers on change
(global-auto-revert-mode t)

;; --- rainbow-delimiters mode ---
;;(global-rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'latex-mode-hook 'rainbow-delimiters-mode)

;; --- flycheck mode ---
(add-hook 'after-init-hook #'global-flycheck-mode)
;;(setq flycheck-check-syntax-automatically '(mode-enabled idle-change))
(setq flycheck-highlighting-mode 'lines)

;; --- haskell-mode ---
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

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
(setq erc-autojoin-channels-alist
      '(("irc.zulusquad.org" "#zulu")
        ("irc.rpis.ec" "#rpisec")))
(erc :server "irc.zulusquad.org" :port 6667 :nick "canned")
(erc :server "irc.rpis.ec" :port 6667 :nick "canned")

;;-------------------------------------------------------------------------------
;;; COSMETICS
;;-------------------------------------------------------------------------------

;; Mode line
(sml/setup)
(sml/apply-theme 'respectful)

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
(setq automargin-target-width 82)

;; solarized
;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-dark t)

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
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(foreground-color "#839496")
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/org/clubs/upe.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
