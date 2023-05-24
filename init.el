(defvar config-root "~/.config/emacs")

;; Sanity

(defvar path-spec
  '((savehist-file . "var/history")
    (auto-save-list-file-prefix . "var/auto-save-list/.saves-")
    (save-place-file . "var/places")
    (bookmark-default-file . "var/bookmarks")
    (tramp-persistency-file-name . "var/tramp")))

(dolist (p path-spec)
  (let* ((var (car p))
     (path (expand-file-name (cdr p) config-root)))
    (customize-set-variable var path)))

(setq auto-save-file-name-transforms `((".", "~/.config/emacs/var/auto-save" t)))
(setq auto-save-file-name-transforms '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.con\\2" t)))
(setq backup-directory-alist '(("." . "~/.config/emacs/var/backups")))
(with-eval-after-load 'tramp
  (add-to-list 'tramp-backup-directory-alist
               (cons tramp-file-name-regexp nil)))

(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
  '(kill-ring
    search-ring
    regexp-search-ring))

(display-time)
(display-battery-mode)

;; Tabs no more
(setq-default tab-width 4 indent-tabs-mode nil)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; sane electric
(setq-default electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'hungry)

; selected text will be overwritten when you start typing
(delete-selection-mode 1)

(use-package persistent-scratch
:init
(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1))

;; Undo-tree

(use-package undo-tree
:config (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/var/undo-tree/")))
:init (global-undo-tree-mode)
:bind (("C-z" . undo-tree-undo)
     ("C-S-z" . undo-tree-redo)))

;; Magit

(use-package magit
:ensure t
:bind (("C-M-g" . magit-status)))

(use-package sudo-edit)

(setq select-enable-clipboard t
  save-interprogram-paste-before-kill t
  yank-pop-change-selection t)

;; Do not allow the cursor in the minibuffer prompt

(setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq use-short-answers t)

;; Editing

(setq sentence-end-double-space nil)

;; Completion

(customize-set-variable 'completion-cycle-threshold 3)
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'completions-detailed t)

;; capf backend for fuzzy-like matching
(use-package orderless
:init
(setq completion-styles '(orderless basic partial-completion)
completion-category-defaults nil
completion-category-overrides '((file (styles partial-completion)))))

;; rich annotations for vertico etc.
(use-package marginalia
:init (marginalia-mode))

;; vertical competion for M-x, find-file etc.
(use-package vertico
:init (vertico-mode))

;; vertical completion at point
(use-package corfu
:straight (corfu :files (:defaults "extensions/*")
       :includes (corfu-history corfu-popupinfo))
:config
(setq corfu-popupinfo-delay t
    corfu-cycle t
    corfu-auto t
    corfu-auto-prefix 2
    corfu-auto-delay 0.0)
:init
(global-corfu-mode 1)
(corfu-popupinfo-mode 1))

;; modeline
(use-package simple-modeline
:hook (after-init . simple-modeline-mode))

;; (use-package minions)

;; cheat sheet
(use-package which-key
:init (which-key-mode))

;; Languages

;; lsp
(use-package eglot
:hook ((js-mode python-mode css-mode go-mode) . eglot-ensure)
:bind (:map eglot-mode-map
          ("C-c r" . 'eglot-rename)
          ("C-c o" . 'eglot-code-action-organize-imports)
          ("C-c h" . 'eldoc)))

(use-package sly
:commands sly
:defer t
:config (progn (setq inferior-lisp-program "sbcl")
             ;(setq sly-contribs '(sly-scratch sly-mrepl sly-stickers))
             (sly-setup))
:bind (:map sly-mode-map
          ("C-c b" . sly-eval-buffer)))

(use-package tree-sitter-langs)
(use-package tree-sitter
:init (global-tree-sitter-mode))

(use-package parinfer-rust-mode)

(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package nix-mode)
(use-package nginx-mode)

(use-package poetry)
(use-package direnv
:config (direnv-mode))

(use-package ein
:config (setq ein:output-area-inlined-images t
            ein:output-type))
(use-package web-mode)

;; Misc

(use-package toolbox-tramp
:straight (toolbox-tramp :type git
           :host github
           :repo "fejfighter/toolbox-tramp"))

;; UI

(setq scroll-margin 0
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

(show-paren-mode 1)

(use-package diminish
:config
(mapc #'diminish (list 'eldoc-mode 'visual-line-mode 'wrap 'undo-tree-mode
                     'helm-mode)))


;; Keys

(when (eq system-type 'darwin)
  ;; things specific to mac keyboard
  (setq mac-command-modifier 'meta
      mac-right-command-modifier 'meta
      mac-option-modifier nil
      mac-right-option-modifier nil))

(defun k60:interactive-find-file (filename)
  (interactive)
  (find-file filename))

(defun edit-init.el ()
  (interactive)
  (find-file
   (expand-file-name "init.el" user-emacs-directory)))

(defun edit-early-init.el ()
  (interactive)
  (find-file
   (expand-file-name "early-init.el" user-emacs-directory)))

(defun edit-nixos-config ()
  (interactive)
  (find-file
   "/sudo:root@localhost:/etc/nixos/configuration.nix"))

(defun edit-home-config ()
  (interactive)
  (find-file "/home/k60/.config/nixpkgs/home.nix"))

(mapc (lambda (x) (global-set-key (kbd (car x)) (cdr x)))
      '(("C-z"     . undo)
        ("M-z"     . zap-up-to-char)
        ("C-j"     . join-line)
        ("C-x g"   . magit-status)
        ("C-c a"   . org-agenda)
        ("C-c e"   . eshell)
        ("<f6>"    . compile)
        ("C-x m"   . mu4e-compose-new)
        ("<f5>"    . recompile)
        ("C->"     . indent-rigidly-right-to-tab-stop)
        ("C-<"     . indent-rigidly-left-to-tab-stop)
        ("M-["     . backward-paragraph)
        ("M-]"     . forward-paragraph)
        ("M-x"     . execute-extended-command)
        ("C-<tab>" . previous-buffer)
        ("C-]"     . next-buffer)
        ("C-x C-f" . find-file)
        ("C-c k e" .   edit-early-init.el)
        ("C-c k i" .   edit-init.el)
        ("C-c k n" .   edit-nixos-config)
        ("C-c k h" .   edit-home-config)
        ("C-x b"   . switch-to-buffer)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; cyrillic
(use-package reverse-im
  :ensure t
  :config
  (reverse-im-activate "russian-computer"))

;; emms
(use-package emms
  :init (progn (emms-all)
               (setq emms-player-list '(emms-player-mpv)
                     emms-source-file-default-directory "/home/k60/Music/library")))


;; tramp

(setq remote-file-name-inhibit-cache nil) ; keep

(tramp-set-completion-function "ssh"
 (append (tramp-get-completion-function "ssh")
         '((tramp-parse-sconfig "/var/home/k60/.ssh/config-rost")
           (tramp-parse-sconfig "/var/home/k60/.ssh/config-private"))))

;; use remote's path too
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("28a34dd458a554d34de989e251dc965e3dc72bace7d096cdc29249d60f395a82" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil))))
