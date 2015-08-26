;; custom cache

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
	("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(safe-local-variable-values (quote ((c-hanging-comment-ender-p)))))

;; install and require packages

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)

(when (not package-archive-contents) (package-refresh-contents))

;; install and require a bunch of packages
(mapc
  (lambda (package)
    (progn
      (unless (package-installed-p package)
        (package-install package))
      (require package)))
  '( nrepl rainbow-delimiters cider ac-cider smartparens ;; lisp
     linum color-theme color-theme-sanityinc-tomorrow    ;; appearance
     projectile ido flx flx-ido iy-go-to-char            ;; search and nav
     clojure-mode php-mode coffee-mode))                 ;; major modes

;; begin setup

(color-theme-sanityinc-tomorrow-night)

(projectile-global-mode)
(setq projectile-enable-caching t)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(tool-bar-mode 0)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

;; go-to-char settings

(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c d") 'iy-go-to-char-backward)

;; line numbers

(setq linum-format "%4d")
(linum-mode 1)
(global-linum-mode 1)

;; php mode

(setq php-force-mode-pear 1)
(define-key php-mode-map (kbd "TAB") 'self-insert-command)

;; clojure mode

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'php-mode-hook (lambda () (electric-indent-local-mode -1)))

;; cider mode
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

;; uniquify for managing file names

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'forward
  uniquify-separator "/")

;; coffee mode


;; indentation (use tabs, not spaces)

(setq indent-tabs-mode 1)
(setq-default indent-tabs-mode 1)

(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

(setq-default c-electric-flag nil)
(set (make-local-variable 'electric-indent-mode) nil)

;; various emacs options

(setq backup-inhibited t)
(setq auto-save-default nil)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add newline to EOF
(setq require-final-newline t)

;; key bindings for various emacs fns
(global-set-key (kbd "C-x C-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-x C-v") 'clipboard-yank)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "M-f") 'rgrep)
(global-set-key (kbd "M-r") 'find-name-dired)
(global-set-key (kbd "C-x C-j") 'ace-jump-char-mode)

;; mac specific
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

(global-set-key (kbd "C-x C-q") 'connect-server)

(set-face-attribute 'default nil :height 120)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)

;; No splash screen
(setq inhibit-startup-message t)
