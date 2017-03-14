;; custom cache

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(org-agenda-files nil)
 '(safe-local-variable-values (quote ((c-hanging-comment-ender-p)))))

;; install and require packages

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents) (package-refresh-contents))

;; install and require a bunch of packages
(mapc
  (lambda (package)
    (progn
      (unless (package-installed-p package)
        (package-install package))
      (require package)))
  '( rainbow-delimiters cider ac-cider smartparens ;; lisp
     linum color-theme color-theme-sanityinc-tomorrow    ;; appearance
     projectile ido flx flx-ido iy-go-to-char            ;; search and nav
     hcl-mode terraform-mode clojure-mode php-mode
     coffee-mode flycheck))  ;; major modes

;; set the path as terminal path [http://lists.gnu.org/archive/html/help-gnu-emacs/2011-10/msg00237.html]
(setq explicit-bash-args (list "--login" "-i"))

;; fix the PATH variable for GUI [http://clojure-doc.org/articles/tutorials/emacs.html#osx]

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; org-mode
(setq org-log-done 'time)

;; begin setup

;;(color-theme-sanityinc-tomorrow-night)

(projectile-global-mode)
(setq projectile-enable-caching t)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(tool-bar-mode 0)
;;(toggle-scroll-bar -1)
(menu-bar-mode -1)

;; line numbers

;; php mode

(setq php-force-mode-pear 1)
(define-key php-mode-map (kbd "TAB") 'self-insert-command)
(add-hook 'php-mode-hook (lambda () (electric-indent-local-mode -1)))

;; python mode

(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-3 79)))
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-global-modes '(python-mode))

(defun check-syntax ()
  (when (eq major-mode 'python-mode)
    (flycheck-list-errors)))

(add-hook 'after-save-hook #'check-syntax)

;; clojure mode

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

;; paredit mode

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-p a") 'paredit-splice-sexp-killing-backward)
     (define-key paredit-mode-map (kbd "M-p b")  'paredit-splice-sexp-killing-forward)
     (define-key paredit-mode-map (kbd "M-p c")  'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-p d")  'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "M-k") 'kill-sexp)))

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

(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

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

;; copy/paste
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-x c") 'pbcopy)
(global-set-key (kbd "C-x v") 'pbpaste)
(global-set-key (kbd "C-x x") 'pbcut)

;; key bindings for various emacs fns
(global-set-key (kbd "C-x C-j") 'ace-jump-char-mode)

;; mac specific
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

(global-set-key (kbd "C-x C-q") 'connect-server)

(set-face-attribute 'default nil :height 150)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eruby-standard-face ((t (:foreground "goldenrod3")))))
(put 'downcase-region 'disabled nil)

;; No splash screen
(setq inhibit-startup-message t)
(put 'upcase-region 'disabled nil)

;; Easier window nav
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
