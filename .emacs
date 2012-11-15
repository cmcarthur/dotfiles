(add-to-list 'load-path "~/emacs")

(load-file "emacs/php-mode.el")
(load-file "emacs/color-theme.el")
(load-file "emacs/linum.el")
(load-file "emacs/coffee-mode.el")

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(require 'ido)
(require 'color-theme)
(require 'color-theme-solarized)
(require 'php-mode)
(require 'coffee-mode)
(require 'linum)
(require 'powerline)

(setq auto-mode-alist
  (append '(("\\.php$" . php-mode)
    ("\\.tpl$" . php-mode))
      auto-mode-alist))

(color-theme-solarized-dark)
(ido-mode 1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

;; Solarized config

(set-face-attribute 'mode-line nil
                    :foreground "#073642"
                    :background "#fdf6e3"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
					:foreground "#657b83"
					:background "#859900"
                    :box nil)

(setq linum-format " %d ")
(linum-mode 1)
(global-linum-mode 1)

(setq php-force-mode-pear 1)

(setq indent-tabs-mode 1)
(setq-default indent-tabs-mode 1)

(define-key php-mode-map (kbd "TAB") 'self-insert-command)

(setq backup-inhibited t)
(setq auto-save-default nil)

(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

(setq-default c-electric-flag nil)

(global-set-key (kbd "C-x C-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-x C-v") 'clipboard-yank)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "M-f") 'rgrep)
(global-set-key (kbd "M-r") 'find-name-dired)

;; add functions to connect to data servers
(defun remote-term (new-buffer-name cmd &rest switches)
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))

(defun connect-server()
  (interactive)
  ((lambda (name)
    (remote-term name "ssh" (concat "rjmdash@" name)))
  (read-from-minibuffer "Server name: " (first query-replace-defaults))))

(defun connect-vm ()
  (interactive)
  (remote-term "vmdev" "ssh" "rjmdash@vmdev"))

(global-set-key (kbd "C-x C-q") 'connect-server)

