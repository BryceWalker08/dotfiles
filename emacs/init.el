(load-theme 'doom-acario-dark t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Remove scrollbar on side
(tool-bar-mode -1) ; Remove toolbar on top
(tooltip-mode -1) ; Remove tooltips
(set-fringe-mode 10) ; Create breathing room

(setq visible-bell t) ; Set up visible bell

;; Set the font for all Emacs frames (including menus and toolbars)
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font-12"))

;; Make sure the menu bar and other UI elements use the same font
(when (fboundp 'set-frame-font)
  (set-frame-font "JetBrainsMono Nerd Font-12" nil t))

;; Set the font for specific parts of the Emacs UI (menu bar, tool bar, etc.)
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-12")
  (set-face-attribute 'mode-line nil :font "JetBrainsMono Nerd Font-12")
  (set-face-attribute 'mode-line-inactive nil :font "JetBrainsMono Nerd Font-12")
  (set-face-attribute 'menu nil :font "JetBrainsMono Nerd Font-12")
  (set-face-attribute 'tool-bar nil :font "JetBrainsMono Nerd Font-12")) 

;; Line Numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable Line Numbers in Some Modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Package Sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package on non-Linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Introduce ivy, a complete framework
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; introduce counsel, a companion to ivy
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-descibe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (global-unset-key (kbd "C-SPC"))
  
  (general-create-definer bryce/leader-keys
    :keymaps 'global-map
    :prefix "C-SPC")

  (bryce/leader-keys
   "M-t" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
	  "scale text"
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out")
	  ("f" nil "finished" :exit t))

(bryce/leader-keys
  "t" '(nil :which-key "text")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "[" '(previous-buffer :which-key "previous buffer")
  "]" '(next-buffer :which-key "next buffer"))

;; Projectile helps manage projects as well as enable some source control (Git) features
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projecile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)
 ; :custom
 ; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))








;;ivy auto added this, idk
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons counsel-projectile doom-modeline doom-themes general
		   helpful hydra ivy-rich magit magit-section
		   rainbow-delimiters transient vterm with-editor)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
