;; init.el — Bootstrap for config.org
;; ----------------------------------------
;; Package setup
;; ----------------------------------------
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ----------------------------------------
;; Paths to configuration files
;; ----------------------------------------
(defvar my/config-org-file "~/.config/emacs/config.org"
  "Main Emacs configuration Org file.")

(defvar my/config-el-file "~/.config/emacs/config.el"
  "Tangled Emacs Lisp file generated from config.org.")

;; ----------------------------------------
;; Function to tangle config.org automatically
;; ----------------------------------------
(defun my/tangle-config-org ()
  "Tangle config.org to config.el if the org file has been modified."
  (when (string-equal (buffer-file-name)
                      (expand-file-name my/config-org-file))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; Hook to auto-tangle on save
(add-hook 'after-save-hook 'my/tangle-config-org)

;; ----------------------------------------
;; Tangle and load config.org at startup
;; ----------------------------------------
(when (file-exists-p my/config-org-file)
  ;; Visit the org file temporarily to tangle properly
  (with-current-buffer (find-file-noselect my/config-org-file)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))
  ;; Load the tangled config.el if it exists
  (when (file-exists-p my/config-el-file)
    (load-file my/config-el-file)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
