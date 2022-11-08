;;; config.el -- Example Crafted Emacs user customization file -*- lexical-binding: t; -*-
;; This file is generated from config.org. If you want to edit the
;; configuration, DO NOT edit config.el, edit config.org, instead.

;; Tangle the code blocks to config.el on save.
(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "config.org" crafted-config-path))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle-config)))

(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-python)      ; python, needs customization
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.
(require 'crafted-evil)        ; An `evil-mode` configuration
(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.
(require 'crafted-project)     ; built-in alternative to projectile
(require 'crafted-speedbar)    ; built-in file-tree
(require 'crafted-screencast)  ; show current command and binding in modeline
(require 'crafted-compile)     ; automatically compile some emacs lisp files

(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "Iosevka SS04 14" :weight semi-light))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Iosevka Aile 14" :weight semi-light)))))))

(crafted-package-install-package 'doom-themes)
(progn
  (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
  (load-theme 'doom-homage-white t))    ; load the doom-homage theme

(blink-cursor-mode 0)

;; frame size at startup
(set-frame-width (selected-frame) 90)
(set-frame-height (selected-frame) 50)

;; fill column
(setq-default fill-column 88)

(crafted-package-install-package 'which-key)

;; set leader key in all states
(evil-set-leader nil (kbd "C-SPC"))
;; set leader key in normal state
(evil-set-leader 'normal (kbd "SPC"))
;; set local leader
(evil-set-leader 'normal "," t)

(evil-define-key 'normal 'global (kbd "<leader>q") nil "Quit")
(evil-define-key 'normal 'global (kbd "<leader>qq") 'evil-quit-all)
;; (evil-define-key 'normal 'global (kbd "<leader>qr") 'evil-quit-all)

;; find file
(evil-define-key 'normal 'global (kbd "<leader>f") nil "Files")
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
;; save file
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)

(evil-define-key 'normal 'global (kbd "<leader>b") nil "Buffers")
;; close buffer
(evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-this-buffer)
;; change buffer
(evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)

;; bind M-x to SPC SPC
(evil-define-key 'normal 'global (kbd "<leader>SPC") 'execute-extended-command "M-x")

(evil-define-key 'normal 'global (kbd "<leader>F1") '(lambda () (interactive)(set-frame-width (selected-frame)  90)))
(evil-define-key 'normal 'global (kbd "<leader>F2") '(lambda () (interactive)(set-frame-width (selected-frame) 180)))
(evil-define-key 'normal 'global (kbd "<leader>F3") '(lambda () (interactive)(set-frame-width (selected-frame) 270)))

;; Useful to insert unicode quickly
(evil-define-key 'normal 'global (kbd "<leader>xu") 'insert-char)

(crafted-package-install-package 'conda)
(require 'conda)

(crafted-package-install-package 'markdown-mode)

(crafted-package-install-package 'magit)

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq crafted-load-custom-file nil)
