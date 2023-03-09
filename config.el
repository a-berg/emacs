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

(crafted-package-install-package 'use-package)
;;; then bootstrap use-package
(eval-when-compile
  (add-to-list 'load-path "./elpa/use-package-2.4.4")
  (require 'use-package))
;;; small extension to use-package-ensure-system-package
(use-package use-package-ensure-system-package
  :ensure t)
;;; now install quelpa and use it to also install quelpa-use-package,
;;; and we are all set!
(crafted-package-install-package 'quelpa)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-python)      ; python, needs customization
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.
(require 'crafted-meow)
(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.
(require 'crafted-project)     ; built-in alternative to projectile
(require 'crafted-speedbar)    ; built-in file-tree
(require 'crafted-workspaces)  ; tabs and workspaces
;; (require 'crafted-screencast)  ; show current command and binding in modeline
(require 'crafted-compile)     ; automatically compile some emacs lisp files

(use-package font-lock
  ;; :ensure t
  :config
  (set-face-attribute 'default nil
                      :family "Iosevka Cosmic" :height 140 :weight 'light)
  (set-face-attribute 'variable-pitch nil
                      :family "Iosevka Aile" :height 140 :weight 'light)
  (set-face-attribute 'fixed-pitch nil
                      :inherit 'default))

;; (crafted-package-install-package 'doom-themes)
(setq modus-themes-syntax '(green-strings yellow-comments)
      modus-themes-fringes 'subtle
      modus-themes-italic-constructs nil
      modus-themes-bold-constructs t
      modus-themes-org-blocks 'gray-background
      modus-themes-hl-line '(intense)
      modus-themes-paren-match '(intense)
      modus-themes-mode-line '(borderless))

(setq modus-themes-operandi-color-overrides
        '((bg-main . "#f7f7f7")
          (bg-dim . "#f2f2f2")
          (bg-alt . "#e8e8e8")
          (bg-hl-line . "#eaeaef")
          (bg-active . "#e0e0e0")
          (bg-inactive . "#e6e6e6")
          (bg-region . "#b5b5b5")
          (bg-header . "#e4e4e4")
          (bg-tab-active . "#f5f5f5")
          (bg-tab-inactive . "#c0c0c0")))

(progn
  (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
  (load-theme 'modus-operandi t))       ; load the modus-operandi theme

(blink-cursor-mode 0)

(modify-all-frames-parameters
 '((right-divider-width . 20)
   (internal-border-width . 20)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; frame size at startup
(set-frame-width (selected-frame) 100)
(set-frame-height (selected-frame) 50)

;; fill column
(setq-default fill-column 88)

(use-package org-modern
  :quelpa (org-modern :fetcher github :repo "minad/org-modern")
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(setq
;; Edit settings
org-auto-align-tags nil
org-tags-column 0
org-catch-invisible-edits 'show-and-error
org-special-ctrl-a/e t
org-insert-heading-respect-content t

;; Org styling, hide markup etc.
org-hide-emphasis-markers t
org-pretty-entities t
org-ellipsis "…"

;; Agenda styling
org-agenda-tags-column 0
org-agenda-block-separator ?─
org-agenda-time-grid
'((daily today require-timed)
  (800 1000 1200 1400 1600 1800 2000)
  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
org-agenda-current-time-string
"⭠ now ─────────────────────────────────────────────────")

(use-package yasnippet
    :quelpa (yasnippet :fetcher github :repo "joaotavora/yasnippet")
    :config
    (crafted-package-install-package 'yasnippet-snippets)
    (yas-global-mode 1))

(crafted-package-install-package 'which-key)
(which-key-mode)

(crafted-package-install-package 'neotree)
;; (evil-define-key 'normal 'global (kbd "<leader>fT") 'neotree-toggle)

(quelpa '(eat :fetcher git
              :url "https://codeberg.org/akib/emacs-eat"
              :files ("*.el" ("term" "term/*.el") "*.texi"
                      "*.ti" ("terminfo/e" "terminfo/e/*")
                      ("terminfo/65" "terminfo/65/*")
                      ("integration" "integration/*")
                      (:exclude ".dir-locals.el" "*-tests.el"))))

(add-hook 'conda-postactivate-hook (lambda () (eglot-reconnect)))
(setenv "WORKON_HOME" "~/mambaforge/envs/")
(use-package ein
  :commands ein:run
  :ensure t)
  ;;  (crafted-package-install-package 'jedi)
  ;;  (add-hook 'python-mode-hook #'jedi-mode)

(use-package quarto-mode
  :ensure-system-package
  quarto
  :ensure t
  ;; :requires (polymode poly-markdown markdown-mode request)
  :config
  (use-package polymode :ensure t)
  (use-package poly-markdown :ensure t)
  (use-package markdown-mode :ensure t)
  (use-package request :ensure t)
  :mode (("\\.Rmd" . poly-quarto-mode)))

(crafted-package-install-package 'markdown-mode)

(crafted-package-install-package 'magit)

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq crafted-load-custom-file nil)
