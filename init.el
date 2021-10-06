;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is now generated from config.org.  Please edit that file instead

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t
      use-package-compute-statistics t
      use-package-always-ensure t
      use-package-always-defer t)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq-default frame-title-format '("%b"))

(setq uniquify-buffer-name-style 'forward)

(setq frame-resize-pixelwise t)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)
(setq enable-local-variables :all)

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq make-backup-files nil
      create-lockfiles nil
      custom-file (make-temp-file "") 
      custom-safe-themes t)

(setq sentence-end-double-space nil)

(setq ring-bell-function 'ignore)

(setq vc-follow-symlinks t)

(global-auto-revert-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode t)
(electric-pair-mode t)

(delete-selection-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package no-littering
  :demand
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq om/default-font "Roboto Mono Light") 
(setq om/variable-font "Alegreya") 
(setq om/default-font-size 150) 
(setq om/variable-font-size 170)

(set-face-attribute 'default nil :family om/default-font :height om/default-font-size)
(set-face-attribute 'variable-pitch nil :family om/variable-font :height om/variable-font-size)

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default cursor-type 'bar)

(use-package doom-themes
  :demand
  :config
  ;; change this line to set the theme persistently
  (load-theme 'doom-one-light t))

(use-package doom-modeline
  :demand
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package dashboard
  :demand
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode))

(use-package evil
  :demand
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-move-beyond-eol t)
  (setq-default evil-cross-lines t)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :demand
  :after evil
  :config
  (setq evil-collection-company-use-tng nil
	evil-collection-setup-minibuffer t)
  (evil-collection-init))
(use-package evil-surround
  :demand
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-commentary
  :demand
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-org
  :after evil org
  :hook (org-mode . evil-org-mode)
  :config
  ;; Enable Tab to cycle headings in TTY
  (setq evil-want-C-i-jump nil)
  (setq evil-org-special-o/O nil)
  (evil-define-key 'normal org-mode-map (kbd "C-i") 'org-cycle)
  )

(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-create-definer om/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")


  (om/leader-keys
    "SPC" '(execute-extended-command :which-key "execute command")

    "f" '(:ignore t :which-key "file")
    "fD" '((lambda () (interactive) (delete-file (buffer-file-name))) :which-key "delete")
    "ff"  'find-file
    "fs" 'save-buffer
    "fr" 'recentf-open-files
    "fR" '((lambda (new-path)
	     (interactive (list (read-file-name "Move file to: ") current-prefix-arg))
	     (rename-file (buffer-file-name) (expand-file-name new-path)))
	   :which-key "move/rename")

    "e" (general-simulate-key "C-c '" :which-key "Simulate C-c '")
    "d" '(org-babel-demarcate-block :which-key "split src block") 
    "i" '(reindent-whole-buffer :which-key "reindent buffer")
    "b" '(consult-buffer :which-key "switch buffer")
    "tw" 'writeroom-mode
    "ll" 'languagetool-correct-at-point
    "ls" 'languagetool-server-mode
    "w" (general-simulate-key "C-w" :which-key "Window")
    

    )
  )

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-backends '((company-files company-capf company-yasnippet)))
  (global-company-mode)
  )

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil)
  )

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode-on)
  :config
  (yas-reload-all))
(use-package yasnippet-snippets)

(use-package vertico
  :demand
  :init
  (vertico-mode))

(use-package savehist
  :demand
  :after vertico
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :demand
  :config
  (all-the-icons-completion-mode)
  )


(use-package orderless
  :after vertico
  :custom (completion-styles '(orderless)))
(use-package consult
  :after vertico)

(use-package org
  :config
  (setq org-src-preserve-indentation t
	org-catch-invisible-edits 'smart)
  )

(use-package org
  :config
  (require 'oc)
  (require 'oc-basic)
  (require 'oc-csl)
  (setq org-cite-csl-styles-dir "~/Documents/zotero/styles/")
  (require 'oc-biblatex)
  (setq org-cite-biblatex-options "backend=biber")
  ;; (setq org-cite-export-processor "biblatex")
  ;; (setq org-cite-export-processors '((t biblatex nil nil)))
  )
(use-package citeproc)

(use-package org
  :config
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  )

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package org
  :config
  (add-to-list 'org-modules 'org-tempo t)
  (add-hook 'org-mode-hook
	    (lambda () (setq-local electric-pair-inhibit-predicate
				   `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  )

(setq org-src-window-setup 'current-window)

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-appear
  :straight
  '(org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autolinks t)
  )

(use-package org
  :hook
  (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t)
  )

(use-package org
  :config
  (setq org-startup-with-inline-images t
	org-image-actual-width nil))
(use-package org-download
  :demand
  :after org)

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-document-title ((t (:inherit outline-1 :bold t :height 1.5)))))
  )

(defun reindent-whole-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min)
                 (point-max)))

(use-package languagetool
  :config
  (setq languagetool-server-language-tool-jar
	"~/Documents/languagetool/languagetool-server.jar")
  (languagetool-server-start)
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))

  )

(use-package crdt
  :commands (crdt-share-buffer)
  )
