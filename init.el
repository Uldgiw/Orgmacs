;;  ___       _ _         _ 
;; |_ _|_ __ (_) |_   ___| |
;;  | || '_ \| | __| / _ \ |
;;  | || | | | | |_ |  __/ |
;; |___|_| |_|_|\__(_)___|_|
;; -----------------------------------------------------------------------

;; Set garbage-collection threshold very high for faster startup
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;;  ____  _               __  __                              
;; |  _ \| | ____ _      |  \/  | __ _ _ __   __ _  __ _  ___ 
;; | |_) | |/ / _` |_____| |\/| |/ _` | '_ \ / _` |/ _` |/ _ \
;; |  __/|   < (_| |_____| |  | | (_| | | | | (_| | (_| |  __/
;; |_|   |_|\_\__, |     |_|  |_|\__,_|_| |_|\__,_|\__, |\___|
;;            |___/                                |___/      
;; --------------------------------------------------------------

;;------------------
;; Straight.el
;;------------------

;; (setq straight-is-used t)
;; (defvar bootstrap-version)

;; (let ((bootstrap-file
;; 	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))

;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;; 	 (url-retrieve-synchronously ;; 	  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;; 	  'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))

;;   (load bootstrap-file nil 'nomessage))

;;   (straight-use-package 'use-package)
;;   (setq straight-use-package-by-default t)
;;   (setq use-package-always-ensure t)


;;------------------
;; Melpa
;;------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;  _                    _    ____             __ _                        
;; | |    ___   __ _  __| |  / ___|___  _ __  / _(_) __ _   ___  _ __ __ _ 
;; | |   / _ \ / _` |/ _` | | |   / _ \| '_ \| |_| |/ _` | / _ \| '__/ _` |
;; | |__| (_) | (_| | (_| | | |__| (_) | | | |  _| | (_| || (_) | | | (_| |
;; |_____\___/ \__,_|\__,_|  \____\___/|_| |_|_| |_|\__, (_)___/|_|  \__, |
;;                                                  |___/            |___/ 

(use-package org)
(use-package org-plus-contrib
  :after org
  :defer t)
;; (require 'org-babel)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold 16777216 ; 16mb
      gc-cons-percentage 0.1)
