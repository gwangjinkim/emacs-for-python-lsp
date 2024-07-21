

(require 'package)

;; Add MELPA and other archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))




;; Initialize the package system
(package-initialize)

;; Ensure package archives are up to date
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))




;; load myinit.org
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(dap-mode elpy try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; reset env HOME
;; works only with ~/.bashrc
;; alias pyemacs="conda activate emacs && env ORIG_HOME=$HOME HOME=$HOME/emacs/emacs-for-python emacs &"
(setenv "HOME" (getenv "ORIG_HOME"))
