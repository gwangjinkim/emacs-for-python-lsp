

(require 'package)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; 'failed load gnu archive' preventer
;; to put infront package-initialize ;; works!
(setq package-enable-at-startup nil)
(setq package-archives '())
;; (add-to-list 'package-archives '("elpa"      . "http://elpa.gnu.org/packages") t)
;; (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; M-x package-refresh-contents
(package-initialize)
(package-refresh-contents)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; load myinit.org
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(elpy try use-package)))
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
