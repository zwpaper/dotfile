
; Vars
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/plugin")
(menu-bar-mode -1)
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(show-paren-mode 1)

; Global Settings:
;; Key bindings
(global-set-key (kbd "M-m") 'goto-line)

;; Whitespace
(require 'whitespace)
(setq-default whitespace-style '(face trailing lines-tail))
(global-whitespace-mode 1)

;; Del trailing space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move backup file to dot folder
;;; Don't clutter up directories with files~
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;;; Don't clutter with #files either
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Ace-window
(global-set-key (kbd "C-M-o") 'ace-window)     ; Ace-window
(setq aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))

;; Theme
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/theme/atom-one-dark-theme/")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black")))))
(load-theme 'atom-one-dark t)


; Package install
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; Guarantee all packages are installed on start
(require 'cl)
(defvar packages-list
  '(ace-window
    auto-complete
    highlight-parentheses
    magit
    ggtags
    ace-window
    flycheck
    go-eldoc
    go-mode
    helm
    helm-projectile
    yasnippet
    )
  "List of packages needs to be installed at launch.")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

; Global
(ac-config-default)

;; Helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
                helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-M-x-fuzzy-match                  t   ; 模糊搜索
      helm-buffers-fuzzy-matching           t
      helm-locate-fuzzy-match               t
      helm-recentf-fuzzy-match              t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t)

(projectile-mode)
(helm-projectile-on)

;;; sr speedbar
;(require 'sr-speedbar)
;(setq sr-speedbar-right-side nil)
;(setq sr-speedbar-width-console 20)
;(setq sr-speedbar-default-width 20)
;(setq sr-speedbar-max-width 20)
;(sr-speedbar-width-console 10)
; (sr-speedbar-max-width 10)
;(global-set-key (kbd "C-c l") 'sr-speedbar-toggle)

;;YASnippet
(require 'yasnippet)
(setq ac-source-yasnippet nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-i") 'yas-expand)
(yas-global-mode 1)

;; Indent Guide
(add-to-list 'load-path "~/.emacs.d/plugin/indent-guide")
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-char "|")
(setq indent-guide-recursive t)

;; Show line numbers
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)
(add-hook 'find-file-hooks (lambda()(linum-mode 1)))

;; gnu global
(add-to-list 'load-path "~/.emacs.d/plugin/ggtags")
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))
(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))
(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))
(add-hook 'after-save-hook #'gtags-update-hook)

;; FlyCheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(defun my-flycheck-c-setup ()
  (setq flycheck-clang-language-standard "gnu99"))
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

;; C
(defun my-flycheck-c-setup ()
  (setq flycheck-clang-language-standard "gnu99"))
(add-hook 'c-mode-hook #'my-flycheck-c-setup)
(setq-default c-basic-offset 4)
(global-set-key (kbd "C-c C-r") 'compile)
(global-set-key (kbd "C-c C-t") 'gdb)

; Go

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(setq gofmt-command "goimports")
(add-to-list 'load-path "~/.emacs.d/elpa/go-mode-20160715.1705")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150618.1949/")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20150626.711/")
(require 'go-mode-load)
(require 'go-autocomplete)
(require 'auto-complete-config)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit yasnippet sr-speedbar highlight-parentheses helm-projectile go-eldoc ggtags flycheck auto-complete ace-window))))
