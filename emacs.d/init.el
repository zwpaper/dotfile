
(server-start)

; Package install
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(setq url-proxy-services
 '(("no_proxy" . "^\\(localhost\\|10.*\\)")
   ("http" . "127.0.0.1:1087")
   ("https" . "127.0.0.1:1087"))
)
(package-initialize)

;; Guarantee all packages are installed on start
;; Common Lisp Emulation
(require 'cl)
(defvar packages-list
  '(
;;; Emacs settings
    neotree
    all-the-icons
    use-package
    pyim
    pyim-wbdict
    ; Run manually
    ; M-x all-the-icons-install-fonts
    ace-window
    helm
    helm-projectile
    virtualenvwrapper
    eshell-z
    esh-autosuggest
    eshell-fringe-status
    eshell-prompt-extras
    exec-path-from-shell
;;; org mode
    ox-gfm
;;; General Programming
    ; auto-complete
    highlight-parentheses
    ggtags
    flycheck
    yasnippet
;;; Git
    magit
;;; Go
    go-eldoc
    go-mode
    go-add-tags
;;; Haskell
    haskell-mode
    intero
;;; Python
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

;;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Bookmarks
(setq bookmark-save-flag 1) ; everytime bookmark is changed, automatically save it

; macOS
;; (menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
;; Set default font
;;; 如果配置好，这24个汉字与下面个48英文字母应该等长
;;; here are 24 chinese and 48 english chars, ended.
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 140
                    :weight 'normal
                    :width 'normal)
(set-fontset-font t 'han      (font-spec
                               :family "PingFang SC"
                               :size 16
                               ))
(set-fontset-font t 'cjk-misc (font-spec
                               :family "PingFang SC"
                               :size 16
                               ))
;; (setq face-font-rescale-alist '(("PingFang SC" . 1.0)))

; Vars
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/plugin")
(add-to-list 'load-path "~/fp")
(add-to-list 'load-path "~/.emacs.d/config")

(menu-bar-mode -1)
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(show-paren-mode 1)

; Global Settings:
;; Key bindings
(global-set-key (kbd "M-m") 'goto-line)
;; scroll one line only when past the bottom of screen
(setq scroll-conservatively 1)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("home"
	     ("emacs-config" (or (filename . ".emacs.d")
			                 (filename . "emacs-config")))
	     ("eshells" (or (name . "\.esh")
			            (name . "*eshell*")))
	     ("Org" (or (mode . org-mode)
		            (filename . "OrgMode")))
	     ("Golang Dev" (or (mode . go-mode)))
	     ("Magit" (name . "\*magit"))
	     ("Help" (or (name . "\*Help\*")
		             (name . "\*Apropos\*")
		             (name . "\*info\*"))))))
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

;; (require 'tramp)
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Whitespace
(require 'whitespace)
(setq-default whitespace-style '(face trailing lines-tail))
(global-whitespace-mode 1)

;; Del trailing space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; multiple cursors
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

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
;; avy
(global-set-key (kbd "C-M-]") 'avy-goto-char)
(global-set-key (kbd "C-M-[") 'avy-goto-char-2)

;; eshell
(add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
(add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)
(add-hook 'eshell-mode-hook
          (defun my-eshell-mode-hook ()
            (require 'eshell-z)))

; (with-eval-after-load "esh-opt"
;   (autoload 'epe-theme-dakrone "eshell-prompt-extras")
;   (setq eshell-highlight-prompt nil
;         eshell-prompt-function 'epe-theme-dakrone))
(with-eval-after-load "esh-opt"
   (require 'virtualenvwrapper)
   (venv-initialize-eshell)
  (autoload 'epe-theme-dakrone "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-dakrone))


; Global
;; Theme
(load-theme 'atom-one-dark t)

(setq inhibit-compacting-font-caches t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOROOT")

;; Pyim
(require 'pyim)
(use-package pyim
  :ensure nil
  :config
  ;; 五笔用户使用 wbdict 词库
   (use-package pyim-wbdict
     :ensure nil
     :config (pyim-wbdict-v98-enable))

  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'wubi)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))
(global-set-key (kbd "C-\\") 'toggle-input-method)

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

;; eshell
(add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

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
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(setq ac-source-yasnippet nil)
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
(yas-global-mode 1)

;; Indent Guide
(add-to-list 'load-path "~/.emacs.d/plugin/indent-guide")
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-char "|")
(setq indent-guide-recursive t)

;; Show line numbers
(add-hook 'find-file-hooks (lambda()(display-line-numbers-mode 1)))

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
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(defun my-flycheck-c-setup ()
  (setq flycheck-clang-language-standard "gnu99"))
(setq flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

;;; local package
(use-package init-org
  :ensure nil)
(use-package init-languages
  :ensure nil)

;;(use-package yequake)
;;  :custom
;;  (yequake-frames
;;   '(("org-capture"
;;      (buffer-fns . (yequake-org-capture))
;;      (width . 0.75)
;;      (height . 0.5)
;;      (alpha . 0.95)
;;      (frame-parameters . ((undecorated . t)
;;                           (skip-taskbar . t)
;;                           (sticky . t))))))

; (setq org-latex-compiler "xelatex")
; (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
; (setq-default TeX-engine 'xetex)
; (setq-default TeX-PDF-mode t)


;; ediff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("adf5275cc3264f0a938d97ded007c82913906fc6cd64458eaae6853f6be287ce" default)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(fci-rule-color "#3E4451")
 '(lsp-ui-sideline-show-hover t)
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(package-selected-packages
   (quote
    (lsp-sh sh-mode shell-script-mode ox-gfm yaml-mode ob-go yequake lsp-go buffer-move elscreen atom-one-dark-theme yasnippet-snippets go-snippets go-rename smooth-scroll markdown-preview-mode markdown-mode helm-ag pyim elpy exec-path-from-shell all-the-icons neotree flycheck-haskell haskell-mode go-add-tags magit yasnippet sr-speedbar highlight-parentheses helm-projectile go-eldoc ggtags flycheck ace-window)))
 '(tramp-mode nil nil (tramp))
 '(tramp-remote-path nil nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
