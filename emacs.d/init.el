
; Package install
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(setq url-proxy-services
 '(("no_proxy" . "^\\(localhost\\|10.*\\)")
   ("http" . "127.0.0.1:1087")
   ("https" . "127.0.0.1:1087"))
)
(package-initialize)


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

; (require 'tramp)
; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; clipboard
(defun paste-from-osx ()
  "Paste using macOS clipboard."
  (interactive)
  (shell-command-to-string "pbpaste"))

(defun copy-to-osx (text &optional push)
  "Copy Emacs content to macOS.
TEXT: selected region
PUSH: push to macOS"
  (interactive)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(global-set-key (kbd "C-c C-y") 'paste-from-osx)
(global-set-key (kbd "C-c C-w") 'copy-to-osx)


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

;; Guarantee all packages are installed on start
(require 'cl)
(defvar packages-list
  '(
    ; Emacs settings
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
    ; General Programming
    auto-complete
    highlight-parentheses
    ggtags
    flycheck
    yasnippet
    ; Git
    magit
    ; Go
    go-eldoc
    go-mode
    go-add-tags
    ; Haskell
    haskell-mode
    intero
    ; Python
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
;; Theme
(load-theme 'atom-one-dark t)

(ac-config-default)
(setq inhibit-compacting-font-caches t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOROOT")

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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
(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))

(add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))
(add-to-list 'ac-modes 'eshell-mode)

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
(add-hook 'after-init-hook #'global-flycheck-mode)
(defun my-flycheck-c-setup ()
  (setq flycheck-clang-language-standard "gnu99"))
(setq flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

; (setq org-latex-compiler "xelatex")
; (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
; (setq-default TeX-engine 'xetex)
; (setq-default TeX-PDF-mode t)

;; C++
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;; C
(defun my-flycheck-c-setup ()
  (setq flycheck-clang-language-standard "gnu99"))
(add-hook 'c-mode-hook #'my-flycheck-c-setup)
(setq-default c-basic-offset 4)
(global-set-key (kbd "C-c C-r") 'compile)
(global-set-key (kbd "C-c C-t") 'gdb)

; Go
(setq gofmt-command "goimports")
(defun my-go-mode-hook ()
  "Auto format and import on save."
  (add-hook 'before-save-hook 'gofmt-before-save)
;  (add-hook 'before-save-hook 'lsp-format-buffer)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook #'yas-minor-mode)
; (add-to-list 'load-path "~/.emacs.d/elpa/go-mode-20160715.1705")
; (add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150618.1949/")
; (add-to-list 'load-path "~/.emacs.d/elpa/popup-20150626.711/")
; (require 'go-mode-load)
; (require 'go-autocomplete)
; (require 'auto-complete-config)
; (add-hook 'go-mode-hook 'go-eldoc-setup)
; (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
; (require 'golint)
(require 'lsp-clients)
(require 'company-lsp)
(push 'company-lsp company-backends)
(require 'lsp-mode)
; (setq lsp-prefer-flymake nil)
(add-hook 'go-mode-hook 'lsp)

;; python
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt -i"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(define-coding-system-alias 'UTF-8 'utf-8)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq elpy-rpc-python-command "python3")

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
 '(package-selected-packages
   (quote
    (lsp-go lsp-ui buffer-move elscreen atom-one-dark-theme yasnippet-snippets go-snippets company-lsp lsp-mode go-rename smooth-scroll markdown-preview-mode markdown-mode helm-ag pyim elpy exec-path-from-shell all-the-icons neotree flycheck-haskell haskell-mode go-add-tags magit yasnippet sr-speedbar highlight-parentheses helm-projectile go-eldoc ggtags flycheck auto-complete ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
