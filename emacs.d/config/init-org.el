;;; package --- Summary
;; org mode
;; org capture, org protocal
;;; Commentary:

;;; Code:

;;; org mode
(use-package org
  :bind (("C-M-f" . org-do-demote)
         ("C-M-b" . org-do-promote))
  :config
  (setq org-log-done t))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(eval-after-load "org"
  (use-package ox-gfm
    :ensure t
    :config
    (setq org-src-fontify-natively t)))
;;;; Capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Dropbox/org-mode/inbox.org")

(use-package org-protocol
  :ensure nil)
(use-package org-capture
  :ensure nil)

;;;; Tamplates
(add-to-list 'org-capture-templates
             '("b" "Book Reading Task" entry
               (file+headline "~/Dropbox/org-mode/notes/memo/readList.org" "inbox")
               "* TODO %^{Book Name}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("w" "Work Task" entry
               (file+headline "~/Dropbox/org-mode/work/inbox.org" "Work")
               "* TODO %^{Task Name}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("t" "Personal Task" entry
               (file+headline "~/Dropbox/org-mode/inbox.org" "Task")
               "* TODO %^{Task Name}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("n" "Notes" entry (file "~/Dropbox/org-mode/notes/inbox.org")
               "* %^{heading} %t %^g\n  %?\n"))
(add-to-list 'org-capture-templates
             '("l" "Notes with link" plain
               (file+function "~/Dropbox/org-mode/notes/inbox.org" org-capture-template-goto-link)
               "  %U - %?\n\n  %:initial" :empty-lines 1))
(defun org-capture-template-goto-link ()
  (org-capture-put :target (list 'file+headline
                                 (nth 1 (org-capture-get :target))
                                 (org-capture-get :annotation)))
  (org-capture-put-target-region-and-position)
  (widen)
  (let ((hd (nth 2 (org-capture-get :target))))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
        (org-end-of-subtree)
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n"))))

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))

;;; GTD
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-tag-alist '(("@office" . ?o) ("@home" . ?h) ("@computer" . ?c)))

(provide 'init-org)
