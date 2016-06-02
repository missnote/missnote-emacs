(require 'cl)

(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  )



;;add whatever packages you want here
(defvar missnote/packages '(
                            company
                            monokai-theme
                            hungry-delete
                            swiper
                            counsel
                            smartparens
                            js2-mode
                            nodejs-repl
                            exec-path-from-shell
                            popwin
                            reveal-in-osx-finder
                            web-mode
                            )  "Default packages")

(setq package-selected-packages missnote/packages)

(defun missnote/packages-installed-p ()
  (loop for pkg in missnote/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (missnote/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg missnote/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; let emacs could find the execuable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;(require 'hungry-delete)
(global-hungry-delete-mode)

;;(require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)


;; config js2-mode for js files
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
         ("\\.html\\'" . web-mode))
       auto-mode-alist))

(global-company-mode t)
(load-theme 'monokai t)

;;config for web-mode
(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )

(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

(defun my-toggle-web-indent ()
  (interactive)
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))
        (setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))
(if (eq major-mode 'web-mode)
    (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
           (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
           (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
(if (eq major-mode 'css-mode)
    (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))
(setq indent-tabs-mode nil))
(global-set-key (kbd "C-c t i") 'my-toggle-web-indent)

(require 'popwin)
(popwin-mode t)

(provide 'init-packages)
