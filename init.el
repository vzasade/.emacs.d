;; Set my email address.
(setq user-mail-address "astemkov@gmail.com")

;; for gpicker
(setenv "PATH"
  (concat
   "/usr/local/bin" ":"
   "/usr/bin" ":"
   (getenv "PATH")
  )
)

(require 'package)
(setq package-list '(go-mode))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Set the shell emacs uses.
(setq explicit-shell-file-name "/bin/bash")

;; Use column and line numbering.
(line-number-mode 1)
(column-number-mode 1)

;; Display time in the minibuffer
(display-time)

(load "~/gpicker/gpicker.el")

(global-set-key (kbd "s-f") 'gpicker-find-file)
(global-set-key (kbd "C-x 4 s-f") 'gpicker-find-file-other-window)
(global-set-key (kbd "C-x 5 s-f") 'gpicker-find-file-other-frame)

;; Specify a color theme
;;(require 'color-theme)
;;(color-theme-initialize)
;;(color-theme-euphoria)

;;
;; Erlang mode and Distel settings
;;

;; This is needed for Erlang mode setup
;; From /usr/local/lib/erlang/lib/tools-2.6.13/emacs

(setq load-path (cons "/Users/artem/.emacs.d/erlang/R16B03-1" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang/erts-5.10.4")
(setq exec-path (cons (concat erlang-root-dir "/bin") exec-path))
(require 'erlang-start)

;; Tell Emacs not to wait the usual 60 seconds for an Erlang prompt
(defvar inferior-erlang-prompt-timeout t)

;; This is needed for Distel setup
(let ((distel-dir "~/distel/elisp"))
(unless (member distel-dir load-path)
;; Add distel-dir to the end of load-path
(setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
(lambda ()
;; when starting an Erlang shell in Emacs, default in the node name
(setq inferior-erlang-machine-options '("-sname" "emacs"))
;; add Erlang functions to an imenu menu
(imenu-add-to-menubar "imenu")))

;; erlang indentation
(setq erlang-indent-level 4)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
'(("\C-\M-i" erl-complete)
("\M-?" erl-complete)
("\M-." erl-find-source-under-point)
("\M-," erl-find-source-unwind)
("\M-*" erl-find-source-unwind)
)
"Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
(lambda ()
;; add some Distel bindings to the Erlang shell
(dolist (spec distel-shell-keys)
(define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(require 'whitespace)
;;(global-whitespace-mode 1)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'erlang-mode-hook 'whitespace-mode)

;; folding functions in javascript code
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs trailing lines-tail empty tab-mark))

;; maybe use in future
;; see http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
;(autoload 'js2-mode "js2-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'espresso-mode "espresso")
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(espresso-indent-level 2)
 '(indent-tabs-mode nil)
 '(js-curly-indent-offset 2)
 '(js-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
