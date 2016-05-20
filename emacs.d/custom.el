;; Load ido
(require 'ido)
(ido-mode)

;; Load helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-p") 'helm-projectile-grep)
(helm-mode 1)

;;
;; DocView auto-revert
;;
(add-hook 'doc-view-mode 'auto-revert-mode)

;;
;; Flyspell --- on-the-fly spell checking
;;
(setq flyspell-issue-welcome-flag nil)
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-buffer)

;;
;; RefTeX --- minor mode for reference management in LaTeX
;; (useful TOC mode; even works for multifile documents)
;;
(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-format-cite-function 
      '(lambda (key fmt)
	 (let ((cite (replace-regexp-in-string "%l" key fmt)))
	   (if (or (= ?~ (string-to-char fmt))
		   (member (preceding-char) '(?\ ?\t ?\n
						     ?~))) cite
	     (concat "~" cite)))))

;; ;; yasnippet code 'optional', before auto-complete
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;; auto-complete setup, sequence is important
;; (require 'auto-complete)
;; (add-to-list 'ac-modes 'latex-mode) ; beware of using 'LaTeX-mode instead
;; (defun my-ac-latex-mode () ; add ac-sources for latex
;;    (setq ac-sources
;;          (append '(ac-source-math-unicode
;;            ac-source-math-latex
;;            ac-source-latex-commands)
;;                  ac-sources)))
;; (add-hook 'LaTeX-mode-hook 'my-ac-latex-mode)
;; (setq ac-math-unicode-in-math-p t)
;; (ac-flyspell-workaround) ; fixes a known bug of delay due to flyspell (if it is there)
;; (add-to-list 'ac-modes 'org-mode) ; auto-complete for org-mode (optional)
;; (require 'auto-complete-config) ; should be after add-to-list 'ac-modes and hooks
;; (ac-config-default)
;; (setq ac-auto-start nil)            ; if t starts ac at startup automatically
;; (setq ac-auto-show-menu t)
;; (global-auto-complete-mode t) 

;; Display this instead of "For information about GNU Emacs and the
;; GNU system, type C-h C-a.". This has been made intentionally hard
;; to customize in GNU Emacs so I have to resort to hackery.
(defun display-startup-echo-area-message ()
  "If it wasn't for this you'd be GNU/Spammed by now"
  (message ""))

;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)

;;
;; Core UI settings
;;

;; Display the line and column number in the modeline
(setq line-number-mode t)
(setq column-number-mode t)
(line-number-mode t)
(column-number-mode t)

;; syntax highlight everywhere
(global-font-lock-mode t)

;; Show matching parens (mixed style)
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; 'mixed highligts the whole sexp making it unreadable, maybe tweak
;; color display?
(setq show-paren-style 'parenthesis)

;; Highlight selection
(transient-mark-mode t)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Invert colours
(set-background-color "black")
(set-foreground-color "white")

;; Switching
(icomplete-mode 1)


;;
;; Emulate vi softtabs
;;
(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or just one
    char if that's not possible. This emulates vim's softtabs
    feature."
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    ;; let's get to work
    (let ((movement (% (current-column) tab-width))
	  (p (point)))
      ;; brain freeze, should be easier to calculate goal
      (when (= movement 0) (setq movement tab-width))
      (if (save-excursion
	    (backward-char movement)
	    (string-match "^\\s-+$" (buffer-substring-no-properties (point) p)))
	  (delete-region (- p movement) p)
	(call-interactively 'backward-delete-char-untabify)))))

(global-set-key (kbd "<DEL>") 'backward-delete-whitespace-to-column)

;;
;; Fullscreen mode
;;
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)

;;
;; Variables set by Custom (menu-based customisation)
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdg-open")
     (output-pdf "xdg-open")
     (output-html "xdg-open"))))
 '(haskell-process-show-debug-tips nil)
 '(ispell-dictionary "en_GB-ise")
 '(org-agenda-files nil)
 '(org-journal-dir "~/Documents/journal"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "light blue"))))
 '(agda2-highlight-function-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "light blue"))))
 '(agda2-highlight-record-face ((t (:foreground "light blue"))))
 '(isabelle-string-face ((t (:background "black")))))

;; erlang-mode commands
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; margins
(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

(global-set-key (kbd "C-c m") 'xah-toggle-margin-right)

;; disable the toolbar, scrollbar and menubar
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

;; Tomorrow Night theme
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)

;; Line numbers
(global-linum-mode 1)

;; Load Proof General
(load-file "/home/pablo/tmp/Isabelle2015/contrib/ProofGeneral-4.2-2/generic/proof-site.el")

;; Load OTT mode
(require 'ottmode)

;; haskell-interactive-mode bindings
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;; Org mode setup
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(global-set-key (kbd "C-c r") 'org-capture)
(setq org-default-notes-file "~/org/notes.org")

;; Typescript mode
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key binding
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommended configuration
(tss-config-default)
