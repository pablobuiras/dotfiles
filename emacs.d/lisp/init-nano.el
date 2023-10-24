(use-package emacs
  :init
  ;; Path to nano emacs modules (mandatory)
  (add-to-list 'load-path ".")

  ;; Default layout (optional)
  (require 'nano-layout)

  ;; Theming Command line options (this will cancel warning messages)
  (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
  (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-compact" . (lambda (args))))


  ;; Customize support for 'emacs -q' (Optional)
  ;; You can enable customizations by creating the nano-custom.el file
  ;; with e.g. `touch nano-custom.el` in the folder containing this file.
  (let* ((this-file  (or load-file-name (buffer-file-name)))
         (this-dir  (file-name-directory this-file))
         (custom-path  (concat this-dir "nano-custom.el")))
    (when (and (eq nil user-init-file)
               (eq nil custom-file)
               (file-exists-p custom-path))
      (setq user-init-file this-file)
      (setq custom-file custom-path)
      (load custom-file)))

  ;; Theme
  (require 'nano-faces)
  (require 'nano-theme)
  (require 'nano-theme-dark)
  (require 'nano-theme-light)

  (cond
   ((member "-default" command-line-args) t)
   ((member "-dark" command-line-args) (nano-theme-set-dark))
   (t (nano-theme-set-light)))
  (call-interactively 'nano-refresh-theme)

  ;; No startup  screen
  (setq inhibit-startup-screen t)

  ;; No startup message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)

  ;; No message in scratch buffer
  (setq initial-scratch-message nil)

  ;; Initial buffer
  (setq initial-buffer-choice nil)

  ;; No frame title
  (setq frame-title-format nil)

  ;; No file dialog
  (setq use-file-dialog nil)

  ;; No dialog box
  (setq use-dialog-box nil)

  ;; No popup windows
  (setq pop-up-windows nil)

  ;; No empty line indicators
  (setq indicate-empty-lines nil)

  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)

  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)

  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)

  ;; Moderate font lock
  (setq font-lock-maximum-decoration nil)

  ;; No limit on font lock
  (setq font-lock-maximum-size nil)

  ;; No line break space points
  (setq auto-fill-mode nil)

  ;; Fill column at 80
  (setq fill-column 80)

  ;; No confirmation for visiting non-existent files
  (setq confirm-nonexistent-file-or-buffer nil)

  ;; Completion style, see
  ;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
  (setq completion-styles '(basic substring))

  ;; Use RET to open org-mode links, including those in quick-help.org
  (setq org-return-follows-link t)

  ;; Mouse active in terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

  ;; No scroll bars
  (if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

  ;; No toolbar
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  ;; No menu bar
  (menu-bar-mode -1)

  ;; Tab behavior
  ;; (setq tab-always-indent 'complete)
  ;; (global-company-mode)
  ;; (define-key company-mode-map [remap indent-for-tab-command]
  ;;   #'company-indent-or-complete-common)

  ;; Pixel scroll (as opposed to char scrool)
  ;; (pixel-scroll-mode t)

  ;; Mac specific
  (when (eq system-type 'darwin)
    (setq ns-use-native-fullscreen t
          mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier nil
          mac-use-title-bar nil))

  ;; Make sure clipboard works properly in tty mode on OSX
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (when (and (not (display-graphic-p))
             (eq system-type 'darwin))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))

  ;; y/n for  answering yes/no questions
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; No tabs
  (setq-default indent-tabs-mode nil)

  ;; Tab.space equivalence
  (setq-default tab-width 4)

  ;; Size of temporary buffers
  (temp-buffer-resize-mode)
  (setq temp-buffer-max-height 8)

  ;; Minimum window height
  (setq window-min-height 1)

  ;; Buffer encoding
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8)

  ;; Unique buffer names
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*")

  ;; Default shell in term
  (unless
      (or (eq system-type 'windows-nt)
          (not (file-exists-p "/bin/zsh")))
    (setq-default shell-file-name "/bin/zsh")
    (setq explicit-shell-file-name "/bin/zsh"))

  ;; Kill term buffer when exiting
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
        (let ((buffer (process-buffer proc)))
          ad-do-it
          (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)

  ;; Nano session saving (optional)
  (require 'nano-session)

  ;; Nano header & mode lines (optional)
  (require 'nano-modeline)

  ;; Nano key bindings modification (optional)
  (require 'nano-bindings)

  ;; Compact layout (need to be loaded after nano-modeline)
  (when (member "-compact" command-line-args)
    (require 'nano-compact))
  
  ;; Nano counsel configuration (optional)
  ;; Needs "counsel" package to be installed (M-x: package-install)
  ;; (require 'nano-counsel)

  ;; Welcome message (optional)
  (let ((inhibit-message t))
    (message "Welcome to GNU Emacs / N Λ N O edition")
    (message (format "Initialization time: %s" (emacs-init-time))))

  ;; Splash (optional)
  (unless (member "-no-splash" command-line-args)
    (require 'nano-splash))

  ;; Help (optional)
  (unless (member "-no-help" command-line-args)
    (require 'nano-help)))

(provide 'init-nano)
