(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; write over selected text
  (delete-selection-mode t)

  ;; no-litter policy
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; follow symlinks
  (setq vc-follow-symlinks t)

  ;; Font size shortcuts
  (require 'face-remap)
  (global-set-key (kbd "S-<wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "S-<wheel-down>") 'text-scale-decrease)

  
  ;; Ensure bold is bold, regardless of nano setup earlier
  (set-face-attribute 'bold nil :weight 'bold))

(provide 'init-defaults)
