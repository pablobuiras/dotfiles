(setq pb/local-root
      (if (string-equal system-type "windows-nt")
          "C:/Users/pablo"
       "~/"))
(setq pb/org-dir (expand-file-name "org" pb/local-root))

(use-package org
  :ensure org-plus-contrib
  :custom
  (org-directory pb/org-dir)
  (org-log-done 'time)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-startup-indented t)
  (org-startup-truncated nil)
  :bind
  ("C-c n" . org-agenda)
  ("C-c c" . org-capture)
  :config
  (setq-local org-fontify-whole-heading-line t)
  :init
  (with-eval-after-load 'org-faces
    (require 'org-indent)
    ;; set basic title font
    ;; Low levels are unimportant => no scaling
    (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
    (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
    (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
    (set-face-attribute 'org-level-4 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :height 1.1)
    (set-face-attribute 'org-level-1 nil :height 1.2)
    (setq org-hidden-keywords '(title))
    (set-face-attribute 'org-document-title nil
                        :height 1.75
                        :foreground 'unspecified
                        :inherit 'org-level-1)
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

    ;; Get rid of the background on column views
    (set-face-attribute 'org-column nil :background nil)
    (set-face-attribute 'org-column-title nil :background nil)))
(require 'nano-writer)

(provide 'init-org)
