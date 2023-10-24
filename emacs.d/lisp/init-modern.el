(use-package vertico
  :init
  (vertico-mode))
(use-package consult)
(use-package embark-consult)
(use-package embark
  :bind
  (("C-c C-a" . embark-act)))
(use-package marginalia)

(provide 'init-modern)
