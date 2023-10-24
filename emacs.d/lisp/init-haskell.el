(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook
    (lambda ()
      (setq buffer-face-mode-face `(:family ,pb/code-font))
      (buffer-face-mode)
      (add-to-list 'tramp-remote-path "/proj/flexasic/app/ghcup/.ghcup/bin")
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
      )))

(provide 'init-haskell)
