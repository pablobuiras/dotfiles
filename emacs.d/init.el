(straight-use-package
 '(nano :type git :host github :repo "rougier/nano-emacs"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq nano-font-family-monospaced "Roboto Mono")
(setq nano-font-family-proportional nil)
(setq nano-font-size 14)
(require 'init-nano)
(set-face-attribute 'default nil :weight 'normal)
(require 'nano-theme-dark)
(nano-theme-set-dark)
(nano-refresh-theme)

(defun pb/font-available-p (font-name)
  "Check if FONT-NAME is installed."
  (car (member font-name (font-family-list))))

(require 'cl-extra)
(setq pb/code-font
      (let ((font-list
             '("FiraCode Nerd Font" "Fira Code" "Iosevka" "Roboto Mono")))
        (cl-some #'pb/font-available-p font-list)))

(require 'init-defaults)
(require 'init-modern)
(require 'init-haskell)
(require 'init-vc)
(require 'init-org)
