;;; packages.el --- hie layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <ben.kolera@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst hie-nix-packages
  '(
    flycheck
    (flycheck-haskell :requires flycheck)
    nix-sandbox
    haskell-mode
    company-lsp
    (lsp-haskell :requires haskell-mode :location (recipe :fetcher github :repo "emacs-lsp/lsp-haskell"))
    ))

(defun hie-nix/post-init-flycheck ()
  (add-hook 'haskell-mode-hook 'flycheck-mode))

; This doesn't work. Just brings up hippie expand suggestions .
;(defun hie-nix/post-init-company-lsp ()
;  (spacemacs|add-company-backends :backend company-lsp :modes haskell-mode))

(defun hie-nix/init-nix-sandbox ()
  (use-package nix-sandbox))

(defun hie-nix/init-lsp-haskell ()
  (use-package lsp-haskell
    :init (add-hook 'haskell-mode-hook #'lsp)))

(defun hie-nix/init-flycheck-haskell ()
  (use-package flycheck-haskell
    :commands flycheck-haskell-configure
    :init
    (progn
      (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)
      (message "NIX WRAPPED %s" spacemacs/lsp-haskell-nix-wrapped)
      (when spacemacs/lsp-haskell-nix-wrapped
        (setq
         lsp-haskell-process-wrapper-function #'hie-nix//default-nix-wrapper
         flycheck-command-wrapper-function #'hie-nix//default-nix-wrapper
         flycheck-executable-find (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd))
         ))
    )))

(defun hie-nix/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :init
    (progn
      (defun spacemacs//force-haskell-mode-loading ()
        "Force `haskell-mode' loading when visiting cabal file."
        (require 'haskell-mode))
      (add-hook 'haskell-cabal-mode-hook
                'spacemacs//force-haskell-mode-loading)

      ;; Haskell cabal files interact badly with electric-indent-mode
      ;; note: we cannot add this hook in :config, since haskell-mode might
      ;; only be loaded after cabal-mode hooks are already run (see add-hook above)
      (add-hook 'haskell-cabal-mode-hook #'hie-nix//disable-electric-indent)

      (setq
       ;; Use notify.el (if you have it installed) at the end of running
       ;; Cabal commands or generally things worth notifying.
       haskell-notify-p t
       ;; Remove annoying error popups
       haskell-interactive-popup-errors nil
       ;; Better import handling
       haskell-process-suggest-remove-import-lines t
       haskell-process-auto-import-loaded-modules t
       haskell-process-type 'cabal-new-repl
       ;; Disable haskell-stylish-on-save, as it breaks flycheck highlighting.
       ;; NOTE: May not be true anymore - taksuyu 2015-10-06
       haskell-stylish-on-save nil
       haskell-tags-on-save t
       )

      (when spacemacs/lsp-haskell-nix-wrapped
        (setq haskell-process-wrapper-function #'hie-nix//haskell-nix-wrapper))

      (dolist (mode haskell-modes)
        (spacemacs/declare-prefix-for-mode mode "mg" "haskell/navigation")
        (spacemacs/declare-prefix-for-mode mode "ms" "haskell/repl")
        (spacemacs/declare-prefix-for-mode mode "me" "haskell/errors")
        (spacemacs/declare-prefix-for-mode mode "mc" "haskell/cabal")
        (spacemacs/declare-prefix-for-mode mode "mh" "haskell/documentation")
        (spacemacs/declare-prefix-for-mode mode "md" "haskell/debug")
        (spacemacs/declare-prefix-for-mode mode "mr" "haskell/refactor"))

      (spacemacs/declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
      (spacemacs/declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

      (dolist (mode haskell-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "gi"  'haskell-navigate-imports
          "gm"  'lsp-ui-imenu
          "gg"  'lsp-ui-peek-find-definitions
          "gr"  'lsp-ui-peek-find-references
          "F"   'haskell-mode-stylish-buffer
          "en"  'flycheck-next-error
          "ep"  'flycheck-previous-error
          "el"  'flycheck-list-errors
          "ee"  'flycheck-explain-error-at-point

          "sb"  'haskell-process-load-file
          "sc"  'haskell-interactive-mode-clear
          "sS"  'spacemacs/haskell-interactive-bring
          "ss"  'haskell-interactive-switch

          "ca"  'haskell-process-cabal
          "cb"  'haskell-process-cabal-build
          "cc"  'haskell-compile
          "cv"  'haskell-cabal-visit-file

          "hd"  'inferior-haskell-find-haddock
          "hh"  'hoogle
          "hH"  'haskell-hoogle-lookup-from-local
          "hi"  'haskell-process-do-info
          "ht"  'haskell-process-do-type
          ;"hT"  'spacemacs/haskell-process-do-type-on-prev-line
          "hy"  'hayoo

          "da"  'haskell-debug/abandon
          "db"  'haskell-debug/break-on-function
          "dB"  'haskell-debug/delete
          "dc"  'haskell-debug/continue
          "dd"  'haskell-debug
          "dn"  'haskell-debug/next
          "dN"  'haskell-debug/previous
          "dp"  'haskell-debug/previous
          "dr"  'haskell-debug/refresh
          "ds"  'haskell-debug/step
          "dt"  'haskell-debug/trace

          ;;"ri"  'spacemacs/haskell-format-imports
          "rR"  'lsp-rename
          "rf"  'lsp-format-buffer
          "ra"  'lsp-ui-sideline-apply-code-actions
          "lr"  'lsp-restart-workspace
          ","   'completion-at-point
          "."   'lsp-describe-thing-at-point
          ))

      (evilified-state-evilify haskell-debug-mode haskell-debug-mode-map
        "RET" 'haskell-debug/select
        "a" 'haskell-debug/abandon
        "b" 'haskell-debug/break-on-function
        "c" 'haskell-debug/continue
        "d" 'haskell-debug/delete
        "i" 'haskell-debug/step
        "s" 'haskell-debug/next
        "S" 'haskell-debug/previous
        "r" 'haskell-debug/refresh
        "t" 'haskell-debug/trace)

      ;; configure C-c C-l so it doesn't throw any errors
      ;; bk: This is crashing things in here. Ignore it till later.
      ;; (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)

      ;; Switch back to editor from REPL
      (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode
        "ss"  'haskell-interactive-switch-back)

      ;; Compile
      (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal
        "C"  'haskell-compile)

      ;; Cabal-file bindings
      (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal-mode
        ;; "="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
        "d"   'haskell-cabal-add-dependency
        "b"   'haskell-cabal-goto-benchmark-section
        "e"   'haskell-cabal-goto-executable-section
        "t"   'haskell-cabal-goto-test-suite-section
        "m"   'haskell-cabal-goto-exposed-modules
        "l"   'haskell-cabal-goto-library-section
        "n"   'haskell-cabal-next-subsection
        "p"   'haskell-cabal-previous-subsection
        "sc"  'haskell-interactive-mode-clear
        "sS"  'spacemacs/haskell-interactive-bring
        "ss"  'haskell-interactive-switch
        "N"   'haskell-cabal-next-section
        "P"   'haskell-cabal-previous-section
        "f"   'haskell-cabal-find-or-create-source-file)

      ;; Make "RET" behaviour in REPL saner
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return))
    ))

;;; packages.el ends here
