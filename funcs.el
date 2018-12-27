(defun hie-nix//default-nix-wrapper (args)
  (let* ((shell-args (if spacemacs/lsp-haskell-nix-shell-args spacemacs/lsp-haskell-nix-shell-args ""))
        (command (append
                  ;; Change this to match your home directory/preferences
                  (append (list "nix-shell" "-I" ".") shell-args (list "--command" )
                          (list (mapconcat 'identity args " "))
                          )
                  (list (nix-current-sandbox)))))
    (progn
      (message "hie-nix command: %s" (mapconcat 'identity command " "))
      command)))

(defun hie-nix//haskell-nix-wrapper (args)
  (apply #'hie-nix//default-nix-wrapper (list (append args (list "--ghc-option" "-Wwarn"))))
  )

(defun hie-nix//disable-electric-indent ()
  "Disable electric indent mode if available"
  ;; use only internal indentation system from haskell
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))
