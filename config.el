(setq haskell-modes '(haskell-mode literate-haskell-mode))

(defvar spacemacs/lsp-haskell-nix-wrapped 't
  "Whether shell commands should be wrapped by nix. For now there is no
  autodetection for a project and it defaults to nix. Don't forget that you can
  override this with .dir-locals if you have one pesky project that breaks the pattern.")

(defvar spacemacs/lsp-haskell-nix-shell-args (list "--arg" "hie" "true")
  "Any additional args to give to nix shell. E.g, if your hie is hidden behind
   a shell.nix param called hie, set this to (list \"--arg\" \"hie\" \"true\"")
