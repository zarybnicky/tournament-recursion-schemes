all:
	ghcid -T run -c "cabal new-repl --ghc-options=-fobject-code --ghc-options=-fno-break-on-exception --ghc-options=-fno-break-on-error --ghc-options=-ferror-spans --ghc-options=-j" --restart=tournament.cabal
