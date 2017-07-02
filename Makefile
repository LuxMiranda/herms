release: herms.hs Utils.hs Types.hs AddCLI.hs
	ghc -threaded -O2 herms.hs Utils.hs Types.hs AddCLI.hs -o herms
