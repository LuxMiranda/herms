release: herms.hs Utils.hs Types.hs
	ghc -O2 herms.hs Utils.hs Types.hs -o herms
