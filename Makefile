stack-all:
	stack --resolver nightly build $(FLAGS)
	@echo
	stack --resolver lts-16 build $(FLAGS)
	@echo
	stack --resolver lts-14 build $(FLAGS)
	@echo
	stack --resolver lts-13 build $(FLAGS)
	@echo
	stack --resolver lts-12 build $(FLAGS)
	@echo
	stack --resolver lts-11 build $(FLAGS)
#	@echo lts10 has no requestFromURI_
#	stack --resolver lts-10 build $(FLAGS)
