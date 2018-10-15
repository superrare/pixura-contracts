######################################################
#### Utils
######################################################

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

clean: ## clean stack
	stack clean

hlint: ## hlint all
	find ./libs -name "*.hs" | xargs hlint "--ignore=Parse error" ;

stylish: ## stylish all
	find ./libs -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i;


######################################################
#### Smart Contract / Solidity related commands
######################################################

compile-contracts: ## compiles contracts and updates abi data in the data folder
	cd truffle; \
	truffle compile;
	cp truffle/build/contracts/* ./data

######################################################
#### Build
######################################################

build: ## build the library
  make compile-contracts; \
	stack build;
