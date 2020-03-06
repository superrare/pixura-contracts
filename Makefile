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
	find hs-contracts -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i;

init: ## install node files
	yarn && \
	npx spago install

######################################################
#### Smart Contract / Solidity related commands
######################################################

compile-contracts: ## compiles contracts 
	npx chanterelle compile

######################################################
#### Build
######################################################

build: ## build the library
	make compile-contracts && \
	stack build;
