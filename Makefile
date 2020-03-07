.PHONY: help clean hlint stylish init compile-contracts purs-contract-gen hs-build purs-build purs-build-all deploy-test-chain takedown-test-chain run-contract-tests contract-tests
.DEFAULT_GOAL := help

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

purs-contract-gen: ## Generate purscript libraries for smart contracts
	npx chanterelle codegen

######################################################
#### Build
######################################################

hs-build: ## Build haskell bindings
	make compile-contracts && \
	stack build;

purs-build: ## Build purescript library
	npx spago build;

purs-build-all: ## Compiles contracts, codegens purescript bindings, and builds purescript
	make compile-contracts && \
	make purs-contract-gen && \
	make purs-build


######################################################
#### Test
######################################################

deploy-test-chain: ## Deploys the test chain
	docker-compose -p pixura-contracts up -d

takedown-test-chain: ## Removes chain containers and wipes volumes
	docker-compose -p pixura-contracts down -v

run-contract-tests: ## Run contract tests
	npx spago test

contract-tests: ## Deploy test environment and run contract tests
	make deploy-test-chain && \
	make run-contract-tests; \
	make takedown-test-chain