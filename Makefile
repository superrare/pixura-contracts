.PHONY: help clean hlint stylish init compile-contracts purs-contract-gen hs-build purs-build purs-build-all deploy-test-chain takedown-test-chain run-contract-tests contract-tests migrate-marketplaceV2
.DEFAULT_GOAL := help

######################################################
#### Env
######################################################
MARKETPLACEV2_CONFIG ?= "./deploy-configs/marketplacev2.json"
SUPERRARE_LEGACY_CONFIG ?= "./deploy-configs/superrareLegacy.json"

######################################################
#### Utils
######################################################

help: ## Ask for help!
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

clean: ## clean stack
	stack clean

hlint: ## hlint all
	find ./libs -name "*.hs" | xargs hlint "--ignore=Parse error" ;

stylish: ## stylish all
	find hs-contracts -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i;

init: ## install node files
	yarn && \
	yarn spago install && \
	yarn spago build -d

######################################################
#### Smart Contract / Solidity related commands
######################################################
compile-contracts-v4: ## compiles contracts solc v4
	@[ -d contracts/v4/node_modules ] || ( cp -r node_modules contracts/v4/node_modules ) && \
	yarn chanterelle -r contracts/v4 compile

compile-contracts-v5: ## compiles contracts solc v5
	@[ -d contracts/v5/node_modules ] || ( cp -r node_modules contracts/v5/node_modules ) && \
	yarn chanterelle -r contracts/v5 compile

compile-contracts: ## compiles contracts 
	make compile-contracts-v4 && \
	make compile-contracts-v5

purs-contract-gen-v4: ## Generate purscript libraries for solc v4 smart contracts
	yarn chanterelle -r contracts/v4 codegen

purs-contract-gen-v5: ## Generate purscript libraries for solc v5 smart contracts
	yarn chanterelle -r contracts/v5 codegen

purs-contract-gen: ## Generate purscript libraries for smart contracts
	make purs-contract-gen-v4 && \
	make purs-contract-gen-v5

######################################################
#### Build
######################################################

hs-build: ## Build haskell bindings
	make compile-contracts && \
	stack build;

purs-build: ## Build purescript library
	yarn spago build;

purs-build-all: ## Compiles contracts, codegens purescript bindings, and builds purescript
	make compile-contracts && \
	make purs-contract-gen && \
	make purs-build

######################################################
#### Migrations
######################################################

migrate-marketplaceV2:  ## Deploy test environment and run contract tests
	CONFIG=$(MARKETPLACEV2_CONFIG) \
	yarn spago run --main Migrations.SuperRareMarketAuctionV2

migrate-legacy:  ## Deploy test environment and run contract tests
	CONFIG=$(SUPERRARE_LEGACY_CONFIG) \
	yarn spago run --main Migrations.SuperRareLegacy

######################################################
#### Test
######################################################

deploy-test-chain: ## Deploys the test chain
	docker-compose -p pixura-contracts up -d

takedown-test-chain: ## Removes chain containers and wipes volumes
	docker-compose -p pixura-contracts kill && \
	docker-compose -p pixura-contracts down -v

run-contract-tests: ## Run contract tests
	npx spago test

contract-tests: ## Deploy test environment and run contract tests
	make deploy-test-chain && \
	sleep 3 && \
	make run-contract-tests; \
	make takedown-test-chain