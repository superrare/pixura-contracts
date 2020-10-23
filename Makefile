.PHONY: help clean hlint stylish init compile-contracts purs-contract-gen hs-build purs-build purs-build-all deploy-test-chain takedown-test-chain run-contract-tests contract-tests migrate-marketplaceV2
.DEFAULT_GOAL := help

######################################################
#### Env
######################################################
MARKETPLACEV2_CONFIG ?= "./deploy-configs/marketplacev2.json"
SUPERRARE_LEGACY_CONFIG ?= "./deploy-configs/superrareLegacy.json"
SUPERRARE_AUCTION_HOUSE_CONFIG ?= "./deploy-configs/superrareAuctionHouse.json"
SUPERRARE_TOKEN_CREATOR_REGISTRY_CONFIG ?= "./deploy-configs/superrareTokenCreatorRegistry.json"
SUPERRARE_ROYALTY_REGISTRY_CONFIG ?= "./deploy-configs/superrareRoyaltyRegistry.json"
MARKETPLACE_SETTINGS_CONFIG ?= "./deploy-configs/marketplaceSettings.json"

######################################################
#### Utils
######################################################

help: ## Ask for help!
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

clean: ## clean stack
	stack clean
	rm -rf node_modules \
		output \
		.spago


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
compile-contracts: ## compiles contracts 
	yarn chanterelle compile

purs-contract-gen: ## Generate purscript libraries for smart contracts
	yarn chanterelle codegen

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

migrate-auction-house:  ## Migration for Auction House
	CONFIG=$(SUPERRARE_AUCTION_HOUSE_CONFIG) \
	yarn spago run --main Migrations.SuperRareAuctionHouse

migrate-token-creator-registry:  ## Migration for token creator registry
	CONFIG=$(SUPERRARE_TOKEN_CREATOR_REGISTRY_CONFIG) \
	yarn spago run --main Migrations.SuperRareTokenCreatorRegistry

migrate-royalty-registry:  ## Migration for royalty registry
	CONFIG=$(SUPERRARE_ROYALTY_REGISTRY_CONFIG) \
	yarn spago run --main Migrations.SuperRareRoyaltyRegistry

migrate-marketplace-settings:  ## Migration for marketplace settings
	CONFIG=$(MARKETPLACE_SETTINGS_CONFIG) \
	yarn spago run --main Migrations.MarketplaceSettings

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