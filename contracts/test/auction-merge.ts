import { expect } from './setup'
import { ethers } from 'hardhat'
import { Contract, Signer } from 'ethers'
import { waffle } from 'hardhat'

const provider = waffle.provider;

describe("Merge", function () {

  let market
  let settings
  let royalty
  let token
  let creatorRegistry
  let auction
  let bazaar
  let owner
  let addr1
  let addr2
  let addr3
  let addrs

  beforeEach(async function () {
    // Get the ContractFactory and Signers here.
    [owner, addr1, addr2, addr3, ...addrs] = await ethers.getSigners()
    settings = await (await ethers.getContractFactory("MarketplaceSettings")).deploy()
    token = await (await ethers.getContractFactory("SuperRareToken")).deploy()
    creatorRegistry = await (await ethers.getContractFactory("SuperRareTokenCreatorRegistry")).deploy([token.address])
    royalty = await (await ethers.getContractFactory("SuperRareRoyaltyRegistry")).deploy(creatorRegistry.address)
    await royalty.setPercentageForSetERC721ContractRoyalty(token.address, 10)
    market = await (await ethers.getContractFactory("SuperRareMarketplace")).deploy(settings.address, royalty.address)
    auction = await (await ethers.getContractFactory("SuperRareAuctionHouse")).deploy(settings.address, royalty.address)
    bazaar = await (await ethers.getContractFactory("SuperRareBazaar")).deploy(market.address, auction.address,
      settings.address, royalty.address)
    await settings.grantMarketplaceAccess(bazaar.address)
  })

  describe("Coldie", function () {
    it("Create auction with no offers", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      await bazaar.connect(owner).createColdieAuction(token.address, tokenId, price, length)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(0)
    })
    it("Create auction with one offer", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const firstBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("11"))
      const firstRequiredCost = firstBidAmount.add(await settings.calculateMarketplaceFee(firstBidAmount))
      await bazaar.connect(addr1).offer(firstBidAmount, token.address, tokenId, {value: firstRequiredCost})

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      await bazaar.connect(owner).createColdieAuction(token.address, tokenId, price, length)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(ethers.BigNumber.from(ethers.utils.parseEther("11")))
      expect(bidder).to.eq(addr1.address)

      const [marketBidder, marketAmount] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      await expect(marketAmount).to.eq(0)

      expect(await token.ownerOf(tokenId)).to.eq(
        (bazaar.address))

    })
    it("Create auction with low offer", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const balance = ethers.utils.formatEther(await provider.getBalance(addr1.address))

      const firstBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("1"))
      const firstRequiredCost = firstBidAmount.add(await settings.calculateMarketplaceFee(firstBidAmount))
      await bazaar.connect(addr1).offer(firstBidAmount, token.address, tokenId, {value: firstRequiredCost})

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      await bazaar.connect(owner).createColdieAuction(token.address, tokenId, price, length)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(0)

      const [marketBidder, marketAmount] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      await expect(marketAmount).to.eq(0)

      expect(Math.round(Number(ethers.utils.formatEther(await provider.getBalance(addr1.address))))).to.eq(
        (Math.round(Number(balance))))

      expect(await token.ownerOf(tokenId)).to.eq(
        (owner.address))

    })
    it("Create auction with multiple offers", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const firstBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("7"))
      const firstRequiredCost = firstBidAmount.add(await settings.calculateMarketplaceFee(firstBidAmount))
      await bazaar.connect(addr1).offer(firstBidAmount, token.address, tokenId, {value: firstRequiredCost})

      const secondBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("100"))
      const secondRequiredCost = secondBidAmount.add(await settings.calculateMarketplaceFee(secondBidAmount))
      await bazaar.connect(addr2).offer(secondBidAmount, token.address, tokenId, {value: secondRequiredCost})

      const thirdBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("11"))
      const thirdRequiredCost = thirdBidAmount.add(await settings.calculateMarketplaceFee(thirdBidAmount))
      await bazaar.connect(addr3).offer(thirdBidAmount, token.address, tokenId, {value: thirdRequiredCost})

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      await bazaar.connect(owner).createColdieAuction(token.address, tokenId, price, length)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(ethers.BigNumber.from(ethers.utils.parseEther("100")))
      expect(bidder).to.eq(addr2.address)

      const [marketBidder, marketAmount] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      await expect(marketAmount).to.eq(0)

      expect(await token.ownerOf(tokenId)).to.eq(
        (bazaar.address))
    })
  })
  describe("Scheduled", function () {
    it("Create auction with no offers", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      const startingBlock = 100;
      await bazaar.connect(owner).createScheduledAuction(token.address, tokenId, price, length, startingBlock)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(0)
    })
    it("Create auction with one offer", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const firstBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("11"))
      const firstRequiredCost = firstBidAmount.add(await settings.calculateMarketplaceFee(firstBidAmount))
      await bazaar.connect(addr1).offer(firstBidAmount, token.address, tokenId, {value: firstRequiredCost})

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      const startingBlock = 100;
      await bazaar.connect(owner).createScheduledAuction(token.address, tokenId, price, length, startingBlock)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(ethers.BigNumber.from(ethers.utils.parseEther("11")))
      expect(bidder).to.eq(addr1.address)

      const [marketBidder, marketAmount] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      await expect(marketAmount).to.eq(0)

      expect(await token.ownerOf(tokenId)).to.eq(
        (bazaar.address))

    })
    it("Create auction with low offer", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const balance = ethers.utils.formatEther(await provider.getBalance(addr1.address))

      const firstBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("1"))
      const firstRequiredCost = firstBidAmount.add(await settings.calculateMarketplaceFee(firstBidAmount))
      await bazaar.connect(addr1).offer(firstBidAmount, token.address, tokenId, {value: firstRequiredCost})

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      const startingBlock = 100;
      await bazaar.connect(owner).createScheduledAuction(token.address, tokenId, price, length, startingBlock)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(0)

      const [marketBidder, marketAmount] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      await expect(marketAmount).to.eq(0)

      expect(Math.round(Number(ethers.utils.formatEther(await provider.getBalance(addr1.address))))).to.eq(
        (Math.round(Number(balance))))

      expect(await token.ownerOf(tokenId)).to.eq(
        (bazaar.address))

    })
    it("Create auction with multiple offers", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const firstBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("7"))
      const firstRequiredCost = firstBidAmount.add(await settings.calculateMarketplaceFee(firstBidAmount))
      await bazaar.connect(addr1).offer(firstBidAmount, token.address, tokenId, {value: firstRequiredCost})

      const secondBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("100"))
      const secondRequiredCost = secondBidAmount.add(await settings.calculateMarketplaceFee(secondBidAmount))
      await bazaar.connect(addr2).offer(secondBidAmount, token.address, tokenId, {value: secondRequiredCost})

      const thirdBidAmount = ethers.BigNumber.from(ethers.utils.parseEther("11"))
      const thirdRequiredCost = thirdBidAmount.add(await settings.calculateMarketplaceFee(thirdBidAmount))
      await bazaar.connect(addr3).offer(thirdBidAmount, token.address, tokenId, {value: thirdRequiredCost})

      const price = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const length = 20
      const startingBlock = 100;
      await bazaar.connect(owner).createScheduledAuction(token.address, tokenId, price, length, startingBlock)

      const [bidder, amount] = await bazaar.getCurrentBid(token.address, tokenId)
      expect(amount).to.eq(ethers.BigNumber.from(ethers.utils.parseEther("100")))
      expect(bidder).to.eq(addr2.address)

      const [marketBidder, marketAmount] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      await expect(marketAmount).to.eq(0)

      expect(await token.ownerOf(tokenId)).to.eq(
        (bazaar.address))
    })
  })
})
