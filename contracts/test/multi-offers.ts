import { expect } from './setup'
import { ethers } from 'hardhat'
import { Contract, Signer } from 'ethers'
import { waffle } from 'hardhat'

const provider = waffle.provider;

describe("Multi-offers", function () {

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

  describe("Offers", function () {
    it("Single Offer", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)

      const offerAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const requiredCost = offerAmount.add(await settings.calculateMarketplaceFee(offerAmount))
      await bazaar.connect(addr1).offer(offerAmount, token.address, tokenId, {value: requiredCost})

      const [bidders, amounts] = await bazaar.currentBidDetailsOfToken(token.address, tokenId)
      expect(bidders[0]).to.eq(addr1.address)
      expect(amounts[0]).to.eq(ethers.utils.parseEther("10"))
      expect(bidders.length).to.eq(1)
    })
    it("Multiple Offers", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)

      const balance = ethers.utils.formatEther(await provider.getBalance(addr1.address))

      const firstOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const firstRequiredCost = firstOfferAmount.add(await settings.calculateMarketplaceFee(firstOfferAmount))
      await bazaar.connect(addr1).offer(firstOfferAmount, token.address, tokenId, {value: firstRequiredCost})

      const secondOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("100"))
      const secondRequiredCost = secondOfferAmount.add(await settings.calculateMarketplaceFee(secondOfferAmount))

      await bazaar.connect(addr1).offer(secondOfferAmount, token.address, tokenId, {value: secondRequiredCost})

      const [bidders, amounts] = await bazaar.currentBidDetailsOfToken(token.address, tokenId)
      expect(bidders[0]).to.eq(addr1.address)
      expect(amounts[0]).to.eq(ethers.utils.parseEther("100"))
      expect(bidders.length).to.eq(1)
    })
    it("Multiple bidders", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)

      const balance_1 = ethers.utils.formatEther(await provider.getBalance(addr1.address))
      const balance_2 = ethers.utils.formatEther(await provider.getBalance(addr2.address))

      const firstOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const firstRequiredCost = firstOfferAmount.add(await settings.calculateMarketplaceFee(firstOfferAmount))
      await bazaar.connect(addr1).offer(firstOfferAmount, token.address, tokenId, {value: firstRequiredCost})

      const secondOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("100"))
      const secondRequiredCost = secondOfferAmount.add(await settings.calculateMarketplaceFee(secondOfferAmount))
      await bazaar.connect(addr2).offer(secondOfferAmount, token.address, tokenId, {value: secondRequiredCost})

      const [bidders, amounts] = await bazaar.currentBidDetailsOfToken(token.address, tokenId)
      expect(bidders.length).to.eq(2)
      expect(bidders[0]).to.eq(addr1.address)
      expect(amounts[0]).to.eq(ethers.utils.parseEther("10"))
      expect(bidders[1]).to.eq(addr2.address)
      expect(amounts[1]).to.eq(ethers.utils.parseEther("100"))

      expect(Math.round(Number(ethers.utils.formatEther(await provider.getBalance(addr1.address))))).to.eq(
        (Math.round(Number(balance_1) - Number(ethers.utils.formatEther(firstRequiredCost)))))
      expect(Math.round(Number(ethers.utils.formatEther(await provider.getBalance(addr2.address))))).to.eq(
        (Math.round(Number(balance_2) - Number(ethers.utils.formatEther(secondRequiredCost)))))
    })
  })
  describe("Accept Bid", function () {
    it("Accept with one offer", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const offerAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const requiredCost = offerAmount.add(await settings.calculateMarketplaceFee(offerAmount))
      await bazaar.connect(addr1).offer(offerAmount, token.address, tokenId, {value: requiredCost})

      await bazaar.connect(owner).safeAcceptOffer(token.address, addr1.address, tokenId, offerAmount, {
        gasLimit: 9500000
      })

      const [bidders, amounts] = await bazaar.currentBidDetailsOfToken(token.address, tokenId)
      expect(bidders.length).to.eq(0)
      expect(amounts.length).to.eq(0)

      expect(await token.ownerOf(tokenId)).to.eq(addr1.address)
      console.log(ethers.utils.formatEther(await provider.getBalance(owner.address)))
      console.log(ethers.utils.formatEther(await provider.getBalance(addr1.address)))
    })
    it("Accept with multiple offers", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const firstOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("100"))
      const firstRequiredCost = firstOfferAmount.add(await settings.calculateMarketplaceFee(firstOfferAmount))
      await bazaar.connect(addr1).offer(firstOfferAmount, token.address, tokenId, {value: firstRequiredCost})

      const secondOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const secondRequiredCost = secondOfferAmount.add(await settings.calculateMarketplaceFee(secondOfferAmount))
      await bazaar.connect(addr2).offer(secondOfferAmount, token.address, tokenId, {value: secondRequiredCost})

      const thirdOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("50"))
      const thirdRequiredCost = thirdOfferAmount.add(await settings.calculateMarketplaceFee(thirdOfferAmount))
      await bazaar.connect(addr3).offer(thirdOfferAmount, token.address, tokenId, {value: thirdRequiredCost})

      await bazaar.connect(owner).safeAcceptOffer(token.address, addr3.address, tokenId, thirdOfferAmount, {
        gasLimit: 9500000
      })
      expect(await token.ownerOf(tokenId)).to.eq(addr3.address)
      console.log(ethers.utils.formatEther(await provider.getBalance(owner.address)))
      console.log(ethers.utils.formatEther(await provider.getBalance(addr1.address)))
      console.log(ethers.utils.formatEther(await provider.getBalance(addr2.address)))
      console.log(ethers.utils.formatEther(await provider.getBalance(addr3.address)))
    })
  })
  describe("Cancel Bid", function () {
    it("Cancel single bid", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)

      const balance = ethers.utils.formatEther(await provider.getBalance(addr1.address))

      const offerAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const requiredCost = offerAmount.add(await settings.calculateMarketplaceFee(offerAmount))
      await bazaar.connect(addr1).offer(offerAmount, token.address, tokenId, {value: requiredCost})
      await bazaar.connect(addr1).cancelOffer(token.address, tokenId)

      const [bidders, amounts] = await bazaar.currentBidDetailsOfToken(token.address, tokenId)
      expect(bidders.length).to.eq(0)

      expect(Math.round(Number(ethers.utils.formatEther(await provider.getBalance(addr1.address))))).to.eq(
        Math.round(Number(balance)))
    })
  })

  describe("Buy", function () {
    it("Buy an NFT when there's an existing bid", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const balance = ethers.utils.formatEther(await provider.getBalance(addr1.address))

      const offerAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      let requiredCost = offerAmount.add(await settings.calculateMarketplaceFee(offerAmount))
      await bazaar.connect(addr1).offer(offerAmount, token.address, tokenId, {value: requiredCost})

      const buyCost = ethers.BigNumber.from(ethers.utils.parseEther("42"))
      await bazaar.connect(owner).setSalePrice(token.address, tokenId, buyCost)

      requiredCost = await buyCost.add(await settings.calculateMarketplaceFee(buyCost))
      await bazaar.connect(addr1).safeBuy(token.address, tokenId, buyCost, {value: requiredCost})

      const [bidders, amounts] = await bazaar.currentBidDetailsOfToken(token.address, tokenId)
      expect(bidders.length).to.eq(0)

      expect(await token.ownerOf(tokenId)).to.eq(addr1.address)
      console.log(ethers.utils.formatEther(await provider.getBalance(addr1.address)))
    })
  })

  describe("Getters", function () {
    it("Should get the highest bid", async function () {
      const tokenId = await token.getCurrentTokenIDTracker()
      await token.mint(owner.address, 1)
      await creatorRegistry.setTokenCreator(token.address, 1, owner.address)
      await token.setApprovalForAll(bazaar.address, true)

      const [bidder_empty, amount_empty] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      await expect(amount_empty).to.eq(0)

      const firstOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("10"))
      const firstRequiredCost = firstOfferAmount.add(await settings.calculateMarketplaceFee(firstOfferAmount))
      await bazaar.connect(addr1).offer(firstOfferAmount, token.address, tokenId, {value: firstRequiredCost})

      const secondOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("100"))
      const secondRequiredCost = secondOfferAmount.add(await settings.calculateMarketplaceFee(secondOfferAmount))
      await bazaar.connect(addr2).offer(secondOfferAmount, token.address, tokenId, {value: secondRequiredCost})

      const thirdOfferAmount = ethers.BigNumber.from(ethers.utils.parseEther("50"))
      const thirdRequiredCost = thirdOfferAmount.add(await settings.calculateMarketplaceFee(thirdOfferAmount))
      await bazaar.connect(addr3).offer(thirdOfferAmount, token.address, tokenId, {value: thirdRequiredCost})

      const [bidder, amount] = await bazaar.getHighestBidderAndBid(token.address, tokenId)
      expect(bidder).to.eq(addr2.address)
      expect(amount).to.eq(secondOfferAmount)
    })
  })
})
