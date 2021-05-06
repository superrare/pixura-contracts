pragma solidity 0.6.12;

import "openzeppelin-solidity-solc6/contracts/token/ERC721/IERC721.sol";
import "openzeppelin-solidity-solc6/contracts/math/SafeMath.sol";
import "openzeppelin-solidity-solc6/contracts/access/Ownable.sol";
import "./ISuperRareBazaar.sol";
import "./ISuperRareMarketplace.sol";
import "./ISuperRareAuctionHouseV2.sol";
import "./Marketplace/IMarketplaceSettings.sol";
import "./IERC721CreatorRoyalty.sol";
import "./Payments.sol";

contract SuperRareBazaar is Ownable, Payments, ISuperRareBazaar {
    using SafeMath for uint256;

    /////////////////////////////////////////////////////////////////////////
    // Structs
    /////////////////////////////////////////////////////////////////////////
    // A reserve auction.
    struct Auction {
        address payable auctionCreator;
        uint256 creationBlock;
        uint256 lengthOfAuction;
        uint256 startingBlock;
        uint256 reservePrice;
        uint256 minimumBid;
        bytes32 auctionType;
    }

    // The active bid for a given token, contains the bidder, the marketplace fee at the time of the bid, and the amount of wei placed on the token
    struct ActiveBid {
        address payable bidder;
        uint8 marketplaceFee;
        uint256 amount;
    }

    // The sale price for a given token containing the seller and the amount of wei to be sold for
    struct SalePrice {
        address payable seller;
        uint256 amount;
    }

    /////////////////////////////////////////////////////////////////////////
    // Interfaces
    /////////////////////////////////////////////////////////////////////////

    // Marketplace Settings Interface
    IMarketplaceSettings public iMarketplaceSettings;

    // Creator Royalty Interface
    IERC721CreatorRoyalty public iERC721CreatorRoyalty;

    /////////////////////////////////////////////////////////////////////////
    // Constants
    /////////////////////////////////////////////////////////////////////////

    // Types of Auctions
    bytes32 public constant COLDIE_AUCTION = "COLDIE_AUCTION";
    bytes32 public constant SCHEDULED_AUCTION = "SCHEDULED_AUCTION";
    bytes32 public constant NO_AUCTION = bytes32(0);

    /////////////////////////////////////////////////////////////////////////
    // Marketplace State Variables
    /////////////////////////////////////////////////////////////////////////

    // Mapping from ERC721 contract to mapping of tokenId to sale price.
    mapping(address => mapping(uint256 => SalePrice)) private tokenPrices;

    // Mapping of ERC721 contract to mapping of token ID to mapping of bidder to current bid.
    mapping(address => mapping(uint256 => mapping(address => ActiveBid))) private tokenCurrentBids;

    // Mapping of ERC721 contract to mapping of token ID to mapping of bidders.
    mapping(address => mapping(uint256 => address[])) private bidders;

    /////////////////////////////////////////////////////////////////////////
    // Auction State Variables
    /////////////////////////////////////////////////////////////////////////

    // Mapping from ERC721 contract to mapping of tokenId to Auctions.
    mapping(address => mapping(uint256 => Auction)) private auctions;

    // Mapping of ERC721 contract to mapping of token ID to the current bid amount.
    mapping(address => mapping(uint256 => ActiveBid)) private currentBids;

    // Number of blocks to begin refreshing auction lengths
    uint256 public auctionLengthExtension;

    // Max Length that an auction can be
    uint256 public maxLength;

    // A minimum increase in bid amount when out bidding someone.
    uint8 public minimumBidIncreasePercentage; // 10 = 10%

    ISuperRareMarketplace public iSuperRareMarketplace;

    ISuperRareAuctionHouseV2 public iSuperRareAuctionHouseV2;

    event AuctionBid(
        address indexed _contractAddress,
        address indexed _bidder,
        uint256 indexed _tokenId,
        uint256 _amount,
        bool _startedAuction,
        uint256 _newAuctionLength,
        address _previousBidder
    );

    constructor(address _iSuperRareMarketplace, address _iSuperRareAuctionHouseV2,
        address _iMarketSettings, address _iERC721CreatorRoyalty)
        public
    {
        maxLength = 43200; // ~ 7 days == 7 days * 24 hours * 3600s / 14s per block
        auctionLengthExtension = 65; // ~ 15 min == 15 min * 60s / 14s per block
        minimumBidIncreasePercentage = 10;

        require(
            _iSuperRareMarketplace != address(0),
            "constructor::Cannot have null address for _iSuperRareMarketplace"
        );

        require(
            _iSuperRareAuctionHouseV2 != address(0),
            "constructor::Cannot have null address for _iSuperRareAuctionHouseV2"
        );

        require(
            _iMarketSettings != address(0),
            "constructor::Cannot have null address for _iMarketSettings"
        );

        require(
            _iERC721CreatorRoyalty != address(0),
            "constructor::Cannot have null address for _iERC721CreatorRoyalty"
        );

        // Set iSuperRareMarketplace
        iSuperRareMarketplace = ISuperRareMarketplace(_iSuperRareMarketplace);

        // Set iSuperRareAuctionHouseV2
        iSuperRareAuctionHouseV2 = ISuperRareAuctionHouseV2(_iSuperRareAuctionHouseV2);

        // Set iMarketSettings
        iMarketplaceSettings = IMarketplaceSettings(_iMarketSettings);

        // Set iERC721CreatorRoyalty
        iERC721CreatorRoyalty = IERC721CreatorRoyalty(_iERC721CreatorRoyalty);
    }

    /////////////////////////////////////////////////////////////////////////
    // Auction Functions
    /////////////////////////////////////////////////////////////////////////
    function createColdieAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _reservePrice,
        uint256 _lengthOfAuction
    ) external override {
        // Rules
        require(
            _lengthOfAuction <= maxLength,
            "createColdieAuction::Cannot have auction longer than maxLength"
        );

        require(
            auctions[_contractAddress][_tokenId].auctionType == NO_AUCTION ||
                (msg.sender !=
                    auctions[_contractAddress][_tokenId].auctionCreator),
            "createColdieAuction::Cannot have a current auction"
        );

        require(
            _lengthOfAuction > 0,
            "createColdieAuction::_lengthOfAuction must be > 0"
        );
        require(
            _reservePrice >= 0,
            "createColdieAuction::_reservePrice must be >= 0"
        );
        require(
            _reservePrice <= iMarketplaceSettings.getMarketplaceMaxValue(),
            "createColdieAuction::Cannot set reserve price higher than max value"
        );

        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "createColdieAuction(address,uint256,uint256,uint256)",
                _contractAddress, _tokenId, _reservePrice, _lengthOfAuction)
        );
        require(success);
        // Get the highest bidder/bid amount for that token if applicable
        (address payable bidder, uint256 amount) = getHighestBidderAndBid(_contractAddress, _tokenId);
        if (amount >= _reservePrice) {
            // Set the new bid
            currentBids[_contractAddress][_tokenId] = ActiveBid(
                bidder,
                iMarketplaceSettings.getMarketplaceFeePercentage(),
                amount
            );

            // Start the coldie auction
            auctions[_contractAddress][_tokenId].startingBlock = block.number;

            // Transfer token to escrow
            IERC721 erc721 = IERC721(_contractAddress);
            erc721.transferFrom(
                auctions[_contractAddress][_tokenId].auctionCreator,
                address(this),
                _tokenId
            );

            // Not sure if this is right
            emit AuctionBid(
                _contractAddress,
                bidder,
                _tokenId,
                amount,
                true,
                0,
                address(0)
            );

            // Make sure to clear the bid
            (success, data) = address(iSuperRareMarketplace).delegatecall(
                abi.encodeWithSignature(
                    "_resetBid(address,address,uint256)",
                    _contractAddress, bidder, _tokenId)
            );
            require (success);
        }
        // Refund and reset the offers for this token
        (success, data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "_refundAndResetAllBids(address,uint256)",
                _contractAddress, _tokenId)
        );
        require (success);
    }

    function createScheduledAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _minimumBid,
        uint256 _lengthOfAuction,
        uint256 _startingBlock
    ) external override {
        require(
            _lengthOfAuction > 0,
            "createScheduledAuction::_lengthOfAuction must be greater than 0"
        );
        require(
            _lengthOfAuction <= maxLength,
            "createScheduledAuction::Cannot have auction longer than maxLength"
        );
        require(
            _startingBlock > block.number,
            "createScheduledAuction::_startingBlock must be greater than block.number"
        );
        require(
            _minimumBid <= iMarketplaceSettings.getMarketplaceMaxValue(),
            "createScheduledAuction::Cannot set minimum bid higher than max value"
        );
        require(
            auctions[_contractAddress][_tokenId].auctionType == NO_AUCTION ||
                (msg.sender !=
                    auctions[_contractAddress][_tokenId].auctionCreator),
            "createScheduledAuction::Cannot have a current auction"
        );

        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "createScheduledAuction(address,uint256,uint256,uint256,uint256)",
                _contractAddress, _tokenId, _minimumBid, _lengthOfAuction, _startingBlock)
        );
        require(success);
    }

    function cancelAuction(address _contractAddress, uint256 _tokenId) external override {
        require(
            auctions[_contractAddress][_tokenId].auctionType != NO_AUCTION,
            "cancelAuction::Must have a current auction"
        );
        require(
            auctions[_contractAddress][_tokenId].startingBlock == 0 ||
                auctions[_contractAddress][_tokenId].startingBlock >
                block.number,
            "cancelAuction::auction cannot be started"
        );
        require(
            auctions[_contractAddress][_tokenId].auctionCreator == msg.sender,
            "cancelAuction::must be the creator of the auction"
        );

        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "cancelAuction(address,uint256)",
                _contractAddress, _tokenId)
        );
        require(success);
    }

    function bid(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _amount
    ) external override payable {
        Auction memory auction = auctions[_contractAddress][_tokenId];

        // Must have existing auction.
        require(
            auction.auctionType != NO_AUCTION,
            "bid::Must have existing auction"
        );

        // Must have existing auction.
        require(
            auction.auctionCreator != msg.sender,
            "bid::Cannot bid on your own auction"
        );

        // Must have pending coldie auction or running auction.
        require(
            auction.startingBlock <= block.number,
            "bid::Must have a running auction or pending coldie auction"
        );

        // Check that bid is greater than 0.
        require(_amount > 0, "bid::Cannot bid 0 Wei.");

        // Check that bid is less than max value.
        require(
            _amount <= iMarketplaceSettings.getMarketplaceMaxValue(),
            "bid::Cannot bid higher than max value"
        );

        // Check that bid is larger than min value.
        require(
            _amount >= iMarketplaceSettings.getMarketplaceMinValue(),
            "bid::Cannot bid lower than min value"
        );

        // Check that bid is larger than minimum bid value or the reserve price.
        require(
            (_amount >= auction.reservePrice && auction.minimumBid == 0) ||
                (_amount >= auction.minimumBid && auction.reservePrice == 0),
            "bid::Cannot bid lower than reserve or minimum bid"
        );

        // Auction cannot have ended.
        require(
            auction.startingBlock == 0 ||
                block.number <
                auction.startingBlock.add(auction.lengthOfAuction),
            "bid::Cannot have ended"
        );

        // Check that enough ether was sent.
        uint256 requiredCost =
            _amount.add(iMarketplaceSettings.calculateMarketplaceFee(_amount));

        require(requiredCost == msg.value, "bid::Must bid the correct amount.");

        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "bid(address,uint256,uint256)",
                _contractAddress, _tokenId, _amount)
        );
        require(success);
    }

    function settleAuction(address _contractAddress, uint256 _tokenId) external override {
        Auction memory auction = auctions[_contractAddress][_tokenId];

        require(
            auction.auctionType != NO_AUCTION && auction.startingBlock != 0,
            "settleAuction::Must have a current auction that has started"
        );
        require(
            block.number >= auction.startingBlock.add(auction.lengthOfAuction),
            "settleAuction::Can only settle ended auctions."
        );

        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "settleAuction(address,uint256)",
                _contractAddress, _tokenId)
        );
        require(success);
    }

    function getAuctionDetails(address _contractAddress, uint256 _tokenId)
        external
        view
        override
        returns (
            bytes32,
            uint256,
            address,
            uint256,
            uint256,
            uint256,
            uint256
        )
    {
        Auction memory auction = auctions[_contractAddress][_tokenId];

        return (
            auction.auctionType,
            auction.creationBlock,
            auction.auctionCreator,
            auction.lengthOfAuction,
            auction.startingBlock,
            auction.minimumBid,
            auction.reservePrice
        );
    }

    function getCurrentBid(address _contractAddress, uint256 _tokenId)
        external
        view
        override
        returns (address, uint256)
    {
        return (
            currentBids[_contractAddress][_tokenId].bidder,
            currentBids[_contractAddress][_tokenId].amount
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // Marketplace Functions
    /////////////////////////////////////////////////////////////////////////
    function tokenPrice(address _originContract, uint256 _tokenId)
        external
        view
        override
        returns (uint256)
    {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId); // TODO: Make sure to write test to verify that this returns 0 when it fails

        if (_priceSetterStillOwnsTheToken(_originContract, _tokenId)) {
            return tokenPrices[_originContract][_tokenId].amount;
        }
        return 0;
    }

    function tokenPriceFeeIncluded(address _originContract, uint256 _tokenId)
        external
        view
        override
        returns (uint256)
    {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId); // TODO: Make sure to write test to verify that this returns 0 when it fails

        if (_priceSetterStillOwnsTheToken(_originContract, _tokenId)) {
            return
                tokenPrices[_originContract][_tokenId].amount.add(
                    iMarketplaceSettings.calculateMarketplaceFee(
                        tokenPrices[_originContract][_tokenId].amount
                    )
                );
        }
        return 0;
    }

    function ownerMustHaveMarketplaceApproved(
        address _originContract,
        uint256 _tokenId
    ) public view {
        IERC721 erc721 = IERC721(_originContract);
        address owner = erc721.ownerOf(_tokenId);
        require(
            erc721.isApprovedForAll(owner, address(this)),
            "owner must have approved contract"
        );
    }

    function _priceSetterStillOwnsTheToken(
        address _originContract,
        uint256 _tokenId
    ) public view returns (bool) {
        IERC721 erc721 = IERC721(_originContract);
        return
            erc721.ownerOf(_tokenId) ==
            tokenPrices[_originContract][_tokenId].seller;
    }

    function setSalePrice(
        address _originContract,
        uint256 _tokenId,
        uint256 _amount
    ) external override {
        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "setSalePrice(address,uint256,uint256)",
                _originContract, _tokenId, _amount)
        );
        require(success);
    }

    function safeBuy(
        address _originContract,
        uint256 _tokenId,
        uint256 _amount
    ) external override payable {
        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "safeBuy(address,uint256,uint256)",
                _originContract, _tokenId, _amount)
        );
        require(success);
    }

    function buy(address _originContract, uint256 _tokenId) public override payable {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId);

        // Check that the person who set the price still owns the token.
        require(
            _priceSetterStillOwnsTheToken(_originContract, _tokenId),
            "buy::Current token owner must be the person to have the latest price."
        );

        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "buy(address,uint256)",
                _originContract, _tokenId)
        );
        require(success);
    }

    function offer(
        uint256 _newBidAmount,
        address _originContract,
        uint256 _tokenId
    ) external override payable {
        // Check that bid is greater than 0. Currently removing bid increase requirements for now.
        require(_newBidAmount > 0, "bid::Cannot bid 0 Wei.");

        // Check that enough ether was sent.
        uint256 requiredCost =
            _newBidAmount.add(
                iMarketplaceSettings.calculateMarketplaceFee(_newBidAmount)
            );
        require(
            requiredCost == msg.value,
            "offer::Must purchase the token for the correct price."
        );

        // Check that bidder is not owner.
        IERC721 erc721 = IERC721(_originContract);
        address tokenOwner = erc721.ownerOf(_tokenId);
        require(tokenOwner != msg.sender, "bid::Bidder cannot be owner.");

        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "offer(uint256,address,uint256)",
                _newBidAmount, _originContract, _tokenId)
        );
        require(success);
    }

    function safeAcceptOffer(
        address _originContract,
		address payable _bidder,
        uint256 _tokenId,
        uint256 _amount
    ) external override {
        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "safeAcceptOffer(address,address,uint256,uint256)",
                _originContract, _bidder, _tokenId, _amount)
        );
        require(success);
    }

    function acceptOffer(address _originContract, address payable _bidder, uint256 _tokenId)
        public
        override
    {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId);

        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "acceptOffer(address,address,uint256)",
                _originContract, _bidder, _tokenId)
        );
        require(success);
    }

    function cancelOffer(address _originContract, uint256 _tokenId) external override {
        // Check that the sender has a current bid.
        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "cancelOffer(address,uint256)",
                _originContract, _tokenId)
        );
        require(success);
    }

    function currentBidDetailsOfToken(address _originContract, uint256 _tokenId)
        external
        view
        override
        returns (address[] memory, uint256[] memory)
    {
        address[] memory currentBidders = bidders[_originContract][_tokenId];
        uint256[] memory bidAmounts = new uint256[](currentBidders.length);
        // Get bid amounts from the list of bidders
        for (uint i = 0; i < currentBidders.length; i++) {
            bidAmounts[i] = tokenCurrentBids[_originContract][_tokenId][currentBidders[i]].amount;
        }
        return (
            currentBidders,
            bidAmounts
        );
    }

    function getHighestBidderAndBid(address _originContract, uint256 _tokenId)
        public
        view
        returns (address payable, uint256)
    {
        if (bidders[_originContract][_tokenId].length == 0) {
            return (address(0), 0);
        }
        address[] memory currentBidders = bidders[_originContract][_tokenId];
        ActiveBid memory highestBid = tokenCurrentBids[_originContract][_tokenId][currentBidders[0]];
        for (uint i = 1; i < currentBidders.length; i++) {
            if (tokenCurrentBids[_originContract][_tokenId][currentBidders[i]].amount > highestBid.amount) {
                highestBid = tokenCurrentBids[_originContract][_tokenId][currentBidders[i]];
            }
        }
        return (highestBid.bidder, highestBid.amount);
    }
}
