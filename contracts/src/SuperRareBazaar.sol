pragma solidity 0.6.12;

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

    constructor(address _iSuperRareMarketplace, address _iSuperRareAuctionHouseV2,
        address _iMarketSettings, address _iERC721CreatorRoyalty)
        public
    {
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

    function createColdieAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _reservePrice,
        uint256 _lengthOfAuction
    ) external override {
        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "createColdieAuction(address,uint256,uint256,uint256)",
                _contractAddress, _tokenId, _reservePrice, _lengthOfAuction)
        );
        require(success);
    }

    function createScheduledAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _minimumBid,
        uint256 _lengthOfAuction,
        uint256 _startingBlock
    ) external override {
        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "createScheduledAuction(address,uint256,uint256,uint256,uint256)",
                _contractAddress, _tokenId, _minimumBid, _lengthOfAuction, _startingBlock)
        );
        require(success);
    }

    function cancelAuction(address _contractAddress, uint256 _tokenId) external override {
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
        (bool success, bytes memory data) = address(iSuperRareAuctionHouseV2).delegatecall(
            abi.encodeWithSignature(
                "bid(address,uint256,uint256)",
                _contractAddress, _tokenId, _amount)
        );
        require(success);
    }

    function settleAuction(address _contractAddress, uint256 _tokenId) external override {
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

    // Marketplace Functionality
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
        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "buy(address,uint256)",
                _originContract, _tokenId)
        );
        require(success);
    }

    /* function tokenPrice(address _originContract, uint256 _tokenId)
        external
        view
        override
        returns (uint256)
    {
    }

    function tokenPriceFeeIncluded(address _originContract, uint256 _tokenId)
        public
        view
        override
        returns (uint256)
    {
    } */

    function offer(
        uint256 _newBidAmount,
        address _originContract,
        uint256 _tokenId
    ) external override payable {
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
        (bool success, bytes memory data) = address(iSuperRareMarketplace).delegatecall(
            abi.encodeWithSignature(
                "acceptOffer(address,address,uint256)",
                _originContract, _bidder, _tokenId)
        );
        require(success);
    }

    function cancelOffer(address _originContract, uint256 _tokenId) external override {
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
		external
		view
        override
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
