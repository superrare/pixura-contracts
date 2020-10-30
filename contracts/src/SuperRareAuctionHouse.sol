pragma solidity 0.6.12;

import "openzeppelin-solidity-solc6/contracts/token/ERC721/IERC721.sol";
import "openzeppelin-solidity-solc6/contracts/math/SafeMath.sol";
import "openzeppelin-solidity-solc6/contracts/access/Ownable.sol";
import "./IERC721CreatorRoyalty.sol";
import "./Marketplace/IMarketplaceSettings.sol";
import "./Payments.sol";

contract SuperRareAuctionHouse is Ownable, Payments {
    using SafeMath for uint256;

    /////////////////////////////////////////////////////////////////////////
    // Constants
    /////////////////////////////////////////////////////////////////////////

    // Types of Auctions
    bytes32 public constant COLDIE_AUCTION = "COLDIE_AUCTION";
    bytes32 public constant SCHEDULED_AUCTION = "SCHEDULED_AUCTION";
    bytes32 public constant NO_AUCTION = bytes32(0);

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

    /////////////////////////////////////////////////////////////////////////
    // State Variables
    /////////////////////////////////////////////////////////////////////////

    // Marketplace Settings Interface
    IMarketplaceSettings public iMarketSettings;

    // Creator Royalty Interface
    IERC721CreatorRoyalty public iERC721CreatorRoyalty;

    // Mapping from ERC721 contract to mapping of tokenId to Auctions.
    mapping(address => mapping(uint256 => Auction)) private auctions;

    // Mapping of ERC721 contract to mapping of token ID to the current bid amount.
    mapping(address => mapping(uint256 => ActiveBid)) private currentBids;

    // Number of blocks to begin refreshing auction lengths
    uint256 public auctionLengthExtension;

    // Max Length that an auction can be
    uint256 public maxLength;
    /////////////////////////////////////////////////////////////////////////
    // Events
    /////////////////////////////////////////////////////////////////////////
    event NewColdieAuction(
        address indexed _contractAddress,
        uint256 indexed _tokenId,
        address indexed _auctionCreator,
        uint256 _reservePrice,
        uint256 _lengthOfAuction
    );

    event CancelAuction(
        address indexed _contractAddress,
        uint256 indexed _tokenId,
        address indexed _auctionCreator
    );

    event AuctionCancelBid(
        address indexed _bidder,
        address indexed _contractAddress,
        uint256 indexed _tokenId,
        uint256 _amount
    );

    event ColdieAuctionBegun(
        address indexed _bidder,
        address indexed _contractAddress,
        uint256 indexed _tokenId,
        uint256 _initialBidAmount,
        uint256 _startingBlock
    );

    event NewScheduledAuction(
        address indexed _contractAddress,
        uint256 indexed _tokenId,
        address indexed _auctionCreator,
        uint256 _startingBlock,
        uint256 _minimumBid,
        uint256 _lengthOfAuction
    );

    event AuctionBid(
        address indexed _contractAddress,
        address indexed _bidder,
        uint256 indexed _tokenId,
        uint256 _amount
    );

    event AuctionExtended(
        address indexed _contractAddress,
        uint256 indexed _tokenId,
        uint256 _newAuctionLength
    );

    event AuctionSettled(
        address indexed _contractAddress,
        address indexed _bidder,
        address _seller,
        uint256 indexed _tokenId,
        uint256 _amount
    );

    /////////////////////////////////////////////////////////////////////////
    // Constructor
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Initializes the contract setting the market settings and creator royalty interfaces.
     * @param _iMarketSettings address to set as iMarketSettings.
     * @param _iERC721CreatorRoyalty address to set as iERC721CreatorRoyalty.
     */
    constructor(address _iMarketSettings, address _iERC721CreatorRoyalty)
        public
    {
        maxLength = 43200; // ~ 7 days == 7 days * 24 hours * 3600s / 14s per block
        auctionLengthExtension = 65; // ~ 15 min == 15 min * 60s / 14s per block

        require(
            _iMarketSettings != address(0),
            "constructor::Cannot have null address for _iMarketSettings"
        );

        require(
            _iERC721CreatorRoyalty != address(0),
            "constructor::Cannot have null address for _iERC721CreatorRoyalty"
        );

        // Set iMarketSettings
        iMarketSettings = IMarketplaceSettings(_iMarketSettings);

        // Set iERC721CreatorRoyalty
        iERC721CreatorRoyalty = IERC721CreatorRoyalty(_iERC721CreatorRoyalty);
    }

    /////////////////////////////////////////////////////////////////////////
    // setIMarketplaceSettings
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Admin function to set the marketplace settings.
     * Rules:
     * - only owner
     * - _address != address(0)
     * @param _address address of the IMarketplaceSettings.
     */
    function setMarketplaceSettings(address _address) public onlyOwner {
        iMarketSettings = IMarketplaceSettings(_address);
    }

    /////////////////////////////////////////////////////////////////////////
    // setIERC721CreatorRoyalty
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Admin function to set the IERC721CreatorRoyalty.
     * Rules:
     * - only owner
     * - _address != address(0)
     * @param _address address of the IERC721CreatorRoyalty.
     */
    function setIERC721CreatorRoyalty(address _address) public onlyOwner {
        iERC721CreatorRoyalty = IERC721CreatorRoyalty(_address);
    }

    /////////////////////////////////////////////////////////////////////////
    // setMaxLength
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Admin function to set the maxLength of an auction.
     * Rules:
     * - only owner
     * - _maxLangth > 0
     * @param _maxLength uint256 max length of an auction.
     */
    function setMaxLength(uint256 _maxLength) public onlyOwner {
        maxLength = _maxLength;
    }

    /////////////////////////////////////////////////////////////////////////
    // setAuctionLengthExtension
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Admin function to set the auctionLengthExtension of an auction.
     * Rules:
     * - only owner
     * - _maxLangth > 0
     * @param _auctionLengthExtension uint256 max length of an auction.
     */
    function setAuctionLengthExtension(uint256 _auctionLengthExtension)
        public
        onlyOwner
    {
        auctionLengthExtension = _auctionLengthExtension;
    }

    /////////////////////////////////////////////////////////////////////////
    // createColdieAuction
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev create a reserve auction token contract address, token id, price
     * Rules:
     * - Cannot create an auction if contract isn't approved by owner
     * - lengthOfAuction (in blocks) > 0
     * - lengthOfAuction (in blocks) <= maxLength
     * - Reserve price must be >= 0
     * - Must be owner of the token
     * - Cannot have a current auction going
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     * @param _reservePrice uint256 Wei value of the reserve price.
     * @param _lengthOfAuction uint256 length of auction in blocks.
     */
    function createColdieAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _reservePrice,
        uint256 _lengthOfAuction
    ) public {
        // Rules
        _requireOwnerApproval(_contractAddress, _tokenId);
        _requireOwnerAsSender(_contractAddress, _tokenId);
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
            _reservePrice <= iMarketSettings.getMarketplaceMaxValue(),
            "createColdieAuction::Cannot set reserve price higher than max value"
        );

        // Create the auction
        auctions[_contractAddress][_tokenId] = Auction(
            msg.sender,
            block.number,
            _lengthOfAuction,
            0,
            _reservePrice,
            0,
            COLDIE_AUCTION
        );

        _refundBid(_contractAddress, _tokenId);

        emit NewColdieAuction(
            _contractAddress,
            _tokenId,
            msg.sender,
            _reservePrice,
            _lengthOfAuction
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // cancelAuction
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev cancel an auction
     * Rules:
     * - Must have an auction for the token
     * - Auction cannot have started
     * - Must be the creator of the auction
     * - Must return outstanding bid
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function cancelAuction(address _contractAddress, uint256 _tokenId)
        external
    {
        require(
            auctions[_contractAddress][_tokenId].auctionType != NO_AUCTION,
            "cancelAuction::Must have a current auction"
        );
        require(
            auctions[_contractAddress][_tokenId].startingBlock == 0 ||
                auctions[_contractAddress][_tokenId].startingBlock <
                block.number,
            "cancelAuction::auction cannot be started"
        );
        require(
            auctions[_contractAddress][_tokenId].auctionCreator == msg.sender,
            "cancelAuction::must be the creator of the auction"
        );

        auctions[_contractAddress][_tokenId] = Auction(
            address(0),
            0,
            0,
            0,
            0,
            0,
            NO_AUCTION
        );

        _refundBid(_contractAddress, _tokenId);

        emit CancelAuction(
            _contractAddress,
            _tokenId,
            auctions[_contractAddress][_tokenId].auctionCreator
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // cancelBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Withdraw a bid from an unstarted auction
     * Rules:
     * - Auction cannot have started
     * - Must have the current bid on the token
     * - Must return outstanding bid
     * - Must be the bidder
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function cancelBid(address _contractAddress, uint256 _tokenId) external {
        require(
            auctions[_contractAddress][_tokenId].startingBlock == 0 ||
                auctions[_contractAddress][_tokenId].startingBlock <
                block.number,
            "cancelBid::auction cannot be started"
        );

        require(
            currentBids[_contractAddress][_tokenId].bidder == msg.sender,
            "cancelBid::must be the current bidder"
        );

        ActiveBid memory currentBid = currentBids[_contractAddress][_tokenId];

        _refundBid(_contractAddress, _tokenId);

        emit AuctionCancelBid(
            currentBid.bidder,
            _contractAddress,
            _tokenId,
            currentBid.amount
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // createScheduledAuction
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev create a scheduled auction token contract address, token id
     * Rules:
     * - lengthOfAuction (in blocks) > 0
     * - startingBlock > currentBlock
     * - Cannot create an auction if contract isn't approved by owner
     * - Minimum bid must be >= 0
     * - Must be owner of the token
     * - Cannot have a current auction going for this token
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     * @param _minimumBid uint256 Wei value of the reserve price.
     * @param _lengthOfAuction uint256 length of auction in blocks.
     * @param _startingBlock uint256 block number to start the auction on.
     */
    function createScheduledAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _minimumBid,
        uint256 _lengthOfAuction,
        uint256 _startingBlock
    ) external {
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
            _minimumBid <= iMarketSettings.getMarketplaceMaxValue(),
            "createScheduledAuction::Cannot set minimum bid higher than max value"
        );
        _requireOwnerApproval(_contractAddress, _tokenId);
        _requireOwnerAsSender(_contractAddress, _tokenId);
        require(
            auctions[_contractAddress][_tokenId].auctionType == NO_AUCTION ||
                (msg.sender !=
                    auctions[_contractAddress][_tokenId].auctionCreator),
            "createScheduledAuction::Cannot have a current auction"
        );

        // Create the scheduled auction.
        auctions[_contractAddress][_tokenId] = Auction(
            msg.sender,
            block.number,
            _lengthOfAuction,
            _startingBlock,
            0,
            _minimumBid,
            SCHEDULED_AUCTION
        );

        _refundBid(_contractAddress, _tokenId);

        // Transfer the token to this contract to act as escrow.
        IERC721 erc721 = IERC721(_contractAddress);
        erc721.transferFrom(msg.sender, _contractAddress, _tokenId);

        emit NewScheduledAuction(
            _contractAddress,
            _tokenId,
            msg.sender,
            _startingBlock,
            _minimumBid,
            _lengthOfAuction
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // bid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Bid on artwork with an auction.
     * Rules:
     * - if auction creator is still owner, owner must have contract approved
     * - There must be a running auction or a reserve price auction for the token
     * - bid > 0
     * - if startingBlock - block.number < auctionLengthExtension
     * -    then auctionLength = Starting block - (currentBlock + extension)
     * - Auction creator != bidder
     * - bid >= minimum bid
     * - block.number < startingBlock + lengthOfAuction
     * - bid > current bid
     * - if previous bid then returned
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     * @param _amount uint256 Wei value of the bid.
     */
    function bid(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _amount
    ) external payable {
        // Check that bid is greater than 0.
        require(_amount > 0, "bid::Cannot bid 0 Wei.");

        // Check that bid is less than max value.
        require(
            _amount <= iMarketSettings.getMarketplaceMaxValue(),
            "bid::Cannot bid higher than max value"
        );

        // Check that bid is larger than min value.
        require(
            _amount >= iMarketSettings.getMarketplaceMinValue(),
            "bid::Cannot bid lower than min value"
        );

        // Must have an auction going.
        require(
            auctions[_contractAddress][_tokenId].auctionType != NO_AUCTION,
            "cancelAuction::Must have a current auction"
        );

        // Auction cannot have ended.
        require(
            block.number <
                auctions[_contractAddress][_tokenId].startingBlock.add(
                    auctions[_contractAddress][_tokenId].lengthOfAuction
                ),
            "settleAuction::Can only settle unsettled auctions"
        );

        // Check that enough ether was sent.
        uint256 requiredCost = _amount.add(
            iMarketSettings.calculateMarketplaceFee(_amount)
        );
        require(requiredCost == msg.value, "bid::Must bid the correct amount.");

        // If owner of token is auction creator make sure they have contract approved
        IERC721 erc721 = IERC721(_contractAddress);
        address owner = erc721.ownerOf(_tokenId);

        // Check that token is owned by creator or by this contract
        require(
            auctions[_contractAddress][_tokenId].auctionCreator == owner ||
                owner == address(this),
            "bid::Cannot bid on auction if auction creator is no longer owner."
        );

        if (auctions[_contractAddress][_tokenId].auctionCreator == owner) {
            _requireOwnerApproval(_contractAddress, _tokenId);
        }

        ActiveBid memory currentBid = currentBids[_contractAddress][_tokenId];

        // Must bid higher than current bid.
        require(
            _amount > currentBid.amount,
            "bid::must bid higher than previous bid"
        );

        // Return previous bid
        // We do this hear because it clears the bid for the refund. This makes is safe from reentrence.
        if (currentBid.amount != 0) {
            _refundBid(_contractAddress, _tokenId);
        }

        // Set the new bid
        currentBids[_contractAddress][_tokenId] = ActiveBid(
            msg.sender,
            iMarketSettings.getMarketplaceFeePercentage(),
            _amount
        );

        emit AuctionBid(_contractAddress, msg.sender, _tokenId, _amount);

        // if the reserve price is met, then the auction has begun.
        if (
            auctions[_contractAddress][_tokenId].startingBlock == 0 ||
            _amount >= auctions[_contractAddress][_tokenId].reservePrice
        ) {
            auctions[_contractAddress][_tokenId].startingBlock = block.number;
            emit ColdieAuctionBegun(
                msg.sender,
                _contractAddress,
                _tokenId,
                _amount,
                block.number
            );
        }

        // If the time left for the auction is less than the extension limit bump the length of the auction.
        if (
            block.number - auctions[_contractAddress][_tokenId].startingBlock <
            auctionLengthExtension
        ) {
            auctions[_contractAddress][_tokenId].lengthOfAuction =
                (block.number + auctionLengthExtension) -
                auctions[_contractAddress][_tokenId].startingBlock;
            emit AuctionExtended(
                _contractAddress,
                _tokenId,
                auctions[_contractAddress][_tokenId].lengthOfAuction
            );
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // settleAuction
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Settles the auction, transferring the auctioned token to the bidder and the bid to auction creator.
     * Rules:
     * - There must be an unsettled auction for the token
     * - current bidder becomes new owner
     * - auction creator gets paid
     * - there is no longer an auction for the token
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function settleAuction(address _contractAddress, uint256 _tokenId)
        external
    {
        Auction memory auction = auctions[_contractAddress][_tokenId];

        require(
            auction.auctionType != NO_AUCTION && auction.startingBlock != 0,
            "settleAuction::Must have a current auction that has started"
        );
        require(
            block.number > auction.startingBlock.add(auction.lengthOfAuction),
            "settleAuction::Can only settle unsettled auctions"
        );

        ActiveBid memory currentBid = currentBids[_contractAddress][_tokenId];

        currentBids[_contractAddress][_tokenId] = ActiveBid(address(0), 0, 0);
        auctions[_contractAddress][_tokenId] = Auction(
            address(0),
            0,
            0,
            0,
            0,
            0,
            NO_AUCTION
        );
        IERC721 erc721 = IERC721(_contractAddress);

        // If there were no bids then end the auction and return the token to its original owner.
        if (currentBid.bidder == address(0)) {
            // Transfer the token to back to original owner.
            erc721.transferFrom(
                _contractAddress,
                auction.auctionCreator,
                _tokenId
            );
            emit AuctionSettled(
                _contractAddress,
                address(0),
                auction.auctionCreator,
                _tokenId,
                0
            );
            return;
        }

        // Transfer the token to the winner of the auction.
        erc721.transferFrom(_contractAddress, currentBid.bidder, _tokenId);

        iMarketSettings.markERC721Token(_contractAddress, _tokenId, true);
        address payable owner = _makePayable(owner());
        Payments.payout(
            currentBid.amount,
            iMarketSettings.hasERC721TokenSold(_contractAddress, _tokenId),
            currentBid.marketplaceFee,
            iERC721CreatorRoyalty.getERC721TokenRoyaltyPercentage(
                _contractAddress,
                _tokenId
            ),
            iMarketSettings.getERC721ContractPrimarySaleFeePercentage(
                _contractAddress
            ),
            auction.auctionCreator,
            owner,
            iERC721CreatorRoyalty.tokenCreator(_contractAddress, _tokenId),
            owner
        );
        emit AuctionSettled(
            _contractAddress,
            currentBid.bidder,
            auction.auctionCreator,
            _tokenId,
            currentBid.amount
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // getAuctionDetails
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Get current auction details for a token
     * Rules:
     * - Return empty when there's no auction
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function getAuctionDetails(address _contractAddress, uint256 _tokenId)
        external
        view
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

    /////////////////////////////////////////////////////////////////////////
    // getCurrentBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Get the current bid
     * Rules:
     * - Return empty when there's no bid
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function getCurrentBid(address _contractAddress, uint256 _tokenId)
        external
        view
        returns (address, uint256)
    {
        return (
            currentBids[_contractAddress][_tokenId].bidder,
            currentBids[_contractAddress][_tokenId].amount
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // _requireOwnerApproval
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Require that the owner have the SuperRareAuctionHouse approved.
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function _requireOwnerApproval(address _contractAddress, uint256 _tokenId)
        internal
        view
    {
        IERC721 erc721 = IERC721(_contractAddress);
        address owner = erc721.ownerOf(_tokenId);
        require(
            erc721.isApprovedForAll(owner, address(this)),
            "owner must have approved contract"
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // _requireOwnerAsSender
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Require that the owner be the sender.
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function _requireOwnerAsSender(address _contractAddress, uint256 _tokenId)
        internal
        view
    {
        IERC721 erc721 = IERC721(_contractAddress);
        address owner = erc721.ownerOf(_tokenId);
        require(owner == msg.sender, "owner must have approved contract");
    }

    /////////////////////////////////////////////////////////////////////////
    // _refundBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to return an existing bid on a token to the
     *      bidder and reset bid.
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _refundBid(address _contractAddress, uint256 _tokenId) internal {
        ActiveBid memory currentBid = currentBids[_contractAddress][_tokenId];
        if (currentBid.bidder == address(0)) {
            return;
        }

        currentBids[_contractAddress][_tokenId] = ActiveBid(address(0), 0, 0);

        // refund the bidder
        Payments.refund(
            currentBid.marketplaceFee,
            currentBid.bidder,
            currentBid.amount
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // _makePayable
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to set a bid.
     * @param _address non-payable address
     * @return payable address
     */
    function _makePayable(address _address)
        internal
        pure
        returns (address payable)
    {
        return address(uint160(_address));
    }
}
