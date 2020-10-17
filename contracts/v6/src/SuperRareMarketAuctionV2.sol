pragma solidity 0.6.12;

import "openzeppelin-solidity-solc6/contracts/token/ERC721/IERC721.sol";
import "openzeppelin-solidity-solc6/contracts/math/SafeMath.sol";
import "openzeppelin-solidity-solc6/contracts/access/Ownable.sol";
import "./IERC721CreatorRoyalty.sol";
import "./IMarketplaceSettings.sol";
import "./Payments.sol";

contract SuperRareMarketAuctionV2 is Ownable, Payments {
    using SafeMath for uint256;

    /////////////////////////////////////////////////////////////////////////
    // Structs
    /////////////////////////////////////////////////////////////////////////

    // The active bid for a given token, contains the bidder, the marketplace fee at the time of the bid, and the amount of wei placed on the token
    struct ActiveBid {
        address payable bidder;
        uint256 marketplaceFee;
        uint256 amount;
    }

    // The sale price for a given token containing the seller and the amount of wei to be sold for
    struct SalePrice {
        address payable seller;
        uint256 amount;
    }

    /////////////////////////////////////////////////////////////////////////
    // State Variables
    /////////////////////////////////////////////////////////////////////////

    // Marketplace Settings Interface
    IMarketplaceSettings public iMarketSettings;

    // Creator Royalty Interface
    IERC721CreatorRoyalty public iERC721CreatorRoyalty;

    // Mapping from ERC721 contract to mapping of tokenId to sale price.
    mapping(address => mapping(uint256 => SalePrice)) private tokenPrices;

    // Mapping of ERC721 contract to mapping of token ID to whether the token has been sold before.
    mapping(address => mapping(uint256 => bool)) private tokenSolds;

    // Mapping of ERC721 contract to mapping of token ID to the current bid amount.
    mapping(address => mapping(uint256 => ActiveBid)) private tokenCurrentBids;

    /////////////////////////////////////////////////////////////////////////////
    // Events
    /////////////////////////////////////////////////////////////////////////////
    event Sold(
        address indexed _originContract,
        address indexed _buyer,
        address indexed _seller,
        uint256 _amount,
        uint256 _tokenId
    );

    event SetSalePrice(
        address indexed _originContract,
        uint256 _amount,
        uint256 _tokenId
    );

    event Bid(
        address indexed _originContract,
        address indexed _bidder,
        uint256 _amount,
        uint256 _tokenId
    );

    event AcceptBid(
        address indexed _originContract,
        address indexed _bidder,
        address indexed _seller,
        uint256 _amount,
        uint256 _tokenId
    );

    event CancelBid(
        address indexed _originContract,
        address indexed _bidder,
        uint256 _amount,
        uint256 _tokenId
    );

    /////////////////////////////////////////////////////////////////////////
    // Modifiers (as functions)
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Checks that the token owner is approved for the ERC721Market
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    function ownerMustHaveMarketplaceApproved(
        address _originContract,
        uint256 _tokenId
    ) internal view {
        IERC721 erc721 = IERC721(_originContract);
        address owner = erc721.ownerOf(_tokenId);
        require(
            erc721.isApprovedForAll(owner, address(this)),
            "owner must have approved contract"
        );
    }

    /**
     * @dev Checks that the token is owned by the sender
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    function senderMustBeTokenOwner(address _originContract, uint256 _tokenId)
        internal
        view
    {
        IERC721 erc721 = IERC721(_originContract);
        require(
            erc721.ownerOf(_tokenId) == msg.sender,
            "sender must be the token owner"
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // setSalePrice
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Set the token for sale. The owner of the token must be the sender and have the marketplace approved.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @param _amount uint256 wei value that the item is for sale
     */
    function setSalePrice(
        address _originContract,
        uint256 _tokenId,
        uint256 _amount
    ) external {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId);

        // The sender must be the token owner
        senderMustBeTokenOwner(_originContract, _tokenId);

        if (_amount == 0) {
            // Set not for sale and exit
            _resetTokenPrice(_originContract, _tokenId);
            emit SetSalePrice(_originContract, _amount, _tokenId);
            return;
        }

        tokenPrices[_originContract][_tokenId] = SalePrice(msg.sender, _amount);
        emit SetSalePrice(_originContract, _amount, _tokenId);
    }

    /////////////////////////////////////////////////////////////////////////
    // safeBuy
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Purchase the token with the expected amount. The current token owner must have the marketplace approved.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @param _amount uint256 wei amount expecting to purchase the token for.
     */
    function safeBuy(
        address _originContract,
        uint256 _tokenId,
        uint256 _amount
    ) external payable {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId);

        // Make sure the tokenPrice is the expected amount
        require(
            tokenPrices[_originContract][_tokenId].amount == _amount,
            "safeBuy::Purchase amount must equal expected amount"
        );
        buy(_originContract, _tokenId);
    }

    /////////////////////////////////////////////////////////////////////////
    // buy
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Purchases the token if it is for sale.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token.
     */
    function buy(address _originContract, uint256 _tokenId) public payable {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId);

        // Check that the person who set the price still owns the token.
        require(
            _priceSetterStillOwnsTheToken(_originContract, _tokenId),
            "buy::Current token owner must be the person to have the latest price."
        );

        SalePrice memory sp = tokenPrices[_originContract][_tokenId];

        // Check that token is for sale.
        require(sp.amount > 0, "buy::Tokens priced at 0 are not for sale.");

        // Check that enough ether was sent.
        uint256 requiredCost = tokenPriceFeeIncluded(_originContract, _tokenId);
        require(
            requiredCost == msg.value,
            "buy::Must purchase the token for the correct price"
        );

        // Get token contract details.
        IERC721 erc721 = IERC721(_originContract);
        address tokenOwner = erc721.ownerOf(_tokenId);

        // Transfer token.
        erc721.safeTransferFrom(tokenOwner, msg.sender, _tokenId);

        // Wipe the token price.
        _resetTokenPrice(_originContract, _tokenId);

        // if the buyer had an existing bid, return it
        if (_addressHasBidOnToken(msg.sender, _originContract, _tokenId)) {
            _refundBid(_originContract, _tokenId);
        }

        // set the token as sold
        _setTokenAsSold(_originContract, _tokenId);

        // Payout all parties.
        iMarketSettings.markERC721Token(_originContract, _tokenId, true);
        address payable owner = _makePayable(owner());
        Payments.payout(
            sp.amount,
            iMarketSettings.hasERC721TokenSold(_originContract, _tokenId),
            iMarketSettings.marketplaceFeePercentage(),
            iERC721CreatorRoyalty.getERC721TokenRoyaltyPercentage(
                _originContract,
                _tokenId
            ),
            iMarketSettings.getERC721ContractPrimarySaleFeePercentage(
                _originContract
            ),
            _makePayable(tokenOwner),
            owner,
            iERC721CreatorRoyalty.tokenCreator(_originContract, _tokenId),
            owner
        );

        emit Sold(_originContract, msg.sender, tokenOwner, sp.amount, _tokenId);
    }

    /////////////////////////////////////////////////////////////////////////
    // tokenPrice
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Gets the sale price of the token
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @return uint256 sale price of the token
     */
    function tokenPrice(address _originContract, uint256 _tokenId)
        external
        view
        returns (uint256)
    {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId); // TODO: Make sure to write test to verify that this returns 0 when it fails

        if (_priceSetterStillOwnsTheToken(_originContract, _tokenId)) {
            return tokenPrices[_originContract][_tokenId].amount;
        }
        return 0;
    }

    /////////////////////////////////////////////////////////////////////////
    // tokenPriceFeeIncluded
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Gets the sale price of the token including the marketplace fee.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @return uint256 sale price of the token including the fee.
     */
    function tokenPriceFeeIncluded(address _originContract, uint256 _tokenId)
        public
        view
        returns (uint256)
    {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId); // TODO: Make sure to write test to verify that this returns 0 when it fails

        if (_priceSetterStillOwnsTheToken(_originContract, _tokenId)) {
            return
                tokenPrices[_originContract][_tokenId].amount.add(
                    Payments.calculateMarketplaceFee(
                        tokenPrices[_originContract][_tokenId].amount
                    )
                );
        }
        return 0;
    }

    /////////////////////////////////////////////////////////////////////////
    // bid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Bids on the token, replacing the bid if the bid is higher than the current bid. You cannot bid on a token you already own.
     * @param _newBidAmount uint256 value in wei to bid.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    function bid(
        uint256 _newBidAmount,
        address _originContract,
        uint256 _tokenId
    ) external payable {
        // Check that bid is greater than 0.
        require(_newBidAmount > 0, "bid::Cannot bid 0 Wei.");

        // Check that bid is higher than previous bid
        uint256 currentBidAmount = tokenCurrentBids[_originContract][_tokenId]
            .amount;
        require(
            _newBidAmount > currentBidAmount,
            "bid::Must place higher bid than existing bid."
        );

        // Check that enough ether was sent.
        uint256 requiredCost = _newBidAmount.add(
            _calcMarketplaceFee(_newBidAmount, marketplaceFee)
        );
        require(
            requiredCost == msg.value,
            "bid::Must purchase the token for the correct price."
        );

        // Check that bidder is not owner.
        IERC721 erc721 = IERC721(_originContract);
        address tokenOwner = erc721.ownerOf(_tokenId);
        require(tokenOwner != msg.sender, "bid::Bidder cannot be owner.");

        // Refund previous bidder.
        _refundBid(_originContract, _tokenId);

        // Set the new bid.
        _setBid(_newBidAmount, msg.sender, _originContract, _tokenId);

        emit Bid(_originContract, msg.sender, _newBidAmount, _tokenId);
    }

    /////////////////////////////////////////////////////////////////////////
    // safeAcceptBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Accept the bid on the token with the expected bid amount.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @param _amount uint256 wei amount of the bid
     */
    function safeAcceptBid(
        address _originContract,
        uint256 _tokenId,
        uint256 _amount
    ) external {
        (uint256 bidAmount, address _) = currentBidDetailsOfToken(
            _originContract,
            _tokenId
        );

        // Make sure accepting bid is the expected amount
        require(bidAmount == _amount, "Bid amount must equal expected amount");
        acceptBid(_originContract, _tokenId);
    }

    /////////////////////////////////////////////////////////////////////////
    // acceptBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Accept the bid on the token.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    function acceptBid(address _originContract, uint256 _tokenId) public {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId);

        // The sender must be the token owner
        senderMustBeTokenOwner(_originContract, _tokenId);

        // Check that a bid exists.
        require(
            _tokenHasBid(_originContract, _tokenId),
            "Cannot accept a bid when there is none."
        );

        // Get current bid on token


            ActiveBid memory currentBid
         = tokenCurrentBids[_originContract][_tokenId];

        // Transfer token.
        IERC721 erc721 = IERC721(_originContract);
        erc721.safeTransferFrom(msg.sender, currentBid.bidder, _tokenId);

        // Wipe the token price and bid.
        _resetTokenPrice(_originContract, _tokenId);
        _resetBid(_originContract, _tokenId);

        // set the token as sold
        _setTokenAsSold(_originContract, _tokenId);

        // Payout all parties.
        _payout(
            currentBid.amount,
            currentBid.marketplaceFee,
            msg.sender,
            _originContract,
            _tokenId
        );

        emit AcceptBid(
            _originContract,
            currentBid.bidder,
            msg.sender,
            currentBid.amount,
            _tokenId
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // cancelBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Cancel the bid on the token.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token.
     */
    function cancelBid(address _originContract, uint256 _tokenId) external {
        // Check that sender has a current bid.
        require(
            _addressHasBidOnToken(msg.sender, _originContract, _tokenId),
            "cancelBid::Cannot cancel a bid if sender hasn't made one."
        );

        // Refund the bidder.
        _refundBid(_originContract, _tokenId);

        emit CancelBid(
            _originContract,
            msg.sender,
            tokenCurrentBids[_originContract][_tokenId].amount,
            _tokenId
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // currentBidDetailsOfToken
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to get current bid and bidder of a token.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function currentBidDetailsOfToken(address _originContract, uint256 _tokenId)
        public
        view
        returns (uint256, address)
    {
        return (
            tokenCurrentBids[_originContract][_tokenId].amount,
            tokenCurrentBids[_originContract][_tokenId].bidder
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // hasTokenBeenSold
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to find whether the token has had a primary sale
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function hasTokenBeenSold(address _originContract, uint256 _tokenId)
        external
        view
        returns (bool)
    {
        return tokenSolds[_originContract][_tokenId];
    }

    /////////////////////////////////////////////////////////////////////////
    // setMarketplaceFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to set the marketplace fee percentage.
     * @param _percentage uint256 fee to take from purchases 1 == 1%.
     */
    function setMarketplaceFee(uint256 _percentage) external onlyOwner {
        require(
            _percentage <= maximumPercentage,
            "Marketpalce fee cannot be greater than max percentage"
        );
        marketplaceFee = _percentage;
        emit MarketplaceFeeSet(_percentage);
    }

    /////////////////////////////////////////////////////////////////////////
    // setERC721ContractRoyaltySettings
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to set the royalty fee percentage.
     * @param _originContract address of the ERC721 tokens
     * @param _percentage uint256 royalty fee to take split between seller and creator 10 = 10%.
     */
    function setERC721ContractRoyaltySettings(
        address _originContract,
        address _erc721CreatorContract,
        uint256 _percentage
    ) external onlyOwner {
        require(
            _percentage <= maximumPercentage,
            "Royalty cannot be greater than max percentage"
        );
        originContractRoyaltySettings[_originContract] = RoyaltySettings(
            IERC721Creator(_erc721CreatorContract),
            _percentage
        );
        emit RoyaltySettingsSet(
            _originContract,
            _erc721CreatorContract,
            _percentage
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // getERC721ContractRoyaltySettings
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to get the royalty fee percentage for an origin contract.
     * @param _originContract address of the ERC721 tokens
     */
    function getERC721ContractRoyaltySettings(address _originContract)
        external
        view
        returns (address, uint256)
    {
        return (
            address(
                originContractRoyaltySettings[_originContract]
                    .iErc721CreatorContract
            ),
            originContractRoyaltySettings[_originContract].percentage
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // setERC721ContractPrimarySaleFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to set the primary sale fee percentage.
     * @param _originContract address of the ERC721 tokens
     * @param _percentage uint256 fee to take from purchases, 1 == 1%.
     */
    function setERC721ContractPrimarySaleFee(
        address _originContract,
        uint256 _percentage
    ) external onlyOwner {
        require(
            _percentage <= maximumPercentage,
            "Primary sale fee cannot be greater than max percentage"
        );
        originContractPrimarySaleFee[_originContract] = _percentage;
    }

    /////////////////////////////////////////////////////////////////////////
    // getERC721ContractPrimarySaleFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to get the primary sale fee percentage for an origin contract.
     * @param _originContract address of the ERC721 tokens
     */
    function getERC721ContractPrimarySaleFee(address _originContract)
        external
        view
        returns (uint256)
    {
        return originContractPrimarySaleFee[_originContract];
    }

    /////////////////////////////////////////////////////////////////////////
    // markTokensAsSold
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to set an array of tokens for a contract as sold, thus not being subject to the primary sale fee, if one exists.
     * @param _originContract address of ERC721 contract.
     * @param _tokenIds uint256[] array of token ids.
     */
    function markTokensAsSold(
        address _originContract,
        uint256[] calldata _tokenIds
    ) external onlyOwner {
        // limit to batches of 2000
        require(
            _tokenIds.length <= 2000,
            "markTokensAsSold::Attempted to mark more than 2000 tokens as sold"
        );

        // Mark provided tokens as sold.
        for (uint256 i = 0; i < _tokenIds.length; i++) {
            tokenSolds[_originContract][_tokenIds[i]] = true;
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // _priceSetterStillOwnsTheToken
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Checks that the token is owned by the same person who set the sale price.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 id of the.
     */
    function _priceSetterStillOwnsTheToken(
        address _originContract,
        uint256 _tokenId
    ) internal view returns (bool) {
        IERC721 erc721 = IERC721(_originContract);
        return
            erc721.ownerOf(_tokenId) ==
            tokenPrices[_originContract][_tokenId].seller;
    }

    /////////////////////////////////////////////////////////////////////////
    // _payout
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to pay the seller, creator, and maintainer.
     * @param _amount uint256 value to be split.
     * @param _marketplacePercentage uint256 percentage of the fee for the marketplace.
     * @param _seller address seller of the token.
     * @param _originContract address of the token contract.
     * @param _tokenId uint256 ID of the token.
     */
    function _payout(
        uint256 _amount,
        uint256 _marketplacePercentage,
        address payable _seller,
        address _originContract,
        uint256 _tokenId
    ) private {
        address maintainer = this.owner();

        bool isPrimarySale = !tokenSolds[_originContract][_tokenId] &&
            originContractPrimarySaleFee[_originContract] > 0;
        uint256 marketplacePayment = _calcMarketplacePayment(
            isPrimarySale,
            _amount,
            _marketplacePercentage,
            _originContract
        );
        uint256 sellerPayment = _calcSellerPayment(
            isPrimarySale,
            _amount,
            _originContract,
            _tokenId
        );
        (
            uint256 royaltyPayment,
            address creator
        ) = _calcRoyaltyPaymentAndGetCreator(
            isPrimarySale,
            _amount,
            _originContract,
            _tokenId
        );

        if (marketplacePayment > 0) {
            sendValueOrEscrow(_makePayable(maintainer), marketplacePayment);
        }
        if (sellerPayment > 0) {
            sendValueOrEscrow(_makePayable(_seller), sellerPayment);
        }
        if (royaltyPayment > 0) {
            sendValueOrEscrow(_makePayable(creator), royaltyPayment);
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // _calcMarketplacePayment
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to calculate Marketplace fees.
     *      If primary sale:  fee + split with seller
            otherwise:        just fee.
     * @param _hasPrimarySaleFee bool of whether there's a primary sale fee
     * @param _amount uint256 value to be split
     * @param _marketplacePercentage uint256 marketplace fee percentage
     * @param _originContract address of the token contract
     * @return uint256 wei value owed the marketplace owner
     */
    function _calcMarketplacePayment(
        bool _hasPrimarySaleFee,
        uint256 _amount,
        uint256 _marketplacePercentage,
        address _originContract
    ) internal view returns (uint256) {
        uint256 marketplaceFeePayment = _calcMarketplaceFee(
            _amount,
            _marketplacePercentage
        );
        if (_hasPrimarySaleFee) {
            uint256 primarySalePayment = _amount
                .mul(originContractPrimarySaleFee[_originContract])
                .div(100);
            return marketplaceFeePayment.add(primarySalePayment);
        }
        return marketplaceFeePayment;
    }

    /////////////////////////////////////////////////////////////////////////
    // _calcRoyaltyPaymentAndGetCreator
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to calculate royalty payment and get the creator.
     *      If primary sale: 0, null address
     *      If no royalty percentage for the contract: 0, null address
     *      otherwise: royalty in wei, creator address
     * @param _hasPrimarySaleFee bool of whether there's a primary sale fee
     * @param _amount uint256 value to be split
     * @param _originContract address of the token contract
     * @param _tokenId id of the token
     * @return (uint256 wei value of royalty, address of token creator)
     */
    function _calcRoyaltyPaymentAndGetCreator(
        bool _hasPrimarySaleFee,
        uint256 _amount,
        address _originContract,
        uint256 _tokenId
    ) internal view returns (uint256, address) {
        if (_hasPrimarySaleFee) {
            return (0, address(0));
        }


            RoyaltySettings memory rs
         = originContractRoyaltySettings[_originContract];
        if (rs.percentage == 0) {
            return (0, address(0));
        }
        return (
            _amount.mul(rs.percentage).div(100),
            rs.iErc721CreatorContract.tokenCreator(_tokenId)
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // _calcSellerPayment
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to calculate seller payment.
     *      If primary sale: _amount - split with marketplace,
     *      otherwise:       _amount - creator royalty.
     * @param _hasPrimarySaleFee bool of whether there's a primary sale fee
     * @param _amount uint256 value to be split
     * @param _originContract address of the token contract
     * @param _tokenId id of the token
     */
    function _calcSellerPayment(
        bool _hasPrimarySaleFee,
        uint256 _amount,
        address _originContract,
        uint256 _tokenId
    ) internal view returns (uint256) {
        if (_hasPrimarySaleFee) {
            uint256 primarySalePayment = _amount
                .mul(originContractPrimarySaleFee[_originContract])
                .div(100);
            return _amount.sub(primarySalePayment);
        }
        (uint256 royaltyPayment, address _) = _calcRoyaltyPaymentAndGetCreator(
            _hasPrimarySaleFee,
            _amount,
            _originContract,
            _tokenId
        );
        return _amount.sub(royaltyPayment);
    }

    /////////////////////////////////////////////////////////////////////////
    // _calcMarketplaceFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function calculate marketplace fee for a given amount.
     *      f(_amount, _fee) =  _amount * (_fee / 100)
     * @param _amount uint256 value to be split.
     * @param _marketplacePercentage uint256 marketplace percentage.
     * @return uint256 marketplace fee.
     */
    function _calcMarketplaceFee(
        uint256 _amount,
        uint256 _marketplacePercentage
    ) internal pure returns (uint256) {
        return _amount.mul(_marketplacePercentage).div(100);
    }

    /////////////////////////////////////////////////////////////////////////
    // _setTokenAsSold
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to set a token as sold.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _setTokenAsSold(address _originContract, uint256 _tokenId)
        internal
    {
        if (tokenSolds[_originContract][_tokenId]) {
            return;
        }
        tokenSolds[_originContract][_tokenId] = true;
    }

    /////////////////////////////////////////////////////////////////////////
    // _resetTokenPrice
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to set token price to 0 for a given contract.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _resetTokenPrice(address _originContract, uint256 _tokenId)
        internal
    {
        tokenPrices[_originContract][_tokenId] = SalePrice(address(0), 0);
    }

    /////////////////////////////////////////////////////////////////////////
    // _addressHasBidOnToken
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function see if the given address has an existing bid on a token.
     * @param _bidder address that may have a current bid.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _addressHasBidOnToken(
        address _bidder,
        address _originContract,
        uint256 _tokenId
    ) internal view returns (bool) {
        return tokenCurrentBids[_originContract][_tokenId].bidder == _bidder;
    }

    /////////////////////////////////////////////////////////////////////////
    // _tokenHasBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function see if the token has an existing bid.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _tokenHasBid(address _originContract, uint256 _tokenId)
        internal
        view
        returns (bool)
    {
        return tokenCurrentBids[_originContract][_tokenId].bidder != address(0);
    }

    /////////////////////////////////////////////////////////////////////////
    // _refundBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to return an existing bid on a token to the
     *      bidder and reset bid.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _refundBid(address _originContract, uint256 _tokenId) internal {

            ActiveBid memory currentBid
         = tokenCurrentBids[_originContract][_tokenId];
        if (currentBid.bidder == address(0)) {
            return;
        }
        _resetBid(_originContract, _tokenId);
        Payments.refund(
            currentBid.marketplaceFee,
            currentBid.bidder,
            currentBid.amount
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // _resetBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to reset bid by setting bidder and bid to 0.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _resetBid(address _originContract, uint256 _tokenId) internal {
        tokenCurrentBids[_originContract][_tokenId] = ActiveBid(
            address(0),
            0,
            0
        );
    }

    /////////////////////////////////////////////////////////////////////////
    // _setBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to set a bid.
     * @param _amount uint256 value in wei to bid. Does not include marketplace fee.
     * @param _bidder address of the bidder.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _setBid(
        uint256 _amount,
        address payable _bidder,
        address _originContract,
        uint256 _tokenId
    ) internal {
        // Check bidder not 0 address.
        require(_bidder != address(0), "Bidder cannot be 0 address.");

        // Set bid.
        tokenCurrentBids[_originContract][_tokenId] = ActiveBid(
            _bidder,
            marketplaceFee,
            _amount
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
