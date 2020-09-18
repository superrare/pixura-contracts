pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/token/ERC721/IERC721.sol";
import "./IERC721Creator.sol";
import "./SendValueOrEscrow.sol";
import "openzeppelin-solidity/contracts/math/SafeMath.sol";
import "openzeppelin-solidity/contracts/ownership/Ownable.sol";

contract SuperRareMarketAuctionV2 is Ownable, SendValueOrEscrow {
    using SafeMath for uint256;

    /////////////////////////////////////////////////////////////////////////
    // Structs
    /////////////////////////////////////////////////////////////////////////
    struct Bid {
        address payable bidder;
        uint256 bidMarketplaceFee;
        uint256 amount;
    }

    struct SalePrice {
        address payable seller;
        uint256 amount;
    }

    /////////////////////////////////////////////////////////////////////////
    // State Variables
    /////////////////////////////////////////////////////////////////////////
    // Mapping from ERC721 contract to mapping of tokenId to sale price.

    // What happens when we clear a sale price?
    mapping(address => mapping(uint256 => SalePrice)) private tokenPrices;

    // Mapping of ERC721 contract to mapping of token ID to whether the token has been sold before.
    mapping(address => mapping(uint256 => bool)) private tokenSolds;

    // Mapping of ERC721 contract to mapping of token ID to the current bid amount.
    mapping(address => mapping(uint256 => Bid)) private tokenCurrentBids;

    // Mapping of ERC721 contract to creator royalty fee. If royalty is 0 for an origin contract then royalty is ignored.
    mapping(address => uint256) private originContractRoyalty;

    // Mapping of ERC721 contract to the primary sale fee. If primary sale fee is 0 for an origin contract then primary sale fee is ignored.
    mapping(address => uint256) private originContractPrimarySaleFee;

    // The maximum primary sale fee.
    uint256 constant maximumPercentage = 100; // 100 %

    // Marketplace fee paid to the owner of the contract.
    uint256 public marketplaceFee = 3; // 3 %

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
    ) internal {
        IERC721 erc721 = IERC721(_originContract);
        address owner = erc721.ownerOf(_tokenId);
        require(
            erc721.isApprovedForAll(owner, address(this)),
            "owner must have approved contract"
        );
    }

    // User Diego -> send a setSalePrice Tx -> marketplace contract
    // tx.sender == User Diego's address
    // msg.sender == User Diego's address

    // User Diego -> send a purchases Tx (msg A) -> marketplace contract -> send a transfer msg (msg B) -> origin token contract thus transferring the token
    // tx.sender == A msg.sender (DIegos addretss)
    // tx.sender != B msg.sender (Marketplace address)

    /**
     * @dev Checks that the token is owned by the sender
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    function senderMustBeTokenOwner(address _originContract, uint256 _tokenId)
        internal
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
    ) public {
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
    ) public payable {
        // The owner of the token must have the marketplace approved
        ownerMustHaveMarketplaceApproved(_originContract, _tokenId);

        SalePrice tokenPrice = tokenPrices[_originContract][_tokenId];

        // Make sure the tokenPrice is the expected amount
        require(
            tokenPrice.amount == _amount,
            "Purchase amount must equal expected amount"
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
            "Current token owner must be the person to have the latest price."
        );

        // Check that token is for sale.
        SalePrice tokenPrice = tokenPrices[_originContract][_tokenId];
        require(tokenPrice.amount > 0, "Tokens priced at 0 are not for sale.");

        // Check that enough ether was sent.
        uint256 requiredCost = tokenPriceFeeIncluded(_originContract, _tokenId);
        require(
            requiredCost == msg.value,
            "Must purchase the token for the correct price"
        );

        // Get token contract details.
        IERC721 erc721 = IERC721(_originContract);
        address tokenOwner = erc721.ownerOf(_tokenId);

        // Payout all parties.
        _payout(
            tokenPrice,
            _makePayable(tokenOwner),
            _originContract,
            _tokenId
        );

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

        emit Sold(
            _originContract,
            msg.sender,
            tokenOwner,
            tokenPrice,
            _tokenId
        );
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
        public
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
                    _calcMarketplaceFee(
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
    ) public payable {
        // Check that bid is greater than 0.
        require(_newBidAmount > 0, "Cannot bid 0 Wei.");

        // Check that bid is higher than previous bid
        uint256 currentBidAmount = tokenCurrentBids[_originContract][_tokenId]
            .amount;
        require(
            _newBidAmount > currentBidAmount,
            "Must place higher bid than existing bid."
        );

        // Check that enough ether was sent.
        uint256 requiredCost = _newBidAmount.add(
            _calcMarketplaceFee(_newBidAmount)
        );
        require(
            requiredCost == msg.value,
            "Must purchase the token for the correct price."
        );

        // Check that bidder is not owner.
        IERC721 erc721 = IERC721(_originContract);
        address tokenOwner = erc721.ownerOf(_tokenId);
        address bidder = msg.sender;
        require(tokenOwner != bidder, "Bidder cannot be owner.");

        // Refund previous bidder.
        _refundBid(_originContract, _tokenId);

        // Set the new bid.
        _setBid(_newBidAmount, bidder, _originContract, _tokenId);

        emit Bid(_originContract, bidder, _newBidAmount, _tokenId);
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
    ) public {
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

        // Payout all parties.
        (uint256 bidAmount, address bidder) = currentBidDetailsOfToken(
            _originContract,
            _tokenId
        );
        _payout(bidAmount, msg.sender, _originContract, _tokenId);

        // Transfer token.
        IERC721 erc721 = IERC721(_originContract);
        erc721.safeTransferFrom(msg.sender, bidder, _tokenId);

        // Wipe the token price and bid.
        _resetTokenPrice(_originContract, _tokenId);
        _resetBid(_originContract, _tokenId);

        // set the token as sold
        _setTokenAsSold(_originContract, _tokenId);

        emit AcceptBid(
            _originContract,
            bidder,
            msg.sender,
            bidAmount,
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
    function cancelBid(address _originContract, uint256 _tokenId) public {
        // Check that sender has a current bid.
        require(
            _addressHasBidOnToken(msg.sender, _originContract, _tokenId),
            "Cannot cancel a bid if sender hasn't made one."
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
        public
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
    function setMarketplaceFee(uint256 _percentage) public onlyOwner {
        require(
            _percentage <= maximumPercentage,
            "Marketpalce fee cannot be greater than max percentage"
        );
        marketplaceFee = _percentage;
    }

    /////////////////////////////////////////////////////////////////////////
    // setERC721ContractRoyaltyFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to set the royalty fee percentage.
     * @param _originContract address of the ERC721 tokens
     * @param _percentage uint256 royalty fee to take split between seller and creator 10 = 10%.
     */
    function setERC721ContractRoyaltyFee(
        address _originContract,
        uint256 _percentage
    ) public onlyOwner {
        require(
            _percentage <= maximumPercentage,
            "Royalty cannot be greater than max percentage"
        );
        originContractRoyalty[_originContract] = _percentage;
    }

    /////////////////////////////////////////////////////////////////////////
    // getERC721ContractRoyaltyFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to get the royalty fee percentage for an origin contract.
     * @param _originContract address of the ERC721 tokens
     */
    function getERC721ContractRoyaltyFee(address _originContract)
        public
        view
        returns (uint256)
    {
        return originContractRoyalty[_originContract];
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
    ) public onlyOwner {
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
        public
        view
        returns (uint256)
    {
        return originContractPrimarySaleFee[_originContract];
    }

    /////////////////////////////////////////////////////////////////////////
    // markTokensAsSold
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to set an array of tokens for a contract as sold.
     * @param _originContract address of ERC721 contract.
     * @param _tokenIds uin256[] array of token ids.
     */
    function markTokensAsSold(
        address _originContract,
        uint256[] calldata _tokenIds
    ) external onlyOwner {
        // Mark provided tokens as sold.
        for (uint256 i = 0; i < _tokenIds.length; i++) {
            tokenSolds[_originContract][_tokenIds[i]] = true;
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // doesOriginContractHavePrimarySaleFees
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Function to check if the origin token contract has primary sale fees.
     * @param _originContract address of ERC721 contract.
     */
    function doesOriginContractHavePrimarySaleFees(address _originContract)
        public
        view
        returns (bool)
    {
        return originContractPrimarySaleFee[_originContract];
    }

    /////////////////////////////////////////////////////////////////////////
    // _priceSetterStillOwnsTheToken
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Checks that the token is owned by the same person who set the sale price.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId address of the contract storing the token.
     */
    function _priceSetterStillOwnsTheToken(
        address _originContract,
        uint256 _tokenId
    ) internal view returns (bool) {
        IERC721 erc721 = IERC721(_originContract);
        address owner = erc721.ownerOf(_tokenId);
        address priceSetter = priceSetters[_originContract][_tokenId];
        return owner == priceSetter;
    }

    /////////////////////////////////////////////////////////////////////////
    // _payout
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to pay the seller, creator, and maintainer.
     * @param _amount uint256 value to be split.
     * @param _seller address seller of the token.
     * @param _originContract address of the token contract.
     * @param _tokenId uint256 ID of the token.
     */
    function _payout(
        uint256 _amount,
        address payable _seller,
        address _originContract,
        uint256 _tokenId
    ) private {
        address maintainer = this.owner();
        address creator = IERC721Creator(_originContract).tokenCreator(
            _tokenId
        );

        uint256 marketplacePayment = _calcMarketplacePayment(
            _amount,
            _originContract,
            _tokenId
        );
        uint256 sellerPayment = _calcSellerPayment(
            _amount,
            _originContract,
            _tokenId
        );
        uint256 royaltyPayment = _calcRoyaltyPayment(
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
  * @param _amount uint256 value to be split
  * @param _originContract address of the token contract
  * @param _tokenId id of the token
  */
    function _calcMarketplacePayment(
        uint256 _amount,
        address _originContract,
        uint256 _tokenId
    ) internal view returns (uint256) {
        uint256 marketplaceFeePayment = _calcMarketplaceFee(_amount);
        bool isPrimarySale = !tokenSolds[_originContract][_tokenId];
        if (isPrimarySale) {
            uint256 primarySalePayment = _amount.mul(primarySaleFee).div(100);
            return marketplaceFeePayment + primarySalePayment;
        }
        return marketplaceFeePayment;
    }

    /////////////////////////////////////////////////////////////////////////
    // _calcRoyaltyPayment
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to calculate royalty payment.
     *      If primary sale: 0
     *      otherwise:       artist royalty.
     * @param _amount uint256 value to be split
     * @param _originContract address of the token contract
     * @param _tokenId id of the token
     */
    function _calcRoyaltyPayment(
        uint256 _amount,
        address _originContract,
        uint256 _tokenId
    ) internal view returns (uint256) {
        bool isPrimarySale = doesOriginContractHavePrimarySaleFees(uint256) &&
            !tokenSolds[_originContract][_tokenId];
        if (isPrimarySale) {
            return 0;
        }
        uint256 royaltyFee = getERC721ContractRoyaltyFee(_originContract);
        return _amount.mul(royaltyFee).div(100);
    }

    /////////////////////////////////////////////////////////////////////////
    // _calcSellerPayment
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to calculate seller payment.
     *      If primary sale: _amount - split with marketplace,
     *      otherwise:       _amount - artist royalty.
     * @param _amount uint256 value to be split
     * @param _originContract address of the token contract
     * @param _tokenId id of the token
     */
    function _calcSellerPayment(
        uint256 _amount,
        address _originContract,
        uint256 _tokenId
    ) internal view returns (uint256) {
        bool isPrimarySale = !tokenSolds[_originContract][_tokenId];
        if (isPrimarySale) {
            uint256 primarySalePayment = _amount.mul(primarySaleFee).div(100);
            return _amount - primarySalePayment;
        }
        uint256 royaltyPayment = _calcRoyaltyPayment(
            _amount,
            _originContract,
            _tokenId
        );
        return _amount - royaltyPayment;
    }

    /////////////////////////////////////////////////////////////////////////
    // _calcMarketplaceFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function calculate marketplace fee for a given amount.
     *      f(_amount) =  _amount * (fee % / 100)
     * @param _amount uint256 value to be split.
     */
    function _calcMarketplaceFee(uint256 _amount)
        internal
        view
        returns (uint256)
    {
        return _amount.mul(marketplaceFee).div(100);
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
     * @dev Internal function to set token price to 0 for a give contract.
     * @param _originContract address of ERC721 contract.
     * @param _tokenId uin256 id of the token.
     */
    function _resetTokenPrice(address _originContract, uint256 _tokenId)
        internal
    {
        tokenPrices[_originContract][_tokenId] = 0;
        priceSetters[_originContract][_tokenId] = address(0);
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
        return tokenCurrentBidders[_originContract][_tokenId] == _bidder;
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
        return tokenCurrentBidders[_originContract][_tokenId] != address(0);
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
        address currentBidder = tokenCurrentBidders[_originContract][_tokenId];
        uint256 currentBid = tokenCurrentBids[_originContract][_tokenId];
        uint256 valueToReturn = currentBid + _calcMarketplaceFee(currentBid);
        if (currentBidder == address(0)) {
            return;
        }
        _resetBid(_originContract, _tokenId);
        sendValueOrEscrow(_makePayable(currentBidder), valueToReturn);
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
        tokenCurrentBidders[_originContract][_tokenId] = address(0);
        tokenCurrentBids[_originContract][_tokenId] = 0;
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
        address _bidder,
        address _originContract,
        uint256 _tokenId
    ) internal {
        // Check bidder not 0 address.
        require(_bidder != address(0), "Bidder cannot be 0 address.");

        // Set bid.
        tokenCurrentBidders[_originContract][_tokenId] = _bidder;
        tokenCurrentBids[_originContract][_tokenId] = _amount;
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
