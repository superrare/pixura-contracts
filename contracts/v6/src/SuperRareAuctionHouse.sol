pragma solidity 0.6.12;

contract SuperRareAuctionHouse {
    uint16 constant maxLength = 10000; // TODO: Is this the correct value?

    /////////////////////////////////////////////////////////////////////////
    // createReserveAuction
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev create a reserve auction token contract address, token id, price
     * Rules:
     * - Cannot create an auction if contract isn't approved by owner
     * - lengthOfAuction (in blocks) > 0
     * - Reserve price must be >= 0
     * - Must be owner of the token
     * - Cannot have a current auction going
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     * @param _price uint256 Wei value of the reserve price.
     * @param _lengthOfAuction uint16 length of auction in blocks.
     */
    function createReserveAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _price,
        uint16 _lengthOfAuction
    ) {
        // make sure this contract is approved to move the token
        // if it's approved
        // create a reserve price auction
    }

    /////////////////////////////////////////////////////////////////////////
    // cancelReserveAuction
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev cancel a reserve auction with token id
     * Rules:
     * - Must have reserve auction for the token
     * - Auction cannot have started
     * - Must be the creator of the auction
     * - Must return outstanding bid
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function cancelReserveAuction(address _contractAddress, uint256 _tokenId) {
        // check to see if this person is the one who created the auction
        // if so remove the auction
    }

    /////////////////////////////////////////////////////////////////////////
    // withdrawBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Withdraw a bid
     * Rules:
     * - Auction cannot have started
     * - Must have the current bid on the token
     * - Must return outstanding bid
     * - Must be the bidder
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     */
    function withdrawBid(address _contractAddress, uint256 _tokenId) {}

    /////////////////////////////////////////////////////////////////////////
    // createTimedAuction
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev create a timed auction token contract address, token id
     * Rules:
     * - lengthOfAuction (in blocks) > 0
     * - startingBlock > currentBlock
     * - Cannot create an auction if contract isn't approved by owner
     * - Minimum bid must be >= 0
     * - Must be owner of the token
     * - Cannot have a current auction going for this token
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     * @param _minBid uint256 Wei value of the reserve price.
     * @param _lengthOfAuction uint16 length of auction in blocks.
     * @param _startingBlock uint256 block number to start the auction on.
     */
    function createTimedAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _minBid,
        uint16 _lengthOfAuction,
        uint256 _startingBlock
    ) {
        // Implementation:
        // take custody of token
    }

    /////////////////////////////////////////////////////////////////////////
    // placeBid
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Bid on artwork with price and token id
     * Rules:
     * - There must be a running auction or a reserve price auction for the token
     * - bid > 0
     * - bid > minimum bid
     * - Auction creator != bidder
     * - bid > current bid
     * - if previous bid then returned
     * @param _contractAddress address of ERC721 contract.
     * @param _tokenId uint256 id of the token.
     * @param _bid uint256 Wei value of the reserve price.
     */
    function placeBid(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _bid
    ) {
        // Implementation
        // if reserve auction price met, take custody of token
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
    function settleAuction(address _contractAddress, uint256 _tokenId);

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
    function getAuctionDetails(address _contractAddress, uint256 _tokenId);
}
