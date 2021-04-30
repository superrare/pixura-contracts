pragma solidity 0.6.12;

interface ISuperRareAuctionHouseV2 {
    // ~ Storage ~
    // - variables defined here will be a combo
    //   of the auction house and marketplace data structures used
    // - same schema will need to be used for both the Auction House and
    //   Marketplace logic contracts

	// Events
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

    event NewScheduledAuction(
        address indexed _contractAddress,
        uint256 indexed _tokenId,
        address indexed _auctionCreator,
        uint256 _startingBlock,
        uint256 _minimumBid,
        uint256 _lengthOfAuction
    );

		event AuctionStarted(
        address indexed _contractAddress,
        address indexed _bidder,
        uint256 indexed _tokenId,
        uint256 _amount,
        uint256 _auctionLength
    );

    event AuctionBid(
        address indexed _contractAddress,
        address indexed _bidder,
        uint256 indexed _tokenId,
        uint256 _amount,
        bool _startedAuction,
        uint256 _newAuctionLength,
        address _previousBidder
    );

    event AuctionSettled(
        address indexed _contractAddress,
        address indexed _bidder,
        address _seller,
        uint256 indexed _tokenId,
        uint256 _amount
    );

    // Auction House Functionality
    function createColdieAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _reservePrice,
        uint256 _lengthOfAuction
    ) external;

    function createScheduledAuction(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _minimumBid,
        uint256 _lengthOfAuction,
        uint256 _startingBlock
    ) external;

    function cancelAuction(address _contractAddress, uint256 _tokenId) external;

    function bid(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _amount
    ) external payable;

    function settleAuction(address _contractAddress, uint256 _tokenId) external;

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
        );

    function getCurrentBid(address _contractAddress, uint256 _tokenId)
        external
        view
        returns (address, uint256);
}
