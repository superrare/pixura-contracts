pragma solidity 0.6.12;

interface ISuperRareBazaar {
    // ~ Storage ~
    // - variables defined here will be a combo
    //   of the auction house and marketplace data structures used
    // - same schema will need to be used for both the Auction House and
    //   Marketplace logic contracts

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

    // Marketplace Functionality
    function setSalePrice(
        address _originContract,
        uint256 _tokenId,
        uint256 _amount
    ) external;

    function safeBuy(
        address _originContract,
        uint256 _tokenId,
        uint256 _amount
    ) external payable;

    function buy(address _originContract, uint256 _tokenId) external payable;

    /* function tokenPrice(address _originContract, uint256 _tokenId)
        external
        view
        returns (uint256);

    function tokenPriceFeeIncluded(address _originContract, uint256 _tokenId)
        external
        view
        returns (uint256); */

    function offer(
        uint256 _newBidAmount,
        address _originContract,
        uint256 _tokenId
    ) external payable;

    function safeAcceptOffer(
        address _originContract,
				address payable _bidder,
        uint256 _tokenId,
        uint256 _amount
    ) external;

    function acceptOffer(address _originContract, address payable _bidder, uint256 _tokenId) external;

    function cancelOffer(address _originContract, uint256 _tokenId) external;

    function currentBidDetailsOfToken(address _originContract, uint256 _tokenId)
        external
        view
        returns (address[] memory, uint256[] memory);
}
