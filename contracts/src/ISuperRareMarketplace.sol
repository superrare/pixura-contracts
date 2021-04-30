pragma solidity 0.6.12;

interface ISuperRareMarketplace {
    // ~ Storage ~
    // - variables defined here will be a combo
    //   of the auction house and marketplace data structures used
    // - same schema will need to be used for both the Auction House and
    //   Marketplace logic contracts

	// Events
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

    event Offer(
        address indexed _originContract,
        address indexed _bidder,
        uint256 _amount,
        uint256 _tokenId
    );

    event AcceptOffer(
        address indexed _originContract,
        address indexed _bidder,
        address indexed _seller,
        uint256 _amount,
        uint256 _tokenId
    );

    event CancelOffer(
        address indexed _originContract,
        address indexed _bidder,
        uint256 _amount,
        uint256 _tokenId
    );

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

    function tokenPrice(address _originContract, uint256 _tokenId)
        external
        view
        returns (uint256);

    function tokenPriceFeeIncluded(address _originContract, uint256 _tokenId)
        external
        view
        returns (uint256);

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

    function acceptOffer(
		address _originContract,
		address payable _bidder,
		uint256 _tokenId
	) external;

    function cancelOffer(address _originContract, uint256 _tokenId) external;

	// Currently have this returning arrays of bidders + amounts for the multi-offers
    function currentBidDetailsOfToken(address _originContract, uint256 _tokenId)
		external
		view
    	returns (address[] memory, uint256[] memory);

	function getHighestBidderAndBid(address _originContract, uint256 _tokenId)
		external
		view
		returns (address payable, uint256);
}
