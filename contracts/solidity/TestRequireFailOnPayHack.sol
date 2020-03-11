pragma solidity ^0.4.24;

import "SuperRareMarketAuctionV2.sol";

contract TestRequireFailOnPayHack is Ownable {
    function() external payable {
        require(false, "ready to fail!!!");
    }

    function bid(
        uint256 _newBidAmount,
        address _originContract,
        uint256 _tokenId,
        address market
    ) public payable {
        SuperRareMarketAuctionV2(market).bid(
            _newBidAmount,
            _originContract,
            _tokenId
        );
    }
}
