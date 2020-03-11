pragma solidity ^0.4.24;

import "SuperRareMarketAuctionV2.sol";

contract TestAssertFailOnPayHack is Ownable {
    function() external payable {
        assert(false);
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
