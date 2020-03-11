pragma solidity ^0.4.24;

import "openzeppelin-solidity/contracts/ownership/Ownable.sol";
import "SuperRareMarketAuctionV2.sol";

contract TestExpensiveWallet is Ownable {
    function() external payable {
        uint256 a = 0;
        while (a < 10000000) {
            a = a + 1;
        }
        owner().transfer(msg.value);
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
