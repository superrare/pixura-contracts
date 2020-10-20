pragma solidity 0.6.12;

import "./SuperRareMarketAuctionV2.sol";

contract TestAssertFailOnPay {
    /**
     * @dev A payment method that will fail by an assertion
     */
    receive() external payable {
        assert(false);
    }

    /**
     * @dev Place a bid for the owner
     * @param _newBidAmount uint256 value in wei to bid, plus marketplace fee.
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @param _market address of the marketplace to make the bid
     */
    function bid(
        uint256 _newBidAmount,
        address _originContract,
        uint256 _tokenId,
        address _market
    ) public payable {
        SuperRareMarketAuctionV2(_market).bid{value: msg.value}(
            _newBidAmount,
            _originContract,
            _tokenId
        );
    }
}
