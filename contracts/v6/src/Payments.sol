pragma solidity 0.6.12;

import "openzeppelin-solidity-solc6/contracts/math/SafeMath.sol";
import "./SendValueOrEscrow.sol";

/**
 * @title Payment contract for NFT .
 */
abstract contract Payment {
    using SafeMath for uint256;

    /////////////////////////////////////////////////////////////////////////
    // payout
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Internal function to pay the seller, creator, and maintainer.

     * Requirements:
     * 
     *  -  _marketplacePercentage + _royaltyPercentage + _primarySalePercentage <= 100
     *  - _payee cannot be the zero address
     *
     * @param _amount uint256 value to be split.
     * @param _isPrimarySale bool of whether this is a primary sale.
     * @param _marketplacePercentage uint8 percentage of the fee for the marketplace.
     * @param _royaltyPercentage uint8 percentage of the fee for the royalty.
     * @param _primarySalePercentage uint8 percentage primary sale fee for the marketplace.
     * @param _payee address seller of the token.
     * @param _marketplacePayee address seller of the token.
     * @param _royaltyPayee address seller of the token.
     * @param _primarySalePayee address seller of the token.
     */
    function payout(
        uint256 _amount,
        bool _isPrimarySale,
        uint8 _marketplacePercentage,
        uint8 _royaltyPercentage,
        uint8 _primarySalePercentage,
        address payable _payee,
        address payable _marketplacePayee,
        address payable _royaltyPayee,
        address payable _primarySalePayee
    ) internal {
        require(
            _marketplacePercentage +
                _royaltyPercentage +
                _primarySalePercentage <=
                100,
            "payout::percentages cannot go beyond 100"
        );

        uint256 marketplacePayment = _calcMarketplacePayment(
            _amount,
            _marketplacePercentage
        );

        uint256 royaltyPayment = _calcRoyaltyPayment(
            _isPrimarySale,
            _amount,
            _royaltyPercentage
        );

        uint256 payeePayment = amount.sub(royaltyPayment).sub(
            primarySalePayment
        );

        if (marketplacePayment > 0) {
            SendValueOrEscrow.sendValueOrEscrow(
                _marketplacePayee,
                marketplacePayment
            );
        }
        if (sellerPayment > 0) {
            SendValueOrEscrow.sendValueOrEscrow(_, sellerPayment);
        }
        if (royaltyPayment > 0) {
            SendValueOrEscrow.sendValueOrEscrow(
                _makePayable(creator),
                royaltyPayment
            );
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // calcMarketplacePayment
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
    function calcMarketplacePayment(
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
    // _calcMarketplaceFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Private function calculate marketplace fee for a given amount.
     *      f(_amount, _fee) =  _amount * (_fee / 100)
     * @param _amount uint256 value to be split.
     * @param _marketplacePercentage uint256 marketplace percentage.
     * @return uint256 marketplace fee.
     */
    function _calcMarketplaceFee(
        uint256 _amount,
        uint256 _marketplacePercentage
    ) private pure returns (uint256) {
        return _amount.mul(_marketplacePercentage).div(100);
    }
}
