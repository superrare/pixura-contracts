pragma solidity 0.6.12;

import "./IMarketplaceSettings.sol";

/**
 * @title MarketplaceSettings Settings governing the marketplace fees.
 */
contract MarketplaceSettings is IMarketplaceSettings {
    /////////////////////////////////////////////////////////////////////////
    // getMarketplaceFeePercentage
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Get the marketplace fee percentage.
     * @return uint8 wei fee.
     */
    function getMarketplaceFeePercentage()
        external
        override
        view
        returns (uint8)
    {
        return 1;
    }

    /////////////////////////////////////////////////////////////////////////
    // setMarketplaceFeePercentage
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Set the marketplace fee percentage.

     * Requirements:

     * - `_percentage` must be <= 100.

     * @param _percentage uint8 percentage fee.
     */
    function setMarketplaceFeePercentage(uint8 _percentage) external {}

    /////////////////////////////////////////////////////////////////////////
    // calculateMarketplaceFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Utility function for calculating the marketplace fee for given amount of wei.
     * @param _amount uint256 wei amount.
     * @return uint256 wei fee.
     */
    function calculateMarketplaceFee(uint256 _amount)
        external
        override
        view
        returns (uint256)
    {
        return 1;
    }

    /////////////////////////////////////////////////////////////////////////
    // getERC721ContractPrimarySaleFeePercentage
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Get the primary sale fee percentage for a specific ERC721 contract.
     * @param _contractAddress address ERC721Contract address.
     * @return uint8 wei primary sale fee.
     */
    function getERC721ContractPrimarySaleFeePercentage(address _contractAddress)
        external
        override
        view
        returns (uint8)
    {
        return 1;
    }

    /////////////////////////////////////////////////////////////////////////
    // setERC721ContractPrimarySaleFeePercentage
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Set the primary sale fee percentage for a specific ERC721 contract.

     * Requirements:
     *
     * - `_contractAddress` cannot be the zero address.
     * - `_percentage` must be <= 100.

     * @param _contractAddress address ERC721Contract address.
     * @param _percentage uint8 percentage fee for the ERC721 contract.
     */
    function setERC721ContractPrimarySaleFeePercentage(
        address _contractAddress,
        uint8 _percentage
    ) external {}

    /////////////////////////////////////////////////////////////////////////
    // calculatePrimarySaleFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Utility function for calculating the primary sale fee for given amount of wei
     * @param _contractAddress address ERC721Contract address.
     * @param _amount uint256 wei amount.
     * @return uint256 wei fee.
     */
    function calculatePrimarySaleFee(address _contractAddress, uint256 _amount)
        external
        override
        view
        returns (uint256)
    {
        return 0;
    }

    /////////////////////////////////////////////////////////////////////////
    // hasERC721TokenSold
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Check whether the ERC721 token has sold at least once.
     * @param _contractAddress address ERC721Contract address.
     * @param _tokenId uint256 token ID.
     * @return bool of whether the token has sold.
     */
    function hasERC721TokenSold(address _contractAddress, uint256 _tokenId)
        external
        override
        view
        returns (bool)
    {
        return false;
    }

    /////////////////////////////////////////////////////////////////////////
    // markERC721TokenAsSold
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Mark a token as sold.

     * Requirements:
     *
     * - `_contractAddress` cannot be the zero address.
     * - token must exist

     * @param _contractAddress address ERC721Contract address.
     * @param _tokenId uint256 token ID.
     */
    function markERC721TokenAsSold(address _contractAddress, uint256 _tokenId)
        external
        override
    {}
}
