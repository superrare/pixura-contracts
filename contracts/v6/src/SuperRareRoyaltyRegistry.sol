pragma solidity 0.6.12;

import "openzeppelin-solidity-solc6/contracts/math/SafeMath.sol";
import "openzeppelin-solidity-solc6/contracts/access/Ownable.sol";
import "./IERC721CreatorRoyalty.sol";

/**
 * @title IERC721 Non-Fungible Token Creator basic interface
 */
contract SuperRareRoyaltyRegistry is Ownable, IERC721CreatorRoyalty {
    using SafeMath for uint256;

    /////////////////////////////////////////////////////////////////////////
    // State Variables
    /////////////////////////////////////////////////////////////////////////

    // Mapping of ERC721 contract to royalty percentage for all NFTs 3 == 3%
    mapping(address => uint8) private contractRoyaltyPercentage;

    // Mapping of ERC721 creator to royalty percentage for all NFTs.
    mapping(address => uint8) private creatorRoyaltyPercentage;

    // Mapping of ERC721 token to royalty percentage for all NFTs.
    mapping(address => mapping(uint256 => uint8))
        private tokenRoyaltyPercentage;

    IERC721TokenCreator public iERC721TokenCreator;

    /////////////////////////////////////////////////////////////////////////
    // getERC721TokenRoyaltyPercentage
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Get the royalty fee percentage for a specific ERC721 contract.
     * @param _contractAddress address ERC721Contract address.
     * @param _tokenId uint256 token ID.
     * @return uint8 wei royalty fee.
     */
    function getERC721TokenRoyaltyPercentage(
        address _contractAddress,
        uint256 _tokenId
    ) public override view returns (uint8) {
        if (tokenRoyaltyPercentage[_contractAddress][_tokenId] > 0) {
            return tokenRoyaltyPercentage[_contractAddress][_tokenId];
        }
        address creator = iERC721TokenCreator.tokenCreator(
            _contractAddress,
            _tokenId
        );
        if (creatorRoyaltyPercentage[creator] > 0) {
            return creatorRoyaltyPercentage[creator];
        }
        return contractRoyaltyPercentage[_contractAddress];
    }

    /////////////////////////////////////////////////////////////////////////
    // getPercentageForSetERC721TokenRoyalty
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Gets the royalty percentage set for an ERC721 token
     * @param _contractAddress address ERC721Contract address.
     * @param _tokenId uint256 token ID.
     * @return uint8 wei royalty fee.
     */
    function getPercentageForSetERC721TokenRoyalty(
        address _contractAddress,
        uint256 _tokenId
    ) external view returns (uint8) {
        return tokenRoyaltyPercentage[_contractAddress][_tokenId];
    }

    /////////////////////////////////////////////////////////////////////////
    // setPercentageForSetERC721TokenRoyalty
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Sets the royalty percentage set for an ERC721 token
     * Requirements:

     * - `_percentage` must be <= 100.
     * - only the owner of this contract or the creator can call this method.
     * @param _contractAddress address ERC721Contract address.
     * @param _tokenId uint256 token ID.
     * @param _percentage uint8 wei royalty fee.
     */
    function setPercentageForSetERC721TokenRoyalty(
        address _contractAddress,
        uint256 _tokenId,
        uint8 _percentage
    ) external returns (uint8) {
        require(
            msg.sender ==
                iERC721TokenCreator.tokenCreator(_contractAddress, _tokenId) ||
                msg.sender == owner(),
            "setPercentageForSetERC721TokenRoyalty::Must be contract owner or creator "
        );
        require(
            _percentage <= 100,
            "setPercentageForSetERC721TokenRoyalty::_percentage must be <= 100"
        );
        tokenRoyaltyPercentage[_contractAddress][_tokenId] = _percentage;
    }

    /////////////////////////////////////////////////////////////////////////
    // getPercentageForSetERC721CreatorRoyalty
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Gets the royalty percentage set for an ERC721 creator
     * @param _contractAddress address ERC721Contract address.
     * @param _tokenId uint256 token ID.
     * @return uint8 wei royalty fee.
     */
    function getPercentageForSetERC721CreatorRoyalty(
        address _contractAddress,
        uint256 _tokenId
    ) external view returns (uint8) {
        address creator = iERC721TokenCreator.tokenCreator(
            _contractAddress,
            _tokenId
        );
        return creatorRoyaltyPercentage[creator];
    }

    /////////////////////////////////////////////////////////////////////////
    // setPercentageForSetERC721CreatorRoyalty
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Sets the royalty percentage set for an ERC721 creator
     * Requirements:

     * - `_percentage` must be <= 100.
     * - only the owner of this contract or the creator can call this method.
     * @param _creatorAddress address token ID.
     * @param _percentage uint8 wei royalty fee.
     */
    function setPercentageForSetERC721CreatorRoyalty(
        address _creatorAddress,
        uint8 _percentage
    ) external returns (uint8) {
        require(
            msg.sender == _creatorAddress || msg.sender == owner(),
            "setPercentageForSetERC721CreatorRoyalty::Must be owner or creator"
        );
        require(
            _percentage <= 100,
            "setPercentageForSetERC721CreatorRoyalty::_percentage must be <= 100"
        );
        creatorRoyaltyPercentage[_creatorAddress] = _percentage;
    }

    /////////////////////////////////////////////////////////////////////////
    // getPercentageForSetERC721ContractRoyalty
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Gets the royalty percentage set for an ERC721 contract
     * @param _contractAddress address ERC721Contract address.
     * @return uint8 wei royalty fee.
     */
    function getPercentageForSetERC721ContractRoyalty(address _contractAddress)
        external
        view
        returns (uint8)
    {
        return contractRoyaltyPercentage[_contractAddress];
    }

    /////////////////////////////////////////////////////////////////////////
    // setPercentageForSetERC721ContractRoyalty
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Sets the royalty percentage set for an ERC721 token
     * Requirements:

     * - `_percentage` must be <= 100.
     * - only the owner of this contract.
     * @param _contractAddress address ERC721Contract address.
     * @param _percentage uint8 wei royalty fee.
     */
    function setPercentageForSetERC721ContractRoyalty(
        address _contractAddress,
        uint8 _percentage
    ) external onlyOwner returns (uint8) {
        require(
            _percentage <= 100,
            "setPercentageForSetERC721ContractRoyalty::_percentage must be <= 100"
        );
        contractRoyaltyPercentage[_contractAddress] = _percentage;
    }

    /////////////////////////////////////////////////////////////////////////
    // calculateRoyaltyFee
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Utililty function to calculate the royalty fee for a token.
     * @param _contractAddress address ERC721Contract address.
     * @param _tokenId uint256 token ID.
     * @param _amount uint256 wei amount.
     * @return uint256 wei fee.
     */
    function calculateRoyaltyFee(
        address _contractAddress,
        uint256 _tokenId,
        uint256 _amount
    ) external override view returns (uint256) {
        return
            _amount
                .mul(
                getERC721TokenRoyaltyPercentage(_contractAddress, _tokenId)
            )
                .div(100);
    }

    /////////////////////////////////////////////////////////////////////////
    // tokenCreator
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Gets the creator of the token
     * @param _contractAddress address of the ERC721 contract
     * @param _tokenId uint256 ID of the token
     * @return address of the creator
     */
    function tokenCreator(address _contractAddress, uint256 _tokenId)
        external
        override
        view
        returns (address payable)
    {
        return iERC721TokenCreator.tokenCreator(_contractAddress, _tokenId);
    }
}
