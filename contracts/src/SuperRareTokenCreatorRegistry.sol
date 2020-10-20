pragma solidity 0.6.12;

import "openzeppelin-solidity-solc6/contracts/math/SafeMath.sol";
import "openzeppelin-solidity-solc6/contracts/access/Ownable.sol";
import "./IERC721CreatorRoyalty.sol";
import "./IERC721Creator.sol";

/**
 * @title IERC721 Non-Fungible Token Creator basic interface
 */
contract SuperRareRoyaltyRegistry is Ownable, IERC721TokenCreator {
    using SafeMath for uint256;

    /////////////////////////////////////////////////////////////////////////
    // State Variables
    /////////////////////////////////////////////////////////////////////////

    // Mapping of ERC721 token to it's creator.
    mapping(address => mapping(uint256 => address payable))
        private tokenCreators;

    // Mapping of addresses that implement IERC721Creator.
    mapping(address => bool) private iERC721Creators;

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
        if (tokenCreators[_contractAddress][_tokenId] != address(0)) {
            return tokenCreators[_contractAddress][_tokenId];
        }

        if (iERC721Creators[_contractAddress]) {
            return IERC721Creator(_contractAddress).tokenCreator(_tokenId);
        }
        return address(0);
    }
}
