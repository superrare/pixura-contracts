pragma solidity 0.6.12;

import "openzeppelin-solidity-solc6/contracts/math/SafeMath.sol";
import "openzeppelin-solidity-solc6/contracts/access/Ownable.sol";
import "./IERC721CreatorRoyalty.sol";
import "./IERC721Creator.sol";

/**
 * @title IERC721 Non-Fungible Token Creator basic interface
 */
contract SuperRareTokenCreatorRegistry is Ownable, IERC721TokenCreator {
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
    // Constructor
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Initializes the contract setting the iERC721Creators with the provided addresses.
     * @param _iERC721Creators address[] to set as iERC721Creators.
     */
    constructor(address[] memory _iERC721Creators) public {
        require(
            _iERC721Creators.length < 1000,
            "constructor::Cannot mark more than 1000 addresses as IERC721Creator"
        );
        for (uint8 i = 0; i < _iERC721Creators.length; i++) {
            require(
                _iERC721Creators[i] != address(0),
                "constructor::Cannot set the null address as an IERC721Creator"
            );
            iERC721Creators[_iERC721Creators[i]] = true;
        }
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
        if (tokenCreators[_contractAddress][_tokenId] != address(0)) {
            return tokenCreators[_contractAddress][_tokenId];
        }

        if (iERC721Creators[_contractAddress]) {
            return IERC721Creator(_contractAddress).tokenCreator(_tokenId);
        }
        return address(0);
    }

    /////////////////////////////////////////////////////////////////////////
    // setTokenCreator
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Sets the creator of the token
     * @param _contractAddress address of the ERC721 contract
     * @param _tokenId uint256 ID of the token
     * @param _creator address of the creator for the token
     */
    function setTokenCreator(
        address _contractAddress,
        uint256 _tokenId,
        address payable _creator
    ) external onlyOwner {
        require(
            _creator != address(0),
            "setTokenCreator::Cannot set null address as creator"
        );
        require(
            _contractAddress != address(0),
            "setTokenCreator::_contractAddress cannot be null"
        );

        tokenCreators[_contractAddress][_tokenId] = _creator;
    }

    /////////////////////////////////////////////////////////////////////////
    // setIERC721Creator
    /////////////////////////////////////////////////////////////////////////
    /**
     * @dev Set an address as an IERC721Creator
     * @param _contractAddress address of the IERC721Creator contract
     */
    function setIERC721Creator(address _contractAddress) external onlyOwner {
        require(
            _contractAddress != address(0),
            "setIERC721Creator::_contractAddress cannot be null"
        );

        iERC721Creators[_contractAddress] = true;
    }
}
