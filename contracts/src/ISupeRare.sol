pragma solidity 0.6.12;

/**
 * @dev Interface for interacting with the SupeRare contract that holds SuperRare beta tokens.
 */
interface ISupeRare {
    /**
     * @notice A descriptive name for a collection of NFTs in this contract
     */
    function name() external pure returns (string memory _name);

    /**
     * @notice An abbreviated name for NFTs in this contract
     */
    function symbol() external pure returns (string memory _symbol);

    /**
     * @dev Returns whether the creator is whitelisted
     * @param _creator address to check
     * @return bool
     */
    function isWhitelisted(address _creator) external view returns (bool);

    /**
     * @notice A distinct Uniform Resource Identifier (URI) for a given asset.
     * @dev Throws if `_tokenId` is not a valid NFT. URIs are defined in RFC
     * 3986. The URI may point to a JSON file that conforms to the "ERC721
     * Metadata JSON Schema".
     */
    function tokenURI(uint256 _tokenId) external view returns (string memory);

    /**
     * @dev Gets the creator of the token
     * @param _tokenId uint256 ID of the token
     * @return address of the creator
     */
    function creatorOfToken(uint256 _tokenId)
        external
        view
        returns (address payable);

    /**
     * @dev Gets the total amount of tokens stored by the contract
     * @return uint256 representing the total amount of tokens
     */
    function totalSupply() external view returns (uint256);

    /**
     * @dev Gets the owner of the specified token ID
     * @param _tokenId uint256 ID of the token to query the owner of
     * @return owner address currently marked as the owner of the given token ID
     */
    function ownerOf(uint256 _tokenId) external view returns (address);
}
