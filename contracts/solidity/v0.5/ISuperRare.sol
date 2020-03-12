pragma solidity ^0.5.0;

interface ISuperRare {
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
    function creatorOfToken(uint256 _tokenId) external view returns (address);

    /**
  * @dev Gets the total amount of tokens stored by the contract
  * @return uint256 representing the total amount of tokens
  */
    function totalSupply() external view returns (uint256);
}
