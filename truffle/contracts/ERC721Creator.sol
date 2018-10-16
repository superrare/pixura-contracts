pragma solidity ^0.4.24;

/**
 * @title ERC721 Non-Fungible Token Creator basic interface
 */
contract ERC721Creator  {
  /**
   * @dev Gets the creator of the token
   * @param _tokenId uint256 ID of the token
   * @return address of the creator
   */
  function tokenCreator(uint256 _tokenId) public view returns (address);
}
