pragma solidity ^0.4.24;

import "openzeppelin-solidity/contracts/token/ERC721/IERC721.sol";

/**
 * @title IERC721 Non-Fungible Token Creator basic interface
 */
contract IERC721Creator is IERC721 {
    /**
   * @dev Gets the creator of the token
   * @param _tokenId uint256 ID of the token
   * @return address of the creator
   */
    function tokenCreator(uint256 _tokenId) external view returns (address);
}
