pragma solidity ^0.4.24;


/**
 * @title ERC721 interface
 * @dev see https://github.com/ethereum/eips/issues/721, code take from: https://github.com/OpenZeppelin/openzeppelin-solidity/blob/v1.6.0/contracts/token/ERC721/ERC721.sol
 */
contract ERC721Old {
  event Transfer(address indexed _from, address indexed _to, uint256 _tokenId);
  event Approval(address indexed _owner, address indexed _approved, uint256 _tokenId);

  function balanceOf(address _owner) public view returns (uint256 _balance);
  function ownerOf(uint256 _tokenId) public view returns (address _owner);
  function transfer(address _to, uint256 _tokenId) public;
  function approve(address _to, uint256 _tokenId) public;
  function takeOwnership(uint256 _tokenId) public;
}