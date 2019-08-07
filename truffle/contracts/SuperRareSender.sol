pragma solidity ^0.4.24;


import "../node_modules/openzeppelin-solidity/contracts/token/ERC721/IERC721.sol";
import "./IERC721Creator.sol";
import "../node_modules/openzeppelin-solidity/contracts/math/SafeMath.sol";
import "../node_modules/openzeppelin-solidity/contracts/ownership/Ownable.sol";
import "../node_modules/0x-monorepo/packages/contracts/contracts/protocol/Exchange/libs/LibOrder.sol";

contract SuperRareSenderContract is Ownable {
    using SafeMath for uint256;

    // Marketplace fee paid to the owner of the contract.
    uint256 private marketplaceFee = 3; // 3 %
    
    // Royalty fee paid to the creator of a token on secondary sales.
    uint256 private royaltyFee = 3; // 3 %
    
    // Primary sale fee split.
    uint256 private primarySaleFee = 15; // 15 %

    // Mapping of ERC721 contract to mapping of token ID to whether the token has been sold before via this contract.
    mapping (address => mapping (uint256 => bool)) private tokenSold;

    event Sold (
      address indexed _originContract,
      address indexed _buyer,
      address indexed _seller,
      uint256 _amount,
      uint256 _tokenId
    );

    event AcceptBid (
      address indexed _originContract,
      address indexed _bidder,
      address indexed _seller,
      uint256 _amount,
      uint256 _tokenId
    );


}
