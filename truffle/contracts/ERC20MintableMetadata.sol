pragma solidity ^0.4.24;

import "../node_modules/openzeppelin-solidity/contracts/token/ERC20/ERC20Mintable.sol";
import "../node_modules/openzeppelin-solidity/contracts/token/ERC20/ERC20Detailed.sol";
import "../node_modules/openzeppelin-solidity/contracts/math/SafeMath.sol";
import "./ERC20Metadata.sol";

contract ERC20MintableMetadata is ERC20Mintable, ERC20Detailed, ERC20Metadata {
    using SafeMath for uint256;

    constructor(
      string memory name,
      string memory symbol,
      uint8 decimals,
      string memory uri
    )
    ERC20Detailed(name, symbol, decimals)
    ERC20Metadata(uri)
    { }
}