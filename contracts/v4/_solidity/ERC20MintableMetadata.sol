pragma solidity ^0.4.24;

import "openzeppelin-solidity-solc4/contracts/token/ERC20/ERC20Mintable.sol";
import "openzeppelin-solidity-solc4/contracts/token/ERC20/ERC20Detailed.sol";
import "openzeppelin-solidity-solc4/contracts/math/SafeMath.sol";
import "./ERC20Metadata.sol";

contract ERC20MintableMetadata is ERC20Mintable, ERC20Detailed, ERC20Metadata {
    using SafeMath for uint256;

    constructor(
        string memory name,
        string memory symbol,
        uint8 decimals,
        string memory uri
    ) ERC20Detailed(name, symbol, decimals) ERC20Metadata(uri) {}
}
