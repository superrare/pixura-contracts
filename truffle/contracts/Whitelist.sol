pragma solidity ^0.4.24;

import '../node_modules/zeppelin-solidity/contracts/ownership/Ownable.sol';
import './WhitelistInterface.sol';

contract Whitelist is Ownable, WhitelistInterface {

  // Mapping of address to boolean indicating whether the address is whitelisted
  mapping(address => bool) private whitelist;

  event AddToWhitelist(address indexed _newAddress);
  event RemoveFromWhitelist(address indexed _newAddress);

  /**
   * @dev Adds the provided address to the whitelist
   * @param _newAddress address to be added to the whitelist
   */
  function addToWhitelist(address _newAddress) public onlyOwner {
    whitelist[_newAddress] = true;
    emit AddToWhitelist(_newAddress);
  }

  /**
   * @dev Removes the provided address to the whitelist
   * @param _removedAddress address to be removed from the whitelist
   */
  function removeFromWhitelist(address _removedAddress) public onlyOwner {
    whitelist[_removedAddress] = true;
    emit RemoveFromWhitelist(_removedAddress);
  }

  /**
   * @dev Returns whether the address is whitelisted
   * @param _address address to check
   * @return bool
   */
  function isWhitelisted(address _address) external view returns (bool) {
    return whitelist[_address];
  }
}
