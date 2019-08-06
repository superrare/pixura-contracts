pragma solidity ^0.4.24;

import "../node_modules/openzeppelin-solidity/contracts/ownership/Ownable.sol";

contract Whitelist is Ownable {

  // Mapping of address to boolean indicating whether the address is whitelisted
  mapping(address => bool) private whitelistMap;

  // flag controlling whether whitelist is enabled.
  bool private whitelistEnabled = true;

  event AddToWhitelist(address indexed _newAddress);
  event RemoveFromWhitelist(address indexed _newAddress);


  /**
   * @dev Enable or disable the whitelist
   * @param _enabled bool of whether to enable the whitelist.
   */
  function enableWhitelist(bool _enabled) public onlyOwner {
    whitelistEnabled = _enabled;
  }

  /**
   * @dev Adds the provided address to the whitelist
   * @param _newAddress address to be added to the whitelist
   */
  function addToWhitelist(address _newAddress) public onlyOwner {
    _whitelist(_newAddress);
  }

  /**
   * @dev Removes the provided address to the whitelist
   * @param _removedAddress address to be removed from the whitelist
   */
  function removeFromWhitelist(address _removedAddress) public onlyOwner {
    _unWhitelist(_removedAddress);
  }

  /**
   * @dev Returns whether the address is whitelisted
   * @param _address address to check
   * @return bool
   */
  function isWhitelisted(address _address) public view returns (bool) {
    if (whitelistEnabled) {
      return whitelistMap[_address];
    } else {
      return true;
    }
  }

  /**
   * @dev Internal function for removing an address from the whitelist
   * @param _removedAddress address to unwhitelisted
   */
  function _unWhitelist(address _removedAddress) internal {
    whitelistMap[_removedAddress] = false;
    emit RemoveFromWhitelist(_removedAddress);
  }

  /**
   * @dev Internal function for adding the provided address to the whitelist
   * @param _newAddress address to be added to the whitelist
   */
  function _whitelist(address _newAddress) internal {
    whitelistMap[_newAddress] = true;
    emit AddToWhitelist(_newAddress);
  }
}
