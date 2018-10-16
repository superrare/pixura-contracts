pragma solidity ^0.4.24;

contract WhitelistInterface {

  /**
   * @dev Returns whether the creator is whitelisted
   * @param _creator address to check
   * @return bool
   */
  function isWhitelisted(address _creator) external view returns (bool);
}
