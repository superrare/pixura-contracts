pragma solidity >= 0.4.24 < 0.5;

import "../node_modules/openzeppelin-solidity/contracts/ownership/Ownable.sol";

interface ISendValueProxy {
    function sendValue(address to, uint256 val) external payable;
}

/**
 * @dev Contract that attempts to send value to an address.
 */
contract SendValueProxy is ISendValueProxy {
    /**
   * @dev Send some wei to the address.
   * @param _to address to send some value to.
   */
    function sendValue(address _to) external payable {
        // Note that `<address>.transfer` limits gas sent to receiver. It may
        // not support complex contract operations in the future.
        _to.transfer(msg.value);
    }
}

/**
 * @dev Contract with a ISendValueProxy that will catch reverts when attempting to transfer funds.
 */
contract MaybeSendValue is Ownable {
    ISendValueProxy proxy;

    /**
   * @dev Maybe send some wei to the address via a proxy. Returns true on success and false if transfer fails.
   * @param _to address to send some value to.
   * @param _value uint256 amount to send.
   */
    function maybeSendValue(address _to, uint256 _value)
        internal
        returns (bool)
    {
        // Call sendValue on the proxy contract and forward the mesg.value.
        /* solium-disable-next-line */
        bool success = address(proxy).call.value(_value)(
            abi.encodeWithSignature("sendValue(address)", _to)
        );
        if (success) {
            return true;
        }
        return false;
    }

    /**
   * @dev Update the ISendValueProxy contract.
   * @param _proxy address to update to.
   */
    function updateProxy(address _proxy) external onlyOwner {
        proxy = ISendValueProxy(_proxy);
    }
}
