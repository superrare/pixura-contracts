pragma solidity ^0.5.0;

import "./SendValueProxy.sol";

/**
 * @dev Contract with a ISendValueProxy that will catch reverts when attempting to transfer funds.
 */
contract MaybeSendValue {
    SendValueProxy proxy;

    constructor() internal {
        proxy = new SendValueProxy();
    }

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
}
