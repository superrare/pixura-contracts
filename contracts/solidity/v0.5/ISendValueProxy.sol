pragma solidity ^0.5.0;

interface ISendValueProxy {
    function sendValue(address _to) external payable;
}
