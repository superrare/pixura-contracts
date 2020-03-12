pragma solidity ^0.4.24;

interface ISendValueProxy {
    function sendValue(address _to) external payable;
}
