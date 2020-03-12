pragma solidity ^0.5.0;

interface ISendValueProxy {
    function sendValue(address payable _to) external payable;
}
