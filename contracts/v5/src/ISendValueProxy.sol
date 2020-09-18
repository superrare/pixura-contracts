pragma solidity ^0.5.17;

interface ISendValueProxy {
    function sendValue(address payable _to) external payable;
}
