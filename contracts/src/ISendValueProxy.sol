pragma solidity 0.6.12;

interface ISendValueProxy {
    function sendValue(address payable _to) external payable;
}
