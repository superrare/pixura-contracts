pragma solidity ^0.4.24;

import "../node_modules/openzeppelin-solidity/contracts/ownership/Ownable.sol";
import "./PixuraNFT.sol";
import "./Operated.sol";

contract PixuraNFTContractGenerator is Ownable, Operated {
    using SafeMath for uint256;

    // Operating cost for the NFT Contracts
    uint256 private nftOperationCost;

    event PixuraNFTContractCreated(
      address indexed _contractAddress,
      address indexed _owner
    );

    constructor(
      uint256 _operationCost,
      uint256 _nftOperationCost
    ) 
    Operated(msg.sender, _operationCost)
    {
      nftOperationCost = _nftOperationCost;
    }
    
    /**
     * @dev Creates an NFT smart contract 
     * @param _name string name of the contract
     * @param _symbol string symbol of the token
     */
    function createNFTContract(string _name, string _symbol) public payable returns (address) {
      payOperatorWhenNeeded();
      PixuraNFT nftContract = new PixuraNFT(_name, _symbol, operator, nftOperationCost);
      nftContract.transferOwnership(msg.sender);
      emit PixuraNFTContractCreated(nftContract, msg.sender);
      return nftContract;
    }
}
