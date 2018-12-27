pragma solidity ^0.4.24;

import '../node_modules/zeppelin-solidity/contracts/ownership/Ownable.sol';
import './PixuraNFT.sol';

contract PixuraNFTContractGenerator is Ownable {
    using SafeMath for uint256;

    // operator address
    address private operator;
    
    // operationCost 
    uint256 private operationCost;

    // Contract Creation Cost 
    uint256 private contractCreationCost;

    event PixuraNFTContractCreated(
      address indexed _contractAddress,
      address indexed _owner
    );

    constructor(address _operator, uint256 _operationCost, uint256 _contractCreationCost) {
      operator = _operator;
      operationCost = _operationCost;
      contractCreationCost = _contractCreationCost;
    }
    
    /**
     * @dev Creates an NFT smart contract 
     * @param _name string name of the contract
     * @param _symbol string symbol of the token
     */
    function createNFTContract(string _name, string _symbol) public payable returns (address) {
      require(operator != address(0));
      require(contractCreationCost <= msg.value);
      operator.transfer(msg.value);
      PixuraNFT nftContract = new PixuraNFT(_name, _symbol, operator, operationCost);
      emit PixuraNFTContractCreated(nftContract, msg.sender);
      return nftContract;
    }
}
