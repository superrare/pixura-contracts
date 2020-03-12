pragma solidity ^0.4.24;

import "openzeppelin-solidity/contracts/math/SafeMath.sol";

contract Operated {
    // operator address
    address public operator;

    // operation cost
    uint256 public operationCost;

    event OperatorSet(address indexed _operator);
    event OperationCostSet(uint256 indexed _operationCost);

    constructor(address _operator, uint256 _operationCost) {
        operator = _operator;
        operationCost = _operationCost;
        emit OperatorSet(operator);
        emit OperationCostSet(operationCost);
    }

    /**
   * @dev Removes the operator and operational cost for the contract
   */
    function removeOperator() public {
        setOperator(address(0));
        setOperationCost(0);
    }

    /**
   * @dev Set operational cost for the NFT contract
   * @param _operationCost uint256 new cost to operate the contract
   */
    function setOperationCost(uint256 _operationCost) public {
        require(operator == msg.sender, "can only be called by the operator");
        operationCost = _operationCost;
        emit OperationCostSet(operationCost);
    }

    /**
   * @dev Set operational cost for the NFT contract
   * @param _operator address new operator for the contract
   */
    function setOperator(address _operator) public {
        require(operator == msg.sender, "can only be called by the operator");
        operator = _operator;
    }

    /**
   * @dev Internal function to pay the operator when there is one.
   */
    function payOperatorWhenNeeded() internal {
        if (operator != address(0)) {
            require(
                operationCost <= msg.value,
                "must pay operation cost if operator set"
            );
            operator.transfer(msg.value);
        }
    }
}
