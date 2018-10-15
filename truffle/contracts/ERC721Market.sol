pragma solidity ^0.4.24;


import '../node_modules/zeppelin-solidity/contracts/token/ERC721/ERC721Basic.sol';
import '../node_modules/zeppelin-solidity/contracts/math/SafeMath.sol';

contract ERC721Market {
    using SafeMath for uint256;

    // Mapping from token ID to the creator's address
    mapping(address => mapping (uint256 => uint256)) private tokenPrices; 

    event Sold(
      address indexed _originContract,
      address indexed _buyer,
      address indexed _seller,
      uint256 _amount,
      uint256 _tokenId
    );

    event SetSalePrice(
      address indexed _originContract,
      uint256 _amount,
      uint256 _tokenId
    );

    /**
     * @dev Checks that the token owner is approved for the ERC721Market
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    modifier approvedForOwner(address _originContract, uint256 _tokenId) {
      ERC721Basic erc721 = ERC721Basic(_originContract);
      address owner = erc721.ownerOf(_tokenId);
      require(erc721.isApprovedForAll(owner, this));
      _;
    }

    /**
     * @dev Checks that the msg.sender is approved for the ERC721Market
     * @param _originContract address of the contract storing the token.
     */
    modifier approvedForSender(address _originContract) {
      ERC721Basic erc721 = ERC721Basic(_originContract);
      require(erc721.isApprovedForAll(msg.sender, this));
      _;
    }

    /**
     * @dev Checks that the token owned by the sender
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    modifier tokenOwner(address _originContract, uint256 _tokenId) {
      ERC721Basic erc721 = ERC721Basic(_originContract);
      require(erc721.ownerOf(_tokenId) == msg.sender);
      _;
    }

    /**
     * @dev Purchases the token if it is for sakle
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    function buy(
      address _originContract,
      uint256 _tokenId
    )
      approvedForOwner(_originContract, _tokenId)
      public
      payable
    {
      uint256 tokenPrice = tokenPrices[_originContract][_tokenId];
      require(tokenPrice > 0);
      require(tokenPrice == msg.value);
      ERC721Basic erc721 = ERC721Basic(_originContract);

      // pay owner
      address owner = erc721.ownerOf(_tokenId);
      owner.transfer(tokenPrice);

      //transfer token
      erc721.safeTransferFrom(owner, msg.sender, _tokenId);

      emit Sold(_originContract, msg.sender, owner, tokenPrice, _tokenId);
    }

    /**
     * @dev Set the token for sale
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @param _amount uint256 wei value that the item is for sale
     */
    function setSalePrice(
      address _originContract,
      uint256 _tokenId,
      uint256 _amount
    )
      approvedForSender(_originContract)
      tokenOwner(_originContract, _tokenId)
      public
      payable
    {
      tokenPrices[_originContract][_tokenId] = _amount;
      emit SetSalePrice(_originContract, _amount, _tokenId);
    }

    /**
     * @dev Gets the sale price of the token
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     * @return sale price of the token
     */
    function tokenPrice(
      address _originContract,
      uint256 _tokenId
    )
      public view returns (uint256)
    {
      return tokenPrices[_originContract][_tokenId];
    }
}
