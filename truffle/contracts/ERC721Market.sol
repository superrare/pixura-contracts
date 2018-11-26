pragma solidity ^0.4.24;


import '../node_modules/zeppelin-solidity/contracts/token/ERC721/ERC721Basic.sol';
import '../node_modules/zeppelin-solidity/contracts/math/SafeMath.sol';

contract ERC721Market {
    using SafeMath for uint256;

    // Mapping from ERC721 contract to mapping of tokenId to sale price
    mapping(address => mapping (uint256 => uint256)) private tokenPrices; 

    // Mapping from ERC721 contract to mapping of tokenId to token owner that set the sale price.
    mapping(address => mapping (uint256 => address)) private tokenOwners; 

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
      public
      approvedForOwner(_originContract, _tokenId)
      payable
    {
      priceSetByOwner(_originContract, _tokenId);
      uint256 tokenPrice = tokenPrices[_originContract][_tokenId];
      require(tokenPrice > 0);
      require(tokenPrice == msg.value);
      ERC721Basic erc721 = ERC721Basic(_originContract);

      // pay owner
      address owner = erc721.ownerOf(_tokenId);
      owner.transfer(tokenPrice);

      // transfer token
      erc721.safeTransferFrom(owner, msg.sender, _tokenId);

      // wipe the token price
      tokenPrices[_originContract][_tokenId] = 0;

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
      public
      approvedForSender(_originContract)
      tokenOwner(_originContract, _tokenId)
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
    
    /**
     * @dev Checks that the token is owned by the same person who set the sale price
     * @param _originContract address of the contract storing the token.
     * @param _tokenId address of the contract storing the token.
     */
    function priceSetByOwner(address _originContract, uint256 _tokenId) internal view {
      ERC721Basic erc721 = ERC721Basic(_originContract);
      address owner = erc721.ownerOf(_tokenId);
      address perceivedOwner = tokenOwners[_originContract][_tokenId];
      require(owner == perceivedOwner);
    }
}
