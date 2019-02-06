pragma solidity ^0.4.24;


import "../node_modules/openzeppelin-solidity/contracts/token/ERC721/IERC721.sol";
import "../node_modules/openzeppelin-solidity/contracts/math/SafeMath.sol";
import "../node_modules/openzeppelin-solidity/contracts/ownership/Ownable.sol";

contract ERC721Market is Ownable {
    using SafeMath for uint256;

    // Mapping from ERC721 contract to mapping of tokenId to sale price
    mapping(address => mapping (uint256 => uint256)) private tokenPrices; 

    // Mapping from ERC721 contract to mapping of tokenId to token owner that set the sale price.
    mapping(address => mapping (uint256 => address)) private tokenOwners; 

    // Marketplace fee paid to the owner of the contract.
    uint256 private marketplaceFee = 3; // 3 %

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
    modifier ownerMustHaveMarketplaceApproved(address _originContract, uint256 _tokenId) {
      IERC721 erc721 = IERC721(_originContract);
      address owner = erc721.ownerOf(_tokenId);
      require(erc721.isApprovedForAll(owner, this));
      _;
    }

    /**
     * @dev Checks that the msg.sender is approved for the ERC721Market
     * @param _originContract address of the contract storing the token.
     */
    modifier senderMustHaveMarketplaceApproved(address _originContract) {
      IERC721 erc721 = IERC721(_originContract);
      require(erc721.isApprovedForAll(msg.sender, this));
      _;
    }
    

    /**
     * @dev Checks that the token owned by the sender
     * @param _originContract address of the contract storing the token.
     * @param _tokenId uint256 ID of the token
     */
    modifier senderMustBeTokenOwner(address _originContract, uint256 _tokenId) {
      IERC721 erc721 = IERC721(_originContract);
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
      ownerMustHaveMarketplaceApproved(_originContract, _tokenId)
      payable
    {
      doesPriceSetterStillOwnTheToken(_originContract, _tokenId);
      uint256 tokenPrice = tokenPrices[_originContract][_tokenId];
      require(tokenPrice > 0, "Tokens priced at 0 are not for sale.");
      require(tokenPrice == msg.value, "Must purchase the token for the correct price");
      IERC721 erc721 = IERC721(_originContract);

      // pay owner and pay marketplace owner
      address owner = erc721.ownerOf(_tokenId);
      address marketplaceOwner = this.owner();

      uint256 marketFeePayment = tokenPrice * marketplaceFee / 100;
      uint256 ownerPayment = tokenPrice - marketFeePayment;

      owner.transfer(ownerPayment);
      marketplaceOwner.transfer(marketFeePayment);

      // transfer token
      erc721.safeTransferFrom(owner, msg.sender, _tokenId);

      // wipe the token price
      tokenPrices[_originContract][_tokenId] = 0;
      tokenOwners[_originContract][_tokenId] = address(0);

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
      senderMustHaveMarketplaceApproved(_originContract)
      senderMustBeTokenOwner(_originContract, _tokenId)
      payable
    {
      tokenPrices[_originContract][_tokenId] = _amount;
      tokenOwners[_originContract][_tokenId] = msg.sender;
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
    function doesPriceSetterStillOwnTheToken(address _originContract, uint256 _tokenId) internal view {
      IERC721 erc721 = IERC721(_originContract);
      address owner = erc721.ownerOf(_tokenId);
      address perceivedOwner = tokenOwners[_originContract][_tokenId];
      require(owner == perceivedOwner, "Current token owner must be the person to have the latest price.");
    }
}
