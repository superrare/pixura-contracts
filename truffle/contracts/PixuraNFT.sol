pragma solidity ^0.4.24;


import "../node_modules/openzeppelin-solidity/contracts/token/ERC721/ERC721Full.sol";
import "../node_modules/openzeppelin-solidity/contracts/ownership/Ownable.sol";
import "../node_modules/openzeppelin-solidity/contracts/math/SafeMath.sol";
import "./IERC721Creator.sol";
import "./Whitelist.sol";

contract PixuraNFT is ERC721Full, IERC721Creator, Ownable, Whitelist {
    using SafeMath for uint256;

    // operator address
    address public operator;
    
    // operationCost 
    uint256 public operationCost;

    // Mapping from token ID to the creator's address
    mapping(uint256 => address) private tokenCreators;

    // Mapping from token ID to the owner metadata uri
    mapping(uint256 => string) private tokenOwnerURIs;

    // Counter for creating token IDs
    uint256 private idCounter = 0;

    event TokenOwnerURISet(
      uint256 indexed _tokenId,
      address indexed _owner,
      string  _uri
    );

    constructor(
      string _name, 
      string _symbol, 
      address _operator, 
      uint256 _operationCost
    ) 
    ERC721Full(_name, _symbol)
    // Whitelist()
    {
      operator = _operator;
      operationCost = _operationCost;
    }


    /**
     * @dev Checks that the token is owned by the sender
     * @param _tokenId uint256 ID of the token
     */
    modifier onlyTokenOwner(uint256 _tokenId) {
      address owner = ownerOf(_tokenId);
      require(owner == msg.sender, "must be the owner of the token");
      _;
    }

    /**
     * @dev Checks that the token is created by the sender
     * @param _tokenId uint256 ID of the token
     */
    modifier onlyTokenCreator(uint256 _tokenId) {
      address creator = tokenCreator(_tokenId);
      require(creator == msg.sender, "must be the creator of the token");
      _;
    }

    /**
     * @dev Adds a new unique token to the supply
     * @param _uri string metadata uri associated with the token
     */
    function addNewToken(string _uri) public payable {
      require(isWhitelisted(msg.sender), "must be whitelisted to create tokens");
      payOperatorWhenNeeded();
      createToken(_uri, msg.sender);
    }

    /**
     * @dev Deletes the token with the provided ID
     * @param _tokenId uint256 ID of the token
     */
    function deleteToken(uint256 _tokenId) public onlyTokenOwner(_tokenId) {
      // Clear owner metadata (if any)
      if (bytes(tokenOwnerURIs[_tokenId]).length != 0) {
        delete tokenOwnerURIs[_tokenId];
      }
      _burn(msg.sender, _tokenId);
    }

    /**
     * @dev Updates the token metadata URI ONLY if the owner is also the
     *      creator.
     * @param _tokenId uint256 ID of the token
     * @param _uri string metadata URI
     */
    function updateTokenMetadata(uint256 _tokenId, string _uri)
      public
      onlyTokenOwner(_tokenId)
      onlyTokenCreator(_tokenId)
    {
      _setTokenURI(_tokenId, _uri);
    }

    /**
     * @dev Updates the token owner metadata URI.
     * @param _tokenId uint256 ID of the token
     * @param _uri string metadata URI
     */
    function updateTokenOwnerMetadata(uint256 _tokenId, string _uri)
      public
      onlyTokenOwner(_tokenId)
    {
      require(_exists(_tokenId), "token must exist to update the owner metadata");
      tokenOwnerURIs[_tokenId] = _uri;
      emit TokenOwnerURISet(_tokenId, msg.sender, _uri);
    }

    /**
     * @dev Removes the operator and operational cost for the NFT contract
     */
    function removeOperator() public {
      require(operator == msg.sender, "can only be called by the operator to remove the operator");
      operator = address(0);
      operationCost = 0;
    }

    /**
    * @dev Gets the creator of the token
    * @param _tokenId uint256 ID of the token
    * @return address of the creator
    */
    function tokenCreator(uint256 _tokenId) public view returns (address) {
        return tokenCreators[_tokenId];
    }

    /**
     * @dev Gets the owner metadata uri of the token
     * @param _tokenId uint256 ID of the token
     * @return metadata uri of the owner
     */
    function tokenOwnerURI(uint256 _tokenId) public view returns (string) {
      return tokenOwnerURIs[_tokenId];
    }
    
    /**
     * @dev Internal function creating a new token.
     * @param _uri string metadata uri associated with the token
     */
    function createToken(string _uri, address _creator) private returns (uint256){
      uint256 newId = idCounter;
      idCounter++;
      _mint(_creator, newId);
      _setTokenURI(newId, _uri);
      tokenCreators[newId] = _creator;
      return newId;
    }

    /**
     * @dev Internal function to pay the operator when there is one.
     */
    function payOperatorWhenNeeded() private {
      if (operator != address(0)) {
        require(
          operationCost <= msg.value, 
          "must pay operation cost if operator set"
        );
        operator.transfer(msg.value);
      }
    }

}
