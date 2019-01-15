pragma solidity ^0.4.18;


import '../node_modules/zeppelin-solidity/contracts/token/ERC721/ERC721Token.sol';
import '../node_modules/zeppelin-solidity/contracts/ownership/Ownable.sol';
import '../node_modules/zeppelin-solidity/contracts/math/SafeMath.sol';
import './ERC721Metadata.sol';

contract SupeRare is ERC721Token, Ownable, ERC721Metadata {
    using SafeMath for uint256;
    
    // Percentage to owner of SupeRare. (* 10) to allow for < 1% 
    uint256 public maintainerPercentage = 30; 
    
    // Percentage to creator of artwork. (* 10) to allow for tens decimal. 
    uint256 public creatorPercentage = 100; 
    
    // Mapping from token ID to the address bidding
    mapping(uint256 => address) private tokenBidder;

    // Mapping from token ID to the current bid amount
    mapping(uint256 => uint256) private tokenCurrentBid;
    
    // Mapping from token ID to the owner sale price
    mapping(uint256 => uint256) private tokenSalePrice;

    // Mapping from token ID to the creator's address
    mapping(uint256 => address) private tokenCreator;
  
    // Mapping from token ID to the metadata uri
    mapping(uint256 => string) private tokenToURI;
    
    // Mapping from metadata uri to the token ID
    mapping(string => uint256) private uriOriginalToken;
    
    // Mapping from token ID to whether the token has been sold before.
    mapping(uint256 => bool) private tokenSold;

    // Mapping of address to boolean indicating whether the add
    mapping(address => bool) private creatorWhitelist;


    event WhitelistCreator(address indexed _creator);
    event Bid(address indexed _bidder, uint256 indexed _amount, uint256 indexed _tokenId);
    event AcceptBid(address indexed _bidder, address indexed _seller, uint256 _amount, uint256 indexed _tokenId);
    event CancelBid(address indexed _bidder, uint256 indexed _amount, uint256 indexed _tokenId);
    event Sold(address indexed _buyer, address indexed _seller, uint256 _amount, uint256 indexed _tokenId);
    event SalePriceSet(uint256 indexed _tokenId, uint256 indexed _price);

    /**
     * @dev Guarantees _uri has not been used with a token already
     * @param _uri string of the metadata uri associated with the token
     */
    modifier uniqueURI(string _uri) {
        require(uriOriginalToken[_uri] == 0);
        _;
    }

    /**
     * @dev Guarantees msg.sender is not the owner of the given token
     * @param _tokenId uint256 ID of the token to validate its ownership does not belongs to msg.sender
     */
    modifier notOwnerOf(uint256 _tokenId) {
        require(ownerOf(_tokenId) != msg.sender);
        _;
    }

    /**
     * @dev Guarantees msg.sender is a whitelisted creator of SupeRare
     */
    modifier onlyCreator() {
        require(creatorWhitelist[msg.sender] == true);
        _;
    }

    /**
     * @dev Transfers the ownership of a given token ID to another address.
     * Sets the token to be on its second sale.
     * @param _to address to receive the ownership of the given token ID
     * @param _tokenId uint256 ID of the token to be transferred
     */
    function transfer(address _to, uint256 _tokenId) public onlyOwnerOf(_tokenId) {
        tokenSold[_tokenId] = true;
        tokenSalePrice[_tokenId] = 0;
        clearApprovalAndTransfer(msg.sender, _to, _tokenId);
    }

    /**
     * @dev Adds a new unique token to the supply
     * @param _uri string metadata uri associated with the token
     */
    function addNewToken(string _uri) public uniqueURI(_uri) onlyCreator {
        uint256 newId = createToken(_uri, msg.sender);
        uriOriginalToken[_uri] = newId;
    }

    /**
     * @dev Adds a new unique token to the supply with N editions. The sale price is set for all editions
     * @param _uri string metadata uri associated with the token.
     * @param _editions uint256 number of editions to create.
     * @param _salePrice uint256 wei price of editions.
     */
    function addNewTokenWithEditions(string _uri, uint256 _editions, uint256 _salePrice) public uniqueURI(_uri) onlyCreator {
      uint256 originalId = createToken(_uri, msg.sender);
      uriOriginalToken[_uri] = originalId;

      for (uint256 i=0; i<_editions; i++){
        uint256 newId = createToken(_uri, msg.sender);
        tokenSalePrice[newId] = _salePrice;
        SalePriceSet(newId, _salePrice);
      }
    }

    /**
    * @dev Bids on the token, replacing the bid if the bid is higher than the current bid. You cannot bid on a token you already own.
    * @param _tokenId uint256 ID of the token to bid on
    */
    function bid(uint256 _tokenId) public payable notOwnerOf(_tokenId) {
        require(isGreaterBid(_tokenId));
        returnCurrentBid(_tokenId);
        tokenBidder[_tokenId] = msg.sender;
        tokenCurrentBid[_tokenId] = msg.value;
        Bid(msg.sender, msg.value, _tokenId);
    }

    /**
     * @dev Accept the bid on the token, transferring ownership to the current bidder and paying out the owner.
     * @param _tokenId uint256 ID of the token with the standing bid
     */
    function acceptBid(uint256 _tokenId) public onlyOwnerOf(_tokenId) {
        uint256 currentBid = tokenCurrentBid[_tokenId];
        address currentBidder = tokenBidder[_tokenId];
        address tokenOwner = ownerOf(_tokenId);
        address creator = tokenCreator[_tokenId];
        clearApprovalAndTransfer(msg.sender, currentBidder, _tokenId);
        payout(currentBid, owner, creator, tokenOwner, _tokenId);
        clearBid(_tokenId);
        AcceptBid(currentBidder, tokenOwner, currentBid, _tokenId);
        tokenSalePrice[_tokenId] = 0;
    }
    
    /**
     * @dev Cancels the bid on the token, returning the bid amount to the bidder.
     * @param _tokenId uint256 ID of the token with a bid
     */
    function cancelBid(uint256 _tokenId) public {
        address bidder = tokenBidder[_tokenId];
        require(msg.sender == bidder);
        uint256 bidAmount = tokenCurrentBid[_tokenId];
        msg.sender.transfer(bidAmount);
        clearBid(_tokenId);
        CancelBid(bidder, bidAmount, _tokenId);
    }
    
    /**
     * @dev Purchase the token if there is a sale price; transfers ownership to buyer and pays out owner.
     * @param _tokenId uint256 ID of the token to be purchased
     */
    function buy(uint256 _tokenId) public payable notOwnerOf(_tokenId) {
        uint256 salePrice = tokenSalePrice[_tokenId];
        uint256 sentPrice = msg.value;
        address buyer = msg.sender;
        address tokenOwner = ownerOf(_tokenId);
        address creator = tokenCreator[_tokenId];
        require(salePrice > 0);
        require(sentPrice >= salePrice);
        returnCurrentBid(_tokenId);
        clearBid(_tokenId);
        clearApprovalAndTransfer(tokenOwner, buyer, _tokenId);
        payout(sentPrice, owner, creator, tokenOwner, _tokenId);
        tokenSalePrice[_tokenId] = 0;
        Sold(buyer, tokenOwner, sentPrice, _tokenId);
    }

    /**
     * @dev Set the sale price of the token
     * @param _tokenId uint256 ID of the token with the standing bid
     */
    function setSalePrice(uint256 _tokenId, uint256 _salePrice) public onlyOwnerOf(_tokenId) {
        uint256 currentBid = tokenCurrentBid[_tokenId];
        require(_salePrice > currentBid);
        tokenSalePrice[_tokenId] = _salePrice;
        SalePriceSet(_tokenId, _salePrice);
    }

    /**
     * @dev Adds the provided address to the whitelist of creators
     * @param _creator address to be added to the whitelist
     */
    function whitelistCreator(address _creator) public onlyOwner {
      creatorWhitelist[_creator] = true;
      WhitelistCreator(_creator);
    }
    
    /**
     * @dev Set the maintainer Percentage. Needs to be 10 * target percentage
     * @param _percentage uint256 percentage * 10.
     */
    function setMaintainerPercentage(uint256 _percentage) public onlyOwner() {
       maintainerPercentage = _percentage;
    }
    
    /**
     * @dev Set the creator Percentage. Needs to be 10 * target percentage
     * @param _percentage uint256 percentage * 10.
     */
    function setCreatorPercentage(uint256 _percentage) public onlyOwner() {
       creatorPercentage = _percentage;
    }
    
    /**
     * @notice A descriptive name for a collection of NFTs in this contract
     */
    function name() external pure returns (string _name) {
        return 'SupeRare';
    }

    /**
     * @notice An abbreviated name for NFTs in this contract
     */
    function symbol() external pure returns (string _symbol) {
        return 'SUPR';
    }

    /**
     * @notice approve is not a supported function for this contract
     */
    function approve(address _to, uint256 _tokenId) public {
        revert();
    }

    /** 
     * @dev Returns whether the creator is whitelisted
     * @param _creator address to check
     * @return bool 
     */
    function isWhitelisted(address _creator) external view returns (bool) {
      return creatorWhitelist[_creator];
    }

    /** 
     * @notice A distinct Uniform Resource Identifier (URI) for a given asset.
     * @dev Throws if `_tokenId` is not a valid NFT. URIs are defined in RFC
     * 3986. The URI may point to a JSON file that conforms to the "ERC721
     * Metadata JSON Schema".
     */
    function tokenURI(uint256 _tokenId) external view returns (string) {
        ownerOf(_tokenId);
        return tokenToURI[_tokenId];
    }

    /**
    * @dev Gets the specified token ID of the uri. It only
    * returns ids of originals.
    * Throw if not connected to a token ID.
    * @param _uri string uri of metadata
    * @return uint256 token ID
    */
    function originalTokenOfUri(string _uri) public view returns (uint256) {
        uint256 tokenId = uriOriginalToken[_uri];
        ownerOf(tokenId);
        return tokenId;
    }

    /**
    * @dev Gets the current bid and bidder of the token
    * @param _tokenId uint256 ID of the token to get bid details
    * @return bid amount and bidder address of token
    */
    function currentBidDetailsOfToken(uint256 _tokenId) public view returns (uint256, address) {
        return (tokenCurrentBid[_tokenId], tokenBidder[_tokenId]);
    }

    /**
    * @dev Gets the creator of the token
    * @param _tokenId uint256 ID of the token
    * @return address of the creator
    */
    function creatorOfToken(uint256 _tokenId) public view returns (address) {
        return tokenCreator[_tokenId];
    }
    
    /**
    * @dev Gets the sale price of the token
    * @param _tokenId uint256 ID of the token
    * @return sale price of the token
    */
    function salePriceOfToken(uint256 _tokenId) public view returns (uint256) {
        return tokenSalePrice[_tokenId];
    }
    
    /**
    * @dev Internal function to return funds to current bidder.
    * @param _tokenId uint256 ID of the token with the standing bid
    */
    function returnCurrentBid(uint256 _tokenId) private {
        uint256 currentBid = tokenCurrentBid[_tokenId];
        address currentBidder = tokenBidder[_tokenId];
        if(currentBidder != address(0)) {
            currentBidder.transfer(currentBid);
        }
    }
    
    /**
    * @dev Internal function to check that the bid is larger than current bid
    * @param _tokenId uint256 ID of the token with the standing bid
    */
    function isGreaterBid(uint256 _tokenId) private view returns (bool) {
        return msg.value > tokenCurrentBid[_tokenId];
    }
    
    /**
    * @dev Internal function to clear bid
    * @param _tokenId uint256 ID of the token with the standing bid
    */
    function clearBid(uint256 _tokenId) private {
        tokenBidder[_tokenId] = address(0);
        tokenCurrentBid[_tokenId] = 0;
    }
    
    /**
    * @dev Internal function to pay the bidder, creator, and maintainer
    * @param _val uint256 value to be split
    * @param _maintainer address of account maintaining SupeRare
    * @param _creator address of the creator of token
    * @param _maintainer address of the owner of token
    */
    function payout(uint256 _val, address _maintainer, address _creator, address _tokenOwner, uint256 _tokenId) private {
        uint256 maintainerPayment;
        uint256 creatorPayment;
        uint256 ownerPayment;
        if (tokenSold[_tokenId]) {
            maintainerPayment = _val.mul(maintainerPercentage).div(1000);
            creatorPayment = _val.mul(creatorPercentage).div(1000);
            ownerPayment = _val.sub(creatorPayment).sub(maintainerPayment); 
        } else {
            maintainerPayment = 0;
            creatorPayment = _val;
            ownerPayment = 0;
            tokenSold[_tokenId] = true;
        }
        _maintainer.transfer(maintainerPayment);
        _creator.transfer(creatorPayment);
        _tokenOwner.transfer(ownerPayment);
      
    }

    /**
     * @dev Internal function creating a new token.
     * @param _uri string metadata uri associated with the token
     */
    function createToken(string _uri, address _creator) private  returns (uint256){
      uint256 newId = totalSupply() + 1;
      _mint(_creator, newId);
      tokenCreator[newId] = _creator;
      tokenToURI[newId] = _uri;
      return newId;
    }

}
