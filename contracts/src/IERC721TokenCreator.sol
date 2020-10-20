pragma solidity 0.6.12;

/**
 * @title IERC721 Non-Fungible Token Creator basic interface
 */
interface IERC721TokenCreator {
    /**
     * @dev Gets the creator of the token
     * @param _contractAddress address of the ERC721 contract
     * @param _tokenId uint256 ID of the token
     * @return address of the creator
     */
    function tokenCreator(address _contractAddress, uint256 _tokenId)
        external
        view
        returns (address payable);
}
