var nftContractGen = artifacts.require("PixuraNFTContractGenerator");
var web3 = require("web3-utils");
module.exports = function(deployer, network) {
  // deployment steps
  const oneDollarInEther = 0.00769230769;
  const fifteenUSD = oneDollarInEther * 15;
  const nftContractCreationCost = web3.toWei(fifteenUSD.toString(), "ether");
  const nftCreationCost = web3.toWei(oneDollarInEther.toString(), "ether");
  deployer.deploy(nftContractGen, nftContractCreationCost, nftCreationCost);
};
