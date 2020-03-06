var superRareMarketAuction = artifacts.require("SuperRareMarketAuction");

module.exports = function(deployer) {
  // deployment steps
  deployer.deploy(superRareMarketAuction);
};
