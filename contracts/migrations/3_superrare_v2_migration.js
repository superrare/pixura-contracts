var superRareV2 = artifacts.require("SuperRareV2");

module.exports = function(deployer) {
  // deployment steps
  deployer.deploy(
    superRareV2,
    "SuperRare",
    "SUPR",
    "0x41a322b28d0ff354040e2cbc676f0320d8c8850d"
  );
};
