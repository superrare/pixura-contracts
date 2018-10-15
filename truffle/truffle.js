var HDWalletProvider = require("truffle-hdwallet-provider");
var mnemonic = undefined;
var mnemonic_live = undefined;

module.exports = {
  // See <http://truffleframework.com/docs/advanced/configuration>
  // to customize your Truffle configuration!
  networks: {
    local: {
      host: "0.0.0.0",
      port: 8545,
      gas: 4612388,
      network_id: "420123" // Match any network id
    },
    ropsten: {
      port: 80,
      network_id: "3",
      gas: 4700000,
      provider: function() {
        return new HDWalletProvider(mnemonic, "https://ropsten.infura.io/Dg4Prwp0LCIkqCq1Sbga",0);
      },
    },
    live: {
      network_id: "1",
      gas: 4700000,
      gasPrice:5000000000,
      provider: function() {
        return new HDWalletProvider(mnemonic_live, "http://livenet.pixura.io:8545",0);
      },
    },
  },
  solc: {
    optimizer: {
      enabled: true,
      runs: 200
    }
  }
};
