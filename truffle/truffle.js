var HDWalletProvider = require("truffle-hdwallet-provider");
var mnemonic =
  "dress deny dog rubber arrest scheme spare own fiscal expose dwarf neither";
var mnemonic_live =
  "";

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
        return new HDWalletProvider(
          mnemonic,
          "https://ropsten.infura.io/Dg4Prwp0LCIkqCq1Sbga",
          0
        );
      }
    },
    live: {
      network_id: "1",
      gas: 4700000,
      gasPrice: 5000000000,
      provider: function() {
        return new HDWalletProvider(
          mnemonic_live,
          "http://ec2-52-41-67-76.us-west-2.compute.amazonaws.com:58545",
          0
        );
      }
    }
  },
  solc: {
    optimizer: {
      enabled: true,
      runs: 200
    }
  }
};
