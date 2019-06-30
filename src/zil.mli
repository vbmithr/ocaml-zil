open Libsecp256k1.External

type tx = {
  version: [`Mainnet | `Testnet];
  nonce: int64;
  toaddr: [`Zil] Bech32.Segwit.t;
  senderpubkey: Key.public Key.t;
  amount: int64;
  gasprice: int64;
  gaslimit: int64;
  code: string option;
  data: Yojson.Safe.t option;
}

val write : Context.t -> tx -> Faraday.t
