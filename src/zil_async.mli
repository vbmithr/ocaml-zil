open Fastrest

type balance = {
  balance: int64;
  nonce: int64;
}

val getBalance :
  ?network:[< `Mainnet | `Testnet > `Mainnet] ->
  [`Zil] Bech32.Segwit.t -> (post_json, balance, 'b) service
