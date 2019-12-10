open Zil
open Fastrest

type balance = {
  balance: int64;
  nonce: int64;
}

val getBalance :
  ?network:[< `Mainnet | `Testnet > `Mainnet] ->
  [`Zil] Bech32.Segwit.t -> (json, balance) service

type tx_result = {
  info: string ;
  txID: Bigstring.t ;
}

val createTransaction :
  tx * Bigstring.t -> (json, tx_result) service
