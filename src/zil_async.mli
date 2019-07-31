open Zil
open Fastrest

type error = {
  code: int ;
  message: string ;
}

type balance = {
  balance: int64;
  nonce: int64;
}

val getBalance :
  ?network:[< `Mainnet | `Testnet > `Mainnet] ->
  [`Zil] Bech32.Segwit.t -> (json, balance, error) service

type tx_result = {
  info: string ;
  txID: Bigstring.t ;
}

val createTransaction :
  tx * Bigstring.t -> (json, tx_result, error) service
