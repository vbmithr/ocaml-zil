open Libsecp256k1.External

val int64str_encoding : int64 Json_encoding.encoding
val addr_encoding : [`Zil] Bech32.Segwit.t Json_encoding.encoding

type tx = {
  version: [`Mainnet | `Testnet];
  nonce: int64;
  toaddr: [`Zil] Bech32.Segwit.t;
  senderpubkey: Key.public Key.t;
  amount: int64;
  gasprice: int64;
  gaslimit: int64;
  code: string option;
  data: Json_repr.ezjsonm option;
}

val write : Context.t -> tx -> Faraday.t
val tx_encoding : Context.t -> (tx * Bigstring.t) Json_encoding.encoding

type 'a msg = {
  name: string ;
  params: 'a list ;
}

val msg_encoding :
  'a Json_encoding.encoding -> 'a msg Json_encoding.encoding
