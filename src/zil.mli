open Libsecp256k1.External

val int64str_encoding : int64 Json_encoding.encoding
val addr_encoding : [`Zil] Bech32.Segwit.t Json_encoding.encoding
val bs_hex_encoding : Bigstring.t Json_encoding.encoding

type tx = {
  network: [`Mainnet | `Testnet];
  nonce: int64;
  toaddr: [`Zil] Bech32.Segwit.t;
  senderpubkey: Key.public Key.t;
  amount: int64;
  gasprice: int64;
  gaslimit: int64;
  code: string option;
  data: Json_repr.ezjsonm option;
}

val unsigned_encoding :
  Context.t -> tx Json_encoding.encoding
val signed_encoding :
  Context.t -> (tx * Bigstring.t) Json_encoding.encoding

val simple_tx :
  ?gasprice:int64 ->
  ?gaslimit:int64 ->
  network:[ `Mainnet | `Testnet ] ->
  nonce:int64 ->
  toaddr:[ `Zil ] Bech32.Segwit.t ->
  senderpubkey:Key.public Key.t -> amount:int64 -> unit -> tx

val write : Context.t -> tx -> Faraday.t
val read : Context.t -> string -> (tx, string) result

type 'a msg = {
  name: string ;
  params: 'a list ;
}

val create_msg : ?params:'a list -> string -> 'a msg

val msg_encoding :
  'a Json_encoding.encoding -> 'a msg Json_encoding.encoding
