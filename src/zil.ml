(* message ByteArray
 * {
 *     required bytes data = 1;
 * }
 * message ProtoTransactionCoreInfo
 * {
 *     optional uint32 version         = 1;
 *     optional uint64 nonce           = 2;
 *     optional bytes toaddr           = 3;
 *     optional ByteArray senderpubkey = 4;
 *     optional ByteArray amount       = 5;
 *     optional ByteArray gasprice     = 6;
 *     optional uint64 gaslimit        = 7;
 *     optional bytes code             = 8;
 *     optional bytes data             = 9;
 * } *)

open Libsecp256k1.External
open Pb

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

module ByteArray = struct
  module ByteArray = (val message "ByteArray")
  include ByteArray
  let data = ByteArray.required string "byte" 1

  let of_string s =
    let ba = create t in
    setf ba data s ;
    ba

  let of_bigarray s = of_string (Bigstring.to_string s)
  let of_int64 i = of_string (Int64.to_string i)
end

module Tx = struct

  let mainnet = Unsigned.UInt32.of_int 0xffff
  let testnet = Unsigned.UInt32.of_int 21823489

  let version_of_p = function
    | `Mainnet -> mainnet
    | `Testnet -> testnet

  module Tx = (val message "Transaction")
  include Tx
  let byteArray = msg ByteArray.t

  let version      = Tx.required uint32 "version" 1
  let nonce        = Tx.required uint64 "nonce" 2
  let toaddr       = Tx.required string "toaddr" 3
  let senderpubkey = Tx.required byteArray "senderpubkey" 4
  let amount       = Tx.required byteArray "amount" 5
  let gasprice     = Tx.required byteArray "gasprice" 6
  let gaslimit     = Tx.required uint64 "gaslimit" 7
  let code         = Tx.optional string "code" 8
  let data         = Tx.optional string "data" 9
end

let write ctx { version; nonce; toaddr = { prog; _ };
                senderpubkey; amount; gasprice;
                gaslimit; code; data } =

  let tx = create Tx.t in
  setf tx Tx.version (Tx.version_of_p version) ;
  setf tx Tx.nonce (Unsigned.UInt64.of_int64 nonce) ;
  setf tx Tx.toaddr prog ;
  setf tx Tx.senderpubkey (ByteArray.of_bigarray (Key.to_bytes ~compress:true ctx senderpubkey)) ;
  setf tx Tx.amount (ByteArray.of_int64 amount) ;
  setf tx Tx.gasprice (ByteArray.of_int64 gasprice) ;
  setf tx Tx.gaslimit (Unsigned.UInt64.of_int64 gaslimit) ;
  setf tx Tx.code code ;
  setf tx Tx.data (match data with None -> None | Some d -> Some (Yojson.Safe.to_string d)) ;
  write tx

