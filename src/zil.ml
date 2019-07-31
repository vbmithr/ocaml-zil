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

open Rresult
open Libsecp256k1.External
open Pb

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

let simple_tx
    ?(gasprice=1000000000L)
    ?(gaslimit=1L)
    ~network ~nonce ~toaddr ~senderpubkey ~amount () =
  { network ; nonce ; toaddr ; senderpubkey ; amount ;
    gasprice ; gaslimit ; code = None ; data = None }

module ByteArray = struct
  module ByteArray = (val message "ByteArray")
  include ByteArray
  let data = ByteArray.required string "byte" 1

  let of_string s =
    let ba = create t in
    setf ba data s ;
    ba

  let of_bigarray s = of_string (Bigstring.to_string s)
  let of_int64 i =
    let buf = Cstruct.create 16 in
    Cstruct.memset buf 0 ;
    Cstruct.BE.set_uint64 buf 8 i ;
    of_string (Cstruct.to_string buf)
end

module Tx = struct

  let mainnet = Unsigned.UInt32.of_int 65537
  let testnet = Unsigned.UInt32.of_int 21823489

  let network_of_p = function
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

let write ctx { network; nonce; toaddr = { prog; _ };
                senderpubkey; amount; gasprice;
                gaslimit; code; data } =

  let tx = create Tx.t in
  setf tx Tx.version (Tx.network_of_p network) ;
  setf tx Tx.nonce (Unsigned.UInt64.of_int64 nonce) ;
  setf tx Tx.toaddr prog ;
  setf tx Tx.senderpubkey (ByteArray.of_bigarray (Key.to_bytes ~compress:true ctx senderpubkey)) ;
  setf tx Tx.amount (ByteArray.of_int64 amount) ;
  setf tx Tx.gasprice (ByteArray.of_int64 gasprice) ;
  setf tx Tx.gaslimit (Unsigned.UInt64.of_int64 gaslimit) ;
  setf tx Tx.code code ;
  setf tx Tx.data (match data with
      | None -> None
      | Some _ -> invalid_arg "not implemented") ;
  write tx

let tx_of_pb ctx v =
  begin match getf v Tx.version with
    | i when i = Tx.testnet -> Ok `Testnet
    | i when i = Tx.mainnet -> Ok `Mainnet
    | _ -> Error "version"
  end >>= fun network ->
  let nonce = Unsigned.UInt64.to_int64 (getf v Tx.nonce) in
  let toaddr = Bech32.Segwit.(create (module Zil) (getf v Tx.toaddr)) in
  Key.read_pk ctx (Bigstring.of_string (getf (getf v Tx.senderpubkey) ByteArray.data)) >>= fun senderpubkey ->
  let amount = getf (getf v Tx.amount) ByteArray.data in
  let amount = Cstruct.(BE.get_uint64 (of_string ~off:8 ~len:8 amount) 0) in
  let gasprice = getf (getf v Tx.gasprice) ByteArray.data in
  let gasprice = Cstruct.(BE.get_uint64 (of_string ~off:8 ~len:8 gasprice) 0) in
  let gaslimit = Unsigned.UInt64.to_int64 (getf v Tx.gaslimit) in
  Ok { network ; nonce ; toaddr ; senderpubkey ;
       amount ; gasprice ; gaslimit ; code = None ; data = None }

let read ctx str =
  match Angstrom.parse_string (read Tx.t) str with
  | Error e -> Error e
  | Ok v -> tx_of_pb ctx v

type 'a msg = {
  name: string ;
  params: 'a list ;
}

let create_msg ?(params = []) name = { name ; params }

let msg_encoding param_encoding =
  let open Json_encoding in
  conv
    (fun { name ; params } -> (), (), name, params)
    (fun ((), (), name, params) -> { name ; params })
    (obj4
       (req "id" (constant "1"))
       (req "jsonrpc" (constant "2.0"))
       (req "method" string)
       (req "params" (list param_encoding)))

let int64str_encoding =
  let open Json_encoding in
  conv Int64.to_string Int64.of_string string

let string_rev s =
  let len = String.length s in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set buf i s.[len - i - 1]
  done ;
  Bytes.unsafe_to_string buf

let hex_of_string pkh =
  let open Z in
  let res = ref [] in
  let z = of_bits (string_rev Digestif.SHA256.(digest_string pkh |> to_raw_string)) in
  for i = 0 to 39 do
    res := (logand z (shift_left one Stdlib.(255 - 6*i)) <> Z.zero) :: !res
  done ;
  let buf = Bytes.create 40 in
  let `Hex pkh_hex = Hex.of_string pkh in
  List.iteri begin fun i -> function
    | true -> Bytes.set buf i (Char.uppercase_ascii pkh_hex.[i])
    | false -> Bytes.set buf i pkh_hex.[i]
  end (List.rev !res) ;
  `Hex (Bytes.unsafe_to_string buf)

open Json_encoding

let addr_encoding =
  conv
    (fun Bech32.Segwit.{ prog ; _ } ->
       let `Hex prog_hex = hex_of_string prog in
       prog_hex)
    (fun _ -> assert false)
    string

let pubkey_encoding ctx =
  conv
    (fun k ->
       let `Hex k_hex = Hex.of_bigstring (Key.to_bytes ~compress:true ctx k) in
       k_hex)
    (fun _ -> assert false)
    string

let bs_hex_encoding =
  conv
    (fun bs -> let `Hex bs_hex = Hex.of_bigstring bs in bs_hex)
    (fun hex -> Hex.to_bigstring (`Hex hex))
    string

let network_encoding =
  conv
    (fun n -> Unsigned.UInt32.to_int32 (Tx.network_of_p n))
    (fun _ -> assert false)
    int32

(* FIXME: code data *)
let unsigned_encoding ctx =
  conv
    (fun ({ network; nonce; toaddr; senderpubkey;
            amount; gasprice; gaslimit; code = _ ;
            data = _ }) ->
      (network, nonce, toaddr, senderpubkey, amount,
       gasprice, gaslimit, (), ()))
    (fun _ -> assert false)
    (obj9
       (req "version" network_encoding)
       (req "nonce" int53)
       (req "toAddr" addr_encoding)
       (req "pubKey" (pubkey_encoding ctx))
       (req "amount" int64str_encoding)
       (req "gasPrice" int64str_encoding)
       (req "gasLimit" int64str_encoding)
       (req "code" (constant ""))
       (req "data" (constant "")))

let signed_encoding ctx =
  merge_objs (unsigned_encoding ctx)
    (obj1 (req "signature" bs_hex_encoding))
