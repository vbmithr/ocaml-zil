open Core
open Fastrest
open Libsecp256k1.External
open Zil

let mainnet =
  Uri.make ~scheme:"https" ~host:"api.zilliqa.com" ()

let testnet =
  Uri.make ~scheme:"https" ~host:"dev-api.zilliqa.com" ()

let uri_of_network = function
  | `Mainnet -> mainnet
  | `Testnet -> testnet

(* let params meth params = [
 *   "id", ["1"];
 *   "jsonrpc", ["2.0"];
 *   "method", [meth];
 *   "params", params;
 * ] *)

(* let json_of_params (params : Fastrest.params) : Ezjsonm.t =
 *   `O [
 *     "id", `String (String.concat "" (List.assoc "id" params)) ;
 *     "jsonrpc", `String (String.concat "" (List.assoc "jsonrpc" params)) ;
 *     "method", `String (String.concat "" (List.assoc "method" params)) ;
 *     "params", `A (List.map (fun s -> `String s) (List.assoc "params" params)) ;
 *   ] *)

let error_encoding =
  let open Json_encoding in
  conv
    (fun _ -> assert false)
    (fun ((), (code, message)) -> Error.createf "%d: %s" code message)
    (merge_objs unit (obj2
                        (req "code" int)
                        (req "message" string)))

let jsonrpc_encoding =
  let open Json_encoding in
  conv (fun () -> (), ()) (fun ((), ()) -> ())
    (obj2
       (req "id" (constant "1"))
       (req "jsonrpc" (constant "2.0")))

let result_encoding result_encoding =
  let open Json_encoding in
  union [
    case (merge_objs jsonrpc_encoding
            (obj1 (req "result" result_encoding)))
      (function Ok v -> Some ((), v) | _ -> None)
      (fun ((), v) -> Ok v) ;
    case (merge_objs jsonrpc_encoding
            (obj1 (req "error" error_encoding)))
      (function Error e -> Some ((), e) | _ -> None)
      (fun ((), v) -> Error v)
  ]

type balance = {
  balance: int64;
  nonce: int64;
}

let balance_res_encoding =
  let open Json_encoding in
  conv
  (fun _ -> assert false)
  (fun (balance, nonce) -> { balance; nonce })
    (obj2
       (req "balance" int64str_encoding)
       (req "nonce" int53))

let getBalance ?(network=`Mainnet) { Bech32.Segwit.prog; _ } =
  let `Hex prog_hex = Hex.of_string prog in
  let msg = create_msg ~params:[prog_hex] "GetBalance" in
  let enc = msg_encoding Json_encoding.string in
  post_json ~params:(enc, msg)
    (result_encoding balance_res_encoding) (uri_of_network network)

type tx_result = {
  info: string ;
  txID: Bigstring.t ;
}

let tx_result_encoding =
  let open Json_encoding in
  conv
    (fun { info ; txID } -> (info, txID))
    (fun (info, txID) -> { info ; txID })
    (obj2
       (req "Info" string)
       (req "TranID" bs_hex_encoding))

let createTransaction tx =
  let ctx = Context.create () in
  let msg = create_msg ~params:[tx] "CreateTransaction" in
  let enc = msg_encoding (signed_encoding ctx) in
  post_json ~params:(enc, msg)
    (result_encoding tx_result_encoding) (uri_of_network (fst tx).network)
