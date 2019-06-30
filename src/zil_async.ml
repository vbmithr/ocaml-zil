open Fastrest
open Zil

let mainnet =
  Uri.make ~scheme:"https" ~host:"api.zilliqa.com" ()

let testnet =
  Uri.make ~scheme:"https" ~host:"dev-api.zilliqa.com" ()

let uri_of_network = function
  | `Mainnet -> mainnet
  | `Testnet -> testnet

let params meth params = [
  "id", ["1"];
  "jsonrpc", ["2.0"];
  "method", [meth];
  "params", params;
]

let json_of_params (params : Fastrest.params) : Ezjsonm.t =
  `O [
    "id", `String (String.concat "" (List.assoc "id" params)) ;
    "jsonrpc", `String (String.concat "" (List.assoc "jsonrpc" params)) ;
    "method", `String (String.concat "" (List.assoc "method" params)) ;
    "params", `A (List.map (fun s -> `String s) (List.assoc "params" params)) ;
  ]

let result_encoding result_encoding =
  let open Json_encoding in
  conv
    (fun _ -> assert false)
    (fun ((), (), v) -> Ok v)
    (obj3
       (req "id" (constant "1"))
       (req "jsonrpc" (constant "2.0"))
       (req "result" result_encoding))

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
  let params = params "GetBalance" [prog_hex] in
  post_json ~params ~json_of_params
    (result_encoding balance_res_encoding) (uri_of_network network)
