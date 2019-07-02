open Core
open Async
(* open Libsecp256k1.External *)
open Alcotest

open Zil_async

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let wrap_request ?(speed=`Quick) n service =
  Alcotest_async.test_case n speed begin fun () ->
    Fastrest.request service >>= function
    | Ok _v -> Deferred.unit
    | Error _ -> failwith ""
  end

(* let sk, addr =
 *   let ctx = Context.create () in
 *   let sk = Key.read_sk_exn ctx (Monocypher.Rand.gen 32) in
 *   let pk = Key.neuterize_exn ctx sk in
 *   let pk_bytes = Key.to_bytes ~compress:true ctx pk in
 *   let pkh = Digestif.SHA256.(to_raw_string (digest_bigstring pk_bytes)) in
 *   sk,
 *   Bech32.Segwit.(create (module Zil) (String.subo ~pos:12 ~len:20 pkh)) *)

let addr =
  let open Bech32.Segwit in
  decode_exn (module Zil)
    "zil1u7dazlsvzxuxfe2m9w3eteg838mr34kccgcc9v"

let bech32 () =
  let bech32 = "zil1zd6y8tf9g2huqckfhepcfa7y3yftf67zlmgec6" in
  let addr_hex  = `Hex "137443AD2542AfC062c9BE4384F7c48912B4eBC2" in
  let addr = Hex.to_string addr_hex in
  let addr_segwit = Bech32.Segwit.(create (module Zil) addr) in
  let addr_segwit_str = Bech32.Segwit.encode_exn addr_segwit in
  check string "bech32" bech32 addr_segwit_str ;
  let { Bech32.Segwit.prog ; _ } =
    Bech32.Segwit.(decode_exn (module Zil) bech32) in
  Logs.app (fun m -> m "%s" addr_segwit_str) ;
  check string "bech32 to byte20" addr prog ;
  ()

let checks = [
  test_case "Bech32" `Quick bech32
]

let rest = [
  wrap_request "getBalance" (getBalance ~network:`Testnet addr) ;
]

let () =
  Alcotest.run "Zilliqa" [
    "checks", checks ;
    "rest", rest ;
  ]
