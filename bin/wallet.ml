open Core
open Async

let getAddr i =
  let h = Hidapi.open_id_exn ~vendor_id:0x2c97 ~product_id:0x0001 in
  let pk, addr =
    Ledgerwallet_zil.get_pk ~display_addr:true h (Int32.of_int_exn i) in
  let pk_hex = Hex.of_cstruct pk in
  Logs.app (fun m -> m "Public key:\t%a" Hex.pp pk_hex) ;
  Logs.app (fun m -> m "Address:\t%a" Bech32.Segwit.pp addr) ;
  Deferred.unit

let getBalance testnet addr =
  let network = if  testnet then `Testnet else `Mainnet in
  let srv = Zil_async.getBalance
    ~network
    Bech32.Segwit.(decode_exn (module Zil) addr) in
  Fastrest.request srv >>= function
  | Error _ -> failwith "getBalance"
  | Ok { balance; nonce } ->
    Logs.app (fun m -> m "Balance:\t%Ld" balance) ;
    Logs.app (fun m -> m "Nonce:\t%Ld" nonce) ;
    Deferred.unit

let addrCmd =
  Command.async
    ~summary:"Get Zilliqa address from Ledger" begin
    let open Command.Let_syntax in
    [%map_open
      let i = anon ("N" %: int)
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        getAddr i
    ] end

let balanceCmd =
  Command.async
    ~summary:"Get Zilliqa balance for address" begin
    let open Command.Let_syntax in
    [%map_open
      let testnet = flag "testnet" ~doc:" Use testnet" no_arg
      and addr = anon ("address" %: string)
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        getBalance testnet addr
    ] end

let () =
  Command.group ~summary:"Zilliqa Wallet" [
    "addr", addrCmd ;
    "balance", balanceCmd ;
  ] |> Command.run
