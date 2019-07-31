open Core
open Async
open Libsecp256k1.External

let getAddr i =
  let h = Hidapi.open_id_exn ~vendor_id:0x2c97 ~product_id:0x0001 in
  let pk, addr =
    Ledgerwallet_zil.get_pk ~display_addr:true h (Int32.of_int_exn i) in
  let pk_hex = Hex.of_cstruct pk in
  Logs.app (fun m -> m "Public key:\t%a" Hex.pp pk_hex) ;
  Logs.app (fun m -> m "Address:\t%a" Bech32.Segwit.pp addr) ;
  Deferred.unit

let getBalance testnet addr =
  let network = if testnet then `Testnet else `Mainnet in
  let srv = Zil_async.getBalance
    ~network
    Bech32.Segwit.(decode_exn (module Zil) addr) in
  Fastrest.request srv >>= function
  | Error _ -> failwith "getBalance"
  | Ok { balance; nonce } ->
    Logs.app (fun m -> m "Balance:\t%.2f" (Int64.to_float balance *. 1e-12)) ;
    Logs.app (fun m -> m "Nonce:\t%Ld" nonce) ;
    Deferred.unit

let transfer testnet keyId dstAddr qty =
  let ctx = Context.create () in
  let network = if testnet then `Testnet else `Mainnet in
  let h = Hidapi.open_id_exn ~vendor_id:0x2c97 ~product_id:0x0001 in
  let pk, srcAddr =
    Ledgerwallet_zil.get_pk ~display_addr:true h (Int32.of_int_exn keyId) in
  let srv = Zil_async.getBalance ~network srcAddr in
  Fastrest.request srv >>= function
  | Error _ -> failwith "GetBalance"
  | Ok { balance = _ ; nonce } ->
    let senderpubkey = Key.read_pk_exn ctx (Cstruct.to_bigarray pk) in
    let amount = Int64.of_float (qty *. 1e12) in
    let tx = Zil.simple_tx
      ~network ~nonce:(Int64.succ nonce)
      ~toaddr:dstAddr ~senderpubkey ~amount () in
    let buf = Zil.write ctx tx in
    let serialized =
      Cstruct.of_bigarray (Faraday.serialize_to_bigstring buf) in
    Logs.debug (fun m -> m "Sign a tx of length %d" (Cstruct.len serialized)) ;
    let signature =
      Ledgerwallet_zil.sign_txn ~pp:Format.err_formatter h (Int32.of_int_exn keyId) serialized in
    Logs.debug (fun m -> m "Got a signature of length %d" (Cstruct.len signature)) ;
    let srv =  Zil_async.createTransaction (tx, Cstruct.to_bigarray signature) in
    Fastrest.request srv >>= function
    | Error _ -> failwith "CreateTransaction"
    | Ok { info; txID } ->
      Logs.app (fun m -> m "info:\t%s" info) ;
      Logs.app (fun m -> m "txID:\t%a" Hex.pp (Hex.of_bigstring txID)) ;
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

let transferCmd =
  let addr = Command.Arg_type.create (Bech32.Segwit.(decode_exn (module Zil))) in
  Command.async
    ~summary:"Transfer from Ledger" begin
    let open Command.Let_syntax in
    [%map_open
      let testnet = flag "testnet" ~doc:" Use testnet" no_arg
      and keyId = anon ("id" %: int)
      and dstAddr = anon ("addr" %: addr)
      and qty = anon ("qty" %: float)
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        transfer testnet keyId dstAddr qty
    ] end

let () =
  Command.group ~summary:"Zilliqa Wallet" [
    "addr", addrCmd ;
    "balance", balanceCmd ;
    "transfer", transferCmd ;
  ] |> Command.run
