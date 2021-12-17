module TezFs
open System
open System.ComponentModel
open System.Dynamic
open System.Text.Json
open System.Threading.Tasks
open Netezos
open Netezos.Contracts
open Netezos.Encoding
open Netezos.Forging
open Netezos.Forging.Models
open Netezos.Keys
open Netezos.Rpc
open Netezos.Rpc
open Newtonsoft.Json
open Newtonsoft.Json.Linq
//open FSharp.Interop.Dynamic
open System.Text
let x : Signature = failwith ""

let rec private tails = function [] -> [] | (x::xs) as vs -> let rest = tails xs in if rest = [[]] then [vs] else vs::rest
let rec private inits = function [] -> [] | xs -> let init = List.take (xs.Length - 1) xs in let rest = inits init in (if rest = [[]] then [xs] else xs::rest)
let private sublists x = x |> (tails >> List.collect (List.tail << inits))
let private substrings x = x |> (List.ofSeq >> sublists >> List.map (fun x -> new string(List.toArray x)))

let random_account() = Key()
let acc_base58 = Key.FromBase58
let florence_rpc = new TezosRpc("https://florencenet.api.tez.ie") 
let test_key = Key.FromBase58("edskRsRU7Kv2QrBFiUAmfnXwy9Fxc4M3F9CjYacndGxYzJW3SiLYXVQRaHrDsPXDA7uEkoz3UQ3sGxPhNd1J92SNm6paip8F5A")
let test_pub_1 = "tz1abwEWbK7NZX1GKvsso9B3g3z4kRVSA9ri"
let signature = test_key.Sign "Hello this is me"
type Key with
    member this.pub = this.PubKey
    member this.pub_hash = this.PubKey.Address
    member this.sign (x:string) = x, this.Sign x

let ONE_TEZ = 1_000_000L
let reveal_txn (key:Key) =
    let t = new RevealContent()
    t.Source <- key.PubKey.Address
    t.PublicKey <- key.PubKey.GetBase58()
    t.GasLimit <- 1500
    t.Fee <- 1000L // 0.001 tez
    t
let send_tez_txn (key:Key) destination amount =
    let t = new TransactionContent() in
    t.Source <- key.PubKey.Address
    t.Amount <- amount
    t.Destination <- destination
    t.GasLimit <- 1500
    t.Fee <- 1_000L // 0.001 tez
    t
let get_head (rpc:TezosRpc) =  rpc.Blocks.Head.Hash.GetAsync<string>() |> Async.AwaitTask
let get_counter (rpc:TezosRpc) address = rpc.Blocks.Head.Context.Contracts.[address].Counter.GetAsync<int>() |> Async.AwaitTask
let forge head content = (new LocalForge()).ForgeOperationGroupAsync(head, content) |> Async.AwaitTask
let inject (rpc:TezosRpc) (msg_and_sig:byte[]) = rpc.Inject.Operation.PostAsync(msg_and_sig, false, Chain.Main)  |> Async.AwaitTask
let call_entrypoint_txn (rpc:TezosRpc) (key:Key) contract_address entrypoint (arguments:'a) =
    async{
        let! code = rpc.Blocks.Head.Context.Contracts.[contract_address].Script.GetCodeAsync() |> Async.AwaitTask
        let cs = new ContractScript(code)
//        let schemaString = cs.Entrypoints.[entrypoint].Humanize();
        let param = cs.BuildParameter(entrypoint, arguments )
        let tx = new TransactionContent()
        tx.Source <- key.PubKey.Address 
        tx.GasLimit <- 100_000 
        tx.StorageLimit <- 1000 
        tx.Fee <- 100_000L 
        tx.Destination <- contract_address
        tx.Parameters <- new Parameters()
        tx.Parameters.Entrypoint <- entrypoint
        tx.Parameters.Value <- param
        return tx
    }
    
type tx = {to_:string; token_id:int; amount:int} 
type transfer = {from_ : string; txs: tx list}  
let send_batch_txns (rpc:TezosRpc) (key:Key) txs =
    async {
        let address = key.PubKey.Address 
        let! head = get_head rpc
        let! counter = get_counter rpc address 
        let content : ManagerOperationContent[] = txs |> Array.mapi (fun i (t:TransactionContent) ->  let _ = t.Counter <- counter + i + 1 in t :> ManagerOperationContent)
        let! bytes = forge head content
        // sign the operation bytes
        let signature: Signature = (key.SignOperation(bytes))
        let signature_bytes = signature.ToBytes()
        let msg_and_sig = Array.concat[bytes;signature_bytes]
        // inject the operation and get its id (operation hash)
        let! result = inject rpc msg_and_sig
        return result
    } 
let test_send_tez key = send_tez_txn key test_pub_1 ONE_TEZ 
let transfer_rex rpc key reckless_contract = call_entrypoint_txn rpc key reckless_contract "transfer" [{from_ = key.PubKey.Address; txs = [{to_ = test_pub_1; token_id=0; amount=5}]}] 
let reckless_contract = "KT1Mw7E46UuQk62imBoYzTSUCpuz3LLXZ7qo"
