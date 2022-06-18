#load @".paket/load/main.group.fsx"

open Xunit
open Swensen.Unquote
open System.Text
open System.Collections.Generic
open System
open System.Security.Cryptography

let newReadableId min max () =
    if max < min then
        failwith $"Min value %i{min} must be lesser than Max value %i{max}"

    let sources =
        [ Guid.NewGuid().ToString() ]

    let sb = StringBuilder()

    let b64StrValue =
        sources
        |> List.fold (fun (s: StringBuilder) t -> s.Append(t)) sb
        |> fun r ->
            r
                .Replace('+', '-')
                .Replace('/', '_')
                .Replace('=', '$')
        |> fun r -> r.ToString()

    let result = System.Convert.ToBase64String(Encoding.UTF8.GetBytes(b64StrValue))
    let resultMaxLength = result.Length
    let possibleSizesOfId =
        [ min..max ]
        |> List.sortBy (fun _ -> RandomNumberGenerator.GetInt32(10000))

    let result =
        let mutable r = String.Empty

        let mutable ct = true
        while ct do
            let output = RandomNumberGenerator.GetInt32(resultMaxLength)
            let p1 =
                possibleSizesOfId
                |> Seq.where (fun e -> (output + e) < resultMaxLength)
                |> Seq.map(fun v -> result.Substring(output,v))
                |> Seq.toArray
            
            let p2 =
                possibleSizesOfId
                |> Seq.where(fun e -> (output - e) > 0)
                |> Seq.map (fun v -> result.Substring(output - v, v))
                |> Seq.toArray    

            let o =
                [|
                    p1
                    p2
                |] 
                |> Array.concat 
                |> Array.sortBy (fun _ -> RandomNumberGenerator.GetInt32(resultMaxLength)) 

            if o.Length > 0 then
                r <-  o |> Array.head 
                ct <- false
        r
    result.ToString()
        
    

let newReadableIdWithMemory (memory: HashSet<string>) min max () =
    let newId = newReadableId min max

    let rec doTheJob times =
        // if times > 10 then
        //     printfn $"Cannot find unique id after %i{times - 1} iterations."

        let id = newId ()

        if memory.Add id |> not then
            doTheJob (times + 1)
        else
            // if times > 10 then printfn $"Unique id found after {times} times."
            id

    doTheJob 0


[<Fact>]
let test f nb =
    let results =
        [| for i in 1..nb -> i |]
        |> Array.map (fun _ -> f ())

    let uniqueResults = results |> Array.distinct
    let percentOfNonUniqueIds = float (nb - uniqueResults.Length) / (float nb)  * 100.0

    test <@ percentOfNonUniqueIds = 0 @>

let memoizedNewId = fun m -> newReadableIdWithMemory m 4 9

let nId = memoizedNewId (HashSet())


let checkNbIdLength (s: string seq) x =
    s
    |> Seq.where (fun i -> i.Length = x)
    |> Seq.length

let results =
    [| for i in 1..1000000 -> i |]
    |> Array.map (fun _ -> nId ())

// [ 4..10 ]
// |> Seq.map (fun i -> i, checkNbIdLength results i)
// |> Seq.iter (fun (l, r) -> printfn $"%i{l} -> %i{r}")

// let newId = newReadableId  4 6
// test newId 100
// test newId 1000
// test newId 10000
// test newId 100000
// test newId 1000000
// test newId 10000000

// let mNewId = fun m -> newReadableIdWithMemory m 4 10
// test (mNewId (HashSet())) 100
// test (mNewId (HashSet())) 1000
// test (mNewId (HashSet())) 10000
// test (mNewId (HashSet())) 100000
// test (mNewId (HashSet())) 1000000
// test (mNewId (HashSet())) 10000000

    
let specialGroup n (input: 'a [])  =
    [|for e in 0..(input.Length-n) -> input |> Array.removeManyAt e n |> Array.take n|]

specialGroup 4 results 
// |> Array.map ( fun items -> (items |> Array.distinct).Length = items.Length)
// |> Array.where ( fun i -> i = false)
// |> Array.length