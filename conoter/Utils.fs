module Utils 

let swap (a, b) = (b, a)

module Dict =
    open System.Collections.Generic

    type Dictionary<'K, 'V> with
        member x.AsTuples
            with get () = 
                seq { for kvp in x do yield (kvp.Key, kvp.Value) }
           
        member x.Do action =
            x.AsTuples |> Seq.iter action

    let toTuples<'TKey, 'TValue> (d: Dictionary<'TKey, 'TValue>) =
        seq { for kvp in d do yield (kvp.Key, kvp.Value) }

    let iter<'TKey, 'TValue> action (d: Dictionary<'TKey, 'TValue>) =
        d.AsTuples |> Seq.iter action