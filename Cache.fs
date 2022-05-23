module Cache

open System.Collections.Generic

// Реализация кэширования функций

let cache f =
    let dict = new Dictionary<_,_>()
    fun n ->
        if dict.ContainsKey(n) then dict.[n] else
            let r = f n
            dict.[n] <- r
            r