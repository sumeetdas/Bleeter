module List

// https://stackoverflow.com/a/70053799/1583813
let updateAt (elemFindFunc: 'a -> bool) (newElem: 'a) (source: 'a list) : 'a list =
    source
    |> List.map
        (fun elem ->
            let foundElem = elemFindFunc elem
            if foundElem then newElem else elem)

// https://stackoverflow.com/a/2889972/1583813
let removeBy (elemRemoveFunc: 'a -> bool) (source: 'a list) : 'a list =
    source
    |> List.map (fun elem -> ((elemRemoveFunc elem), elem))
    // keep elems to include in the list, i.e. remove = false
    |> List.filter (fun (remove, _) -> not remove)
    |> List.map snd

// https://stackoverflow.com/a/45523281/1583813
let removeAt2 (idx: int) (source: 'a list) : 'a list =
    source
    |> List.indexed
    |> List.filter (fun (id, _) -> id <> idx)
    |> List.map (fun (_, elem) -> elem)
