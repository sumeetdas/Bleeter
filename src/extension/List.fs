module List

let updateAt (elemFindFunc: 'a -> bool) (newElem: 'a) (source: 'a list): 'a list = 
    source |> List.map (fun elem -> 
        let foundElem = elemFindFunc elem
        if foundElem then newElem else elem
    )
    

