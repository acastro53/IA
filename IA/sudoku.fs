namespace busqueda

module sudoku =
    type accion =
        | Uno
        | Dos
        | Tres
        | Cuatro 
        | Cinco
        | Seis
        | Siete
        | Ocho 
        | Nueve
    type estado = list<list<int>*list<int>*list<int>*list<int>*list<int>*list<int>*list<int>*list<int>*list<int>>

    let inicio =   [[0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],

                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],

                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0]]

    let goal = [    [1;2;3  ;4;5;6; 7;8;9],
                    [4;5;6  ;7;8;9; 1;2;3],
                    [7;8;9  ;1;2;3; 4;5;6],

                    [2;3;1 ;6;4;5; 9;7;8],
                    [5;6;4 ;9;7;8; 3;1;2],
                    [8;9;7 ;3;1;2; 6;4;5],

                    [3;1;2 ;5;6;4; 8;9;7],
                    [6;4;5 ;8;9;7; 2;3;1],
                    [9;7;8 ;2;3;1; 5;6;4]]


    let sucesor i accion (estado : estado) =
        let swap i j =
            estado
            |> List.mapi (fun indx x ->
                    if indx = i then
                        List.item j estado
                    elif indx = j then
                        List.item i estado
                    else 
                        x
                )
        match accion with
        | Uno -> if i % 3 <> 0
                    then Some (accion, swap i (i-1))
                    else None
        | Dos -> if i % 3 <> 2
                    then Some (accion, swap i (i+1))
                    else None
        | Tres -> if i > 2
                    then Some (accion, swap i (i-3))
                    else None
        | Cuatro-> if i < 6
                    then Some (accion, swap i (i+3))
                    else None

(*


    let meta estado =
        List.map2 (fun x y -> (x,y)) goal estado
        |> List.forall (fun (x,y) -> x = y)

    let cero estado =
        List.findIndex (fun x -> x = 0) estado

    let sucesor i accion (estado : estado) =
        let swap i j =
            estado
            |> List.mapi (fun indx x ->
                    if indx = i then
                        List.item j estado
                    elif indx = j then
                        List.item i estado
                    else 
                        x
                )
        match accion with
        | Left -> if i % 3 <> 0
                    then Some (accion, swap i (i-1))
                    else None
        | Right -> if i % 3 <> 2
                    then Some (accion, swap i (i+1))
                    else None
        | Up -> if i > 2
                    then Some (accion, swap i (i-3))
                    else None
        | Down -> if i < 6
                    then Some (accion, swap i (i+3))
                    else None
    let sucesores estado =
        let indice = cero estado
        [
            sucesor indice Left estado
            sucesor indice Right estado
            sucesor indice Up estado
            sucesor indice Down estado
        ]   |> List.choose id

    let problema estado = {
        inicio = estado
        sucesores = sucesores
        meta = meta
        costo = costo
    }

    let h1 nodo = 
        List.zip goal nodo.estado
        |> List.sumBy (fun (x,y) -> if x <> y && x <> 0 then 1.0 else 0.0)
*)