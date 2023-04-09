
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
        type estado = list<list<int>>
        let costo _ _ _ = 1.0
        let mutable contadorNodo = 0



   



(*
    let meta2 estado = 
            not List.exists (fun subList - > List.exists ((=) 0) subList) estado 
    *)
    let meta estado =
        
        not (List.exists (fun subList -> List.exists ((=) 0) subList) estado)
           





    // funcion tiene Fila, verifica si esta en la lista
    let Tienefila estado x n  = 
        if not (List.contains x (List.item  (n) estado )) then
            printfn "El número %d no está en la fila %d." x n
            //false
        else
            printfn "El número %d está en la fila %d." x n 
            //true
    // funcion tiene fila con booleanos
    let Tienefila2 estado x n  = 
        if not (List.contains x (List.item  (n) estado )) then
            printfn "El número %d no está en la fila %d." x n
            true
        else
            printfn "El número %d  está en la fila %d." x n 
            false




    // funcion tiene columna, verifica si esta en la columna
    let Tienecolumna estado x n =
        let transpuesta  = List.transpose estado   
        if not (List.contains x (List.item  (n) transpuesta )) then
            printfn "El número %d no está en la columna %d." x n
            //false
        else
            printfn "El número %d está en la columna %d." x n 
            //true
    // funcion tiene columna con booleanos
    let Tienecolumna2 estado x n =
        let transpuesta  = List.transpose estado   
        if not (List.contains x (List.item  (n) transpuesta )) then
            printfn "El número %d no está en la columna %d." x n
            true
        else
            printfn "El número %d está en la columna %d." x n
            false


     let Tienesubgrupo2 x y n (lst:int list list) =
        [ for i in y..(y+2) ->
            List.take 3 (List.skip x (List.item  i lst ))
        ] 
        |> List.concat
        |> printfn "%A" 



    // funcion tiene columna subgrupo , hace un grupo de 3 *3 y verifica si esta ahi  verifica si esta en la columna
    let Tienesubgrupo x y n (lst:int list list) =
        Tienesubgrupo2 x y n 
        [ for i in y..(y+2) ->
            List.take 3 (List.skip x (List.item  i lst ))
        ] 
        |> List.concat
        |>  List.contains n 
    // funcion que imprime la lista del subgrupo


    
    

    

        

   
        

    

    // funcion buscador verifica si hay elementos en la lista 
    let Buscador n (matriz: int list list  ) =
        for i in 0 .. List.length matriz - 1 do
            for j in 0 .. List.length (List.item i matriz) - 1 do
                printfn "El elemento %d,%d es %d" i j (List.item i (List.item j matriz))
                Tienecolumna matriz n j
                Tienefila matriz n i
                if (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz)then 
                    printfn "true. %d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz      
                else 
                    printfn "false.%d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz

    let BuscadorSimple i j  n (matriz: int list list  ) =
        let result=((not (Tienesubgrupo (3*((j)/3)) (3*((i)/3)) n matriz )) &&   (Tienecolumna2 matriz n j) && (Tienefila2 matriz  n i))
        if result then contadorNodo <- contadorNodo + 1
        result
    //funcion buscador booleano
    let BuscadorCiclo  n (matriz: int list list  ) =
        for i in 0 .. List.length matriz - 1 do
            for j in 0 .. List.length (List.item i matriz) - 1 do
                BuscadorSimple i j n matriz
                
                        


    




   
(*
let findFirstZeroIndex estado =
     estado
    |> List.mapi (fun i lst -> (i, lst))
    |> List.tryPick (fun (i, lst) -> List.tryFindIndex ((=) 0) lst |> Option.map (fun j -> (i, lst, j)))
*)


    let escribirNumero i j num estado =
        estado
        |> List.mapi (fun idx fila ->
            if idx = i then
                fila |> List.mapi (fun idx2 x -> if idx2 = j then num else x)
            else fila)
           
   
            
   
    


    let sucesor (i, j) accion (estado : estado) =
            let escribirNumero i j num estado =
                estado
                |> List.mapi (fun idx fila ->
                    if idx = i then
                        fila |> List.mapi (fun idx2 x -> if idx2 = j then num else x)
                    else fila)
            printfn "  i %i j % i estado  \n %A" i j estado
            match accion with
            | Uno-> if (BuscadorSimple i j 1 estado)
                        then Some (accion, escribirNumero i j 1 estado)
                        else None
            | Dos -> if (BuscadorSimple  i j 2 estado)
                        then Some (accion, escribirNumero i j 2 estado)
                        else None
            | Tres -> if (BuscadorSimple  i j 3 estado)
                        then Some (accion, escribirNumero i j 3 estado)
                        else None
            | Cuatro-> if (BuscadorSimple  i j 4 estado)
                        then Some (accion, escribirNumero i j 4 estado)
                        else None
            | Cinco -> if (BuscadorSimple  i j 5 estado)
                        then Some (accion, escribirNumero i j 5 estado)
                        else None
            | Seis -> if (BuscadorSimple  i j 6   estado)
                        then Some (accion, escribirNumero i j 6 estado)
                        else None
            | Siete-> if (BuscadorSimple  i j 7 estado)
                        then Some (accion, escribirNumero i j 7 estado)
                        else None
            | Ocho -> if (BuscadorSimple  i j 8 estado)
                        then Some (accion, escribirNumero i j 8 estado)
                        else None
            | Nueve  -> if (BuscadorSimple  i j 9 estado)
                        then Some (accion, escribirNumero i j 9 estado ) 
                        else None

    let findFirstZeroIndex estado =
        match estado |> List.mapi (fun i lst -> (i, lst)) |> List.tryPick (fun (i, lst) -> List.tryFindIndex ((=) 0) lst |> Option.map (fun j -> (i, j))) with
        | Some (i, j) -> (i, j)
        | None -> (-1, -1) 
    
    let sucesores estado =
            let indices = findFirstZeroIndex estado
            [
                sucesor indices Uno estado
                sucesor indices Dos estado
                sucesor indices Tres estado
                sucesor indices Cuatro estado
                sucesor indices Cinco estado
                sucesor indices Seis  estado
                sucesor indices Siete  estado
                sucesor indices Ocho  estado
                sucesor indices Nueve estado
            ]   |> List.choose id
        
     let problema estado = {
        inicio = estado
        sucesores = sucesores
        meta = meta
        costo = costo
    }
(*


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
    type estado = list<list<int>>
    
    let inicio =[[1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];

                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;0;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];

                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];]

        let inicio =[[1;2;3; 4;5;6; 7;8;9];
                    [1;2;3; 4;5;6; 7;8;9];
                    [1;2;3; 4;5;6; 7;8;9];

                    [1;2;3; 4;5;6; 7;8;9];
                    [1;2;3; 4;0;6; 7;8;9];
                    [1;2;3; 4;5;6; 7;8;9];

                    [1;2;3; 4;5;6; 7;8;9];
                    [1;2;3; 4;5;6; 7;8;9];
                    [1;2;3; 4;5;6; 7;8;9];]
   




    let meta estado =
            List.exists (fun subList -> List.exists ((=) 0) subList) estado




    // funcion tiene Fila, verifica si esta en la lista
    let Tienefila estado x n  = 
        if not (List.contains x (List.item  (n) estado )) then
            printfn "El número %d no está en la fila %d." x n
            //false
        else
            printfn "El número %d está en la fila %d." x n 
            //true
    // funcion tiene fila con booleanos
    let Tienefila2 estado x n  = 
        if not (List.contains x (List.item  (n) estado )) then
            false
        else
            printfn "El número %d está en la fila %d." x n 
            true
let Buscador n (matriz: int list list  ) =
        for i in 0 .. List.length matriz - 1 do
            for j in 0 .. List.length (List.item i matriz) - 1 do
                printfn "El elemento %d,%d es %d" i j (List.item i (List.item j matriz))
                Tienecolumna inicio  n j
                Tienefila inicio n i
                if (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz)then 
                    printfn "true. %d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz      
                else 
                    printfn "false.%d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz




    // funcion tiene columna, verifica si esta en la columna
    let Tienecolumna estado x n =
        let transpuesta  = List.transpose estado   
        if not (List.contains x (List.item  (n) transpuesta )) then
            printfn "El número %d no está en la columna %d." x n
            //false
        else
            printfn "El número %d está en la columna %d." x n 
            //true
    // funcion tiene columna con booleanos
    let Tienecolumna2 estado x n =
        let transpuesta  = List.transpose estado   
        if not (List.contains x (List.item  (n) transpuesta )) then
            
            false
        else
            true





    // funcion tiene columna subgrupo , hace un grupo de 3 *3 y verifica si esta ahi  verifica si esta en la columna
    let Tienesubgrupo x y n (lst:int list list) =
        [ for i in y..(y+2) ->
            List.take 3 (List.skip x (List.item  i lst ))
        ] 
        |> List.concat
        |>List.contains n
    // funcion que imprime la lista del subgrupo
    let Tienesubgrupo2 x y  (lst:int list list) =
        [ for i in y..(y+2) ->
            List.take 3 (List.skip x (List.item  i lst ))
        ] 
        |> List.concat
        |> printfn "%A" 

    let ceros estado =
        [ for i in 0..8 do
            for j in 0..8 do
                if estado = 0 then yield (i,j) ]
        

    

    // funcion buscador verifica si hay elementos en la lista 
    let Buscador n (matriz: int list list  ) =
        for i in 0 .. List.length matriz - 1 do
            for j in 0 .. List.length (List.item i matriz) - 1 do
                printfn "El elemento %d,%d es %d" i j (List.item i (List.item j matriz))
                Tienecolumna inicio  n j
                Tienefila inicio n i
                if (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz)then 
                    printfn "true. %d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz      
                else 
                    printfn "false.%d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz

    //funcion buscador booleano
    let BuscadorCiclo  n (matriz: int list list  ) =
        for i in 0 .. List.length matriz - 1 do
            for j in 0 .. List.length (List.item i matriz) - 1 do
                (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz ) &&  (Tienecolumna2 inicio  n j) && (Tienefila2 inicio  n j) 
                        


    let BuscadorSimple i j  n (matriz: int list list  ) =
        (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz ) &&  (Tienecolumna2 inicio  n j) && (Tienefila2 inicio  n j)

    let findFirstZeroIndex (listOfLists: int list list) =
        listOfLists
        |> List.tryPick (fun (lst: int list) -> List.tryFindIndex ((=) 0) lst |> Option.map (fun i -> (lst, i)))




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

        *  
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
    type estado = list<list<int>>
    
    let inicio =[[1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];

                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;0;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];

                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];
                [1;2;3; 4;5;6; 7;8;9];]


   




    let meta estado =
            List.exists (fun subList -> List.exists ((=) 0) subList) estado




    // funcion tiene Fila, verifica si esta en la lista
    let Tienefila estado x n  = 
        if not (List.contains x (List.item  (n) estado )) then
            printfn "El número %d no está en la fila %d." x n
            //false
        else
            printfn "El número %d está en la fila %d." x n 
            //true
    // funcion tiene fila con booleanos
    let Tienefila2 estado x n  = 
        if not (List.contains x (List.item  (n) estado )) then
            false
        else
            printfn "El número %d está en la fila %d." x n 
            true




    // funcion tiene columna, verifica si esta en la columna
    let Tienecolumna estado x n =
        let transpuesta  = List.transpose estado   
        if not (List.contains x (List.item  (n) transpuesta )) then
            printfn "El número %d no está en la columna %d." x n
            //false
        else
            printfn "El número %d está en la columna %d." x n 
            //true
    // funcion tiene columna con booleanos
    let Tienecolumna2 estado x n =
        let transpuesta  = List.transpose estado   
        if not (List.contains x (List.item  (n) transpuesta )) then
            
            false
        else
            true





    // funcion tiene columna subgrupo , hace un grupo de 3 *3 y verifica si esta ahi  verifica si esta en la columna
    let Tienesubgrupo x y n (lst:int list list) =
        [ for i in y..(y+2) ->
            List.take 3 (List.skip x (List.item  i lst ))
        ] 
        |> List.concat
        |>List.contains n
    // funcion que imprime la lista del subgrupo
    let Tienesubgrupo2 x y  (lst:int list list) =
        [ for i in y..(y+2) ->
            List.take 3 (List.skip x (List.item  i lst ))
        ] 
        |> List.concat
        |> printfn "%A" 

   
        

    

    // funcion buscador verifica si hay elementos en la lista 
    let Buscador n (matriz: int list list  ) =
        for i in 0 .. List.length matriz - 1 do
            for j in 0 .. List.length (List.item i matriz) - 1 do
                printfn "El elemento %d,%d es %d" i j (List.item i (List.item j matriz))
                Tienecolumna inicio  n j
                Tienefila inicio n i
                if (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz)then 
                    printfn "true. %d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz      
                else 
                    printfn "false.%d %d " (3*((i)/3)) (3*((j)/3)) 
                    Tienesubgrupo2 (3*((i)/3)) (3*((j)/3))  matriz

    let BuscadorSimple i j  n (matriz: int list list  ) =
        (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz ) &&  (Tienecolumna2 inicio  n j) && (Tienefila2 inicio  n j)

    //funcion buscador booleano
    let BuscadorCiclo  n (matriz: int list list  ) =
        for i in 0 .. List.length matriz - 1 do
            for j in 0 .. List.length (List.item i matriz) - 1 do
                BuscadorSimple i j n matriz
                
                        


    




   
(*
let findFirstZeroIndex estado =
     estado
    |> List.mapi (fun i lst -> (i, lst))
    |> List.tryPick (fun (i, lst) -> List.tryFindIndex ((=) 0) lst |> Option.map (fun j -> (i, lst, j)))
*)


let escribirNumero i j num estado =
    estado
    |> List.mapi (fun idx fila ->
        if idx = i then
            fila |> List.mapi (fun idx2 x -> if idx2 = j then num else x)
        else fila)


let sucesor (i, j) accion (estado : estado) =
        let escribirNumero i j num estado =
            estado
            |> List.mapi (fun idx fila ->
                if idx = i then
                    fila |> List.mapi (fun idx2 x -> if idx2 = j then num else x)
                else fila)
        match accion with
        | Uno-> if (BuscadorSimple 1 i j estado)
                    then Some (accion, escribirNumero i j 1 estado)
                    else None
        | Dos -> if (BuscadorSimple 2 i j estado)
                    then Some (accion, escribirNumero i j 2 estado)
                    else None
        | Tres -> if (BuscadorSimple 3 i j estado)
                    then Some (accion, escribirNumero i j 3 estado)
                    else None
        | Cuatro-> if (BuscadorSimple 4 i j estado)
                    then Some (accion, escribirNumero i j 4 estado)
                    else None
        | Cinco -> if (BuscadorSimple 5 i j estado)
                    then Some (accion, escribirNumero i j 5 estado)
                    else None
        | Seis -> if (BuscadorSimple 6 i j estado)
                    then Some (accion, escribirNumero i j 6 estado)
                    else None
        | Siete-> if (BuscadorSimple 7 i j estado)
                    then Some (accion, escribirNumero i j 7 estado)
                    else None
        | Ocho -> if (BuscadorSimple 8 i j estado)
                    then Some (accion, escribirNumero i j 8 estado)
                    else None
        | Nueve  -> if (BuscadorSimple 9 i j estado)
                    then Some (accion, escribirNumero i j 9 estado)
                    else None

let findFirstZeroIndex estado =
    match estado |> List.mapi (fun i lst -> (i, lst)) |> List.tryPick (fun (i, lst) -> List.tryFindIndex ((=) 0) lst |> Option.map (fun j -> (i, j))) with
    | Some (i, j) -> (i, j)
    | None -> (-1, -1)
 
let sucesores estado =
        let indices = findFirstZeroIndex estado
        [
            sucesor indices Uno estado
            sucesor indices Dos estado
            sucesor indices Tres estado
            sucesor indices Cuatro estado
            sucesor indices Cinco estado
            sucesor indices Seis  estado
            sucesor indices Siete  estado
            sucesor indices Ocho  estado
            sucesor indices Nueve estado
        ]   |> List.choose id
 

*)
