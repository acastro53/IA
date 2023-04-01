open busqueda
open Microsoft.FSharp.Core
   
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
                [1;2;3; 4;5;6; 7;8;9];
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

//funcion buscador booleano
let BuscadorCiclo  n (matriz: int list list  ) =
    for i in 0 .. List.length matriz - 1 do
        for j in 0 .. List.length (List.item i matriz) - 1 do
            (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz ) &&  (Tienecolumna2 inicio  n j) && (Tienefila2 inicio  n j) 
                    
let BuscadorSimple i j  n (matriz: int list list  ) =
    (Tienesubgrupo (3*((i)/3)) (3*((j)/3)) n matriz ) &&  (Tienecolumna2 inicio  n j) && (Tienefila2 inicio  n j)


           

Buscador 1 inicio


   






(* let estado = [1;2;4;0;5;3;7;8;6]
    +match Capitulo3.busquedaGrafo
    (astar.key OchoCasillas.h1)
    (astar.estrategia OchoCasillas.h1)
    (OchoCasillas.problema estado) with
| Some n -> let sol = Capitulo3.acciones n
            printfn "solucion con greedy: %A" sol
            printfn "profundidad: %i" (List.length sol)
| None -> printfn "no hay solucion"

let estado2 =    [[1;2;4 ;0;5;3; 7;8;6],
                [1;2;4; 0;5;3; 7;8;6],
                [1;2;4; 0;5;3; 7;8;6],

                [1;2;4; 0;5;3; 7;8;6],
                [1;2;4 ;0;5;3; 7;8;6],
                [1;2;4; 0;5;3; 7;8;6],

                [1;2;4; 0;5;3; 7;8;6],
                [1;2;4; 0;5;3; 7;8;6],
                [1;2;4; 0;5;3; 7;8;6]]*)

