open busqueda

let estado = [
    [2;5;0;0;3;0;9;0;1];
[0;1;0;0;0;4;0;0;0];
[4;0;7;0;0;0;2;0;8];
[0;0;5;2;0;0;0;0;0];
[0;0;0;0;9;8;1;0;0];
[0;4;0;0;0;3;0;0;0];
[0;0;0;3;6;0;0;7;2];
[0;7;0;0;0;0;0;0;3];
[9;0;3;0;0;0;6;0;4];
]
let key n = n.estado 
let t0=System.DateTime.UtcNow //registra el tiempo actual

let printSudoku (nodofinal: int list list) =
    for i in [0..8] do
        if i % 3 = 0 && i <> 0 then printfn "------+-------+------"
        for j in [0..8] do
            if j % 3 = 0 && j <> 0 then printf "| "
            printf "%d " nodofinal.[i].[j]
        printfn ""

let dip = 999999999
match Capitulo3.busquedaGrafo key BFS.estrategia (sudoku.problema estado) with
| Some n -> let sol = Capitulo3.acciones n
            printfn "solucion : %A" sol
            printf "Entrada :\n"
            printSudoku estado
            printf "Solucion:\n"
            printSudoku n.estado
| None -> printfn "no hay solucion"


let delta=System.DateTime.UtcNow - t0
printf "Tiempo trascurrido %A "delta
printf "Numero de nodos: %d" Sudoku.contadorNodo
(*

        
        BFS.estrategia (sudoku.problema estado)with
| Some n->
    printf "solucion %A " n
| None -> printf "Solucion no encontrada"
*)


