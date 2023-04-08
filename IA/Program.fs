open busqueda

let estado = [
    [5;3;0;0;7;0;0;0;0];
    [6;0;0;1;9;5;0;0;0];
    [0;9;8;0;0;0;0;6;0];
    [8;0;0;0;6;0;0;0;3];
    [4;0;0;8;0;3;0;0;1];
    [7;0;0;0;2;0;0;0;6];
    [0;6;0;0;0;0;2;8;0];
    [0;0;0;4;1;9;0;0;5];
    [0;0;0;0;8;0;0;7;9];
]
let key n = n.estado 
let t0=System.DateTime.UtcNow //registra el tiempo actual

let dip = 100000000
match Capitulo3.busquedaGrafo BFS.key BFS.estrategia (sudoku.problema estado) with
| Some n -> let sol = Capitulo3.acciones n
            printfn "solucion con BFS: %A" sol
| None -> printfn "no hay solucion"




let delta=System.DateTime.UtcNow - t0
printf "Tiempo trascurrido %A "delta

(*
    match Capitulo3.busquedaArbol DFSLi.estrategia (sudoku.problema estado) with
| Some n -> let sol = Capitulo3.acciones n
            printfn "solucion con BFS: %A" sol
| None -> printfn "no hay solucion"
    match  Capitulo3.busquedaArbol
        
        BFS.estrategia (sudoku.problema estado)with
| Some n->
    printf "solucion %A " n
| None -> printf "Solucion no encontrada"
*)


