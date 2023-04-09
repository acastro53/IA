open busqueda

let estado = [
    [0; 6; 0; 1; 0; 4; 0; 5; 0];
        [0; 0; 8; 3; 0; 5; 6; 0; 0];
        [2; 0; 0; 0; 0; 0; 0; 0; 1];
        [8; 0; 0; 4; 0; 7; 0; 0; 6];
        [0; 0; 6; 0; 0; 0; 3; 0; 0];
        [7; 0; 0; 9; 0; 1; 0; 0; 4];
        [5; 0; 0; 0; 0; 0; 0; 0; 2];
        [0; 0; 7; 2; 0; 6; 9; 0; 0];
        [0; 4; 0; 5; 0; 8; 0; 7; 0]
]
let key n = n.estado 
let t0=System.DateTime.UtcNow //registra el tiempo actual

let dip = 999999999
match Capitulo3.busquedaGrafo key BFS.estrategia (sudoku.problema estado) with
| Some n -> let sol = Capitulo3.acciones n
            printfn "solucion con BFS: %A" sol
| None -> printfn "no hay solucion"


let delta=System.DateTime.UtcNow - t0
printf "Tiempo trascurrido %A "delta

(*

        
        BFS.estrategia (sudoku.problema estado)with
| Some n->
    printf "solucion %A " n
| None -> printf "Solucion no encontrada"
*)


