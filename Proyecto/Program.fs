open Busqueda
let estado=[[2;5;0;0;3;0;9;0;1];
                        [0;1;0;0;0;4;0;0;0];
                        [4;0;7;0;0;0;2;0;8];
                        [0;0;5;2;0;0;0;0;0];
                        [0;0;0;0;9;8;1;0;0];
                        [0;4;0;0;0;3;0;0;0];
                        [0;0;0;3;6;0;0;7;2];
                        [0;7;0;0;0;0;0;0;3];
                        [9;0;3;0;0;0;6;0;4]]
let t0=System.DateTime.UtcNow //registra el tiempo actual
match Proyec.busquedaGrafo
        BFS.key //necesaria para busqueda grafo
        BFS.estrategia
        (Sudoku.problema estado)with
| Some n->
    printf "solucion %A " n
| None -> printf "Solucion no encontrada"

let delta=System.DateTime.UtcNow - t0
printf "Tiempo trascurrido %A "delta
