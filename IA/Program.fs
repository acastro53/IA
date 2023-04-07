open busqueda

let estado=[[2;5;0;0;3;0;9;0;1];
                        [0;1;0;0;0;4;0;0;0];
                        [4;0;7;0;0;0;2;0;8];
                        [0;0;5;2;0;0;0;0;0];
                        [0;0;0;0;9;8;1;0;0];
                        [0;4;0;0;0;3;0;0;0];
                        [0;0;0;3;6;0;0;7;2];
                        [0;7;0;0;0;0;0;0;3];
                        [9;0;3;0;0;0;6;0;4]]

match Capitulo3.busquedaArbol BFS.estrategia (sudoku.problema estado) with
| Some n -> let sol = Capitulo3.acciones n
            printfn "solucion con BFS: %A" sol
| None -> printfn "no hay solucion"


