open busqueda

let estado = [1;2;4;0;5;3;7;8;6]


match Capitulo3.busquedaGrafo
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
                [1;2;4; 0;5;3; 7;8;6]]

