namespace Busqueda
module Sudoku=
    
    //tipo de estado
    type estado=list<list<int>>
    //estado inicial del problema
    let inicio=[[2;5;0;0;3;0;9;0;1];
                        [0;1;0;0;0;4;0;0;0];
                        [4;0;7;0;0;0;2;0;8];
                        [0;0;5;2;0;0;0;0;0];
                        [0;0;0;0;9;8;1;0;0];
                        [0;4;0;0;0;3;0;0;0];
                        [0;0;0;3;6;0;0;7;2];
                        [0;7;0;0;0;0;0;0;3];
                        [9;0;3;0;0;0;6;0;4]]
    //estado meta o solucion del problema
    let estado_meta=[[2;5;8;7;3;6;9;4;1];
                      [6;1;9;8;2;4;3;5;7];
                      [4;3;7;9;1;5;2;6;8];
                      [3;9;5;2;7;1;4;8;6];
                      [7;6;2;4;9;8;1;3;5];
                      [8;4;1;6;5;3;7;2;9];
                      [1;8;4;3;6;9;5;7;2];
                      [5;7;6;1;4;2;8;9;3];
                      [9;2;3;5;8;7;6;1;4]]
    //costo por accion que tiene el problema
    let costo _ _ _=1.0
    //identifica si se llego al estado meta
    let meta estado=
        List.map2(fun x y ->(x,y))
            estado_meta estado
        |> List.forall(fun(x,y)->x=y)
    //encuentra un valor espesifico en el arreglo de estado
    let cero estado=
        List.findIndex(fun x->x=0) estado
    
    let generateSuccessors (node:nodo<estado>) =
        let successors = ref []
        for row in 0..8 do
            for col in 0..8 do
                if node.board.[row, col] = 0 then
                    let possibleValues = 
                        [1..9] |> List.filter (fun n -> not (Array.exists ((=) n) node.board.[row, ..] )) 
                               |> List.filter (fun n -> not (Array.exists ((=) n) node.board.[.., col] )) 
                               |> List.filter (fun n -> not (Array.exists ((=) n) (node.board.[(row/3)*3..(row/3)*3+2, (col/3)*3..(col/3)*3+2] |> Array.concat)))
                    if possibleValues = [] then
                        yield { node with board = Array.copy node.board }
                    else
                        let newNode = { node with board = Array.copy node.board; nextValue = (row, col, possibleValues) :: node.nextValue }
                        for value in possibleValues do
                            let newBoard = Array.copy node.board
                            newBoard.[row, col] <- value
                            successors := { board = newBoard; nextValue = [] } :: !successors
                        yield newNode
        yield! !successors

    let problema estado=
            {
                inicio      =estado
                sucesores   =generateSuccessors
                meta        =meta
                costo       =costo
            }



