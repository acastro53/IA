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
    //costo por accion que tiene el problema
    let costo _=1.0
    //identifica si se llego al estado meta retorna falos si se encuentra almenos un 0 y verdadero si no
    let meta estado =
        estado |> List.forall (fun lista -> not (List.contains 0 lista))

    let generateSuccessors (node:estado) =
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



