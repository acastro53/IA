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

    let sucesor (tablero:estado) =
        // Busca la siguiente casilla vacía
        let rec buscar_casilla_vacia fila columna =
            if fila = 9 then None
            elif List.nth (List.nth tablero fila) columna = 0 then Some (fila, columna)
            elif columna = 8 then buscar_casilla_vacia (fila + 1) 0
            else buscar_casilla_vacia fila (columna + 1)
        // Encuentra los valores posibles para la casilla vacía
        let valores_posibles fila columna =
            let fila_valores = List.filter ((<>) 0) (List.nth tablero fila)
            let columna_valores = List.filter ((<>) 0) (List.map (fun r -> List.nth r columna) tablero)
            let caja_fila_inicio = (fila / 3) * 3
            let caja_columna_inicio = (columna / 3) * 3
            let caja_valores = List.filter ((<>) 0) [
                List.nth (List.nth tablero (caja_fila_inicio + i)) (caja_columna_inicio + j)
                for i in 0..2 for j in 0..2 ]
            [1..9] |> List.filter (fun v -> not (List.contains v fila_valores || List.contains v columna_valores || List.contains v caja_valores))
        // Crea un nodo hijo con un valor posible para la casilla vacía
        let crear_nodo_hijo (fila, columna) valor =
            let nuevo_tablero = 
                List.mapi (fun i fila_valores -> 
                    if i = fila then List.mapi (fun j v -> if j = columna then valor else v) fila_valores
                    else fila_valores) tablero
            nuevo_tablero
        // Encuentra la siguiente casilla vacía y los valores posibles para esa casilla
        match buscar_casilla_vacia 0 0 with
        | None -> // No hay casillas vacías, por lo que no hay nodos sucesores
            Seq.empty
        | Some (fila, columna) -> // Crea un nodo hijo para cada valor posible en la casilla vacía
            valores_posibles fila columna
            |> Seq.map (crear_nodo_hijo (fila, columna))

    let problema estado=
            {
                inicio      =estado
                sucesores   =sucesor
                meta        =meta
                costo       =costo
            }



