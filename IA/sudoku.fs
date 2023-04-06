namespace busqueda

module sudoku =
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
    
    type estado = list<list<int>*list<int>*list<int>*list<int>*list<int>*list<int>*list<int>*list<int>*list<int>>

    let inicio =   [[0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],

                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],

                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0],
                    [0;0;0 ;0;0;0; 0;0;0]]

    let goal = [    [1;2;3  ;4;5;6; 7;8;9],
                    [4;5;6  ;7;8;9; 1;2;3],
                    [7;8;9  ;1;2;3; 4;5;6],

                    [2;3;1 ;6;4;5; 9;7;8],
                    [5;6;4 ;9;7;8; 3;1;2],
                    [8;9;7 ;3;1;2; 6;4;5],

                    [3;1;2 ;5;6;4; 8;9;7],
                    [6;4;5 ;8;9;7; 2;3;1],
                    [9;7;8 ;2;3;1; 5;6;4]]

    let meta estado =
        List.map2(fun x y ->(x,y))
            goal estado
        |> List.forall(fun(x,y)->x=y)

    let succesores (estado:estado)=
        let indicesVacios = //comprueba si se encuentran valores 0 en alguna posicion de la lista
            [ for i in 0..8 do
                for j in 0..8 do
                    if estado.[i].[j] = 0 then yield (i,j) ] //si encuentra un 0 regresa la pocicion donde se enontro el valor

        let movimientosValidos (i,j)=//comprueba los valores validos para la posicion donde se encontraron 0 para ello se lleva la posicion de donde se encontro el 0
            let fila = estado.[i]//a fila le asigna la fila de la posicion del 0
            let columna = [ for k in 0..8 -> estado.[k].[j] ]
            let subcuadricula = //asigna la cuadricula donde se encontro un 0
                [ for k in [0..2] do
                    for l in [0..2] do
                        yield estado.[i/3*3+k].[j/3*3+l] ]//regresa la cuadricula
        
            [1..9]//secuencia de posibles valores
                |> Seq.filter (fun num -> not (num |> List.contains fila))//revisa si se pueden asignar valores a la fila encontrada anteriormente dentro de una secuencia
                |> Seq.filter (fun num -> not (num |> List.contains columna))//asoigna valores a la columna 
                |> Seq.filter (fun num -> not (num |> List.contains subcuadricula))//asigna posibles valores a la cuadricula
                |> Seq.toList//regresa la union de las secuencias anteriores
            
        [ for (i,j) in indicesVacios do//creara un tablero nuevo con los posibles valores apartir de donde se encontro el 0
            [ for num in movimientosValidos (i,j) -> 
                let nuevoTablero = 
                    estado
                    |> List.mapi (fun r fila -> 
                        fila 
                        |> List.mapi (fun c valor -> 
                            if r=i && c=j then num else valor))
                nuevoTablero ] ]

    let problema estado = {
        inicio = estado
        sucesores = sucesores
        meta = meta
        costo = costo
    }
