namespace Busqueda
type problema<'s>= //'s estado 'a accion
    {
        inicio      :'s
        sucesores   :'s->list<'s>
        meta        :'s->bool
        costo       :'s->float
    }
type nodo<'s>=
    {
        profundidad :int
        costo_ruta  :float
        estado      :'s
        board       :'s
        padre       :option< nodo<'s> >
    }
type estrategia<'s,'a ,'d>=
    {
        vacia   : 'd //'d=bolsa la bolsa puede ser una lista o cola
        insertar: 'd -> nodo<'s>-> 'd //la bolsa recibe un nodo y escupe una bolsa con el nuevo nodo incertado
        remover : 'd -> option< nodo<'s> * 'd > //devuelve el nodo y lo elimina de la bolsa
    }
module Proyec=
    let expandir problema padre=
        problema.sucesores padre.estado
        |> List.map(fun(s)->
                    {
                        profundidad =padre.profundidad
                        estado      =s
                        board       =s
                        padre       =Some padre
                        costo_ruta  =padre.costo_ruta
                    }
                )
                
    let busquedaGrafo key estrategia problema=
        let raiz= //crear el nodo raiz
            {
                estado      =problema.inicio
                profundidad =0
                costo_ruta  =0
                board       =problema.inicio
                padre       =None
            }

        let bolsa=estrategia.insertar //inserta el nodo a la bolsa
                    estrategia.vacia //crea una nueva bolsa
                    raiz //asigna el primer nodo, nodo raiz
        let rec loop (bolsa,procesado)=
            match estrategia.remover bolsa with
            |Some (n,bolsa')-> //recibe un nodo y la bolsa
                if problema.meta n.estado //comprueba si el nodo es la meta
                then Some n //regresa el nodo
                else //incerta el nodo
                    if Set.contains (key n)procesado
                    then loop(bolsa',procesado)
                    else
                        expandir problema n 
                        |>List.fold estrategia.insertar bolsa'
                        |>(fun bolsa->loop(bolsa,Set.add(key n)procesado))
            |None->None
        
        loop (bolsa,Set.empty)