namespace Busqueda
//cola implementada con list
type cola<'a>=list<'a>

module Cola=
    let empty=[]
    let enqueue cola x=cola @ [x] //@ agrega el x al final
    let dequeque cola= //elimina un elemento de la cola
        match cola with
        | h::t -> Some (h,t) //regresa la bolsa sin el elemneto
        | [] -> None //si la cola esta vacia regresa None