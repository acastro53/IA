namespace busqueda

module BFS = 
    open cola
    let estrategia =
        {
            vacia = empty
            insertar = enquque
            remover = dequeue
        }

    let key n = n.estado  