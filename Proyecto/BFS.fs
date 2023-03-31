namespace Busqueda
module BFS=
    let estrategia=
        {
            vacia   =Cola.empty
            insertar=Cola.enqueue
            remover =Cola.dequeque
        }
    let key n=n.estado
    