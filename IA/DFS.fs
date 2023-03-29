namespace busqueda

module DFS =
     open Pila

     let estrategia l =
      {
         vacia = empty 
         insertar = push 
         remover = pop 
      }