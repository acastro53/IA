open busqueda
open System

let mutable estado = []

for i = 1 to 9 do //lectura con formato de entrada 
    printfn "Ingresa los 9 números de la fila %d, separados por espacios:" i
    let input = Console.ReadLine()
    let fila = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Array.toList
    estado <- estado @ [fila]
    
printfn "La matriz que ingresaste es:"
printfn "%A" estado

let key n = n.estado 
let t0=System.DateTime.UtcNow //registra el tiempo actual

let printSudoku (nodofinal: int list list) = //imprime en el formato correcto
    for i in [0..8] do
        if i % 3 = 0 && i <> 0 then printfn "------+-------+------"
        for j in [0..8] do
            if j % 3 = 0 && j <> 0 then printf "| "
            printf "%d " nodofinal.[i].[j]
        printfn ""

let rec NewtonRaphson (N:int, K:int, b:float) = //funcion para sacar el factor de ramificacion mediante newton raphson
    let f = pown b (K+1)(*b**(K+1.0)*) + b*(1.0 - float(N)) - 1.0
    let f_prime = float(K+1) * (pown b (K))(*b**(K)*) - float(N)
    let b_prime = b - f/f_prime
    if abs(b_prime - b) < 0.00001 then b_prime
    else NewtonRaphson(N, K, b_prime)
//let r=NewtonRaphson (100.0,5.0,2.0)

let dip = 999999999 // funcion para la busqueda, admite todas las busquedas no informadas
match Capitulo3.busquedaGrafo key BFS.estrategia (sudoku.problema estado) with 
| Some n -> let sol = Capitulo3.acciones n
            let N=sudoku.contadorNodo//es int
            let ramPro=NewtonRaphson (Seq.length sol,N, 1.0)
            printfn "solucion : %A" sol
            printf "Entrada :\n"
            printSudoku estado
            printf "Solucion:\n"
            printSudoku n.estado
| None -> printfn "no hay solucion"


let delta=System.DateTime.UtcNow - t0
printf "Tiempo trascurrido %A "delta
printf "Numero de nodos: %d" sudoku.contadorNodo

(*

        
        BFS.estrategia (sudoku.problema estado)with
| Some n->
    printf "solucion %A " n
| None -> printf "Solucion no encontrada"
*)


