namespace busqueda

type cola<'a> = list<'a>
module cola=

    let enquque cola x = cola @ [x]
    let dequeue cola = 
        match cola with 
        |h :: t -> Some (h,t)
        |[]-> None
    let empty = []
