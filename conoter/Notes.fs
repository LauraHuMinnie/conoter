module Notes

type Note = string
type Notes = {current: Note; aboves: list<Note>; belows: list<Note>}

let selectNext ({aboves = a; current = c; belows = b} as notes) = 
    match List.tryHead b with
    | Some(n) -> {aboves = c::a; current = n; belows = List.tail b}
    | None -> notes

let selectPrevious ({aboves = a; current = c; belows = b} as notes) =
    match List.tryHead a with
    | Some(n) -> {aboves = List.tail a; current = n; belows = c::b}
    | None -> notes

let initNotes = {current = "Test note!"; aboves = ["One above!"]; belows = ["One below!"]}

let insertAbove (notes: Notes) =
    { notes with belows = notes.current::notes.belows; current = "" }

let insertBelow (notes: Notes) =
    { notes with aboves = notes.current::notes.aboves; current = "" }

let deleteCurrent (notes: Notes) = 
    match (List.tryHead notes.aboves, List.tryHead notes.belows) with
    | (Some(n), None) | (Some(n), Some(_)) -> {notes with aboves = List.tail notes.aboves; current = n}
    | (None, Some(n)) -> {notes with belows = List.tail notes.belows; current = n}
    | _ -> notes