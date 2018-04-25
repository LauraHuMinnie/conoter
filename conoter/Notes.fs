module Notes

open System.Diagnostics

type Note = string
// if there is a child note which is selected, current is Some(n).
// If a child note is not selected, everything should be in belows.
type Item = {content: Note; cursor: int; current: Option<Item>; aboves: list<Item>; belows: list<Item>}

let newItem content = {content = content; current = None; aboves = []; belows = []; cursor = 0}

let selectNext notes = 
    match (notes.current, List.tryHead notes.belows) with
    | (Some(oldCur), (Some(_) as newCur)) -> 
        {notes with current = newCur; aboves = oldCur::notes.aboves;  belows = List.tail notes.belows; cursor = 0}
    | (None, (Some(_) as newCur)) -> 
        {notes with current = newCur; belows = List.tail notes.belows; cursor = 0}
    | _ -> notes

let selectPrevious notes =
    match (notes.current, List.tryHead notes.aboves) with
    | (Some(oldCur), (Some(_) as newCur)) -> 
        {notes with current = newCur; aboves = List.tail notes.aboves;  belows = oldCur::notes.belows; cursor = 0}
    | _ -> notes

let initNotes = {content = "Parent!"; current = Some(newItem (new string('a', 100))); aboves = [newItem "One above!"]; belows = [newItem "One below!"]; cursor = 0}

let insertAbove notes =
    match notes.current with
    | Some(n) -> 
        { notes with belows = n::notes.belows; current = Some(newItem "") }
    | None ->
        { notes with current = Some(newItem "") }

let insertBelow notes =
    match notes.current with
    | Some(n) ->
        { notes with aboves = n::notes.aboves; current = Some(newItem "") }
    | None ->
        { notes with current = Some(newItem "") }

let deleteCurrent i = 
    match (List.tryHead i.aboves, List.tryHead i.belows) with
    | (Some(n), None) | (Some(n), Some(_)) -> {i with aboves = List.tail i.aboves; current = Some(n)}
    | (None, Some(n)) -> {i with belows = List.tail i.belows; current = Some(n)}
    | (None, None) -> {i with current = None}

let hasChildren item =
    not ((List.isEmpty item.aboves) && (List.isEmpty item.belows) && item.current.IsNone)

let rec dig (item: Item) =
    let rec go i (p: Item option) =
        match i.current with
        | Some(nextItem) -> go nextItem (Some(i))
        | None -> p.Value

    go item (Some(item))

let rec digMap f (item: Item) =
    match item.current with
    | Some(i) -> (f item)::(digMap f i)
    | None -> [f item]

let rec digModify f (item: Item) =
    match item.current with
    | Some(i) -> { item with current = Some(digModify f i) }
    | None -> f item

let rec digModifyParent f (item: Item) =
    match item.current with
    | Some(child) ->
        match child.current with
        | Some(_) -> {item with current = Some(digModifyParent f child) }
        | None -> f item
    | None -> 
        item

let rec navigateOut root =
    digModifyParent (fun i -> {i with current = None; belows = i.current.Value :: i.belows}) root

let rec navigateIn =
    let f item = 
        let (cur, belows) = match List.tryHead item.belows with
                            | Some(_) as newCur -> (newCur, List.tail item.belows)
                            | None -> (None, item.belows)
        { item with current = cur; belows = belows }

    digModify f