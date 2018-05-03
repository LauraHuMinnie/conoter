module Notes

open System.Diagnostics
open System.Text.RegularExpressions

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

let initNotes = {content = "Notes"; current = Some(newItem (new string('a', 100))); aboves = [newItem "One above!"]; belows = [newItem "One below!"]; cursor = 0}

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
        let cur, belows = match List.tryHead item.belows with
                          | Some(_) as newCur -> newCur, List.tail item.belows
                          | None -> None, item.belows
        { item with current = cur; belows = belows }

    digModify f

let children item =
    [ 
        for i in List.rev item.aboves -> i
        if item.current.IsSome then yield item.current.Value
        for i in item.belows -> i
    ]
        
let rec serialize item =
    let rec go level i =
       let rest = children i |> List.map (go (level + 1)) |> List.fold (+) ""
       "\n{text@" + level.ToString() + "}-\n" + i.content + "\n" + rest
    
    go 0 item

let recordStartRegex = new Regex(@"\n{(?<NoteType>[a-z]+)@(?<Depth>\d+)}-\n")

let rec deserialize (s: string) =
    // read "\n{text@level}-\n" followed by its content.
    // TODO: Make this prettier--very ugly implementation here just to get something working.
    let iterateEntries str = 
        seq {    
            let mutable shouldContinue = true
            let mutable m = recordStartRegex.Match(str)
            while shouldContinue do
                let nextM = m.NextMatch()
                let startIndex = m.Index + m.Length
                let depth = int m.Groups.["Depth"].Value
                if nextM.Success 
                then yield depth, s.Substring(startIndex, nextM.Index - startIndex).Trim()
                else yield depth, s.Substring(startIndex).Trim()
                shouldContinue <- nextM.Success
                m <- nextM
        }
    
    // takes a list of nodes beneath some depth level
    // returns a list of items that are siblings, with their children populated.
    let rec parseChildren ls =
        match ls with
        | [] -> []
        | (depth, content)::rest ->
            let itemsBeneath = rest |> List.takeWhile (fun (nextDepth, _) -> nextDepth > depth) 
            match List.skip (List.length itemsBeneath) ls with
            | [] -> []
            | _::rest -> {newItem content with belows = parseChildren itemsBeneath }::parseChildren rest
        
    let parseItem ls =
        let _, content = List.head ls
        {newItem content with belows = parseChildren <| List.tail ls }
    
    iterateEntries s |> Seq.toList |> parseItem
