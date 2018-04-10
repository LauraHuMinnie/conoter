open System
open System.Collections.Generic
open Utils
open System.Diagnostics
open System.IO
open System.Linq
open System.Linq
open System.Linq
open Screen


type Note = string
type Notes = {current: Note; aboves: list<Note>; belows: list<Note>}

let selectNext ({aboves = a; current = c; belows = b} as notes) = 
    match List.tryHead b with
    | Some(n) -> {aboves = c::a; current = n; belows = List.tail b}
    | None -> notes
    

let initNotes = {current = "Test note!"; aboves = ["One above!"]; belows = ["One below!"]}

type EditorMode = Text | Tree

type State = {buffer: list<String>; notes: Notes; shouldQuit: bool; mode: EditorMode}
let initState = {buffer = []; notes = initNotes; shouldQuit = false; mode = Tree}

type KeyPress = {asChar: char; asEnum: ConsoleKey; withAlt: bool; withCtrl: bool; withShift: bool}

let renderNotes (notes: Notes) (screen, pos) =
    let rec go notes isCurrent (screen, (x, y as pos)) = 
        match notes with
        | [] -> (screen, pos)
        | note::xs -> 
            let prefix = if isCurrent then "==>" else " - "
            let (s, (_, y)) = putString screen pos defaultForegroundColor defaultBackgroundColor (prefix + note)
            go xs false (s, (0, y + 1))
    
    (screen, pos) 
        |> go (List.rev notes.aboves) false
        |> go [notes.current] true 
        |> go notes.belows false 
        |> fst

let processKey ({buffer=b} as s: State) key =
    match key with
    | { asChar = 'q' } -> { s with shouldQuit = true }
    | { asChar = 'j' } -> { s with notes = selectNext s.notes }
    | { asChar = 'k' } -> { s with shouldQuit = true }
    | { asEnum = ConsoleKey.Escape } -> { s with buffer = [] }
    | { asEnum = ConsoleKey.Z; withCtrl = true } when List.isEmpty b |> not -> { s with buffer = List.tail b}
    | { withCtrl = true } | { withAlt = true } -> s
    | c -> { s with buffer = c.asChar.ToString() :: b }

let readKey () : KeyPress =
    let k = Console.ReadKey(true)
    {
        asChar = k.KeyChar; asEnum = k.Key;
        withAlt = k.Modifiers.HasFlag(ConsoleModifiers.Alt);
        withShift = k.Modifiers.HasFlag(ConsoleModifiers.Shift);
        withCtrl = k.Modifiers.HasFlag(ConsoleModifiers.Control)
    }

let renderBuffer buffer screen =
    let bufferText = sprintf "%A" buffer
    let (s, _) = putString screen (0, consoleHeight - 1) ConsoleColor.Cyan defaultBackgroundColor bufferText
    s

let render state =
    (emptyScreen, (0, 0))
        |> renderNotes state.notes 
        |> renderBuffer state.buffer

[<EntryPoint>]
let main argv =
    use out = new StreamWriter(Console.OpenStandardOutput())
    let rec mainLoop state =
        let screen = render state
        do display out screen

        match readKey() |> processKey state with
        | {shouldQuit = true as state'} -> state'
        | state' -> mainLoop state' 
        
    mainLoop initState |> ignore
    0