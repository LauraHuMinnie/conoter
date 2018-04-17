open System
open System.Collections.Generic
open Utils
open System.Diagnostics
open System.IO
open System.Linq
open Screen
open Notes

type EditorMode = Text | Tree | Normal

type State = {buffer: list<String>; notes: Notes; shouldQuit: bool; mode: EditorMode}
let initState = {buffer = []; notes = initNotes; shouldQuit = false; mode = Tree}

type KeyPress = {asChar: char; asEnum: ConsoleKey; withAlt: bool; withCtrl: bool; withShift: bool}

// returns (screen, cursorPos)
let renderNotes (notes: Notes) (screen, pos) =
    //let rec getCursorPosFromIndex (startX, startY as noteStartPos) index =
        
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
 
let appendCharToCurrentNote (c: char) (s: State) =
    { s with notes = {s.notes with current = s.notes.current + c.ToString() } }

let backspaceToCurrentNote (s: State) =
    if s.notes.current.Length > 0 then
        { s with notes = {s.notes with current = s.notes.current.Substring(0, s.notes.current.Length - 1) } }
    else
        s

let processKey ({buffer=b} as s: State) key =
    match s.mode with
    | Tree ->
        match key with
        | { asChar = 'q' } -> { s with shouldQuit = true }
        | { asChar = 'j' } -> { s with notes = selectNext s.notes }
        | { asChar = 'k' } -> { s with notes = selectPrevious s.notes }
        | { asChar = 'i' } -> { s with mode = Text}
        | { asChar = 'o' } -> { s with notes = insertBelow s.notes; mode = Text }
        | { asChar = 'O' } -> { s with notes = insertAbove s.notes; mode = Text }
        | { asChar = 'd' } -> { s with notes = deleteCurrent s.notes }
        | { asEnum = ConsoleKey.Escape } -> { s with buffer = [] }
        | { asEnum = ConsoleKey.Z; withCtrl = true } when List.isEmpty b |> not -> { s with buffer = List.tail b}
        | { withCtrl = true } | { withAlt = true } -> s
        | c -> { s with buffer = c.asChar.ToString() :: b }
    | Text ->
        match key with
        | { asEnum = ConsoleKey.Escape } -> { s with mode = Tree }
        | { asChar = c } when isPrintable c -> appendCharToCurrentNote c s
        | { asEnum = ConsoleKey.Backspace } -> backspaceToCurrentNote s
        | _ -> s
    | Normal ->
        s

let readKey () : KeyPress =
    let k = Console.ReadKey(true)
    {
        asChar = k.KeyChar; asEnum = k.Key;
        withAlt = k.Modifiers.HasFlag(ConsoleModifiers.Alt);
        withShift = k.Modifiers.HasFlag(ConsoleModifiers.Shift);
        withCtrl = k.Modifiers.HasFlag(ConsoleModifiers.Control)
    }

let renderStatusLine state screen =
    let bufferText = sprintf "%A - %A" state.mode state.buffer
    fst <| putString screen (0, consoleHeight - 1) ConsoleColor.Cyan defaultBackgroundColor bufferText

let render state =
    (emptyScreen, (0, 0))
        |> renderNotes state.notes 
        |> renderStatusLine state

[<EntryPoint>]
let main argv =
    use out = new StreamWriter(Console.OpenStandardOutput())
    let rec mainLoop screen state =
        let screen' = render state
        do displayDiff out screen screen'

        match readKey() |> processKey state with
        | {shouldQuit = true as state'} -> state'
        | state' -> mainLoop screen' state' 
        
    mainLoop emptyScreen initState |> ignore
    0