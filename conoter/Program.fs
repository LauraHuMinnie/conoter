open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open Screen
open Notes
open Utils

type EditorMode = Text | Tree | Normal

type State = {buffer: list<String>; root: Item; shouldQuit: bool; mode: EditorMode}
let initState = {buffer = []; root = initNotes; shouldQuit = false; mode = Tree}

type KeyPress = {asChar: char; asEnum: ConsoleKey; withAlt: bool; withCtrl: bool; withShift: bool}

// returns (screen, cursorPos)
let renderNotes root (screen, pos) =
    let rec go items isCurrent (screen, (startX, startY as pos)) = 
        match items with
        | [] -> screen, pos
        | i::xs -> 
            let selectionChar = if hasChildren i then "+" else "-"
            let prefix = if isCurrent then (String.replicate 2 selectionChar) + ">" else " " + selectionChar + " "
            let renderedText = prefix + i.content
            let s, (_, y) = putString screen pos defaultForegroundColor defaultBackgroundColor renderedText
            if isCurrent then
                do Console.SetCursorPosition (walkStringPositions (renderedText + " ") pos
                                                |> Seq.tryFind (fun (ix, _) -> ix = i.cursor + prefix.Length) 
                                                |> Option.defaultValue (0, (startX + prefix.Length, startY))
                                                |> snd)
            go xs false (s, (0, y + 1))

    let currentItem = dig root
    let selectedChildAsList = match currentItem.current with
                              | Some(i) -> [i]
                              | None -> []

    (screen, pos) 
        |> go (List.rev currentItem.aboves) false
        |> go selectedChildAsList true 
        |> go currentItem.belows false 
        |> fst
 
let insertCharAtCursor (c: char) =
    digModify (fun i -> {i with content = i.content.Insert(i.cursor, c.ToString()); cursor = i.cursor + 1 })

let backspaceAtCursor =
    let f (i: Item) = 
        if i.content.Length > 0 && i.cursor > 0 then
            { i with content = i.content.Remove(i.cursor - 1, 1); cursor = i.cursor - 1  } 
        else
            i
    
    digModify f

let moveCursorByChar distance =
    digModify (fun i -> {i with cursor = Math.Clamp(i.cursor + distance, 0, i.content.Length) }) 

let processKey ({buffer=b} as s: State) key =
    match s.mode with
    | Tree ->
        match key with
        | { withCtrl = true } | { withAlt = true } -> s
        | { asChar = 'q' } -> { s with shouldQuit = true }
        | { asChar = 'j' } -> { s with root = digModifyParent selectNext s.root }
        | { asChar = 'k' } -> { s with root = digModifyParent selectPrevious s.root }
        | { asChar = 'a' } -> { s with mode = Text; root = digModify (fun i -> {i with cursor = i.content.Length}) s.root}
        | { asChar = 'i' } -> { s with mode = Text; root = digModify (fun i -> {i with cursor = 0}) s.root}
        | { asChar = 'o' } -> { s with root = digModifyParent insertBelow s.root; mode = Text }
        | { asChar = 'O' } -> { s with root = digModifyParent insertAbove s.root; mode = Text }
        | { asChar = 'd' } -> { s with root = digModifyParent deleteCurrent s.root }
        | { asChar = 'h' } -> { s with root = navigateOut s.root }
        | { asChar = 'l' } -> { s with root = navigateIn s.root }
        | { asChar = 'e' } -> { s with mode = Normal }
        | { asChar = 's' } -> { s with root = digModify (fun i -> {i with current = Some(newItem "")} ) s.root}
        | { asChar = 'S' } -> 
            File.WriteAllText("test.notes", serialize s.root)
            s
        | { asChar = 'L' } -> { s with root = File.ReadAllText("test.notes") |> deserialize |> selectNext }
        | { asEnum = ConsoleKey.Escape } -> { s with buffer = [] }
        | { asEnum = ConsoleKey.Z; withCtrl = true } when List.isEmpty b |> not -> { s with buffer = List.tail b}
        | c -> { s with buffer = c.asChar.ToString() :: b }
    | Text ->
        match key with
        | { asEnum = ConsoleKey.Escape } -> { s with mode = Normal }
        | { asChar = c } when isPrintable c -> {s with root = insertCharAtCursor c s.root}
        | { asEnum = ConsoleKey.Backspace } -> {s with root = backspaceAtCursor s.root}
        | _ -> s
    | Normal ->
        match key with
        | { asEnum = ConsoleKey.Escape } -> { s with mode = Tree; root = digModify (fun i -> {i with cursor = 0}) s.root }
        | { asChar = 'l' } -> {s with root = moveCursorByChar 1 s.root}
        | { asChar = 'h' } -> {s with root = moveCursorByChar -1 s.root}
        | { asChar = 'i' } -> { s with mode = Text }
        | _ -> s

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
    fst <| putString screen (0, Console.BufferHeight - 1) ConsoleColor.Cyan defaultBackgroundColor bufferText

let configureCursor state screen =
    Console.CursorSize <- match state.mode with
                          | Normal -> 100
                          | Text -> 50
                          | Tree -> 1

    Console.CursorVisible <- state.mode <> Tree
    screen

let renderBreadcrumbs item (screen, startPos) =
    let trail = digMap (fun i -> i.content) item
    let text = List.reduce (fun a b -> sprintf "%s > %s" a b) (List.truncate (trail.Length - 1) trail)
    let screen, (_, y) = putString screen startPos ConsoleColor.White ConsoleColor.DarkBlue text
    screen, (0, y + 1)

let render state =
    (emptyScreen, (0, 0))
        |> renderBreadcrumbs state.root
        |> renderNotes state.root 
        |> renderStatusLine state
        |> configureCursor state

[<EntryPoint>]
let main argv =
    use out = new StreamWriter(Console.OpenStandardOutput())
    let mutable lastBufferSize = (Console.BufferWidth, Console.BufferHeight)
    let rec mainLoop screen state =
        let bufferSize = (Console.BufferWidth, Console.BufferHeight)
        let didWindowSizeChange = bufferSize <> lastBufferSize

        Console.SetBufferSize(Console.WindowWidth, Console.WindowHeight)

        let screen' = render state

        if didWindowSizeChange
        then do display out true screen'
        else do displayDiff out screen screen'

        lastBufferSize <- bufferSize

        match readKey() |> processKey state with
        | {shouldQuit = true as state'} -> state'
        | state' -> mainLoop screen' state' 
        
    mainLoop emptyScreen initState |> ignore
    0