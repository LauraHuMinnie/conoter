open System
open System.Collections.Generic
open Utils

type Cell = {glyph: char; foreground: ConsoleColor; background: ConsoleColor}

// Not sure about this being mutable.
// TODO: Convert this to Map
type Screen = Map<(int * int), Cell>
let emptyScreen = Map.empty

type Notes = list<string>

type EditorMode = Text | Tree
type State = {screen: Screen; buffer: list<String>; shouldQuit: bool; mode: EditorMode}
type KeyPress = {asChar: char; asEnum: ConsoleKey; withAlt: bool; withCtrl: bool; withShift: bool}

let defaultBackgroundColor = ConsoleColor.Black
let defaultForegroundColor = ConsoleColor.White
let defaultCell = {glyph = ' '; foreground = defaultForegroundColor; background = defaultBackgroundColor}
let consoleWidth = 80

let putString (s: Screen) ((startX, startY) as startPos) foreground background (string: String) =
    let cellProto = {defaultCell with foreground = foreground; background = background}

    let getPosAndGlyph charIndex =
        let (yOffset, x) = Math.DivRem(charIndex + startX, consoleWidth - startX)
        let y = yOffset + startY
        ((x, y), string.[charIndex])

    seq { 0 .. string.Length - 1 }
        |> Seq.map getPosAndGlyph 
        |> Seq.fold (fun (screen, _) (pos, c) -> 
                        (Map.add pos {cellProto with glyph = c} screen, pos)) 
                    (s, startPos)
            
let renderCell (x, y) cell =
    do 
        Console.SetCursorPosition(x, y)
        Console.ForegroundColor <- cell.foreground
        Console.BackgroundColor <- cell.background
        Console.Write(cell.glyph)

let display = Map.iter renderCell

let renderNotes (screen: Screen) (notes: Notes) =
    let rec go y notes screen = 
        match notes with
        | [] -> screen
        | note::xs -> 
            let (s, (_, y)) = putString screen (0, y) defaultForegroundColor defaultBackgroundColor (" - " + note)
            go (y + 1) xs s
    
    go 0 notes screen

        
 




let initState = {screen = emptyScreen; buffer = []; shouldQuit = false; mode = Tree}

let processKey ({buffer=b} as s: State) key =
    match key with
    | { asChar = 'q' } -> { s with shouldQuit = true }
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


[<EntryPoint>]
let main argv =
    let s = renderNotes emptyScreen ["First note"; "And another one!"]
    //putString s (3, 3) ConsoleColor.White ConsoleColor.DarkBlue "OK!" |> ignore
    do display s
    Console.ReadKey() |> ignore
    0
(*

type EditorMode = Text | Tree
type Cell = {glyph: char; foreground: ConsoleColor; background: ConsoleColor}
type State = {buffer: list<String>; shouldQuit: bool; mode: EditorMode}
type KeyPress = {asChar: char; asEnum: ConsoleKey; withAlt: bool; withCtrl: bool; withShift: bool}

let initState = {buffer = []; shouldQuit = false; mode = Tree}

let processKey ({buffer=b} as s: State) key =
    match key with
    | { asChar = 'q' } -> { s with shouldQuit = true }
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

[<EntryPoint>]
let main argv =
    Console.Title <- "Conoter"
    let mutable state = initState
    while not state.shouldQuit do
        state <- readKey () |> processKey state 
        printfn "buffer = %A" state.buffer
    0
*)