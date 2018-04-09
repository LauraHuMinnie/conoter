open System
open System.Collections.Generic
open Utils
open System.Diagnostics
open System.IO
open System.Linq
open System.Linq
open System.Linq

type Cell = {glyph: char; foreground: ConsoleColor; background: ConsoleColor}

type Screen = Map<int * int, Cell>
let emptyScreen = Map.empty

type Notes = list<string>

type EditorMode = Text | Tree

type State = {buffer: list<String>; notes: Notes; shouldQuit: bool; mode: EditorMode}
let initState = {buffer = []; notes = ["Test note!"]; shouldQuit = false; mode = Tree}

type KeyPress = {asChar: char; asEnum: ConsoleKey; withAlt: bool; withCtrl: bool; withShift: bool}

let defaultBackgroundColor = ConsoleColor.Black
let defaultForegroundColor = ConsoleColor.White
let defaultCell = {glyph = ' '; foreground = defaultForegroundColor; background = defaultBackgroundColor}

let consoleWidth = Console.WindowWidth
let consoleHeight = Console.WindowHeight

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
            
let rec chunkBy (f: 'a -> 'a -> bool) (ls: list<'a>) =
    // for (=) [1; 1; 2; 3;] returns [1; 1]
    let rec firstChunk (f2: 'a -> 'a -> bool) =
        function 
        | [] -> []
        | [first] -> [first]
        | first::rest ->
            if f2 first (List.head rest) then
                first::(firstChunk f2 rest)
            else
                [first]
    
    match firstChunk f ls with
    | [] -> []
    | c -> c::(chunkBy f (List.skip (List.length c) ls))

let display (outStream: StreamWriter) screen = 
    Console.CursorVisible <- false
    Console.Clear()
    let originalCursorPos = (Console.CursorLeft, Console.CursorTop)
    let needsFlush {foreground = fg1; background = bg1} {foreground = fg2; background = bg2} =
        fg1 = fg2 && bg1 = bg2
    
    // iterate through coords between start and end pos and generate string to write
    // set fg and bg, write string to out, flush it.
    let renderChunk chunk =
        let ((startX, startY), startCell) = List.head chunk
        let ((endX, endY), _) = List.last chunk
        let cellLookup = Map.ofList chunk

        Console.SetCursorPosition(startX, startY)
        Console.ForegroundColor <- startCell.foreground
        Console.BackgroundColor <- startCell.background

        for y in startY .. endY do
            for x in startX .. endX do
                outStream.Write(match Map.tryFind (x, y) cellLookup with
                                | Some({glyph = g}) ->  g
                                | None -> ' ')
        
        outStream.Flush()

    Map.toList screen 
        |> List.sortBy (fun ((x, y), _) -> y * consoleWidth + x)
        |> chunkBy (fun (_, c1) (_, c2) -> needsFlush c1 c2)
        |> List.iter renderChunk

    Console.SetCursorPosition originalCursorPos
    Console.CursorVisible <- true

let renderNotes notes screen =
    let rec go y notes screen = 
        match notes with
        | [] -> screen
        | note::xs -> 
            let (s, (_, y)) = putString screen (0, y) defaultForegroundColor defaultBackgroundColor (" - " + note)
            go (y + 1) xs s
    
    go 0 notes screen

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

let renderBuffer buffer screen =
    let bufferText = sprintf "%A" buffer
    let (s, _) = putString screen (0, consoleHeight - 1) ConsoleColor.Cyan defaultBackgroundColor bufferText
    s

let render state =
    emptyScreen 
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