module Screen

open System
open System.IO

type Cell = {glyph: char; foreground: ConsoleColor; background: ConsoleColor}

type Screen = Map<int * int, Cell>
let emptyScreen = Map.empty

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
    // Perf note: This function could be a lot simpler, but to make the console
    // in windows not take forever to draw everything, I have to write to the stream
    // and flush a minimal number of times. Console.WRite(' ') flushes every char
    // which is way too slow.
    Console.CursorVisible <- false
    Console.Clear()
    let originalCursorPos = (Console.CursorLeft, Console.CursorTop)
    let needsFlush a b =
        a.foreground = b.foreground && a.background = b.background
    
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
