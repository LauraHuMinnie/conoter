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

let walkStringPositions (s: string) ((startX, startY) as startPos) =
    seq {
        let mutable pos = startPos
        for i in 0 .. s.Length - 1 do
            let (x, y) = pos
            pos <- match s.[i] with
                   | '\n' | '\r' -> (startX, y + 1)
                   | _ -> if x + 1 > Console.WindowWidth 
                          then (startX, y + 1)
                          else (x + 1, y)
            yield (i, pos)
    }

let isPrintable (c: char) =
    match int c with
    | i when i >= 10 && i <= 126 -> true
    | _ -> false

let putString (s: Screen) startPos foreground background (string: String) =
    let cellProto = {defaultCell with foreground = foreground; background = background}

    let writeIfPrintable char pos screen =
        if isPrintable char
        then Map.add pos {cellProto with glyph = char} screen
        else screen

    walkStringPositions string startPos 
        |> Seq.fold (fun (screen, _) (i, pos) -> (writeIfPrintable string.[i] pos screen, pos)) (s, startPos)
            
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
        let (startPos, startCell) = List.head chunk
        let (endPos, _) = List.last chunk
        let cellLookup = Map.ofList chunk

        let delinearize index =
            let (y, x) = Math.DivRem(index, consoleWidth)
            (x, y)
        
        let linearize (x, y) =
            x + y * consoleWidth

        Console.SetCursorPosition startPos
        Console.ForegroundColor <- startCell.foreground
        Console.BackgroundColor <- startCell.background

        for index in linearize startPos .. linearize endPos do
            outStream.Write(match Map.tryFind (delinearize index) cellLookup with
                            | Some({glyph = g}) ->  g
                            | None -> ' ')
        
        outStream.Flush()

    Map.toList screen 
        |> List.sortBy (fun ((x, y), _) -> y * consoleWidth + x)
        |> chunkBy (fun (_, c1) (_, c2) -> needsFlush c1 c2)
        |> List.iter renderChunk

    Console.SetCursorPosition originalCursorPos
    Console.CursorVisible <- true
