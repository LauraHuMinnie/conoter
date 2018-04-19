module Screen

open System
open System.IO
open System.Diagnostics

type Cell = {glyph: char; foreground: ConsoleColor; background: ConsoleColor}

type Screen = Map<int * int, Cell>
let emptyScreen = Map.empty

let defaultBackgroundColor = ConsoleColor.Black
let defaultForegroundColor = ConsoleColor.White
let defaultCell = {glyph = ' '; foreground = defaultForegroundColor; background = defaultBackgroundColor}

let walkStringPositions (s: string) (startX, startY) =
    seq {
        let mutable pos = (startX - 1, startY)
        let mutable shouldEmit = false
        for i in 0 .. s.Length - 1 do
            let (x, y) = pos
            pos <- match s.[i] with
                   | '\n' | '\r' -> 
                        shouldEmit <- false
                        (startX - 1, y + 1)
                   | _ -> 
                        shouldEmit <- true
                        if x + 1 >= Console.BufferWidth 
                        then (startX, y + 1)
                        else (x + 1, y)
            if shouldEmit then
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

let unwrapOrSkip seq =
   Seq.filter Option.isSome seq
        |> Seq.map Option.get

let isInBuffer (x, y) =
    x >= 0 && x < Console.BufferWidth && y >= 0 && y < Console.BufferHeight

let diff (prev: Screen) next =
    let getKeysSet =
        Map.toSeq 
            >> Seq.map (fun (key, _) -> key)
            >> Set.ofSeq 
            
    let includeInDiff pos =
        if isInBuffer pos then
            match (Map.tryFind pos prev, Map.tryFind pos next) with
            | (Some(c1), Some(c2)) when c1 <> c2 -> Some((pos, c2))
            | (None, Some(c2)) -> Some((pos, c2))
            | (Some(_), None) -> Some((pos, defaultCell))
            | _ -> None
        else
            None
        
    Set.union (getKeysSet prev) (getKeysSet next)
        |> Set.toSeq
        |> Seq.map includeInDiff
        |> unwrapOrSkip
        |> Map.ofSeq

let delinearize index =
    let (y, x) = Math.DivRem(index, Console.BufferWidth)
    (x, y)

let linearize (x, y) =
    x + y * Console.BufferWidth

let nextPosition =
    linearize  
        >> (+) 1
        >> delinearize

let needsFlush (pos, a) (nextPos, b) =
    a.foreground = b.foreground && a.background = b.background && (nextPosition pos) = nextPos

let renderChunk (outStream: StreamWriter) chunk =
    let (startPos, startCell) = List.head chunk
    Console.ForegroundColor <- startCell.foreground
    Console.BackgroundColor <- startCell.background

    Console.SetCursorPosition startPos
    List.iter (fun (_, cell) -> outStream.Write(cell.glyph)) chunk
    
    outStream.Flush()

let display (outStream: StreamWriter) shouldClearScreen screen = 
    // Perf note: This function could be a lot simpler, but to make the console
    // in windows not take forever to draw everything, I have to write to the stream
    // and flush a minimal number of times. Console.WRite(' ') flushes every char
    // which is way too slow.
    let originalCursorVisible = Console.CursorVisible
    Console.CursorVisible <- false
    if shouldClearScreen then Console.Clear()
    let originalCursorPos = (Console.CursorLeft, Console.CursorTop)

    Map.toList screen 
        |> List.sortBy (fun (pos, _) -> linearize pos)
        |> chunkBy needsFlush
        |> List.iter (renderChunk outStream)

    Console.SetCursorPosition originalCursorPos
    Console.CursorVisible <- originalCursorVisible

let displayDiff (outStream: StreamWriter) oldScreen newScreen =
    diff oldScreen newScreen
        |> display outStream false
