namespace Tables

module Table =

    module U = Utils

    /// Extracts one row of a table.
    let getRow r t =
        [| 0 .. t.NumCols - 1 |]
        |> Array.Parallel.map (fun c -> t.Cell r c)
        |> List.ofArray

    /// Extracts one column of a table.
    let getCol c t = 
        [| 0 .. t.NumRows - 1 |]
        |> Array.Parallel.map (fun r -> t.Cell r c)
        |> List.ofArray

    /// Create a table.
    let mkTable r c f = {NumRows = r; NumCols = c; Cell = f}

    /// Set number of columns.
    let withNumCols n t = {t with NumCols = n}
    
    /// Set number of rows.
    let withNumRows n (t: Table<'C>) = {t with NumRows = n}

    /// Wraps a value in a one cell table.
    let result x = mkTable 1 1 <| fun r c ->
        if r = 0 && c = 0 then Some x else None

    /// Map over all cells of the table (including empty ones).
    let mapOption f t = mkTable t.NumRows t.NumCols <| fun r c -> f (t.Cell r c)

    /// Map over filled cells of the table.
    let map f = mapOption (Option.map f)

    /// Empty table.
    let empty<'T> : Table<'T> = mkTable 0 0 <| fun _ _ -> None

    /// Table with one empty cell.
    let emptyCell<'T> : Table<'T> = empty |> withNumRows 1 |> withNumCols 1

    /// Transpose a table.
    let transpose t = mkTable t.NumCols t.NumRows <| fun r c -> t.Cell c r

    /// Check if row and col index are in
    let inBound r c t = r >= 0 && r < t.NumRows && c >= 0 && c < t.NumCols

    /// Materializes a table as a list of rows.
    let toRows t = 
        [|0 .. t.NumRows - 1|]
        |> Array.Parallel.map (fun r -> getRow r t) 
        |> Array.toList

    /// Materializes a table as a list of columns.
    let toCols t = toRows <| transpose t

    /// Sort rows by.
    let sortRowsBy f t =
        let cell =
            let rs = toRows t |> List.sortBy f |> Array.ofList
            fun r c -> if inBound r c t then rs.[r].[c] else None
        { t with Cell = cell}

    /// Sort by columns.
    let sortColsBy f = transpose >> sortRowsBy f >> transpose

    /// Reverses the order of the rows.
    let reverseRows t = {t with Cell = fun r c -> t.Cell (t.NumRows - r - 1) c}
    let reverseCols t = transpose >> reverseRows >> transpose

    /// Composes two tables horizontally.
    let next t1 t2 =
        let nR = max t1.NumRows t2.NumRows
        let nC = t1.NumCols + t2.NumCols
        mkTable nR nC <| fun r c ->
            if c < t1.NumCols then  t1.Cell r c else  t2.Cell r (c - t1.NumCols)

    /// Composes two tables vertically.
    let over t1 t2 =  transpose (next (transpose t1) (transpose t2))

    /// Composes a sequence of tables vertically.
    let vertical ts = Seq.fold over empty ts

    /// Composes a sequence of tables horizontally.
    let horizontal ts = Seq.fold next empty ts

    /// Create a table from a list of rows.
    let fromRows rows =
        [
            for r in rows do
                yield
                    r
                    |> List.map (function
                        | Some x    -> result x
                        | None      -> emptyCell
                    )
                    |> horizontal
        ]
        |> vertical

    /// Evaluates a table which forces each cell to be computed.
    /// Cells are computed in parallel.
    let evaluate<'T> : Table<'T> -> Table<'T> =  toRows >> fromRows

    /// Concatenates a table of tables.
    let concat (t: Table<Table<'T>>) : Table<'T> =
        [
            for rIx in [0 .. t.NumRows - 1] do
               yield 
                [
                    for cIx in [0 .. t.NumCols - 1] do 
                        match t.Cell rIx cIx with
                        | Some t    -> yield t
                        | None      -> yield emptyCell
                ]
                |> horizontal
        ]
        |> vertical

    /// Applicative functor operator. Given a table of functions and 
    /// a table of values, returns a new table where each function cells are
    /// applied to each value cell.
    /// Example:
    /// ft =                t =                 ft <*> t =
    ///     [                   [                   [
    ///         f1 f2               a1 a2               (f1 a1) (f1 a2)  (f2 a1) (f2 a2)
    ///         f3 f4           ]                       (f3 a1) (f3 a1)  (f4 a1) (f4 a2)
    ///     ]                                       ]
    let rec apply (ft: Table<'a -> 'b>)  (t: Table<'a>) =
        let mapCell = function
            | Some f    ->
                map f t
            | None      ->
                t
                |> map (fun _ -> emptyCell )
                |> concat
        mapOption (mapCell >> Some) ft |> concat

    /// Appends a row. Forces evaluation of each values
    let addRow f t =
        let t = evaluate t
        let mapCells f t = { t with Cell = fun r c -> f r c  (t.Cell r c) }
        t
        |> withNumRows (t.NumRows + 1)
        |> mapCells (fun r c v ->
            if r = t.NumRows then 
                f c (getCol c t)
            else 
                t.Cell r c
        )

    /// Appends a column. Forces evaluation of each value of the table.
    let addCol f = transpose >> addRow f >> transpose

    /// Selects a single row of a table.
    let row ix t =
        {t with Cell = fun r c ->
            if r = 0 then
                t.Cell ix c
            else
                None
        }
        |> withNumRows 1

    /// Selects a single column of a table.
    let col ix = transpose >> row ix >> transpose

    /// Removes a number of rows.
    let skipRows n t = 
        {t with 
            NumRows = t.NumRows - n
            Cell = fun r c -> t.Cell (r + n ) c 
        }

    /// Removes a number of columns.
    let skipCols n = transpose >> skipRows n >> transpose

    /// Operators:
    let (!) x = result x
    let (!<) ts = horizontal ts
    let (!^) ts = vertical ts
    let (<*>) f t = apply f t
        