namespace Tables

[<AutoOpen>]
module Interfaces =

    /// Index rows.
    type Row = int

    /// Index columns.
    type Col = int

    /// A table is a function from row and column indexes to an optional value.
    /// A table also contains the domain of rows and columns (starting from 0).
    type Table<'Cell> =
        {
            NumRows : int
            NumCols : int
            Cell : Row -> Col-> option<'Cell>
        }
