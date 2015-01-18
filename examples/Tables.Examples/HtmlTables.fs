namespace Tables.Examples
module HtmlTable =

    open Tables
    open Utils.Html
    module T = Tables.Table

    let private culture = System.Globalization.CultureInfo.InvariantCulture

    /// Cell type that supports nested tables.
    type Cell =
        | Header of string
        | Number of float
        | Table of Table<Cell>

    /// Render HTML table
    let rec toHtmlTable (t: Table<Cell>) =
        let rows = T.toRows t
        T.toRows t
        |> List.map (fun row ->
            row
            |> List.map (fun cell ->
                match cell with
                | Some v    ->
                    match v with
                    | Header h  ->
                        td ["align" := "center"; "style" := "padding : 10px"] [strong [] [text h]]
                    | Number n  ->
                        td ["align" := "right"; "style" := "padding : 10px"] [text (string n)]
                    | Table t   ->
                        td [] [toHtmlTable t]
                | None      ->
                    td [] []
            )
            |> tr []
        )
        |> table ["class" :="display" ; "cellspacing" :="0"; "width" :="100%"]

    /// Render as HTML.
    let renderAsHtml = toHtmlTable >> renderElement