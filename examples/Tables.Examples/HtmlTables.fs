namespace Tables.Examples
module HtmlTable =
    module T = Tables.Table
    open Tables
    
    open Utils.Printer

    let private culture = System.Globalization.CultureInfo.InvariantCulture

    /// Cell type that supports nested tables.
    type Cell =
        | Header of string
        | Number of float
        | Table of Table<Cell>

    type Attr = {Name: string; Value : string}

    type Element = Text of string | Tag of Tag
    and Tag = 
        {
            Name : string
            Attributes : list<Attr>
            Children : list<Element>
        }

    let withChildren chs t = {t with Children = chs}

    let tag name attrs chs = Tag {Name = name; Attributes = attrs; Children = chs}
    let text = Text
    let strong = tag "strong"
    let td = tag "td"
    let tr = tag "tr"
    let table = tag "table"

    let (:=) n v = {Name = n; Value = v}
    let renderElement =
        let renderAttrs (attrs: list<Attr>) =
            let atts = 
                attrs
                |> List.map (fun a -> sprintf "%s=\"%s\":" a.Name a.Value) 
            System.String.Join (" ", atts)

        let rec go = function
            | Text s    -> 
                !s
            | Tag t     ->
                !<[
                    yield !(sprintf "<%s %s>" t.Name (renderAttrs t.Attributes))
                    yield! List.map (go >> indent) t.Children
                    yield !(sprintf "</%s>" t.Name)
                ]
        go >> run

    let rec toHtmlTable (t: Table<Cell>) =
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
        |> table ["border" := "1"]

    let renderAsHtml = toHtmlTable >> renderElement