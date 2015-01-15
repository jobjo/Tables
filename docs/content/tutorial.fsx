(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Introducing your project
========================
Following are three different examples of creating a 2x2 table holding
integer values.

*)
#r "Tables.dll"
open Tables

open Tables
open Tables.Table

let t1 =
    fromRows [
        [Some "A11"; None]
        [Some "A21"; Some "A22"]
    ]

let t2 = 
    vertical [
        horizontal [result "A11"; empty]
        horizontal [result "A21"; result "A22"]
    ]

let t3 =
    !^[
        !<[!"A11"; empty]
        !<[!"A21"; !"A22"]
    ]

(**
Some more info
*)
