namespace Tables
module internal Utils =

    let maybe d f = function
        | Some x    -> f x
        | None      -> d

