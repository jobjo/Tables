namespace Tables.Test
module Generators =
    open FsCheck
    open Tables
    module T = Tables.Table

    let optional<'T> =
        gen {
            let! t = Arb.generate<int>
            if t % 3 = 0 then
                return None
            else
                return! Gen.map Some (Arb.generate<'T>)
        }

    let genList n = Gen.sequence (List.replicate n optional)

    let genTable<'T> : Gen<Table<'T>> =
        gen {
            let! nr = Gen.choose (0,10)
            let! nc = Gen.choose (0,3)
            let! rows = Gen.sequence (List.replicate nr (genList nc)) 
            return T.fromRows rows
        }

    /// Custom generators.
    type CustomGenerators =
        static member Table() = 
            {new Arbitrary<Table<'T>>() with override x.Generator = genTable }


