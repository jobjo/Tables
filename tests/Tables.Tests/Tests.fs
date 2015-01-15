namespace Tables.Test
module Tests =
    open FsCheck
    open FsCheck.Xunit
    open Tables
    open Tables.Table
    open Generators

    /// Compare table
    let private (==) t1 t2 =
        toRows t1 = toRows t2

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Empty is identity for over`` (t: Table<int>) = 
        over t empty == t

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Empty is identity for next`` (t: Table<int>) = 
        next t empty == t

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Map is a functor`` (f: int -> int) (g: int -> int) t =
        let f1 = map g << map f
        let f2 = map ( g << g)
        f1 t == f2 t

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Transpose twice is identity`` (t: Table<int>) = 
        transpose (transpose t) == t

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Applicative functor identity`` (t: Table<int>) = 
        (result id <*> t) == t

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Applicative functor composition`` (t1: Table<int -> int>) (t2: Table<int -> int>) (t3: Table<int>) = 
        let a = !(<<) <*> t1 <*> t2 <*> t3
        let b = t1 <*> (t2 <*> t3)
        a == b

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Applicative functor homomorphism`` (f: int -> int) x =
        result f <*> result x == result (f x)

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Applicative functor interchange`` (ft: Table<int -> int>) (x: int) =
        let t1 = ft <*> result x
        let t2 = result (fun f -> f x) <*> ft
        t1 == t2

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Applicative functor behaves as map when applying lifted function`` (f: int -> int) (t: Table<int>) =
        map f t == (result f <*> t)

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Evaluate is identity`` (t: Table<int>) =
        evaluate t == t


