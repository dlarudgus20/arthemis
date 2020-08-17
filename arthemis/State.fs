namespace Arthemis

type State<'s, 'a> = State of ('s -> 'a * 's)

module State =
    let ofValue x = State (fun s -> x, s)
    let run (State p) = p

    let put s = State (fun _ -> ((), s))
    let get = State (fun s -> (s, s))
    let update f = State (fun s -> let r = f s in (r, r))

    // functor
    let fmap f (State p) = State (fun s -> let (r, s') = p s in (f r, s'))
    let fvalue v (State p) = State (fun s -> let (_, s') = p s in (v, s'))
    let fvoid s = fvalue () s

    // applicative
    let apply (State fs) (State xs) = State (fun s ->
        let (f, s') = fs s
        let (x, s'') = xs s'
        (f x), s'')

    let liftA2 f (State p) (State q) = State (fun s ->
        let (x, s') = p s
        let (y, s'') = q s'
        (f x y), s'')

    let combine (State a) (State b) =
        State (fun s -> let (r, s') = a s in b s')
    let combineL (State a) (State b) =
        State (fun s ->
            let (r, s') = a s
            let (_, s'') = b s'
            r, s'')
    let combineBoth (State a) (State b) =
        State (fun s ->
            let (r, s') = a s
            let (r', s'') = b s'
            (r, r'), s'')
    let combine3 (State a) (State b) (State c) =
        State (fun s ->
            let (r, s') = a s
            let (r', s'') = b s'
            let (r'', s''') = c s''
            (r, r', r''), s''')

    let unpackList lst = List.foldBack (liftA2 (fun a b -> a :: b)) lst (ofValue [])
    let unpackSeq seq = Seq.foldBack (liftA2 (fun a b -> a :: b)) seq (ofValue [])

    let mapM f lst = lst |> List.map f |> unpackList
    let traverse f seq = seq |> Seq.map f |> unpackSeq

    let rec whileIf f x =
        if f () then combine x (whileIf f x)
        else ofValue ()

    // monad
    let bind f (State p) =
        State (fun s ->
            let (r, s') = p s
            let (State f') = f r
            f' s')

    let composite g f x = bind g (f x)

type State<'s, 'a> with
    static member (|>>) (p, f) = State.fmap f p
    static member (<<|) (f, p) = State.fmap f p
    static member (%<<) (v, p) = State.fvalue v p
    static member (>>%) (p, v) = State.fvalue v p
    static member (<*>) (fs, xs) = State.apply fs xs
    static member (>>.) (a, b) = State.combine a b
    static member (.>>) (b, a) = State.combineL a b
    static member (.>>.) (a, b) = State.combineBoth a b
    static member (>>=) (p, f) = State.bind f p
    static member (=<<) (f, p) = State.bind f p
    static member (<=<) (g, f) = State.composite g f
    static member (>=>) (f, g) = State.composite g f

type StateBuilder() =
    member this.Zero () = State.ofValue ()
    member this.Return x = State.ofValue x
    member this.ReturnFrom (x: State<'s, 'a>) = x
    member this.Bind (x, f) = State.bind f x
    member this.Combine (a, b) = State.combine a b
    member this.Delay f : State<'s, 'a> = f ()
    member this.For (s, f): State<'s, 'b> =
        s |> Seq.map f |> Seq.reduceBack State.combine
    member this.While (f, x) = State.whileIf f x

module StateBuilder =
    let state = StateBuilder ()
