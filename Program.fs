open System

let pow d = d * d

let sumBy (f: double*double -> double) (m: (double*double) list): double = List.map f m |> List.sum

let liniar t0 t1 = fun x -> t0 + t1 * x

let margin = 0.000000000001

let liniar_regression (m: (double * double) list) (i: (double * double)) (a: double) = 
    let mutable t0, t1 = i

    let mutable convergence = false
    while not convergence do
        let h = liniar t0 t1

        let new_t0 = t0 - a * ((1.0 / double m.Length) * sumBy (fun (x, y) -> h x - y) m)
        let new_t1 = t1 - a * ((1.0 / double m.Length) * sumBy (fun (x, y) -> (h x - y) * x) m)

        if Math.Abs(t0 - new_t0) < margin && Math.Abs(t1 - new_t1) < margin then convergence <- true

        t0 <- new_t0
        t1 <- new_t1

    t0, t1

let data = [
    (0.0, 2.0)
    (1.0, 1.0)
    (2.0, 0.0)
    (3.0, -1.0)
    (4.0, -2.0)
    (5.0, -3.0)
    (6.0, -4.0)
    (7.0, -5.0)
    (8.0, -6.0)
    (9.0, -7.0)
    (10.0, -8.0)
]

[<EntryPoint>]
let main argv =
    printfn "%A" <| liniar_regression data (0.0, 0.0) 0.005

    0
