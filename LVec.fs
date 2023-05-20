module LVec
open System.Numerics
// All vector are stored in float lists.
let Mudulus (vec:float list) =
    sqrt(List.fold(fun acc elem -> acc + elem ** 2.0) 0. vec)

let Dot (v1:float list, v2:float list) =
    List.fold2(fun acc el1 el2 -> acc + el1 * el2) 0. v1 v2

let Muti (num:double, v:float list) =
    List.map(fun x -> num * x) v

let Add (v1:float list, v2:float list) =
    List.map2(fun el1 el2 -> el1 + el2) v1 v2

let Subtract (v1:float list, v2:float list) =
    List.map2(fun el1 el2 -> el1 - el2) v1 v2

let T (m:(float list) list) =
    List.map(fun i -> 
        List.map(fun j ->
        m[j][i]) [0..(m.Length - 1)]) (* number of col. *) 
        [0..(m[0].Length - 1)] (* number of lines *)

let SubMat (m:float list list, beg:int * int, ends:int * int) =
    List.map(fun (line:float list) -> line[beg.Item2..ends.Item2]) m[beg.Item1..ends.Item1]

let Confactor (m:float list list, i:int, j:int) =
    let sm1 = List.map (fun (line:float list) -> line[..(j-1)] @ line[(j+1)..]) m
    sm1[..(i-1)] @ sm1[(i+1)..]
    
let rec Det (d:float list list) = 
    if d.Length = 1 then d[0][0]
    else
        let mutable sums = 0.
        List.iteri(fun i d0x -> do
            sums <- sums + (-1.)**(float i) * d0x * Det(Confactor(d, 0, i))) d[0]
        sums

let MutiNum (num:double) (m:float list list) =
    List.map(fun line -> List.map(fun x -> x * num) line) m

let MutiMat (m1:float list list, m2:float list list) =
    //           [1]  [1]  [1]
    //           [j]  [j]  [j]
    // 1[1..j]
    // i[1..j]
    let MutiPlace i j =
        List.map(fun ii -> m1[i][ii] * m2[ii][j]) [0..(m1[0].Length - 1)]
        |> List.sum
    List.map(fun i -> List.map(fun j -> MutiPlace i j) [0..(m2[0].Length - 1)]) [0..(m1.Length - 1)]

let Rev (d:float list list):float list list =
    MutiNum (1./Det d)
        (List.map(fun i -> List.map(fun j -> 
            (-1.)**float(i+j) * Det(Confactor(d, i, j)) ) [0..(d.Length-1)]) [0..(d.Length-1)]
            |> T )

let PrtMat (m:float list list) = do
    printfn "[Mat]"
    for i in m do
        printfn "\t%A" i
