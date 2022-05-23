module SBox
open GaloisField
open Cache
open MathNet.Numerics.LinearAlgebra //для реализации умножения больших матриц

// С целью экономии оперативки SBox вычисляется на лету 

let SBoxTMatrix = DenseMatrix.ofRowList   [[ 1.0; 0.0; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0 ];
                                           [ 1.0; 1.0; 0.0; 0.0; 0.0; 1.0; 1.0; 1.0 ];
                                           [ 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 1.0; 1.0 ];
                                           [ 1.0; 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 1.0 ];
                                           [ 1.0; 1.0; 1.0; 1.0; 1.0; 0.0; 0.0; 0.0 ];
                                           [ 0.0; 1.0; 1.0; 1.0; 1.0; 1.0; 0.0; 0.0 ];
                                           [ 0.0; 0.0; 1.0; 1.0; 1.0; 1.0; 1.0; 0.0 ];
                                           [ 0.0; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0; 1.0 ]] // вспомогательная матрица для вычисления SBox 

let InvSBoxTMatrix = DenseMatrix.ofRowList [[ 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 1.0 ];
                                            [ 1.0; 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0 ];
                                            [ 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 0.0; 1.0 ];
                                            [ 1.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 0.0 ];
                                            [ 0.0; 1.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0 ];
                                            [ 0.0; 0.0; 1.0; 0.0; 1.0; 0.0; 0.0; 1.0 ];
                                            [ 1.0; 0.0; 0.0; 1.0; 0.0; 1.0; 0.0; 0.0 ];
                                            [ 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 1.0; 0.0 ]] // для InvSBox

let SBoxConstVector = DenseMatrix.ofColumnList [[1.0; 1.0; 0.0; 0.0; 0.0; 1.0; 1.0; 0.0]] // вспомогательный вектор для вычисления SBox

let InvSBoxConstVector = DenseMatrix.ofColumnList [[1.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0]] // для InvSBox

let rec suppl (bL : seq<float>) =
    match (Seq.length bL) with
    |8 -> bL
    |_ -> Seq.append bL (Seq.singleton 0.0) |> suppl //вспомогательная рекурсивная функция расширения вектора до размерности 8 нулями

let intFunc (b : byte) = 
    System.Convert.ToString(b, 2)
    |> Seq.map (fun x -> string x |> float)
    |> Seq.rev
    |> suppl
    |> Seq.singleton
    |> DenseMatrix.ofColumnSeq // разбиваем байт на список из 8 бит, расширенных до байта

let afterTrns a =
    a
    |> Array.exactlyOne
    |> Array.map (fun x -> (int x) % 2 |> string)
    |> Array.rev
    |> Array.append [|"0b"|]
    |> Array.fold (fun acc x -> acc + x) ""
    |> byte // собираем биты в байт

let SBox (b : byte) =
    let invBMatrix =
        let wB = invByte b
        intFunc wB
        |> ( * ) SBoxTMatrix
        |> ( + ) SBoxConstVector // вычисление SBox для байта: берем обратный элемент байту в поле Галуа, 
                                 //умножаем на матрицу и прибавляем вектор
    invBMatrix.ToColumnArrays() |> afterTrns

let InvSBox (b : byte) =
    let invBMatrix =
        intFunc b
        |> ( * ) InvSBoxTMatrix
        |> ( + ) InvSBoxConstVector
    invBMatrix.ToColumnArrays() |> afterTrns |> invByte // аналогично для InvSBox

let cachedSBox = cache SBox         // кэшируем функции, так как существует всего 256 возможных входных значений 
                                    // и 256 возможных выходных значений
let cachedInvSBox = cache InvSBox   // и одно и тоже значение придется вычислять несколько раз
    
    
