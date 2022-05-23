module GaloisField

//Реализация поля Галуа GF(2^8)

let xTimes (b : uint16) = //реализация умножения на одночлен x
    let shl = (b <<< 1) &&& 255us
    match b &&& 128us with
    |0us -> shl
    |_ -> shl ^^^ 27us  // умножение на x эквивалентно сдвигу влево и 
                        //сложению по модулю с неприводимым в поле многочленом x^8+x^4+x^3+x+1

let rec powMul (exp : int) (acc : uint16) = // рекурсивная реализация возведения в степень
    match exp with
    |0 -> acc
    |_ ->
        let new_exp = exp - 1
        let new_acc = xTimes (acc)
        powMul new_exp new_acc

let byteToPolyExp (b : uint16) = // функция, которая преобразует байт в соответствующий ему полином
    let filterPredicate (x : int*int) =
        match x with
        |_, last -> last = 1
    let mapper (x : int*int) =
        match x with
        |first, _ -> first
    System.Convert.ToString((byte b), 2)
    |> Seq.toList
    |> List.map (fun x -> string x |> int)
    |> List.rev
    |> List.indexed
    |> List.filter filterPredicate
    |> List.map mapper
    |> List.rev

type GFMember (value : uint16) = // объявляем тип "элемент поля Галуа" и реализуем необходимые арифметические операции
    member this.value = value // член класса, хранящий значение
    static member ( + ) (l : GFMember, r : GFMember) =
         GFMember(l.value ^^^ r.value) // перегрузка оператора сложения побитовым XOR
    static member ( - ) (l : GFMember, r : GFMember) =
         GFMember(l.value ^^^ r.value) // вычитание эквивалентно сложению
    static member ( * ) (l : GFMember, r : GFMember) = // умножение
        let multGF (lu : uint16) (ru : uint16) =
            let ruList = byteToPolyExp ru
            List.fold (fun acc x -> acc ^^^ (powMul x lu)) 0us ruList
        GFMember(multGF l.value r.value) 
    member this.invert = // операция нахождения обратного путем возведения в степень
        let rec power (b : GFMember) (acc : GFMember) exp =
            match exp with
            |0 -> acc
            |_ -> 
                let new_acc = acc * b
                let new_exp = exp - 1
                power b new_acc new_exp
        power this this 253 // GF(2^8) является циклической группой порядка 2^n - 1, т.е. a^(2^n-2) = a^-1
    static member ( / ) (l : GFMember, r : GFMember) =
        l * r.invert // операция деления реализована как умножение на обратное число

let invByte (b : byte) = // операция нахождения обратного для байта
    let GFMb = GFMember(uint16 b)
    GFMb.invert.value |> byte


