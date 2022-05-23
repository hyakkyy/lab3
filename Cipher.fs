module Cipher

open System.Text
open System.Security.Cryptography
open MathNet.Numerics.LinearAlgebra
open SBox
open GaloisField

let xor x y = x ^^^ y   // алиас для часто применяемой операции побитового исключающего ИЛИ
let xor3 x y z = (x ^^^ y) ^^^ z

let rotWord (l : 'a list) =
    [l.[1]; l.[2]; l.[3]; l.[0]]   //функция для циклического сдвига списка влево

let makeSquare (l : 'a list) =
    [[l.[0]; l.[1]; l.[2]; l.[3]]
     [l.[4]; l.[5]; l.[6]; l.[7]]
     [l.[8]; l.[9]; l.[10]; l.[11]]
     [l.[12]; l.[13]; l.[14]; l.[15]]]  // функция для "оквадрачивания" пароля

let (MC : GFMember list list)=
    [[02us; 03us; 01us; 01us]
     [01us; 02us; 03us; 01us]
     [01us; 01us; 02us; 03us]
     [03us; 01us; 01us; 02us]] |> List.map (fun x -> List.map GFMember x)   //матрица, умножение на которую эквивалентно преобразованию
                                                                            //MixColumns

let (iMC : GFMember list list)=
    [[14us; 11us; 13us; 09us]
     [09us; 14us; 11us; 13us]
     [13us; 09us; 14us; 11us]
     [11us; 13us; 09us; 14us]] |> List.map (fun x -> List.map GFMember x)  //аналогично для обратного преобразования InvMixColumns


let RCon =
    [[0x01; 0x00; 0x00; 0x00]
     [0x02; 0x00; 0x00; 0x00]
     [0x04; 0x00; 0x00; 0x00]
     [0x08; 0x00; 0x00; 0x00]
     [0x10; 0x00; 0x00; 0x00]
     [0x20; 0x00; 0x00; 0x00]
     [0x40; 0x00; 0x00; 0x00]
     [0x80; 0x00; 0x00; 0x00]
     [0x1b; 0x00; 0x00; 0x00]
     [0x36; 0x00; 0x00; 0x00]] |> List.map (fun x -> List.map byte x)  // постоянный массив для генерации ключей путем XOR

let hashPassword (s : string) =
    use MD = MD5.Create()
    let result = MD.ComputeHash (Encoding.UTF8.GetBytes s)|> Array.toList |> makeSquare
    MD.Dispose()
    result  // функция хэширования пароля

let tBytesSplit128 (s : byte[]) =
    let rec supplementList (lst : byte list)=
        let l = lst.Length
        match (l % 16) with
        |0 -> lst
        |_ -> List.append lst (List.singleton 0uy) |> supplementList
    s |> Array.toList |> supplementList |> List.chunkBySize 16  // функция разбития текста на блоки

let rec keyExpansion (keySchedule : byte list list list) (rcon : byte list list) = //рекурсивная функция генерации расписания паролей
    match keySchedule.Length with
    |11 -> keySchedule |> List.map (fun x -> List.concat x) //если ключей в расписании 11 - прекращаем генерацию
    |_ ->
        let work_key = keySchedule.[keySchedule.Length - 1] //за основу нового ключа берем предыдущий
        let work_rcon :: tail_rcon = rcon // берем новый кусок RCon (a::b = array == в a - первый элемент array, в b - все остальное)
        let nth_col n =
            [work_key.[0].[n]; work_key.[1].[n]; work_key.[2].[n]; work_key.[3].[n]] // получаем энную колонку ключа
        let w_last_column =
            nth_col 3
            |> rotWord
            |> List.map cachedSBox // вспомогательная колонка для генерации ключа: берем последюю колонку прошлого ключа, сдвигаем ее 
                                   // и при помощи SBox заменяем байты
        let new_first_col = List.map3 xor3 (nth_col 0) w_last_column work_rcon // первая колонка ключа - 
                                                                               //поэлементный XOR первой колонки
                                                                               // прошлого ключа, вспомогательной и куска RCon
        let new_second_col = List.map2 xor new_first_col (nth_col 1)
        let new_third_col = List.map2 xor new_second_col (nth_col 2)
        let new_last_col = List.map2 xor new_third_col (nth_col 3)          //остальные - поэлементный XOR прошлой колонки и 
                                                                            //соответсвующей
                                                                            //колонки прошлого ключа
        let new_nth_row n = [new_first_col.[n]; new_second_col.[n]; new_third_col.[n]; new_last_col.[n]]
        let new_keySchedule = 
            [new_nth_row 0; new_nth_row 1; new_nth_row 2; new_nth_row 3]
             |> List.singleton
             |> List.append keySchedule                                     //собираем ключ и кладем его в расписание
        keyExpansion new_keySchedule tail_rcon  // рекурсия: снова запускаем функцию с расширенным расписанием и хвостом RCon  

let mixColA (col : GFMember list) =
    let nth_el n =
        let w = MC.[n]
        w.[0]*col.[0] + w.[1]*col.[1] + w.[2]*col.[2] + w.[3]*col.[3]
    [nth_el 0; nth_el 1; nth_el 2; nth_el 3] //преобразование одной колонки MixColumns путем умножения на MC

let invMixColA (col : GFMember list) =
    let nth_el n =
        let w = iMC.[n]
        w.[0]*col.[0] + w.[1]*col.[1] + w.[2]*col.[2] + w.[3]*col.[3]
    [nth_el 0; nth_el 1; nth_el 2; nth_el 3] //аналогично для обратного InvMixColumns

let addRoundKey (key : byte list) (state : byte list)  =
    List.map2 xor state key     //преобразование addRoundKey - простой XOR блока и ключа

let subBytes (state : byte list) =
    List.map cachedSBox state   //преобразование subBytes - замена байтов блока по таблице SBox

let invSubBytes (state : byte list) =
    List.map cachedInvSBox state    //аналогично для invSubBytes

let shiftRows (state : byte list) =
    [[state.[0]; state.[1]; state.[2]; state.[3]]; [state.[5]; state.[6]; state.[7]; state.[4]]; [state.[10]; state.[11]; state.[8]; state.[9]]; [state.[15]; state.[12]; state.[13]; state.[14]]]
    |> List.map (fun x -> List.map (fun y -> GFMember(uint16 y)) x) //преобразование shiftRows - сдвиг строк. 
                                                                    //На выходе не байты, а члены поля Галуа GF(2^8)

let invShiftRows (state : byte list) =
    [state.[0]; state.[1]; state.[2]; state.[3]; state.[7]; state.[4]; state.[5]; state.[6]; state.[10]; state.[11]; state.[8]; state.[9]; state.[13]; state.[14]; state.[15]; state.[12]]
    // аналогично для invShiftRows. На выходе байты

let mixColumns (state : GFMember list list) =
    let n_state = List.transpose state
    List.map mixColA n_state
    |> List.map (fun (x : GFMember list) -> List.map (fun (y : GFMember) -> y.value |> byte) x)
    |> List.transpose
    |> List.concat  //преобразование всего блока MixColumns

let invMixColumns (state : byte list) =
    let n_state =
        state
        |> makeSquare
        |> List.map (fun x -> List.map (fun y -> GFMember(uint16 y)) x)
        |> List.transpose
    List.map invMixColA n_state
    |> List.map (fun (x : GFMember list) -> List.map (fun (y : GFMember) -> y.value |> byte) x)
    |> List.transpose
    |> List.concat // аналогично для invMixColumns
     
let crypt (keyEx : byte list list) (state : byte list)  = // функция зашифровки
    let initial_key :: main_keyEx = keyEx   // берем самый первый ключ из расписания
    let initial_round = addRoundKey initial_key state // предварительный раунд - простой addRoundKey
    let rec main_rounds count (st : byte list) (kS : byte list list) = //рекурсивная вспомогательная функция зашифровки
        match count with
        |10 ->  // последний раунд отличается - не применяется MixColumns
            let round_key :: [] = kS
            let new_st = 
                st
                |> subBytes
                |> shiftRows
                |> List.map (fun (x : GFMember list) -> List.map (fun (y : GFMember) -> y.value |> byte) x)
                |> List.concat //простой перевод из матрицы членов поля Галуа в байты
                |> addRoundKey round_key
            new_st
    
        |_ ->
            let new_count = count + 1
            let round_key :: new_kS = kS
            let new_st =
                st
                |> subBytes
                |> shiftRows
                |> mixColumns
                |> addRoundKey round_key // последовательное применение преобразований
            main_rounds new_count new_st new_kS //вызываем новый раунд шифрования
    main_rounds 1 initial_round main_keyEx // вызываем вспомогательную функцию зашифровки               

let decrypt (keyEx : byte list list) (state : byte list)  = // расшифровка происходит в обратном порядке с обратными функциями
                                                            //(addRoundKey - обратная самой себе, т.к. (x XOR y) XOR y == x)
    let wKeyEx = List.rev keyEx
    let initial_key :: main_keyEx = wKeyEx
    let initial_round = addRoundKey initial_key state |> invShiftRows |> invSubBytes
    let rec main_rounds count (st : byte list) (kS : byte list list) =
        match count with
        |10 ->
            let round_key :: [] = kS
            let new_st = 
                st
                |> addRoundKey round_key
            new_st
    
        |_ ->
            let new_count = count + 1
            let round_key :: new_kS = kS
            let new_st =
                st
                |> addRoundKey round_key
                |> invMixColumns
                |> invShiftRows
                |> invSubBytes 
            main_rounds new_count new_st new_kS
    main_rounds 1 initial_round main_keyEx  


