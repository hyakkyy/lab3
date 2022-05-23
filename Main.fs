module AES_Crypt_CLI

open Cache
open GaloisField
open SBox
open FS
open Cipher

(* Главный файл; функция aes принимает следующие параметры: выбор расшифровка/дешифровка, путь рабочего файла,
   путь выходного файла, пароль для шифрования*)


let aes isCrypting input output pass =
    match isCrypting with
    |true ->
        let password = pass |> hashPassword |> List.singleton 
        (* при помощи хэш-функции MD5 хэшируем пароль, преобразуем в "квадратный" (4*4) список
        (стандартная для F# структура данных) 
        *)                                                    
        let keySchedule = keyExpansion password RCon //генерируем расписание ключей - промежуточные ключи для каждого раунда
        let textToWork = readFile input |> tBytesSplit128 // разбиваем файл на блоки по 128 бит
        List.map (crypt keySchedule) textToWork
        |> List.concat 
        |> List.toArray
        |> writeFile output     // к каждому блоку применяем функцию зашифровки crypt с расписанием keySchedule, 
                                //склеиваем блоки в один и записываем в файл

    |false ->
        let password = pass |> hashPassword |> List.singleton
        let keySchedule = keyExpansion password RCon
        let textToWork = readFile input |> tBytesSplit128
        List.map (decrypt keySchedule) textToWork
        |> List.concat 
        |> List.toArray
        |> writeFile output    // аналогично
    0
