module FS
open System.Text
open System.IO

// Алиасы для работы с файловой системой

let readFile filePath = File.ReadAllBytes (filePath)

let writeFile filePath contents = File.WriteAllBytes (filePath, contents)


