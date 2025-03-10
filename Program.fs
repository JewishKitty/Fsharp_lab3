open System
open System.IO

let rec main() =
    let continuation() = 
        printfn "\nВведите 'y' (англ.) для продолжения "
        let continuationinput = System.Console.ReadLine()
        if continuationinput = "y" then 
            main()
        else 
            printfn "done"


    let rec if_number_int() =
        printfn "Введите значение "
        let input = System.Console.ReadLine()
        match Int32.TryParse(input) with
        |(true, number) -> 
            number
        | _ ->
            printfn "Ошибка! Введите целое число!"
            if_number_int()

    let mult_of_dig_seq strNum =
        strNum
        |> Seq.filter System.Char.IsDigit
        |> Seq.map (fun c -> float(int c - int '0'))
        |> Seq.fold (fun acc x -> acc * x) 1.0


    let Random_seq n =
        let rec generate count  =
            seq {
                if count > 0 then
                    let rand = Random()
                    let int_part = rand.Next(1, int (2147483647)) // Генерируем целую часть
                    let float_part = float(rand.Next(1,999))/1000.0 // Генерируем дробную часть (0.0 <= x < 1.0)
                    yield string(abs((float int_part) + float_part))
                    yield! generate (count - 1) 
            }
        generate n


    let rec readNumbers acc =
        printf "Введите число (или stop для завершения): "
        let input = Console.ReadLine()
        match Double.TryParse(input) with
        | (true, number) -> readNumbers (Seq.append acc (seq { yield string(abs(number)) }))
        | _ when input = "stop" -> acc
        | _ ->
            printfn "Ошибка! Введите число (или stop для завершения): "
            readNumbers acc



    let rec remove_unnessecery_zeros binary =
        match binary with
        | "" -> "0"
        | _ when binary.[0] = '0' -> remove_unnessecery_zeros (binary.Substring(1))
        | _ -> binary



    let convertBinary x =
        match x with
        | "1" -> 1
        | "10" -> 2
        | "11" -> 3
        | "100" -> 4
        | "101" -> 5
        | "110" -> 6
        | "111" -> 7
        | "1000" -> 8
        | "1001" -> 9
        | _ -> -1


    let rec readBinaryNumbers acc =
        printf "Введите бинарное число от 1 до 9 (или 'stop' для завершения): "
        let input = System.Console.ReadLine()
        let normalized_input = remove_unnessecery_zeros input
        match normalized_input with
        | "stop" -> acc
        | "1" | "10" | "11" | "100" | "101" | "110" | "111" | "1000" | "1001" -> 
            readBinaryNumbers (Seq.append acc (seq {yield normalized_input}))
        | _ -> 
            printfn "Ошибка! Введите бинарное число от 1 до 9!"
            readBinaryNumbers acc

    let rec findFile directory fileName =
        seq {
        
            for file in Directory.EnumerateFiles(directory) do
            
                if Path.GetFileNameWithoutExtension(file) = fileName then
                    yield file

            for subDir in Directory.EnumerateDirectories(directory) do
                yield! findFile subDir fileName
            }

    

    let rec choice () =
        printfn "Выберите операцию(1 - сумма произведений цифр соответствующий цифр, 2 - сумма двоичных чисел, 3 - файл в каталоге"
        let input = System.Console.ReadLine()
        match System.Int32.TryParse(input) with
        |(true, number) -> 
                if number = 1 then
                        let seq_for_mul = readNumbers Seq.empty 
                        let result = seq_for_mul |> Seq.map mult_of_dig_seq 
                        if seq_for_mul = Seq.empty then
                            printfn "Последовательность пуста "
                            continuation()
                        else
                            result |> Seq.iter (printf "%.3f ")
                            continuation()
                elif number = 2 then
                        let binirylist = readBinaryNumbers Seq.empty
                        if binirylist = Seq.empty then
                            printfn "Последовательность пуста "
                        else
                            let demonstration = binirylist|> Seq.map convertBinary
                            let result = 
                                binirylist
                                |> Seq.map convertBinary
                                |> Seq.fold (fun acc bin -> acc + bin) 0
                            binirylist |> Seq.iter (printf "%s ") 
                            printfn "\n"
                            demonstration |> Seq.iter (printf "%A ")  
                            printfn "\n"
                            printfn "Сумма элементов списка = %A " result
                            continuation()
                elif number = 3 then
                    printf "Введите название каталога для поиска: "
                    let directory = Console.ReadLine()
                    printf "Введите название файла: "
                    let fileName = Console.ReadLine()
    
                    match directory, fileName with
                    | "", _ | _, "" -> printfn "Название директории и/или файла не введено"
                    | _ ->
                        match Directory.Exists(directory) with
                    | false -> printfn "Ошибка: директории не существует"
                    | true ->
                        let result = findFile directory fileName |> Seq.tryHead
                        match result with
                        | Some filePath -> printfn "Файл найден: %A" filePath
                        | None -> printfn "Файл не найден"
                    continuation()       
                else 
                    printfn "Нет операции с подходящим номером "
                    choice()
            | _ ->
                printfn "Введён не номер операции "
                choice()
 
    choice()

   


main()

