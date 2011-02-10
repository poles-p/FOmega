
let main =
    while true do
        System.Console.Write "> ";
        match System.Console.ReadLine() |> Parser.parsujText with
        | Parsor.Core.Success kind ->
            System.Console.WriteLine kind
        | Parsor.Core.Error -> ()
    done
