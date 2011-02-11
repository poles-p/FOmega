
open Core
open Sub

let main =
    while true do
        System.Console.Write "> ";
        match System.Console.ReadLine() |> Parser.parsujText with
        | Parsor.Core.Success term ->
            try
                opt{
                    let! (s, typ) = AlgorytmW.rekTyp (KontekstTypowania[]) term;
                    let! (_, kind) = AlgorytmW.rekRodzaj (KontekstTypowania[]) typ;
                    System.Console.WriteLine("{0}  :  {1}  ::  {2}", s.Aplikuj term, typ, kind);
                    return ()
                } |> ignore
            with
            | SubstitutionException ex ->
                System.Console.WriteLine("Error : {0}", ex)
        | Parsor.Core.Error -> ()
    done
