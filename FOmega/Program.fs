
open Core
open Sub

let main =
    while true do
        System.Console.Write "> ";
        let ins = System.Console.ReadLine();
        if ins.Length > 0 then
            try
                match (if ins.[0] = '@' then System.IO.File.ReadAllText(ins.Substring 1) else ins) |> Parser.parsujText with
                | Parsor.Core.Success term ->
                    opt{
                        let! (s, typ) = AlgorytmW.rekTyp (KontekstTypowania[]) term;
                        let! (_, kind) = AlgorytmW.rekRodzaj (KontekstTypowania[]) typ;
                        System.Console.WriteLine("{0}  :  {1}  ::  {2}", s.Aplikuj term, typ, kind);
                        return ()
                    } |> ignore                
                | Parsor.Core.Error -> ()
            with
            | SubstitutionException ex ->
                System.Console.WriteLine("Error : {0}", ex)
            | :? System.IO.IOException as ex ->
                System.Console.WriteLine("IO Exception : {0}", ex.Message)
    done
