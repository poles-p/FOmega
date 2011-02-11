/// <summary>
/// Zmodyfikowany algorytm W rekonstrukcji typów i rodzajów.
/// </summary>
module AlgorytmW

open Core
open Sub
open BetaUnifikacja

/// <summary>
/// Rekonstrukcja rodzaju
/// </summary>
let rec rekRodzaj (gamma : KontekstTypowania) typ =
    match typ with
    | TWZmienna _ ->
        Some(Podstawienie [], KGwiazdka)
    | TZmienna x ->
        match gamma.SchematRodzaju x with
        | None ->
            System.Console.WriteLine("Undefined type variable {0}.", x);
            None
        | Some(kv, kind) ->
            let podst = Podstawienie(List.map (fun x -> PrzypisanieRodzaju(x, KWZmienna(Fresh.swierzaNazwa()))) kv);
            Some(Podstawienie[], podst.Aplikuj kind)
    | TFunkcja(t1, t2) ->
        opt{
            let! (s1, k1) = rekRodzaj gamma t1;
            let! (s2, k2) = rekRodzaj (s1.Aplikuj gamma) (s1.Aplikuj t2);
            let! (s3, _) = unifikuj(s2.Aplikuj k1, KGwiazdka);
            let! (s4, _) = unifikuj(s3.Aplikuj k2, KGwiazdka);
            return (s4 * s3 * s2 * s1, KGwiazdka)
        }
    | TUniwersalny(x, k, t) ->
        opt{
            let! (s1, kt) = rekRodzaj (gamma.Rozszerz <| SchematRodzaju(x, [], k)) t;
            let! (s2, _) = unifikuj(kt, KGwiazdka)
            return (s2 * s1, KGwiazdka)
        }
    | TLambda(x, k, t) ->
        opt{
            let! (s1, kt) = rekRodzaj (gamma.Rozszerz <| SchematRodzaju(x, [], k)) t;
            return (s1, KFunkcja(s1.Aplikuj k, kt))
        }
    | TAplikacja(t1, t2) ->
        opt{
            let! (s1, k1) = rekRodzaj gamma t1;
            let! (s2, k2) = rekRodzaj (s1.Aplikuj gamma) (s1.Aplikuj t2);
            let fresh = KWZmienna(Fresh.swierzaNazwa());
            let! (s3, _) = unifikuj(s2.Aplikuj k1, KFunkcja(k2, fresh))
            return (s3 * s2 * s1, s3.Aplikuj fresh)
        }
    | TAnotacja(t, k) ->
        opt{
            let! (s1, kt) = rekRodzaj gamma t;
            let! (s2, rk) = unifikuj(kt, s1.Aplikuj k);
            return (s2 * s1, rk)
        }
