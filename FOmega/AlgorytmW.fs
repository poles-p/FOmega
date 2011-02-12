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

/// <summary>
/// Rekonstrukcja typów
/// </summary>
let rec rekTyp (gamma : KontekstTypowania) term =
    match term with
    | EZmienna x ->
        match gamma.SchematTypu x with
        | None ->
            System.Console.WriteLine("Undefined variable {0}.", x);
            None
        | Some(tv, kv, typ) ->
            let przemianowanieT x = PrzypisanieTypu(x, Fresh.swierzaNazwa() |> TWZmienna);
            let przemianowanieK x = PrzypisanieRodzaju(x, Fresh.swierzaNazwa() |> KWZmienna);
            let podst = Podstawienie(List.append (List.map przemianowanieT tv) (List.map przemianowanieK kv));
            Some(Podstawienie[], podst.Aplikuj typ)
    | ELambda(x, t, e) ->
        opt{
            let! (s1, k) = rekRodzaj gamma t;
            let! (s2, _) = unifikuj(k, KGwiazdka);
            let s21 = s2 * s1;
            let! (s3, te) = rekTyp (s21.Aplikuj(gamma.Rozszerz (SchematTypu(x, [], [], t)))) (s21.Aplikuj e);
            let s321 = s3 * s21;
            return (s321, TFunkcja(s321.Aplikuj t, te))
        }
    | EAplikacja(e1, e2) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let! (s2, t2) = rekTyp (s1.Aplikuj gamma) (s1.Aplikuj e2);
            let fresh = TWZmienna(Fresh.swierzaNazwa());
            let! (s3, t3) = betaUnifikuj(s2.Aplikuj t1, TFunkcja(t2, fresh));
            match t3 with
            | TFunkcja(_, xv) ->
                return (s3*s2*s1, xv)
            | _ -> return! None
        }
    | ETLambda(x, k, e) ->
        opt{
            let! (s1, t) = rekTyp (gamma.Rozszerz(SchematRodzaju(x, [], k))) e;
            return (s1.UsunZmiennaTypowa x, TUniwersalny(x, s1.Aplikuj k, t))
        }
    | ETAplikacja(e, t) ->
        opt{
            let! (s1, te) = rekTyp gamma e;
            let! (s2, k) = rekRodzaj (s1.Aplikuj gamma) (s1.Aplikuj t);
            let freshX = TWZmienna(Fresh.swierzaNazwa());
            let y = Fresh.swierzaNazwa()
            let! (s3, res) = betaUnifikuj(s2.Aplikuj te, TUniwersalny(y, k, freshX));
            match res with
            | TUniwersalny(uy, uk, ut) ->
                let s321 = s3 * s2 * s1;
                return (s321, ut.Podstaw uy (s321.Aplikuj t))
            | _ -> return! None
        }
    | EAnotacja(e, t) ->
        opt{
            let! (s1, te) = rekTyp gamma e;
            let! (s2, k) = rekRodzaj (s1.Aplikuj gamma) (s1.Aplikuj t);
            let! (s3, _) = unifikuj(k, KGwiazdka);
            let! (s4, rt) = betaUnifikuj((s3*s2).Aplikuj te, (s3*s2*s1).Aplikuj t);
            return (s4 * s3 * s2 * s1, rt)
        }
    | ELet(x, e1, e2) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let tv = Set.toList (t1.WolneTWZmienne - gamma.WolneTWZmienne); // TODO: nie jestem pewien, czy trzeba wykonać podstawienie na gammie
            let kv = Set.toList (t1.WolneKWZmienne - gamma.WolneKWZmienne);
            let gamma2 = (s1.Aplikuj gamma).Rozszerz(SchematTypu(x, tv, kv, t1));
            let! (s2, t2) = rekTyp gamma2 (s1.Aplikuj e2);
            return (s2 * s1, t2)
        }
    | ETLet(x, t, e) ->
        opt{
            // TODO: wykomentowałem stare złe rozwiązanie, mam zamiar je przerobić na dobre tak, by drukować ładne komunikaty
            (* let! (s1, k) = rekRodzaj gamma t;
            let kv = Set.toList (k.WolneKWZmienne - gamma.WolneKWZmienne);
            let gamma2 = (s1.Aplikuj gamma).Rozszerz(SchematRodzaju(x, kv, k));
            let! (s2, tk) = rekTyp gamma2 (s1.Aplikuj e);
            let s21 = s2 * s1;
            return (s21, typBetaNormalny (tk.Podstaw x (s21.Aplikuj t))) // TODO: może nie trzeba robić beta redukcji typu. *)
            let! (s1, k) = rekRodzaj gamma t;
            let kv = Set.toList (k.WolneKWZmienne - gamma.WolneKWZmienne);
            let! (s2, tk) = rekTyp (s1.Aplikuj gamma) ((s1.Aplikuj e).PodstawKopieTypu x kv (s1.Aplikuj t));
            return (s2 * s1, tk)
        }
