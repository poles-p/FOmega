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
let rec rekRodzaj (gamma : KontekstTypowania) typ pos =
    match typ with
    | TWZmienna _ ->
        Some(Podstawienie [], KGwiazdka)
    | TWartosc(_, t) ->
        rekRodzaj gamma t pos
    | TZmienna(x, x') ->
        match gamma.SchematRodzaju x' with
        | None ->
            System.Console.WriteLine("Undefined type variable {0}.", x);
            None
        | Some(kv, kind) ->
            let podst = Podstawienie(List.map (fun x -> PrzypisanieRodzaju(x, KWZmienna(Fresh.swierzaNazwa()))) kv);
            Some(Podstawienie[], podst.Aplikuj kind)
    | TFunkcja(t1, t2) ->
        opt{
            let! (s1, k1) = rekRodzaj gamma t1 pos;
            let! (s2, k2) = rekRodzaj (s1.Aplikuj(pos, gamma)) (s1.Aplikuj(pos, t2)) pos;
            let! (s3, _) = unifikuj(s2.Aplikuj k1, KGwiazdka) pos;
            let! (s4, _) = unifikuj(s3.Aplikuj k2, KGwiazdka) pos;
            return (s4 * s3 * s2 * s1, KGwiazdka)
        }
    | TUniwersalny(x, x2, k, t) ->
        opt{
            let! (s1, kt) = rekRodzaj (gamma.Rozszerz <| SchematRodzaju(x2, [], k)) t pos;
            let! (s2, _) = unifikuj(kt, KGwiazdka) pos
            return (s2 * s1, KGwiazdka)
        }
    | TLambda(x, x2, k, t) ->
        opt{
            let! (s1, kt) = rekRodzaj (gamma.Rozszerz <| SchematRodzaju(x2, [], k)) t pos;
            return (s1, KFunkcja(s1.Aplikuj k, kt))
        }
    | TAplikacja(t1, t2) ->
        opt{
            let! (s1, k1) = rekRodzaj gamma t1 pos;
            let! (s2, k2) = rekRodzaj (s1.Aplikuj(pos, gamma)) (s1.Aplikuj(pos, t2)) pos;
            let fresh = KWZmienna(Fresh.swierzaNazwa());
            let! (s3, _) = unifikuj(s2.Aplikuj k1, KFunkcja(k2, fresh)) pos
            return (s3 * s2 * s1, s3.Aplikuj fresh)
        }
    | TAnotacja(t, k) ->
        opt{
            let! (s1, kt) = rekRodzaj gamma t pos;
            let! (s2, rk) = unifikuj(kt, s1.Aplikuj k) pos;
            return (s2 * s1, rk)
        }
    | TNat | TBool -> Some(Podstawienie[], KGwiazdka)
    | TPara(t1, t2) ->
        opt{
            let! (s1, k1) = rekRodzaj gamma t1 pos;
            let! (s2, k2) = rekRodzaj (s1.Aplikuj(pos, gamma)) (s1.Aplikuj(pos, t2)) pos;
            let! (s3, _) = unifikuj(s2.Aplikuj k1, KGwiazdka) pos;
            let! (s4, _) = unifikuj(s3.Aplikuj k2, KGwiazdka) pos;
            return (s4 * s3 * s2 * s1, KGwiazdka)
        }
    | TKopara(t1, t2) ->
        opt{
            let! (s1, k1) = rekRodzaj gamma t1 pos;
            let! (s2, k2) = rekRodzaj (s1.Aplikuj(pos, gamma)) (s1.Aplikuj(pos, t2)) pos;
            let! (s3, _) = unifikuj(s2.Aplikuj k1, KGwiazdka) pos;
            let! (s4, _) = unifikuj(s3.Aplikuj k2, KGwiazdka) pos;
            return (s4 * s3 * s2 * s1, KGwiazdka)
        }

/// <summary>
/// Rekonstrukcja typów
/// </summary>
let rec rekTyp (gamma : KontekstTypowania) term =
    match term with
    | EZmienna(x, pos) ->
        match gamma.SchematTypu x with
        | None ->
            System.Console.WriteLine("Error at {1} : Undefined variable {0}.", x, pos);
            None
        | Some(tv, kv, typ) ->
            let przemianowanieT x = PrzypisanieTypu(x, Fresh.swierzaNazwa() |> TWZmienna);
            let przemianowanieK x = PrzypisanieRodzaju(x, Fresh.swierzaNazwa() |> KWZmienna);
            let podst = Podstawienie(List.append (List.map przemianowanieT tv) (List.map przemianowanieK kv));
            Some(Podstawienie[], podst.Aplikuj(pos, typ))
    | ELambda(x, t, e, pos) ->
        opt{
            let! (s1, k) = rekRodzaj gamma t pos;
            let! (s2, _) = unifikuj(k, KGwiazdka) pos;
            let s21 = s2 * s1;
            let! (s3, te) = rekTyp (s21.Aplikuj(e.Polozenie, gamma.Rozszerz (SchematTypu(x, [], [], t)))) (s21.Aplikuj e);
            let s321 = s3 * s21;
            return (s321, TFunkcja(s321.Aplikuj(pos, t), te))
        }
    | EAplikacja(e1, e2, pos) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let! (s2, t2) = rekTyp (s1.Aplikuj(pos, gamma)) (s1.Aplikuj e2);
            let fresh = TWZmienna(Fresh.swierzaNazwa());
            let! (s3, t3) = betaUnifikuj(s2.Aplikuj(e1.Polozenie, t1), TFunkcja(t2, fresh)) pos;
            match t3 with
            | TFunkcja(_, xv) ->
                return (s3*s2*s1, xv)
            | _ -> return! None
        }
    | ETLambda(x, x2, k, e, pos) ->
        opt{
            let! (s1, t) = rekTyp (gamma.Rozszerz(SchematRodzaju(x2, [], k))) e;
            return (s1.UsunZmiennaTypowa x2, TUniwersalny(x, x2, s1.Aplikuj k, t))
        }
    | ETAplikacja(e, t, pos) ->
        opt{
            let! (s1, te) = rekTyp gamma e;
            let! (s2, k) = rekRodzaj (s1.Aplikuj(pos, gamma)) (s1.Aplikuj(e.Polozenie, t)) pos;
            let freshX = TWZmienna(Fresh.swierzaNazwa());
            let y = Fresh.swierzaNazwa()
            let! (s3, res) = betaUnifikuj(s2.Aplikuj(e.Polozenie, te), TUniwersalny(y, y, k, freshX)) e.Polozenie;
            match res with
            | TUniwersalny(uy, uy', uk, ut) ->
                let s321 = s3 * s2 * s1;
                return (s321, ut.Podstaw uy' (s321.Aplikuj(pos, t)))
            | _ -> return! None
        }
    | EAnotacja(e, t, pos) ->
        opt{
            let! (s1, te) = rekTyp gamma e;
            let! (s2, k) = rekRodzaj (s1.Aplikuj(pos, gamma)) (s1.Aplikuj(pos, t)) pos;
            let! (s3, _) = unifikuj(k, KGwiazdka) pos;
            let! (s4, rt) = betaUnifikuj((s3*s2).Aplikuj(pos, te), (s3*s2*s1).Aplikuj(pos, t)) pos;
            return (s4 * s3 * s2 * s1, rt)
        }
    | ELet(x, e1, e2, pos) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let tv = Set.toList (t1.WolneTWZmienne - gamma.WolneTWZmienne); // TODO: nie jestem pewien, czy trzeba wykonać podstawienie na gammie
            let kv = Set.toList (t1.WolneKWZmienne - gamma.WolneKWZmienne);
            let gamma2 = (s1.Aplikuj(pos, gamma)).Rozszerz(SchematTypu(x, tv, kv, t1));
            let! (s2, t2) = rekTyp gamma2 (s1.Aplikuj e2);
            return (s2 * s1, t2)
        }
    | ETLet(x, x2, t, e, pos) ->
        opt{
            let! (s1, k) = rekRodzaj gamma t pos;
            let kv = Set.toList (k.WolneKWZmienne - gamma.WolneKWZmienne);
            let! (s2, tk) = rekTyp (s1.Aplikuj(pos, gamma)) ((s1.Aplikuj e).PodstawKopieTypu x2 kv (s1.Aplikuj(pos, t)));
            return (s2 * s1, tk)
        }
    | ENat _ -> Some(Podstawienie[], TNat)
    | ETrue _ | EFalse _ -> Some(Podstawienie[], TBool)
    | EOpArytmetyczny(e1, e2, s, f, pos) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let! (s2, t2) = rekTyp (s1.Aplikuj(pos, gamma)) (s1.Aplikuj e2);
            let! (s3, _) = betaUnifikuj(s2.Aplikuj(e1.Polozenie, t1), TNat) e1.Polozenie;
            let! (s4, _) = betaUnifikuj(s3.Aplikuj(e2.Polozenie, t2), TNat) e2.Polozenie;
            return (s4 * s3 * s2 * s1, TNat)
        }
    | EOpPorownania(e1, e2, s, f, pos) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let! (s2, t2) = rekTyp (s1.Aplikuj(pos, gamma)) (s1.Aplikuj e2);
            let! (s3, _) = betaUnifikuj(s2.Aplikuj(e1.Polozenie, t1), TNat) e1.Polozenie;
            let! (s4, _) = betaUnifikuj(s3.Aplikuj(e2.Polozenie, t2), TNat) e2.Polozenie;
            return (s4 * s3 * s2 * s1, TBool)
        }
    | EIf(e1, e2, e3, pos) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let! (s2, t2) = rekTyp (s1.Aplikuj(pos, gamma)) (s1.Aplikuj e2);
            let s21 = s2 * s1;
            let! (s3, t3) = rekTyp (s21.Aplikuj(pos, gamma)) (s21.Aplikuj e3);
            let! (s4, _) = betaUnifikuj((s3*s2).Aplikuj(e1.Polozenie, t1), TBool) e1.Polozenie;
            let! (s5, rt) = betaUnifikuj((s4*s3).Aplikuj(e2.Polozenie, t2), s4.Aplikuj(e3.Polozenie, t3)) e2.Polozenie;
            return (s5 * s4 * s3 * s21, rt)
        }
    | EPara(e1, e2, pos) ->
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let! (s2, t2) = rekTyp (s1.Aplikuj(e2.Polozenie, gamma)) (s1.Aplikuj e2);
            return (s2 * s1, TPara(s2.Aplikuj(e1.Polozenie, t1), t2))
        }
    | EProjLewy(e, pos) ->
        opt{
            let! (s1, te) = rekTyp gamma e;
            let freshX = TWZmienna(Fresh.swierzaNazwa());
            let freshY = TWZmienna(Fresh.swierzaNazwa());
            let! (se, t) = betaUnifikuj(te, TPara(freshX, freshY)) pos;
            match t with
            | TPara(lt, _) ->
                return (se*s1, lt)
            | _ -> return! None
        }
    | EProjPrawy(e, pos) ->
        opt{
            let! (s1, te) = rekTyp gamma e;
            let freshX = TWZmienna(Fresh.swierzaNazwa());
            let freshY = TWZmienna(Fresh.swierzaNazwa());
            let! (se, t) = betaUnifikuj(te, TPara(freshX, freshY)) pos;
            match t with
            | TPara(_, rt) ->
                return (se*s1, rt)
            | _ -> return! None
        }
    | ELewy(e, pos) ->
        opt{
            let! (s1, t) = rekTyp gamma e;
            let fresh = TWZmienna(Fresh.swierzaNazwa());
            return (s1, TKopara(t, fresh))
        }
    | EPrawy(e, pos) ->
        opt{
            let! (s1, t) = rekTyp gamma e;
            let fresh = TWZmienna(Fresh.swierzaNazwa());
            return (s1, TKopara(fresh, t))
        }
    | ECase(e1, e2, e3, pos) -> 
        opt{
            let! (s1, t1) = rekTyp gamma e1;
            let! (s2, t2) = rekTyp (s1.Aplikuj(pos, gamma)) (s1.Aplikuj e2);
            let s21 = s2 * s1;
            let! (s3, t3) = rekTyp (s21.Aplikuj(pos, gamma)) (s21.Aplikuj e3);
            let freshX = TWZmienna(Fresh.swierzaNazwa());
            let freshY = TWZmienna(Fresh.swierzaNazwa());
            let! (s4, res) = betaUnifikuj((s3*s2).Aplikuj(e1.Polozenie, t1), TKopara(freshX, freshY)) e1.Polozenie;
            match res with
            | TKopara(typL, typR) ->
                let freshV = TWZmienna(Fresh.swierzaNazwa());
                let! (s5, rt) = betaUnifikuj((s4*s3).Aplikuj(e2.Polozenie, t2), TFunkcja(typL, freshV)) e2.Polozenie;
                match rt with
                | TFunkcja(_, vTyp) ->
                    let! (s6, _) = betaUnifikuj((s5*s4).Aplikuj(e3.Polozenie, t3), TFunkcja(typR, vTyp)) e3.Polozenie
                    return (s6 * s5 * s4 * s3 * s21, s6.Aplikuj(pos, vTyp) )
                | _ -> return! None
            | _ -> return! None
        }
    | EFix(x, e, pos) ->
        opt{
            let freshX = TWZmienna(Fresh.swierzaNazwa());
            let! (s1, te) = rekTyp (gamma.Rozszerz (SchematTypu(x, [], [], freshX))) e;
            let! (s2, res) = betaUnifikuj(s1.Aplikuj(pos, freshX), te) pos;
            return (s2 * s1, res);
        }
