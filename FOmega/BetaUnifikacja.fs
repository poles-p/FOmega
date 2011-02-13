/// <summary>
/// Beta unifikacja konstruktorów typów
/// </summary>
module BetaUnifikacja

open Core
open Sub

/// <summary>
/// Klasyczna unifikacja rodzajów traktowanych jako termy.
/// </summary>
let rec unifikuj(k1 : Rodzaj, k2 : Rodzaj) pos =
    match (k1, k2) with
    | (KGwiazdka, KGwiazdka) ->
        Some(Podstawienie[], KGwiazdka)
    | (KWZmienna x, k)
    | (k, KWZmienna x) ->
        if k.ZawieraZmiennaRodzajowa x then
            System.Console.WriteLine("Error at {2} : Can not unify {0} with {1}.", x, k, pos);
            None
        else Some(Podstawienie[PrzypisanieRodzaju(x,k)], k)
    | (KFunkcja(k1, l1), KFunkcja(k2, l2)) ->
        opt{
            let! (p, k) = unifikuj(k1, k2) pos;
            let! (p2, l) = unifikuj(p.Aplikuj l1, p.Aplikuj l2) pos;
            return (p2 * p, KFunkcja(p2.Aplikuj k, l))
        }
    | _ ->
        System.Console.WriteLine("Error at {2} : Can not unify {0} with {1}.", k1, k2, pos);
        None

/// <summary>
/// Beta redukcja konstruktorów typów. Funkcja wykonuje jeden krok w porządku gorliwym.
/// </summary>
let rec betaRedukuj t =
    match t with
    | TAplikacja(TLambda(x, x2, k, t1), t2) ->
        match betaRedukuj t2 with
        | None -> Some(t1.Podstaw x2 t2)
        | Some t2' -> Some(TAplikacja(TLambda(x, x2, k, t1), t2))
    | TAplikacja(t1, t2) ->
        opt{
            let! t1' = betaRedukuj t1;
            return TAplikacja(t1', t2)
        }
    | TWartosc(x, t2) ->
        match betaRedukuj t2 with
        | None -> Some t2
        | Some t2' -> Some( TWartosc(x, t2') )
    | _ -> None

/// <summary>
/// Beta unifikacja konstruktorów typów.
/// </summary>
let rec betaUnifikuj (t1, t2) pos =
    match (t1, t2) with
    | (TWZmienna x, TWZmienna y) when x=y ->
        Some(Podstawienie[], TWZmienna x)
    | (TZmienna(x, x2), TZmienna(y, y2)) ->
        if x2 = y2 then
            Some(Podstawienie[], TZmienna(x, x2))
        else
            System.Console.WriteLine("Error at {2} : Can not unify {0} with {1}.", t1, t2, pos);
            None
    | (TNat, TNat) -> Some(Podstawienie[], TNat)
    | (TBool, TBool) -> Some(Podstawienie[], TBool)
    | (TWZmienna x, t)
    | (t, TWZmienna x) ->
        if t.ZawieraZmiennaTypowaW x then
            System.Console.WriteLine("Error at {2} : Can not unify {0} with {1}.", t1, t2, pos);
            None
        else
            Some(Podstawienie[PrzypisanieTypu(x, t)], t)
    | (TUniwersalny(x1, x1', k1, t1), TUniwersalny(x2, x2', k2, t2)) ->
        opt{
            let! (sk, k) = unifikuj(k1, k2) pos;
            let x3 = Fresh.swierzaNazwa();
            let! (st, t) = betaUnifikuj (sk.Aplikuj(pos, t1.Podstaw x1' (TZmienna(x1,x3))), sk.Aplikuj(pos, t2.Podstaw x2' (TZmienna(x1,x3)))) pos;
            return (st.UsunZmiennaTypowa x3 * sk, TUniwersalny(x1, x3, st.Aplikuj k, t))
        }
    | (TLambda(x1, x1', k1, t1), TLambda(x2, x2', k2, t2)) ->
        opt{
            let! (sk, k) = unifikuj(k1, k2) pos;
            let x3 = Fresh.swierzaNazwa();
            let! (st, t) = betaUnifikuj(sk.Aplikuj(pos, t1.Podstaw x1' (TZmienna(x1,x3))), sk.Aplikuj(pos, t2.Podstaw x2' (TZmienna(x1,x3)))) pos;
            return (st.UsunZmiennaTypowa x3 * sk, TLambda(x1, x3, st.Aplikuj k, t))
        }
    | (TFunkcja(t1, s1), TFunkcja(t2, s2)) ->
        opt{
            let! (st, t) = betaUnifikuj(t1, t2) pos;
            let! (ss, s) = betaUnifikuj(st.Aplikuj(pos,s1), st.Aplikuj(pos,s2)) pos;
            return (ss * st, TFunkcja(ss.Aplikuj(pos, t), s))
        }
    | (TPara(t1, s1), TPara(t2, s2)) ->
        opt{
            let! (st, t) = betaUnifikuj(t1, t2) pos;
            let! (ss, s) = betaUnifikuj(st.Aplikuj(pos,s1), st.Aplikuj(pos,s2)) pos;
            return (ss * st, TPara(ss.Aplikuj(pos, t), s))
        }
    | (TKopara(t1, s1), TKopara(t2, s2)) ->
        opt{
            let! (st, t) = betaUnifikuj(t1, t2) pos;
            let! (ss, s) = betaUnifikuj(st.Aplikuj(pos,s1), st.Aplikuj(pos,s2)) pos;
            return (ss * st, TKopara(ss.Aplikuj(pos, t), s))
        }
    | (TWartosc(x, t1), t2)
    | (t1, TWartosc(x, t2)) ->
        opt{
            let! (st, t) = betaUnifikuj(t1, t2) pos;
            return (st, TWartosc(x, t))
        }
    | _ ->
        match betaRedukuj t1 with
        | Some t1' -> betaUnifikuj(t1', t2) pos
        | _ ->
        match betaRedukuj t2 with
        | Some t2' -> betaUnifikuj(t1, t2') pos
        | _ ->
        match (t1, t2) with
        | (TAplikacja(v1, a1), TAplikacja(v2, a2)) ->
            opt{
                let! (sv, v) = betaUnifikuj(v1, v2) pos;
                let! (sa, a) = betaUnifikuj(sv.Aplikuj(pos, a1), sv.Aplikuj(pos, a2)) pos;
                return (sa * sv, TAplikacja(sa.Aplikuj(pos, v), a))
            }
        | _ ->
            System.Console.WriteLine("Error at {2} : Can not unify {0} with {1}.", t1, t2, pos);
            None

/// <summary>
/// Znajduje postać beta normalną typu.
/// </summary>
let rec typBetaNormalny typ = // TODO: zrobić to porządnie
    match typ with
    | TWZmienna _
    | TZmienna _ -> typ
    | TWartosc(x, t) -> typBetaNormalny t
    | TFunkcja(a,b) ->
        TFunkcja(typBetaNormalny a, typBetaNormalny b)
    | TLambda(x, x2, k, t) ->
        TLambda(x, x2, k, typBetaNormalny t)
    | TAplikacja(a, b) ->
        match typBetaNormalny a with
        | TLambda(x, x2, _, a') ->
            a'.Podstaw x2 (typBetaNormalny b)
        | a' -> TAplikacja(a', typBetaNormalny b)
    | TUniwersalny(x, x2, k, t) ->
        TUniwersalny(x, x2, k, typBetaNormalny t)
    | TAnotacja(t, _) -> 
        typBetaNormalny t
    | TNat | TBool -> typ
    | TPara(t1, t2) -> TPara(typBetaNormalny t1, typBetaNormalny t2)
    | TKopara(t1, t2) -> TKopara(typBetaNormalny t1, typBetaNormalny t2)