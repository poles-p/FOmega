/// <summary>
/// Beta unifikacja konstruktorów typów
/// </summary>
module BetaUnifikacja

open Core
open Sub

/// <summary>
/// Klasyczna unifikacja rodzajów traktowanych jako termy.
/// </summary>
let rec unifikuj(k1 : Rodzaj, k2 : Rodzaj) =
    match (k1, k2) with
    | (KGwiazdka, KGwiazdka) ->
        Some(Podstawienie[], KGwiazdka)
    | (KWZmienna x, k)
    | (k, KWZmienna x) ->
        if k.ZawieraZmiennaRodzajowa x then
            System.Console.WriteLine("Can not unify {0} with {1}.", x, k);
            None
        else Some(Podstawienie[PrzypisanieRodzaju(x,k)], k)
    | (KFunkcja(k1, l1), KFunkcja(k2, l2)) ->
        opt{
            let! (p, k) = unifikuj(k1, k2);
            let! (p2, l) = unifikuj(p.Aplikuj l1, p.Aplikuj l2);
            return (p2 * p, KFunkcja(p2.Aplikuj k, l))
        }
    | _ ->
        System.Console.WriteLine("Can not unify {0} with {1}.", k1, k2);
        None

/// <summary>
/// Beta redukcja konstruktorów typów. Funkcja wykonuje jeden krok w porządku gorliwym.
/// </summary>
let rec betaRedukuj t =
    match t with
    | TAplikacja(TLambda(x, k, t1), t2) ->
        match betaRedukuj t2 with
        | None -> Some(t1.Podstaw x t2)
        | Some t2' -> Some(TAplikacja(TLambda(x, k, t1), t2))
    | TAplikacja(t1, t2) ->
        opt{
            let! t1' = betaRedukuj t1;
            return TAplikacja(t1', t2)
        }
    | _ -> None

/// <summary>
/// Beta unifikacja konstruktorów typów.
/// </summary>
let rec betaUnifikuj(t1, t2) =
    match (t1, t2) with
    | (TZmienna x, TZmienna y) ->
        if x = y then
            Some(Podstawienie[], TZmienna x)
        else
            System.Console.WriteLine("Can not unify {0} with {1}.", t1, t2);
            None
    | (TWZmienna x, t)
    | (t, TWZmienna x) ->
        if t.ZawieraZmiennaTypowaW x then
            System.Console.WriteLine("Can not unify {0} with {1}.", t1, t2);
            None
        else
            Some(Podstawienie[PrzypisanieTypu(x, t)], t)
    | (TUniwersalny(x1, k1, t1), TUniwersalny(x2, k2, t2)) ->
        opt{
            let! (sk, k) = unifikuj(k1, k2);
            let! (st, t) = betaUnifikuj(sk.Aplikuj t1, sk.Aplikuj(t2.Podstaw x2 (TZmienna x1)));
            return (st.UsunZmiennaTypowa x1 * sk, TUniwersalny(x1, st.Aplikuj k, t))
        }
    | (TLambda(x1, k1, t1), TLambda(x2, k2, t2)) ->
        opt{
            let! (sk, k) = unifikuj(k1, k2);
            let! (st, t) = betaUnifikuj(sk.Aplikuj t1, sk.Aplikuj(t2.Podstaw x2 (TZmienna x1)));
            return (st.UsunZmiennaTypowa x1 * sk, TLambda(x1, st.Aplikuj k, t))
        }
    | (TFunkcja(t1, s1), TFunkcja(t2, s2)) ->
        opt{
            let! (st, t) = betaUnifikuj(t1, t2);
            let! (ss, s) = betaUnifikuj(st.Aplikuj s1, st.Aplikuj s2);
            return (ss * st, TFunkcja(ss.Aplikuj t, s))
        }
    | _ ->
        match betaRedukuj t1 with
        | Some t1' -> betaUnifikuj(t1', t2)
        | _ ->
        match betaRedukuj t2 with
        | Some t2' -> betaUnifikuj(t1, t2')
        | _ ->
        match (t1, t2) with
        | (TAplikacja(v1, a1), TAplikacja(v2, a2)) ->
            opt{
                let! (sv, v) = betaUnifikuj(v1, v2);
                let! (sa, a) = betaUnifikuj(sv.Aplikuj a1, sv.Aplikuj a2);
                return (sa * sv, TAplikacja(sa.Aplikuj v, a))
            }
        | _ ->
            System.Console.WriteLine("Can not unify {0} with {1}.", t1, t2);
            None