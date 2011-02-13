/// <summary>
/// Moduł interpretera.
/// </summary>
module Interpreter

open Core

/// <summary>
/// Wyjątek rzucany, gdy wartość nie chce się obliczyć do wartości. Jeżeli program się otypuje,
/// Wyjątek nie powinien zostać rzucony.
/// </summary>
exception FOmegaRuntimeException of string * string

/// <summary>
/// oblicza wyrażenie zgodnie z semantyką dużych kroków w porządku gorliwym
/// </summary>
let rec oblicz t =
    match t with
    | EZmienna(x, pos) ->
        FOmegaRuntimeException(pos.ToString(), "Can not evaluate variable " + x + ".") |> raise
    | ELambda _ -> t
    | EAplikacja(f, a, pos) ->
        match oblicz f with
        | ELambda(x, _, t, _) ->
            t.Podstaw x (oblicz a) |> oblicz
        | t -> FOmegaRuntimeException(f.Polozenie.ToString() , t.ToString() + " is not a function.") |> raise
    | ETLambda _ -> t
    | ETAplikacja(f, typ, pos) ->
        match oblicz f with
        | ETLambda(_, x, _, t, _) ->
            t.PodstawTyp x typ |> oblicz
        | t -> FOmegaRuntimeException(f.Polozenie.ToString(), t.ToString() + " is not a type function.") |> raise
    | EAnotacja(t, _, _) -> oblicz t
    | ELet(x, t1, t2, _) ->
        t2.Podstaw x (oblicz t1) |> oblicz
    | ETLet(_, x, typ, t, _) ->
        t.PodstawTyp x typ |> oblicz
    | ENat _ | ETrue _ | EFalse _ -> t
    | EOpArytmetyczny(e1, e2, _, f, pos) ->
        match oblicz e1 with
        | ENat(n1, _) ->
            match oblicz e2 with
            | ENat(n2, _) ->
                ENat(f n1 n2, pos)
            | t -> FOmegaRuntimeException(e2.Polozenie.ToString(), t.ToString() + " is not a number.") |> raise
        | t -> FOmegaRuntimeException(e1.Polozenie.ToString(), t.ToString() + " is not a number.") |> raise
    | EOpPorownania(e1, e2, _, f, pos) ->
        match oblicz e1 with
        | ENat(n1, _) ->
            match oblicz e2 with
            | ENat(n2, _) ->
                if f n1 n2 then ETrue pos else EFalse pos
            | t -> FOmegaRuntimeException(e2.Polozenie.ToString(), t.ToString() + " is not a number.") |> raise
        | t -> FOmegaRuntimeException(e1.Polozenie.ToString(), t.ToString() + " is not a number.") |> raise
    | EIf(e1, e2, e3, pos) ->
        match oblicz e1 with
        | ETrue _ -> oblicz e2
        | EFalse _ -> oblicz e3
        | t -> FOmegaRuntimeException(t.Polozenie.ToString(), t.ToString() + " is not a bool.") |> raise
