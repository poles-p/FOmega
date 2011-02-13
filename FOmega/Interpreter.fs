/// <summary>
/// Moduł interpretera.
/// </summary>
module Interpreter

open Core

/// <summary>
/// Wyjątek rzucany, gdy wartość nie chce się obliczyć do wartości. Jeżeli program się otypuje,
/// Wyjątek nie powinien zostać rzucony.
/// </summary>
exception FOmegaRuntimeException of string

/// <summary>
/// oblicza wyrażenie zgodnie z semantyką dużych kroków w porządku gorliwym
/// </summary>
let rec oblicz t =
    match t with
    | EZmienna x ->
        FOmegaRuntimeException("Can not evaluate variable " + x + ".") |> raise
    | ELambda _ -> t
    | EAplikacja(f, a) ->
        match oblicz f with
        | ELambda(x, _, t) ->
            t.Podstaw x (oblicz a) |> oblicz
        | t -> FOmegaRuntimeException(t.ToString() + " is not a function.") |> raise
    | ETLambda _ -> t
    | ETAplikacja(f, typ) ->
        match oblicz f with
        | ETLambda(_, x, _, t) ->
            t.PodstawTyp x typ |> oblicz
        | t -> FOmegaRuntimeException(t.ToString() + " is not a type function.") |> raise
    | EAnotacja(t, _) -> oblicz t
    | ELet(x, t1, t2) ->
        t2.Podstaw x (oblicz t1) |> oblicz
    | ETLet(_, x, typ, t) ->
        t.PodstawTyp x typ |> oblicz