/// <summary>
/// W tym module znajduje się definicja podstawienia potrzebna przy rekonstrukcji typów.
/// </summary>
module Sub

open Core

/// <summary>
/// Wyjątek rzucany w przypadku, gdy podstawienie nie jest możliwe (występuje w nim przypisanie X := fail).
/// Niesie ono informację jakiego podstawienia nie wolno wykonać.
/// </summary>
exception SubstitutionException of string

/// <summary>
/// Pojedyncze przypisanie w podstawieniu.
/// </summary>
type Przypisanie =
    /// <summary> Przypisanie rodzaju do zmiennej rodzajowej </summary>
    | PrzypisanieRodzaju of string * Rodzaj
    /// <summary> Przypisanie rodzaju do zmiennej typowej (kwantyfikowanej schematem) </summary>
    | PrzypisanieTypu of string * Typ
    /// <summary> Zabronione przypisanie </summary>
    /// <remarks> może się pojawić, gdy używamy przypisania zawierającego zmienne lokalne w obszarze gdzie one nie obowiązują </remarks>
    | PrzypisanieZabronione of string * string

/// <summary>
/// Podstawienie (za zmienne schematowe).
/// </summary>
type Podstawienie(podstawienie : Przypisanie list) =

    /// <summary>
    /// Aplikacja podstawienia do rodzaju
    /// </summary>
    member this.Aplikuj rodzaj =
        match rodzaj with
        | KWZmienna x ->
            let ok p =
                match p with
                | PrzypisanieRodzaju(y, _) -> y = x;
                | _ -> false
            match List.tryFind ok podstawienie with
            | Some(PrzypisanieRodzaju(_, k)) -> k
            | _ -> rodzaj
        | KGwiazdka -> rodzaj
        | KFunkcja(a,b) -> KFunkcja(this.Aplikuj a, this.Aplikuj b)

    /// <summary>
    /// Aplikacja podstawienia do typu
    /// </summary>
    member this.Aplikuj typ =
        match typ with
        | TWZmienna x ->
            let ok p =
                match p with
                | PrzypisanieTypu(y, _)
                | PrzypisanieZabronione(y, _) -> y = x;
                | _ -> false
            match List.tryFind ok podstawienie with
            | Some(PrzypisanieTypu(_, t)) -> t
            | Some(PrzypisanieZabronione(_, ex)) -> SubstitutionException ex |> raise
            | _ -> typ
        | TZmienna _ -> typ
        | TFunkcja(a, b) -> TFunkcja(this.Aplikuj a, this.Aplikuj b)
        | TLambda(x,k,t) -> TLambda(x, this.Aplikuj k, this.Aplikuj t)
        | TAplikacja(t1,t2) -> TAplikacja(this.Aplikuj t1, this.Aplikuj t2)
        | TUniwersalny(x,k,t) -> TUniwersalny(x, this.Aplikuj k, this.Aplikuj t)
        | TAnotacja(t, k) -> TAnotacja(this.Aplikuj t, this.Aplikuj k)

    /// <summary>
    /// Aplikacja podstawienia do termu
    /// </summary>
    member this.Aplikuj term =
        match term with
        | EZmienna _ -> term
        | ELambda(x, t, e) -> ELambda(x, this.Aplikuj t, this.Aplikuj e)
        | EAplikacja(e1, e2) -> EAplikacja(this.Aplikuj e1, this.Aplikuj e2)
        | ETLambda(x, k, e) -> ETLambda(x, this.Aplikuj k, this.Aplikuj e)
        | ETAplikacja(e, t) -> ETAplikacja(this.Aplikuj e, this.Aplikuj t)
        | EAnotacja(e, t) -> EAnotacja(this.Aplikuj e, this.Aplikuj t)
        | ELet(x, e1, e2) -> ELet(x, this.Aplikuj e1, this.Aplikuj e2)
        | ETLet(x, t, e) -> ETLet(x, this.Aplikuj t, this.Aplikuj e)

    member private this.Przypisania =
        podstawienie

    /// <summary>
    /// Składanie podstawień.
    /// </summary>
    static member ( * ) (s1 : Podstawienie, s2 : Podstawienie) =
        Podstawienie(
            s2.Przypisania |>
            List.map (
                fun p ->
                    match p with
                    | PrzypisanieRodzaju(x, k) ->
                        PrzypisanieRodzaju(x, s1.Aplikuj k)
                    | PrzypisanieTypu(x, t) ->
                        try
                            PrzypisanieTypu(x, s1.Aplikuj t)
                        with
                        | SubstitutionException ex -> PrzypisanieZabronione(x, ex)
                    | PrzypisanieZabronione _ -> p
            ) |>
            List.append s1.Przypisania
        )

    /// <summary>
    /// Usuwanie zmiennych typowych z podstawienia.
    /// </summary>
    member this.UsunZmiennaTypowa x = // TODO: dwie różne zmienne mogą mieć tę samą nazwę!
        Podstawienie(
            podstawienie |>
            List.map (
                fun p ->
                    match p with
                    | PrzypisanieTypu(y, t) when t.ZawieraZmiennaTypowa x ->
                        PrzypisanieZabronione(y, "Invalid use of bound variable " + x + " ouside the definition.")
                    | _ -> p
            )
        )