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
    | PrzypisanieZabronione of string * string * Typ

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
                | PrzypisanieZabronione(y, _, _) -> y = x;
                | _ -> false
            match List.tryFind ok podstawienie with
            | Some(PrzypisanieTypu(_, t)) -> t
            | Some(PrzypisanieZabronione(_, ex, _)) -> SubstitutionException ex |> raise
            | _ -> typ
        | TWartosc(x, t) -> TWartosc(x, this.Aplikuj t)
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

    /// <summary>
    /// Aplikacja podstawienia do kontekstu
    /// </summary>
    member this.Aplikuj (gamma : KontekstTypowania) =
        let aplikuj schemat =
            match schemat with
            | SchematTypu(x, tv, kv, t) ->
                let tv2 = List.map (fun _ -> Fresh.swierzaNazwa()) tv;
                let kv2 = List.map (fun _ -> Fresh.swierzaNazwa()) kv;
                let pt = Podstawienie( List.map2 (fun x y -> PrzypisanieTypu(x, TWZmienna y)) tv tv2 );
                let pk = Podstawienie( List.map2 (fun x y -> PrzypisanieRodzaju(x, KWZmienna y)) kv kv2 );
                SchematTypu(x, tv2, kv2, (this * pk * pt).Aplikuj t)
            | SchematRodzaju(x, kv, k) ->
                let kv2 = List.map (fun _ -> Fresh.swierzaNazwa()) kv;
                let pk = Podstawienie( List.map2 (fun x y -> PrzypisanieRodzaju(x, KWZmienna y)) kv kv2 );
                SchematRodzaju(x, kv, (this * pk).Aplikuj k)
        in
            KontekstTypowania(List.map aplikuj gamma.Kontekst)

    /// <summary>
    /// Aplikacja podstawienia do typu, ignoruje podstawienia zabronione
    /// </summary>
    member this.FAplikuj typ =
        match typ with
        | TWZmienna x ->
            let ok p =
                match p with
                | PrzypisanieTypu(y, _)
                | PrzypisanieZabronione(y, _, _) -> y = x;
                | _ -> false
            match List.tryFind ok podstawienie with
            | Some(PrzypisanieTypu(_, t)) -> (true, System.String.Empty, t)
            | Some(PrzypisanieZabronione(_, ex, t)) -> (false, ex, t)
            | _ -> (true, System.String.Empty, typ)
        | TWartosc(x, t) ->
            let (rt, ex, t') = this.FAplikuj t;
            (rt, ex, TWartosc(x, t'))
        | TZmienna _ -> (true, System.String.Empty, typ)
        | TFunkcja(a, b) -> 
            let (ra, ex1, a') = this.FAplikuj a;
            let (rb, ex2, b') = this.FAplikuj b;
            (ra && rb, (if not ra then ex1 else ex2), TFunkcja(a', b'))
        | TLambda(x,k,t) -> 
            let (rt, ex, t') = this.FAplikuj t
            (rt, ex, TLambda(x, this.Aplikuj k, t'))
        | TAplikacja(t1,t2) -> 
            let (rt1, ex1, t1') = this.FAplikuj t1;
            let (rt2, ex2, t2') = this.FAplikuj t2;
            (rt1 && rt2, (if not rt1 then ex1 else ex2), TAplikacja(t1', t2'))
        | TUniwersalny(x,k,t) -> 
            let (rt, ex, t') = this.FAplikuj t;
            (rt, ex, TUniwersalny(x, this.Aplikuj k, t'))
        | TAnotacja(t, k) -> 
            let (rt, ex, t') = this.FAplikuj t;
            (rt, ex, TAnotacja(t', this.Aplikuj k))

    /// <summary>
    /// Aplikacja podstawienia do termu, ignoruje podstawienia zabronione
    /// </summary>
    member this.FAplikuj term =
        match term with
        | EZmienna _ -> (true, term)
        | ELambda(x, t, e) -> 
            let (rt, _, t') = this.FAplikuj t;
            let (re, e') = this.FAplikuj e;
            (rt && re, ELambda(x, t', e'))
        | EAplikacja(e1, e2) -> 
            let (re1, e1') = this.FAplikuj e1;
            let (re2, e2') = this.FAplikuj e2;
            (re1 && re2, EAplikacja(e1', e2'))
        | ETLambda(x, k, e) -> 
            let (re, e') = this.FAplikuj e;
            (re, ETLambda(x, this.Aplikuj k, e'))
        | ETAplikacja(e, t) ->
            let (re, e') = this.FAplikuj e;
            let (rt, _, t') = this.FAplikuj t;
            (re && rt, ETAplikacja(e', t'))
        | EAnotacja(e, t) -> 
            let (re, e') = this.FAplikuj e;
            let (rt, _, t') = this.FAplikuj t;
            (re && rt, EAnotacja(e', t'))
        | ELet(x, e1, e2) ->
            let (re1, e1') = this.FAplikuj e1;
            let (re2, e2') = this.FAplikuj e2;
            (re1 && re2, ELet(x, e1', e2'))
        | ETLet(x, t, e) -> 
            let (re, e') = this.FAplikuj e;
            let (rt, _, t') = this.FAplikuj t;
            (re && rt, ETLet(x, t', e'))

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
                        let (ok, ex, t') = s1.FAplikuj t;
                        if ok then
                            PrzypisanieTypu(x, t')
                        else
                            PrzypisanieZabronione(x, ex, t')
                    | PrzypisanieZabronione(x, ex, t) -> 
                        let (_, _, t') = s1.FAplikuj t;
                        PrzypisanieZabronione(x, ex, t') 
            ) |>
            List.append(
                let (domT, domK) = s2.Dziedzina in
                s1.Przypisania |>
                List.filter (
                    fun p ->
                        match p with
                        | PrzypisanieTypu(x, _)
                        | PrzypisanieZabronione(x, _, _) ->
                            List.forall (fun y -> x <> y) domT
                        | PrzypisanieRodzaju(x, _) ->         
                            List.forall (fun y -> x <> y) domK    
                )
            )
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
                        PrzypisanieZabronione(y, "Invalid use of bound variable " + x + " ouside the definition.", t)
                    | _ -> p
            )
        )

    /// <summary>
    /// Zmienne typowe i rodzajowe z dziedziny podstawienia
    /// </summary>
    member private this.Dziedzina =
        let rec dziedzina acct acck l =
            match l with
            | [] -> (acct, acck)
            | (PrzypisanieTypu(x, _))::xs
            | (PrzypisanieZabronione(x, _, _))::xs ->
                dziedzina (x::acct) acck xs
            | (PrzypisanieRodzaju(x, _))::xs ->
                dziedzina acct (x::acck) xs
        in dziedzina [] [] podstawienie
