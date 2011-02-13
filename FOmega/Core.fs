/// <summary>
/// W tym module znajdują się definicje typów reprezentujących 
/// wyrażenia, typy i rodzaje
/// </summary>
module Core

/// <summary>
/// Moduł generujący świerze nazwy
/// </summary>
module Fresh =
    /// <summary>
    /// Licznik, zwiększając go przy każdym wygenerowaniu nowej nazwy, gwarantujemy,
    /// że wygenerujemy nie występującą nigdzie nazwę.
    /// </summary>
    let mutable private swierzyLicznik = 0L
    /// <summary>
    /// Wygeneruj świerzą nazwę. Wygenerowana nazwa jest postaci "^x{numer}".
    /// Nie dopuszczając takich nazw przy parsowaniu, mamy pewność, że wygenerowaliśmy
    /// unikatową nazwę.
    /// </summary>
    /// <returns> Funkcja zwraca świerzą, unikatową nazwę. </returns>
    let swierzaNazwa() =
        swierzyLicznik <- swierzyLicznik + 1L;
        "^x" + swierzyLicznik.ToString()

/// <summary>
/// Rodzaj 
/// </summary>
type Rodzaj =
    /// <summary> zmienna rodzajowa (potrzebna przy rekonstrukcji rodzaju) </summary>
    | KWZmienna  of string
    /// <summary> rodzaj wszystkich typow ( * ) </summary>
    | KGwiazdka
    /// <summary> rodzaj funkcji typowej (K => K) </summary>
    | KFunkcja  of Rodzaj * Rodzaj
    /// <summary>
    /// Wykonuje podstawienie rodzaju <paramref name="kind"/> za wszystkie wystąpienia zmiennej <paramref name="x"/>
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="kind"> rodzaj który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy rodzaj po wykonaniu podstawienia. </returns>
    member this.WPodstaw x kind =
        match this with
        | KWZmienna y when x = y -> kind
        | KFunkcja(k1,k2) -> KFunkcja(k1.WPodstaw x kind, k2.WPodstaw x kind)
        | _ -> this

    /// <summary>
    /// Wykonuje podstawienie jednocześnie kilku zmiennych
    /// </summary>
    /// <param name="sub"> lista par nazwa-wartość, która odpowiada wykonywanemu podstawieniu </param>
    /// <returns> Funkcja zwraca nowy rodzaj po wykonaniu podstawienia. </returns>
    member this.WPodstawKilka sub =
        match this with
        | KWZmienna x ->
            match List.tryFind (fun p -> x = fst p) sub with
            | None -> this
            | Some(_, kind) -> kind
        | KFunkcja(k1,k2) -> KFunkcja(k1.WPodstawKilka sub, k2.WPodstawKilka sub)
        | _ -> this

    /// <summary>
    /// Sprawdza, czy dany rodzaj zawiera wystąpienie podanej zmiennej rodzajowej.
    /// </summary>
    /// <param name="x"> nazwa szukanej zmiennej rodzajowej </param>
    /// <returns> 
    /// Funkcja zwraca wartość prawdziwą, jeśli istnieje zmienna rodzajowa o podanej nazwie,
    /// w przeciwnym wypadku zwraca wartość fałszywą.
    /// </returns>
    member this.ZawieraZmiennaRodzajowa x =
        match this with
        | KWZmienna y -> x = y
        | KGwiazdka -> false
        | KFunkcja(k1, k2) -> k1.ZawieraZmiennaRodzajowa x || k2.ZawieraZmiennaRodzajowa x

    /// <summary>
    /// Wolne zmienne kwantyfikowane schematem
    /// </summary>
    /// <returns>
    /// Właściwość zwiera zbiór zmiennych wolnych kwantyfikowanych schematem.
    /// </returns>
    member this.WolneKWZmienne =
        match this with
        | KWZmienna x -> Set.singleton x
        | KGwiazdka -> Set.empty
        | KFunkcja(a,b) -> Set.union a.WolneKWZmienne b.WolneKWZmienne

    /// <summary>
    /// Zamienia rodzaj na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <param name="prior"> oczekiwany priorytet wyrażenia. Zero jeśli nie chcemy całego wyrażenia w nawiasie </param>
    /// <returns> Ciąg znaków reprezentujący rodzaj </returns>
    member this.ToString prior =
        match this with
        | KWZmienna x -> x
        | KGwiazdka -> "*"
        | KFunkcja(a,b) ->
            let res = a.ToString 1 + " => " + b.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
    /// <summary>
    /// Zamienia rodzaj na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <returns> Ciąg znaków reprezentujący rodzaj </returns>
    override this.ToString() =
        this.ToString 0

/// <summary>
/// Konstruktor typu
/// </summary>
type Typ =
    /// <summary> zmienna typowa kwantyfikowana schematem typowy </summary>
    | TWZmienna        of string
    /// <summary> zmienna typowa kwantyfikowana abstrakcja po wykonaniu podstawienia </summary>
    /// <remarks> Jest to to samo co typ, jest to wprowadzone po to by ładnie drukować typy </remarks>
    | TWartosc        of string * Typ
    /// <summary> zmienna typowa kwantyfikowana abstrakcją </summary>
    /// <remarks> Zmienna ma dwie nazwy: jedną podaną przez użytkownika, drugą unikatową dla samego systemu typów.</remarks>
    | TZmienna         of string * string
    /// <summary> typ funkcji (T -> T) </summary>
    | TFunkcja         of Typ * Typ
    /// <summary> konstruktor abstrakcji z anotowanym argumentem </summary>
    | TLambda          of string * string * Rodzaj * Typ
    /// <summary> konstruktor aplikacji typowe  </summary>
    | TAplikacja       of Typ * Typ
    /// <summary> typ uniwersalny anotowany </summary>
    | TUniwersalny     of string * string * Rodzaj * Typ
    /// <summary> anotacja rodzajowa (T :: K) </summary>
    | TAnotacja        of Typ * Rodzaj
    /// <summary> typ całkowitoliczbowy </summary>
    | TNat
    /// <summary> typ logiczny </summary>
    | TBool
    /// <summary> typ pary </summary>
    | TPara of Typ * Typ
    /// <summary> typ kopary (wariantu dwuelementowego) </summary>
    | TKopara of Typ * Typ

    /// <summary>
    /// Wykonuje podstawienie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej schematem
    /// </summary>
    /// <param name="x"> nazwa zmiennej (wprowadzona przez system typów) za którą należy wykonać podstawienie </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy konstruktor typu po wykonaniu podstawienia. </returns>
    member this.WPodstaw x (typ : Typ) =
        // Tu nie zrobimy sobie krzywdy, przy lambdzie bo rozróżniamy dwie klasy zmiennych typowych:
        // tych kwantyfikowanych schematem, i tych kwantyfikowanych abstrakcją
        match this with
        | TWZmienna y when x = y  -> typ.AlfaKopia
        | TWartosc(y,t)           -> TWartosc(y, t.WPodstaw x typ)
        | TFunkcja(t1,t2)         -> TFunkcja(t1.WPodstaw x typ, t2.WPodstaw x typ)
        | TLambda(y,y2,k,t)       -> TLambda(y, y2, k, t.WPodstaw x typ) 
        | TAplikacja(t1,t2)       -> TAplikacja(t1.WPodstaw x typ, t2.WPodstaw x typ)
        | TUniwersalny(y,y2,k, t) -> TUniwersalny(y, y2, k, t.WPodstaw x typ)
        | TAnotacja(t,k)          -> TAnotacja(t.WPodstaw x typ, k)
        | TPara(t1, t2)           -> TPara(t1.WPodstaw x typ, t2.WPodstaw x typ)
        | TKopara(t1, t2)         -> TKopara(t1.WPodstaw x typ, t2.WPodstaw x typ)
        | _ -> this
    /// <summary>
    /// Wykonuje podstawienie rodzaju <paramref name="kind"/> za wszystkie wystąpienia 
    /// zmiennej rodzajowej <paramref name="x"/>.
    /// </summary>
    /// <param name="x"> nazwa zmiennej rodzajowej za którą należy wykonać podstawienie </param>
    /// <param name="kind"> rodzaj który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy konstruktor typu po wykonaniu podstawienia. </returns>
    member this.WPodstawRodzaj x kind =
        match this with
        | TWartosc(y,t)           -> TWartosc(y,t.WPodstawRodzaj x kind)
        | TFunkcja(t1,t2)         -> TFunkcja(t1.WPodstawRodzaj x kind, t2.WPodstawRodzaj x kind)
        | TLambda(y,y2,k,t)       -> TLambda(y, y2, k.WPodstaw x kind, t.WPodstawRodzaj x kind)
        | TAplikacja(t1,t2)       -> TAplikacja(t1.WPodstawRodzaj x kind, t2.WPodstawRodzaj x kind)
        | TUniwersalny(y,y2,k,t)  -> TUniwersalny(y,y2,k.WPodstaw x kind, t.WPodstawRodzaj x kind)
        | TAnotacja(t,k)          -> TAnotacja(t.WPodstawRodzaj x kind, k.WPodstaw x kind)
        | TPara(t1,t2)            -> TPara(t1.WPodstawRodzaj x kind, t2.WPodstawRodzaj x kind)
        | TKopara(t1,t2)          -> TKopara(t1.WPodstawRodzaj x kind, t2.WPodstawRodzaj x kind)
        | _ -> this
    /// <summary>
    /// Wykonuje podstawienie jednocześnie kilku zmiennych
    /// </summary>
    /// <param name="sub"> lista par nazwa-wartość, która odpowiada wykonywanemu podstawieniu </param>
    /// <returns> Funkcja zwraca nowy konstruktor typu po wykonaniu podstawienia. </returns>
    member this.WPodstawKilkaRodzajow sub =
        match this with
        | TWartosc(y,t)          -> TWartosc(y, t.WPodstawKilkaRodzajow sub)
        | TFunkcja(t1,t2)        -> TFunkcja(t1.WPodstawKilkaRodzajow sub, t2.WPodstawKilkaRodzajow sub)
        | TLambda(y,y2,k,t)      -> TLambda(y, y2, k.WPodstawKilka sub, t.WPodstawKilkaRodzajow sub)
        | TAplikacja(t1,t2)      -> TAplikacja(t1.WPodstawKilkaRodzajow sub, t2.WPodstawKilkaRodzajow sub)
        | TUniwersalny(y,y2,k,t) -> TUniwersalny(y,y2,k.WPodstawKilka sub, t.WPodstawKilkaRodzajow sub)
        | TAnotacja(t,k)         -> TAnotacja(t.WPodstawKilkaRodzajow sub, k.WPodstawKilka sub)
        | TPara(t1,t2)           -> TPara(t1.WPodstawKilkaRodzajow sub, t2.WPodstawKilkaRodzajow sub)
        | TKopara(t1,t2)         -> TKopara(t1.WPodstawKilkaRodzajow sub, t2.WPodstawKilkaRodzajow sub)
        | _ -> this
    /// <summary>
    /// Sprawdza, czy dany konstruktor typu zawiera wolne wystąpienie podanej zmiennej
    /// typowej kwantyfikowanej abstrakcją.
    /// </summary>
    /// <param name="x"> nazwa szukanej zmiennej wolnej </param>
    /// <returns>
    /// Funkcja zwraca wartość prawdziwą, jeśli istnieje zmienna wolna o podanej nazwie,
    /// w przeciwnym wypadku zwraca wartość fałszywą.
    /// </returns>
    member this.ZawieraZmiennaTypowa x =
        match this with
        | TWZmienna _ -> false
        | TWartosc(_, t) -> t.ZawieraZmiennaTypowa x
        | TZmienna(_,y) -> x = y
        | TFunkcja(t1,t2) -> t1.ZawieraZmiennaTypowa x || t2.ZawieraZmiennaTypowa x
        | TLambda(_,y,_,_) when x = y -> false
        | TLambda(_,_,_,t) -> t.ZawieraZmiennaTypowa x
        | TAplikacja(t1,t2) -> t1.ZawieraZmiennaTypowa x || t2.ZawieraZmiennaTypowa x
        | TUniwersalny(_,y,_,_) when x = y -> false
        | TUniwersalny(_,_,_,t) -> t.ZawieraZmiennaTypowa x
        | TAnotacja(t,_) -> t.ZawieraZmiennaTypowa x
        | TNat
        | TBool -> false
        | TPara(t1,t2) | TKopara(t1,t2) -> t1.ZawieraZmiennaTypowa x || t2.ZawieraZmiennaTypowa x
    /// <summary>
    /// Sprawdza, czy dany konstruktor typu zawiera wystąpienie podanej zmiennej 
    /// typowej kwantyfikowanej schematem.
    /// </summary>
    /// <param name="x"> nazwa szukanej zmiennej schematowej </param>
    /// <returns> 
    /// Funkcja zwraca wartość prawdziwą, jeśli istnieje zmienna schematowa o podanej nazwie,
    /// w przeciwnym wypadku zwraca wartość fałszywą.
    /// </returns>
    member this.ZawieraZmiennaTypowaW x =
        match this with
        | TWZmienna y -> x = y
        | TWartosc(_, t) -> t.ZawieraZmiennaTypowaW x
        | TZmienna _ -> false
        | TFunkcja(t1,t2) -> t1.ZawieraZmiennaTypowaW x || t2.ZawieraZmiennaTypowaW x
        | TLambda(_,_,_,t) -> t.ZawieraZmiennaTypowaW x
        | TAplikacja(t1,t2) -> t1.ZawieraZmiennaTypowaW x || t2.ZawieraZmiennaTypowaW x
        | TUniwersalny(_,_,_,t) -> t.ZawieraZmiennaTypowaW x
        | TAnotacja(t,_) -> t.ZawieraZmiennaTypowaW x
        | TNat
        | TBool -> false
        | TPara(t1,t2) | TKopara(t1,t2) -> t1.ZawieraZmiennaTypowaW x || t2.ZawieraZmiennaTypowaW x
    /// <summary>
    /// Wykonuje podstawienie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej abstrakcją
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy konstruktor typu po wykonaniu podstawienia. </returns>
    member this.Podstaw x (typ : Typ) =
        match this with
        | TWZmienna y -> TWZmienna y
        | TWartosc(y, t) -> TWartosc(y, t.Podstaw x typ)
        | TZmienna(y,y2) when x = y2 -> typ.AlfaKopia
        | TZmienna _ -> this
        | TFunkcja(t1,t2) -> TFunkcja(t1.Podstaw x typ, t2.Podstaw x typ)
        | TLambda(y,y2,k,t) ->
            // Tu nie zrobimy sobie krzywdy, bo zmienne związane są unikatowe
            TLambda(y,y2, k, t.Podstaw x typ)
        | TAplikacja(t1,t2) -> TAplikacja(t1.Podstaw x typ,t2.Podstaw x typ)
        | TUniwersalny(y,y2,k,t) ->
            TUniwersalny(y,y2, k, t.Podstaw x typ)
        | TAnotacja(t,k) -> TAnotacja(t.Podstaw x typ, k)
        | TNat -> TNat
        | TBool -> TBool
        | TPara(t1,t2) -> TPara(t1.Podstaw x typ, t2.Podstaw x typ)
        | TKopara(t1,t2) -> TKopara(t1.Podstaw x typ, t2.Podstaw x typ)

    /// <summary>
    /// Wykonuje podstawienie kopie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej abstrakcją, w której za zmienne rodzajowe w <paramref name="kv"/>
    /// podstawiono świerze zmienne
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="kv"> zmienne rodzajowe, które powinny mieć własną kopię </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy konstruktor typu po wykonaniu podstawienia. </returns>
    member this.PodstawKopie x kv (typ : Typ) =
        match this with
        | TWZmienna y -> TWZmienna y
        | TWartosc(y,t) -> TWartosc(y, t.PodstawKopie x kv typ)
        | TZmienna(y,y2) when x = y2 -> 
            let t =
                kv |>
                List.map (fun x -> (x, KWZmienna(Fresh.swierzaNazwa()))) |>
                typ.WPodstawKilkaRodzajow
            in TWartosc(y, t)
        | TZmienna _ -> this
        | TFunkcja(t1,t2) -> TFunkcja(t1.PodstawKopie x kv typ, t2.PodstawKopie x kv typ)
        | TLambda(y,y2,k,t) ->
            TLambda(y, y2, k, t.PodstawKopie x kv typ)
        | TAplikacja(t1,t2) -> TAplikacja(t1.PodstawKopie x kv typ,t2.PodstawKopie x kv typ)
        | TUniwersalny(y,y2,k,t) ->
            TUniwersalny(y,y2, k, t.PodstawKopie x kv typ)
        | TAnotacja(t,k) -> TAnotacja(t.PodstawKopie x kv typ, k)
        | TNat -> TNat
        | TBool -> TBool
        | TPara(t1,t2) -> TPara(t1.PodstawKopie x kv typ, t2.PodstawKopie x kv typ)
        | TKopara(t1,t2) -> TKopara(t1.PodstawKopie x kv typ, t2.PodstawKopie x kv typ)

    /// <summary>
    /// Wolne zmienne rodzajowe kwantyfikowane schematem
    /// </summary>
    /// <returns>
    /// Właściwość zwiera zbiór rodzajowych zmiennych wolnych kwantyfikowanych schematem.
    /// </returns>
    member this.WolneKWZmienne =
        match this with
        | TWZmienna _ -> Set.empty
        | TWartosc(_, t) -> t.WolneKWZmienne
        | TZmienna _ -> Set.empty
        | TFunkcja(a, b) -> Set.union a.WolneKWZmienne b.WolneKWZmienne
        | TLambda(_, _, k, t) -> Set.union k.WolneKWZmienne t.WolneKWZmienne
        | TAplikacja(a, b) -> Set.union a.WolneKWZmienne b.WolneKWZmienne
        | TUniwersalny(_, _, k, t) -> Set.union k.WolneKWZmienne t.WolneKWZmienne
        | TAnotacja(t, k) -> Set.union t.WolneKWZmienne k.WolneKWZmienne
        | TNat
        | TBool -> Set.empty
        | TPara(t1,t2) | TKopara(t1,t2) -> Set.union t1.WolneKWZmienne t2.WolneKWZmienne

    /// <summary>
    /// Wolne zmienne typowe kwantyfikowane schematem
    /// </summary>
    /// <returns>
    /// Właściwość zwiera zbiór typowych zmiennych wolnych kwantyfikowanych schematem.
    /// </returns>
    member this.WolneTWZmienne =
        match this with
        | TWZmienna x -> Set.singleton x
        | TWartosc(_, t) -> t.WolneTWZmienne
        | TZmienna _ -> Set.empty
        | TFunkcja(a, b) -> Set.union a.WolneTWZmienne b.WolneTWZmienne
        | TLambda(_, _, k, t) -> t.WolneTWZmienne
        | TAplikacja(a, b) -> Set.union a.WolneTWZmienne b.WolneTWZmienne
        | TUniwersalny(_, _, k, t) -> t.WolneTWZmienne
        | TAnotacja(t, k) -> t.WolneTWZmienne
        | TNat
        | TBool -> Set.empty
        | TPara(t1,t2) | TKopara(t1,t2) -> Set.union t1.WolneTWZmienne t2.WolneTWZmienne

    /// <summary>
    /// Typ po alfa-konwersji wprowadzającej świeże zmienne typowe
    /// </summary>
    member this.AlfaKopia =
        match this with
        | TWZmienna _ -> this
        | TWartosc(x, t) -> TWartosc(x, t.AlfaKopia)
        | TZmienna _ -> this
        | TFunkcja(a, b) -> TFunkcja(a.AlfaKopia, b.AlfaKopia)
        | TLambda(x, x2, k, t) ->
            let x2' = Fresh.swierzaNazwa();
            let t' = t.Podstaw x2 (TZmienna(x, x2'));
            TLambda(x, x2', k, t'.AlfaKopia)
        | TAplikacja(a, b) -> TAplikacja(a.AlfaKopia, b.AlfaKopia)
        | TUniwersalny(x, x2, k, t) ->
            let x2' = Fresh.swierzaNazwa();
            let t' = t.Podstaw x2 (TZmienna(x, x2'));
            TUniwersalny(x, x2', k, t'.AlfaKopia)
        | TAnotacja(t, k) -> TAnotacja(t.AlfaKopia, k)
        | TNat -> TNat
        | TBool -> TBool
        | TPara(t1,t2) -> TPara(t1.AlfaKopia, t2.AlfaKopia)
        | TKopara(t1,t2) -> TKopara(t1.AlfaKopia, t2.AlfaKopia)

    /// <summary>
    /// Zamienia typ na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <param name="prior"> oczekiwany priorytet wyrażenia. Zero jeśli nie chcemy całego wyrażenia w nawiasie </param>
    /// <returns> Ciąg znaków reprezentujący typ </returns>
    member this.ToString prior =
        match this with
        | TWZmienna x -> x
        | TWartosc(x, _) -> x
        | TZmienna(x, x2) -> x
        | TFunkcja(a,b) ->
            let res = a.ToString 1 + " -> " + b.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | TLambda(x,_,k,t) ->
            let res = "\\" + x + "::" + k.ToString() + "." + t.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | TAplikacja(a,b) ->
            let res = a.ToString 1 + " " + b.ToString 2
            if prior >= 2 then
                "(" + res + ")"
            else res
        | TUniwersalny(x,_,k,t) -> 
            let res = "All " + x + "::" + k.ToString() + "." + t.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | TAnotacja(t,k) ->
            let res = t.ToString 2 + " :: " + k.ToString();
            if prior >= 2 then
                "(" + res + ")"
            else res
        | TNat -> "Nat"
        | TBool -> "Bool"
        | TPara(t1,t2) -> "{" + t1.ToString() + "," + t2.ToString() + "}"
        | TKopara(t1,t2) -> "<" + t1.ToString() + "|" + t2.ToString() + ">"
    /// <summary>
    /// Zamienia rodzaj na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <returns> Ciąg znaków reprezentujący rodzaj </returns>
    override this.ToString() =
        this.ToString 0

/// <summary> 
/// Wyrażenie
/// </summary>
type Wyrazenie =
    /// <summary> zmienna </summary>
    | EZmienna     of string * Parsor.Core.IPosition
    /// <summary> abstrakcja z anotowanym argumentem </summary>
    | ELambda      of string * Typ * Wyrazenie * Parsor.Core.IPosition
    /// <summary> aplikacja </summary>
    | EAplikacja   of Wyrazenie * Wyrazenie * Parsor.Core.IPosition
    /// <summary> abstrakcja typowa z anotowanym argumentem </summary>
    | ETLambda     of string * string * Rodzaj * Wyrazenie * Parsor.Core.IPosition
    /// <summary> aplikacja typowa </summary>
    | ETAplikacja  of Wyrazenie * Typ * Parsor.Core.IPosition
    /// <summary> anotacja typowa </summary>
    | EAnotacja    of Wyrazenie * Typ * Parsor.Core.IPosition
    /// <summary> definicja lokalna </summary>
    | ELet         of string * Wyrazenie * Wyrazenie * Parsor.Core.IPosition
    /// <summary> typ lokalny </summary>
    | ETLet        of string * string * Typ * Wyrazenie * Parsor.Core.IPosition
    /// <summary> wartść calkowitoliczbowa </summary>
    | ENat of int * Parsor.Core.IPosition
    /// <summary> prawdziwa wartość logiczna </summary>
    | ETrue of Parsor.Core.IPosition
    /// <summary> fałszywa wartość logiczna </summary>
    | EFalse of Parsor.Core.IPosition
    /// <summary> operator arytmetyczny </summary>
    | EOpArytmetyczny of Wyrazenie * Wyrazenie * string * (int -> int -> int) * Parsor.Core.IPosition
    /// <summary> operator porównania </summary>
    | EOpPorownania   of Wyrazenie * Wyrazenie * string * (int -> int -> bool) * Parsor.Core.IPosition
    /// <summary> instrukcja warunkowa </summary>
    | EIf of Wyrazenie * Wyrazenie * Wyrazenie * Parsor.Core.IPosition
    /// <summary> para elementów </summary>
    | EPara of Wyrazenie * Wyrazenie * Parsor.Core.IPosition
    /// <summary> projekcja na pierwszy element pary </summary>
    | EProjLewy of Wyrazenie * Parsor.Core.IPosition
    /// <summary> projekcja na drugi element pary </summary>
    | EProjPrawy of Wyrazenie * Parsor.Core.IPosition
    /// <summary> lewy element kopary </summary>
    | ELewy of Wyrazenie * Parsor.Core.IPosition
    /// <summary> prawy element kopary </summary>
    | EPrawy of Wyrazenie * Parsor.Core.IPosition
    /// <summary> destruktor kopary (wybór wariantu) </summary>
    | ECase of Wyrazenie * Wyrazenie * Wyrazenie * Parsor.Core.IPosition

    /// <summary>
    /// Wykonuje podstawienie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej abstrakcją
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowe wyrazenie po wykonaniu podstawienia. </returns>
    member this.PodstawTyp x typ =
        match this with
        | EZmienna _ -> this
        | ELambda(y,t,e,pos) -> ELambda(y, t.Podstaw x typ, e.PodstawTyp x typ, pos)
        | EAplikacja(e1,e2,pos) -> EAplikacja(e1.PodstawTyp x typ, e2.PodstawTyp x typ, pos)
        | ETLambda(y,y2,k,e,pos) -> ETLambda(y, y2, k, e.PodstawTyp x typ, pos)
        | ETAplikacja(e,t,pos) -> ETAplikacja(e.PodstawTyp x typ, t.Podstaw x typ, pos)
        | EAnotacja(e,t,pos) -> EAnotacja(e.PodstawTyp x typ, t.Podstaw x typ, pos)
        | ELet(y,e1,e2,pos) -> ELet(y, e1.PodstawTyp x typ, e2.PodstawTyp x typ, pos)
        | ETLet(y,y2,t,e,pos) -> ETLet(y, y2, t.Podstaw x typ, e.PodstawTyp x typ,pos)
        | ENat _ | ETrue _ | EFalse _ -> this
        | EOpArytmetyczny(e1, e2, s, f, pos) -> 
            EOpArytmetyczny(e1.PodstawTyp x typ, e2.PodstawTyp x typ, s, f, pos)
        | EOpPorownania(e1, e2, s, f, pos) ->
            EOpPorownania(e1.PodstawTyp x typ, e2.PodstawTyp x typ, s, f, pos)
        | EIf(e1,e2,e3, pos) -> EIf(e1.PodstawTyp x typ, e2.PodstawTyp x typ, e3.PodstawTyp x typ, pos)
        | EPara(e1,e2,pos) -> EPara(e1.PodstawTyp x typ, e2.PodstawTyp x typ, pos)
        | EProjLewy(e,pos) -> EProjLewy(e.PodstawTyp x typ, pos)
        | EProjPrawy(e,pos) -> EProjPrawy(e.PodstawTyp x typ, pos)
        | ELewy(e,pos) -> ELewy(e.PodstawTyp x typ, pos)
        | EPrawy(e,pos) -> EPrawy(e.PodstawTyp x typ, pos)
        | ECase(e1, e2, e3, pos) -> ECase(e1.PodstawTyp x typ, e2.PodstawTyp x typ, e3.PodstawTyp x typ, pos)

    /// <summary>
    /// Wykonuje podstawienie kopie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej abstrakcją, w której za zmienne rodzajowe w <paramref name="kv"/>
    /// podstawiono świerze zmienne
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="kv"> zmienne rodzajowe, które powinny mieć własną kopię </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowe wyrazenie po wykonaniu podstawienia. </returns>
    member this.PodstawKopieTypu x kv typ =
        match this with
        | EZmienna _ -> this
        | ELambda(y,t,e,pos) -> ELambda(y, t.PodstawKopie x kv typ, e.PodstawKopieTypu x kv typ, pos)
        | EAplikacja(e1,e2,pos) -> EAplikacja(e1.PodstawKopieTypu x kv typ, e2.PodstawKopieTypu x kv typ, pos)
        | ETLambda(y,y2,k,e,pos) -> ETLambda(y, y2, k, e.PodstawKopieTypu x kv typ, pos)
        | ETAplikacja(e,t,pos) -> ETAplikacja(e.PodstawKopieTypu x kv typ, t.PodstawKopie x kv typ, pos)
        | EAnotacja(e,t,pos) -> EAnotacja(e.PodstawKopieTypu x kv typ, t.PodstawKopie x kv typ, pos)
        | ELet(y,e1,e2,pos) -> ELet(y, e1.PodstawKopieTypu x kv typ, e2.PodstawKopieTypu x kv typ, pos)
        | ETLet(y,y2,t,e,pos) -> ETLet(y, y2, t.PodstawKopie x kv typ, e.PodstawKopieTypu x kv typ, pos)
        | ENat _ | ETrue _ | EFalse _ -> this
        | EOpArytmetyczny(e1, e2, s, f, pos) -> 
            EOpArytmetyczny(e1.PodstawKopieTypu x kv typ, e2.PodstawKopieTypu x kv typ, s, f, pos)
        | EOpPorownania(e1, e2, s, f, pos) ->
            EOpPorownania(e1.PodstawKopieTypu x kv typ, e2.PodstawKopieTypu x kv typ, s, f, pos)
        | EIf(e1,e2,e3,pos) -> 
            EIf(e1.PodstawKopieTypu x kv typ, e2.PodstawKopieTypu x kv typ, e3.PodstawKopieTypu x kv typ, pos)
        | EPara(e1,e2,pos) ->
            EPara(e1.PodstawKopieTypu x kv typ, e2.PodstawKopieTypu x kv typ, pos)
        | EProjLewy(e,pos) -> EProjLewy(e.PodstawKopieTypu x kv typ, pos)
        | EProjPrawy(e,pos) -> EProjPrawy(e.PodstawKopieTypu x kv typ, pos)
        | ELewy(e,pos) -> ELewy(e.PodstawKopieTypu x kv typ, pos)
        | EPrawy(e,pos) -> EPrawy(e.PodstawKopieTypu x kv typ, pos)
        | ECase(e1,e2,e3,pos) -> 
            ECase(e1.PodstawKopieTypu x kv typ, e2.PodstawKopieTypu x kv typ, e3.PodstawKopieTypu x kv typ, pos)

    /// <summary>
    /// Sprawdza, czy dane wyrażenie zawiera wolne wystąpienie podanej zmiennej.
    /// </summary>
    /// <param name="x"> nazwa szukanej zmiennej wolnej </param>
    /// <returns> 
    /// Funkcja zwraca wartość prawdziwą, jeśli istnieje zmienna wolna o podanej nazwie,
    /// w przeciwnym wypadku zwraca wartość fałszywą.
    /// </returns>
    member this.ZawieraZmienna x =
        match this with
        | EZmienna(y, _) -> x = y
        | ELambda(y,_,_,_) when x = y -> false
        | ELambda(_,_,e,_) -> e.ZawieraZmienna x
        | EAplikacja(e1,e2,_) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | ETLambda(_,_,_,e,_) -> e.ZawieraZmienna x
        | ETAplikacja(e,_,_) -> e.ZawieraZmienna x
        | EAnotacja(e,_,_) -> e.ZawieraZmienna x
        | ELet(y,e1,e2,_) when x = y -> e1.ZawieraZmienna x
        | ELet(y,e1,e2,_) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | ETLet(_,_,_,e,_) -> e.ZawieraZmienna x
        | ENat _ | ETrue _ | EFalse _ -> false
        | EOpArytmetyczny(e1, e2, _, _, _) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | EOpPorownania(e1, e2, _, _, _) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | EIf(e1,e2,e3, _) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x || e3.ZawieraZmienna x
        | EPara(e1,e2,_) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | EProjLewy(e,_) -> e.ZawieraZmienna x
        | EProjPrawy(e,_) -> e.ZawieraZmienna x
        | ELewy(e,_) -> e.ZawieraZmienna x
        | EPrawy(e,_) -> e.ZawieraZmienna x
        | ECase(e1,e2,e3, _) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x || e3.ZawieraZmienna x
    /// <summary>
    /// Wykonuje podstawienie wyrażenie <paramref name="expr"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/>.
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="expr"> wyrażenie który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowe wyrażenie po wykonaniu podstawienia. </returns>
    member this.Podstaw x expr =
        match this with
        | EZmienna(y, _) when x = y -> expr
        | EZmienna _ -> this
        | ELambda(y,t,e,pos) ->
            if x = y then ELambda(y,t,e,pos)
            elif expr.ZawieraZmienna y then
                let z = Fresh.swierzaNazwa();
                ELambda(z, t, (e.Podstaw y (EZmienna(z, pos))).Podstaw x expr, pos)
            else ELambda(y, t, e.Podstaw x expr, pos)
        | EAplikacja(e1,e2,pos) -> EAplikacja(e1.Podstaw x expr, e2.Podstaw x expr, pos)
        | ETLambda(y,y2,k,e,pos) -> ETLambda(y,y2,k,e.Podstaw x expr, pos)
        | ETAplikacja(e,t,pos) -> ETAplikacja(e.Podstaw x expr, t, pos)
        | EAnotacja(e,t, pos) -> EAnotacja(e.Podstaw x expr, t, pos)
        | ELet(y,e1,e2,pos) ->
            if x = y then ELet(y,e1,e2,pos)
            elif expr.ZawieraZmienna y then
                let z = Fresh.swierzaNazwa();
                ELet(z, e1.Podstaw x expr, (e2.Podstaw y (EZmienna(z,pos))).Podstaw x expr, pos)
            else ELet(y, e1.Podstaw x expr, e2.Podstaw x expr, pos)
        | ETLet(y,y2,t,e,pos) ->
            ETLet(y,y2,t,e.Podstaw x expr,pos)
        | ENat _ | ETrue _ | EFalse _ -> this
        | EOpArytmetyczny(e1, e2, s, f, pos) -> EOpArytmetyczny(e1.Podstaw x expr, e2.Podstaw x expr, s, f, pos)
        | EOpPorownania(e1, e2, s, f, pos) -> EOpPorownania(e1.Podstaw x expr, e2.Podstaw x expr, s, f, pos)
        | EIf(e1, e2, e3, pos) -> EIf(e1.Podstaw x expr, e2.Podstaw x expr, e3.Podstaw x expr, pos)
        | EPara(e1, e2, pos) -> EPara(e1.Podstaw x expr, e2.Podstaw x expr, pos)
        | EProjLewy(e, pos) -> EProjLewy(e.Podstaw x expr, pos)
        | EProjPrawy(e, pos) -> EProjPrawy(e.Podstaw x expr, pos)
        | ELewy(e, pos) -> ELewy(e.Podstaw x expr, pos)
        | EPrawy(e, pos) -> EPrawy(e.Podstaw x expr, pos)
        | ECase(e1, e2, e3, pos) -> ECase(e1.Podstaw x expr, e2.Podstaw x expr, e3.Podstaw x expr, pos)

    /// <summary>
    /// Polożenie danego wyrażenia w kodzie źródłowym.
    /// </summary>
    member this.Polozenie =
        match this with
        | EZmienna(_, pos) | ELambda(_,_,_,pos) | EAplikacja(_,_,pos) | ETLambda(_,_,_,_,pos)
        | ETAplikacja(_,_,pos) | EAnotacja(_,_,pos) | ELet(_,_,_,pos) | ETLet(_,_,_,_,pos)
        | ENat(_,pos) | ETrue pos | EFalse pos | EOpArytmetyczny(_,_,_,_,pos) | EOpPorownania(_,_,_,_,pos) 
        | EIf(_,_,_,pos) | EPara(_,_,pos) | EProjLewy(_, pos) | EProjPrawy(_, pos) | ELewy(_, pos) | EPrawy(_, pos)
        | ECase(_,_,_,pos) ->  pos

    /// <summary>
    /// Zamienia term na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <param name="prior"> oczekiwany priorytet wyrażenia. Zero jeśli nie chcemy całego wyrażenia w nawiasie </param>
    /// <returns> Ciąg znaków reprezentujący term </returns>
    member this.ToString prior =
        match this with
        | EZmienna(x, _) -> x
        | ELambda(x,t,e,_) ->
            let res = "\\" + x + ":" + t.ToString() + "." + e.ToString 0;
            if prior > 8 then
                "(" + res + ")"
            else res
        | EAplikacja(a,b,_) ->
            let res = a.ToString 8 + " " + b.ToString 9
            if prior > 8 then
                "(" + res + ")"
            else res
        | ETLambda(x,_,k,e,_) ->
            let res = "\\\\" + x + "::" + k.ToString() + "." + e.ToString 0;
            if prior > 0 then
                "(" + res + ")"
            else res
        | ETAplikacja(a,b,_) ->
            let res = a.ToString 8 + "[" + b.ToString() + "]"
            if prior > 8 then
                "(" + res + ")"
            else res
        | EAnotacja(e,t,_) ->
            let res = e.ToString 9 + " :: " + t.ToString();
            if prior > 8 then
                "(" + res + ")"
            else res
        | ELet(x,e1,e2,_) ->
            let res = "let " + x + " = " + e1.ToString() + " in " + e2.ToString()
            if prior > 0 then
                "(" + res + ")"
            else res
        | ETLet(x,_,t,e,_) ->
            let res = "tlet " + x + " = " + t.ToString() + " in " + e.ToString()
            if prior > 0 then
                "(" + res + ")"
            else res
        | ENat(n,_) -> n.ToString()
        | ETrue _ -> "true"
        | EFalse _ -> "false"
        | EOpArytmetyczny(e1, e2, s, _, _) ->
            let p = if s = "+" || s = "-" then 4 else 5;
            let res = e1.ToString p + s + e2.ToString(p+1);
            if prior > p then
                "(" + res + ")"
            else res
        | EOpPorownania(e1, e2, s, _, _) ->
            let res = e1.ToString 3 + s + e2.ToString 3;
            if prior > 2 then
                "(" + res + ")"
            else res
        | EIf(e1,e2,e3,_) ->
            let res = "if " + e1.ToString() + " then " + e2.ToString() + " else " + e3.ToString();
            if prior > 0 then
                "(" + res + ")"
            else res
        | EPara(e1,e2,_) -> "{" + e1.ToString() + "," + e2.ToString() + "}"
        | EProjLewy(e,_) -> e.ToString 12 + ".left"
        | EProjPrawy(e,_) -> e.ToString 12 + ".right"
        | ELewy(e,_) ->
            let res = "left " + e.ToString();
            if prior > 0 then
                "(" + res + ")"
            else res
        | EPrawy(e,_) ->
            let res = "right " + e.ToString();
            if prior > 0 then
                "(" + res + ")"
            else res
        | ECase(e1,e2,e3,_) ->
            let res = "case " + e1.ToString() + " of left=> " + e2.ToString() + " |right=> " + e3.ToString();
            if prior > 0 then
                "(" + res + ")"
            else res
    /// <summary>
    /// Zamienia rodzaj na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <returns> Ciąg znaków reprezentujący rodzaj </returns>
    override this.ToString() =
        this.ToString 0

/// <summary>
/// Monada opt.
/// </summary>
type OptionBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some v -> f v
    member this.Return x = Some x
    member this.ReturnFrom x = x
    member this.Zero() = None

/// <summary>
/// Monada opt.
/// </summary>
let opt = OptionBuilder()

/// <summary>
/// Schemat typu lub rodzaju.
/// </summary>
type Schemat =
    /// <summary>
    /// Przesłanka o schemacie typu zmiennej. Zawiera informacje kolejno o nazwie zmiennej,
    /// wykorzystywanych typowych zmiennych schematowych, wykorzystywanych rodzajowych zmiennych schematowych oraz
    /// o typie występującym w schemacie.
    /// </summary>
    | SchematTypu of string * string list * string list * Typ
    /// <summary>
    /// Przesłanka o schemacie rodzaju zmiennej. Zawiero informacje kolejno o nazwie zmiennej,
    /// wykorzystywanych zmiennych rodzajowych i o rodzjau występującym w schemacie.
    /// </summary>
    | SchematRodzaju of string * string list * Rodzaj

/// <summary>
/// Kontekst typowania.
/// </summary>
type KontekstTypowania(kontekst : Schemat list) =
    /// <summary>
    /// Schematy występujące w kontekście.
    /// </summary>
    member this.Kontekst = kontekst
    /// <summary>
    /// Znajduje schemat typu dla podanej zmiennej typowej schematowej
    /// </summary>
    member this.SchematTypu x =
        let ok s =
            match s with
            | SchematTypu(y, _, _, _) -> x = y
            | _ -> false
        match List.tryFind ok kontekst with
        | Some(SchematTypu(_, tv, kv, t)) -> Some (tv, kv, t)
        | _ -> None
    /// <summary>
    /// Znajduje schemat rodzaju dla podanej zmiennej rodzajowej schematowej
    /// </summary>
    member this.SchematRodzaju x =
        let ok s =
            match s with
            | SchematRodzaju(y, _, _) -> x = y
            | _ -> false
        match List.tryFind ok kontekst with
        | Some(SchematRodzaju(_, kv, t)) -> Some (kv, t)
        | _ -> None
    /// <summary>
    /// Rozszerza kontekst typowania o podaną przesłankę.
    /// </summary>
    member this.Rozszerz schemat =
        KontekstTypowania(schemat::kontekst)

    /// <summary>
    /// Wolne zmienne rodzajowe kwantyfikowane schematem
    /// </summary>
    /// <returns>
    /// Właściwość zwiera zbiór rodzajowych zmiennych wolnych kwantyfikowanych schematem.
    /// </returns>
    member this.WolneKWZmienne =
        let fkv s =
            match s with
            | SchematTypu(x, tv, kv, t) ->
                t.WolneKWZmienne - Set.ofList kv
            | SchematRodzaju(x, kv, k) ->
                k.WolneKWZmienne - Set.ofList kv
        in
            kontekst |>
            List.fold (fun stat schem -> Set.union stat (fkv schem)) Set.empty

    /// <summary>
    /// Wolne zmienne typowe kwantyfikowane schematem
    /// </summary>
    /// <returns>
    /// Właściwość zwiera zbiór typowych zmiennych wolnych kwantyfikowanych schematem.
    /// </returns>
    member this.WolneTWZmienne =
        let fkv s =
            match s with
            | SchematTypu(x, tv, kv, t) ->
                t.WolneTWZmienne - Set.ofList tv
            | SchematRodzaju(x, kv, k) ->
                Set.empty
        in
            kontekst |>
            List.fold (fun stat schem -> Set.union stat (fkv schem)) Set.empty
