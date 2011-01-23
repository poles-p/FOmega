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
    /// <summary> rodzaj wszystkich typow (*) </summary>
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
/// Konstruktor typu
/// </summary>
type Typ =
    /// <summary> zmienna typowa kwantyfikowana schematem typowy </summary>
    | TWZmienna        of string
    /// <summary> zmienna typowa kwantyfikowana abstrakcją </summary>
    | TZmienna         of string
    /// <summary> typ funkcji (T -> T) </summary>
    | TFunkcja         of Typ * Typ
    /// <summary> konstruktor abstrakcji typowej </summary>
    | TLambda          of string * Typ
    /// <summary> konstruktor abstrakcji z anotowanym argumentem </summary>
    | TLambdaAnot      of string * Rodzaj * Typ
    /// <summary> konstruktor aplikacji typowe  </summary>
    | TAplikacja       of Typ * Typ
    /// <summary> typ uniwersalny </summary>
    | TUniwersalny     of string * Typ
    /// <summary> typ uniwersalny anotowany </summary>
    | TUniwersalnyAnot of string * Rodzaj * Typ
    /// <summary> anotacja rodzajowa (T :: K) </summary>
    | TAnotacja        of Typ * Rodzaj
    /// <summary>
    /// Wykonuje podstawienie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej schematem
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy konstruktor typu po wykonaniu podstawienia. </returns>
    member this.WPodstaw x typ =
        // Tu nie zrobimy sobie krzywdy, przy lambdzie bo rozróżniamy dwie klasy zmiennych typowych:
        // tych kwantyfikowanych schematem, i tych kwantyfikowanych abstrakcją
        match this with
        | TWZmienna y when x = y  -> typ
        | TFunkcja(t1,t2)         -> TFunkcja(t1.WPodstaw x typ, t2.WPodstaw x typ)
        | TLambda(y,t)            -> TLambda(y, t.WPodstaw x typ) 
        | TLambdaAnot(y,k,t)      -> TLambdaAnot(y, k, t.WPodstaw x typ)
        | TAplikacja(t1,t2)       -> TAplikacja(t1.WPodstaw x typ, t2.WPodstaw x typ)
        | TUniwersalny(y,t)       -> TUniwersalny(y, t.WPodstaw x typ)
        | TUniwersalnyAnot(y,k,t) -> TUniwersalnyAnot(y, k, t.WPodstaw x typ)
        | TAnotacja(t,k)          -> TAnotacja(t.WPodstaw x typ, k)
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
        | TFunkcja(t1,t2)         -> TFunkcja(t1.WPodstawRodzaj x kind, t2.WPodstawRodzaj x kind)
        | TLambda(y,t)            -> TLambda(y, t.WPodstawRodzaj x kind)
        | TLambdaAnot(y,k,t)      -> TLambdaAnot(y, k.WPodstaw x kind, t.WPodstawRodzaj x kind)
        | TAplikacja(t1,t2)       -> TAplikacja(t1.WPodstawRodzaj x kind, t2.WPodstawRodzaj x kind)
        | TUniwersalny(y,t)       -> TUniwersalny(y,t.WPodstawRodzaj x kind)
        | TUniwersalnyAnot(y,k,t) -> TUniwersalnyAnot(y,k.WPodstaw x kind, t.WPodstawRodzaj x kind)
        | TAnotacja(t,k)          -> TAnotacja(t.WPodstawRodzaj x kind, k.WPodstaw x kind)
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
        | TZmienna y -> x = y
        | TFunkcja(t1,t2) -> t1.ZawieraZmiennaTypowa x || t2.ZawieraZmiennaTypowa x
        | TLambda(y,_) when x = y -> false
        | TLambda(_,t) -> t.ZawieraZmiennaTypowa x
        | TLambdaAnot(y,_,_) when x = y -> false
        | TLambdaAnot(_,_,t) -> t.ZawieraZmiennaTypowa x
        | TAplikacja(t1,t2) -> t1.ZawieraZmiennaTypowa x || t2.ZawieraZmiennaTypowa x
        | TUniwersalny(y,_) when x = y -> false
        | TUniwersalny(_,t) -> t.ZawieraZmiennaTypowa x
        | TUniwersalnyAnot(y,_,_) when x = y -> false
        | TUniwersalnyAnot(_,_,t) -> t.ZawieraZmiennaTypowa x
        | TAnotacja(t,_) -> t.ZawieraZmiennaTypowa x
    /// <summary>
    /// Wykonuje podstawienie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej abstrakcją
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy konstruktor typu po wykonaniu podstawienia. </returns>
    member this.Podstaw x typ =
        match this with
        | TWZmienna y -> TWZmienna y
        | TZmienna y when x = y -> typ
        | TZmienna y -> TZmienna y
        | TFunkcja(t1,t2) -> TFunkcja(t1.Podstaw x typ, t2.Podstaw x typ)
        | TLambda(y,t) ->
            if x = y then TLambda(y, t)
            elif typ.ZawieraZmiennaTypowa y then
                let z = Fresh.swierzaNazwa();
                TLambda(z, (t.Podstaw y (TZmienna z)).Podstaw x typ)
            else TLambda(y, t.Podstaw x typ)
        | TLambdaAnot(y,k,t) ->
            if x = y then TLambdaAnot(y,k,t)
            elif typ.ZawieraZmiennaTypowa y then
                let z = Fresh.swierzaNazwa();
                TLambdaAnot(z, k, (t.Podstaw y (TZmienna z)).Podstaw x typ)
            else TLambdaAnot(y, k, t.Podstaw x typ)
        | TAplikacja(t1,t2) -> TAplikacja(t1.Podstaw x typ,t2.Podstaw x typ)
        | TUniwersalny(y,t) ->
            if x = y then TUniwersalny(y, t)
            elif typ.ZawieraZmiennaTypowa y then
                let z = Fresh.swierzaNazwa();
                TUniwersalny(z, (t.Podstaw y (TZmienna z)).Podstaw x typ)
            else TUniwersalny(y, t.Podstaw x typ)
        | TUniwersalnyAnot(y,k,t) ->
            if x = y then TUniwersalnyAnot(y,k,t)
            elif typ.ZawieraZmiennaTypowa y then
                let z = Fresh.swierzaNazwa();
                TUniwersalnyAnot(z, k, (t.Podstaw y (TZmienna z)).Podstaw x typ)
            else TUniwersalnyAnot(y, k, t.Podstaw x typ)
        | TAnotacja(t,k) -> TAnotacja(t.Podstaw x typ, k)

/// <summary> 
/// Wyrażenie
/// </summary>
type Wyrazenie =
    /// <summary> zmienna </summary>
    | EZmienna     of string
    /// <summary> abstrakcja </summary>
    | ELambda      of string * Wyrazenie
    /// <summary> abstrakcja z anotowanym argumentem </summary>
    | ELambdaAnot  of string * Typ * Wyrazenie
    /// <summary> aplikacja </summary>
    | EAplikacja   of Wyrazenie * Wyrazenie
    /// <summary> abstrakcja typowa </summary>
    | ETLambda     of string * Wyrazenie
    /// <summary> abstrakcja typowa z anotowanym argumentem </summary>
    | ETLambdaAnot of string * Rodzaj * Wyrazenie
    /// <summary> aplikacja typowa </summary>
    | ETAplikacja  of Wyrazenie * Typ
    /// <summary> anotacja typowa </summary>
    | EAnotacja    of Wyrazenie * Typ
    /// <summary>
    /// Wykonuje podstawienie konstruktora typu <paramref name="typ"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/> kwantyfikowanej abstrakcją
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="typ"> konstruktor typu który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowe wyrazenie po wykonaniu podstawienia. </returns>
    member this.PodstawTyp x typ =
        match this with
        | EZmienna y -> EZmienna y
        | ELambda(y,e) -> ELambda(y, e.PodstawTyp x typ)
        | ELambdaAnot(y,t,e) -> ELambdaAnot(y, t.Podstaw x typ, e.PodstawTyp x typ)
        | EAplikacja(e1,e2) -> EAplikacja(e1.PodstawTyp x typ, e2.PodstawTyp x typ)
        | ETLambda(y,e) -> ETLambda(y, e.PodstawTyp x typ)
        | ETLambdaAnot(y,k,e) -> ETLambdaAnot(y, k, e.PodstawTyp x typ)
        | ETAplikacja(e,t) -> ETAplikacja(e.PodstawTyp x typ, t.Podstaw x typ)
        | EAnotacja(e,t) -> EAnotacja(e.PodstawTyp x typ, t.Podstaw x typ)
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
        | EZmienna y -> x = y
        | ELambda(y,_) when x = y -> false
        | ELambda(_,e) -> e.ZawieraZmienna x
        | ELambdaAnot(y,_,_) when x = y -> false
        | ELambdaAnot(_,_,e) -> e.ZawieraZmienna x
        | EAplikacja(e1,e2) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | ETLambda(_,e) -> e.ZawieraZmienna x
        | ETLambdaAnot(_,_,e) -> e.ZawieraZmienna x
        | ETAplikacja(e,_) -> e.ZawieraZmienna x
        | EAnotacja(e,_) -> e.ZawieraZmienna x
    /// <summary>
    /// Wykonuje podstawienie wyrażenie <paramref name="expr"/> za wszystkie wystąpienia 
    /// zmiennej <paramref name="x"/>.
    /// </summary>
    /// <param name="x"> nazwa zmiennej za którą należy wykonać podstawienie </param>
    /// <param name="expr"> wyrażenie który należy podstawić za zmienną <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowe wyrażenie po wykonaniu podstawienia. </returns>
    member this.Podstaw x expr =
        match this with
        | EZmienna y when x = y -> expr
        | EZmienna y -> EZmienna y
        | ELambda(y,e) ->
            if x = y then ELambda(y,e)
            elif expr.ZawieraZmienna y then
                let z = Fresh.swierzaNazwa();
                ELambda(z, (e.Podstaw y (EZmienna z)).Podstaw x expr)
            else ELambda(y, e.Podstaw x expr)
        | ELambdaAnot(y,t,e) ->
            if x = y then ELambdaAnot(y,t,e)
            elif expr.ZawieraZmienna y then
                let z = Fresh.swierzaNazwa();
                ELambdaAnot(z, t, (e.Podstaw y (EZmienna z)).Podstaw x expr)
            else ELambdaAnot(y, t, e.Podstaw x expr)
        | EAplikacja(e1,e2) -> EAplikacja(e1.Podstaw x expr, e2.Podstaw x expr)
        | ETLambda(y,e) -> ETLambda(y,e.Podstaw x expr)
        | ETLambdaAnot(y,k,e) -> ETLambdaAnot(y,k,e.Podstaw x expr)
        | ETAplikacja(e,t) -> ETAplikacja(e.Podstaw x expr, t)
        | EAnotacja(e,t) -> EAnotacja(e.Podstaw x expr, t)
