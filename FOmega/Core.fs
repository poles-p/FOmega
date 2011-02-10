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
    /// <summary> zmienna typowa kwantyfikowana abstrakcją </summary>
    | TZmienna         of string
    /// <summary> typ funkcji (T -> T) </summary>
    | TFunkcja         of Typ * Typ
    /// <summary> konstruktor abstrakcji z anotowanym argumentem </summary>
    | TLambda          of string * Rodzaj * Typ
    /// <summary> konstruktor aplikacji typowe  </summary>
    | TAplikacja       of Typ * Typ
    /// <summary> typ uniwersalny anotowany </summary>
    | TUniwersalny     of string * Rodzaj * Typ
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
        | TLambda(y,k,t)          -> TLambda(y, k, t.WPodstaw x typ) 
        | TAplikacja(t1,t2)       -> TAplikacja(t1.WPodstaw x typ, t2.WPodstaw x typ)
        | TUniwersalny(y,k, t)    -> TUniwersalny(y, k, t.WPodstaw x typ)
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
        | TLambda(y,k,t)          -> TLambda(y, k.WPodstaw x kind, t.WPodstawRodzaj x kind)
        | TAplikacja(t1,t2)       -> TAplikacja(t1.WPodstawRodzaj x kind, t2.WPodstawRodzaj x kind)
        | TUniwersalny(y,k,t)     -> TUniwersalny(y,k.WPodstaw x kind, t.WPodstawRodzaj x kind)
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
        | TLambda(y,_,_) when x = y -> false
        | TLambda(_,_,t) -> t.ZawieraZmiennaTypowa x
        | TAplikacja(t1,t2) -> t1.ZawieraZmiennaTypowa x || t2.ZawieraZmiennaTypowa x
        | TUniwersalny(y,_,_) when x = y -> false
        | TUniwersalny(_,_,t) -> t.ZawieraZmiennaTypowa x
        | TAnotacja(t,_) -> t.ZawieraZmiennaTypowa x
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
        | TZmienna y -> false
        | TFunkcja(t1,t2) -> t1.ZawieraZmiennaTypowaW x || t2.ZawieraZmiennaTypowaW x
        | TLambda(_,_,t) -> t.ZawieraZmiennaTypowaW x
        | TAplikacja(t1,t2) -> t1.ZawieraZmiennaTypowaW x || t2.ZawieraZmiennaTypowaW x
        | TUniwersalny(_,_,t) -> t.ZawieraZmiennaTypowaW x
        | TAnotacja(t,_) -> t.ZawieraZmiennaTypowaW x
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
        | TLambda(y,k,t) ->
            if x = y then TLambda(y,k,t)
            elif typ.ZawieraZmiennaTypowa y then
                let z = Fresh.swierzaNazwa();
                TLambda(z, k, (t.Podstaw y (TZmienna z)).Podstaw x typ)
            else TLambda(y, k, t.Podstaw x typ)
        | TAplikacja(t1,t2) -> TAplikacja(t1.Podstaw x typ,t2.Podstaw x typ)
        | TUniwersalny(y,k,t) ->
            if x = y then TUniwersalny(y,k,t)
            elif typ.ZawieraZmiennaTypowa y then
                let z = Fresh.swierzaNazwa();
                TUniwersalny(z, k, (t.Podstaw y (TZmienna z)).Podstaw x typ)
            else TUniwersalny(y, k, t.Podstaw x typ)
        | TAnotacja(t,k) -> TAnotacja(t.Podstaw x typ, k)

    /// <summary>
    /// Zamienia typ na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <param name="prior"> oczekiwany priorytet wyrażenia. Zero jeśli nie chcemy całego wyrażenia w nawiasie </param>
    /// <returns> Ciąg znaków reprezentujący typ </returns>
    member this.ToString prior =
        match this with
        | TWZmienna x -> x
        | TZmienna x -> x
        | TFunkcja(a,b) ->
            let res = a.ToString 1 + " -> " + b.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | TLambda(x,k,t) ->
            let res = "\\" + x + "::" + k.ToString() + "." + t.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | TAplikacja(a,b) ->
            let res = a.ToString 1 + " " + b.ToString 2
            if prior >= 2 then
                "(" + res + ")"
            else res
        | TUniwersalny(x,k,t) -> 
            let res = "All " + x + "::" + k.ToString() + "." + t.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | TAnotacja(t,k) ->
            let res = t.ToString 2 + " :: " + k.ToString();
            if prior >= 2 then
                "(" + res + ")"
            else res
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
    | EZmienna     of string
    /// <summary> abstrakcja z anotowanym argumentem </summary>
    | ELambda      of string * Typ * Wyrazenie
    /// <summary> aplikacja </summary>
    | EAplikacja   of Wyrazenie * Wyrazenie
    /// <summary> abstrakcja typowa z anotowanym argumentem </summary>
    | ETLambda     of string * Rodzaj * Wyrazenie
    /// <summary> aplikacja typowa </summary>
    | ETAplikacja  of Wyrazenie * Typ
    /// <summary> anotacja typowa </summary>
    | EAnotacja    of Wyrazenie * Typ
    /// <summary> definicja lokalna </summary>
    | ELet         of string * Wyrazenie * Wyrazenie
    /// <summary> typ lokalny </summary>
    | ETLet        of string * Typ * Wyrazenie
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
        | ELambda(y,t,e) -> ELambda(y, t.Podstaw x typ, e.PodstawTyp x typ)
        | EAplikacja(e1,e2) -> EAplikacja(e1.PodstawTyp x typ, e2.PodstawTyp x typ)
        | ETLambda(y,k,e) -> ETLambda(y, k, e.PodstawTyp x typ)
        | ETAplikacja(e,t) -> ETAplikacja(e.PodstawTyp x typ, t.Podstaw x typ)
        | EAnotacja(e,t) -> EAnotacja(e.PodstawTyp x typ, t.Podstaw x typ)
        | ELet(x,e1,e2) -> ELet(x, e1.PodstawTyp x typ, e2.PodstawTyp x typ)
        | ETLet(x,t,e) -> ETLet(x, t.Podstaw x typ, e.PodstawTyp x typ)
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
        | ELambda(y,_,_) when x = y -> false
        | ELambda(_,_,e) -> e.ZawieraZmienna x
        | EAplikacja(e1,e2) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | ETLambda(_,_,e) -> e.ZawieraZmienna x
        | ETAplikacja(e,_) -> e.ZawieraZmienna x
        | EAnotacja(e,_) -> e.ZawieraZmienna x
        | ELet(y,e1,e2) when x = y -> e1.ZawieraZmienna x
        | ELet(y,e1,e2) -> e1.ZawieraZmienna x || e2.ZawieraZmienna x
        | ETLet(_,_,e) -> e.ZawieraZmienna x
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
        | ELambda(y,t,e) ->
            if x = y then ELambda(y,t,e)
            elif expr.ZawieraZmienna y then
                let z = Fresh.swierzaNazwa();
                ELambda(z, t, (e.Podstaw y (EZmienna z)).Podstaw x expr)
            else ELambda(y, t, e.Podstaw x expr)
        | EAplikacja(e1,e2) -> EAplikacja(e1.Podstaw x expr, e2.Podstaw x expr)
        | ETLambda(y,k,e) -> ETLambda(y,k,e.Podstaw x expr)
        | ETAplikacja(e,t) -> ETAplikacja(e.Podstaw x expr, t)
        | EAnotacja(e,t) -> EAnotacja(e.Podstaw x expr, t)
        | ELet(y,e1,e2) ->
            if x = y then ELet(y,e1,e2)
            elif expr.ZawieraZmienna y then
                let z = Fresh.swierzaNazwa();
                ELet(z, e1.Podstaw x expr, (e2.Podstaw y (EZmienna z)).Podstaw x expr)
            else ELet(y, e1.Podstaw x expr, e2.Podstaw x expr)
        | ETLet(y,t,e) ->
            ETLet(y,t,e.Podstaw x expr)

    /// <summary>
    /// Zamienia term na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <param name="prior"> oczekiwany priorytet wyrażenia. Zero jeśli nie chcemy całego wyrażenia w nawiasie </param>
    /// <returns> Ciąg znaków reprezentujący term </returns>
    member this.ToString prior =
        match this with
        | EZmienna x -> x
        | ELambda(x,t,e) ->
            let res = "\\" + x + ":" + t.ToString() + "." + e.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | EAplikacja(a,b) ->
            let res = a.ToString 1 + " " + b.ToString 2
            if prior >= 2 then
                "(" + res + ")"
            else res
        | ETLambda(x,k,e) ->
            let res = "\\\\" + x + "::" + k.ToString() + "." + e.ToString 0;
            if prior >= 1 then
                "(" + res + ")"
            else res
        | ETAplikacja(a,b) ->
            let res = a.ToString 1 + "[" + b.ToString() + "]"
            if prior >= 2 then
                "(" + res + ")"
            else res
        | EAnotacja(e,t) ->
            let res = e.ToString 2 + " :: " + t.ToString();
            if prior >= 2 then
                "(" + res + ")"
            else res
        | ELet(x,e1,e2) ->
            let res = "let " + x + " = " + e1.ToString() + " in " + e2.ToString()
            if prior >= 1 then
                "(" + res + ")"
            else res
        | ETLet(x,t,e) ->
            let res = "tlet " + x + " = " + t.ToString() + " in " + e.ToString()
            if prior >= 1 then
                "(" + res + ")"
            else res
    /// <summary>
    /// Zamienia rodzaj na ciąg znaków z możliwie najoszczędniejszym nawiasowaniem
    /// </summary>
    /// <returns> Ciąg znaków reprezentujący rodzaj </returns>
    override this.ToString() =
        this.ToString 0
