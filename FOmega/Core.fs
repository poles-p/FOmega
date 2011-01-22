/// <summary>
/// W tym module znajdują się definicje typów reprezentujących 
/// wyrażenia, typy i rodzaje
/// </summary>
module Core

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
    /// <param name="kind"> rodzaj który należy podstawić za <paramref name="x"/>. </param>
    /// <returns> Funkcja zwraca nowy rodzaj po wykonaniu podstawienia. </returns>
    member this.Podstaw x kind =
        match this with
        | KWZmienna y when x = y -> kind
        | KFunkcja(k1,k2) -> KFunkcja(k1.Podstaw x kind, k2.Podstaw x kind)
        | _ -> this

/// <summary>
/// Konstruktor typu
/// </summary>
type Typ =
    /// <summary> zmienna typowa kwantyfikowana schematem typowy </summary>
    | TWZmienna    of string
    /// <summary> zmienna typowa kwantyfikowana abstrakcją </summary>
    | TZmienna     of string
    /// <summary> typ funkcji (T -> T) </summary>
    | TFunkcja     of Typ * Typ
    /// <summary> konstruktor abstrakcji typowej </summary>
    | TLambda      of string * Typ
    /// <summary> konstruktor abstrakcji z anotowanym argumentem </summary>
    | TLambdaAnot  of string * Rodzaj * Typ
    /// <summary> konstruktor aplikacji typowe  </summary>
    | TAplikacja   of Typ * Typ
    /// <summary> typ uniwersalny </summary>
    | TUniwersalny of string * Typ
    /// <summary> anotacja rodzajowa (T :: K) </summary>
    | TAnotacja    of Typ * Rodzaj

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
