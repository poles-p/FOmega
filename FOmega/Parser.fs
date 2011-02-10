/// <summary>
/// W tym module znajduje się parser.
/// </summary>
module Parser

open Core
open Parsor.Core
open Parsor.Primitives
open Parsor.Combinators

/// <summary>
/// Białe znaki i komentarze. Komentarzem jest ciąg znaków zaczynający się podwójnym ukośnikiem i kończący się 
/// znakiem nowego wiersza, albo ciąg znaków pomiędzy sekwencjami '/*' i '*/)'
/// </summary>
let private ws =
    whiteSpace >>. 
    skipMany (
        (skipString "//" <!> fun () -> skipLine) <|>
        (skipString "/*" <!> fun () -> skipUntil (skipString "*/") getToken .>> whiteSpace)
    ) 

let private oper str f =
    skipString str >>. ws >>. parsor.Return f

let private slowoKluczowe s =
    match s with
    | "All"
    | "in" | "let" | "tlet" -> true
    | _ -> false

let private ident upper =
    let id =
        parsor{
            let! c0 = letter;
            let! cs = (manyChars (letter <|> digit) .>> ws)
            return (c0.ToString() + cs);
        } <?> "identifier"
    in
        parsor{
            let! i = lookAhead id
            if slowoKluczowe i then 
                return! fatalError (i + " is a keyword. Expected identifier.")
            elif upper = System.Char.IsUpper i.[0] then
                return! id
            elif upper then
                return! fatalError ("First letter of type variables should be uppercase")
            else
                return! fatalError ("First letter of variables should be lowercase")
        }

let kw k =
    skipString k >>. ws

/// <summary>
/// Parser rodzajów
/// </summary>
let private rodzaj = Parsor.Expresion.ExpresionParsor()
do
    rodzaj.Atom <-
        (
            (skipChar '(' >>. ws <!> fun () ->
                rodzaj.Parsor .>> skipChar ')' .>> ws
            ) <|>
            (skipChar '*' >>. ws <!> fun () -> parsor.Return KGwiazdka)
        ) <??> "kind";
    rodzaj.AddInfixOperator (oper "=>" (fun a b -> KFunkcja(a, b))) Parsor.Expresion.Assoc.RightAssoc 4

/// <summary>
/// Parser typow
/// </summary>
let private typ = Parsor.Expresion.ExpresionParsor()
let private atomTypu =
    (skipChar '(' >>. ws <!> fun () ->
        typ.Parsor .>> skipChar ')' .>> ws
    ) <|>
    (kw "All" <!> fun () ->
        parsor{
            let! tvar = ident true;
            let! kind =
                parsor{
                    let! hasKind = tryParse (kw "::");
                    match hasKind with
                    | None -> return Fresh.swierzaNazwa() |> KWZmienna
                    | Some _ ->
                        return! rodzaj.Parsor
                    };
            do! skipChar '.' >>. ws;
            let! rest = typ.Parsor;
            return TUniwersalny(tvar,kind,rest)
        }) <|>
    (kw "\\" <!> fun () ->
        parsor{
            let! tvar = ident true;
            let! kind =
                parsor{
                    let! hasKind = tryParse (kw "::");
                    match hasKind with
                    | None -> return Fresh.swierzaNazwa() |> KWZmienna
                    | Some _ ->
                        return! rodzaj.Parsor
                    };
            do! skipChar '.' >>. ws;
            let! rest = typ.Parsor;
            return TLambda(tvar,kind,rest)
        }) <|>
    (ident true <!> fun x -> parsor.Return(TZmienna x))
do
    typ.Atom <- atomTypu <??> "type constructor";
    typ.AddInfixOperator (oper "->" (fun a b -> TFunkcja(a, b))) Parsor.Expresion.Assoc.RightAssoc 4
    typ.AddSuffixOperator (parsor{ let! arg = atomTypu in return fun x -> TAplikacja(x, arg) }) 8
    typ.AddSuffixOperator (kw "::" <!> fun () -> parsor{ let! kind = rodzaj.Parsor in return fun x -> TAnotacja(x, kind) }) 8

/// <summary>
/// Parser wyrażeń
/// </summary>
let private wyrazenie = Parsor.Expresion.ExpresionParsor();
let private atom =
    (skipChar '(' >>. ws <!> fun () ->
        wyrazenie.Parsor .>> skipChar ')' .>> ws
    ) <|>
    (kw "\\" <!> fun () ->
        parsor{
            let! var = ident false;
            let! typ =
                parsor{
                    let! hasType = tryParse (kw ":");
                    match hasType with
                    | None -> return Fresh.swierzaNazwa() |> TWZmienna
                    | Some _ ->
                        return! typ.Parsor
                    };
            do! skipChar '.' >>. ws;
            let! rest = wyrazenie.Parsor;
            return ELambda(var,typ,rest)
        }) <|>
    (kw "\\\\" <!> fun () ->
        parsor{
            let! tvar = ident true;
            let! kind =
                parsor{
                    let! hasKind = tryParse (kw "::");
                    match hasKind with
                    | None -> return Fresh.swierzaNazwa() |> KWZmienna
                    | Some _ ->
                        return! rodzaj.Parsor
                    };
            do! skipChar '.' >>. ws;
            let! rest = wyrazenie.Parsor;
            return ETLambda(tvar,kind,rest)
        }) <|>
    (kw "let" <!> fun () ->
        parsor{
            let! x = ident false .>> skipChar '=' .>> ws;
            let! e1 = wyrazenie.Parsor .>> kw "in";
            let! e2 = wyrazenie.Parsor;
            return ELet(x, e1, e2)
        }) <|>
    (kw "tlet" <!> fun () ->
        parsor{
            let! x = ident true .>> skipChar '=' .>> ws;
            let! t = typ.Parsor .>> kw "in";
            let! e = wyrazenie.Parsor;
            return ETLet(x, t, e)
        }) <|>
    (ident false <!> fun x -> parsor{ return EZmienna x })
do
    wyrazenie.Atom <- atom <??> "expresion"
    wyrazenie.AddSuffixOperator (parsor{ let! arg = atom in return fun x -> EAplikacja(x, arg) }) 8
    wyrazenie.AddSuffixOperator (
        parsor{ 
            let! arg = skipChar '[' >>. ws >>. typ.Parsor .>> skipChar ']' .>> ws in 
            return fun x -> ETAplikacja(x, arg) 
        }) 8
    wyrazenie.AddSuffixOperator (kw ":" <!> fun () -> parsor{ let! typ = typ.Parsor in return fun x -> EAnotacja(x, typ) }) 8

let parsujText text =
    parseString (ws >>. wyrazenie.Parsor .>> eof) () (OutputConsole()) text