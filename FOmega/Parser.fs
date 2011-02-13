/// <summary>
/// W tym module znajduje się parser.
/// </summary>
module Parser

open Core
open Parsor.Core
open Parsor.Primitives
open Parsor.Combinators
open Parsor.State

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
    (skipString str >>. ws >>. parsor.Return f) <?> str

let private slowoKluczowe s =
    match s with
    | "All" | "Bool" | "Nat"
    | "case" | "else" | "false" | "if" | "in" | "left" | "let" 
    | "of" | "right" | "then" | "tlet" | "true" -> true
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
    (skipString k >>. ws) <?> k

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
    (skipChar '{' >>. ws <!> fun () ->
        parsor{
            let! t1 = typ.Parsor;
            do! skipChar ',' .>> ws;
            let! t2 = typ.Parsor;
            do! skipChar '}' .>> ws;
            return TPara(t1,t2)
        }) <|>
    (skipChar '<' >>. ws <!> fun () ->
        parsor{
            let! t1 = typ.Parsor;
            do! skipChar '|' .>> ws;
            let! t2 = typ.Parsor;
            do! skipChar '>' .>> ws;
            return TKopara(t1,t2)
        }) <|>
    (kw "All" <!> fun () ->
        parsor{
            let! tvar = ident true;
            let tvar2 = Fresh.swierzaNazwa();
            let! kind =
                parsor{
                    let! hasKind = tryParse (kw "::");
                    match hasKind with
                    | None -> return Fresh.swierzaNazwa() |> KWZmienna
                    | Some _ ->
                        return! rodzaj.Parsor
                };
            do! skipChar '.' >>. ws;
            let! rest = localStateUpdateV (fun s -> (tvar, tvar2)::s) typ.Parsor;
            return TUniwersalny(tvar,tvar2,kind,rest)
        }) <|>
    (kw "\\" <!> fun () ->
        parsor{
            let! tvar = ident true;
            let tvar2 = Fresh.swierzaNazwa();
            let! kind =
                parsor{
                    let! hasKind = tryParse (kw "::");
                    match hasKind with
                    | None -> return Fresh.swierzaNazwa() |> KWZmienna
                    | Some _ ->
                        return! rodzaj.Parsor
                    };
            do! skipChar '.' >>. ws;
            let! rest = localStateUpdateV (fun s -> (tvar, tvar2)::s) typ.Parsor;
            return TLambda(tvar,tvar2,kind,rest)
        }) <|>
    (kw "Nat" <!> fun () -> parsor.Return TNat) <|>
    (kw "Bool" <!> fun () -> parsor.Return TBool) <|>
    (ident true <!> fun x -> 
        parsor{
            let! var = getState;
            match List.tryFind (fun z -> x = fst z) var with
            | Some(x, x2) ->
                return TZmienna(x, x2)
            | None ->
                do! warning ("Undefined type variable " + x + ".");
                return TZmienna(x, Fresh.swierzaNazwa())
        })

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
    (withPos(skipChar '{' >>. ws) <!> fun (_,pos) ->
        parsor{
            let! e1 = wyrazenie.Parsor;
            do! skipChar ',' .>> ws;
            let! e2 = wyrazenie.Parsor;
            do! skipChar '}' .>> ws;
            return EPara(e1,e2,pos)
        }) <|>
    (withPos (kw "\\") <!> fun (_,pos) ->
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
            return ELambda(var,typ,rest,pos)
        }) <|>
    (withPos(kw "\\\\") <!> fun (_,pos) ->
        parsor{
            let! tvar = ident true;
            let tvar2 = Fresh.swierzaNazwa();
            let! kind =
                parsor{
                    let! hasKind = tryParse (kw "::");
                    match hasKind with
                    | None -> return Fresh.swierzaNazwa() |> KWZmienna
                    | Some _ ->
                        return! rodzaj.Parsor
                    };
            do! skipChar '.' >>. ws;
            let! rest = localStateUpdateV (fun s -> (tvar,tvar2)::s) wyrazenie.Parsor;
            return ETLambda(tvar,tvar2,kind,rest,pos)
        }) <|>
    (withPos(kw "let") <!> fun (_,pos) ->
        parsor{
            let! x = ident false .>> skipChar '=' .>> ws;
            let! e1 = wyrazenie.Parsor .>> kw "in";
            let! e2 = wyrazenie.Parsor;
            return ELet(x, e1, e2,pos)
        }) <|>
    (withPos(kw "tlet") <!> fun (_,pos) ->
        parsor{
            let! x = ident true .>> skipChar '=' .>> ws;
            let x2 = Fresh.swierzaNazwa();
            let! t = typ.Parsor .>> kw "in";
            let! e = localStateUpdateV (fun s -> (x,x2)::s) wyrazenie.Parsor;
            return ETLet(x, x2, t, e, pos)
        }) <|>
    (withPos(kw "true") <!> fun (_,pos) -> parsor.Return(ETrue pos)) <|>
    (withPos(kw "false") <!> fun (_,pos) -> parsor.Return(EFalse pos)) <|>
    (withPos(kw "if") <!> fun (_,pos) ->
        parsor{
            let! e1 = wyrazenie.Parsor;
            do! kw "then"
            let! e2 = wyrazenie.Parsor;
            do! kw "else"
            let! e3 = wyrazenie.Parsor;
            return EIf(e1, e2, e3, pos)
        }) <|>
    (withPos(kw "left") <!> fun (_,pos) ->
        parsor{
            let! e = wyrazenie.Parsor;
            return ELewy(e, pos)
        }) <|>
    (withPos(kw "right") <!> fun (_,pos) ->
        parsor{
            let! e = wyrazenie.Parsor;
            return EPrawy(e, pos)
        }) <|>
    (withPos(kw "case") <!> fun (_,pos) ->
        parsor{
            let! e1 = wyrazenie.Parsor;
            do! kw "of" |> skip;
            do! tryParse(skipChar '|' .>> ws) |> skip
            do! kw "left" |> skip;
            do! kw "=>" |> skip;
            let! e2 = wyrazenie.Parsor;
            do! skipChar '|' .>> ws;
            do! kw "right" |> skip;
            do! kw "=>" |> skip;
            let! e3 = wyrazenie.Parsor;
            return ECase(e1,e2,e3,pos)
        }) <|>
    (withPos(ident false) <!> fun x -> parsor{ return EZmienna x }) <|>
    (withPos(localStateV () pnumber .>> ws) <!> fun n -> parsor.Return( ENat n ))
do
    wyrazenie.Atom <- atom <??> "expresion"
    wyrazenie.AddSuffixOperator (parsor{ let! arg = atom in return fun x -> EAplikacja(x, arg, arg.Polozenie) }) 8
    wyrazenie.AddSuffixOperator (
        parsor{ 
            let! arg = withPos(skipChar '[' >>. ws >>. typ.Parsor .>> skipChar ']' .>> ws) in 
            return fun x -> ETAplikacja(x, fst arg, snd arg) 
        }) 8
    wyrazenie.AddSuffixOperator (withPos(kw ":") <!> fun (_,pos) -> parsor{ let! typ = typ.Parsor in return fun x -> EAnotacja(x, typ, pos) }) 8
    let aoper s f = kw s >>. parsor.Return(fun a b -> EOpArytmetyczny(a, b, s, f, a.Polozenie))
    wyrazenie.AddInfixOperator (aoper "+" ( + )) Parsor.Expresion.LeftAssoc 4
    wyrazenie.AddInfixOperator (aoper "-" ( - )) Parsor.Expresion.LeftAssoc 4
    wyrazenie.AddInfixOperator (aoper "/" ( / )) Parsor.Expresion.LeftAssoc 5
    wyrazenie.AddInfixOperator (aoper "*" ( * )) Parsor.Expresion.LeftAssoc 5
    let coper s f = box(kw s) >>. parsor.Return(fun a b -> EOpPorownania(a, b, s, f, a.Polozenie))
    wyrazenie.AddInfixOperator (coper ">=" (>=)) Parsor.Expresion.LeftAssoc 2
    wyrazenie.AddInfixOperator (coper "<=" (<=)) Parsor.Expresion.LeftAssoc 2
    wyrazenie.AddInfixOperator (coper "=" (=)) Parsor.Expresion.LeftAssoc 2
    wyrazenie.AddInfixOperator (coper "<>" (<>)) Parsor.Expresion.LeftAssoc 2
    wyrazenie.AddInfixOperator (coper ">" (>)) Parsor.Expresion.LeftAssoc 2
    wyrazenie.AddInfixOperator (coper "<" (<)) Parsor.Expresion.LeftAssoc 2

    wyrazenie.AddSuffixOperator (withPos(kw ".left") <!> fun (_,pos) -> parsor.Return(fun x -> EProjLewy(x,pos))) 12
    wyrazenie.AddSuffixOperator (withPos(kw ".right") <!> fun (_,pos) -> parsor.Return(fun x -> EProjPrawy(x,pos))) 12

let parsujText text =
    parseString (ws >>. wyrazenie.Parsor .>> eof) [] (OutputConsole()) text