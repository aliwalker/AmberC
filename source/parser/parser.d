///     This file contains the parsing utilities.
///     Copyright 2019 Yiyong Li.

module parser.parser;

import std.stdint;
import std.format;
import std.algorithm;
import parser.ast;
import parser.lexer;
import parser.types;
import sema.expr;
import sema.env;
import reporter;
debug import std.stdio;

/// Report parsing error and perfomrs error recovery.
private Expr parseError(ref TokenStream tokstr, string msg, SrcLoc loc)
{
    // TODO: error recovery on TokenStream.

    report(
        SVR_ERR,
        msg,
        loc
    );

    return null;
}

/// Consume a token and expect it to be a separator.
private void expectSep(ref TokenStream tokstr, string sep)
{
    if (!tokstr.matchSep(sep))
    {
        parseError(
            tokstr,
            format!"expected '%s'"(sep),
            SrcLoc(tokstr.peek.pos, tokstr.filename)
        );
    }
}

/**
Helpers for comparing token.stringVal
*/

private bool compTokStr(Token tok, int kind, string val)
{
    return (tok.kind == kind) && (tok.stringVal == val);
}

private bool isPostfixUOp(Token tok)
{
    auto pfx = ["++", "--", ".", "->", "[", "("];

    return any!((val) => compTokStr(tok, Token.SEP, val))(pfx);
}

private bool isPrefixUOp(Token tok)
{
    auto uop = ["++", "--", "sizeof", "&", "*", "+", "-", "~", "!"];

    return any!((val) => compTokStr(tok, Token.SEP, val))(uop);
}

private bool isQualifier(Token tok)
{
    return any!((val) => compTokStr(tok, Token.KW, val))(tqualkw);
}

/**

Expressions.

*/

/// expression:
///
Expr parseExpr(ref TokenStream tokstr)
{
    return parseAssignment(tokstr);
}

/// 
Expr parseAssignment(ref TokenStream tokstr)
{
    return null;
}

/// Unary:
///     postfix
///     ("++" | "--") unary
///     uop unary
///     "sizeof" unary
///     "sizeof" '(' type-name ')'
///     ^
Expr parseUnary(ref TokenStream tokstr)
{
    if (tokstr.peek().isPrefixUOp())
    {
        auto tokop = tokstr.read();
        // TODO:
        // Handle "sizeof" '(' type-name ')' separately.

        auto rhs = parseUnary(tokstr);

        // Handle parsing/semantic errors.
        if (rhs is null)
        {
            return null;
        }

        switch (tokop.stringVal)
        {
            case "++", "--":
                return semaIncrDecr!"pre"(
                    rhs, 
                    tokop.stringVal, 
                    SrcLoc(tokop.pos, tokstr.filename));
            
            case "sizeof":
                // the opnd of "sizeof" operator is not evaluated.
                return new IntExpr(
                    uintType, 
                    rhs.type.typeSize,
                    rhs.loc);

            case "&":
                return semaAddrof(
                    rhs, 
                    SrcLoc(tokop.pos, 
                    tokstr.filename));

            case "*":
                return semaDeref(rhs);

            case "+", "-", "~", "!":
                return semaUAOp(
                    tokop.stringVal, 
                    rhs, 
                    SrcLoc(tokop.pos, tokstr.filename));

            default:
                assert(false);
        }
    }
    // Descend to postfix.
    else
    {
        return parsePostfix(tokstr);
    }
}

/// Postfix:
///     primary operators*
///     ^
/// Operators:
///     increment-decrement-suffix operators*
///     | rec-access operators*
///     | call-and-subs operators*
Expr parsePostfix(ref TokenStream tokstr)
{
    auto expr = parsePrimary(tokstr);

    // Exhaust postfix operators.
    while (expr && isPostfixUOp(tokstr.peek()))
    {
        switch (tokstr.peek().stringVal)
        {
            case "++", "--":    expr = parseIncrDecrSfx(tokstr, expr); break;
            case ".", "->":     expr = parseRecAccess(tokstr, expr); break;
            case "[", "(":      expr = parseCallAndSubs(tokstr, expr); break;
            default:
                assert(false, "Not implemented");
        }
    }
    return expr;
}

/// Increment-decrement-suffix:
///     ("++" | "--")+
///     ^
Expr parseIncrDecrSfx(ref TokenStream tokstr, Expr lhs)
{
    auto tok = tokstr.read();
    assert(tok.kind == Token.SEP);
    assert(tok.stringVal == "++" || tok.stringVal == "--");
    assert(lhs);

    while (
        lhs &&
        (tok.kind == Token.SEP) &&
        (tok.stringVal == "++" || tok.stringVal == "--")
    )
    {
        lhs = semaIncrDecr!"post"(lhs, tok.stringVal, SrcLoc(tok.pos, tokstr.filename));
        tok = tokstr.read();
    }
    tokstr.unread();
    return lhs;
}

/// Rec-access:
///     (('.' | '->') identifier)+
///     ^
Expr parseRecAccess(ref TokenStream tokstr, Expr struc)
{
    auto tok = tokstr.read();
    assert(tok.kind == Token.SEP);
    assert(tok.stringVal == "." || tok.stringVal == "->");
    assert(struc);

    while (
        struc &&
        (tok.kind == Token.SEP) &&
        (tok.stringVal == "." || tok.stringVal == "->")
    )
    {
        // Read the identifier(member name).
        auto tokIdent = tokstr.read();
        if (tokIdent.kind != Token.IDENT)
        {
            return parseError(
                tokstr,
                "expect identifier",
                SrcLoc(tokIdent.pos, tokstr.filename)
            );
        }

        // Deref the pointer.
        if (tok.stringVal == "->")
        {
            struc = semaDeref(struc);
        }

        // Stop the rest if deref error.
        if (struc is null)
        {
            return null;
        }

        struc = semaMemberExpr(
            struc, 
            tokIdent.stringVal, 
            SrcLoc(tokIdent.pos, tokstr.filename)
        );

        tok = tokstr.read();
    }

    tokstr.unread();
    return struc;
}

/// Call-and-subs:
///     ('[' expression ']' | '(' arg-list ')')+
///     ^
/// arg-list:
///     assignment, arg-list
Expr parseCallAndSubs(ref TokenStream tokstr, Expr lhs)
{
    auto tok = tokstr.read();

    assert(lhs !is null);
    assert(tok.kind == Token.SEP);
    assert(tok.stringVal == "(" || tok.stringVal == "[");

    Expr[] args;

    while (
        lhs &&
        (tok.kind == Token.SEP) &&
        ((tok.stringVal == "(") || 
        (tok.stringVal == "["))
    )
    {
        // Subscript
        if (tok.stringVal == "[")
        {
            auto idx = parseExpr(tokstr);
            lhs = semaDeref(lhs, idx);
            expectSep(tokstr, "]");
        }
        // Call.
        else
        {   
            // Exhaust the args.
            while (!tokstr.matchSep(")"))
            {
                tokstr.expectSep(",");
                auto arg = parseAssignment(tokstr);

                // If error occurs during parseAssignment,
                // stop parsing the rest args.
                if (arg is null)
                {
                    return null;
                }

                args ~= arg;
            }
            lhs = semaCall(lhs, args, SrcLoc(tok.pos, tokstr.filename));
        }

        // Advance.
        tok = tokstr.read();
    }

    tokstr.unread();
    return lhs;
}

/// primary-expression:
///     identifier
///     | constant
///     | string-literal
///     | paren
Expr parsePrimary(ref TokenStream tokstr)
{
    auto tok = tokstr.read();
    auto loc = SrcLoc(tok.pos, tokstr.filename);

    switch (tok.kind)
    {
        case Token.IDENT:   return semaIdent(tok.stringVal, loc);
        case Token.INT:     return semaInt(tok.intVal, tok.intsfx, loc);
        case Token.FLOAT:   return semaFloat(tok.floatVal, tok.fsfx, loc);
        case Token.STRING:  return new StringExpr(tok.stringVal, loc);
        case Token.SEP:
            if (tok.stringVal != "(")
            {
                return parseError(
                    tokstr,
                    "expect '('",
                    SrcLoc(tok.pos, tokstr.filename)
                );
            }
            return parseParen(tokstr);

        default:
            assert(false, "ParsePrimary called on non-primary tokens");
    }
}

/// paren:
///     (expr)                             - grouping.
///     | (type - name) { initalizer-list }  - compound literal.
///     ^
Expr parseParen(ref TokenStream tokstr)
{
    assert(
        (tokstr.peek.kind == Token.SEP) &&
        (tokstr.peek.stringVal == "(")
    );

    auto type = tryParseTypeName(tokstr);
    if (type)
    {
        // TODO: compound-literal.
    }
    
    return parseExpr(tokstr);
}

/// Try to parse a type name.
Type tryParseTypeName(ref TokenStream tokstr)
{
    // TODO
    return null;
}

/// Try to parse a qualifier.
uint8_t tryParseTypeQual(ref TokenStream tokstr)
{
    uint8_t quals = 0;
    auto tok = tokstr.read();
    while (isQualifier(tok))
    {
        switch (tok.stringVal)
        {
            case "const":    quals |= QUAL_CONST; break;
            case "register": quals |= QUAL_REG; break;
            default:
                report(
                    SVR_WARN,
                    format!"qualifier '%s' is not implemented"(tok.stringVal),
                    SrcLoc(tok.pos, tokstr.filename),
                );
        }
        tok = tokstr.read();
    }

    tokstr.unread();
    return quals;
}

/// Try to parse an object type specifier from [tokstr].
/// This function fails silently and returns [null]; 
/// otherwise it returns the type parsed.
Type tryParseObjTypeSpec(ref TokenStream tokstr)
{
    // Try matching a type specifier.
    string matched = "";
    foreach (kw; tkw)
    {
        if (tokstr.matchKW(kw))
        {
            matched = kw;
            break;
        }
    }

    switch (matched)
    {
        case "":            break; // Does not match.
        case "_Bool":       return boolType;
        case "char":        return charType;
        case "short":
            tokstr.matchKW("int");
            return shortType;

        case "int":         return parseIntTypeSpec!"signed"(tokstr);
        case "signed":      return parseIntTypeSpec!"signed"(tokstr);
        case "unsigned":    return parseIntTypeSpec!"unsigned"(tokstr);
        case "long":
            // long long (int)
            if (tokstr.matchKW("long"))
            {
                tokstr.matchKW("int");
                return llongType;
            }
            // long (int)
            else
            {
                return longType;
            }

        case "float":       return floatType;
        case "double":      return doubleType;
        case "void":        return voidType;
        case "struct":      return parseAggregTypeSpec(tokstr, false);
        case "union":       return parseAggregTypeSpec(tokstr, true);
        
        default:
            // TODO: typedef name and enum.
            return null;
    }
    return null;
}

/// intTypeSpec:
///     "signed" intType
///              ^
///     "unsigned" intType
///                ^
/// intType:
///     "char" | "short" | "int" | "long" | "long long"
Type parseIntTypeSpec(string pref)(ref TokenStream tokstr)
{
    static assert(pref == "signed" || pref == "unsigned");
    static if (pref == "signed")
    {
        Type charTy = scharType;
        Type shortTy = shortType;
        Type intTy = intType;
        Type longTy = longType;
        Type llongTy = llongType;
    }
    else
    {
        Type charTy = ucharType;
        Type shortTy = ushortType;
        Type intTy = uintType;
        Type longTy = ulongType;
        Type llongTy = ullongType;
    }

    auto tok = tokstr.read();

    // "signed" or "unsigned".
    if (tok.kind != Token.KW)
    {
        tokstr.unread();
        return intTy;
    }

    switch (tok.stringVal)
    {
        case "char":
            return charTy;

        case "short":
            // "signed short int". "int" is optional.
            tokstr.matchKW("int");
            return shortTy;

        case "int":
            return intTy;
        
        case "long":
            // "signed long long (int)"
            if (tokstr.matchKW("long"))
            {
                tokstr.matchKW("int");
                return llongTy;
            }
            // "signed long (int)"
            else
            {
                tokstr.matchKW("int");
                return longTy;
            }

        default:
            report(
                SVR_ERR,
                format!"unexpected keyword '%s'"(tok),
                SrcLoc(tok.pos, tokstr.filename)
            );

            return null;
    }
}

/// aggregTypeSpec:
///     ("struct" | "union") (name)? ('{' struct-decl-list '}')?
///                          ^
/// struct-decl-list:
///     (spec-qual-list declarator ';')*
Type parseAggregTypeSpec(ref TokenStream tokstr, bool isUnion)
{
    auto isDef = false;
    auto tok = tokstr.read();

    RecType recType;
    RecField[] decls;
    string ident;

    // identifier.
    if (tok.kind == Token.IDENT)
    {
        ident = tok.stringVal;
    }

    // struct-decl-list.
    if (tokstr.matchSep("{"))
    {
        // Already exists.
        if (getRecType(ident, ident, isUnion))
        {
            report(
                SVR_ERR,
                format!"redeclaration of '%s'"(ident),
                SrcLoc(tok.pos, tokstr.filename),
            );
            return null;
        }

        isDef = true;
        decls = parseStructDeclList(tokstr);
    }

    string tystr;
    recType = getRecType(ident, decls, tystr, isUnion);

    if (isDef)
    {
        // Rm this type info at the end of this env.
        envAddExitCb(()
        {
            removeType(tystr);
        });
    }
    return recType;
}

/// struct-decl-list:
///     (spec-qual-list declarator)* ;
///
RecField[] parseStructDeclList(ref TokenStream tokstr)
{
    // TODO:
    tokstr.expectSep("}");
    return null;
}

/// Test parsePrimary.
unittest
{
    uniProlog();
    void testPrimary(T)(string code, T ast)
    {
        static assert(
            is (T == IntExpr)   ||
            is (T == FloatExpr) ||
            is (T == StringExpr)
        );

        auto tokstr = TokenStream(code, "dummy.c");
        auto e = parsePrimary(tokstr);
        auto expr = cast(T)e;

        assert(expr !is null);
        assert(expr.value == ast.value);
        assert(expr.type == ast.type);
    }

    // Test integer.
    testPrimary("56L;", new IntExpr(longType, 56, SrcLoc()));

    // Test FP.
    testPrimary("23.20", new FloatExpr(floatType, 23.20, SrcLoc()));

    // Test overflow.
    testPrimary("2147483650", new IntExpr(intType, -2_147_483_646, SrcLoc()));

    // Test overflow.
    testPrimary("4294967298", new IntExpr(intType, 2, SrcLoc()));

    // Test string.
    testPrimary("\"dummy string\";", new StringExpr("dummy string", SrcLoc()));
    
    uniEpilog();
}

/// Test parseIntTypeSpec
unittest
{
    uniProlog();
    void testParseIntTypeSpec(string pref)(string code, Type etype)
    {
        auto tokstr = TokenStream(code, "testIntTypeSpec.c");
        auto type = parseIntTypeSpec!(pref)(tokstr);

        assert(type == etype);
    }

    testParseIntTypeSpec!"signed"(
        "short int",
        shortType
    );

    testParseIntTypeSpec!"unsigned"(
        "long",
        ulongType
    );

    testParseIntTypeSpec!"signed"(
        "int",
        intType
    );

    uniEpilog();
}

/// Test tryParseObjTypeSpec
unittest
{
    uniProlog();

    void testTryParseObjTypeSpec(string code, Type etype)
    {
        auto tokstr = TokenStream(code, "testTryParseObjTypeSpec.c");
        auto type = tryParseObjTypeSpec(tokstr);

        assert(type == etype);
    }

    testTryParseObjTypeSpec(
        "_Bool",
        boolType
    );

    testTryParseObjTypeSpec(
        "int",
        intType
    );

    testTryParseObjTypeSpec(
        "long long",
        llongType
    );

    testTryParseObjTypeSpec(
        "long long int",
        llongType
    );

    uniEpilog();
}

/// Test parsePostfix
unittest
{
    uniProlog();

    auto tokstr = TokenStream("56L", "testParsePostfix.c");
    auto intExpr = cast(IntExpr)parsePostfix(tokstr);
    assert(intExpr);
    assert(intExpr.value == 56);
    assert(intExpr.type == longType);

    void testParseCallExpr(FuncDecl funcDecl, string argstr = "")
    {
        assert(funcDecl);
        envPush();
        envAddDecl(funcDecl.name, funcDecl);
        
        auto tokstr = TokenStream(
            format!"%s(%s)"(funcDecl.name, argstr), 
            "testParseCallExpr.c"
        );
        auto expr = cast(CallExpr)parsePostfix(tokstr);
        assert(expr);
        assert(cast(IdentExpr)expr.callee);

        envPop();
    }

    /// Test function with no arg.
    testParseCallExpr(new FuncDecl(
        getFuncType(intType, []),/* function type */
        "foo",                   /* function name */
        null,                    /* AST of the function body. */
        SrcLoc()
    ));

    // func with "void" as arg.
    testParseCallExpr(new FuncDecl(
        getFuncType(intType, [voidType]),
        "foo",
        null,
        SrcLoc()
    ));

    uniEpilog();
}

/// Test parseUnary
unittest
{
    uniProlog();

    auto declVarA = new VarDecl(
        intType,
        "a",
        null,
        SrcLoc(),
    );
    envPush();
    envAddDecl("a", declVarA);

    auto tokstr = TokenStream("a++", "testParseUnary.c");
    auto expr = parseUnary(tokstr);
    assert(expr);
    assert(cast(UnaryExpr)expr);
    
    tokstr = TokenStream("b++", "testParseUnary.c");
    expr = parseUnary(tokstr);
    assert(!expr, "b is not declared");

    tokstr = TokenStream("++a++", "testParseUnary.c");
    expr = parseUnary(tokstr);
    assert(!expr, "a++ is an rvalue");

    tokstr = TokenStream("&a", "testParseUnary.c");
    expr = parseUnary(tokstr);
    assert(expr);

    tokstr = TokenStream("&\"string\"", "testParseUnary.c");
    expr = parseUnary(tokstr);
    assert(!expr);

    envPop();
    uniEpilog();
}