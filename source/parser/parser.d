///     This file contains the parsing utilities.
///     Copyright 2019 Yiyong Li.

module parser.parser;

import std.format;
import parser.ast;
import parser.lexer;
import parser.types;
import sema.expr;
import reporter;

/// Consume a token and expect it to be a separator.
private void expectSep(ref TokenStream tokstr, string sep)
{
    if (!tokstr.matchSep(sep))
    {
        report(
            SVR_ERR,
            format!"expected '%s'"(sep),
            SrcLoc(tokstr.peek.pos, tokstr.filename)
        );

        // TODO: error recovery.
    }
}

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

/// call-and-subs:
///     postfix '[' expression ']'
///              ^
///     postfix '(' arg-list ')'
///              ^
///
/// arg-list:
///     assignment, arg-list
Expr parseCallAndSubs(ref TokenStream tokstr, Expr lhs)
{
    auto tok = tokstr.read();

    assert(tok.kind == Token.SEP);
    assert(tok.stringVal == "(" || tok.stringVal == "[");

    Expr callsub;
    Expr[] args;

    while (
        (tok.kind == Token.SEP) &&
        ((tok.stringVal == "(") || 
        (tok.stringVal == "["))
    )
    {
        // Accumulate.
        lhs = callsub;

        // Subscript
        if (tok.stringVal == "[")
        {
            auto idx = parseExpr(tokstr);
            callsub = semaArrayDeref(lhs, idx);
            expectSep(tokstr, "]");
        }
        // Call.
        else
        {
            args ~= parseAssignment(tokstr);
            
            // Exhaust the args.
            while (!tokstr.matchSep(")"))
            {
                tokstr.expectSep(",");
                args ~= parseAssignment(tokstr);
            }
            callsub = semaCall(lhs, args, tok.loc);
        }

        // Advance.
        tok = tokstr.read();
    }

    tokstr.unread();
    return callsub;
}

/// primary-expression:
///     identifier
///     constant
///     string-literal
///     paren
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
            assert(tok.stringVal == "(");
            return parseParen(tokstr);

        default:
            assert(false, "ParsePrimary called on non-primary tokens");
    }
}

/// paren:
///     (expr)                      - grouping.
///     (type) { initalizer-list }  - compound literal.
Expr parseParen(ref TokenStream tokstr)
{
    auto tok = tokstr.read();

    assert(
        (tok.kind == Token.SEP) &&
        (tok.stringVal == "(")
    );
    
}

/// Try to parse a type from [tokstr]. This function
/// fails silently and returns [null]; otherwise it returns
/// the type parsed.
private Type tryParseType(ref TokenStream tokstr)
{
    
}

/// Test parsePrimary.
unittest
{
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
}