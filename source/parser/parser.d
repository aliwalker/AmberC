///     This file contains the parsing utilities.
///     Copyright 2019 Yiyong Li.

module parser.parser;

import parser.ast;
import parser.lexer;
import parser.ctypes;
import sema.expr;

/// primary-expression:
///     identifier
///     constant
///     string-literal
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
            // TODO:
            
        default:
            assert(false, "ParsePrimary called on non-primary tokens");
    }
}

/// Test parsePrimary.
unittest
{
    // Test integer.
    auto tokstr = TokenStream("56L;", "dummy.c");
    auto expr = parsePrimary(tokstr);
    auto intExpr = cast(IntExpr)expr;
    assert(intExpr !is null);
    assert(intExpr.value == 56);
    assert(intExpr.type == longType);

    // Test FP.
    tokstr = TokenStream("23.20", "dummy.c");
    expr = parsePrimary(tokstr);
    auto fpExpr = cast(FloatExpr)expr;
    assert(fpExpr !is null);
    assert(fpExpr.value == 23.20);
    assert(fpExpr.type == floatType);
}