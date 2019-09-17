///     This file contains the parsing utilities.
///     Copyright 2019 Yiyong Li.

module parser.parser;

import parser.ast;
import parser.lexer;
import parser.ctypes;
import parser.sema;

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
        default:
            assert(false, "ParsePrimary called on non-primary tokens");
    }
}