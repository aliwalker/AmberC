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

    switch (tok.kind)
    {
        case Token.IDENT:
            return semaIdent(tok.stringVal, tok.pos);

        case Token.INT:
            return semaInt(tok.intVal, tok.intsfx, tok.pos);

        case Token.FLOAT:
            return semaFloat(tok.floatVal, );
    }
}