///     This file contains the parser.
///     Copyright 2019 Yiyong Li.

module parser.parser;

import parser.ast;
import parser.lexer;
import parser.ctypes;

/// Context for parsing.
class Context
{
    /// Tokens to be consumed.
    Token[] tokens;

    /// Constructor.
    this(wstring code, string filename)
    {
        assert(code);
        
        auto chars = CharStream(code, filename);
        auto lexRes = lexStream(chars);

        if (!lexRes.errors.empty)
        {
            reportError(lexRes.errors);
        }
        this.tokens = lexRes.tokens;
    }

    /// Peek current token without consuming it.
    Token peek()
    {
        assert(!tokens.empty);
        return tokens[0];
    }

    /// Consumes and returns the current token.
    Token get()
    {
        assert(!tokens.empty);
        auto tok = tokens[0];
        tokens = tokens[1 .. $];
        return tok;
    }

    /// Matches token that has a stringVal.
    /// Returns true and consumes the matched token; otherwise
    /// returns false and leaves tokens unchanged.
    bool match(Token.Kind kind, wstring str)
    {
        if ((tokens[0].kind == kind) && (tokens[0].stringVal == str))
        {
            tokens = tokens[1 .. $];
            return true;
        }
        return false;
    }

    /// Matches keyword.
    bool matchKW(wstring kw)
    {
        return match(Token.KW, kw);
    }

    /// Matches and consumes separator [sep].
    bool matchSep(wstring sep)
    {
        return match(Token.SEP, sep);
    }

    /// Expect a token of [kind] and consumes it.
    bool expect(Token.kind kind)
    {
        
    }
}

void reportError(Error[] errors)
{

}

Expr parsePrimary(Context ctx)
{

}