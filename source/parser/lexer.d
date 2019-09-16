///     This file is modified from Higgs JS VM's lexer module written by
///     Maxime Chevalier-Boisvert. See:
///     https://github.com/maximecb/Higgs

module parser.lexer;

import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.range;
import std.regex;
import std.string;

/// A list of C keywords.
/// TODO: Some of those keywords are currently unsupported.
immutable string[] keywords = [
    // Type specifiers.
    // 6.7.2
    "_Bool",
    "char",
    "double",
    "float",
    "int",
    "long",
    "short",
    "signed",
    "unsigned",
    "void",
    "struct",
    "union",

    // Control flow.
    "break",
    "continue",
    "do",
    "else",
    "for",
    "if",
    "switch",
    "while",

    "sizeof",

    // Type qualifiers
    // 6.7.3
    "const",
    "restrict",
    "volatile",
    "inline",

    // Storage-class specifiers.
    // 6.7.1
    "auto",
    "extern",
    "register",
    "static",
    "typedef",
];

/// A list of seperators string repr.
immutable string[] seperators = [
    "\\",
    "\"",
    "'",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    ",",
    ":",
    ";",
    "?",
];

/// A list of operator string repr.
/// Be sure to add operators according to their lengths,
/// because the lexer depends on this order.
immutable string[] operators = [
    // Single char
    "+",
    "-",
    "*",
    "/",
    "%",
    "!",
    "<",
    ">",
    "~",
    ".",
    "^",
    "&",
    "|",
    "=",

    // Multiple chars.
    "++",
    "--",
    "==",
    "!=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "&&",
    "||",
    "<=",
    ">=",
    "<<",
    ">>",
    "->",
];

/// Line & Column.
struct SrcPos {
    int line;

    int col;
}


/// Token struct.
struct Token
{
    alias Kind = int;
    enum : Kind
    {
        /// both seperators and operators.
        SEP,
        /// Identifier.
        IDENT,
        /// Keyword.
        KW,
        /// All integers.
        INT,
        /// Both float and double precisions.
        FLOAT,
        /// Char literal.
        CHAR,
        /// String literal.
        STRING,
        /// End-of-file indicator.
        EOF,
    }

    /// Type of this token.
    Kind kind;
    union
    {
        /// Kind::INT
        long intVal;

        /// Kind::FLOAT
        double floatVal;

        /// Kind::SEP, Kind::STRING, Kind::IDENT, Kind::KW
        string stringVal;

        /// Kind::CHAR
        char charVal;
    }

    /// Position of this token.
    SrcPos pos;

    /// Integer.
    this(Kind kind, long value, SrcPos pos) {
        assert(kind == INT, "Expect integer value");
        this.kind = kind;
        this.intVal = value;
        this.pos = pos;
    }

    /// FP.
    this(Kind kind, double value, SrcPos pos) {
        assert(kind == FLOAT, "Expect floating point value");
        this.kind = kind;
        this.floatVal = value;
        this.pos = pos;
    }

    /// Char.
    this(Kind kind, char ch, SrcPos pos)
    {
        assert(kind == CHAR, "Expect CHAR");
        this.kind = kind;
        this.charVal = ch;
        this.pos = pos;
    }

    /// The rest.
    this(Kind kind, string value, SrcPos pos) {
        assert(
            kind == KW      ||
            kind == IDENT   ||
            kind == STRING  ||
            kind == SEP
        );

        this.kind = kind;
        this.stringVal = value;
        this.pos = pos;
    }

    /// EOF.
    this(SrcPos pos) {
        this.kind = EOF;
        this.pos = pos;
    }

    string toString() const {
        switch (kind) {
        case KW:        return format("keyword: %s", stringVal);
        case IDENT:     return format("identifier: %s", stringVal);
        case CHAR:      return format("charactor: %s", charVal);
        case STRING:    return format("string: %s", stringVal);
        case SEP:       return format("separator: %s", stringVal);
        case INT:       return format("integer: %s", intVal);
        case FLOAT:     return format("floating point: %s", stringVal);
        default:        return "token";
        }
    }
}

/// Tests for struct Token
unittest {
    SrcPos dummy;
    
    const eof = Token(dummy);
    assert(eof.kind == Token.EOF);

    Token a = Token(
        Token.STRING,
        "String value",
        dummy
    );
    assert(a.toString() == "string: String value");

    Token b = Token(
        Token.SEP,
        ")",
        dummy
    );
    assert(b.toString() == "separator: )");
}

/// Stream for reading chars from source code. 
struct CharStream
{
    /// Input code.
    string code;

    /// Source file name that contains this [code].
    string filename;

    /// Current index.
    int index = 0;

    /// Current line.
    int line = 1;

    /// Current column.
    int col = 1;

    /// Constructor.
    this(string code, string filename) {
        this.code = code;
        this.filename = filename;
    }

    /// Reads a single character from source.
    char read() {
        const char ch = (index >= code.length) ? '\0' : code[index++];

        if (ch == '\n') {
            line++;
            col = 1;
        } else if (ch != '\r') {
            col++;
        }

        return ch;
    }

    /// Peeks the current char without consuming it.
    char peek(size_t offset = 0) const {
        const char ch = (index + offset >= code.length) ? '\0' : code[index + offset];
        return ch;
    }

    /// Tries to match the given [str]. 
    /// Returns true if matched, and the [str] is consumed;
    /// otherwise returns false without consuming any chars.
    bool match(string str) {
        // Too many chars
        if (index + str.length > code.length)
            return false;

        // Test match.
        if (str != code[index .. index + str.length])
            return false;

        // Consume the matched chars.
        for (size_t i = 0; i < str.length; i++)
            read();

        return true;
    }

    /// Ditto. Matching against regular expression.
    /// Returns a Capture that contains the match; or an empty Capture.
    auto match(StaticRegex!char re)
    {
        // tries to match from current head.
        auto m = matchFirst(code[index .. code.length], re);

        if (m.captures.empty == false)
            // consumes the bytes 
            for (int i = 0; i < m.captures[0].length; ++i)
                read();

        return m;
    }

    /// Returns the position of the next char to be read.
    SrcPos pos() const
    {
        return SrcPos(line, col);
    }
}

/// CharStream & Error.
unittest {
    string filename = "dummy";
    string code = "int someName = 1923;";
    CharStream chars = CharStream(code, filename);

    assert(chars.peek() == 'i');
    assert(chars.read() == 'i');

    assert(chars.match("nt ") == true);

    enum nameRe = ctRegex!(`^[a-z|A-Z|_][a-z|A-Z|_|0-9]*\b`);
    auto m = chars.match(nameRe);
    assert(!m.empty);
    assert(m.captures[0] == "someName");

    chars.read();
    assert(chars.match("="));
    assert(chars.match(" 1923;"));
    assert(chars.read() == '\0');

    auto error = new LexingError("bad lexing", "dummy.c", SrcPos(10, 10));
    assert(error.toString() == "dummy.c:10:10: error: bad lexing");
}

/// Bookkeeping information for lexing errors.
class LexingError : Error
{
    /// Reason that lexing fails.
    string msg;

    /// The name of the source file.
    string filename;

    /// Position in current code.
    SrcPos pos;

    /// Constructor
    this(string msg, string filename, SrcPos pos)
    {
        this.msg = msg;
        this.filename = filename;
        this.pos = pos;

        super(msg);
    }

    override string toString() const {
        return format(
            "%s:%s:%s: error: %s",
            filename, 
            pos.line, 
            pos.col, 
            msg
        );
    }
}

/// Wrapper around result tokens or error.
struct LexingResult
{
    /// Result tokens.
    Token[] tokens;

    /// Errors that occur in lexing.
    LexingError[] errors;
}

/// Returns true if [ch] is a digit.
private bool digit(char ch)
{
    return ch >= '0' && ch <= '9';
}

/// Returns true if [ch] is an alpha char.
private bool alpha(char ch)
{
    return ((ch >= 'a' && ch <= 'z') ||
            (ch >= 'A' && ch <= 'Z') ||
            (ch == '_'));
}

/// Returns true if [ch] is a whitespace.
private bool whitespace(char ch)
{
    return (ch == '\r' || ch == ' ' || ch == '\n' || ch == '\t');
}

/// Returns true if [ch] is alpha or digit
private bool alphaNumberic(char ch)
{
    return alpha(ch) || digit(ch);
}

/// Takes a CharStream and lexes all tokens for this stream.
LexingResult lexStream(ref CharStream chars)
{
    // Token appender.
    auto tokens = appender!(Token[]);

    // Lexing errors.
    LexingError[] errors;

    // The most recent error.
    LexingError error;

    // Helper that records error.
    auto handleError(string msg, SrcPos pos)
    {
        error = new LexingError(msg, chars.filename, pos);
        errors ~= error;
        return;
    }

    // Matches seperators or operators. 
    // Returns true for matched.
    bool matchSep(immutable(string[]) seps, SrcPos pos)
    {
        // For seperators, the order doesn't matter.
        // For operators, the order matters.
        foreach_reverse (sep; seps)
        {
            if (chars.match(sep))
            {
                tokens.put(Token(
                    Token.SEP,
                    sep,
                    pos,
                ));
                return true;
            }
        }

        return false;
    }

    // Exhaust the CharStream.
    for (;;)
    {
        // Skip whitespace and comments.
        for (;;)
        {
            if (error !is null)
            {
                // TODO: 
                // Instead of exiting lexing on the first error,
                // we should include some error recovery.
                return LexingResult(tokens.data, errors);
            }
            auto ch = chars.peek();

            if (whitespace(ch))
            {
                chars.read();
            }

            // Single-line comment.
            else if (chars.match("//"))
            {
                for (;;)
                {
                    ch = chars.read();
                    if (ch == '\0' || ch == '\n')
                        break;
                }
            }

            // Multi-line comment
            else if (chars.match("/*"))
            {
                for(;;)
                {
                    auto pos = chars.pos();
                    ch = chars.read();
                    if (ch == '\0')
                    {
                        handleError("unterminated multi-line comment", pos);
                        break;
                    }

                    else if (chars.match("*/"))
                        break;
                }
            }

            // Done.
            else
            { 
                break;
            }
        }

        auto ch = chars.peek();
        auto pos = chars.pos();

        // Finished lexing.
        if (ch == '\0')
        {
            // EOF.
            tokens.put(Token(pos));
            break;
        }

        // Hex number.
        if (chars.match("0x"))
        {
            enum hexRegex = ctRegex!(`^[0-9|a-f|A-F]+`);
            auto m = chars.match(hexRegex);

            if (m.empty)
            {
                handleError("invalid hex number", pos);
                break;
            }

            long val;
            formattedRead(m.captures[0], "%x", &val);
            tokens.put(Token(
                Token.INT,
                val,
                pos
            ));
        }

        // Octal number or 0.
        else if (ch == '0')
        {
            enum octRegex = ctRegex!(`^0([0-7]+)`);
            auto m = chars.match(octRegex);

            long val = 0;
            // An actual octal number.
            if (!m.empty)
            {
                auto octStr = m.captures[0];
                formattedRead(octStr, "%o", &val);
            }
            // Zero
            else
            {
                // Consume the char.
                chars.read();
            }

            tokens.put(Token(
                Token.INT,
                val,
                pos
            ));
        }

        // Decimal int or floating point number.
        else if (digit(ch))
        {
            enum decRegex = ctRegex!(`^[1-9][0-9]*(\.[0-9]+)?`);
            auto m = chars.match(decRegex);

            assert(!m.empty);
            assert(m.captures.length == 2);

            const isFloat = (m.captures[1] == "") ? false : true;
            if (isFloat)
            {
                const val = to!double(m.captures[0]);
                tokens.put(Token(
                    Token.FLOAT,
                    val,
                    pos
                ));
            }

            else
            {
                const val = to!long(m.captures[0]);
                tokens.put(Token(
                    Token.INT,
                    val,
                    pos
                ));
            }
        }

        // Floating point number.
        else if (ch == '.' && chars.peek(1).digit)
        {
            enum fpRegex = ctRegex!(`^\.[0-9]+`);
            auto m = chars.match(fpRegex);

            assert(!m.empty);
            const val = to!double(m.captures[0]);
            tokens.put(Token(
                Token.FLOAT,
                val,
                pos
            ));
        }

        // Character literal.
        else if (ch == '\'')
        {
            // TODO: read escape char.
            chars.read();
            auto val = chars.read();
            ch = chars.read();

            if (ch == '\'')
            {
                tokens.put(Token(
                    Token.CHAR,
                    val,
                    pos
                ));
            }
            // Error
            else
            {
                handleError("invalid character literal", pos);
                break;
            }
        }

        // String literal.
        else if (ch == '"')
        {
            string str = "";

            chars.read();
            for (;;)
            {
                ch = chars.read();
                if (ch == '"')
                    break;

                if (ch == '\0')
                {
                    handleError("unterminated string literal", pos);
                    break;
                }

                if (ch == '\n')
                {
                    handleError("newline in string literal", pos);
                    break;
                }
                str ~= ch;
            }

            if (error is null)
            {
                tokens.put(Token(
                    Token.STRING,
                    str,
                    pos
                ));
            }
        }

        // Identifier or keyword.
        else if (alpha(ch))
        {
            string name = "";
            name ~= chars.read();

            while (chars.peek().alphaNumberic)
            {
                name ~= chars.read();
            }

            bool matched = false;
            // Try matching keywords.
            foreach (kw; keywords)
            {
                if (kw == name)
                {
                    matched = true;
                    break;
                }
            }

            auto kind = matched ? Token.KW : Token.IDENT;
            tokens.put(Token(
                kind,
                name,
                pos
            ));
        }

        // Seperators or operators.
        else
        {
            if (matchSep(seperators, pos))
                continue;

            else if (matchSep(operators, pos))
                continue;

            handleError("unknown character '" ~ to!string([ch]) ~ "'", pos);
            break;
        }
    }

    return LexingResult(tokens.data, errors);
}

/// A stream of tokens.
struct TokenStream
{
    /// Errors.
    LexingError[] errors;

    /// Results.
    Token[] tokens;

    /// Current index.
    size_t idx;

    /// Constructor.
    this(string code, string filename)
    {
        auto chars = CharStream(code, filename);
        auto res = lexStream(chars);
        
        errors = res.errors;
        tokens = res.tokens;
    }

    /// Peek without consuming a token.
    Token peek(size_t dis = 0)
    {
        if (idx + dis >= tokens.length)
        {
            return tokens[$ - 1];
        }

        return tokens[idx + dis];
    }

    /// Consume a token from stream.
    Token read()
    {
        if (idx >= tokens.length)
        {
            return tokens[$ - 1];
        }

        return tokens[idx++];
    }

    private bool matchStringval(Token.Kind kind, string val)
    {
        if (
            idx >= tokens.length     ||
            tokens[idx].kind != kind ||
            tokens[idx].stringVal != val
        )
        {
            return false;
        }
        
        idx++;
        return true;
    }

    /// Match and consume current tokens against the
    /// given [sep]. Return true for success.
    bool matchSep(string sep)
    {
        return matchStringval(Token.SEP, sep);
    }

    /// Match and consume current tokens against the
    /// given [kw]. Return true for success.
    bool matchKW(string kw)
    {
        return matchStringval(Token.KW, kw);
    }
}

/// Test lexStream.
unittest {

    void compareTokens(Token[] expectedToks, Token[] tokens)
    {
        assert(expectedToks.length == tokens.length);
        foreach (i, ref tok; tokens)
        {
            auto etok = &expectedToks[i];
            assert(etok.kind == tok.kind, format("expected %sth token to be of the same type", i));

            switch (tok.kind)
            {
            case Token.INT: 
                assert(
                    etok.intVal == tok.intVal,
                    format("expected %s to equal %s", tok.intVal, etok.intVal)
                );
                break;
            
            case Token.SEP, Token.STRING, Token.KW:
                assert(
                    etok.stringVal == tok.stringVal,
                    format("expected %s to equal %s", tok.stringVal, etok.stringVal)
                );
                break;

            case Token.CHAR:
                assert(
                    etok.charVal == tok.charVal,
                    format("expected %s to equal %s", tok.charVal, etok.charVal)
                );
                break;

            default:
            }
        }
    }

    void testValidCode(string code, Token[] expectedToks)
    {
        auto chars = CharStream(code, "dummy.c");
        auto lexingRes = lexStream(chars);

        assert(lexingRes.errors.empty);
        assert(!lexingRes.tokens.empty);

        compareTokens(expectedToks, lexingRes.tokens);
    }

    void testInvalidCode(string code, string expectedMsg)
    {
        auto chars = CharStream(code, "dummy.c");
        auto lexingRes = lexStream(chars);

        assert(!lexingRes.errors.empty);
        // TODO:
        // Change this when error recovery is added.
        assert(lexingRes.errors[0].msg == expectedMsg);
    }

    // Two statements.
    testValidCode(
        "int dumb = 100;\ndumb += 10;",
        [
            Token(Token.KW, "int", SrcPos()),
            Token(Token.IDENT, "dumb", SrcPos()),
            Token(Token.SEP, "=", SrcPos()),
            Token(Token.INT, 100L, SrcPos()),
            Token(Token.SEP, ";", SrcPos()),
            Token(Token.IDENT, "dumb", SrcPos()),
            Token(Token.SEP, "+=", SrcPos()),
            Token(Token.INT, 10L, SrcPos()),
            Token(Token.SEP, ";", SrcPos()),
            Token(SrcPos()),
        ]
    );

    // Floating point.
    testValidCode(
        "double a = 10.0;",
        [
            Token(Token.KW, "double", SrcPos()),
            Token(Token.IDENT, "a", SrcPos()),
            Token(Token.SEP, "=", SrcPos()),
            Token(Token.FLOAT, 10.0, SrcPos()),
            Token(Token.SEP, ";", SrcPos()),
            Token(SrcPos()),
        ]
    );

    // Hex.
    testValidCode(
        "0x10",
        [
            Token(Token.INT, 16L, SrcPos()),
            Token(SrcPos())
        ]
    );

    // Octal.
    testValidCode(
        "011",
        [
            Token(Token.INT, 9L, SrcPos()),
            Token(SrcPos())
        ]
    );

    // 0.
    testValidCode(
        "0; ident",
        [
            Token(Token.INT, 0L, SrcPos()),
            Token(Token.SEP, ";", SrcPos()),
            Token(Token.IDENT, "ident", SrcPos()),
            Token(SrcPos()),
        ]
    );

    // Comments & whitespace.
    testValidCode(
        "/* some comments */\n//",
        [
            Token(SrcPos())
        ]
    );

    // String literal
    testValidCode(
        "\"String\"",
        [
            Token(Token.STRING, "String", SrcPos()),
            Token(SrcPos()),
        ]
    );

    // Char literal.
    testValidCode(
        "'c'",
        [
            Token(Token.CHAR, 'c', SrcPos()),
            Token(SrcPos()),
        ]
    );

    // Invalid hex.
    testInvalidCode(
        "0xl",
        "invalid hex number"
    );

}

/// Test TokenStream.
unittest
{
    auto tokstr = new TokenStream("int code = 4;", "dummy.c");

    assert(tokstr.errors is null);
    assert(tokstr.peek().kind == Token.KW);
    assert(tokstr.peek().stringVal == "int");
    assert(tokstr.matchKW("int"));

    assert(tokstr.peek().kind == Token.IDENT);
    assert(tokstr.peek().stringVal == "code");
    tokstr.read();

    assert(tokstr.matchSep("="));
    assert(tokstr.read().intVal == 4);
    assert(tokstr.matchSep(";"));
    assert(tokstr.read().kind == Token.EOF);
}