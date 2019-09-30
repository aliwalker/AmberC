///     This file contains the parsing utilities.
///     Copyright 2019 Yiyong Li.

module parser.parser;

import std.stdint;
import std.format;
import std.algorithm;
import std.range;
import parser.ast;
import parser.lexer;
import parser.types;
import sema.expr;
import sema.env;
import reporter;
debug import std.stdio;

private T parseErrorImp(T)(ref TokenStream tokstr, string msg, SrcLoc loc)
{
    // TODO: error recovery on TokenStream.
    report(
        SVR_ERR,
        msg,
        loc
    );

    return null;
}

/// Report parsing error and perform error recovery.
private alias parseError = parseErrorImp!(Expr);

/// Report parsing type error and perform error recovery.
private alias parseTypeError = parseErrorImp!(Type);

/// Consume a token and expect it to be a separator.
private bool expectSep(ref TokenStream tokstr, string sep)
{
    if (!tokstr.matchSep(sep))
    {
        parseError(
            tokstr,
            format!"expected '%s'"(sep),
            SrcLoc(tokstr.peek.pos, tokstr.filename)
        );
        return false;
    }
    return true;
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
    auto uop = ["++", "--", "&", "*", "+", "-", "~", "!"];
    auto isUop = any!((val) => compTokStr(tok, Token.SEP, val))(uop);

    return (isUop || (tok.kind == Token.KW && tok.stringVal == "sizeof"));
}

private bool isQualifier(Token tok)
{
    return any!((val) => compTokStr(tok, Token.KW, val))(tqualkw);
}

private bool isSpecifier(Token tok)
{
    return any!((val) => compTokStr(tok, Token.KW, val))(tkw);
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
    return parseCast(tokstr);
}

/// cast
///     : unary
///     | "(" type-name ")" cast
Expr parseCast(ref TokenStream tokstr)
{
    Expr castexpr;
    SrcLoc lparenLoc = SrcLoc(tokstr.peek().pos, tokstr.filename);

    if (tokstr.matchSep("("))
    {
        auto type = parseTypeName(tokstr);
        if (!type)
        {
            // Abort on errors.
            return null;
        }

        if (!tokstr.expectSep(")"))
        {
            return null;
        }

        auto opnd = parseCast(tokstr);
        if (!opnd)
        {
            // Abort on errors.
            return null;
        }

        return semaCast(type, opnd, lparenLoc);
    }
    else
    {
        castexpr = parseUnary(tokstr);
    }

    return castexpr;
}

/// unary
///     : postfix
///     | ("++" | "--") unary
///     | uop unary
///     | "sizeof" unary
///     | "sizeof" '(' type-name ')'
///       ^
Expr parseUnary(ref TokenStream tokstr)
{
    if (tokstr.peek().isPrefixUOp())
    {
        auto tokop = tokstr.read();

        // sizeof(int)
        if (tokop.stringVal == "sizeof" && 
            tokstr.peekSep("(") &&
            (tokstr.peek(1).isQualifier() || tokstr.peek(1).isSpecifier())
        )
        {
            auto lparenLoc = SrcLoc(tokstr.read().pos, tokstr.filename);
            auto type = parseTypeName(tokstr);
            if (!type)
            {
                return null;
            }

            if (!tokstr.expectSep(")"))
            {
                return null;
            }

            return new IntExpr(
                ulongType,
                type.typeSize(),
                lparenLoc
            );
        }

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
                    ulongType, 
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
            case ".", "->":     expr = parseMemberExpr(tokstr, expr); break;
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

/// member-expr:
///     (('.' | '->') identifier)+
///     ^
Expr parseMemberExpr(ref TokenStream tokstr, Expr struc)
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
                debug return parseError(
                    tokstr,
                    "parsePrimary: expect '('",
                    SrcLoc(tok.pos, tokstr.filename)
                );
                return parseError(
                    tokstr,
                    "expect '('",
                    SrcLoc(tok.pos, tokstr.filename)
                );
            }
            return parseParen(tokstr);

        default:
            return parseError(
                tokstr,
                format!"unexpected token '%s'"(tok),
                SrcLoc(tok.pos, tokstr.filename)
            );
    }
}

/// paren:
///     "(" expr ")"                             - grouping.
///        ^
///     | "(" typename ")" "{" initalizer-list "}"  - compound literal.
///           ^
Expr parseParen(ref TokenStream tokstr)
{
    assert(
        (tokstr.peek.kind == Token.SEP) &&
        (tokstr.peek.stringVal == "(")
    );

    // Compound literal.
    if (tokstr.peek().isSpecifier() || tokstr.peek().isQualifier())
    {
        auto type = parseTypeName(tokstr);
        if (!type)
        {
            // Abort on errors.
            return null;
        }
        
        
    }
    
    return parseExpr(tokstr);
}

/// Type-name:
///     specifier-qualifier-list ptr? abs-decltr*
///     ^
Type parseTypeName(ref TokenStream tokstr)
{
    assert(tokstr.peek().isSpecifier() || tokstr.peek().isQualifier());
    Type objtype = parseSpecQualList(tokstr);

    // Abort when errors.
    if (!objtype)
    {
        return null;
    }

    Type type = parsePtr(tokstr, objtype);
    if (tokstr.peekSep("(") || tokstr.peekSep("["))
    {
        return parsePtrToArrayOrFuncType(tokstr, type);
    }
    else
    {
        return type;
    }
}

/// NOTE: we're currently parsing limited abstract declarators.
/// We can only parse something like:
///
///     int (*const *[])(unsigned)      - ✓
///
/// Multiple-parentheses is not supported:
///
///     int (*([4]))                    - ✗
///
/// Abstract-declarator:
///     "(" (ptr | ("[" IntExpr? "]"))+ ")"?
///         ^
Type parseAbsDecltr(ref TokenStream tokstr, Type type)
{
    assert(type);
    assert(
        tokstr.peekSep("*") || 
        tokstr.peekSep("[") || 
        tokstr.peek().kind == Token.EOF);

    while (!tokstr.peekSep(")") && !tokstr.peek().kind != Token.EOF)
    {
        auto sloc = SrcLoc(tokstr.peek().pos, tokstr.filename);

        // Ptr
        if (tokstr.peekSep("*"))
        {
            type = parsePtr(tokstr, type);
        }
        // Array
        else if (tokstr.peekSep("["))
        {
            auto lbrackLoc = SrcLoc(tokstr.peek().pos, tokstr.filename);
            
            tokstr.read();  // '['
            if (cast(FuncType)type)
            {
                return parseTypeError(
                    tokstr,
                    "array of function is not allowed",
                    lbrackLoc
                );
            }

            if (tokstr.matchSep("]"))
            {
                // TODO:
            }
            else
            {
                // FIXME:
                // The size must be determined at compile time.
                auto arraySize = cast(IntExpr)parseAssignment(tokstr);
                if (!arraySize)
                {
                    return parseTypeError(
                        tokstr,
                        "variable-length array is not supported",
                        lbrackLoc
                    );
                }

                type = getArrayType(type, arraySize.value);
                if (!tokstr.matchSep("]"))
                {
                    return parseTypeError(
                        tokstr,
                        "expect ']'",
                        SrcLoc(tokstr.peek().pos, tokstr.filename)
                    );
                }

                // Must end after "[expr]".
                if (!tokstr.matchSep(")"))
                {
                    return parseTypeError(
                        tokstr,
                        "expect ')'",
                        SrcLoc(tokstr.peek().pos, tokstr.filename)
                    );
                }
                return type;
            }
        }
        // Not supported.
        else
        {
            return parseTypeError(
                tokstr,
                "unsupported type",
                sloc
            );
        }
    }
    return type;
}

/// This is a bit tricky. We'll have to parse the inner-most
/// type first, then wrap it up.
///
/// For example, to parse type:
///     "int (*const(*))[5]"
///      ^^^            ^^^
/// We'll first have to parse the inner-most type, which is, in
/// this case, "pointer that points to an int array of length 5",
/// indicated by "^" in the above.
/// Then we'll have to wrap this into(done by [parseAbsDecltr]):
///  "pointer to a const-qualified pointer that points to an int array of length 5".
Type parsePtrToArrayOrFuncType(ref TokenStream tokstr, Type type)
{
    auto tok = tokstr.read();
    assert(
        (tok.kind == Token.SEP) &&
        ((tok.stringVal == "(") || (tok.stringVal == "["))
    );

    /// Array Type
    if (tok.stringVal == "[")
    {
        if (tokstr.matchSep("]"))
        {
            // TODO.
        }

        auto asz = cast(IntExpr)parseAssignment(tokstr);
        if (!asz)
        {
            return parseTypeError(
                tokstr,
                "array size must be determinable in compile time",
                SrcLoc(tok.pos, tokstr.filename)
            );
        }
        return getArrayType(type, asz.value);
    }

    // Stack that records how many '(' are waited to be
    // balanced.
    auto stack = ["("];

    // Tokens that are skipped.
    Token[] skippedToks = [];

    // Either pointer or array.
    if (!tokstr.peekSep("*") && !tokstr.peekSep("["))
    {
        return parseTypeError(
            tokstr,
            "expect '*'",
            SrcLoc(tokstr.peek().pos, tokstr.filename)
        );
    }

    // Skip until we see the final '(' or '['.
    while (stack.length != 0)
    {
        if (tokstr.peek().kind == Token.EOF)
        {
            return parseTypeError(
                tokstr,
                "non-terminated type name",
                SrcLoc(tokstr.peek().pos, tokstr.filename)
            );
        }

        // Pop one.
        if (tokstr.peekSep(")"))
        {
            if (stack.empty)
            {
                return parseTypeError(
                    tokstr,
                    "non-balanced ')'",
                    SrcLoc(tokstr.peek().pos, tokstr.filename)
                );
            }

            stack.popBack();
        }
        else if (tokstr.peekSep("("))
        {
            stack ~= "(";
        }

        // Add it to the skipped list.
        skippedToks ~= tokstr.read();
    }
    // Add an EOF token.
    skippedToks ~= Token(tokstr.peek().pos);

    // These tokens are to parsed by [parseAbsDecltr].
    auto skippedTokstr = TokenStream(skippedToks, tokstr.filename);
    
    // Function.
    if (tokstr.matchSep("("))
    {
        Type[] params;

        // Parse param-type-list.
        while (!tokstr.matchSep(")"))
        {
            // TODO.
        }

        // Wrap the function ptr up.
        return parseAbsDecltr(
            skippedTokstr,
            // NOTE: the pointer is not parsed yet.
            getFuncType(type, params)
        );
    }

    // Array.
    else if (tokstr.matchSep("["))
    {
        auto exprLoc = SrcLoc(tokstr.peek().pos, tokstr.filename);
        auto intExpr = cast(IntExpr)parseAssignment(tokstr);
        if (!intExpr)
        {
            return parseTypeError(
                tokstr,
                "cannot determine the length of array",
                exprLoc
            );
        }

        exprLoc = SrcLoc(tokstr.peek().pos, tokstr.filename);
        expectSep(tokstr, "]");

        // Wrap the ptr to array up.
        return parseAbsDecltr(
            skippedTokstr,
            // NOTE: the pointer is not parsed yet.
            getArrayType(type, intExpr.value)
        );
    }

    // Plain object or ptr type.
    else
    {
        return parseAbsDecltr(
            skippedTokstr,
            // NOTE: the pointer is not parsed yet.
            type
        );
    }
}

/// Ptr :
///     * type-qualifier-list Ptr*
///
/// [type] is the current type represented by spec-qual list.
Type parsePtr(ref TokenStream tokstr, Type type)
{
    assert(type);

    while (tokstr.matchSep("*"))
    {
        uint8_t quals = parseTypeQuals(tokstr);
        type = getPtrType(type, quals);
    }

    return type;
}

/// Specifier-qualifier-list:
///     type-specifier specifier-qualifier-list
///     | type-qualifier specifier-qualifier-list
Type parseSpecQualList(ref TokenStream tokstr)
{
    // Consume any prefix qulifiers.
    uint8_t quals = parseTypeQuals(tokstr);

    // Type specifiers.
    if (!tokstr.peek().isSpecifier())
    {
        report(
            SVR_ERR,
            "expect type specifier",
            SrcLoc(tokstr.peek().pos, tokstr.filename)
        );
        return null;
    }
    auto type = parseTypeSpecs(tokstr);

    // Consume any postfix qualifiers.
    quals |= parseTypeQuals(tokstr);

    return (quals == 0) ? type : getQualType(type, quals);
}

/// Type-qualifiers:
///     "const" type-qualifiers*
///     | "register" type-qualifiers*
uint8_t parseTypeQuals(ref TokenStream tokstr)
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

/// Type-specifiers:
///     basic-type-specifiers
///     | aggregType-specifiers
///     | enum-specifiers   - TODO.
///     | typedef-name      - TODO.
Type parseTypeSpecs(ref TokenStream tokstr)
{
    auto tokspec = tokstr.read();
    assert(isSpecifier(tokspec));

    switch (tokspec.stringVal)
    {
        case "_Bool":       return boolType;
        case "char":        return charType;
        case "short":
            tokstr.matchKW("int");
            return shortType;

        case "int":         return intType;
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
        case "struct":      return parseRecTypeSpec(tokstr, false);
        case "union":       return parseRecTypeSpec(tokstr, true);
        
        default:
            assert(false);
    }
}

/// intTypeSpec:
///     "signed" qualIntType
///              ^
///     "unsigned" qualIntType
///                ^
/// qualIntType:
///     qual qualIntType*
///     | ("char" | "short" | "int" | "long" | "long long") qualIntType*
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

    // Consume any qualifiers.
    uint8_t quals = 0;
    if (tokstr.peek().isQualifier())
    {
        quals |= parseTypeQuals(tokstr);
    }

    auto tok = tokstr.read();

    // "signed" or "unsigned".
    if (tok.kind != Token.KW)
    {
        tokstr.unread();
        return (quals == 0) ? intType : getQualType(intType, quals);
    }

    Type resType;
    switch (tok.stringVal)
    {
        case "char":
            resType = charTy;
            goto POSTFIX_QUALS;

        case "short":
            if (tokstr.peek().isQualifier())
            {
                quals |= parseTypeQuals(tokstr);
            }

            // "signed short int". "int" is optional.
            tokstr.matchKW("int");
            resType = shortTy;
            goto POSTFIX_QUALS;

        case "int":
            resType = intTy;
            goto POSTFIX_QUALS;
        
        case "long":
            if (tokstr.peek().isQualifier())
            {
                quals |= parseTypeQuals(tokstr);
            }

            // "signed long long (int)"
            if (tokstr.matchKW("long"))
            {
                if (tokstr.peek().isQualifier())
                {
                    quals |= parseTypeQuals(tokstr);
                }
                tokstr.matchKW("int");
                resType = llongTy;
            }
            // "signed long (int)"
            else
            {
                tokstr.matchKW("int");
                resType = longTy;
            }
            goto POSTFIX_QUALS;

        default:
            report(
                SVR_ERR,
                format!"unexpected keyword '%s'"(tok),
                SrcLoc(tok.pos, tokstr.filename)
            );

            return null;
    }
    
POSTFIX_QUALS:
    return (quals == 0) ? resType : getQualType(resType, quals);
}

/// RecTypeSpec:
///     ("struct" | "union") (name)? ('{' struct-decl-list '}')?
///                          ^
/// Struct-decl-list:
///     (spec-qual-list declarator ';')*
Type parseRecTypeSpec(ref TokenStream tokstr, bool isUnion)
{
    auto isDef = false;
    auto tok = tokstr.read();

    Token tokident = Token(Token.IDENT, "", tok.pos);
    RecType recType;
    RecField[] decls;

    // identifier.
    if (tok.kind == Token.IDENT)
    {
        tokident = tok;
        tok = tokstr.read();
    }

    // struct-decl-list.
    if (tok.kind == Token.SEP && tok.stringVal == "{")
    {
        string tystr;
        if (getRecType(tokident.stringVal, tystr, isUnion).members)
        {
            report(
                SVR_ERR,
                format!"redeclaration of '%s'"(tokident.stringVal),
                SrcLoc(tokident.pos, tokstr.filename),
            );
            // TODO: synchronization.
            return null;
        }

        isDef = true;
        decls = parseStructDeclList(tokstr);
    }
    else
    {
        tokstr.unread();
    }

    string tystr;
    recType = getRecType(tokident.stringVal, decls, tystr, isUnion);

    if (isDef && isLocalEnv())
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

/// Test parseTypeSpec
unittest
{
    uniProlog();

    void testTryParseObjTypeSpec(string code, Type etype)
    {
        auto tokstr = TokenStream(code, "testTryParseObjTypeSpecs.c");
        auto type = parseTypeSpecs(tokstr);

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

    T testValid(T)(string code, string msg = "assert failed")
    {
        auto tokstr = TokenStream(code, "testParseUnary.c");
        auto expr = parseUnary(tokstr);
        assert(expr, msg);
        assert(cast(T)expr, msg);

        return cast(T)expr;
    }

    void testInvalid(string code, string msg = "assert falied")
    {
        auto tokstr = TokenStream(code, "testParseUnary.c");
        auto expr = parseUnary(tokstr);
        assert(!expr, msg);
    }
    /// Fall back to parsePostfix().
    testValid!UnaryExpr("a++");

    /// Fall back to parsePostfix(). b is not declared.
    testInvalid("b++");

    /// Test invalid opnd for prefix ++.
    testInvalid("++a++", "a++ is an rvalue");

    /// Test valid usage of &.
    testValid!UnaryExpr("&a");

    /// Test & on rvalue.
    testInvalid("&\"string\"");

    /// Test sizeof expr.
    auto intexpr = testValid!IntExpr("sizeof 4", "sizeof operator must be evaluated at parsing time.");
    assert(intexpr.value == intType.typeSize());

    /// Test sizeof expr.
    intexpr = testValid!IntExpr("sizeof(int*)");
    assert(intexpr.value == PTR_SIZE);

    /// Test * on non-ptr and non-array expr.
    testInvalid("*a", "cannot deref a non-ptr type");

    envPop();
    uniEpilog();
}

/// Test parseSpecQualList
unittest
{
    uniProlog();
    void testValid(string code, string tystr)
    {
        auto tokstr = TokenStream(code, "testParseSpecQualList.c");
        auto type = parseSpecQualList(tokstr);
        assert(type);
        assert(tokstr.peek().kind == Token.EOF);
        assert(type.toString == tystr);
    }

    // Normal.
    testValid("const int", "const int");

    // Qualifiers can be at front or tail.
    testValid("int const", "const int");

    // Redundant qualifiers are ignored.
    testValid("const int const", "const int");

    // Intermix qualifiers.
    testValid("signed const int", "const int");

    // Postfix qualifers.
    testValid("signed int const", "const int");

    uniEpilog();
}

/// Test parseTypename
unittest
{
    uniProlog();

    void testParseType(T)(string code, string repr)
        if (is(T : Type))
    {
        auto tokstr = TokenStream(code, "testParseTypename.c");
        auto type = cast(T)parseTypeName(tokstr);
        assert(type);
        assert(type.toString == repr, format!"expected type string '%s', but got '%s'"(repr, type));
    }

    alias testBasic = testParseType!(Type);
    alias testPtr = testParseType!(PtrType);
    alias testArray = testParseType!(ArrayType);
    alias testStruc = testParseType!(RecType);

    testBasic("int", "int");

    // Basic type ptr.
    testPtr("int*", "int*");

    testPtr("int *const", "int*const "/* The tailing whitespace exists. */);

    // Simple func ptr.
    testPtr("int(*)()", "int(*)()");

    // Test array.
    testArray("int[5]", "int[5]");

    // Test struct.
    testStruc("struct Foo", "struct Foo");

    // Test struct ptr.
    testPtr("struct BBB*", "struct BBB*");

    // Test funcPtr array.
    testArray("int(*const [2])()", "int(*const [2])()");
    uniEpilog();
}

/// Test parseCast
unittest
{
    void testValid(string code)
    {
        auto tokstr = TokenStream(code, "testParseCast.c");
        auto expr = cast(UnaryExpr)parseCast(tokstr);
        assert(expr);
        assert(expr.kind == UnaryExpr.CAST);
    }

    void testInvalid(string code)
    {
        auto tokstr = TokenStream(code, "testParseCast.c");
        auto expr = cast(UnaryExpr)parseCast(tokstr);
        assert(!expr);
    }

    // auto fooStruc = getRecType("foo", [
    //     RecField(intType, "bar")
    // ]);
    // envPush();
    // envAddDecl("fooStruc", fooStruc);

    testValid("(long)4");
    testInvalid("(struct Foo)4");
    testValid("(struct Foo*)4");

    // envPop();
}