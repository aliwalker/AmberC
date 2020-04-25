///     This file contains the parsing utilities.
///     Copyright 2019 Yiyong Li.

module parser.parser;

import std.stdint;
import std.format;
import std.algorithm;
import std.range;
import std.ascii;
import core.stdc.stdlib : exit;

import parser.ast;
import parser.lexer;
import parser.types;
import reporter;

class Parser
{
    TokenStream tokstr;

    /// Symbol table for faster lookups.
    alias SymTable = Decl[string];
    SymTable genv;
    SymTable[] lenv;

    SymTable symTable()
    {
        if (lenv.length > 0)    return lenv[$ - 1];
        return genv;
    }

    /// Resolve symbols from tables above.
    Decl symResolve(string name)
    {
        int i = cast(int)lenv.length;
        
        for (i -= 1; i >= 0; i -= 1)
        {
            SymTable sym = lenv[i];
            if (name in sym)
                return sym[name];
        }

        return (name in genv) ? genv[name] : null;
    }

    /// AST nodes.
    Decl[] toplevels;
    Decl[] locals;

    this(string code, string filename)
    {
        this.tokstr = TokenStream(code, filename);
    }

    /*
    Error handling.
    NOTE: I've chosen to avoid error recovery for this toy compiler simply
    because it results to cleaner code.
    */

    private void parseError(string msg, SrcLoc loc = SrcLoc())
    {
        report(SVR_ERR, msg, loc == SrcLoc() ? tokstr.currLoc() : loc);
        exit(-1);
    }

    private void expectSep(string sep)
    {
        if (!tokstr.matchSep(sep))
            parseError(format!"expected '%s' but got '%s"(sep, tokstr.peek()));
    }

    private void ensureLvalue(Expr expr)
    {
        // identifier or member access.
        if (cast(IdentExpr)expr || cast(MemberExpr)expr)
            return;

        // * operator.
        if (auto unary = cast(UnaryExpr)expr)
            if (unary.kind == UnaryExpr.DEREF)
                return;

        parseError("expected an lvalue", expr.loc);
    }

    private void ensureNotConst(Expr expr)
    {
        if (expr && !expr.type.isconst)
            return;

        parseError("unexpected const lvalue", expr.loc);
    }

    /// NOTE: expression with [dest] type should be a non-const lvalue.
    private void ensureAssignable(Type dest, Type src)
    {
        if (dest.isScalar() && src.isScalar())
            return;

        if (src.isSame(dest))
            return;

        parseError(format!"incompatible type from '%s' to '%s'"(src, dest));
    }

    private Type ensureMember(Type type, string name)
    {
        if (type.isStruct())
        {
            if (type.fields.get(name))
                return type.fields.get(name);

            parseError(format!"member '%s' does not exist in type '%s'"(name, type));
        }
        parseError(format!"member reference base type '%s' is not a struct or union"(type));
        assert(0);
    }

    /// 6.3 Type conversion.
    /// Implicit array decay, function designator conversion, and integer promotion.
    private Expr typeConv(Expr node)
    {
        if (!node)
            return null;

        Type type = node.type;

        // Expression that has type ‘‘array of type’’ is converted
        // to an expression with type ‘‘pointer to type’’ that points
        // to the initial element of the array object and is not an lvalue.
        // (Exceptions are the opnds of sizeof, &, and string literal used to initialize an array).
        if (type.isArray())
            return new UnaryExpr(UnaryExpr.DECAY, type.base().makePointer(), node, node.loc);

        // Function designator with type ‘‘function returning type’’ is converted 
        // to an expression that has type ‘‘pointer to function returning type’’.
        // (Exceptions are the opnds of sizeof and &).
        if (type.isFunction())
            return new UnaryExpr(UnaryExpr.ADDR_OF, type.makePointer(), node, node.loc);

        // If an int can represent all values of the original type, the value is converted 
        // to an int; otherwise, it is converted to an unsigned int.
        if (type.isSame(boolType) || type.isSame(charType) || type.isSame(shortType))
            return new UnaryExpr(UnaryExpr.CAST, intType, node, node.loc);

        return node;
    }

    /// 6.3.1.8 Usual arithmetic conversions.
    private Type typeConvArith(Type a, Type b)
    {
        assert(a.isArithmetic() && b.isArithmetic(), "expected arithmetic types");

        if (a.size < b.size)
        {
            Type tmp = a;
            a = b;
            b = tmp;
        }

        // Always convert to floating point number if needed.
        if (a.isFP())
            return a;

        assert(a.isInteger() && b.isInteger(), "expected two integer types");

        // If a and b are the same, no conversion is needed.
        if (a.isSame(b))
            return a;

        // Return the one with larger size.
        if (a.size > b.size)
            return a;

        assert(a.size == b.size, "internal error");

        if ((a.isSigned() && b.isSigned()) || a.isUnsigned())
            return a;
        return a.getUnsigned();
    }

    /// Used in combination with [typeConvArith].
    private Expr typeConvWrap(Expr node, Type type)
    {
        if (node.type.isSame(type))
            return node;

        return new UnaryExpr(UnaryExpr.CAST, type, node, node.loc);
    }

    private void ensurePointerBinop(string op)
    {
        if (op != "-" || op != ">" || op != "<" || 
            op != ">=" || op != "<=" || op != "!=" || op != "==")
            parseError("invalid pointer arithmetics");
    }

    private BinExpr createBinExpr(string op, Expr lhs, Expr rhs)
    {
        SrcLoc loc = lhs.loc;
        // Both are pointers.
        if (lhs.type.isPointer() && rhs.type.isPointer())
        {
            ensurePointerBinop(op);

            if (op == "-")
                return new BinExpr(ulongType, op, lhs, rhs, loc);

            return new BinExpr(boolType, op, lhs, rhs, loc);
        }

        // One is pointer.
        if (lhs.type.isPointer())
            return new BinExpr(lhs.type, op, lhs, rhs, loc);
        if (rhs.type.isPointer())
            return new BinExpr(rhs.type, op, lhs, rhs, loc);

        if (!lhs.type.isArithmetic() || !rhs.type.isArithmetic())
            parseError("invalid binary operator", loc);

        Type type = typeConvArith(lhs.type, rhs.type);
        lhs = typeConvWrap(lhs, type);
        rhs = typeConvWrap(rhs, type);
        return new BinExpr(type, op, lhs, rhs, loc);
    }

    /**
    Helpers for comparing token.stringVal
    */

    private static bool compTokStr(Token tok, int kind, string val)
    {
        return (tok.kind == kind) && (tok.stringVal == val);
    }

    private static bool isQualifier(Token tok)
    {
        return any!((val) => compTokStr(tok, Token.KW, val))(tqualkw);
    }

    private static bool isSpecifier(Token tok)
    {
        return any!((val) => compTokStr(tok, Token.KW, val))(tkw);
    }

    private static bool isStorageClassSpecifier(Token tok)
    {
        return any!((val) => compTokStr(tok, Token.KW, val))(sckw);
    }

    private bool isType(Token tok)
    {
        if (isSpecifier(tok) || isQualifier(tok) || isStorageClassSpecifier(tok))
            return true;

        if (tok.kind == Token.IDENT)
        {
            TypedefDecl tydef = cast(TypedefDecl)symResolve(tok.stringVal);
            if (tydef)
                return true;
        }
        return false;
    }

    /// 6.7.6 Type names
    /// type-name:
    ///     specifier-qualifier-list abstract-declarator.
    private Type typeName()
    {
        return declarator(declSpec(), /* isAbstract */true);
    }

    private struct ParamStruct
    {
        string[] params;
    }

    private Type declarator(Type base, bool isAbstract, 
                            string *name = null,
                            ParamStruct *params = null)
    {
        if (tokstr.matchSep("*"))
        {
            if (qualifiers() & Qualifier.CONST)
                base = base.getConst();

            return declarator(base.makePointer(), isAbstract, name, params);
        }

        if (tokstr.matchSep("("))
        {
            if (isType(tokstr.peek()) || tokstr.peekSep(")"))
                // Start of function parameters list.
                return declFunc(base, params);

            Type tmp = base;
            if (tokstr.peekSep("*"))
                // Create a placeholder type because next call to declarator
                // will return type of "pointer to placeholder".
                tmp = new Type();

            Type type = declarator(tmp, isAbstract, name, params);
            expectSep(")");
            Type result = declarator(base, isAbstract, name, params);
            return (tmp == base) ? result : fillType(result, type);
        }

        if (tokstr.matchSep("["))
            return declArray(base);

        Token tok = tokstr.read();
        if (tok.kind == Token.IDENT && isAbstract)
            parseError(format!"unexpected identifier: %s"(tok.stringVal));

        if (tok.kind == Token.IDENT && name)
        {
            if (*name)
                parseError(format!"unexpected identifier: %s"(tok.stringVal));
            *name = tok.stringVal;
        }
        else
            tokstr.unread();

        return declarator(base, isAbstract, name, params);
    }

    /// FIXME: this is quite tricky because we'll have to change
    /// the member of a type.
    /// Fill a placeholder type in [result] with [type].
    private Type fillType(Type result, Type type)
    {
        if (result.isArray() || result.isPointer())
        {
            if (result.base().kind != Type.PLACE_HOLDER)
                return fillType(result.base(), type);

            (cast(PtrType)result).base_ = type;
            if (result.isArray())
                result.size = result.size * type.size;
            return result;
        }

        assert(0, "go back and fix declarator().");
    }

    private Type declFunc(Type ret, ParamStruct *names)
    {
        return funcParams(ret, names);
    }

    private Type funcParams(Type ret, ParamStruct *names)
    {
        bool varArgs = false;

        if (ret.isFunction())
            parseError("function returning function is not allowed");
        if (ret.isArray())
            parseError("function returning an array is not allowed");

        if (tokstr.matchKW("void"))
        {
            expectSep(")");
            return new FuncType(ret, null, false);
        }

        if (tokstr.matchSep(")"))
            return new FuncType(ret, null, false);

        Type[] params;
        while (!tokstr.peekSep(")"))
        {
            // TODO: add support for varArgs once we can
            // recognize "..." token.
            string name = null;
            Type type = funcParam(&name);

            if (name && names)
                names.params ~= name;
            params ~= type;

            if (!tokstr.peekSep(")"))
                expectSep(",");
        }

        expectSep(")");
        return new FuncType(ret, params, varArgs);
    }

    /// Read a single parameter.
    private Type funcParam(string *name)
    {
        StorageClass sc;
        // In case [type] itself is a function or pointer to function type,
        // its parameter names are ignored.
        Type type = declarator(declSpec(&sc), /* isAbstract */false, name);

        if (sc != StorageClass.UNSPECIFIED && sc != StorageClass.REGISTER)
            parseError("function parameter with storage class");

        if (tokstr.peek().kind == Token.IDENT)
        {
            Token tok = tokstr.read();

            if (name && *name)
                parseError(format!"unexpected identifier '%s'"(tokstr.peek().stringVal));
            if (name)
                *name = tok.stringVal;
        }

        if (type.isArray())
            return type.base().makePointer();

        if (type.isFunction())
            return type.makePointer();

        return type;
    }

    private Type declArray(Type base)
    {
        ulong len = -1;
        if (!tokstr.matchSep("]"))
        {
            len = constInteger();
            expectSep("]");
        }

        SrcLoc loc = tokstr.nextLoc();
        Type type = declarator(base, false);
        if (type.isFunction())
            parseError("array of function", loc);

        return type.makeArray(len);
    }

    private uint8_t qualifiers()
    {
        uint8_t qual = 0;

        while (isQualifier(tokstr.peek()))
            if (tokstr.read().stringVal == "const")
                qual |= Qualifier.CONST;

        return qual;
    }

    /// declaration-specifiers
    ///     : storage-class-specifier declaration-specifiers-opt
    ///     | type-specifier declaration-specifiers-opt
    ///     | type-qualifier declaration-specifiers-opt
    ///     | function-specifier declaration-specifiers-opt
    private Type declSpec(StorageClass *sc_ = null)
    {
        StorageClass sc = StorageClass.UNSPECIFIED;
        if (!isType(tokstr.peek()))
            parseError("type specifier expected", tokstr.nextLoc());

        Type type;
        bool isconst = false;
        bool isllong = false;
        ulong size = 0;

        void type_check() { if (type) parseError("two or more data types in declaration specifiers"); }
        void storage_class_check()
        {
            if (sc != StorageClass.UNSPECIFIED)
                parseError("two or more storage classes are specified");
        }

        while (isType(tokstr.peek()))
        {
            Token tok = tokstr.read();
            switch (tok.stringVal)
            {
                case "void": type_check(); type = voidType; break;
                case "_Bool": type_check(); type = boolType; break;
                case "char": type_check(); type = charType; break;
                case "int": type_check(); type = intType; break;
                case "float": type_check(); type = floatType; break;
                case "double": type_check(); type = doubleType; break;
                case "short": {
                    if (size) 
                        parseError("incompatible size of declaration specifiers"); 
                    size = shortType.size;
                    break;
                }
                case "long": {
                    if (!size || size == intType.size)
                        size = longType.size;
                    else if (size == longType.size)
                    {
                        size = llongType.size;
                        isllong = true;
                    }
                    else
                        parseError("incompatible size of declaration specifiers");
                    break;
                }
                case "unsigned": {
                    if (!type)
                        type = isllong ? ullongType : (size == longType.size) ? ulongType : uintType;
                    else if (type.isSigned())
                        type = type.getUnsigned();
                    else
                        parseError("incompatible declaration specifiers");
                    break;
                }
                case "signed": {
                    if (!type)
                        type = isllong ? llongType : (size == longType.size) ? longType : intType;
                    else if (type.isUnsigned())
                        type = type.getSigned();
                    else
                        parseError("incompatible declaration specifiers");
                    break;
                }
                case "struct": type_check(); type = declStruct(); break;
                case "union": type_check(); type = declUnion(); break;
                case "enum": type_check(); type = declEnum(); break;

                case "extern": storage_class_check(); sc = StorageClass.EXTERN; break;
                case "static": storage_class_check(); sc = StorageClass.STATIC; break;
                case "auto": storage_class_check(); sc = StorageClass.AUTO; break;
                case "register": storage_class_check(); sc = StorageClass.REGISTER; break;

                case "const": isconst = true; break;
                case "restrict": break;
                case "inline": break;
                case "volatile": break;
                
                case "typedef": // TODO.
                default:
                    assert(0, "internal error");
            }
        }

        if (sc_)
            *sc_ = sc;

        if (type == voidType && (size != 0 || isconst))
            parseError("incompatible declaration specifiers");
        if ((type == floatType || type == charType || type == boolType) && size != 0)
            parseError("incompatible declaration specifiers");
        if (size == shortType.size && (type && type != intType && type != uintType))
            parseError("incompatible declaration specifiers");
        if (size == longType.size && 
            (type && type.size != intType.size && type.size != longType.size))
            parseError("incompatible declaration specifiers");

        if (!type && size == shortType.size)
            type = shortType;

        if (!type && size == longType.size)
            type = isllong ? llongType : longType;

        return isconst ? type.getConst() : type;
    }

    private Type declStruct()
    {
        return null;
    }

    private Type declUnion()
    {
        return null;
    }

    private Type declEnum()
    {
        return null;
    }

    private ulong constInteger()
    {
        return -1;
    }

    private Expr expr()
    {
        Expr node = comma();
        if (!node)
            parseError("expected an expression");
        return node;
    }

    private Expr comma()
    {
        Expr[] exprs = [];
        Expr expr = assignment();
        while (tokstr.matchSep(","))
        {
            exprs ~= expr;
            expr = assignment();
        }

        if (exprs.length)
            expr = new CommaExpr(exprs[$ - 1].type, exprs, exprs[0].loc);
        return expr;
    }

    private Expr assignment()
    {
        Expr lhs = conditional();
        Token tokop = tokstr.read();
        SrcLoc loc = tokstr.currLoc();
        string op = getCompoundAssignop(tokop);

        if ((tokop.kind == Token.SEP && tokop.stringVal == "=") || op)
        {
            Expr rhs = typeConv(assignment());
            ensureLvalue(lhs);
            ensureNotConst(lhs);

            // If this is a compound assignment operator, we'll
            // turn it into a binary expression + an assignment.
            rhs = op ? createBinExpr(op, typeConv(lhs), rhs) : rhs;
            // Implicit cast.
            if (lhs.type.isArithmetic() && !rhs.type.isSame(lhs.type))
                rhs = new UnaryExpr(UnaryExpr.CAST, lhs.type, rhs, rhs.loc);
            else if (!lhs.type.isSame(rhs.type))
                parseError("incompatible types", loc);

            return new AssignExpr(lhs, rhs, loc);
        }
        tokstr.unread();
        return lhs;
    }

    private string getCompoundAssignop(Token tok)
    {
        if (tok.kind == Token.SEP)
        {
            switch (tok.stringVal)
            {
                case "+=":  return "+";
                case "-=":  return "-";
                case "*=":  return "*";
                case "/=":  return "/";
                case "%=":  return "%";
                case "&=":  return "&";
                case "|=":  return "|";
                case "^=":  return "^";
                case "<<=": return "<<";
                case ">>=": return ">>";
                default:
                    return null;
            }
        }
        return null;
    }

    private Expr conditional()
    {
        Expr cond = logicalOr();

        if (tokstr.matchSep("?"))
        {
            SrcLoc loc = tokstr.currLoc();
            Expr then = typeConv(expr());
            expectSep(":");
            Expr _else = typeConv(conditional());

            if (then.type.isArithmetic() && _else.type.isArithmetic())
            {
                Type type = typeConvArith(then.type, _else.type);
                then = typeConvWrap(then, type);
                _else = typeConvWrap(_else, type);
            }
            return new CondExpr(then.type, cond, then, _else, loc);
        }
        return cond;
    }

    private Expr logicalOr()
    {
        Expr lhs = logicalAnd();
        while (tokstr.matchSep("||"))
        {
            SrcLoc loc = tokstr.currLoc();
            lhs = new BinExpr(intType, "||", lhs, logicalAnd(), loc);
        }

        return lhs;
    }

    private Expr logicalAnd()
    {
        Expr lhs = bitwiseOr();
        while (tokstr.matchSep("&&"))
        {
            SrcLoc loc = tokstr.currLoc();
            lhs = new BinExpr(intType, "&&", lhs, bitwiseOr(), loc);
        }

        return lhs;
    }

    private Expr bitwiseOr()
    {
        Expr lhs = bitwiseExor();
        while (tokstr.matchSep("|"))
        {
            Expr rhs = bitwiseExor();
            lhs = createBinExpr("|", typeConv(lhs), typeConv(rhs));
        }
        return lhs;
    }

    private Expr bitwiseExor()
    {
        Expr lhs = bitwiseAnd();
        while (tokstr.matchSep("^"))
        {
            Expr rhs = bitwiseAnd();
            lhs = createBinExpr("^", typeConv(lhs), typeConv(rhs));
        }
        return lhs;
    }

    private Expr bitwiseAnd()
    {
        Expr lhs = equality();
        while (tokstr.matchSep("&"))
        {
            Expr rhs = equality();
            lhs = createBinExpr("&", typeConv(lhs), typeConv(rhs));
        }
        return lhs;
    }

    private Expr equality()
    {
        Expr lhs = relational();
        Token tokop = tokstr.read();

        for (;;)
        {
            if (tokop.kind == Token.SEP && (tokop.stringVal == "==" || 
                tokop.stringVal == "!="))
            {
                Expr rhs = relational();
                lhs = createBinExpr(tokop.stringVal, typeConv(lhs), typeConv(rhs));
            }
            else
            {
                tokstr.unread();
                return lhs;
            }
        }
        return lhs;
    }

    private Expr relational()
    {
        Expr lhs = shift();
        Token tokop = tokstr.read();

        for (;;)
        {
            if (tokop.kind == Token.SEP && (tokop.stringVal == "<" ||
                tokop.stringVal == ">" || tokop.stringVal == "<=" ||
                tokop.stringVal == ">="))
            {
                Expr rhs = shift();
                lhs = createBinExpr(tokop.stringVal, typeConv(lhs), typeConv(rhs));
            }
            else
            {
                tokstr.unread();
                return lhs;
            }
        }
        return lhs;
    }

    private Expr shift()
    {
        Expr lhs = additive();
        Token tokop = tokstr.read();

        for (;;)
        {
            SrcLoc loc = tokstr.currLoc();
            if (tokop.kind == Token.SEP && (tokop.stringVal == "<<" ||
                tokop.stringVal == ">>"))
            {
                Expr rhs = additive();
                if (!lhs.type.isInteger() || !rhs.type.isInteger())
                    parseError("non-integer type on shift operator", loc);
                lhs = createBinExpr(tokop.stringVal, typeConv(lhs), typeConv(rhs));
            }
            else
            {
                tokstr.unread();
                break;
            }
            tokop = tokstr.read();
        }
        return lhs;
    }

    private Expr additive()
    {
        Expr lhs = multiplicative();
        Token tokop = tokstr.read();

        // Consume as many operators as we can.
        for (;;)
        {
            if (tokop.kind == Token.SEP)
            {
                if (tokop.stringVal == "+" || tokop.stringVal == "-")
                    lhs = createBinExpr(tokop.stringVal, typeConv(lhs), typeConv(multiplicative()));
                else
                {
                    tokstr.unread();
                    break;
                }
            }
            else
            {
                tokstr.unread();
                break;
            }
            tokop = tokstr.read();
        }
        return lhs;
    }

    private Expr multiplicative()
    {
        Expr lhs = _cast();
        Token tokop = tokstr.read();

        // Consume as many operators as we can.
        for (;;)
        {
            if (tokop.kind == Token.SEP)
            {
                if (tokop.stringVal == "*" || tokop.stringVal == "/" || tokop.stringVal == "%")
                    lhs = createBinExpr(tokop.stringVal, typeConv(lhs), typeConv(_cast()));
                else
                {
                    tokstr.unread();
                    break;
                }
            }
            else
            {
                tokstr.unread();
                break;
            }

            tokop = tokstr.read();
        }
        return lhs;
    }

    private Expr _cast()
    {
        SrcLoc loc = tokstr.currLoc();
        // Compound literal or type cast.
        if (tokstr.peekSep("(") && isType(tokstr.peek(1)))
        {
            tokstr.read();  // '('
            Type type = typeName();
            expectSep(")");

            // Compound literal.
            if (tokstr.matchSep("{"))
                return postfix(compoundLiteral(type));

            if (!type.isScalar())
                parseError("conversion to non-scalar type");
            Expr operand = _cast();
            return new UnaryExpr(UnaryExpr.CAST, type, operand, loc);
        }

        return unary();
    }

    private Expr compoundLiteral(Type)
    {
        return null;
    }

    /// Unary expression.
    private Expr unary()
    {
        auto tokop = tokstr.read();
        Expr expr;

        if (tokop.kind == Token.SEP)
        {
            switch (tokop.stringVal)
            {
                case "++":      expr = unaryIncrDecr(true); break;
                case "--":      expr = unaryIncrDecr(false); break;
                case "sizeof":  expr = unarySizeof(); break;
                case "&":       expr = unaryAddrof(); break;
                case "*":       expr = unaryDeref(); break;
                case "+":       expr = _cast(); break;
                case "-":       expr = unaryMinus(); break;
                case "~":       expr = unaryBitNot(); break;
                case "!":       expr = unaryLogicalNot(); break;
                default: {
                    tokstr.unread();
                    expr = primary();
                    break;
                }
            }
        }
        // Descend to primary.
        else
        {
            tokstr.unread();
            expr = primary();
        }

        return postfix(expr);
    }

    private Expr unaryIncrDecr(bool incr)
    {
        auto k = incr ? UnaryExpr.PREF_INCR : UnaryExpr.PREF_DECR;
        SrcLoc loc = tokstr.currLoc();
        Expr opnd = typeConv(unary());
        ensureLvalue(opnd);
        return new UnaryExpr(k, opnd.type, opnd, loc);
    }

    private Expr unarySizeof()
    {
        Type type;
        SrcLoc loc = tokstr.currLoc();

        // sizeof (type)
        if (tokstr.peekSep("(") && isType(tokstr.peek(1)))
        {
            tokstr.read();      // "("
            type = typeName();
            expectSep(")");
        }
        else
            type = unary().type;

        return new IntExpr(ulongType, type.size, loc);
    }

    private Expr unaryAddrof()
    {
        SrcLoc loc = tokstr.currLoc();
        Expr opnd = _cast();
        if (opnd.type.isFunction())
            return typeConv(opnd);

        ensureLvalue(opnd);
        return new UnaryExpr(UnaryExpr.ADDR_OF, opnd.type.makePointer(), opnd, loc);
    }

    private Expr unaryDeref()
    {
        SrcLoc loc = tokstr.currLoc();
        Expr opnd = typeConv(_cast());

        if (!opnd.type.isPointer())
            parseError(format!"unable to deference type '%s'"(opnd.type), loc);

        if (opnd.type.base().isFunction())
            return opnd;
        return new UnaryExpr(UnaryExpr.DEREF, opnd.type.base(), opnd, loc);
    }

    private Expr unaryMinus()
    {
        SrcLoc loc = tokstr.currLoc();
        Expr opnd = typeConv(_cast());

        if (!opnd.type.isArithmetic())
            parseError("'-' on non-arithmetic type", loc);
        return new UnaryExpr(UnaryExpr.MINUS, opnd.type, opnd, loc);
    }

    private Expr unaryBitNot()
    {
        SrcLoc loc = tokstr.currLoc();
        Expr opnd = typeConv(_cast());

        if (!opnd.type.isInteger())
            parseError("'~' on non-integer type", loc);
        return new UnaryExpr(UnaryExpr.BIT_NOT, opnd.type, opnd, loc);
    }

    private Expr unaryLogicalNot()
    {
        SrcLoc loc = tokstr.currLoc();
        Expr opnd = typeConv(_cast());

        return new UnaryExpr(UnaryExpr.BOOL_NOT, boolType, opnd, loc);
    }

    /// Consumes postfix operators. [expr] is passed from unary.
    private Expr postfix(Expr expr)
    {
        // Exhaust all possible postfix operators.
        for (;;)
        {
            Token tok = tokstr.read();
            if (tok.kind == Token.SEP)
            {
                switch (tok.stringVal)
                {
                    case "++", "--":    expr = postfixIncrDecr(expr, tok.stringVal); break;
                    case ".":           expr = postfixMember(expr, false); break;
                    case "->":          expr = postfixMember(expr, true); break;
                    case "[":           expr = postfixSubscript(expr); break;
                    case "(":           expr = postfixCall(expr); break;
                    default: {
                        tokstr.unread(); 
                        return expr;
                    }
                }
            }
            else
            {
                tokstr.unread();
                return expr;
            }
        }
    }

    private Expr postfixIncrDecr(Expr lhs, string op)
    {
        SrcLoc loc = tokstr.currLoc();
        ensureLvalue(lhs);
        ensureNotConst(lhs);
        if (!lhs.type.isScalar())
            parseError(format!"cannot perform %s on type '%s"(op, lhs.type), lhs.loc);

        return new UnaryExpr(
            (op == "++" ? UnaryExpr.POST_INCR : UnaryExpr.POST_DECR),
            lhs.type, lhs, loc);
    }

    private Expr postfixMember(Expr lhs, bool deref)
    {
        SrcLoc loc = tokstr.currLoc();
        // Member name.
        auto tokIdent = tokstr.read();
        auto identLoc = tokstr.currLoc();
        if (tokIdent.kind != Token.IDENT)
            parseError("expect identifier", identLoc);

        // Deref the pointer.
        if (deref)
        {
            if (!lhs.type.isPointer() && !lhs.type.isArray())
                parseError(format!"cannot dereference expression of type '%s'"(lhs.type), loc);

            lhs = new UnaryExpr(
                UnaryExpr.DEREF,
                lhs.type.base(), /* result type. */
                lhs,        /* opnd. */
                lhs.loc);
        }

        Type ty = ensureMember(lhs.type, tokIdent.stringVal);
        return new MemberExpr(ty, lhs, tokIdent.stringVal, loc);
    }

    private Expr postfixSubscript(Expr lhs)
    {
        Expr idx = expr();
        if (!idx.type.isPointer() && !idx.type.isArray())
            parseError(format!"'%s' is not a ptr/array type"(lhs.type), lhs.loc);

        if (!idx.type.isInteger())
            parseError("array subscript is not an integer", idx.loc);
        
        expectSep("]");
        lhs = createBinExpr("+", lhs, idx);
        return new UnaryExpr(UnaryExpr.DEREF, lhs.type.base(), lhs, idx.loc);
    }

    private Expr postfixCall(Expr lhs)
    {
        // Function designator.
        if (lhs.type.isFunction())
        {
            Expr[] args = funcArgs(lhs.type.params(), lhs.type.varArgs());
            IdentExpr funIdent = cast(IdentExpr)lhs;
            assert(funIdent);

            return new CallExpr(lhs.type.ret(), funIdent.var.name, args, funIdent.loc);
        }

        if (!lhs.type.isPointer() || !lhs.type.base().isFunction())
            parseError(format!"expression of type '%s' is not callable"(lhs.type), lhs.loc);

        // Call a pointer to a function.
        Expr[] args = funcArgs(lhs.type.base().params(), lhs.type.base().varArgs());
        return new CallExpr(lhs, args, lhs.loc);
    }

    private Expr[] funcArgs(Type[] params, bool varArgs = false)
    {
        int i = 0;
        Expr[] args;
        while (!tokstr.peekSep(")"))
        {
            Type type = null;
            Expr arg = assignment();

            if (i >= params.length && !varArgs)
                parseError("too many arguments to function of type", arg.loc);
            else if (i >= params.length)
                type = arg.type.isFP() ? doubleType : arg.type.isInteger() ? intType : arg.type;
            else
                type = params[i];

            ensureAssignable(type, arg.type);
            if (!arg.type.isSame(type))
                arg = new UnaryExpr(UnaryExpr.CAST, type, arg, arg.loc);

            args ~= arg;
            if (tokstr.peekSep(")"))
                break;

            expectSep(",");
        }

        expectSep(")");
        return args;
    }

    /// primary-expression
    ///     : identifier
    ///     | constant
    ///     | string-literal
    ///     | "(" expr ")"
    private Expr primary()
    {
        auto tok = tokstr.read();
        auto loc = SrcLoc(tok.pos, tokstr.filename);

        switch (tok.kind)
        {
            case Token.IDENT:   return identifier(tok.stringVal, loc);
            case Token.INT:     return integer(tok.intVal, tok.intsfx, loc);
            case Token.FLOAT:   return floating(tok.floatVal, tok.fsfx, loc);
            case Token.STRING:  return new StringExpr(tok.stringVal, loc);
            case Token.SEP:
                // NOTE: we've already handled compound literal in cast expression.
                expectSep("(");
                Expr grouping = expr();
                expectSep(")");
                return grouping;

            default:
                assert(false, "internal error");
        }
    }

    private Expr identifier(string name, SrcLoc loc)
    {
        // Function designator.
        if (tokstr.peekSep("(") && !(name in genv))
        {
            auto fntype = new FuncType(intType, null);
            auto fndecl = new FuncDecl(fntype, name, null, SrcLoc());
            auto fndesg = new IdentExpr(fndecl, loc);

            report(SVR_WARN, format!"implicit declaration of %s"(name), loc);
            // Pointer to function. As the result of the conversion is
            // not an lvalue.
            return typeConv(fndesg);
        }

        Decl decl = null;
        if (tokstr.peekSep("("))
            decl = genv[name];

        if (!decl)
            decl = symResolve(name);

        if (!decl)
            parseError(format!"use of undeclared variable '%s'"(name), loc);

        return new IdentExpr(decl, loc);
    }

    private Expr integer(long val, string sfx, SrcLoc loc)
    {
        const lsfx = map!toLower(sfx).array;
        switch (lsfx)
        {
            case "l":   return new IntExpr(longType, val, loc);
            case "u":   return new IntExpr(uintType, val, loc);
            case "ul":  return new IntExpr(ulongType, val, loc);
            case "ll":  return new IntExpr(llongType, val, loc);
            case "ull": return new IntExpr(ullongType, val, loc);
            default:    return new IntExpr(val > int32_t.max ? longType : intType, val, loc);
        }
        assert(0);
    }

    private Expr floating(double val, string sfx, SrcLoc loc)
    {
        switch (sfx)
        {
            case "f", "F", "":  return new FloatExpr(floatType, val, loc);
            case "l", "L":      return new FloatExpr(doubleType, val, loc);
            default:            return new FloatExpr(doubleType, val, loc);
        }
        assert(0);
    }
}
