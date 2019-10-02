///     This file contains semantic utilities
///     Copyright 2019 Yiyong Li.

module sema.expr;

import std.format;
import parser.types;
import parser.ast;
import sema.env;
import reporter;

/// Simple wrapper for reporting an error and return null.
private T semaErrExpr(T)(string msg, SrcLoc loc)
    if (is (T : Expr))
{
    report(
        SVR_ERR,
        msg,
        loc
    );

    return null;
}

/// Abort and report an error if [expr] is not an lvalue.
private bool isLvalue(Expr expr)
{
    if (cast(IdentExpr)expr || cast (MemberExpr)expr)
    {
        return true;
    }

    auto unary = cast(UnaryExpr)expr;
    if (unary !is null)
    {
        switch (unary.kind)
        {
            case UnaryExpr.DEREF:   return true;
            default:
                break;
        }
    }

    return false;
}

/// Whether expr is a constant literal.
/// NOTE: compound literals are not taken as constant literals.
private bool litExpr(Expr expr)
{
    if (
        cast(IntExpr)expr   || 
        cast(FloatExpr)expr ||
        cast(StringExpr)expr)
    {
        return true;
    }
    return false;
}

/// p 6.3.1.8
/// Common real type for an arithmetic results.
private Type arithCommType(Type a, Type b)
{
    assert(isArithmetic(a) && isArithmetic(b));

    if (a == doubleType || b == doubleType)
    {
        return doubleType;
    }

    if (a == floatType || b == floatType)
    {
        return floatType;
    }

    // No need to change.
    if (a == b)
    {
        return a;
    }

    Type wider = (intRank(a) > intRank(b) ? a : b);
    Type narrower = (intRank(a) > intRank(b) ? b : a);

    // Pick the one with greater length.
    if ((isSigned(a) && isSigned(b))     ||
        (isUnsigned(a) && isUnsigned(b)) ||
        (isUnsigned(wider)))
    {
        return wider;
    }

    // wider is signed and narrower is unsigned.
    if (intMaxVal(wider) > cast(ulong)intMaxVal(narrower))
    {
        return wider;
    }

    return getUnsigned(narrower);
}

/// Convert expr [opnd] into an expr with [commType] type.
private Expr arithConv(Expr opnd, Type commType)
{
    assert(isArithmetic(opnd.type) && isArithmetic(commType));

    if (opnd.type == commType)
    {
        // No need to convert.
        return opnd;
    }

    // If [opnd] is a literal, we'll convert them directly.
    auto intexpr = cast(IntExpr)opnd;
    auto fexpr = cast(FloatExpr)opnd;

    if (intexpr && isInteger(commType))
    {
        assert(!fexpr, "No FP values are demoted into integers");

        return new IntExpr(
            commType,
            intexpr.value,
            intexpr.loc
        );
    }

    if ((intexpr || fexpr) && isFP(commType))
    {
        return new FloatExpr(
            commType,
            (intexpr ? intexpr.value : fexpr.value ),
            (intexpr ? intexpr.loc : fexpr.loc )
        );
    }

    // Implicit cast is necessary.
    return new UnaryExpr(
        UnaryExpr.CAST,
        commType,
        opnd,
        opnd.loc
    );
}

/// Evaluate binary expressions.
private Expr semaEvalLit(string op, Expr lhs, Expr rhs, Type commType)
{
    assert(litExpr(lhs) && litExpr(rhs));

    auto lintexpr = cast(IntExpr)lhs;
    auto lfexpr = cast(FloatExpr)lhs;
    auto rintexpr = cast(IntExpr)rhs;
    auto rfexpr = cast(FloatExpr)rhs;

    string genLitExpr(string op)
    {
        return format!"
        return fp
            ? new FloatExpr(commType, lfexpr.value %s rfexpr.value, lhs.loc)
            : new IntExpr(commType, lintexpr.value %s rintexpr.value, lhs.loc);"(op, op);
    }

    auto fp = isFP(commType);
    switch (op)
    {
        case "*":   mixin(genLitExpr("*"));
        case "/":   mixin(genLitExpr("/"));
        case "%":   mixin(genLitExpr("%"));
        case "+":   mixin(genLitExpr("+"));
        case "-":   mixin(genLitExpr("-"));
        default:
            assert(false);
    }
}

/// Semantic action on additive expressions.
Expr semaAdd(string op, Expr lhs, Expr rhs, SrcLoc opLoc)
{
    assert(op == "+" || op == "-");
    assert(lhs && rhs);

    /*
    Normal arithmetics.
    */
    if (isArithmetic(lhs.type) && isArithmetic(rhs.type))
    {
        auto commType = arithCommType(lhs.type, rhs.type);
        lhs = arithConv(lhs, commType);
        rhs = arithConv(rhs, commType);

        if (litExpr(lhs) && litExpr(rhs))
        {
            return semaEvalLit(op, lhs, rhs, commType);
        }

        return new BinExpr(
            commType,
            op,
            lhs,
            rhs,
            opLoc
        );
    }

    auto lptr = cast(PtrType)lhs.type;
    auto rptr = cast(PtrType)rhs.type;
    
    /*
    Ptr arithmetics.
    */

    // Both ptr.
    if (lptr && rptr)
    {
        if (op != "-")
        {
            return semaErrExpr!Expr(
                format!"invalid operands to binary expression ('%s' and '%s')"(lptr, rptr),
                opLoc
            );
        }

        // Incompatible ptr arithmetics.
        else if (lptr != rptr)
        {
            return semaErrExpr!Expr(
                format!"'%s' and '%s' are not pointers to compatible types"(lptr, rptr),
                opLoc
            );
        }
    }
    // One is a ptr.
    else if (lptr)
    {
        if (!isInteger(rhs.type))
        {
            return semaErrExpr!Expr(
                format!"invalid operands to binary expression ('%s' and '%s')"(lptr, rhs.type),
                opLoc
            );
        }
    }
    // [rptr] cannot be a ptr.
    else
    {
        return semaErrExpr!Expr(
            format!"invalid operands to binary expression ('%s' and '%s')"(lhs.type, rhs.type),
            opLoc
        );
    }

    assert(lptr);
    return new BinExpr(
        lptr,
        op,
        lhs,
        rhs,
        opLoc
    );
}

/// Semantic action on multiplicative expressions.
Expr semaMult(string op, Expr lhs, Expr rhs, SrcLoc opLoc)
{
    assert(op == "*" || op == "/" || op == "%");
    assert(lhs && rhs);

    /// Ensure both operands are of arithmetic type.
    bool ensureOpnd(Expr opnd, bool function(Type) pred)
    {
        if (!pred(opnd.type))
        {
            report(
                SVR_ERR,
                "invalid operands to binary expression",
                opnd.loc
            );
            return false;
        }
        return true;
    }

    if (
        !ensureOpnd(lhs, &isArithmetic) || 
        !ensureOpnd(rhs, &isArithmetic))
    {
        return null;
    }

    if (op == "%" && (
        !ensureOpnd(lhs, &isInteger) || 
        !ensureOpnd(rhs, &isInteger)))
    {
        return null;
    }

    // Obtain common type.
    auto commType = arithCommType(lhs.type, rhs.type);
    
    lhs = arithConv(lhs, commType);
    rhs = arithConv(rhs, commType);
    if (litExpr(lhs) && litExpr(rhs))
    {
        // Long as we can compute them.
        return semaEvalLit(op, lhs, rhs, commType);
    }

    return new BinExpr(
        commType,
        op,
        lhs,
        rhs,
        opLoc
    );
}

// Convert [lit] expression to a literal of [type].
private Expr litConv(Type type, Expr lit)
{
    assert(litExpr(lit) && isReal(type));

    auto intexpr = cast(IntExpr)lit;
    auto fexpr = cast(FloatExpr)lit;
    auto strexpr = cast(StringExpr)lit;
    auto strptr = (strexpr ? cast(char*)strexpr.value : null);

    assert(intexpr || fexpr || strexpr);

    // Integer.
    if (isInteger(type))
    {
        return new IntExpr(
            type,
            (intexpr 
                ? intexpr.value 
                : (fexpr 
                    ? cast(long)fexpr.value 
                    : cast(long)strptr)),
            lit.loc
        );
    }

    // FP.
    assert(isFP(type));
    return new FloatExpr(
        type,
        (fexpr 
            ? fexpr.value
            : (intexpr
                ? cast(double)intexpr.value
                : cast(double)strptr)),
        lit.loc
    );
}

/// Semantic action on cast expressions.
Expr semaCast(Type type, Expr opnd, SrcLoc parenLoc)
{
    assert(type && opnd);

    Type errType;
    SrcLoc errLoc;

    bool nonScalar(SrcLoc loc, Type type)
    {
        errLoc = loc;
        errType = type;
        
        return !isScalar(type);
    }

    // Decay the array if needed.
    if (cast(ArrayType)opnd.type)
    {
        auto arrayType = cast(ArrayType)opnd.type;

        opnd = new UnaryExpr(
            UnaryExpr.DECAY,
            getPtrType(arrayType.elemTy),
            opnd,
            opnd.loc);
    }

    // Error on non scalar type.
    if (nonScalar(parenLoc, type) || nonScalar(opnd.loc, opnd.type))
    {
        goto NON_SCALAR_ERR;
    }
    // If this is a literal, cast it directly.
    else if (isReal(type) && litExpr(opnd))
    {
        return litConv(type, opnd);
    }
    // Cast unary expression.
    else
    {
        return new UnaryExpr(
            UnaryExpr.CAST,
            type,
            opnd,
            parenLoc
        );
    }

NON_SCALAR_ERR:
    return semaErrExpr!UnaryExpr(
        format!"used type '%s' where arithmetic or pointer type is required"(errType),
        errLoc
    );
}

/// Semantic action on unary arithmetic operators.
UnaryExpr semaUAOp(string op, Expr opnd, SrcLoc oploc)
{
    UnaryExpr.Kind kind;
    Type resType;

    // TODO: type promotion.
    switch (op)
    {
        case "+", "-":
        if (!isArithmetic(opnd.type))
        {
            goto INVALID_OPND;
        }
        else
        {
            kind = (op == "+") ? UnaryExpr.PLUS : UnaryExpr.MINUS;
            resType = opnd.type;
            goto DONE;
        }

        case "~":
        if (!isInteger(opnd.type))
        {
            goto INVALID_OPND;
        }
        else
        {
            kind = UnaryExpr.BIT_NOT;
            resType = opnd.type;
            goto DONE;
        }

        case "!":
        if (!isScalar(opnd.type))
        {
            goto INVALID_OPND;
        }
        else
        {
            kind = UnaryExpr.BOOL_NOT;
            resType = boolType;
            goto DONE;
        }
        
        default:
            assert(false);
    }

INVALID_OPND:
    return semaErrExpr!UnaryExpr(
        format!"invalid operand type '%s' for unary expression"(opnd.type),
        oploc
    );

DONE:
    return new UnaryExpr(
        kind,
        resType,
        opnd,
        oploc
    );
}

/// Semantic action on increment and decrement uop.
UnaryExpr semaIncrDecr(string prepost)(Expr opnd, string op, SrcLoc opLoc)
{
    assert(opnd);
    assert(op == "++" || op == "--");
    static assert((prepost == "pre") || (prepost == "post"));
    static if (prepost == "pre")
    {
        const INCR = UnaryExpr.PREF_INCR;
        const DECR = UnaryExpr.PREF_DECR;
    }
    else
    {
        const INCR = UnaryExpr.POST_INCR;
        const DECR = UnaryExpr.POST_DECR;
    }

    // opnd must be an lvalue.
    if (!isLvalue(opnd))
    {
        return semaErrExpr!UnaryExpr(
            "expected an lvalue",
            opnd.loc
        );
    }

    // opnd must not be const qualified.
    if (opnd.type.qual & QUAL_CONST)
    {
        return semaErrExpr!UnaryExpr(
            format!"cannot assign to a const qualified type '%s' lvalue"(opnd.type),
            opnd.loc
        );
    }

    auto opnType = opnd.type;

    if (
        (!isInteger(opnType)) &&
        (opnType != floatType) &&
        (opnType != doubleType) &&
        (cast(PtrType)opnType is null)
    )
    {
        return semaErrExpr!UnaryExpr(
            format!"cannot perform %s on type '%s'"(
                (op == "++") ? "increment" : "decrement",
                opnType
            ),
            opLoc
        );
    }

    return new UnaryExpr(
        (op == "++") ? INCR : DECR,
        opnd.type,
        opnd,
        (INCR == UnaryExpr.PREF_INCR) ? opLoc : opnd.loc
    );
}

/// Semantic action on RecType member access.
MemberExpr semaMemberExpr(Expr struc, string ident, SrcLoc loc)
{
    assert(struc);
    ulong findMemberIdx(RecType recType, string name)
    {
        foreach (i, m; recType.members)
        {
            if (m.name == name)
            {
                return i;
            }
        }
        return recType.members.length;
    }


    auto recType = cast(RecType)(struc.type);
    if (recType is null)
    {
        return semaErrExpr!MemberExpr(
            format!"member reference base type '%s' is not a struct or union"(struc.type),
            struc.loc
        );
    }

    auto idx = findMemberIdx(recType, ident);
    if (idx == recType.members.length)
    {
        return semaErrExpr!MemberExpr(
            format!"member '%s' does not exist in type '%s'"(ident, recType),
            loc
        );
    }
    return new MemberExpr(recType.members[idx].type, struc, ident, loc);
}

/// Semantic action on "&" operator.
UnaryExpr semaAddrof(Expr opnd, SrcLoc amploc)
{
    if (!isLvalue(opnd))
    {
        return semaErrExpr!UnaryExpr(
            format!"cannot take the address of an rvalue of type '%s'"(opnd.type),
            amploc
        );
    }

    if (opnd.type.qual & QUAL_REG)
    {
        return semaErrExpr!UnaryExpr(
            "cannot take the address of a register variable",
            amploc
        );
    }

    return new UnaryExpr(
        UnaryExpr.ADDR_OF,
        getPtrType(opnd.type),
        opnd,
        amploc
    );
}

/// Semantic action on array access.
UnaryExpr semaDeref(Expr base, Expr idx = null)
{
    assert(base);

    auto arrayType = cast(ArrayType)(base.type);
    auto ptrType = cast(PtrType)(base.type);
    Type elemTy;

    // When base is of ArrayType, decay array to pointer.
    if (arrayType)
    {
        elemTy = arrayType.elemTy;
        base = new UnaryExpr(
            UnaryExpr.DECAY, 
            getPtrType(arrayType.elemTy), 
            base, 
            base.loc);
    }

    // Whne base is of PtrType, the result type will be
    // the pointee type.
    else if (ptrType)
    {
        elemTy = ptrType.base;
        elemTy.qual = ptrType.qual;
    }

    else
    {
        return semaErrExpr!UnaryExpr(
            format!"cannot dereference expression of type '%s'"(base.type),
            base.loc,
        );
    }

    // idx is an optional offset.
    if (idx)
    {
        if (!isInteger(idx.type))
        {
            return semaErrExpr!UnaryExpr(
                "array subscript is not an integer",
                idx.loc
            );
        }

        base = new BinExpr(
            base.type,  /* Type of the + result. */
            "+",        /* op */
            base,       /* lhs */
            idx,        /* rhs */
            base.loc
        );
    }

    return new UnaryExpr(
        UnaryExpr.DEREF,
        elemTy,
        base,
        base.loc
    );
}

/// Semantic action on call expression.
CallExpr semaCall(Expr callee, Expr[] args, SrcLoc parenLoc)
{
    assert(callee);
    auto ftype = cast(FuncType)(callee.type);

    // Error if callee is not a function.
    if (ftype is null)
    {
        return semaErrExpr!CallExpr(
            format!"called object type '%s' is not a function or function pointer"(callee.type),
            callee.loc
        );
    }

    // When callee is an identifier.
    if (cast(IdentExpr)callee)
    {
        auto fexpr = cast(IdentExpr)callee;
        auto fdecl = fexpr.identDecl;

        if (fdecl is null)
        {
            // Do not allow implicit declaration.
            return semaErrExpr!CallExpr(
                format!"implicit declaration of function '%s' is not valid"(fexpr.identStr),
                fdecl.loc
            );
        }
    }

    // Check arity.
    if (ftype.params.length != args.length)
    {
        // The "void" param is preserved, so we'll deal with it here.
        if (
            (ftype.params.length == 1) &&
            (ftype.params[0] == voidType) &&
            (args.length == 0))
        {
            return new CallExpr(ftype.retType, callee, [], parenLoc);
        }

        return semaErrExpr!CallExpr(
            "unmatched number of arguments for function",
            callee.loc
        );
    }

    // Check arguments.
    foreach (i, arg; args)
    {
        // TODO: arg-promotion.
        if (arg.type != ftype.params[i])
        {
            return semaErrExpr!CallExpr(
                format!"incompatible arg type '%s'"(arg.type),
                arg.loc
            );
        }
    }

    return new CallExpr(ftype.retType, callee, args, parenLoc);
}

/// Semantic action on identier. 
IdentExpr semaIdent(string name, SrcLoc loc)
{
    auto decl = envResolv(name);

    if (decl is null)
    {
        return semaErrExpr!IdentExpr(
            format!"use of undeclared variable '%s'"(name),
            loc
        );
    }
    auto ident = new IdentExpr(decl, name, loc);
    return ident;
}

/// Truncate a signed [val] if it overflows.
/// [S] stands for signed-type. [U] stands for unsigned
/// type with the same size as [S].
private void trunc(S, U, N)(ref N val, SrcLoc loc)
{
    static assert(__traits(isIntegral, S));
    static assert(__traits(isIntegral, U));
    static assert(__traits(isIntegral, N));

    // Overflow.
    if (val < S.min || val > S.max)
    {
        report(
            SVR_WARN, 
            format!"%s gets implicitly converted to %s"(val, cast(S)val),
            loc
        );
        val = cast(S)val;
    }

    // Greater than the unsigned version can represent.
    else if (val > U.max)
    {
        report(
            SVR_WARN, 
            format!"%s gets truncated to %s"(val, S.max & val),
            loc
        );
        val = S.max & val;
    }
}

/// Semantic action on int literal.
IntExpr semaInt(long val, string sfx, SrcLoc loc)
{
    import std.algorithm : map;
    import std.ascii : toLower;
    import std.array : array;

    auto lcsfx = map!(toLower)(sfx).array;
    switch (lcsfx)
    {
        case "":
            // Truncate overflow.
            trunc!(int, uint, long)(val, loc);
            return new IntExpr(intType, val, loc);

        // FIXME: for now assume these are fine.
        case "l":   return new IntExpr(longType, val, loc);
        case "u":   return new IntExpr(uintType, val, loc);
        case "ul":  return new IntExpr(ulongType, val, loc);
        case "ll":  return new IntExpr(llongType, val, loc);
        case "ull": return new IntExpr(ullongType, val, loc);

        default:
            return semaErrExpr!IntExpr(
                format!"Unknown suffix for integer: %s"(val), 
                loc
            );
    }
}

/// Semantic action on FP literal.
FloatExpr semaFloat(double val, string sfx, SrcLoc loc)
{
    switch (sfx)
    {
        case "f", "F", "":  return new FloatExpr(floatType, val, loc);
        case "l", "L":      return new FloatExpr(doubleType, val, loc);
        default:
            return semaErrExpr!FloatExpr(
                format!"Unknown suffix for floating literal: %s"(val),
                loc
            );
    }
}
