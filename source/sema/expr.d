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

    // lhs must be an lvalue.
    if (!isLvalue(opnd))
    {
        return semaErrExpr!UnaryExpr(
            "expected an lvalue",
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
        opLoc
    );
}

/// Semantic action on RecType member access.
MemberExpr semaRecAccess(Expr struc, string ident, SrcLoc loc)
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
    }

    else
    {
        return semaErrExpr!UnaryExpr(
            "subscripted value is not an array or pointer",
            idx.loc,
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
    switch (sfx)
    {
        case "":
            // Truncate overflow.
            trunc!(int, uint, long)(val, loc);
            return new IntExpr(intType, val, loc);

        // FIXME: for now assume these are fine.
        case "l", "L":  return new IntExpr(longType, val, loc);
        case "u", "U":  return new IntExpr(uintType, val, loc);

        case "ul", "Ul", "uL", "UL":  return new IntExpr(ulongType, val, loc);
        case "ll", "lL", "Ll", "LL":  return new IntExpr(llongType, val, loc);
        case "ull", "Ull", "ULl", "UlL", "uLL", "ULL":
            return new IntExpr(ullongType, val, loc);

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