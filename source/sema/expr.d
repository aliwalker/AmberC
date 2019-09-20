///     This file contains semantic utilities
///     Copyright 2019 Yiyong Li.

module sema.expr;

import std.format;
import parser.types;
import parser.ast;
import sema.env;
import reporter;

/// Semantic action on array access.
UnaryExpr semaArrayDeref(Expr base, Expr idx)
{
    auto arrayTy = cast(ArrayType)(base.type);
    auto ptrTy = cast(PtrType)(base.type);
    Type elemTy;

    // When base is of ArrayType, decay array to pointer.
    if (arrayTy !is null)
    {
        auto elemPtrTy = getPtrType(arrayTy.elemTy);

        elemTy = arrayTy.elemTy;
        base = new UnaryExpr(
            UnaryExpr.DECAY, 
            elemPtrTy, 
            base, 
            base.loc);
    }

    else if (ptrTy !is null)
    {
        elemTy = ptrTy.base;
    }

    else
    {
        report(
            SVR_ERR,
            "subscripted value is not an array or pointer",
            idx.loc,
        );
    }

    if (!isInteger(idx))
    {
        report(
            SVR_ERR,
            "array subscript is not an integer",
            idx.loc
        );
    }

    // TODO: semaBin
    auto addrExpr = new BinExpr(
        base.type, 
        "+",
        base,
        idx,
        base.loc
    );

    return new UnaryExpr(
        UnaryExpr.DEREF,
        elemTy,
        addrExpr,
        addrExpr.loc
    );
}

/// Semantic action on call expression.
CallExpr semaCall(Expr callee, Expr[] args)
{

}

/// Semantic action on identier. 
IdentExpr semaIdent(string name, SrcLoc loc)
{
    auto decl = envResolv(name);
    auto ident = new IdentExpr(decl, loc);

    if (decl is null)
    {
        envUnresolv(name, ident);
    }

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
            report(
                SVR_ERR, 
                format!"Unknown suffix for integer: %s"(val), 
                loc
            );

            return null;
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
            report(
                SVR_ERR,
                format!"Unknown suffix for floating literal: %s"(val),
                loc
            );

            return null;
    }
}
