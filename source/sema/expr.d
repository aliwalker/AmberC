///     This file contains semantic utilities
///     Copyright 2019 Yiyong Li.

module sema.expr;

import std.format;
import parser.ctypes;
import parser.ast;
import sema.env;

private const int SVR_ERR = 1;
private const int SVR_WARN = 2;
private const int SVR_INFO = 3; 
private void semaReport(int svr, string msg, SrcLoc loc)
{
    // TODO.
}

/// Semantic action on identier. 
IdentExpr semaIdent(string name, SrcLoc loc)
{
    auto decl = envResolv(name);

    if (decl !is null)
    {
        semaReport(SVR_ERR, format!"name '%s' cannot resolve!"(name), loc);
    }
    return new IdentExpr(decl, loc);
}

/// Truncate [val] if it overflows.
private void trunc(T, N)(T min, T max, ref N val, SrcLoc loc)
{
    static assert(__traits(isIntegral, T));
    static assert(__traits(isIntegral, N));

    if (val > min || val < max)
    {
        semaReport(
            SVR_WARN, 
            format!"%s gets truncated to %s"(val, max & val),
            loc
        );
        val = max & val;
    }
}

/// Semantic action on int literal.
IntExpr semaInt(long val, string sfx, SrcLoc loc)
{
    switch (sfx)
    {
        case "":
            // Truncate overflow.
            trunc!(int, long)(int.min, int.max, val, loc);
            return new IntExpr(intType, val, loc);

        // FIXME: for now assume these are fine.
        case "l", "L":  return new IntExpr(longType, val, loc);
        case "u", "U":  return new IntExpr(uintType, val, loc);

        case "ul", "Ul", "uL", "UL":  return new IntExpr(ulongType, val, loc);
        case "ll", "lL", "Ll", "LL":  return new IntExpr(llongType, val, loc);
        case "ull", "Ull", "ULl", "UlL", "uLL", "ULL":
            return new IntExpr(ullongType, val, loc);

        default:
            semaReport(
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
            semaReport(
                SVR_ERR,
                format!"Unknown suffix for floating literal: %s"(val),
                loc
            );

            return null;
    }
}
