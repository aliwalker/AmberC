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

/// Semantic action on RecType member access.
MemberExpr semaRecAccess(Expr lhs, string op, string ident, SrcLoc loc)
{
    assert(op == "." || op == "->");

    // The record type.
    RecType recType;

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

    // lhs is a ptr.
    if (op == "->")
    {
        auto recPtrType = cast(PtrType)(lhs.type);
        if (recPtrType is null)
        {
            return semaErrExpr!MemberExpr(
                format!"member reference type '%s' is not a pointer"(lhs.type),
                lhs.loc
            );
        }

        recType = cast(RecType)(recPtrType);
        if (recType is null)
        {
            return semaErrExpr!MemberExpr(
                format!"member reference base type '%s' is not a struct or union"(recPtrType),
                lhs.loc
            );
        }
    }
    // lhs must be of a struct or union type.
    else
    {
        recType = cast(RecType)(lhs.type);
        if (recType is null)
        {
            return semaErrExpr!MemberExpr(
                format!"member reference base type '%s' is not a struct or union"(lhs.type),
                lhs.loc
            );
        }
    }

    auto idx = findMemberIdx(recType, ident);
    if (idx == recType.members.length)
    {
        return semaErrExpr!MemberExpr(
            format!"member '%s' does not exist in type '%s'"(ident, recType),
            loc
        );
    }
    return new MemberExpr(recType.members[idx].type, lhs, op, ident, loc);
}

/// Semantic action on array access.
UnaryExpr semaArrayDeref(Expr base, Expr idx)
{
    auto arrayTy = cast(ArrayType)(base.type);
    auto ptrTy = cast(PtrType)(base.type);
    Type elemTy;

    // When base is of ArrayType, decay array to pointer.
    if (arrayTy)
    {
        auto elemPtrTy = getPtrType(arrayTy.elemTy);

        elemTy = arrayTy.elemTy;
        base = new UnaryExpr(
            UnaryExpr.DECAY, 
            elemPtrTy, 
            base, 
            base.loc);
    }

    else if (ptrTy)
    {
        elemTy = ptrTy.base;
    }

    else
    {
        return semaErrExpr!UnaryExpr(
            "subscripted value is not an array or pointer",
            idx.loc,
        );
    }

    if (!isInteger(idx.type))
    {
        return semaErrExpr!UnaryExpr(
            "array subscript is not an integer",
            idx.loc
        );
    }

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
CallExpr semaCall(Expr callee, Expr[] args, SrcLoc parenLoc)
{
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
    auto ident = new IdentExpr(decl, name, loc);

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
