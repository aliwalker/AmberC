///     This file contains semantic utilities
///     Copyright 2019 Yiyong Li.

module parser.sema;

import std.format;
import parser.ctypes;
import parser.ast;

/// Environment. Roughly the same as "scope".
class Env
{
    /// Enclosing env. Each env except the global env
    /// has a parent.
    Env parent;

    /// Variable declarations within this env.
    VarDecl[string] names;

    /// Constructor.
    this(Env parent) {
        this.parent = parent;
    }
}

/// A chain of Env objects. 
Env envs;
static this()
{
    envs = new Env(null);
}

/// TODO.
private void semaError(string msg, SrcLoc loc)
{

}

/// Resolve [name] from environments and return
/// the declaration that corresponds to it. Return
/// null if [name] is not found. 
VarDecl resolveName(string name, out bool local)
{
    auto curenv = envs;

    while (curenv !is null)
    {
        if (name in curenv)
        {
            local = curenv.parent != null;
            return curenv[name];
        }

        curenv = curenv.parent;
    }
    return null;
}

/// Semantic action on identier. 
IdentExpr semaIdent(string name, SrcLoc loc)
{
    auto local = true;
    auto decl = resolveName(name, local);

    if (decl !is null)
    {
        semaError(format!"name '%s' cannot resolve!"(name), loc);
    }
    return new IdentExpr(decl.type, decl, !local, loc);
}

/// Semantic action on int literal.
IntExpr semaInt(long val, string sfx, SrcLoc loc)
{
    switch (sfx)
    {
        case "":       return new IntExpr(intType, val, loc);
        case "l", "L": return new IntExpr(longType, val, loc);
        case "u", "U": return new IntExpr(uintType, val, loc);
        case "ul", "Ul", "uL", "UL":  return new IntExpr(ulongType, val, loc);
        case "ll", "lL", "Ll", "LL":  return new IntExpr(llongType, val, loc);
        case "ull", "Ull", "ULl", "UlL", "uLL", "ULL":
            return new IntExpr(ullongType, val, loc);
        default:
            semaError(format!"Unknown suffix for integer: %s"(val), loc);
    }
}