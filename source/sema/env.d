///     This module contains the environment object.
///     Copyright 2019 Yiyong Li

module sema.env;

import std.format;
import parser.ast;
import parser.types;
import reporter;

/// Environment. Roughly the same as "scope".
class Env
{
    /// Enclosing env. Each env except the global env
    /// has a parent.
    Env parent;

    /// Declarations within this env.
    Decl[string] names;

    /// Unresolved identifiers.
    IdentExpr[string] unresolved;

    /// Constructor.
    this(Env parent) {
        this.parent = parent;
    }
}

/// Global environment.
private __gshared Env glenv = new Env(null);
/// Environment chain.
private __gshared Env envs;

shared static this()
{
    envs = glenv;
}

/// Resolve [name] from [env]. Return the Decl
/// node if found; otherwise null. [local] is true
/// when [name] is resolved to a local Decl.
Decl envResolv(string name)
{
    // Start from envs.
    auto curenv = envs;

    // Search the chain.
    while (curenv !is null)
    {
        if (name in curenv.names)
        {
            return curenv.names[name];
        }

        curenv = curenv.parent;
    }
    return null;
}

/// Add a declaration to current env.
void envAddDecl(string name, Decl decl)
{
    auto curenv = envs;
    auto vardecl = cast(VarDecl)decl;
    auto fundecl = cast(FuncDecl)decl;

    // Var
    if (vardecl !is null)
    {
        if (name in curenv.names)
        {
            report(
                SVR_ERR,
                format!"redeclaration of name '%s'"(name),
                decl.loc
            );
        }
        else
        {
            curenv.names[name] = decl;
        }
    }
    // Func
    else
    {
        assert(fundecl !is null);

        // Function definitions are only allowed globally.
        if (curenv != glenv && fundecl.isDefinition)
        {
            report(
                SVR_ERR,
                "function definition is not allowed here",
                fundecl.loc
            );
        }

        auto prevdecl = envResolv(name);
        if (prevdecl !is null)
        {
            // Incompatible types.
            if (prevdecl.type != fundecl.type)
            {
                report(
                    SVR_ERR,
                    format!"conflicting types for '%s'"(name),
                    fundecl.loc
                );
            }
        }
        else
        {
            curenv.names[name] = fundecl;
        }
    }
}

/// Same as [envResolv] except that this function
/// resolves from [glenv] directly.
Decl envGResolv(string name)
{
    if (name in glenv.names)
    {
        return glenv.names[name];
    }

    return null;
}

/// Adds an unresolved global name.
void envUnresolv(string name, IdentExpr ident)
{
    glenv.unresolved[name] = ident;
}

/// Whether [name] is an unresolved name.
bool isUnresolved(string name)
{
    return (name in glenv.unresolved) is null;
}

/// Return true if [decl] is a local declaration.
bool isLocal(const Decl decl)
{
    assert(decl !is null);
    
    if (decl.name !in glenv.names)
    {
        return true;
    }

    return (glenv.names[decl.name] != decl);
}

/// Test resolv
unittest
{
    envs = new Env(envs);
    envs.names["foo"] = new Decl(intType, "foo", SrcLoc());
    auto decl = envResolv("foo");
    assert(decl !is null);
    assert(decl.isLocal);
    assert(decl.name == "foo");
    assert(decl.type == intType);
}