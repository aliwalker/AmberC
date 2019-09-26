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

    alias ExitCallback = void delegate();
    /// Callbacks on exit of the env.
    ExitCallback[] cb;

    /// Constructor.
    this(Env parent) {
        this.parent = parent;
    }
}

/// Global environment.
private __gshared Env glenv = new Env(null);
/// Current environment.
private __gshared Env curenv;

shared static this()
{
    curenv = glenv;
}

/// Called when parser encounters a new environment.
void envPush()
{
    curenv = new Env(curenv);
}

/// Called when parser exits an environment.
void envPop()
{
    assert(curenv);
    curenv = curenv.parent;
}

/// Resolve [name] from [env]. Return the Decl
/// node if found; otherwise null. [local] is true
/// when [name] is resolved to a local Decl.
Decl envResolv(string name)
{
    // Start from curenv.
    auto env = curenv;

    // Search the chain.
    while (env !is null)
    {
        if (name in env.names)
        {
            return env.names[name];
        }

        env = env.parent;
    }
    return null;
}

/// Add a declaration to current env.
void envAddDecl(string name, Decl decl)
{
    auto env = curenv;
    auto vardecl = cast(VarDecl)decl;
    auto fundecl = cast(FuncDecl)decl;

    // Var
    if (vardecl !is null)
    {
        if (name in env.names)
        {
            report(
                SVR_ERR,
                format!"redeclaration of name '%s'"(name),
                decl.loc
            );
        }
        else
        {
            env.names[name] = decl;
        }
    }
    // Func
    else
    {
        assert(fundecl !is null);

        // Function definitions are only allowed globally.
        if (env != glenv && fundecl.isDefinition)
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
            env.names[name] = fundecl;
        }
    }
}

/// Add a callback for exit.
void envAddExitCb(Env.ExitCallback cb)
{
    curenv.cb ~= cb;
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

/// Are we currently at some local scope?
bool isLocalEnv()
{
    return curenv == glenv;
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
    uniProlog();
    curenv = new Env(curenv);
    curenv.names["foo"] = new Decl(intType, "foo", SrcLoc());
    auto decl = envResolv("foo");
    assert(decl !is null);
    assert(decl.isLocal);
    assert(decl.name == "foo");
    assert(decl.type == intType);
    uniEpilog();
}