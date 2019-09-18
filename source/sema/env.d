///     This module contains the environment object.
///     Copyright 2019 Yiyong Li

module sema.env;

import parser.ast;
import parser.ctypes;

/// Environment. Roughly the same as "scope".
class Env
{
    /// Enclosing env. Each env except the global env
    /// has a parent.
    Env parent;

    /// Variable declarations within this env.
    Decl[string] names;

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