///     This file contains AST node types.
///     Copyright 2019 Yiyong Li.

module parser.ast;

import parser.lexer;

/// Used in AST, represents the span of an AST node in the source code.
struct SrcSpan
{
    /// Beginning position.
    SrcPos  begin;

    /// Ending position(inclusive).
    SrcPos  end;
}

/// Base class for all AST node types.
class Node
{
    /// All nodes should have a span.
    SrcSpan span;

    /// Constructor.
    this(SrcSpan span)
    {
        this.span = span;
    }

    override string toString() const
    {
        return "Node";
    }
}

/// Base class for expressions.
class Expr : Node
{

}