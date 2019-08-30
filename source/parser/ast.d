///     This file contains AST node types.
///     Copyright 2019 Yiyong Li.

module parser.ast;

import std.format;
import std.conv;
import parser.ctypes;
import parser.lexer;

/// Represents the span of an AST node in the source code.
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
    /// Type of the expression.
    Type type;
    
    /// Constructor.
    this(SrcSpan span)
    {
        super(span);
    }
}

/// Represents an integer literal.
class IntExpr : Expr
{
    /// Constant value.
    long value;

    /// Integer.
    this(Type type, long val, SrcSpan span)
    {
        assert(
            type == shortType   ||
            type == intType     ||
            type == longType    ||
            type == llongType   ||
            type == ushortType  ||
            type == uintType    ||
            type == ulongType   ||
            type == ullongType
        );

        super(type, span);
        this.value = val;
    }

    override string toString() const
    {
        return to!string(value);
    }
}

/// Represents FP literal.
class FloatExpr : Expr
{
    /// Constant value.
    double value;

    /// Constructor.
    this(Type type, double val, SrcSpan span)
    {
        assert(type == floatType || type == doubleType);

        super(type, span);
        this.value = val;
    }

    override string toString() const
    {
        return to!string(value);
    }
}

/// Represents a string literal.
class StringExpr : Expr
{
    /// Constant value.
    wstring value;

    /// Constructor.
    this(wstring val, SrcSpan span)
    {
        // String literal is const char*
        super(new Type(Type.PTR, charType), span);
        this.value = val;
    }

    override string toString() const
    {
        return to!string(value);
    }
}

/// Represents an identifier.
class IdentExpr : Expr
{
    /// Name of the identifier.
    wstring name;

    /// Constructor
    this(Type type, wstring name, SrcSpan span)
    {
        assert(name !is null);
        super(type, span);
        this.name = name;
    }

    override string toString() const
    {
        return to!string(name);
    }
}

// Postfix operator.

/// Represents a function call.
class CallExpr : Expr
{
    /// Expr that represents the callee.
    wstring callee;

    /// Args passed to this call.
    Expr[] args;

    /// Constructor.
    /// [funcType] - A function type.
    /// [callee] - Func name.
    this(Type funcType, wstring callee, Expr[] args, SrcSpan span)
    {
        assert(funcType.kind == Type.FUNC);
        assert(callee !is null);

        super(funcType, span);
        this.callee = callee;
        this.args = args;
    }

    override string toString() const
    {
        return this.type.toString ~ "()";
    }
}

/// Represents a subscript access.
class SubExpr : Expr
{
    /// Node that represents array.
    Expr array;

    /// Node that represents index.
    Expr index;

    /// Constructor.
    this(Expr array, Expr index, SrcSpan span)
    {
        assert(array !is null);
        assert(index !is null);

        // Array access.
        if (array.type.kind == Type.ARRAY)
        {
            super(array.type.asArr.type, span);
        }

        // Pointer access.
        else if (array.type.kind == Type.PTR)
        {
            super(array.type.asPtr.type, span);
        }

        // Semantic error.
        else
        {
            assert(false, "Subscript on non-pointer type");
        }

        this.array = array;
        this.index = index;
    }
}

/// Represents an object member access.
class ObjMember : Expr
{
    /// Node that represents the object.
    Expr obj;

    /// Member name.
    wstring name;

    /// Constructor
    this(Expr obj, wstring name, SrcSpan span)
    {
        assert(obj !is null);
        assert(name !is null);
        assert(
            obj.type.kind == Type.STRUCT ||
            obj.type.kind == Type.UNION,
            "Type of `obj` should be a struct or union"
        );

        
    }
}

/// Represents a cast expression. E.g. (int*)malloc(4);
class CastExpr : Expr
{
    /// Operand of the cast.
    Expr base;

    /// Constructor.
    /// [toType] - Type to cast to.
    /// [base] - Operand.
    this(Type toType, Expr base, SrcSpan span)
    {
        assert(base !is null);

        super(toType, span);
        this.base = base;
    }

    override string toString() const
    {
        return "(" ~ this.type.toString ~ ")" ~ base.toString;
    }
}

/// Represents a sizeof expression. E.g. sizeof(int);
class SizeofExpr : Expr
{
    /// Whether the sizeof is applied to an expression.
    bool onExpr;

    /// Operand.
    union {
        /// E.g. sizeof int
        Type t;

        /// E.g. sizeof 5
        Expr opnd;
    }

    /// Expr.
    this(Expr opnd, SrcSpan span)
    {
        super(ulongType, span);
        this.onExpr = true;
        this.opnd = opnd;
    }

    /// Type.
    this(Type t, SrcSpan span)
    {
        super(ulongType, span);
        this.onExpr = false;
        this.t = t;
    }

    override string toString() const
    {
        if (onExpr)
            return "sizeof " ~ this.opnd.toString;
        else
            return "sizeof " ~ this.t;
    }
}

/// Unary expression with operator +, -, ~, ++ and --.
/// ++ and -- can be prefix or postfix.
class ArithUnaryExpr : Expr
{
    /// Prefix operator.
    bool prefix;

    /// Operator.
    wstring op;

    /// Operand. Operand should be of integer type.
    Expr opnd;

    /// Constructor.
    /// FIXME: + and - are not postfix operators.
    this(bool prefix, wstring op, Expr opnd, SrcSpan span)
    {
        assert(opnd !is null);
        assert(
            op == "+" || 
            op == "-" || 
            op == '~' ||
            op == "++" ||
            op == "--"
        );

        super(opnd.type, span);
        this.prefix = prefix;
        this.op = op;
        this.opnd = opnd;
    }

    override string toString() const
    {
        return (prefix
            ? to!string(op) ~ opnd.toString
            : opnd.toString ~ to!string(op)
        );
    }
}