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
}

/// Represents an identifier. E.g. foo, bar
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
}

/// Wrapper for unary expressions.
/// NOTE: The parser ensure the semantic correctness
/// before constructing the AST node instances.
class UnaryExpr : Expr
{
    alias Kind = int;
    enum : Kind {
        /// Array type can decay into a pointer.
        DECAY,
        /// E.g., (int*)malloc(4)
        CAST,
        /// E.g., &variable
        ADDR_OF,
        /// Represents Several kinds of derefence.
        /// E.g., *ptr, ptr[index], obj->mem.
        DEREF,
        /// E.g., -variable
        MINUS,
        /// E.g., variable++
        POST_INCR,
        /// E.g., variable--
        POST_DECR,
        /// E.g., ++variable
        PREF_INCR,
        /// E.g., --variable
        PREF_DECR,
        /// E.g., ~variable
        BIT_NOT,
        /// E.g., !foo
        BOOL_NOT,
    }

    /// Operand.
    Expr opnd;

    /// Constructor.
    this(Kind kind, Type exprTy, Expr opnd, SrcSpan span)
    {
        assert(exprTy !is null);
        assert(opnd !is null);
        
        super(exprTy, span);
        this.opnd = opnd;
    }

    override string toString() const
    {
        final switch (kind)
        {
            case DECAY:     return "decay: " ~ opnd.toString;
            case CAST:      return "(" ~ type.toString ~ ")" ~ opnd.toString;
            case ADDR_OF:   return "&" ~ opnd.toString;
            case DEREF:     return "*" ~ opnd.toString;
            case MINUS:     return "-" ~ opnd.toString;
            case POST_DECR: return opnd.toString ~ "--";
            case POST_INCR: return opnd.toString ~ "++";
            case PREF_DECR: return "--" ~ opnd.toString;
            case PREF_INCR: return "++" ~ opnd.toString;
            case BIT_NOT:   return "~" ~ opnd.toString;
            case BOOL_NOT:  return "!" ~ opnd.toString;
        }
    }
}

/// Represents a member access expression.
/// E.g. obj.foo
class MemberExpr : Expr
{
    /// The structure to reference.
    Expr struc;

    /// Member name.
    wstring name;

    /// Constructor.
    this(Expr struc, wstring name, SrcSpan span)
    {
        assert(struc !is null);
        assert(name !is null);
        assert(isStructTy(struc.type));

        super(struc.type, span);
        this.struc = struc;
        this.name = name;
    }
}

/// Represents a function call. E.g., foo();
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
}

/// Represents a binary expression. E.g., 1 + 2
class BinExpr : Expr
{
    /// Operator.
    /// Because binary operators are all infix operators,
    /// we need not use an enum.
    wstring op;

    /// Left-hand side.
    Expr lhs;

    /// Right-hand side.
    Expr rhs;

    /// Constructor.
    this(Type resType, wstring op, Expr lhs, Expr rhs, SrcSpan span)
    {
        assert(resType !is null);
        assert(op !is null);
        assert(lhs !is null && rhs !is null);

        super(resType, span);
        this.op = op;
        this.lhs = lhs;
        this.rhs = rhs;
    }
}

/// Represents an assignment.
class AssignExpr : Expr
{
    /// Operator.
    wstring op;

    /// An lvalue.
    Expr lhs;

    /// Expression on the rhs.
    Expr rhs;

    /// Constructor.
    this(Type resType, wstring op, Expr lhs, Expr rhs, SrcSpan span)
    {
        assert(lhs !is null && rhs !is null);
        assert(lhs.type == resType);
        assert(
            op == "=" ||
            op == "+=" ||
            op == "-=" ||
            op == "*=" ||
            op == "/=" ||
            op == "%="
        );

        super(resType, span);
        this.lhs = lhs;
        this.rhs = rhs;
    }
}

/// Represents a function desinator, which is an
/// expression that has a function type.
class FuncExpr : Expr
{
    /// The name of the function.
    wstring name;

    /// Constructor.
    this(Type type, wstring name, SrcSpan span)
    {
        assert(type !is null);
        assert(name !is null);
        assert(name.isFuncTy);

        super(type, span);
        this.name = name;
    }
}

/*
* Statements.
*/

/// Base class for representing statements.
class Stmt : Node
{
    /// Constructor.
    this(SrcSpan span)
    {
        super(span);
    }
}

/// Represents an expression statement.
class ExprStmt : Stmt
{
    /// The expression.
    Expr expr;

    /// Constructor.
    this(Expr expr, SrcSpan span)
    {
        super(span);
        this.expr = expr;
    }

    /// Whether it is an empty stmt.
    bool empty() const
    {
        return (expr is null);
    }
}

/// Represents an if-statement.
class IfStmt : Stmt
{
    /// Condition.
    Expr cond;

    /// Then clause.
    Stmt then;

    /// Else clause.
    Stmt else_;

    /// Constructor.
    this(Expr cond, Stmt then, Stmt else_, SrcSpan span)
    {
        super(span);

        this.cond = cond;
        this.then = then;
        this.else_ = else_;
    }
}

/// Represents a for, while or do-while statement.
class LoopStmt : Stmt
{
    alias Kind = int;
    
    /// Tag.
    enum : Kind
    {
        FOR,
        WHILE,
        DO,
    }
    Kind kind;

    /// Loop condition.
    Expr cond;

    /// Init-statement of for loop.
    Stmt init_;

    /// Increment-statement of for loop.
    Stmt incr;

    /// Body-statement.
    Stmt body;

    /// For.
    this(Kind kind, Stmt init_, Expr cond,
                 Stmt incr, Stmt body, SrcSpan span)
    {
        super(span);

        this.kind = kind;
        this.cond = cond;
        this.init_ = init;
        this.incr = incr;
        this.body = body;
    }

    /// While or do-while.
    this(Kind kind, Expr cond, Stmt body, SrcSpan span)
    {
        assert(kind == WHILE || kind == DO);
        
        this(kind, null, cond, null, body, span);
    }
}

/// Represents a break or continue statement.
class BCStmt : Stmt
{
    alias Kind = int;
    enum : Kind
    {
        /// Break stmt.
        BREAK,
        /// Continue stmt.
        CONTINUE,
    }
    /// Tag
    Kind kind;

    /// Constructor.
    this(Kind kind, SrcSpan span)
    {
        super(span);
        this.kind = kind;
    }
}

/// Represents a return-statement.
class RetStmt : Stmt
{
    /// Return value.
    Expr retValue;

    /// Constructor.
    this(Expr retValue, SrcSpan span)
    {
        super(span);
        this.retValue = retValue;
    }

    /// Constructor.
    this(SrcSpan span)
    {
        super(span);
        this.retValue = null;
    }
}

/// Represents a compound-statement.
class CompStmt : Stmt
{
    /// A collection of stmts.
    Stmt[] body;

    /// Constructor.
    this(Stmt[] stmts, SrcSpan)
    {
        super(span);
        this.body = stmts;
    }
}

// TODO:
// class SwitchStmt : Stmt
