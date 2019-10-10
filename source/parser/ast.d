///     This file contains AST node types.
///     Copyright 2019 Yiyong Li.

module parser.ast;

import std.format;
import std.conv;
import std.stdint;
import parser.types;
import parser.lexer : SrcPos;

/// Represents the source location.
struct SrcLoc
{
    /// Location within source file.
    SrcPos  pos;
    /// Name of the source file.
    string  filename;

    string toString() const
    {
        return format!"%s:%s:%s:"(filename, pos.line, pos.col);
    }
}

/// Base class for all AST node types.
class Node
{
    /// Starting location in the source file.
    SrcLoc loc;

    /// Constructor.
    this(SrcLoc loc)
    {
        this.loc = loc;
    }
}

/// Base class for expressions.
class Expr : Node
{
    /// Type of the expression.
    Type type;
    
    /// Constructor.
    this(Type type, SrcLoc loc)
    {
        super(loc);
        this.type = type;
    }
}

/// Represents an integer literal.
class IntExpr : Expr
{
    /// Constant value.
    long value;

    /// Integer.
    this(Type type, long val, SrcLoc loc)
    {
        assert(
            type == shortType   ||
            type == intType     ||
            type == longType    ||
            type == llongType   ||
            type == ushortType  ||
            type == uintType    ||
            type == ulongType   ||
            type == ullongType  ||
            (cast(PtrType)type) // NULL constant
        );

        super(type, loc);
        this.value = val;
    }
}

/// Represents FP literal.
class FloatExpr : Expr
{
    /// Constant value.
    double value;

    /// Constructor.
    this(Type type, double val, SrcLoc loc)
    {
        assert(type == floatType || type == doubleType);

        super(type, loc);
        this.value = val;
    }
}

/// Represents a string literal.
class StringExpr : Expr
{
    /// Constant value.
    string value;

    /// Constructor.
    this(string val, SrcLoc loc)
    {
        // String literal is const char*
        super(getPtrType(charType, QUAL_CONST), loc);
        this.value = val;
    }
}

/// Represents an identifier. E.g. foo, bar
class IdentExpr : Expr
{
    /// The resolved Decl.
    Decl identDecl;

    /// Identifier string.
    string identStr;

    /// Constructor
    this(Decl identDecl, string idstr, SrcLoc loc)
    {
        assert(idstr );
        assert(identDecl );

        super(identDecl.type, loc);
        this.identStr = idstr;
        this.identDecl = identDecl;
    }

    /// Is it a function designator?
    bool isFuncDesg() const
    {
        return (cast(FuncDecl)identDecl !is null);
    }

    /// Is it a variable reference?
    bool isVaref() const
    {
        return (cast(VarDecl)identDecl !is null);
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
        /// E.g. +variable
        PLUS,
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
    Kind kind;

    /// Operand.
    Expr opnd;

    /// Constructor.
    this(Kind kind, Type exprTy, Expr opnd, SrcLoc loc)
    {
        assert(exprTy );
        assert(opnd );
        
        super(exprTy, loc);
        this.opnd = opnd;
        this.kind = kind;
    }

    override string toString()
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
    string name;

    /// Constructor.
    this(Type resType, Expr struc, string name, SrcLoc loc)
    {
        assert(resType );
        assert(struc );
        assert(name);
        assert(cast(RecType)(struc.type) );

        super(resType, loc);
        this.struc = struc;
        this.name = name;
    }
}

/// Represents a function call. E.g., foo();
class CallExpr : Expr
{
    /// Expr that represents the callee.
    Expr callee;

    /// Args passed to this call.
    Expr[] args;

    /// Constructor.
    /// [retType] - function's return type.
    /// [callee] - Func name.
    this(Type retType, Expr callee, Expr[] args, SrcLoc loc)
    {
        assert(retType );
        assert(callee );

        super(retType, loc);
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
    string op;

    /// Left-hand side.
    Expr lhs;

    /// Right-hand side.
    Expr rhs;

    /// Constructor.
    this(Type resType, string op, Expr lhs, Expr rhs, SrcLoc loc)
    {
        assert(resType );
        assert(op );
        assert(lhs  && rhs );

        super(resType, loc);
        this.op = op;
        this.lhs = lhs;
        this.rhs = rhs;
    }
}

/// Represents a conditional expression.
class CondExpr : Expr
{
    /// Condition.
    Expr cond;

    /// Value when [cond] is true.
    Expr first;

    /// Value when [cond] is false.
    Expr sec;

    /// Constructor.
    this(Type type, Expr cond, Expr first, Expr sec, SrcLoc loc)
    {
        assert(type && cond && first && sec);
        super(type, loc);

        this.cond = cond;
        this.first = first;
        this.sec = sec;
    }
}

/// Represents an assignment.
class AssignExpr : Expr
{
    /// Operator: =, +=, -=, etc.
    string op;

    /// An lvalue.
    Expr lhs;

    /// Expression on the rhs.
    Expr rhs;

    /// Constructor.
    this(Type resType, string op, Expr lhs, Expr rhs, SrcLoc loc)
    {
        assert(resType);
        assert(lhs  && rhs );
        assert(lhs.type == resType);
        assert(
            op == "="  ||
            op == "+=" ||
            op == "-=" ||
            op == "*=" ||
            op == "/=" ||
            op == "%="
        );

        super(resType, loc);
        this.lhs = lhs;
        this.rhs = rhs;
    }
}

/// Represents an non-compound initializer.
/// For example, the following declaration
///
/// int arr[] = {1, 2, 3};
///
/// has 3 InitExpr's:
/// InitExpr(value: 1, offset: 0)
/// InitExpr(value: 2, offset: 4)
/// InitExpr(value: 3, offset: 8)
class InitExpr : Expr
{
    /// Initial value.
    Expr value;

    /// Offset from the variable. Used in array.
    size_t offset;

    /// Constructor.
    this(Expr value, size_t offset, SrcLoc loc)
    {
        assert(value );

        super(value.type, loc);
        this.value = value;
        this.offset = offset;
    }
}

/// Represents a compound literal.
class CompLitExpr : Expr
{
    /// A list of innitializer.
    InitExpr[] inits;

    /// Constructor.
    this(Type litType, InitExpr[] inits, SrcLoc loc)
    {
        assert(litType);
        super(litType, loc);

        this.inits = inits;
    }
}

/*
* Statements.
*/

/// Base class for representing statements.
class Stmt : Node
{
    /// Constructor.
    this(SrcLoc loc)
    {
        super(loc);
    }
}

/// Represents an expression statement.
class ExprStmt : Stmt
{
    /// The expression.
    Expr expr;

    /// Constructor.
    this(Expr expr, SrcLoc loc)
    {
        super(loc);
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
    this(Expr cond, Stmt then, Stmt else_, SrcLoc loc)
    {
        super(loc);

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
                 Stmt incr, Stmt body, SrcLoc loc)
    {
        super(loc);

        this.kind = kind;
        this.cond = cond;
        this.init_ = init_;
        this.incr = incr;
        this.body = body;
    }

    /// While or do-while.
    this(Kind kind, Expr cond, Stmt body, SrcLoc loc)
    {
        assert(kind == WHILE || kind == DO);
        
        this(kind, null, cond, null, body, loc);
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
    this(Kind kind, SrcLoc loc)
    {
        super(loc);
        this.kind = kind;
    }
}

/// Represents a return-statement.
class RetStmt : Stmt
{
    /// Return value.
    Expr retValue;

    /// Constructor.
    this(Expr retValue, SrcLoc loc)
    {
        super(loc);
        this.retValue = retValue;
    }

    /// Constructor.
    this(SrcLoc loc)
    {
        super(loc);
        this.retValue = null;
    }
}

/// Represents a compound-statement.
class CompStmt : Stmt
{
    /// A collection of stmts.
    Stmt[] body;

    /// Constructor.
    this(Stmt[] stmts, SrcLoc loc)
    {
        super(loc);
        this.body = stmts;
    }
}

// TODO:
// class SwitchStmt : Stmt;
// class GotoStmt : Stmt;

/*
* Declaration.
*/

/// Base class that represents a declaration.
class Decl : Node
{
    /// Type of [name] being declared.
    Type type;

    /// Bound identifier.
    string name;

    /// Constructor.
    this(Type type, string name, SrcLoc loc)
    {
        assert(type );
        assert(name );

        super(loc);
        this.type = type;
        this.name = name;
    }
}

/// Represents a variable declaration.
class VarDecl : Decl
{
    /// Initial values.
    /// Use array type because this VarDecl might declare an array.
    InitExpr[] initVals;

    /// Constructor.
    this(Type type, string name, InitExpr[] initVals, SrcLoc loc)
    {
        super(type, name, loc);
        this.initVals = initVals;
    }

    /// Whether the variable being declared is of type T.
    bool isofType(T)() const
    {
        static assert(is (T : Type));

        return (cast(T)(type) !is null);
    }
}

/// Represents a function declaration.
class FuncDecl : Decl
{
    /// Statements within function body.
    Stmt[] body;

    /// Constructor.
    this(Type type, string name, Stmt[] body, SrcLoc loc)
    {
        super(type, name, loc);
        this.body = body;
    }

    /// Whether this decl contains a body.
    bool isDefinition() const
    {
        return (body !is null);
    }
}