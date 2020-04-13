///     This file contains a simple AST dumper.
///     Copyright 2019 Yiyong Li.

module parser.ast_dumper;

import std.stdio;
import std.format;
import parser.ast;
import parser.types;
debug import reporter;

/// Number of identations.
private uint indents = 0;

private const BOLD_TEXT = "\033[1m";
private const COLOR_RES = "\033[0m";
private const COLOR_RED = "\033[31m";
private const COLOR_PURPLE = "\033[35m";
private const COLOR_CYN = "\033[96m";
private const COLOR_GRN = "\033[0;32m";
private const COLOR_YEL = "\033[0;33m";
private const COLOR_BLUE = "\033[0;34m";

private string getIndents()
{
    string str;
    foreach (i; 0 .. indents)
    {
        str ~= "\t";
    }
    return str;
}

/// Dump Exprs.
void dumpExpr(Expr[] nodes)
{
    foreach (node; nodes)
    {
        dumpExpr(node);
    }
}

/// Dump Exprs.
void dumpExpr(Expr node)
{
    string litExprStr(T)(T expr)
    {
        return format!"%s%s%s(%s, %s)"(
            COLOR_BLUE,
            T.stringof,
            COLOR_RES,
            expr.type,
            expr.value
        );
    }

    auto exprstr = getIndents() ~ BOLD_TEXT;

    /*
    Literals.
    */
    if (auto intexpr = cast(IntExpr)node)
    {
        exprstr ~= litExprStr(intexpr);
        writeln(exprstr);
    }

    else if (auto fexpr = cast(FloatExpr)node)
    {
        exprstr ~= litExprStr(fexpr);
        writeln(exprstr);
    }

    else if (auto sexpr = cast(StringExpr)node)
    {
        exprstr ~= litExprStr(sexpr);
        writeln(exprstr);
    }

    /*
    Primary, Unary, Binary, CondExpr.
    */
    else if (auto ident = cast(IdentExpr)node)
    {
        exprstr ~= format!"%sIdentExpr%s(%s, %s)"(
            COLOR_GRN,
            COLOR_RES,
            ident.type,
            ident.identStr
        );
        writeln(exprstr);
    }

    else if (auto unary = cast(UnaryExpr)node)
    {
        exprstr ~= format!"%sUnaryExpr%s(%s, %s)"(
            COLOR_PURPLE,
            COLOR_RES,
            unary.type,
            unary.opstring()
        );
        indents++;
        writeln(exprstr);
        dumpExpr(unary.opnd);
        indents--;
    }

    else if (auto bin = cast(BinExpr)node)
    {
        exprstr ~= format!"%sBinExpr%s(%s, %s)"(
            COLOR_CYN,
            COLOR_RES,
            bin.type,
            bin.op
        );
        indents++;
        writeln(exprstr);
        dumpExpr(bin.lhs);
        dumpExpr(bin.rhs);
        indents--;
    }

    else if (auto cond = cast(CondExpr)node)
    {
        exprstr ~= format!"%sCondExpr%s"(
            COLOR_PURPLE,
            COLOR_RES,
        );
        indents++;
        writeln(exprstr);
        dumpExpr(cond.cond);
        dumpExpr(cond.first);
        dumpExpr(cond.sec);
        indents--;
    }

    /*
    MemberExpr and CallExpr.
    */
    else if (auto mem = cast(MemberExpr)node)
    {
        exprstr ~= format!"%sMemberExpr%s(%s)"(
            COLOR_YEL,
            COLOR_RES,
            mem.type
        );
        indents++;
        writeln(exprstr);
        dumpExpr(mem.struc);
        writeln(getIndents() ~ mem.name);
        indents--;
    }

    else if (auto call = cast(CallExpr)node)
    {
        exprstr ~= format!"%sCallExpr%s(%s)"(
            COLOR_RED,
            COLOR_RES,
            call.type
        );
        indents++;
        writeln(exprstr);
        dumpExpr(call.callee);
        dumpExpr(call.args);
        indents--;
    }

    /*
    Init and Comp
    */
    else if (auto init = cast(InitExpr)node)
    {
        exprstr ~= format!"%sInitExpr%s(%s)"(
            COLOR_CYN,
            COLOR_RES,
            init.type,
        );
        indents++;
        writeln(exprstr);
        writeln(getIndents() ~ format!"%s"(init.offset));
        dumpExpr(init.value);
        indents--;
    }

    else if (auto comp = cast(CompLitExpr)node)
    {
        exprstr ~= format!"%sCompLitExpr%s(%s)"(
            COLOR_CYN,
            COLOR_RES,
            comp.type,
        );
        indents++;
        writeln(exprstr);

        InitExpr init = comp.inits;
        while (init !is null)
        {
            dumpExpr(init);
            init = init.next;
        }
        indents--;
    }

    /*
    AssignExpr
    */
    else if (auto assign = cast(AssignExpr)node)
    {
        exprstr ~= format!"%sAssignExpr%s(%s, %s)"(
            COLOR_PURPLE,
            COLOR_RES,
            assign.type,
            assign.op
        );
        indents++;
        writeln(exprstr);
        dumpExpr(assign.lhs);
        dumpExpr(assign.rhs);
        indents--;
    }

    else if (auto comexpr = cast(CommaExpr)node)
    {
        exprstr ~= format!"%sCommaExpr%s(%s)"(
            COLOR_YEL,
            COLOR_RES,
            comexpr.type
        );
        indents++;
        writeln(exprstr);
        dumpExpr(comexpr.lists);
        indents--;
    }
    else
    {
        assert(false, "Non-Expr node");
    }
}