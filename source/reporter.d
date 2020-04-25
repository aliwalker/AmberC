///     This module contains message reporter.
///     Copyright 2019 Yiyong Li
module reporter;

import std.stdio;
import std.format;
import parser.ast;

/// Severity.
const int SVR_ERR = 1;
const int SVR_WARN = 2;
const int SVR_INFO = 3;

private const BOLD_TEXT = "\033[1m";
private const COLOR_RES = "\033[0m";
private const COLOR_ERR = "\033[31m";
private const COLOR_WARN = "\033[35m";
private const COLOR_INFO = "\033[96m";

/// Reports a message with [svr] severity.
void report(int svr, string msg, SrcLoc loc)
{
    assert(
        svr == SVR_ERR || 
        svr == SVR_INFO ||
        svr == SVR_WARN
    );

    // Result string.
    string locstr = BOLD_TEXT ~ loc.toString();

    switch (svr)
    {
        case SVR_ERR:   locstr ~= COLOR_ERR ~ " error: "; break;
        case SVR_WARN:  locstr ~= COLOR_WARN ~ " warning: "; break;
        case SVR_INFO:  locstr ~= COLOR_INFO ~ " info: "; break;
        default:
            assert(false, "Unknow severity!");
    }
    locstr ~= COLOR_RES;
    stderr.writeln(locstr ~ msg);
}

/*
Helpers for marking unittest.
*/

/// Called on unittest starts.
debug void uniProlog(string file = __FILE__, int line = __LINE__)
{
    stderr.writefln("\n%s==== unittest starts at %s:%s ====%s", COLOR_INFO, file, line, COLOR_RES);
}

/// Called on unittest ends.
debug void uniEpilog(string file = __FILE__, int line = __LINE__)
{
    stderr.writefln("%s==== unittest ends at %s:%s ====%s\n", COLOR_INFO, file, line, COLOR_RES);
}