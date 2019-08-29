///     This file contains type information for C.
///     Copyright 2019 Yiyong Li.

module parser.ctypes;

import std.array;
import parser.lexer;

/// C type constructor. Used in AST.
class Type
{
    alias Kind = int;

    /// Specifiers.
    enum : Kind {
        VOID,
        BOOL_,
        CHAR,
        SHORT,
        INT,
        LONG,
        LLONG,  // long long
        FLOAT,
        DOUBLE,
        ARRAY,
        ENUM,   // TODO.
        PTR,
        STRUCT,
        UNION,
        FUNC,
    }

    struct ArrayInfo
    {
        /// Elem type.
        Type type;
        /// Array length.
        size_t length;
    }

    struct FuncInfo
    {
        /// Return type.
        Type retType;
        /// Types of params.
        Type[] params;
    }

    /// Struct or union.
    struct StructInfo
    {
        /// Member types.
        Type[] types;
        /// Corresponding offsets.
        size_t[] offsets;
    }

    struct PtrInfo
    {
        /// Pointee type.
        Type type;
    }

    /// Size in bytes.
    size_t size = 0;

    /// Unsigned.
    bool unsig = false;

    /// Static.
    bool stat = false;

    /// Tag.
    Kind kind;
    union {
        /// ARRAY.
        ArrayInfo asArr;
        /// PTR.
        PtrInfo asPtr;
        /// STRUCT or UNION.
        StructInfo asStruct;
        /// FUNC.
        FuncInfo asFunc;
    }

    /// void.
    this() {
        kind = VOID;
    }

    /// Numberic.
    this(Kind kind, size_t size, bool unsig = false)
    {
        assert(
            kind == BOOL_   ||
            kind == CHAR    ||
            kind == SHORT   ||
            kind == INT     ||
            kind == LONG    ||
            kind == LLONG   ||
            kind == FLOAT   ||
            kind == DOUBLE  ||
            kind == ENUM
        );

        this.kind = kind;
        this.size = size;
        this.unsig = unsig;
    }

    /// Pointer or array.
    this(Kind kind, Type type, size_t len = 0)
    {
        assert(kind == ARRAY || kind == PTR);

        this.kind = kind;
        if (kind == ARRAY)
        {
            this.asArr = ArrayInfo(type, len);
            this.size = type.size * len;
        }

        else
        {
            this.asPtr = PtrInfo(type);
            this.size = size_t.sizeof;
        }
    }

    /// Function.
    this(Kind kind, Type retType, Type[] params)
    {
        assert(kind == FUNC);

        this.kind = kind;
        this.asFunc = FuncInfo(retType, params);

        // Func name is a ptr.
        this.size = size_t.sizeof;
    }

    /// Struct or union
    this(Kind kind, Type[] types)
    {
        assert(kind == STRUCT || kind == UNION);

        this.kind = kind;
        this.asStruct = StructInfo(
            types,
            kind == UNION ? null : types.memberOffsets
        );
    }
}

/// Calculates the offsets of each member in a struct.
size_t[] memberOffsets(Type[] types)
{
    auto offsets = appender!(size_t[]);
    offsets.reserve(10);

    // Find the max member size.
    size_t max = 0;
    foreach (t; types)
    {
        if (t.size > max)
            max = t.size;
    }

    // Assign offsets.
    size_t currOffset = 0;
    for (size_t i = 0; i < types.length;)
    {
        size_t acc = 0;
        for (; acc + types[i].size < max; i++)
        {
            offsets.put(currOffset);
            currOffset += types[i].size;
            acc += types[i].size;
        }

        // Add padding.
        if (acc < max)
        {
            currOffset += max - acc;
        }
    }

    return offsets.data;
}

/// Primitive types.
const Type voidType = new Type();
const Type boolType = new Type(Type.BOOL_, 1);
const Type charType = new Type(Type.CHAR, 1);
const Type shortType = new Type(Type.SHORT, 2);
const Type intType = new Type(Type.INT, 4);
const Type longType = new Type(Type.LONG, 8);
const Type llongType = new Type(Type.LLONG, 8);
const Type ucharType = new Type(Type.CHAR, 1, true);
const Type ushortType = new Type(Type.SHORT, 2, true);
const Type uintType = new Type(Type.INT, 4, true);
const Type ulongType = new Type(Type.LONG, 8, true);
const Type ullongType = new Type(Type.LLONG, 8, true);
const Type floatType = new Type(Type.FLOAT, 4);
const Type doubleType = new Type(Type.DOUBLE, 8);
const Type enumType = new Type(Type.ENUM, 4);

unittest {
}