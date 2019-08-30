///     This file contains type information for C.
///     Copyright 2019 Yiyong Li.

module parser.ctypes;

import std.array;
import std.algorithm.searching;
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
            types.memberOffsets(kind == UNION)
        );
        
        size_t max = !types.empty ? maxElement!"a.size"(types).size : 0;
        this.size = kind == UNION ? max : max * types.length;
    }

    // TODO:
    override bool opEquals(Object o) const
    {
        auto rhs = cast(const Type)o;
        return ((rhs.kind == kind) &&
                (rhs.size == size) &&
                (rhs.unsig == unsig) &&
                (rhs.stat == stat) &&
                (rhs.asArr == this.asArr) &&
                (rhs.asFunc == this.asFunc) &&
                (rhs.asPtr == this.asPtr) &&
                (rhs.asStruct == this.asStruct));
    }
}

/// Calculates the offsets of each member in a struct.
size_t[] memberOffsets(Type[] types, bool isUnion)
{
    auto offsets = appender!(size_t[]);
    offsets.reserve(10);

    // All member starts at offset 0 within a union.
    if (isUnion)
    {
        for (size_t i = 0; i < types.length; i++)
            offsets.put(0);

        return offsets.data;
    }

    // Find the max member size.
    size_t max = !types.empty ? maxElement!"a.size"(types).size : 0;

    // Assign offsets.
    size_t currOffset = 0;
    for (size_t i = 0; i < types.length;)
    {
        size_t acc = 0;
        for (; (i < types.length) &&
               (acc + types[i].size <= max); i++)
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

// TODO: 
Type voidType;
Type boolType;
Type charType;
Type shortType;
Type intType;
Type longType;
Type llongType;
Type ucharType;
Type ushortType;
Type uintType;
Type ulongType;
Type ullongType;
Type floatType;
Type doubleType;
Type enumType;

/// Primitive types.
static this()
{
    voidType   = new Type();
    boolType   = new Type(Type.BOOL_, 1);
    charType   = new Type(Type.CHAR, 1);
    shortType  = new Type(Type.SHORT, 2);
    intType    = new Type(Type.INT, 4);
    longType   = new Type(Type.LONG, 8);
    llongType  = new Type(Type.LLONG, 8);
    ucharType  = new Type(Type.CHAR, 1, true);
    ushortType = new Type(Type.SHORT, 2, true);
    uintType   = new Type(Type.INT, 4, true);
    ulongType  = new Type(Type.LONG, 8, true);
    ullongType = new Type(Type.LLONG, 8, true);
    floatType  = new Type(Type.FLOAT, 4);
    doubleType = new Type(Type.DOUBLE, 8);
    enumType   = new Type(Type.ENUM, 4);
}

unittest {
    Type fooStruct = new Type(Type.STRUCT, [boolType, intType]);
    assert(fooStruct.kind == Type.STRUCT);
    assert(fooStruct.size == 8);
    assert(fooStruct.asStruct.types.length == 2);
    assert(fooStruct.asStruct.types[0] == boolType);
    assert(fooStruct.asStruct.types[1] == intType);
    assert(fooStruct.asStruct.offsets[0] == 0);
    assert(fooStruct.asStruct.offsets[1] == 4);

    Type barUnion = new Type(Type.UNION, [shortType, longType]);
    assert(barUnion.kind == Type.UNION);
    assert(barUnion.size == 8);
    assert(barUnion.asStruct.types.length == 2);
    assert(barUnion.asStruct.types[0] == shortType);
    assert(barUnion.asStruct.types[1] == longType);
    assert(barUnion.asStruct.offsets[0] == 0);
    assert(barUnion.asStruct.offsets[1] == 0);

    Type funcT = new Type(Type.FUNC, voidType, [intType]);
    assert(funcT.kind == Type.FUNC);
    assert(funcT.asFunc.retType == voidType);
    assert(funcT.asFunc.params[0] == intType);

    Type intc = new Type(Type.INT, 4);
    assert(intc == intType);
}