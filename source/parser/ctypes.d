///     This file contains type information for C.
///     Copyright 2019 Yiyong Li.

module parser.ctypes;

import std.array;
import std.algorithm;
import std.typecons;
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

    alias StructMember = Tuple!(Type, "type", wstring, "name");

    /// Struct or union.
    struct StructInfo
    {
        /// StructMembers.
        StructMember[] members;

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
    private this() {
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
    this(Kind kind, StructMember[] members)
    {
        assert(kind == STRUCT || kind == UNION);
        auto types = members
            .map!(m => m.type)
            .array;

        this.kind = kind;
        this.asStruct = StructInfo(
            members,
            types.assignOffsets(kind == UNION)
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

    // TODO:
    override string toString() const
    {
        return "Type";
    }
}

/// Returns the offset of [name] within [structure].
/// Returns -1 if [name] is not a member of [structure].
size_t offsetFrom(wstring name, Type structure, bool allowPtr = true)
{
    assert(
        (structure.kind == Type.STRUCT)   ||
        (structure.kind == Type.UNION)    ||
        (structure.kind == Type.PTR && allowPtr)
    );

    // If [structure] is a struct ptr type, search that struct.
    if (structure.kind == Type.PTR && allowPtr)
    {
        return name.offsetFrom(structure.asPtr.type, false);
    }

    // A ptr cannot have any member.
    else if (structure.kind == Type.PTR)
    {
        return -1;
    }

    auto structInfo = &structure.asStruct;
    foreach (i, m; structInfo.members)
    {
        if (m.name == name)
            return structInfo.offsets[i];
    }
    return -1;
}

/// Returns the type of [name] within [structure].
/// Returns null if [name] is a not member of [structure].
Type memberType(wstring name, Type structure, bool allowPtr = true)
{
    assert(
        (structure.kind == Type.STRUCT)   || 
        (structure.kind == Type.UNION)    ||
        (structure.kind == Type.PTR && allowPtr)
    );

    // Ditto.
    if (structure.kind == Type.PTR && allowPtr)
    {
        return name.memberType(structure.asPtr.type, false);
    }

    else if (structure.kind == Type.PTR)
    {
        return null;
    }

    auto structInfo = structure.asStruct;
    foreach (m; structInfo.members)
    {
        if (m.name == name)
            return m.type;
    }

    return null;
}

/// Calculates the offsets of each member in a struct.
private size_t[] assignOffsets(Type[] types, bool isUnion)
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
    alias StructMember = Type.StructMember;
    auto fooStruct = new Type(Type.STRUCT, [
        StructMember(boolType, "foo"),
        StructMember(intType, "bar"),
    ]);

    assert(fooStruct.kind == Type.STRUCT);
    assert(fooStruct.size == 8);
    assert(fooStruct.asStruct.members.length == 2);
    assert(fooStruct.asStruct.members[0].type == boolType);
    assert(fooStruct.asStruct.members[1].type == intType);
    assert(fooStruct.asStruct.members[0].name == "foo");
    assert(fooStruct.asStruct.members[1].name == "bar");
    assert(fooStruct.asStruct.offsets[0] == 0);
    assert(fooStruct.asStruct.offsets[1] == 4);

    Type barUnion = new Type(Type.UNION, [
        StructMember(shortType, "integer"), 
        StructMember(longType, "longInteger"),
    ]);
    assert(barUnion.kind == Type.UNION);
    assert(barUnion.size == 8);
    assert(barUnion.asStruct.members.length == 2);
    assert(barUnion.asStruct.members[0].type == shortType);
    assert(barUnion.asStruct.members[1].type == longType);
    assert(barUnion.asStruct.members[0].name == "integer");
    assert(barUnion.asStruct.members[1].name == "longInteger");
    assert(barUnion.asStruct.offsets[0] == 0);
    assert(barUnion.asStruct.offsets[1] == 0);

    Type funcT = new Type(Type.FUNC, voidType, [intType]);
    assert(funcT.kind == Type.FUNC);
    assert(funcT.asFunc.retType == voidType);
    assert(funcT.asFunc.params[0] == intType);

    Type intc = new Type(Type.INT, 4);
    assert(intc == intType);

    assert(0 == "foo".offsetFrom(fooStruct));
    assert(4 == "bar".offsetFrom(fooStruct));
    assert(0 == "integer".offsetFrom(barUnion));
    assert(0 == "longInteger".offsetFrom(barUnion));

    assert(boolType == "foo".memberType(fooStruct));
    assert(intType == "bar".memberType(fooStruct));
    assert(shortType == "integer".memberType(barUnion));
    assert(longType == "longInteger".memberType(barUnion));

    Type fooStructPtrTy = new Type(Type.PTR, fooStruct);
    Type barPtrTy = new Type(Type.PTR, barUnion);
    assert(boolType == "foo".memberType(fooStructPtrTy));
    assert(intType == "bar".memberType(fooStructPtrTy));
    assert(shortType == "integer".memberType(barPtrTy));
    assert(longType == "longInteger".memberType(barPtrTy));
}