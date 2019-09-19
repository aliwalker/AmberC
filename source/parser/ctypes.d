///     This file contains C type related utilities.
///     Copyright 2019 Yiyong Li.

module parser.ctypes;

import std.stdio;
import std.stdint;
import std.array;
import std.format;

/// Size of a pointer.
const PTR_SIZE = 8;

/// Const qualifier.
const uint8_t QUAL_CONST = 1;
/// Register qualifier.
const uint8_t QUAL_REG = 2;

/// Base type.
class Type
{
    alias Kind = int;

    /// Specifiers.
    enum : Kind {
        VOID = 1,
        BOOL_,
        CHAR,
        SHORT,
        INT,
        LONG,
        LLONG,
        FLOAT,
        DOUBLE,
        // ENUM,
        /// Derived types:
        /// array, record, function, ptr.
        DERV,
    }
    Kind kind;

    /// Constructor for a base type in C.
    this(Kind kind)
    {
        assert(
            kind == VOID    ||
            kind == BOOL_   ||
            kind == CHAR    ||
            kind == SHORT   ||
            kind == INT     ||
            kind == LONG    ||
            kind == LLONG   ||
            kind == FLOAT   ||
            kind == DOUBLE  ||
            // kind == ENUM    ||
            kind == DERV
        );
        this.kind = kind;
    }

    /// C's sizeof operator.
    ulong typeSize() const
    {
        switch (kind)
        {
            case VOID:     return 0;
            case BOOL_:    return 1;
            case CHAR:     return 1;
            case SHORT:    return 2;
            case INT:      return 4;
            case LONG:     return 8;
            case LLONG:    return 8;
            case FLOAT:    return 4;
            case DOUBLE:   return 8;
            default:
                return -1;
        }
    }

    override size_t toHash() const
    {
        return cast(size_t)kind;
    }

    override bool opEquals(Object other) const
    {
        auto otherTy = cast(Type)other;
        
        if (otherTy is null)
            return false;

        return kind == otherTy.kind;
    }

    override string toString() const
    {
        switch (kind)
        {
            case VOID:      return "void";
            case BOOL_:     return "Bool_";
            case CHAR:      return "char";
            case SHORT:     return "short";
            case INT:       return "int";
            case LONG:      return "long";
            case LLONG:     return "long long";
            case FLOAT:     return "float";
            case DOUBLE:    return "double";
            // case ENUM:
            case DERV:      return "derived";
            default:
                return "";
        }
    }
}

/// Struct or Union type
class RecType : Type
{
    /// Name of the struct.
    string name;

    /// Field type.
    struct Field {
        /// Type of the field.
        Type type;

        /// Name of the field.
        string name;

        /// Offset from the beginning address.
        size_t offset;
    }

    /// Whether this is a union type.
    bool isUnion;

    /// Member names and types.
    Field[] members;

    /// Constructor.
    this(string name, Field[] members, bool isUnion = false)
    {
        super(DERV);

        this.name = name;
        this.members = members;
        this.isUnion = isUnion;
    }

    override ulong typeSize() const
    {
        if (members is null)
            return -1;

        if (members.length == 0)
            return 0;

        if (isUnion)
        {
            ulong max = 0;
            foreach (m; members)
            {
                if (m.type.typeSize > max)
                    max = m.type.typeSize;
            }
            return max;
        }

        auto lmember = members[$ - 1];
        return lmember.offset + lmember.type.typeSize();
    }

    override size_t toHash() const
    {
        size_t hash = 1;
        
        foreach (m; members)
        {
            hash *= m.type.toHash;
        }
        return hash;
    }

    override bool opEquals(Object other) const
    {
        auto rec = cast(RecType)other;

        if (rec is null)
            return false;

        if (name != rec.name)
            return false;

        foreach (i, m; members)
        {
            if (m != rec.members[i])
                return false;
        }
        return true;
    }

    override string toString() const
    {
        string tystr = (isUnion ? "union(" : "struct(") ~ name ~ ")(";

        foreach (i, m; members)
        {
            tystr ~= (i == members.length - 1)
                ? m.type.toString
                : m.type.toString ~ ",";
        }
        return tystr ~ ")";
    }
}

/// Function type.
class FuncType : Type
{
    /// Return type.
    Type retType;

    /// Param types.
    Type[] params;

    /// Constructor.
    this(Type retType, Type[] params)
    {
        super(DERV);

        this.retType = retType;
        this.params = params;
    }

    override ulong typeSize() const
    {
        return PTR_SIZE;
    }

    override size_t toHash() const
    {
        size_t hash = retType.toHash();
        
        foreach (p; params)
        {
            hash *= p.toHash();
        }
        return hash;
    }

    override bool opEquals(Object other) const
    {
        auto func = cast(FuncType)other;

        if (func is null)
            return false;

        if (func.retType != retType)
            return false;

        foreach (i, p; params)
        {
            if (func.params[i] != p)
                return false;
        }

        return true;
    }

    override string toString() const
    {
        string tystr = retType.toString() ~ "(*)(";

        foreach (i, p; params)
        {
            tystr ~= (i == (params.length - 1))
                ? p.toString()
                : p.toString() ~ ",";
        }
        return tystr ~ ")";
    }
}

/// Array type.
class ArrayType : Type
{
    /// Type of the element.
    Type elemTy;

    /// Size of the array.
    size_t size;

    /// Constructor.
    this(Type elemTy, size_t size)
    {
        super(DERV);

        this.elemTy = elemTy;
        this.size = size;
    }

    override ulong typeSize() const
    {
        return elemTy.typeSize() * size;
    }

    override size_t toHash() const
    {
        return elemTy.toHash() * size;
    }

    override bool opEquals(Object other) const
    {
        auto arr = cast(ArrayType)other;

        if (arr is null)
            return false;

        if ((elemTy != arr.elemTy) || (size != size))
            return false;

        return true;
    }

    override string toString() const
    {
        return format!"%s[%s]"(elemTy.toString, size);
    }
}

/// Pointer type.
class PtrType : Type
{
    /// Pointee type.
    Type base;

    /// Constructor.
    this(Type base)
    {
        super(DERV);

        this.base = base;
    }

    override ulong typeSize() const
    {
        return PTR_SIZE;
    }

    override size_t toHash() const
    {
        return base.toHash() * DERV;
    }

    override bool opEquals(Object other) const
    {
        auto ptr = cast(PtrType)other;

        if (ptr is null)
            return false;

        if (base != ptr.base)
            return false;

        return true;
    }

    override string toString() const
    {
        return base.toString() ~ "*";
    }
}

/// Primitive types
__gshared Type voidType   = new Type(Type.VOID);
__gshared Type boolType   = new Type(Type.BOOL_);
__gshared Type charType   = new Type(Type.CHAR);
__gshared Type shortType  = new Type(Type.SHORT);
__gshared Type intType    = new Type(Type.INT);
__gshared Type longType   = new Type(Type.LONG);
__gshared Type llongType  = new Type(Type.LLONG);
__gshared Type ucharType  = new Type(Type.CHAR);
__gshared Type ushortType = new Type(Type.SHORT);
__gshared Type uintType   = new Type(Type.INT);
__gshared Type ulongType  = new Type(Type.LONG);
__gshared Type ullongType = new Type(Type.LLONG);
__gshared Type floatType  = new Type(Type.FLOAT);
__gshared Type doubleType = new Type(Type.DOUBLE);

/// Type stores.
private FuncType[string] functypes;
private ArrayType[string] arrayTypes;
private PtrType[string] ptrTypes;

/// Helper for iterating record fields.
/// [funct] accepts as params the type of the field, 
/// and the name of the field.
private void iterFields(T...)(
    void delegate(typeof(T[0]), typeof(T[1])) funct
)
{
    static assert((T.length & 0x1) == 0);

    foreach (i, t; T)
    {
        static if ((i & 0x1) == 0)
        {
            static assert(is (typeof(t) : Type), "Expect \"Type\"");
            static assert(is (typeof(T[i + 1]) == string), "Expect field name as a string");

            funct(t, T[i + 1]);
        }
        else
        {
            // Do nothing.
        }
    }
}

/// Creates and returns a RecType.
/// Example usage:
///
/// auto fooStrucType = makeStrucType!(
///     intType, "foo",
///     longType, "bar"
/// )("fooStrucType");
///
/// This function calculates alignments and assigns
/// an offset to each member field.
RecType makeStrucType(T...)(string strucName)
{
    // Field constructor.
    alias F = RecType.Field;

    // Find alignment.
    ulong alig = 0;
    iterFields!T((t, _)
    {
        if (t.typeSize() > alig)
            alig = t.typeSize();
    });

    // Construct fields.
    auto fields = appender!(F[]);
    ulong offset = 0;
    ulong size = 0;
    iterFields!T((t, name)
    {
        if (size + t.typeSize() < alig)
        {
            size += t.typeSize();
        }

        // Clear size.
        else if (size + t.typeSize() == alig)
        {
            size = 0;
        }

        // Add padding.
        else
        {
            offset += alig - size;
            size = t.typeSize();
        }

        fields.put(F(t, name, offset));
        offset += t.typeSize();
    });

    return new RecType(strucName, fields.data);
}

/// Same as makeStrucType, for union type.
RecType makeUnionType(T...)(string unionName)
{
    // Field constructor.
    alias F = RecType.Field;
    auto fields = appender!(F[]);

    iterFields!T((t, name)
    {
        fields.put(F(t, name, 0));
    });

    return new RecType(unionName, fields.data, true);
}

unittest
{
    RecType fooStrucType = makeStrucType!(
        intType, "foo",
        longType, "bar"
    )("fooStruc");

    assert(fooStrucType.isUnion == false);
    assert(fooStrucType.typeSize == 16);
    assert(fooStrucType.members[0].type == intType);
    assert(fooStrucType.members[0].offset == 0);
    assert(fooStrucType.members[0].name == "foo");
    assert(fooStrucType.members[1].type == longType);
    assert(fooStrucType.members[1].offset == 8);
    assert(fooStrucType.members[1].name == "bar");

    RecType barUnionType = makeUnionType!(
        longType, "foo",
        intType, "bar"
    )("barUnion");
    assert(barUnionType.isUnion);
    assert(barUnionType.typeSize == 8);
    assert(barUnionType.members[0].type == longType);
    assert(barUnionType.members[0].offset == 0);
    assert(barUnionType.members[1].type == intType);
    assert(barUnionType.members[1].offset == 0);
}

unittest
{
    /// These should be equal.
    Type intPtr = new PtrType(intType);
    Type intPtr2 = new PtrType(intType);
    assert(intPtr == intPtr2);
    assert(intPtr.toHash == intPtr2.toHash);

    Type longPtr = new PtrType(longType);
    Type struc = new RecType("fooStruc", [
        RecType.Field(longPtr, "foo", 0),
        RecType.Field(intType, "bar", 8),
    ]);
    Type struc2 = new RecType("fooStruc", [
        RecType.Field(longPtr, "foo", 0),
        RecType.Field(intType, "bar", 8),
    ]);
    Type struc3 = new RecType("foo2Struc", [
        RecType.Field(longPtr, "foo2", 0),
        RecType.Field(intType, "bar2", 8),
    ]);
    assert(struc == struc2);
    assert(struc.toHash() == struc2.toHash());
    assert(struc3 != struc);
    assert(struc.toHash() == struc3.toHash());

    Type intarr = new ArrayType(intType, 10);
    Type intarr2 = new ArrayType(intType, 10);
    assert(intarr == intarr2);
    assert(intarr.toHash() == intarr2.toHash());
    static assert(is (typeof(intPtr) : Type));
}

/// Test toString
unittest
{
    assert(intType.toString == "int");

    /// PtrType.
    auto intPtrTy = new PtrType(intType);
    assert(intPtrTy.toString == "int*");
    auto intPtrPtrTy = new PtrType(intPtrTy);
    assert(intPtrPtrTy.toString == "int**");

    /// FuncType.
    auto funcTy = new FuncType(voidType, [intPtrTy]);
    assert(funcTy.toString == "void(*)(int*)");

    /// RecType.
    auto fooStrucTy = makeStrucType!(
        intType, "foo",
        longType, "bar"
    )("fooStruc");
    assert(fooStrucTy.toString == "struct(fooStruc)(int,long)");
    auto barUnionTy = makeUnionType!(
        intType, "foo",
        longType, "bar"
    )("barUnion");
    assert(barUnionTy.toString == "union(barUnion)(int,long)");
}