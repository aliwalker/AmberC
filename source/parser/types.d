///     This file contains C type related utilities.
///     Copyright 2019 Yiyong Li.

module parser.types;

import std.stdio;
import std.stdint;
import std.array;
import std.format;
import std.typecons;
debug import reporter;

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
        SCHAR,
        SHORT,
        INT,
        LONG,
        LLONG,
        UCHAR,
        USHORT,
        UINT,
        ULONG,
        ULLONG,
        FLOAT,
        DOUBLE,
        // ENUM,
        /// Derived types:
        /// array, record, function, ptr.
        DERV,
    }
    Kind kind;

    /// Qualifiers.
    uint8_t qual;

    /// Constructor for a base type in C.
    private this(Kind kind, uint8_t qual = 0)
    {
        assert(
            kind == VOID    ||
            kind == BOOL_   ||
            kind == CHAR    ||
            kind == SCHAR   ||
            kind == SHORT   ||
            kind == INT     ||
            kind == LONG    ||
            kind == LLONG   ||
            kind == UCHAR   ||
            kind == USHORT  ||
            kind == UINT    ||
            kind == ULONG   ||
            kind == ULLONG  ||
            kind == FLOAT   ||
            kind == DOUBLE  ||
            // kind == ENUM    ||
            kind == DERV
        );
        this.kind = kind;
        this.qual = qual;
    }

    /// C's sizeof operator.
    ulong typeSize() const
    {
        switch (kind)
        {
            case VOID:            return 0;
            case BOOL_:           return 1;
            case CHAR, SCHAR, UCHAR:     return 1;
            case SHORT, USHORT:   return 2;
            case INT, UINT:       return 4;
            case LONG, ULONG:     return 8;
            case LLONG, ULLONG:   return 8;
            case FLOAT:           return 4;
            case DOUBLE:          return 8;
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

    /// Qualifier string.
    string qualString() const
    {
        string[] quals;

        if (qual & QUAL_CONST)
        {
            quals ~= "const";
        }

        if (qual & QUAL_REG)
        {
            quals ~= "register";
        }

        if (quals.empty)
            return "";

        return quals.join(" ") ~ " ";
    }

    override string toString() const
    {
        switch (kind)
        {
            case VOID:      return qualString() ~ "void";
            case BOOL_:     return qualString() ~ "Bool_";
            case CHAR:      return qualString() ~ "char";
            case SCHAR:     return qualString() ~ "signed char";
            case SHORT:     return qualString() ~ "short";
            case INT:       return qualString() ~ "int";
            case LONG:      return qualString() ~ "long";
            case LLONG:     return qualString() ~ "long long";
            case FLOAT:     return qualString() ~ "float";
            case UCHAR:     return qualString() ~ "unsigned char";
            case USHORT:    return qualString() ~ "unsigned short";
            case UINT:      return qualString() ~ "unsigned";
            case ULONG:     return qualString() ~ "unsigned long";
            case ULLONG:    return qualString() ~ "unsigned long long";
            case DOUBLE:    return qualString() ~ "double";
            case DERV:      return qualString() ~ "derived";
            default:
                return "";
        }
    }
}

/// Signed integer?
bool isSigned(Type type)
{
    return (
        (type.kind == Type.SCHAR) ||
        (type.kind == Type.SHORT) ||
        (type.kind == Type.INT)   ||
        (type.kind == Type.LONG)  ||
        (type.kind == Type.LLONG));
}

/// Unsigned integer?
bool isUnsigned(Type type)
{
    return (
        (type.kind == Type.UCHAR) ||
        (type.kind == Type.USHORT) ||
        (type.kind == Type.UINT)   ||
        (type.kind == Type.ULONG)  ||
        (type.kind == Type.ULLONG));
}

/// Get unsigned version.
Type getUnsigned(Type type)
{
    assert(isSigned(type));

    switch (type.kind)
    {
        case Type.SCHAR:    return ucharType;
        case Type.SHORT:    return ushortType;
        case Type.INT:      return uintType;
        case Type.LONG:     return ulongType;
        case Type.LLONG:    return ullongType;
        default:
            assert(false);
    }
}

/// Get signed version.
Type getSigned(Type type)
{
    assert(isUnsigned(type));

    switch (type.kind)
    {
        case Type.UCHAR:     return scharType;
        case Type.USHORT:    return shortType;
        case Type.UINT:      return intType;
        case Type.ULONG:     return longType;
        case Type.ULLONG:    return llongType;
        default:
            assert(false);
    }
}

/// Integer?
bool isInteger(Type type)
{
    return (isSigned(type) || isUnsigned(type));
}

/// Integer rank.
uint8_t intRank(Type type)
{
    assert(isInteger(type));

    switch (type.kind)
    {
        case Type.BOOL_:                        return 0;
        case Type.CHAR, Type.SCHAR, Type.UCHAR: return 1;
        case Type.SHORT, Type.USHORT:           return 2;
        case Type.INT, Type.UINT:               return 3;
        case Type.LONG, Type.ULONG:             return 4;
        case Type.LLONG, Type.ULLONG:           return 5;
        default:
            assert(false);
    }
}

/// Max value of an integer type.
long intMaxVal(Type type)
{
    assert(isInteger(type));

    switch (type.kind)
    {
        case Type.BOOL_:                return 1;
        case Type.CHAR, Type.UCHAR:     return uint8_t.max;
        case Type.SCHAR:                return int8_t.max;
        case Type.SHORT:                return short.max;
        case Type.USHORT:               return ushort.max;
        case Type.INT:                  return int.max;
        case Type.UINT:                 return uint.max;
        case Type.LONG:                 return long.max;
        case Type.ULONG:                return ulong.max;
        case Type.LLONG:                return long.max;
        case Type.ULLONG:               return ulong.max;
        default:
            assert(false);
    }
}

/// p 6.2.5 10, 11
/// Floating point.
/// Note we're only supporting real FP types.
bool isFP(Type type)
{
    return (
        (type.kind == Type.FLOAT) ||
        (type.kind == Type.DOUBLE));
}

/// p 6.2.5 17
/// Integer or FP.
bool isReal(Type type)
{
    return (
        isInteger(type) ||
        isFP(type)
    );
}

/// p 6.2.5 18
/// Integer and FP types are collectively 
/// called arithmetic types.
alias isArithmetic = isReal;

/// p 6.2.5 21 Scalar type.
/// Arithmetic types and pointer types are
/// collectively called scalar types.
bool isScalar(Type type)
{
    return (
        isArithmetic(type) ||
        (cast(PtrType)type)
    );
}

/// Struct or Union type
class RecType : Type
{
    /// Used for denoting anonymous RecType.
    static ulong anonId = 0;

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

        /// Constructor.
        this(Type type, string name, size_t offset = 0)
        {
            this.type = type;
            this.name = name;
            this.offset = offset;
        }
    }

    /// Whether this is a union type.
    bool isUnion;

    /// Member names and types.
    Field[] members;

    /// Constructor.
    private this(
        string name, 
        Field[] members, 
        bool isUnion = false, 
        uint8_t qual = 0)
    {
        // A name will be assigned even for an anonymous
        // struct/union.
        assert(name !is null);
        super(DERV, qual);

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
        return qualString() ~ (isUnion ? "union " : "struct ") ~ name;
    }
}

/// Access to Field constructor.
alias RecField = RecType.Field;

/// Function type.
class FuncType : Type
{
    /// Return type.
    Type retType;

    /// Param types.
    Type[] params;

    /// Constructor.
    private this(Type retType, Type[] params)
    {
        super(DERV, 0);

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

    private string paramString() const
    {
        auto ap = appender!string();
        
        ap.put("(");
        foreach (i, p; params)
        {
            if (i == (params.length - 1))
            {
                ap.put(p.toString());
            }
            else
            {
                ap.put(p.toString() ~ ",");
            }
        }
        return ap.data ~ ")";
    }

    override string toString() const
    {
        return format!"%s%s"(
            retType.toString(),
            paramString()
        );
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
    private this(Type elemTy, size_t size, uint8_t qual = 0)
    {
        super(DERV, qual);

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
        // Array of FuncType ptrs.
        auto ptrType = cast(PtrType)elemTy;
        if (ptrType)
        {
            auto funcType = cast(FuncType)ptrType.base;
            if (funcType)
            {
                return format!"%s(*%s[%s])%s"(
                    funcType.retType.toString(),
                    ptrType.qualString(),
                    size,
                    funcType.paramString()
                );
            }
        }

        return format!"%s[%s]"(elemTy.toString, size);
    }
}

/// Pointer type.
class PtrType : Type
{
    /// Pointee type.
    Type base;

    /// Constructor.
    private this(Type base, uint8_t qual = 0)
    {
        super(DERV, qual);

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
        // Ptr to FuncType.
        auto funcType = cast(FuncType)base;
        if (funcType)
        {
            return format!"%s(*%s)%s"(
                funcType.retType,
                qualString(),
                funcType.paramString()
            );
        }

        // Ptr to ArrayType.
        auto arrayType = cast(ArrayType)base;
        if (arrayType)
        {
            return format!"%s(*)[%s]"(
                arrayType.elemTy,
                arrayType.size
            );
        }

        return base.toString() ~ "*" ~ qualString();
    }
}

/// Primitive types
__gshared Type voidType   = new Type(Type.VOID);
__gshared Type boolType   = new Type(Type.BOOL_);
__gshared Type charType   = new Type(Type.CHAR);
__gshared Type scharType  = new Type(Type.SCHAR);
__gshared Type shortType  = new Type(Type.SHORT);
__gshared Type intType    = new Type(Type.INT);
__gshared Type longType   = new Type(Type.LONG);
__gshared Type llongType  = new Type(Type.LLONG);
__gshared Type ucharType  = new Type(Type.UCHAR);
__gshared Type ushortType = new Type(Type.USHORT);
__gshared Type uintType   = new Type(Type.UINT);
__gshared Type ulongType  = new Type(Type.ULONG);
__gshared Type ullongType = new Type(Type.ULLONG);
__gshared Type floatType  = new Type(Type.FLOAT);
__gshared Type doubleType = new Type(Type.DOUBLE);

/// Derived type store.
private Type[string] dvtypes;

static this()
{
    // Create basic pointer types.
    getPtrType(voidType  );
    getPtrType(boolType  );
    getPtrType(charType  );
    getPtrType(shortType );
    getPtrType(intType   );
    getPtrType(longType  );
    getPtrType(llongType );
    getPtrType(ucharType );
    getPtrType(ushortType);
    getPtrType(uintType  );
    getPtrType(ulongType );
    getPtrType(ullongType);
    getPtrType(floatType );
    getPtrType(doubleType);
}

private T getType(T)(string tystr, T delegate() ctor)
{
    debug import std.stdio : writefln;

    if (tystr in dvtypes)
    {
        auto ty = cast(T)(dvtypes[tystr]);
        assert(ty !is null);

        debug writefln(
            "Get \"%s: %s\"", 
            T.stringof, 
            ty,
            tystr
        );
        return ty;
    }

    auto ty = ctor();
    dvtypes[tystr] = ty;

    debug writefln(
        "Added \"%s: %s\"", 
        T.stringof, 
        ty,
        tystr
    );
    return ty;
}

/// Remove a type from [dvtypes].
void removeType(string tystr)
{
    dvtypes.remove(tystr);
}

private string __preRecTypeStr;

/// [name] is the tag optional identifier in struct declaration.
/// For anonymous struct, [name] is "".
/// if [name] exists in [dvtypes], return it.
/// Otherwise create an incomplete RecType for [name].
RecType getRecType(
    string name, 
    out string tystr = __preRecTypeStr, 
    bool isUnion = false,
    uint8_t qual = 0)
{
    if (name == "")
    {
        name = format!"anon%s"(RecType.anonId++);
    }

    tystr = new RecType(name, null, isUnion, qual).toString();

    return getType!(RecType)(
        tystr,
        { return new RecType(name, null, isUnion); });
}

/// Ditto.
RecType getRecType(
    string name,
    RecField[] fields, 
    out string tystr = __preRecTypeStr,
    bool isUnion = false,
    uint8_t qual = 0)
{
    auto ty = getRecType(name, tystr, isUnion, qual);

    // Already complete or cannot be completed.
    if ((ty.members !is null) || (!fields))
    {
        return ty;
    }
    // Create a complete version of it.
    else
    {
        dvtypes.remove(tystr);
        return getType!(RecType)(
            tystr,
            { return (isUnion ? makeUnionType(name, fields) : makeStrucType(name, fields)); });
    }
}

/// Get or create an array type.
ArrayType getArrayType(Type elemTy, size_t size, uint8_t qual = 0)
{
    auto astr = new ArrayType(elemTy, size, qual).toString();
    
    return getType!(ArrayType)(
        astr,
        { return new ArrayType(elemTy, size); });
}

/// Get or create a function type.
FuncType getFuncType(Type retType, Type[] params)
{
    auto fstr = new FuncType(retType, params).toString();
    
    return getType!(FuncType)(
        fstr, 
        { return new FuncType(retType, params); });
}

/// Get or create a pointer type.
PtrType getPtrType(Type base, uint8_t qual = 0)
{
    auto ptrstr = new PtrType(base, qual).toString();

    return getType!(PtrType)(
        ptrstr, 
        { return new PtrType(base, qual); });
}

/// Get qualified type of 'type'.
Type getQualType(Type type, uint8_t qual)
{
    auto recType = cast(RecType)type;
    if (recType)
    {
        return getRecType(recType.name, __preRecTypeStr, recType.isUnion, qual);
    }

    auto arrayType = cast(ArrayType)type;
    if (arrayType)
    {
        return getArrayType(arrayType.elemTy, arrayType.size, qual);
    }

    auto funcType = cast(FuncType)type;
    if (funcType)
    {
        return getFuncType(funcType.retType, funcType.params);
    }

    auto ptrType = cast(PtrType)type;
    if (ptrType)
    {
        return getPtrType(ptrType.base, qual);
    }

    // TODO: qualified types are not derived types.
    auto tystr = new Type(type.kind, qual).toString();
    return getType!(Type)(
        tystr,
        { return new Type(type.kind, qual); }
    );
}

/// Helper for iterating record fields.
/// [funct] accepts as params the type of the field, 
/// and the name of the field.
private void iterFields(
    ref RecField[] fields,
    void delegate(Type, ulong) opField
)
{
    assert((fields.length & 0x1) == 0);

    foreach (i, f; fields)
    {
        opField(f.type, i);
    }
}

private RecType makeStrucType(
    string strucName, 
    RecField[] fields)
{
    // Find alignment.
    ulong alig = 0;
    iterFields(fields, (t, _)
    {
        if (t.typeSize() > alig)
            alig = t.typeSize();
    });

    // Assign offsets.
    ulong offset = 0;
    ulong size = 0;
    iterFields(fields, (t, i)
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

        fields[i].offset = offset;
        offset += t.typeSize();
    });

    return new RecType(strucName, fields);
}

private RecType makeUnionType(
    string unionName, 
    RecField[] fields)
{
    iterFields(fields, (t, i)
    {
        fields[i].offset = 0;
    });

    return new RecType(unionName, fields, true);
}

/// Test makeStrucType and makeUnionType.
unittest
{
    uniProlog();
    RecType fooStrucType = makeStrucType(
        "fooStruc",
        [
            RecField(intType, "foo"),
            RecField(longType, "bar"),
        ]
    );

    assert(fooStrucType.isUnion == false);
    assert(fooStrucType.typeSize == 16);
    assert(fooStrucType.members[0].type == intType);
    assert(fooStrucType.members[0].offset == 0);
    assert(fooStrucType.members[0].name == "foo");
    assert(fooStrucType.members[1].type == longType);
    assert(fooStrucType.members[1].offset == 8);
    assert(fooStrucType.members[1].name == "bar");

    RecType barUnionType = makeUnionType(
        "barUnion",
        [
            RecField(longType, "foo"),
            RecField(intType, "bar")
        ]
    );
    assert(barUnionType.isUnion);
    assert(barUnionType.typeSize == 8);
    assert(barUnionType.members[0].type == longType);
    assert(barUnionType.members[0].offset == 0);
    assert(barUnionType.members[1].type == intType);
    assert(barUnionType.members[1].offset == 0);
    uniEpilog();
}

/// Test Type constructor.
unittest
{
    uniProlog();
    /// These should be equal.
    Type intPtr = new PtrType(intType);
    Type intPtr2 = new PtrType(intType);
    assert(intPtr == intPtr2);
    assert(intPtr.toHash == intPtr2.toHash);

    Type longPtr = new PtrType(longType);
    Type struc = new RecType("fooStruc", [
        RecField(longPtr, "foo", 0),
        RecField(intType, "bar", 8),
    ]);
    Type struc2 = new RecType("fooStruc", [
        RecField(longPtr, "foo", 0),
        RecField(intType, "bar", 8),
    ]);
    Type struc3 = new RecType("foo2Struc", [
        RecField(longPtr, "foo2", 0),
        RecField(intType, "bar2", 8),
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
    uniEpilog();
}

/// Test toString
unittest
{
    uniProlog();
    assert(intType.toString == "int");

    /// PtrType.
    auto intPtrTy = new PtrType(intType);
    assert(intPtrTy.toString == "int*");
    auto intPtrPtrTy = new PtrType(intPtrTy);
    assert(intPtrPtrTy.toString == "int**");

    /// FuncType.
    auto funcTy = new FuncType(voidType, [intPtrTy]);
    assert(funcTy.toString == "void(int*)");

    /// Ptr to array.
    auto ptrToIntOfThree = new PtrType(
        new ArrayType(intType, 3)
    );
    assert(ptrToIntOfThree.toString == "int(*)[3]");

    /// Ptr to function.
    auto ptrToFunc = new PtrType(
        funcTy
    );
    assert(ptrToFunc.toString == "void(*)(int*)");

    auto arrOfFunc = new ArrayType(
        ptrToFunc,
        3
    );
    assert(arrOfFunc.toString == "void(*[3])(int*)");

    /// RecType.
    auto fooStrucTy = makeStrucType(
        "fooStruc",
        [
            RecField(intType, "foo"),
            RecField(longType, "bar")
        ]
    );
    assert(fooStrucTy.toString == "struct fooStruc");
    auto barUnionTy = makeUnionType(
        "barUnion",
        [
            RecField(intType, "foo"),
            RecField(longType, "bar")
        ]
    );
    assert(barUnionTy.toString == "union barUnion");
    uniEpilog();
}

/// Test getXXXType.
unittest
{
    uniProlog();
    auto intPtrTy = getPtrType(intType);
    assert(intPtrTy !is null);
    assert(getPtrType(intType) == getPtrType(intType));

    string fooStrucTyStr;
    auto fooStrucTy = getRecType("fooStruc", fooStrucTyStr);
    assert(fooStrucTy.members is null);
    fooStrucTy = getRecType(
        "fooStruc",
        [
            RecField(intType, "foo"),
            RecField(longType, "bar"),
        ],
        fooStrucTyStr);
    assert(fooStrucTy.members !is null);
    assert(getRecType("fooStruc", fooStrucTyStr).members !is null);

    auto fooStrucMems = getRecType("fooStruc").members;
    assert(fooStrucMems[0].type == intType);
    assert(fooStrucMems[0].name == "foo");
    assert(fooStrucMems[1].type == longType);
    assert(fooStrucMems[1].name == "bar");
    uniEpilog();
}