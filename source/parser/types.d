///     This file contains C type related utilities.
///     Copyright 2019 Yiyong Li.

module parser.types;

import std.range;
import std.stdint;
import std.array;
import std.format;

/// Size of a pointer.
const PTR_SIZE = 8;

enum StorageClass : uint8_t
{
    UNSPECIFIED,
    TYPEDEF,
    EXTERN,
    STATIC,
    AUTO,
    REGISTER,
}

/// NOTE: we simply opt for ignoring all these
/// qualifiers except "const".
enum Qualifier : uint8_t
{
    UNSPECIFIED,
    CONST,
    RESTRICT,
    VOLATILE,
    INLINE
}

/// Base type.
class Type
{
    alias Kind = uint8_t;
    enum : Kind {
        PLACE_HOLDER,
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
        ENUM,
        ARRAY,
        STRUCT,
        FUNC,
        PTR,
    }

    Kind kind;
    bool isconst;
    ulong offset;
    ulong size;

    /// Constructor for a base type in C.
    private this(Kind kind, ulong size, bool isconst = 0)
    {
        this.kind = kind;
        this.size = size;

        this.isconst = isconst;
        this.offset = 0;
    }

    /// Constructor for a placeholder type.
    this() {
        this.kind = PLACE_HOLDER;
        this.size = 1;
    }

    Type dup()
    {
        return new Type(kind, size, isconst);
    }

    Type getConst()
    {
        if (isconst)
            return this;

        Type copy = this.dup();
        copy.isconst = true;
        return copy;
    }

    bool isComplete() const
    {
        return true;
    }

    bool isUnsigned() const
    {
        return (
            (kind == UCHAR)  ||
            (kind == USHORT) ||
            (kind == UINT)   ||
            (kind == ULONG)  ||
            (kind == ULLONG));
    }

    Type getUnsigned() const
    {
        assert(isSigned());
        switch (kind)
        {
            case SCHAR: return (isconst ? ucharType.getConst() : ucharType);
            case SHORT: return (isconst ? ushortType.getConst() : ushortType);
            case INT:   return (isconst ? uintType.getConst() : uintType);
            case LONG:  return (isconst ? ulongType.getConst() : ulongType);
            case LLONG: return (isconst ? ullongType.getConst() : ullongType);
            default: assert(0);
        }
    }

    bool isSigned() const
    {
        return (
            (kind == SCHAR) ||
            (kind == SHORT) ||
            (kind == INT)   ||
            (kind == LONG)  ||
            (kind == LLONG));
    }

    Type getSigned() const
    {
        assert(isUnsigned());
        switch (kind)
        {
            case UCHAR:     return (isconst ? scharType.getConst() : scharType);
            case USHORT:    return (isconst ? shortType.getConst() : shortType);
            case UINT:      return (isconst ? intType.getConst() : intType);
            case ULONG:     return (isconst ? longType.getConst() : longType);
            case ULLONG:    return (isconst ? llongType.getConst() : llongType);
            default:
                assert(0);
        }
    }

    bool isInteger() const { return (isSigned() || isUnsigned() || (kind == BOOL_)); }
    bool isPointer() const { return (kind == PTR);}
    bool isArray() const { return (kind == ARRAY); }
    bool isStruct() const { return (kind == STRUCT); }
    bool isFunction() const { return (kind == FUNC); }

    StructFields fields() { assert(0, "not struct type"); }
    Type ret() { assert(0, "not func type"); }
    Type[] params() { assert(0, "not func type"); }
    bool varArgs() { assert(0, "not func type"); }
    Type base() { assert(0, "not ptr/array type"); }

    PtrType makePointer()
    {
        return new PtrType(PTR, this);
    }

    PtrType makeArray(ulong n)
    {
        assert(!isFunction(), "cannot make array of function type");

        return new PtrType(ARRAY, this, n);
    }

    /// p 6.2.5 10, 11
    /// Floating point.
    /// Note we're only supporting real FP types.
    bool isFP() const
    {
        return (
            (kind == FLOAT) ||
            (kind == DOUBLE));
    }

    /// p 6.2.5 17
    /// Integer or FP.
    bool isReal() const
    {
        return (
            isInteger() ||
            isFP());
    }

    /// p 6.2.5 18
    /// Integer and FP types are collectively 
    /// called arithmetic types.
    alias isArithmetic = isReal;

    /// p 6.2.5 21 Scalar type.
    /// Arithmetic types and pointer types are
    /// collectively called scalar types.
    bool isScalar() const
    {
        return (
            isArithmetic() ||
            (cast(PtrType)this !is null)
        );
    }

    /// Returns whether they're the same type ignoring qualifiers(isconst).
    bool isSame(Type type) const
    {
        return kind == type.kind;
    }

    override string toString() const
    {
        switch (kind)
        {
            case PLACE_HOLDER:  return "placeholder";
            case VOID:      return "void";
            case BOOL_:     return "Bool_";
            case CHAR:      return "char";
            case SCHAR:     return "signed char";
            case SHORT:     return "short";
            case INT:       return "int";
            case LONG:      return "long";
            case LLONG:     return "long long";
            case FLOAT:     return "float";
            case UCHAR:     return "unsigned char";
            case USHORT:    return "unsigned short";
            case UINT:      return "unsigned";
            case ULONG:     return "unsigned long";
            case ULLONG:    return "unsigned long long";
            case DOUBLE:    return "double";
            default:
                return "";
        }
    }
}

/// Represent fields/members of a struct.
class StructFields
{
    private Type[string] kv;
    string[] names;

    this() {}
    this(Type[string] kv, string[] names)
    {
        this.kv = kv;
        this.names = names;
    }

    StructFields dup()
    {
        return new StructFields(kv.dup, names.dup);
    }

    ulong length() const
    {
        return kv.length;
    }

    /// Add a field/member.
    void add(Type type, string name)
    {
        assert((name in kv) is null);

        names ~= name;
        kv[name] = type;
    }

    Type get(string name)
    {
        if (name in kv)
            return kv[name];
        return null;
    }

    const(Type) get(string name) const
    {
        if (name in kv)
            return kv[name];
        return null;
    }
}

/// Represent struct or union type.
class StructType : Type
{
    bool isUnion;
    /// Fields/members.
    StructFields fields_;

    this(StructFields fields, ulong size, bool isUnion, bool isconst = false)
    {
        super(STRUCT, size, isconst);

        this.isUnion = isUnion;
        this.fields_ = fields;
    }

    override StructType dup()
    {
        return new StructType(fields_.dup, size, isStruct, isconst);
    }

    override bool isComplete() const
    {
        return (fields_ && (size != -1));
    }

    override string toString() const
    {
        auto s = appender!string();

        s.put(isStruct ? "(struct " : "(union ");
        foreach (string name; fields_.names)
        {
            s.put(format!"(%s %s)"(fields_.get(name), name));
        }
        s.put(")");
        return s.data;
    }

    override StructFields fields()
    {
        return fields_;
    }

    override bool isSame(Type type) const
    {
        StructType stype = cast(StructType)type;
        if (!stype)
            return false;

        if (stype.isUnion != isUnion)
            return false;

        if (stype.fields_.length() != fields_.length())
            return false;

        for (int i = 0; i < fields_.length(); i++)
        {
            const(Type) a = this.fields_.get(this.fields_.names[i]);
            Type b = cast(Type)stype.fields_.get(stype.fields_.names[i]);
            if (!a.isSame(b))
                return false;
        }
        return true;
    }
}

/// Represent function type.
class FuncType : Type
{
    Type ret_;
    Type[] params_;
    bool varArgs_;

    this(Type ret, Type[] params, bool varArgs = false)
    {
        super(FUNC, 1, true);

        this.ret_ = ret;
        this.params_ = params;
        this.varArgs_ = varArgs;
    }

    override FuncType dup()
    {
        return new FuncType(ret_.dup, params.dup, varArgs);
    }

    override Type ret()
    {
        return ret_;
    }

    override Type[] params()
    {
        return params_;
    }

    override bool varArgs()
    {
        return varArgs_;
    }

    override string toString() const
    {
        auto ap = appender!string();

        ap.put(format!"%s ("(ret_));
        foreach (i, p; params_)
        {
            ap.put(p.toString());
            if (i + 1 != params_.length)
                ap.put(",");
        }
        return ap.data ~ ")";
    }

    /// Function pointers are always the same.
}

/// Represent pointer and array type.
class PtrType : Type
{
    Type base_;

    /// [n] is the number of elements.
    this(Kind kind, Type base, ulong n = -1, bool isconst = false)
    {
        assert(kind == PTR || kind == ARRAY);
        super(kind, (kind == PTR ? PTR_SIZE : n * base.size), isconst);

        this.base_ = base;
    }

    override PtrType dup()
    {
        return new PtrType(kind, base_.dup, size == PTR_SIZE ? -1 : size / base_.size, isconst);
    }

    override Type base()
    {
        return base_;
    }

    override bool isComplete() const
    {
        if (kind == PTR)
            return true;

        return size > 0;
    }

    override string toString() const
    {
        if (kind == ARRAY)
        {
            return size == -1 ? "[]" ~ base_.toString() : format!"[%s]%s"(size, base_);
        }
        return format!"*%s"(base_);
    }

    override bool isSame(Type type) const
    {
        if (isPointer() && type.isPointer())
            return base_.isSame(type.base());

        if (isArray() && type.isArray())
        {
            if (this.size != type.size)
                return false;
            return base_.isSame(type.base());
        }
        return false;
    }
}

/// Primitive types
__gshared Type voidType   = new Type(Type.VOID, 1);
__gshared Type boolType   = new Type(Type.BOOL_, 1);
__gshared Type charType   = new Type(Type.CHAR, 1);
__gshared Type scharType  = new Type(Type.SCHAR, 1);
__gshared Type shortType  = new Type(Type.SHORT, 2);
__gshared Type intType    = new Type(Type.INT, 4);
__gshared Type longType   = new Type(Type.LONG, 8);
__gshared Type llongType  = new Type(Type.LLONG, 8);
__gshared Type ucharType  = new Type(Type.UCHAR, 1);
__gshared Type ushortType = new Type(Type.USHORT, 2);
__gshared Type uintType   = new Type(Type.UINT, 4);
__gshared Type ulongType  = new Type(Type.ULONG, 8);
__gshared Type ullongType = new Type(Type.ULLONG, 8);
__gshared Type floatType  = new Type(Type.FLOAT, 4);
__gshared Type doubleType = new Type(Type.DOUBLE, 8);

/// NOTE: Treat all enum types as a single enum type.
__gshared Type enumType   = new Type(Type.ENUM, 4);
