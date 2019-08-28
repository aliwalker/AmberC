///     This file contains C type constructors and primitive types.
///     Copyright 2019 Yiyong Li.

module parser.ctypes;

import std.algorithm;
import std.array;
import std.conv;
import std.format;
import parser.lexer;

/// Base type constructor.
class Type
{
    /// Size of a variable of this type.
    size_t size;

    /// Constructor.
    this(size_t size = 0)
    {
        this.size = size;
    }

    /// Checks whether [other] is compatible.
    bool compatWith(const Type) const
    {
        return false;
    }

    override string toString() const
    {
        return "Type";
    }
}

/// Integer type constructor.
class IntType : Type
{
    /// Whether signed or unsigned.
    bool signed;

    /// Special case for _Bool type.
    bool isBool;

    /// Constructor.
    this(size_t size, bool signed = true)
    {
        super(size);
        this.signed = signed;
    }

    override string toString() const
    {
        auto repr = signed ? "Signed" : "Unsigned";
        repr ~= " " ~ to!string(size);
        return repr;
    }
}

/// Floating point type constructor.
class FloatType : Type
{
    /// Constructor.
    this(size_t size)
    {
        // TODO:
        // We're currently limiting it to be either a float or a double.
        assert(size == 4 || size == 8);
        super(size);
    }

    override string toString() const 
    {
        return (size == 32) ? "float" : "double";
    }
}

/// Void type constructor.
class VoidType : Type {
    this() {
        super();
    }
}

/// Pointer type constructor.
class PointerType : Type
{
    /// Pointee type.
    Type pointeeT;

    /// Constructor.
    this(Type pointeeT)
    {
        // NOTE: we're only targeting 64-bit posix.
        super(64);
        this.pointeeT = pointeeT;
    }

    override string toString() const
    {
        return pointeeT.toString() ~ "*";
    }
}

/// Array(one dimension) type constructor.
class ArrType : Type
{
    /// Element type.
    Type elemT;

    /// Capicity belongs to the type.
    size_t cap;

    /// Constructor.
    this(Type elemT, size_t cap)
    {
        super(elemT.size * cap);

        this.elemT = elemT;
        this.cap = cap;
    }

    override string toString() const
    {
        return elemT.toString() ~ format("[%s]", cap);
    }
}

/// Struct type constructor.
class StructType : Type
{
    /// Field types.
    Type[] fields;

    /// Offset of each field.
    size_t[] offsets;

    /// Constructor.
    this(Type[] fields)
    {
        super();

        this.fields = fields;
        assignOffset(alignment);
    }

    private size_t alignment() const
    {
        size_t alignment = 0;

        foreach (f; fields)
        {
            if (f.size > alignment)
                alignment = f.size;
        }

        return alignment;
    }

    private void assignOffset(size_t alignment)
    {
        size_t currOffset = 0;
        for (size_t i = 0; i < fields.length;)
        {
            auto acc = 0;
            const auto curr = i;
            
            // Count for a batch of fields that satisfies
            // the alignment.
            while ((i < fields.length) &&
                   (acc + fields[i].size < alignment))
            {
                acc += fields[i].size;
                i++;
            }

            // Assign offsets to current batch.
            for (size_t j = curr; j < i; j++)
            {
                offsets ~= currOffset;
                currOffset += fields[j].size;
            }

            // Insert necessary padding.
            if (acc != alignment)
            {
                currOffset = alignment - acc;
            }
        }
    }

    /// Size of the struct.
    size_t size() const
    {
        if (fields.empty)
            return 0;

        return offsets[$ - 1] + fields[$ - 1].size;
    }
}

/// Union type constructor.
class UnionType : Type
{
    /// Variant fields.
    Type[] fields;

    /// Constructor.
    this(Type[] fields)
    {
        this.fields = fields;
        super(maxField());
    }

    private size_t maxField() const
    {
        size_t max = 0;

        foreach (f; fields)
        {
            if (f.size > max)
                max = f.size;
        }

        return max;
    }
}

// Primitive C types.

const Type CChar = new IntType(1);
const Type CUnsigChar = new IntType(1, false);
const Type CShort = new IntType(2);
const Type CUnsigShort = new IntType(2, false);
const Type CInt = new IntType(4);
const Type CUnsigInt = new IntType(4, false);
const Type CLong = new IntType(8);
const Type CUnsigLong = new IntType(8, false);
const Type CFloat = new FloatType(4);
const Type CDouble = new FloatType(8);
const Type CVoid = new VoidType();

/// Maps token to primitive types.
/// Returns the above types if matched; otherwise returns null.
/// NOTE: For type compatibility, see 
/// https://www.cs.auckland.ac.nz/references/unix/digital/AQTLTBTE/DOCU_020.HTM
auto getPrimitiveType(ref Token tok)
{
    switch (tok.kind)
    {
    case Token.KW:
        switch (tok.stringVal)
        {
        case "short", "short int", "signed short", "signed short int":
            return CShort;
        
        case "unsigned short", "unsigned short int":
            return CUnsigShort;
        
        case "int", "signed", "signed int":
            return CInt;

        case "unsigned", "unsigned int":
            return CUnsigInt;

        case "long", "signed long", "long int", "signed long int":
            return CLong;

        case "unsigned long", "unsigned long int":
            return CUnsigLong;

        case "float":
            return CFloat;

        case "double":
            return CDouble;

        default:
            return null;
        }

    default:
        return null;
    }
    return null;
}

unittest {
    
}