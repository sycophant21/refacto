package expression

import "encoding/json"

type (
	Expression interface {
		GetContents() string
		GetExpressionType() ExpressionType
	}
	ExpressionType int

	// SymbolKind describes what an identifier refers to after name resolution.
	SymbolKind int

	// BinaryOpKind normalizes all binary operators across languages.
	BinaryOpKind int

	// CompareOpKind normalizes comparison operators.
	CompareOpKind int

	// UnaryOpKind normalizes prefix/unary operators.
	UnaryOpKind int

	// IdentifierCategory is a syntactic hint about how an identifier is used.
	IdentifierCategory int
)

const (
	SymbolUnknown SymbolKind = iota
	SymbolVar
	SymbolParam
	SymbolField
	SymbolFunc
)

const (
	BinaryOpUnknown BinaryOpKind = iota
	BinaryOpAdd
	BinaryOpSub
	BinaryOpMul
	BinaryOpDiv
	BinaryOpMod
	BinaryOpAndLogical
	BinaryOpOrLogical
	BinaryOpAndBitwise
	BinaryOpOrBitwise
	BinaryOpXorBitwise
	BinaryOpShiftLeft
	BinaryOpShiftRight
)

const (
	CompareOpUnknown CompareOpKind = iota
	CompareOpLT
	CompareOpGT
	CompareOpLE
	CompareOpGE
	CompareOpEQ
	CompareOpNE
)

const (
	UnaryOpUnknown    UnaryOpKind = iota
	UnaryOpNeg                    // -x
	UnaryOpPlus                   // +x
	UnaryOpNotLogical             // !x
	UnaryOpBitwiseNot             // ^x or ~x
	UnaryOpDeref                  // *p
	UnaryOpAddressOf              // &x
)

const (
	IdentCategoryUnknown     IdentifierCategory = iota
	IdentCategoryVarLike                        // appears in var/short-var/param positions
	IdentCategoryTypeLike                       // appears in type positions
	IdentCategoryPackageLike                    // appears before a dot at top-level
)

const (
	ExpressionTypeIdentifier ExpressionType = iota
	ExpressionTypeIntegerLiteral
	ExpressionTypeStringLiteral
	ExpressionTypeBooleanLiteral
	ExpressionTypePrefix
	ExpressionTypeInfix
	ExpressionTypeComparison
	ExpressionTypeCall
	ExpressionTypeIndex
	ExpressionTypeFunction
	ExpressionTypeSelector
)

// --- JSON marshaling for enums: serialize as strings ---

func (k BinaryOpKind) MarshalJSON() ([]byte, error) {
	var s string
	switch k {
	case BinaryOpAdd:
		s = "Add"
	case BinaryOpSub:
		s = "Sub"
	case BinaryOpMul:
		s = "Mul"
	case BinaryOpDiv:
		s = "Div"
	case BinaryOpMod:
		s = "Mod"
	case BinaryOpAndLogical:
		s = "AndLogical"
	case BinaryOpOrLogical:
		s = "OrLogical"
	case BinaryOpAndBitwise:
		s = "AndBitwise"
	case BinaryOpOrBitwise:
		s = "OrBitwise"
	case BinaryOpXorBitwise:
		s = "XorBitwise"
	case BinaryOpShiftLeft:
		s = "ShiftLeft"
	case BinaryOpShiftRight:
		s = "ShiftRight"
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

func (k CompareOpKind) MarshalJSON() ([]byte, error) {
	var s string
	switch k {
	case CompareOpLT:
		s = "LT"
	case CompareOpGT:
		s = "GT"
	case CompareOpLE:
		s = "LE"
	case CompareOpGE:
		s = "GE"
	case CompareOpEQ:
		s = "EQ"
	case CompareOpNE:
		s = "NE"
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

func (k UnaryOpKind) MarshalJSON() ([]byte, error) {
	var s string
	switch k {
	case UnaryOpNeg:
		s = "Neg"
	case UnaryOpPlus:
		s = "Plus"
	case UnaryOpNotLogical:
		s = "NotLogical"
	case UnaryOpBitwiseNot:
		s = "BitwiseNot"
	case UnaryOpDeref:
		s = "Deref"
	case UnaryOpAddressOf:
		s = "AddressOf"
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

func (k SymbolKind) MarshalJSON() ([]byte, error) {
	var s string
	switch k {
	case SymbolVar:
		s = "Var"
	case SymbolParam:
		s = "Param"
	case SymbolField:
		s = "Field"
	case SymbolFunc:
		s = "Func"
	case SymbolUnknown:
		fallthrough
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

func (k IdentifierCategory) MarshalJSON() ([]byte, error) {
	var s string
	switch k {
	case IdentCategoryVarLike:
		s = "VarLike"
	case IdentCategoryTypeLike:
		s = "TypeLike"
	case IdentCategoryPackageLike:
		s = "PackageLike"
	case IdentCategoryUnknown:
		fallthrough
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

func (t ExpressionType) MarshalJSON() ([]byte, error) {
	var s string
	switch t {
	case ExpressionTypeIdentifier:
		s = "Identifier"
	case ExpressionTypeIntegerLiteral:
		s = "IntegerLiteral"
	case ExpressionTypeStringLiteral:
		s = "StringLiteral"
	case ExpressionTypeBooleanLiteral:
		s = "BooleanLiteral"
	case ExpressionTypePrefix:
		s = "Prefix"
	case ExpressionTypeInfix:
		s = "Infix"
	case ExpressionTypeComparison:
		s = "Comparison"
	case ExpressionTypeCall:
		s = "Call"
	case ExpressionTypeIndex:
		s = "Index"
	case ExpressionTypeFunction:
		s = "Function"
	case ExpressionTypeSelector:
		s = "Selector"
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}
