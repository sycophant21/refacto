package projection

import (
	"fmt"

	"refacto/internal/domain/ast/expression"
)

// This file contains helper functions shared across projection logic,
// such as enum <-> string mappings and small utility builders.

// stringFromAny is a small helper to safely convert an interface{} into a
// string, returning ok=false when the value is not a string.
func stringFromAny(v any) (string, bool) {
	s, ok := v.(string)
	return s, ok
}

// BinaryOpKindToLLM maps BinaryOpKind to a human-readable operator string
// for LLM-facing JSON (e.g. "add", "sub", "mul").
func BinaryOpKindToLLM(k expression.BinaryOpKind) string {
	switch k {
	case expression.BinaryOpAdd:
		return "add"
	case expression.BinaryOpSub:
		return "sub"
	case expression.BinaryOpMul:
		return "mul"
	case expression.BinaryOpDiv:
		return "div"
	case expression.BinaryOpMod:
		return "mod"
	case expression.BinaryOpAndLogical:
		return "and"
	case expression.BinaryOpOrLogical:
		return "or"
	case expression.BinaryOpAndBitwise:
		return "bitand"
	case expression.BinaryOpOrBitwise:
		return "bitor"
	case expression.BinaryOpXorBitwise:
		return "bitxor"
	case expression.BinaryOpShiftLeft:
		return "shl"
	case expression.BinaryOpShiftRight:
		return "shr"
	default:
		return "unknown"
	}
}

// LLMToBinaryOpKind maps an LLM operator string back to BinaryOpKind.
func LLMToBinaryOpKind(s string) (expression.BinaryOpKind, error) {
	switch s {
	case "add":
		return expression.BinaryOpAdd, nil
	case "sub":
		return expression.BinaryOpSub, nil
	case "mul":
		return expression.BinaryOpMul, nil
	case "div":
		return expression.BinaryOpDiv, nil
	case "mod":
		return expression.BinaryOpMod, nil
	case "and":
		return expression.BinaryOpAndLogical, nil
	case "or":
		return expression.BinaryOpOrLogical, nil
	case "bitand":
		return expression.BinaryOpAndBitwise, nil
	case "bitor":
		return expression.BinaryOpOrBitwise, nil
	case "bitxor":
		return expression.BinaryOpXorBitwise, nil
	case "shl":
		return expression.BinaryOpShiftLeft, nil
	case "shr":
		return expression.BinaryOpShiftRight, nil
	default:
		return expression.BinaryOpUnknown, fmt.Errorf("unknown binary op: %q", s)
	}
}

// CompareOpKindToLLM maps CompareOpKind to LLM-facing strings.
func CompareOpKindToLLM(k expression.CompareOpKind) string {
	switch k {
	case expression.CompareOpLT:
		return "lt"
	case expression.CompareOpGT:
		return "gt"
	case expression.CompareOpLE:
		return "le"
	case expression.CompareOpGE:
		return "ge"
	case expression.CompareOpEQ:
		return "eq"
	case expression.CompareOpNE:
		return "ne"
	default:
		return "unknown"
	}
}

// LLMToCompareOpKind maps LLM strings back to CompareOpKind.
func LLMToCompareOpKind(s string) (expression.CompareOpKind, error) {
	switch s {
	case "lt":
		return expression.CompareOpLT, nil
	case "gt":
		return expression.CompareOpGT, nil
	case "le":
		return expression.CompareOpLE, nil
	case "ge":
		return expression.CompareOpGE, nil
	case "eq":
		return expression.CompareOpEQ, nil
	case "ne":
		return expression.CompareOpNE, nil
	default:
		return expression.CompareOpUnknown, fmt.Errorf("unknown compare op: %q", s)
	}
}

// UnaryOpKindToLLM maps UnaryOpKind to LLM-facing strings.
func UnaryOpKindToLLM(k expression.UnaryOpKind) string {
	switch k {
	case expression.UnaryOpNeg:
		return "neg"
	case expression.UnaryOpPlus:
		return "plus"
	case expression.UnaryOpNotLogical:
		return "not"
	case expression.UnaryOpBitwiseNot:
		return "bitnot"
	case expression.UnaryOpDeref:
		return "deref"
	case expression.UnaryOpAddressOf:
		return "addr"
	default:
		return "unknown"
	}
}

// LLMToUnaryOpKind maps LLM strings back to UnaryOpKind.
func LLMToUnaryOpKind(s string) (expression.UnaryOpKind, error) {
	switch s {
	case "neg":
		return expression.UnaryOpNeg, nil
	case "plus":
		return expression.UnaryOpPlus, nil
	case "not":
		return expression.UnaryOpNotLogical, nil
	case "bitnot":
		return expression.UnaryOpBitwiseNot, nil
	case "deref":
		return expression.UnaryOpDeref, nil
	case "addr":
		return expression.UnaryOpAddressOf, nil
	default:
		return expression.UnaryOpUnknown, fmt.Errorf("unknown unary op: %q", s)
	}
}
