package expression

import (
	"fmt"
	"strings"
)

type (
	SymbolBinding struct {
		Kind   SymbolKind `json:"Kind"`
		DeclId string     `json:"DeclId,omitempty"`
	}

	Identifier struct {
		Value    string             `json:"Value,omitempty"`
		Category IdentifierCategory `json:"Category,omitempty"`
		Binding  *SymbolBinding     `json:"Binding,omitempty"`
	}
	IntegerLiteral struct {
		Value int64
	}
	StringLiteral struct {
		Value string
	}
	BooleanLiteral struct {
		Value bool
	}
	PrefixExpression struct {
		Operator UnaryOpKind
		Right    Expression
	}
	InfixExpression struct {
		Left     Expression
		Operator BinaryOpKind
		Right    Expression
	}
	ComparisonExpression struct {
		Left     Expression
		Operator CompareOpKind
		Right    Expression
	}
	CallExpression struct {
		Function  Expression
		Arguments []Expression
	}
	SelectorExpression struct {
		Operand  Expression  `json:"Operand,omitempty"`
		Selector *Identifier `json:"Selector,omitempty"` // Right side of dot is always identifier
	}
)

// Identifier
func (i *Identifier) GetExpressionType() ExpressionType { return ExpressionTypeIdentifier }
func (i *Identifier) GetContents() string               { return i.Value }

// IntegerLiteral
func (i *IntegerLiteral) GetExpressionType() ExpressionType { return ExpressionTypeIntegerLiteral }
func (i *IntegerLiteral) GetContents() string               { return fmt.Sprintf("%d", i.Value) }

// StringLiteral
func (s *StringLiteral) GetExpressionType() ExpressionType { return ExpressionTypeStringLiteral }
func (s *StringLiteral) GetContents() string               { return fmt.Sprintf("\"%s\"", s.Value) }

// BooleanLiteral
func (b *BooleanLiteral) GetExpressionType() ExpressionType { return ExpressionTypeBooleanLiteral }
func (b *BooleanLiteral) GetContents() string               { return fmt.Sprintf("%t", b.Value) }

// PrefixExpression
func (p *PrefixExpression) GetExpressionType() ExpressionType { return ExpressionTypePrefix }
func (p *PrefixExpression) GetContents() string {
	op := unaryOpToString(p.Operator)
	return fmt.Sprintf("(%s%s)", op, p.Right.GetContents())
}

// InfixExpression
func (e *InfixExpression) GetExpressionType() ExpressionType { return ExpressionTypeInfix }
func (e *InfixExpression) GetContents() string {
	// Map BinaryOpKind back to a symbol for printing.
	op := binaryOpToString(e.Operator)
	if e.Left == nil || e.Right == nil {
		// Post-increment/decrement: identifier++ / identifier--
		if op == "++" || op == "--" {
			if e.Left != nil {
				return fmt.Sprintf("%s%s", e.Left.GetContents(), op)
			}
		}
		return fmt.Sprintf("(incomplete: op=%s)", op)
	}
	return fmt.Sprintf("(%s %s %s)", e.Left.GetContents(), op, e.Right.GetContents())
}

// ComparisonExpression
func (e *ComparisonExpression) GetExpressionType() ExpressionType { return ExpressionTypeComparison }
func (e *ComparisonExpression) GetContents() string {
	op := compareOpToString(e.Operator)
	if e.Left == nil || e.Right == nil {
		return fmt.Sprintf("(incomplete-compare: op=%s)", op)
	}
	return fmt.Sprintf("(%s %s %s)", e.Left.GetContents(), op, e.Right.GetContents())
}

// Helpers to print operators from enums.
func binaryOpToString(k BinaryOpKind) string {
	switch k {
	case BinaryOpAdd:
		return "+"
	case BinaryOpSub:
		return "-"
	case BinaryOpMul:
		return "*"
	case BinaryOpDiv:
		return "/"
	case BinaryOpMod:
		return "%"
	case BinaryOpAndLogical:
		return "&&"
	case BinaryOpOrLogical:
		return "||"
	case BinaryOpAndBitwise:
		return "&"
	case BinaryOpOrBitwise:
		return "|"
	case BinaryOpXorBitwise:
		return "^"
	case BinaryOpShiftLeft:
		return "<<"
	case BinaryOpShiftRight:
		return ">>"
	default:
		return "?"
	}
}

func compareOpToString(k CompareOpKind) string {
	switch k {
	case CompareOpLT:
		return "<"
	case CompareOpGT:
		return ">"
	case CompareOpLE:
		return "<="
	case CompareOpGE:
		return ">="
	case CompareOpEQ:
		return "=="
	case CompareOpNE:
		return "!="
	default:
		return "?"
	}
}

func unaryOpToString(k UnaryOpKind) string {
	switch k {
	case UnaryOpNeg:
		return "-"
	case UnaryOpPlus:
		return "+"
	case UnaryOpNotLogical:
		return "!"
	case UnaryOpBitwiseNot:
		return "^"
	case UnaryOpDeref:
		return "*"
	case UnaryOpAddressOf:
		return "&"
	default:
		return "?"
	}
}

// CallExpression
func (c *CallExpression) GetExpressionType() ExpressionType { return ExpressionTypeCall }
func (c *CallExpression) GetContents() string {
	var args []string
	for _, a := range c.Arguments {
		args = append(args, a.GetContents())
	}
	return fmt.Sprintf("%s(%s)", c.Function.GetContents(), strings.Join(args, ", "))
}

// SelectorExpression
func (s *SelectorExpression) GetExpressionType() ExpressionType { return ExpressionTypeSelector }
func (s *SelectorExpression) GetContents() string {
	return fmt.Sprintf("%s.%s", s.Operand.GetContents(), s.Selector.GetContents())
}
