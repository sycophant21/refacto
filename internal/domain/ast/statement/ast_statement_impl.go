package statement

import (
	"encoding/json"
	"fmt"
	"refacto/internal/domain/ast/expression"
	"strings"
)

type (
	//ExpressionStatement          struct{}
	AssignmentStatement struct {
		Assignee   expression.Expression
		Expression expression.Expression
		Operator   AssignOpKind
	}
	VariableDeclarationStatement struct {
		Name       string
		Type       string
		Expression expression.Expression
	}
	ReturnStatement struct {
		ReturnExpressions []expression.Expression
	}
	IfStatement struct {
		IfExpression expression.Expression
		Body         []Statement
	}
	ElseIfStatement struct {
		ElseIfExpression expression.Expression
		Body             []Statement
	}
	ElseStatement struct {
		ElseExpression expression.Expression // Usually nil, but keeping for consistency if needed
		Body           []Statement
	}

	// ForHeaderKind describes which form the header takes (clause vs range, etc.).
	ForHeaderKind int
)

const (
	ForHeaderKindClause ForHeaderKind = iota
	ForHeaderKindRange
)

type (

	// ForHeader is a generic header representation for all for-loop syntaxes.
	// Pre/Core/Post are always Statements (which may wrap expressions).
	ForHeader struct {
		Kind ForHeaderKind `json:"Kind"`

		Pre  Statement `json:"Pre,omitempty"`  // init / pre-header logic
		Core Statement `json:"Core,omitempty"` // condition or range-header
		Post Statement `json:"Post,omitempty"` // post/update logic
	}

	// RangeHeaderStatement is a generic range/foreach header that can be used by any language
	// to represent constructs like `for k, v := range src` or `for x in collection`.
	RangeHeaderStatement struct {
		Key   expression.Expression `json:"Key,omitempty"`   // index/key or nil
		Value expression.Expression `json:"Value,omitempty"` // value/element or nil
		Src   expression.Expression `json:"Src,omitempty"`   // source expression after range/in
	}

	ForStatement struct {
		Header ForHeader   `json:"Header"`
		Body   []Statement `json:"Body,omitempty"`
	}
	WhileStatement struct {
		Condition expression.Expression
		Body      []Statement
	}
	DoWhileStatement struct {
		Condition expression.Expression
		Body      []Statement
	}
	SwitchStatement struct {
		Expression expression.Expression
		Cases      []*SwitchCaseStatement
	}
	SwitchCaseStatement struct {
		Expressions []expression.Expression
		Body        []Statement
	}
	SelectStatement struct {
		Cases []*SelectCaseStatement
	}
	SelectCaseStatement struct {
		Expressions []expression.Expression
		Body        []Statement
	}
	BreakStatement struct {
		Body expression.Expression
	}
	ContinueStatement struct {
		Body expression.Expression
	}
	BlockStatement struct {
		Body []Statement
	}
	DeferStatement struct {
		Call expression.Expression
	}
	TryStatement struct {
		Expression       expression.Expression
		Body             []Statement
		CatchStatement   *CatchStatement
		FinallyStatement *FinallyStatement
	}
	CatchStatement struct {
		Expression expression.Expression
		Body       []Statement
	}
	FinallyStatement struct {
		Body []Statement
	}
	ThrowStatement struct {
		Expression expression.Expression
	}
	GotoStatement struct {
		Label string
	}
	LabelStatement struct {
		Name string
	}
	ImportStatement struct {
		ModulePath string   `json:"ModulePath,omitempty"`
		Alias      string   `json:"Alias,omitempty"`
		Imports    []string `json:"Imports,omitempty"`
		Raw        string   `json:"-"`
	}
	YieldStatement struct {
		Expression expression.Expression
	}
	AwaitStatement struct {
		Expression expression.Expression
	}
	ExpressionStatement struct {
		Expression expression.Expression
	}
)

// --- JSON marshaling for statement enums: serialize as strings ---

func (k AssignOpKind) MarshalJSON() ([]byte, error) {
	var s string
	switch k {
	case AssignOpEq:
		s = "Eq"
	case AssignOpAdd:
		s = "Add"
	case AssignOpSub:
		s = "Sub"
	case AssignOpMul:
		s = "Mul"
	case AssignOpDiv:
		s = "Div"
	case AssignOpMod:
		s = "Mod"
	case AssignOpAnd:
		s = "And"
	case AssignOpOr:
		s = "Or"
	case AssignOpXor:
		s = "Xor"
	case AssignOpShiftLeft:
		s = "ShiftLeft"
	case AssignOpShiftRight:
		s = "ShiftRight"
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

func (t StatementType) MarshalJSON() ([]byte, error) {
	var s string
	switch t {
	case StatementTypeExpression:
		s = "Expression"
	case StatementTypeAssignment:
		s = "Assignment"
	case StatementTypeVarDecl:
		s = "VarDecl"
	case StatementTypeReturn:
		s = "Return"
	case StatementTypeIf:
		s = "If"
	case StatementTypeElse:
		s = "Else"
	case StatementTypeElseIf:
		s = "ElseIf"
	case StatementTypeFor:
		s = "For"
	case StatementTypeWhile:
		s = "While"
	case StatementTypeDoWhile:
		s = "DoWhile"
	case StatementTypeSwitch:
		s = "Switch"
	case StatementTypeCase:
		s = "Case"
	case StatementTypeSelect:
		s = "Select"
	case StatementTypeDefault:
		s = "Default"
	case StatementTypeBreak:
		s = "Break"
	case StatementTypeContinue:
		s = "Continue"
	case StatementTypeBlock:
		s = "Block"
	case StatementTypeDefer:
		s = "Defer"
	case StatementTypeTry:
		s = "Try"
	case StatementTypeCatch:
		s = "Catch"
	case StatementTypeFinally:
		s = "Finally"
	case StatementTypeThrow:
		s = "Throw"
	case StatementTypeGoto:
		s = "Goto"
	case StatementTypeLabel:
		s = "Label"
	case StatementTypeImport:
		s = "Import"
	case StatementTypeYield:
		s = "Yield"
	case StatementTypeAwait:
		s = "Await"
	case StatementTypeForHeader:
		s = "ForHeader"
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

func (k ForHeaderKind) MarshalJSON() ([]byte, error) {
	var s string
	switch k {
	case ForHeaderKindClause:
		s = "Clause"
	case ForHeaderKindRange:
		s = "Range"
	default:
		s = "Unknown"
	}
	return json.Marshal(s)
}

// Helper to reconstruct body
func reconstructBody(stmts []Statement) string {
	var sb strings.Builder
	sb.WriteString("{\n")
	for _, s := range stmts {
		sb.WriteString(s.GetContents())
		sb.WriteString("\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// Helper to render switch/select cases
func renderCase(prefix string, exprs []expression.Expression, body []Statement) string {
	// If no explicit prefix, derive from expressions (case/default semantics)
	if prefix == "" {
		prefix = "default"
		if len(exprs) > 0 {
			var parts []string
			for _, e := range exprs {
				parts = append(parts, e.GetContents())
			}
			prefix = "case " + strings.Join(parts, ", ")
		}
	}

	var sb strings.Builder
	sb.WriteString(prefix + ":\n")
	for _, stmt := range body {
		sb.WriteString(stmt.GetContents())
		sb.WriteString("\n")
	}
	return sb.String()
}

// Helper to stringify assignment operators
func assignOpToString(op AssignOpKind) string {
	switch op {
	case AssignOpEq:
		return "="
	case AssignOpAdd:
		return "+="
	case AssignOpSub:
		return "-="
	case AssignOpMul:
		return "*="
	case AssignOpDiv:
		return "/="
	case AssignOpMod:
		return "%="
	case AssignOpAnd:
		return "&="
	case AssignOpOr:
		return "|="
	case AssignOpXor:
		return "^="
	case AssignOpShiftLeft:
		return "<<="
	case AssignOpShiftRight:
		return ">>="
	default:
		return "="
	}
}

// AssignmentStatement
func (s *AssignmentStatement) GetStatementType() StatementType { return StatementTypeAssignment }
func (s *AssignmentStatement) GetContents() string {
	op := assignOpToString(s.Operator)
	return fmt.Sprintf("%s %s %s", s.Assignee.GetContents(), op, s.Expression.GetContents())
}

// VariableDeclarationStatement
func (s *VariableDeclarationStatement) GetStatementType() StatementType { return StatementTypeVarDecl }
func (s *VariableDeclarationStatement) GetContents() string {
	if s.Expression != nil {
		return fmt.Sprintf("var %s %s = %s", s.Name, s.Type, s.Expression.GetContents())
	}
	return fmt.Sprintf("var %s %s", s.Name, s.Type)
}

// ReturnStatement
func (s *ReturnStatement) GetStatementType() StatementType { return StatementTypeReturn }
func (s *ReturnStatement) GetContents() string {
	if len(s.ReturnExpressions) == 0 {
		return "return"
	}
	var exprStrs []string
	for _, expr := range s.ReturnExpressions {
		if expr != nil {
			exprStrs = append(exprStrs, expr.GetContents())
		}
	}
	if len(exprStrs) == 0 {
		return "return"
	}
	return fmt.Sprintf("return %s", strings.Join(exprStrs, ", "))
}

// IfStatement
func (s *IfStatement) GetStatementType() StatementType { return StatementTypeIf }
func (s *IfStatement) GetContents() string {
	return fmt.Sprintf("if %s %s", s.IfExpression.GetContents(), reconstructBody(s.Body))
}

// ElseIfStatement
func (s *ElseIfStatement) GetStatementType() StatementType { return StatementTypeElseIf }
func (s *ElseIfStatement) GetContents() string {
	return fmt.Sprintf("else if %s %s", s.ElseIfExpression.GetContents(), reconstructBody(s.Body))
}

// ElseStatement
func (s *ElseStatement) GetStatementType() StatementType { return StatementTypeElse }
func (s *ElseStatement) GetContents() string {
	return fmt.Sprintf("else %s", reconstructBody(s.Body))
}

// RangeHeaderStatement
func (s *RangeHeaderStatement) GetStatementType() StatementType { return StatementTypeForHeader }
func (s *RangeHeaderStatement) GetContents() string {
	var keyStr, valueStr, srcStr string
	if s.Key != nil {
		keyStr = s.Key.GetContents()
	}
	if s.Value != nil {
		valueStr = s.Value.GetContents()
	}
	if s.Src != nil {
		srcStr = s.Src.GetContents()
	}

	// Print forms:
	// - range src
	// - v := range src
	// - k, v := range src
	if keyStr == "" && valueStr == "" {
		return fmt.Sprintf("range %s", srcStr)
	}
	if keyStr != "" && valueStr == "" {
		return fmt.Sprintf("%s := range %s", keyStr, srcStr)
	}
	return fmt.Sprintf("%s, %s := range %s", keyStr, valueStr, srcStr)
}

// ForStatement
func (s *ForStatement) GetStatementType() StatementType { return StatementTypeFor }
func (s *ForStatement) GetContents() string {
	var pre, core, post string
	if s.Header.Pre != nil {
		pre = s.Header.Pre.GetContents()
	}
	if s.Header.Core != nil {
		core = s.Header.Core.GetContents()
	}
	if s.Header.Post != nil {
		post = s.Header.Post.GetContents()
	}

	// Very simple pretty-printing based on which slots are populated.
	// This is intentionally generic and not Go-specific.
	// - Only Core:   for <core> { body }
	// - No header:   for { body }
	// - Clause form: for <pre>; <core>; <post> { body }

	bodyStr := reconstructBody(s.Body)

	if pre == "" && post == "" {
		// Infinite loop (no core) or condition-only/range-only loop
		return fmt.Sprintf("for %s %s", core, bodyStr)
	}

	return fmt.Sprintf("for %s; %s; %s %s", pre, core, post, bodyStr)
}

// WhileStatement
func (s *WhileStatement) GetStatementType() StatementType { return StatementTypeWhile }
func (s *WhileStatement) GetContents() string {
	return fmt.Sprintf("while %s %s", s.Condition.GetContents(), reconstructBody(s.Body))
}

// DoWhileStatement
func (s *DoWhileStatement) GetStatementType() StatementType { return StatementTypeDoWhile }
func (s *DoWhileStatement) GetContents() string {
	return fmt.Sprintf("do %s while %s", reconstructBody(s.Body), s.Condition.GetContents())
}

// SwitchStatement
func (s *SwitchStatement) GetStatementType() StatementType { return StatementTypeSwitch }
func (s *SwitchStatement) GetContents() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("switch %s {\n", s.Expression.GetContents()))
	for _, c := range s.Cases {
		sb.WriteString(c.GetContents())
		sb.WriteString("\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// SwitchCaseStatement
func (s *SwitchCaseStatement) GetStatementType() StatementType { return StatementTypeCase }
func (s *SwitchCaseStatement) GetContents() string {
	return renderCase("", s.Expressions, s.Body)
}

// SelectStatement
func (s *SelectStatement) GetStatementType() StatementType { return StatementTypeSelect }
func (s *SelectStatement) GetContents() string {
	var sb strings.Builder
	sb.WriteString("select {\n")
	for _, c := range s.Cases {
		sb.WriteString(c.GetContents())
		sb.WriteString("\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// SelectCaseStatement
func (s *SelectCaseStatement) GetStatementType() StatementType { return StatementTypeCase }
func (s *SelectCaseStatement) GetContents() string {
	return renderCase("", s.Expressions, s.Body)
}

// BreakStatement
func (s *BreakStatement) GetStatementType() StatementType { return StatementTypeBreak }
func (s *BreakStatement) GetContents() string             { return "break" }

// ContinueStatement
func (s *ContinueStatement) GetStatementType() StatementType { return StatementTypeContinue }
func (s *ContinueStatement) GetContents() string             { return "continue" }

// BlockStatement
func (s *BlockStatement) GetStatementType() StatementType { return StatementTypeBlock }
func (s *BlockStatement) GetContents() string             { return reconstructBody(s.Body) }

// DeferStatement
func (s *DeferStatement) GetStatementType() StatementType { return StatementTypeDefer }
func (s *DeferStatement) GetContents() string {
	return fmt.Sprintf("defer %s", s.Call.GetContents())
}

// TryStatement
func (s *TryStatement) GetStatementType() StatementType { return StatementTypeTry }
func (s *TryStatement) GetContents() string {
	// Simplified
	return "try " + reconstructBody(s.Body)
}

// CatchStatement
func (s *CatchStatement) GetStatementType() StatementType { return StatementTypeCatch }
func (s *CatchStatement) GetContents() string             { return "catch" }

// FinallyStatement
func (s *FinallyStatement) GetStatementType() StatementType { return StatementTypeFinally }
func (s *FinallyStatement) GetContents() string             { return "finally" }

// ThrowStatement
func (s *ThrowStatement) GetStatementType() StatementType { return StatementTypeThrow }
func (s *ThrowStatement) GetContents() string             { return "throw" }

// GotoStatement
func (s *GotoStatement) GetStatementType() StatementType { return StatementTypeGoto }
func (s *GotoStatement) GetContents() string             { return "goto " + s.Label }

// LabelStatement
func (s *LabelStatement) GetStatementType() StatementType { return StatementTypeLabel }
func (s *LabelStatement) GetContents() string             { return s.Name + ":" }

// ImportStatement
func (s *ImportStatement) GetStatementType() StatementType { return StatementTypeImport }
func (s *ImportStatement) GetContents() string             { return "import " + s.ModulePath }

// YieldStatement
func (s *YieldStatement) GetStatementType() StatementType { return StatementTypeYield }
func (s *YieldStatement) GetContents() string             { return "yield" }

// AwaitStatement
func (s *AwaitStatement) GetStatementType() StatementType { return StatementTypeAwait }
func (s *AwaitStatement) GetContents() string             { return "await" }

// ExpressionStatement
func (s *ExpressionStatement) GetStatementType() StatementType { return StatementTypeExpression }
func (s *ExpressionStatement) GetContents() string             { return s.Expression.GetContents() }
