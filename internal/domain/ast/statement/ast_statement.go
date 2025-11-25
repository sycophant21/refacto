package statement

type (
	Statement interface {
		GetContents() string
		GetStatementType() StatementType
	}
	StatementType int
	AssignOpKind  int
)

const (
	AssignOpUnknown AssignOpKind = iota
	AssignOpEq
	AssignOpAdd
	AssignOpSub
	AssignOpMul
	AssignOpDiv
	AssignOpMod
	AssignOpAnd
	AssignOpOr
	AssignOpXor
	AssignOpShiftLeft
	AssignOpShiftRight
)

const (
	StatementTypeExpression StatementType = iota
	StatementTypeAssignment
	StatementTypeVarDecl
	StatementTypeReturn
	StatementTypeIf
	StatementTypeElse
	StatementTypeElseIf
	StatementTypeFor
	StatementTypeWhile
	StatementTypeDoWhile
	StatementTypeSwitch
	StatementTypeCase
	StatementTypeSelect
	StatementTypeDefault
	StatementTypeBreak
	StatementTypeContinue
	StatementTypeBlock
	StatementTypeDefer
	StatementTypeTry
	StatementTypeCatch
	StatementTypeFinally
	StatementTypeThrow
	StatementTypeGoto
	StatementTypeLabel
	StatementTypeImport
	StatementTypeYield
	StatementTypeAwait
	StatementTypeForHeader
	// Appended statement kinds for Go-specific control-flow extensions.
	StatementTypeFallthrough
	StatementTypeGo
)

// --- JSON marshaling for statement enums: serialize as strings ---
