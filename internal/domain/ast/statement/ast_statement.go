package statement

type (
	Statement interface {
		GetContents() string
		GetStatementType() StatementType
	}
	StatementType int
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
)
