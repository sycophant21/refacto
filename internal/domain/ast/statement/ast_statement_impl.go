package statement

import (
	"refacto/internal/domain/ast/expression"
)

type (
	//ExpressionStatement          struct{}
	AssignmentStatement struct {
		Assignee   expression.Expression
		Expression expression.Expression
	}
	VariableDeclarationStatement struct {
	}
	ReturnStatement struct {
		ReturnExpression expression.Expression
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
		ElseExpression expression.Expression
		Body           []Statement
	}
	ForStatement struct {
		Initializer Statement
		Condition   expression.Expression
		Post        Statement
		Body        []Statement
	}
	WhileStatement struct {
		Body []Statement
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
		Body expression.Expression
	}
	TryStatement struct {
		Expression       *expression.Expression
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
		ModulePath string
		Alias      string
		Imports    []string
		Raw        string
	}
	YieldStatement struct {
		Expression expression.Expression
	}
	AwaitStatement struct {
		Expression expression.Expression
	}
)
