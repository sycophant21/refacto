package projection

import (
	"refacto/internal/domain/ast/declaration"
	"refacto/internal/domain/ast/expression"
	"refacto/internal/domain/ast/statement"
)

// Projector defines a high-level interface for projecting between the
// internal AST representation and the LLM-facing JSON shapes. This makes
// it easier to follow SOLID by depending on abstractions instead of the
// concrete functions in this package.
type Projector interface {
	ProjectExpression(expression.Expression) (map[string]any, error)
	ParseExpression(map[string]any) (expression.Expression, error)

	ProjectStatement(statement.Statement) (map[string]any, error)
	ParseStatement(map[string]any) (statement.Statement, error)

	ProjectDeclaration(declaration.Declaration) (map[string]any, error)
	ParseDeclaration(map[string]any) (declaration.Declaration, error)
}

// DefaultProjector is a thin adapter that implements Projector by
// delegating to the top-level functions in this package. It is the
// primary implementation used by the rest of the system.
type DefaultProjector struct{}

func (DefaultProjector) ProjectExpression(e expression.Expression) (map[string]any, error) {
	return ProjectExpression(e)
}

func (DefaultProjector) ParseExpression(m map[string]any) (expression.Expression, error) {
	return ParseExpression(m)
}

func (DefaultProjector) ProjectStatement(s statement.Statement) (map[string]any, error) {
	return ProjectStatement(s)
}

func (DefaultProjector) ParseStatement(m map[string]any) (statement.Statement, error) {
	return ParseStatement(m)
}

func (DefaultProjector) ProjectDeclaration(d declaration.Declaration) (map[string]any, error) {
	return ProjectDeclaration(d)
}

func (DefaultProjector) ParseDeclaration(m map[string]any) (declaration.Declaration, error) {
	return ParseDeclaration(m)
}
