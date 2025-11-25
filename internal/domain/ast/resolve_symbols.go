package ast

import (
	"fmt"
	"refacto/internal/domain/ast/child"
	"refacto/internal/domain/ast/declaration"
	"refacto/internal/domain/ast/expression"
	"refacto/internal/domain/ast/statement"
)

// ResolveFileSymbols walks a parsed File and attaches SymbolBinding metadata
// to identifiers wherever possible (vars, params, range headers, locals).
//
// It is language-agnostic and works purely on the shared AST model.
func ResolveFileSymbols(file *child.File) {
	if file == nil {
		return
	}

	// File-scope symbols: top-level variables and functions.
	fileScope := make(symbolTable)

	for _, dptr := range file.Declarations {
		if dptr == nil {
			continue
		}
		decl := *dptr
		switch dd := decl.(type) {
		case *declaration.VariableDeclaration:
			if dd.Name != "" {
				fileScope[dd.Name] = &bindingInfo{
					Kind:   expression.SymbolVar,
					DeclId: dd.Id,
				}
			}
		case *declaration.FunctionDeclaration:
			// Register function name so calls can be bound later if desired.
			if dd.Name != "" && dd.Id != "" {
				fileScope[dd.Name] = &bindingInfo{
					Kind:   expression.SymbolFunc,
					DeclId: dd.Id,
				}
			}
		}
	}

	// Now resolve inside function bodies.
	for _, dptr := range file.Declarations {
		if dptr == nil {
			continue
		}
		if fn, ok := (*dptr).(*declaration.FunctionDeclaration); ok {
			resolveFunctionSymbols(fn, fileScope)
		}
	}
}

type symbolTable map[string]*bindingInfo

type bindingInfo struct {
	Kind   expression.SymbolKind
	DeclId string
}

func resolveFunctionSymbols(fn *declaration.FunctionDeclaration, fileScope symbolTable) {
	if fn == nil {
		return
	}

	// Start with a copy of the file scope so function-local names shadow globals.
	fnScope := make(symbolTable)
	for k, v := range fileScope {
		fnScope[k] = v
	}

	// Register parameters.
	for _, p := range fn.Parameters {
		if p == nil || p.Name == "" {
			continue
		}
		// Synthetic decl id if none exists.
		declId := p.Id
		if declId == "" {
			declId = fmt.Sprintf("%s:param:%s", fn.Id, p.Name)
		}
		fnScope[p.Name] = &bindingInfo{
			Kind:   expression.SymbolParam,
			DeclId: declId,
		}
	}

	// Walk the function body statements with a lexical scope.
	walkStatements(fn.Body, fnScope)
}

// walkStatements walks a slice of statements, updating and using the given scope.
func walkStatements(stmts []statement.Statement, scope symbolTable) {
	for _, st := range stmts {
		if st == nil {
			continue
		}
		walkStatement(st, scope)
	}
}

func walkStatement(st statement.Statement, scope symbolTable) {
	switch s := st.(type) {
	case *statement.VariableDeclarationStatement:
		// First resolve RHS in current scope, then introduce the new name.
		if s.Expression != nil {
			walkExpression(s.Expression, scope)
		}
		if s.Name != "" {
			id := fmt.Sprintf("local:%s", s.Name)
			scope[s.Name] = &bindingInfo{
				Kind:   expression.SymbolVar,
				DeclId: id,
			}
		}
	case *statement.AssignmentStatement:
		if s.Assignee != nil {
			walkExpression(s.Assignee, scope)
		}
		if s.Expression != nil {
			walkExpression(s.Expression, scope)
		}
	case *statement.ExpressionStatement:
		if s.Expression != nil {
			walkExpression(s.Expression, scope)
		}
	case *statement.ReturnStatement:
		for _, e := range s.ReturnExpressions {
			if e != nil {
				walkExpression(e, scope)
			}
		}
	case *statement.IfStatement:
		if s.IfExpression != nil {
			walkExpression(s.IfExpression, scope)
		}
		// New block scope for body
		inner := cloneScope(scope)
		walkStatements(s.Body, inner)
	case *statement.ForStatement:
		// Handle header slots; range headers may introduce new vars.
		inner := cloneScope(scope)
		if s.Header.Pre != nil {
			walkStatement(s.Header.Pre, inner)
		}
		if s.Header.Core != nil {
			if rh, ok := s.Header.Core.(*statement.RangeHeaderStatement); ok {
				// Range header: Key/Value introduce new loop vars in inner scope.
				if id, ok2 := rh.Key.(*expression.Identifier); ok2 && id.Value != "" {
					inner[id.Value] = &bindingInfo{
						Kind:   expression.SymbolVar,
						DeclId: fmt.Sprintf("for:key:%s", id.Value),
					}
				}
				if id, ok2 := rh.Value.(*expression.Identifier); ok2 && id.Value != "" {
					inner[id.Value] = &bindingInfo{
						Kind:   expression.SymbolVar,
						DeclId: fmt.Sprintf("for:value:%s", id.Value),
					}
				}
				if rh.Src != nil {
					walkExpression(rh.Src, inner)
				}
			} else {
				walkStatement(s.Header.Core, inner)
			}
		}
		if s.Header.Post != nil {
			walkStatement(s.Header.Post, inner)
		}
		walkStatements(s.Body, inner)
	case *statement.BlockStatement:
		inner := cloneScope(scope)
		walkStatements(s.Body, inner)
	case *statement.DeferStatement:
		if s.Call != nil {
			walkExpression(s.Call, scope)
		}
		// Other statements can be added here as needed.
	}
}

func walkExpression(expr expression.Expression, scope symbolTable) {
	switch e := expr.(type) {
	case *expression.Identifier:
		if b, ok := scope[e.Value]; ok {
			e.Binding = &expression.SymbolBinding{
				Kind:   b.Kind,
				DeclId: b.DeclId,
			}
		}
	case *expression.IntegerLiteral,
		*expression.StringLiteral,
		*expression.BooleanLiteral:
		// nothing
	case *expression.PrefixExpression:
		if e.Right != nil {
			walkExpression(e.Right, scope)
		}
	case *expression.InfixExpression:
		if e.Left != nil {
			walkExpression(e.Left, scope)
		}
		if e.Right != nil {
			walkExpression(e.Right, scope)
		}
	case *expression.ComparisonExpression:
		if e.Left != nil {
			walkExpression(e.Left, scope)
		}
		if e.Right != nil {
			walkExpression(e.Right, scope)
		}
	case *expression.CallExpression:
		if e.Function != nil {
			walkExpression(e.Function, scope)
		}
		for _, a := range e.Arguments {
			if a != nil {
				walkExpression(a, scope)
			}
		}
	case *expression.SelectorExpression:
		if e.Operand != nil {
			walkExpression(e.Operand, scope)
		}
		// Selector is an Identifier but usually a field/method name; not bound here.
	}
}

func cloneScope(src symbolTable) symbolTable {
	out := make(symbolTable)
	for k, v := range src {
		out[k] = v
	}
	return out
}
