package parser

import (
	"refacto/internal/domain/ast"
	"refacto/internal/domain/ast/declaration"
)

type (
	LanguageParser interface {
		ParseFile(filePath string) (*ast.AST, error)
		ExtractFunctions(ast *ast.AST) ([]*declaration.FunctionDeclaration, error)
	}
	Language int
)

const (
	Assembly Language = iota
	C
	CPP
	CSharp
	Dart
	Erlang
	Go
	Java
	JavaScript
	Lisp
	OCaml
	Perl
	Python
	R
	Ruby
	Rust
)
