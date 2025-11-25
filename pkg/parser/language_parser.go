package parser

import (
	"refacto/internal/domain/ast"
	"refacto/internal/domain/ast/child"
)

type (
	LanguageParser interface {
		ParseProject(rootDir string) (*ast.AST, error)
		ParseFile(filePath string) (*child.File, error)
		GetLanguage() Language
	}
	Language int
)

func (l Language) String() string {
	var supportedLanguageList = []string{"Assembly", "C", "CPP", "CSharp", "Dart", "Erlang", "Go", "Java", "JavaScript", "Lisp", "OCaml", "Perl", "Python", "R", "Ruby", "Rust"}
	if l < Assembly || l > Rust {
		return "Unknown"
	}
	return supportedLanguageList[l]
}

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
