package root

import "refacto/internal/domain/ast/child"

type (
	Root interface {
		GetName() string
		GetChildren() []child.Child
	}
	Program struct {
		Name  string
		Files []*child.File
	}
)
