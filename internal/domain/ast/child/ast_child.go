package child

import (
	"refacto/internal/domain/ast/declaration"
	"refacto/internal/domain/ast/specification"
)

type (
	Child interface {
		GetId() string
		GetName() string
		GetPath() string
		GetSpecifications() []*specification.Specification
		GetDeclarations() []*declaration.Declaration
		GetRaw() string
	}
	File struct {
		Id             string
		Name           string
		Path           string
		Specifications []*specification.Specification
		Declarations   []*declaration.Declaration
		Contents       string
	}
)
