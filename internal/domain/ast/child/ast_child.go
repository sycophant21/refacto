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
		Id             string                         `json:"Id,omitempty"`
		Name           string                         `json:"Name,omitempty"`
		Path           string                         `json:"Path,omitempty"`
		Specifications []*specification.Specification `json:"Specifications,omitempty"`
		Declarations   []*declaration.Declaration     `json:"Declarations,omitempty"`
		Contents       string                         `json:"-"`
	}
)
