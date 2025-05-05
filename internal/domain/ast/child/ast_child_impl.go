package child

import (
	"refacto/internal/domain/ast/declaration"
	"refacto/internal/domain/ast/specification"
)

func (f *File) GetId() string {
	return f.Id
}
func (f *File) GetName() string {
	return f.Name
}
func (f *File) GetPath() string {
	return f.Path
}
func (f *File) GetSpecifications() []*specification.Specification {
	return f.Specifications
}
func (f *File) GetDeclarations() []*declaration.Declaration {
	return f.Declarations
}
func (f *File) GetRaw() string {
	return f.Contents
}
