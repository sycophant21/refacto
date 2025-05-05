package root

import "refacto/internal/domain/ast/child"

func (p *Program) GetName() string {
	return p.Name
}
func (p *Program) GetChildren() []child.Child {
	children := make([]child.Child, len(p.Files))
	for _, file := range p.Files {
		children = append(children, file)
	}
	return children
}
