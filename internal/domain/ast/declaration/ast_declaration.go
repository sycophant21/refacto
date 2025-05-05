package declaration

type (
	Declaration interface {
		GetId() string
		GetDeclarationType() string
		GetContents() string
	}
	BaseDeclaration struct {
		Id       string
		Name     string
		Contents string
	}
)

func (b *BaseDeclaration) GetId() string {
	return b.Id
}

func (b *BaseDeclaration) GetContents() string {
	return b.Contents
}

func (f *FunctionDeclaration) GetDeclarationType() string {
	return "Function"
}

func (t *TypeDeclaration) GetDeclarationType() string {
	return "Type"
}
func (v *VariableDeclaration) GetDeclarationType() string {
	return "Variable"
}
