package specification

type ImportSpecification struct {
	Id       string `json:"Id,omitempty"`
	Path     string `json:"Path,omitempty"`
	Contents string `json:"-"`
}

func (i *ImportSpecification) GetId() string {
	return i.Id
}

func (i *ImportSpecification) GetSpecificationType() string {
	return "ImportSpecification"
}

func (i *ImportSpecification) GetContents() string {
	return i.Contents
}
