package declaration

type (
	Declaration interface {
		GetId() string
		GetDeclarationType() string
		GetContents() string
	}
	BaseDeclaration struct {
		Id       string `json:"Id,omitempty"`
		Name     string `json:"Name,omitempty"`
		Contents string `json:"-"` // Exclude from JSON serialization
	}
)
