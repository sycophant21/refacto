package parser

type (
	Token interface {
		Literal() string
		Position() Position
		Language() Language
		SetUniqueIdentifier(id string)
	}
	Position struct {
		Column int
		Row    int
		Offset int
	}

	TokenType string
)
