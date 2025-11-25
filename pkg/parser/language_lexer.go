package parser

type (
	LexingToken interface {
		Category() LexTokenCategory
		SubCategory() LexTokenSubCategory
		Value() string
		Position() Position
		Language() Language
	}
	LexTokenCategory interface {
		CategoryIdentifier() string
		Language() Language
		Value() string
	}
	LexTokenSubCategory interface {
		SubCategoryIdentifier() string
		Language() Language
		Value() string
	}
	Lexer interface {
		NextToken() LexingToken
	}
)
