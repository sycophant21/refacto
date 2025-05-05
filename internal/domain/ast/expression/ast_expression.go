package expression

type (
	Expression interface {
		GetContents() string
		GetExpressionType()
	}
)
