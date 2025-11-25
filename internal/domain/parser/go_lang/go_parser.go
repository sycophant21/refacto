package go_lang

import (
	"refacto/internal/domain/ast/declaration"
	"refacto/internal/domain/ast/expression"
	logg "refacto/internal/util/log"
	"refacto/pkg/parser"
)

var (
	go_parser *GoParser
	logger    = logg.Logger
)

type GoParser struct{}

// Recursive Descent Parser
type (
	prefixParseFn func() expression.Expression
	infixParseFn  func(expression.Expression) expression.Expression
)

const (
	LOWEST        int = iota
	OR_LOGICAL        // ||
	AND_LOGICAL       // &&
	EQUALS            // ==
	LESSGREATER       // > or <
	SUM               // +, |, ^
	BITWISE_SHIFT     // <<, >>, & (binary)
	PRODUCT           // *
	PREFIX            // -X or !X
	CALL              // myFunction(X) or obj.Method
)

type Parser struct {
	l           parser.Lexer
	curToken    parser.LexingToken
	peekToken   parser.LexingToken
	filePath    string
	content     string
	packageName string
	errors      []string

	prefixParseFns map[parser.LexTokenSubCategory]prefixParseFn
	infixParseFns  map[parser.LexTokenSubCategory]infixParseFn

	// Type linking infrastructure
	typeRegistry      map[string]*declaration.TypeDeclaration
	pendingReferences []struct {
		typeName     string
		reference    *declaration.TypeReference
		ownerContext string
	}
}

var precedenceMap = map[parser.LexTokenSubCategory]int{
	GoLexTokenSubCategory(EQUALS_COMPARSION_OPERATOR):              EQUALS,
	GoLexTokenSubCategory(NOT_EQUALS_COMPARSION_OPERATOR):          EQUALS,
	GoLexTokenSubCategory(LESS_THAN_COMPARSION_OPERATOR):           LESSGREATER,
	GoLexTokenSubCategory(GREATER_THAN_COMPARSION_OPERATOR):        LESSGREATER,
	GoLexTokenSubCategory(LESS_THAN_EQUALS_COMPARSION_OPERATOR):    LESSGREATER,
	GoLexTokenSubCategory(GREATER_THAN_EQUALS_COMPARSION_OPERATOR): LESSGREATER,
	GoLexTokenSubCategory(ADD_ARITHMETIC_OPERATOR):                 SUM,
	GoLexTokenSubCategory(SUBTRACT_ARITHMETIC_OPERATOR):            SUM,
	GoLexTokenSubCategory(OR_BITWISE_OPERATOR):                     SUM,
	GoLexTokenSubCategory(XOR_BITWISE_OPERATOR):                    SUM,
	GoLexTokenSubCategory(AND_BITWISE_OPERATOR):                    BITWISE_SHIFT,
	GoLexTokenSubCategory(SHIFT_LEFT_BITWISE_OPERATOR):             BITWISE_SHIFT,
	GoLexTokenSubCategory(SHIFT_RIGHT_BITWISE_OPERATOR):            BITWISE_SHIFT,
	GoLexTokenSubCategory(MULTIPLY_ARITHMETIC_OPERATOR):            PRODUCT,
	GoLexTokenSubCategory(DIVIDE_ARITHMETIC_OPERATOR):              PRODUCT,
	GoLexTokenSubCategory(MODULO_ARITHMETIC_OPERATOR):              PRODUCT,
	GoLexTokenSubCategory(AND_LOGICAL_OPERATOR):                    AND_LOGICAL,
	GoLexTokenSubCategory(OR_LOGICAL_OPERATOR):                     OR_LOGICAL,
	GoLexTokenSubCategory(LEFT_PARENTHESIS_PUNCTUATION):            CALL,
	GoLexTokenSubCategory(DOT_PUNCTUATION):                         CALL,
}
