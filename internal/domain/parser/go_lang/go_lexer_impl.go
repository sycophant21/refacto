package go_lang

import (
	"refacto/pkg/parser"
	"strings"
	"unicode/utf8"
)

// goSubcategories defines all Go lexer token subcategory descriptors.
var goSubcategories = [][]string{
	//Identifier
	{"IDENT", "IDENTIFIER"},
	//Literal
	{"INT", "INT_LITERAL"},
	{"FLOAT", "FLOAT_LITERAL"},
	{"STRING", "STRING_LITERAL"},
	{"RUNE", "RUNE_LITERAL"},
	//Keyword
	{"bool", "BOOL_KEYWORD"},
	{"break", "BREAK_KEYWORD"},
	{"byte", "BYTE_KEYWORD"},
	{"case", "CASE_KEYWORD"},
	{"chan", "CHAN_KEYWORD"},
	{"complex64", "COMPLEX_64_BIT_KEYWORD"},
	{"complex128", "COMPLEX_128_BIT_KEYWORD"},
	{"const", "CONST_KEYWORD"},
	{"continue", "CONTINUE_KEYWORD"},
	{"default", "DEFAULT_KEYWORD"},
	{"defer", "DEFER_KEYWORD"},
	{"else", "ELSE_KEYWORD"},
	{"fallthrough", "FALL_THROUGH_KEYWORD"},
	{"float32", "FLOAT_32_BIT_KEYWORD"},
	{"float64", "FLOAT_64_BIT_KEYWORD"},
	{"for", "FOR_KEYWORD"},
	{"func", "FUNC_KEYWORD"},
	{"go", "GO_KEYWORD"},
	{"goto", "GOTO_KEYWORD"},
	{"if", "IF_KEYWORD"},
	{"import", "IMPORT_KEYWORD"},
	{"int", "INT_KEYWORD"},
	{"int8", "INT_8_BIT_KEYWORD"},
	{"int16", "INT_16_BIT_KEYWORD"},
	{"int32", "INT_32_BIT_KEYWORD"},
	{"int64", "INT_64_BIT_KEYWORD"},
	{"interface", "INTERFACE_KEYWORD"},
	{"map", "MAP_KEYWORD"},
	{"package", "PACKAGE_KEYWORD"},
	{"range", "RANGE_KEYWORD"},
	{"return", "RETURN_KEYWORD"},
	{"rune", "RUNE_KEYWORD"},
	{"select", "SELECT_KEYWORD"},
	{"string", "STRING_KEYWORD"},
	{"struct", "STRUCT_KEYWORD"},
	{"switch", "SWITCH_KEYWORD"},
	{"type", "TYPE_KEYWORD"},
	{"uint", "UNSIGNED_INT_KEYWORD"},
	{"uint8", "UNSIGNED_INT_8_BIT_KEYWORD"},
	{"uint16", "UNSIGNED_INT_16_BIT_KEYWORD"},
	{"uint32", "UNSIGNED_INT_32_BIT_KEYWORD"},
	{"uint64", "UNSIGNED_INT_64_BIT_KEYWORD"},
	{"uintptr", "UNSIGNED_POINTER_KEYWORD"},
	{"var", "VAR_KEYWORD"},
	//Operator
	//Arithmetic
	{"+", "ADD_ARITHMETIC_OPERATOR"},
	{"-", "SUBTRACT_ARITHMETIC_OPERATOR"},
	{"*", "MULTIPLY_ARITHMETIC_OPERATOR"},
	{"/", "DIVIDE_ARITHMETIC_OPERATOR"},
	{"%", "MODULO_ARITHMETIC_OPERATOR"},
	//Assignment
	{"=", "ASSIGN_ASSIGNMENT_OPERATOR"},
	{"+=", "ADD_ASSIGN_ASSIGNMENT_OPERATOR"},
	{"-=", "SUBTRACT_ASSIGN_ASSIGNMENT_OPERATOR"},
	{"*=", "MULTIPLY_ASSIGN_ASSIGNMENT_OPERATOR"},
	{"/=", "DIVIDE_ASSIGN_ASSIGNMENT_OPERATOR"},
	{"%=", "MODULO_ASSIGNMENT_OPERATOR"},
	{"&=", "AND_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR"},
	{"|=", "OR_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR"},
	{"^=", "XOR_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR"},
	{":=", "SHORT_ASSIGN_ASSIGNMENT_OPERATOR"},
	//Comparison
	{"==", "EQUALS_COMPARSION_OPERATOR"},
	{"!=", "NOT_EQUALS_COMPARSION_OPERATOR"},
	{"<", "LESS_THAN_COMPARSION_OPERATOR"},
	{">", "GREATER_THAN_COMPARSION_OPERATOR"},
	{"<=", "LESS_THAN_EQUALS_COMPARSION_OPERATOR"},
	{">=", "GREATER_THAN_EQUALS_COMPARSION_OPERATOR"},
	//Logical
	{"&&", "AND_LOGICAL_OPERATOR"},
	{"||", "OR_LOGICAL_OPERATOR"},
	{"!", "NOT_LOGICAL_OPERATOR"},
	//Bitwise
	{"&", "AND_BITWISE_OPERATOR"},
	{"|", "OR_BITWISE_OPERATOR"},
	{"^", "XOR_BITWISE_OPERATOR"},
	{"<<", "SHIFT_LEFT_BITWISE_OPERATOR"},
	{">>", "SHIFT_RIGHT_BITWISE_OPERATOR"},
	//Punctuation
	{"(", "LEFT_PARENTHESIS_PUNCTUATION"},
	{")", "RIGHT_PARENTHESIS_PUNCTUATION"},
	{"{", "LEFT_BRACES_PUNCTUATION"},
	{"}", "RIGHT_BRACES_PUNCTUATION"},
	{"[", "LEFT_BRACKET_PUNCTUATION"},
	{"]", "RIGHT_BRACKET_PUNCTUATION"},
	{",", "COMMA_PUNCTUATION"},
	{".", "DOT_PUNCTUATION"},
	{";", "SEMI_COLON_PUNCTUATION"},
	{":", "COLON_PUNCTUATION"},
	{"...", "ELLIPSIS_PUNCTUATION"},
	//Special
	{"\n", "NEW_LINE_SPECIAL_CONSTRUCT"},
	{"EOF", "EOF_SPECIAL_CONSTRUCT"},
	{"//", "LINE_COMMENT_SPECIAL_CONSTRUCT"},
	{"/*", "BLOCK_COMMENT_START_SPECIAL_CONSTRUCT"},
	{"*/", "BLOCK_COMMENT_END_SPECIAL_CONSTRUCT"},
	//ERROR/INVALID
	{"ERR", "ILLEGAL"},
}

type scanHandler func(*Lexer, parser.Position) parser.LexingToken

var scanHandlers map[rune]scanHandler

func init() {
	initGoLexerTables(goSubcategories)
	initScanHandlers()
}

func initScanHandlers() {
	scanHandlers = map[rune]scanHandler{
		'=': scanEqualsToken,
		'+': scanPlusToken,
		'-': scanMinusToken,
		'*': scanStarToken,
		'/': scanSlashToken,
		'<': scanLessToken,
		'>': scanGreaterToken,
		'&': scanAmpersandToken,
		'|': scanPipeToken,
		'^': scanCaretToken,
		';': scanSemicolonToken,
		'(': scanLeftParenToken,
		')': scanRightParenToken,
		',': scanCommaToken,
		'.': scanDotToken,
		'{': scanLeftBraceToken,
		'}': scanRightBraceToken,
		'[': scanLeftBracketToken,
		']': scanRightBracketToken,
		':': scanColonToken,
		'"': scanStringToken,
		0:   scanEOFToken,
	}
}

// initGoLexerTables populates the global lexer category and keyword tables
// from the provided subcategory definitions.
func initGoLexerTables(subcategories [][]string) {
	for i, sc := range subcategories {
		categoryName := strings.Split(sc[1], "_")
		category := &goLexTokenCategoryWrapper{Category: GoLexTokenCategory(i), Value: categoryName[len(categoryName)-1]}
		if c, exists := categoriesMap[category.Value]; !exists {
			categoriesMap[category.Value] = category
		} else {
			category = c
		}

		if _, exists := categoriesIndexMap[category.Category]; !exists {
			categoriesIndexMap[category.Category] = category
		}

		subCatWrapper := &goLexTokenSubCategoryWrapper{SubCategory: GoLexTokenSubCategory(i), Value: sc[1]}
		if _, exists := subcategoriesIndexMap[subCatWrapper.SubCategory]; !exists {
			subcategoriesIndexMap[subCatWrapper.SubCategory] = subCatWrapper
		}

		// Populate keywords map
		if category.Value == "KEYWORD" {
			keywords[sc[0]] = GoLexTokenSubCategory(i)
		}
	}
}

func NewLexer(input string) *Lexer {
	l := &Lexer{input: input, line: 1, column: 0}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		r, width := utf8.DecodeRuneInString(l.input[l.readPosition:])
		l.ch = r
		l.position = l.readPosition
		l.readPosition += width
		l.column++
	}
}

func (l *Lexer) peekChar() rune {
	if l.readPosition >= len(l.input) {
		return 0
	}
	r, _ := utf8.DecodeRuneInString(l.input[l.readPosition:])
	return r
}

func (l *Lexer) NextToken() parser.LexingToken {
	l.skipWhitespace()
	pos := parser.Position{Row: l.line, Column: l.column, Offset: l.position}
	return l.scanTokenAt(pos)
}

// scanTokenAt delegates tokenization of the current rune to a handler function
// and falls back to the default scanner for identifiers, numbers, and illegal
// characters.
func (l *Lexer) scanTokenAt(pos parser.Position) parser.LexingToken {
	if handler, ok := scanHandlers[l.ch]; ok {
		return handler(l, pos)
	}
	return l.scanDefaultToken(pos)
}

// scanDefaultToken handles identifiers, numbers, EOF, and illegal characters
// when there is no specific handler registered for the current rune.
func (l *Lexer) scanDefaultToken(pos parser.Position) parser.LexingToken {
	if isLetter(l.ch) {
		literal := l.readIdentifier()
		subCat := l.lookupIdent(literal)
		tok := l.newToken(subCat, literal)
		tok.position = pos
		return tok
	}

	if isDigit(l.ch) {
		literal := l.readNumber()
		tok := l.newToken(GoLexTokenSubCategory(INT), literal)
		tok.position = pos
		return tok
	}

	// Fallback: illegal single character
	tok := l.newToken(GoLexTokenSubCategory(ILLEGAL), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

// --- Single-rune scan handlers ---

func scanEqualsToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	if l.peekChar() == '=' {
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(EQUALS_COMPARSION_OPERATOR), string(ch)+string(l.ch)) // ==
		l.readChar()
	} else {
		tok = l.newToken(GoLexTokenSubCategory(ASSIGN_ASSIGNMENT_OPERATOR), string(l.ch)) // =
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanPlusToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	if l.peekChar() == '=' {
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(ADD_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch)) // +=
		l.readChar()
	} else {
		tok = l.newToken(GoLexTokenSubCategory(ADD_ARITHMETIC_OPERATOR), string(l.ch)) // +
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanMinusToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	if l.peekChar() == '=' {
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(SUBTRACT_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch)) // -=
		l.readChar()
	} else {
		tok = l.newToken(GoLexTokenSubCategory(SUBTRACT_ARITHMETIC_OPERATOR), string(l.ch)) // -
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanStarToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	if l.peekChar() == '=' {
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(MULTIPLY_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch)) // *=
		l.readChar()
	} else {
		tok = l.newToken(GoLexTokenSubCategory(MULTIPLY_ARITHMETIC_OPERATOR), string(l.ch)) // *
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanSlashToken(l *Lexer, pos parser.Position) parser.LexingToken {
	// /=
	if l.peekChar() == '=' {
		ch := l.ch
		l.readChar()
		tok := l.newToken(GoLexTokenSubCategory(DIVIDE_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
		tok.position = pos
		return tok
	}

	// Line comment //
	if l.peekChar() == '/' {
		l.skipLineComment()
		return l.NextToken()
	}

	// Block comment /* */
	if l.peekChar() == '*' {
		l.skipBlockComment()
		return l.NextToken()
	}

	// Simple divide operator
	tok := l.newToken(GoLexTokenSubCategory(DIVIDE_ARITHMETIC_OPERATOR), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanLessToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	peek := l.peekChar()
	if peek == '=' {
		// <=
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(LESS_THAN_EQUALS_COMPARSION_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else if peek == '<' {
		// << shift-left operator. We tokenize `<<=` as `<<` + `=` for now.
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(SHIFT_LEFT_BITWISE_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else {
		// For now treat `<-` as `<` followed by `-` and any other single '<' as '<'.
		tok = l.newToken(GoLexTokenSubCategory(LESS_THAN_COMPARSION_OPERATOR), string(l.ch))
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanGreaterToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	peek := l.peekChar()
	if peek == '=' {
		// >=
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(GREATER_THAN_EQUALS_COMPARSION_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else if peek == '>' {
		// >> shift-right operator. We tokenize `>>=` as `>>` + `=` for now.
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(SHIFT_RIGHT_BITWISE_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else {
		tok = l.newToken(GoLexTokenSubCategory(GREATER_THAN_COMPARSION_OPERATOR), string(l.ch))
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanSemicolonToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(SEMI_COLON_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanAmpersandToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	peek := l.peekChar()
	if peek == '&' {
		// && logical AND
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(AND_LOGICAL_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else if peek == '=' {
		// &= bitwise-AND assignment
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(AND_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else {
		// & bitwise AND / address-of (unary handled at parse time)
		tok = l.newToken(GoLexTokenSubCategory(AND_BITWISE_OPERATOR), string(l.ch))
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanPipeToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	peek := l.peekChar()
	if peek == '|' {
		// || logical OR
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(OR_LOGICAL_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else if peek == '=' {
		// |= bitwise-OR assignment
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(OR_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else {
		// | bitwise OR
		tok = l.newToken(GoLexTokenSubCategory(OR_BITWISE_OPERATOR), string(l.ch))
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanCaretToken(l *Lexer, pos parser.Position) parser.LexingToken {
	var tok *GoLexToken
	if l.peekChar() == '=' {
		// ^= bitwise-XOR assignment
		ch := l.ch
		l.readChar()
		tok = l.newToken(GoLexTokenSubCategory(XOR_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch))
		l.readChar()
	} else {
		// ^ bitwise XOR / unary bitwise NOT
		tok = l.newToken(GoLexTokenSubCategory(XOR_BITWISE_OPERATOR), string(l.ch))
		l.readChar()
	}
	tok.position = pos
	return tok
}

func scanLeftParenToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(LEFT_PARENTHESIS_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanRightParenToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(RIGHT_PARENTHESIS_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanCommaToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(COMMA_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanDotToken(l *Lexer, pos parser.Position) parser.LexingToken {
	// Ellipsis or illegal `..`
	if l.peekChar() == '.' {
		l.readChar()
		if l.peekChar() == '.' {
			l.readChar()
			tok := l.newToken(GoLexTokenSubCategory(ELLIPSIS_PUNCTUATION), "...")
			l.readChar()
			tok.position = pos
			return tok
		}
		tok := l.newToken(GoLexTokenSubCategory(ILLEGAL), "..")
		l.readChar()
		tok.position = pos
		return tok
	}

	// Single dot
	tok := l.newToken(GoLexTokenSubCategory(DOT_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanLeftBraceToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(LEFT_BRACES_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanRightBraceToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(RIGHT_BRACES_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanLeftBracketToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(LEFT_BRACKET_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanRightBracketToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(RIGHT_BRACKET_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanColonToken(l *Lexer, pos parser.Position) parser.LexingToken {
	if l.peekChar() == '=' {
		ch := l.ch
		l.readChar()
		tok := l.newToken(GoLexTokenSubCategory(SHORT_ASSIGN_ASSIGNMENT_OPERATOR), string(ch)+string(l.ch)) // :=
		l.readChar()
		tok.position = pos
		return tok
	}

	tok := l.newToken(GoLexTokenSubCategory(COLON_PUNCTUATION), string(l.ch))
	l.readChar()
	tok.position = pos
	return tok
}

func scanStringToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.readString()
	tok.position = pos
	return tok
}

func scanEOFToken(l *Lexer, pos parser.Position) parser.LexingToken {
	tok := l.newToken(GoLexTokenSubCategory(EOF_SPECIAL_CONSTRUCT), "")
	tok.position = pos
	return tok
}

func (l *Lexer) newToken(subCat GoLexTokenSubCategory, literal string) *GoLexToken {
	scWrapper := subcategoriesIndexMap[subCat]

	var catWrapper *goLexTokenCategoryWrapper
	// Simple heuristic based on ranges or name
	if strings.HasSuffix(scWrapper.Value, "KEYWORD") {
		catWrapper = categoriesMap["KEYWORD"]
	} else if strings.HasSuffix(scWrapper.Value, "LITERAL") {
		catWrapper = categoriesMap["LITERAL"]
	} else if strings.HasSuffix(scWrapper.Value, "OPERATOR") {
		catWrapper = categoriesMap["OPERATOR"]
	} else if strings.HasSuffix(scWrapper.Value, "PUNCTUATION") {
		catWrapper = categoriesMap["PUNCTUATION"]
	} else if scWrapper.Value == "IDENTIFIER" {
		catWrapper = categoriesMap["IDENTIFIER"]
	} else {
		catWrapper = categoriesMap["SPECIAL"]
	}

	if catWrapper == nil {
		// Fallback
		catWrapper = &goLexTokenCategoryWrapper{Value: "UNKNOWN"}
	}

	return &GoLexToken{
		category:    *catWrapper,
		subCategory: *scWrapper,
		value:       literal,
	}
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readNumber() string {
	// Read an integer literal, including Go-style base prefixes and underscores.
	// Examples: 123, 0xFF, 0b1010, 0o755.
	position := l.position

	// Always consume the leading digit(s).
	for isDigit(l.ch) {
		l.readChar()
	}

	// Allow base prefixes and hex/octal/binary digits as part of the same token.
	for isNumberChar(l.ch) {
		l.readChar()
	}

	return l.input[position:l.position]
}

// isNumberChar reports whether a rune can appear in a Go integer literal
// after the initial digit run. We are permissive here and allow hexadecimal
// letters and base designators; strconv.ParseInt with base 0 will validate.
func isNumberChar(ch rune) bool {
	// Allow digits, underscores, hex digits, and base-prefix letters inside
	// a numeric literal after the initial digit run.
	if isDigit(ch) || ch == '_' {
		return true
	}
	if ch >= 'a' && ch <= 'f' {
		return true
	}
	if ch >= 'A' && ch <= 'F' {
		return true
	}
	// Base prefixes o, O (for 0o / 0O style octal) inside the literal.
	if ch == 'o' || ch == 'O' {
		return true
	}
	return false
}

func (l *Lexer) readString() *GoLexToken {
	// Advance past opening quote
	l.readChar()

	position := l.position
	for {
		if l.ch == '"' || l.ch == 0 {
			break
		}
		l.readChar()
	}
	literal := l.input[position:l.position]

	// Consume the closing quote
	if l.ch == '"' {
		l.readChar()
	}

	return l.newToken(GoLexTokenSubCategory(STRING), literal) // STRING_LITERAL
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		if l.ch == '\n' {
			l.line++
			l.column = 0
		}
		l.readChar()
	}
}

func (l *Lexer) skipLineComment() {
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
	l.skipWhitespace()
}

func (l *Lexer) skipBlockComment() {
	// consume /*
	l.readChar()
	l.readChar()

	for {
		if l.ch == 0 {
			break
		}
		if l.ch == '*' && l.peekChar() == '/' {
			l.readChar()
			l.readChar()
			break
		}
		if l.ch == '\n' {
			l.line++
			l.column = 0
		}
		l.readChar()
	}
	l.skipWhitespace()
}

func (l *Lexer) lookupIdent(ident string) GoLexTokenSubCategory {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return GoLexTokenSubCategory(IDENT) // IDENT
}

func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

// Interface implementations

func (g *GoLexToken) Category() parser.LexTokenCategory {
	return g.category.Category
}
func (g *GoLexToken) CategoryName() string {
	return g.category.Category.Value()
}
func (g *GoLexToken) SubCategory() parser.LexTokenSubCategory {
	return g.subCategory.SubCategory
}
func (g *GoLexToken) SubCategoryName() string {
	return g.subCategory.SubCategory.Value()
}
func (g *GoLexToken) Value() string {
	return g.value
}
func (g *GoLexToken) Position() parser.Position {
	return g.position
}
func (g *GoLexToken) Language() parser.Language {
	return parser.Go
}

func (g GoLexTokenCategory) CategoryIdentifier() string {
	return g.Language().String() + g.Value()
}
func (g GoLexTokenCategory) Language() parser.Language {
	return parser.Go
}
func (g GoLexTokenCategory) Value() string {
	if w, ok := categoriesIndexMap[g]; ok {
		return w.Value
	}
	return "UNKNOWN"
}

func (g GoLexTokenSubCategory) SubCategoryIdentifier() string {
	return g.Language().String() + g.Value()
}
func (g GoLexTokenSubCategory) Language() parser.Language {
	return parser.Go
}
func (g GoLexTokenSubCategory) Value() string {
	if w, ok := subcategoriesIndexMap[g]; ok {
		return w.Value
	}
	return "UNKNOWN"
}
