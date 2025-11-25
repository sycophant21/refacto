package go_lang

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"refacto/internal/domain/ast"
	"refacto/internal/domain/ast/child"
	"refacto/internal/domain/ast/declaration"
	"refacto/internal/domain/ast/expression"
	"refacto/internal/domain/ast/root"
	"refacto/internal/domain/ast/specification"
	"refacto/internal/domain/ast/statement"
	program_utils "refacto/internal/util/program"
	"refacto/pkg/parser"
)

func NewGoParser() *GoParser {
	if go_parser == nil {
		go_parser = &GoParser{}
	}
	return go_parser
}

func (g *GoParser) ParseProject(rootDir string) (*ast.AST, error) {
	program := &root.Program{
		Name:  filepath.Base(rootDir),
		Files: []*child.File{},
	}
	err := filepath.WalkDir(rootDir, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			logger.Error(fmt.Sprintf("Error accessing path %q: %v\n", path, err), true)
			return nil
		}
		if d.IsDir() {
			return nil
		}
		if !program_utils.HasValidExtension(path, []string{".go"}) {
			return nil
		}
		file, err := g.ParseFile(path)
		if err != nil {
			logger.Error(fmt.Sprintf("Parse error in file %s: %v\n", path, err), true)
			return nil
		}
		program.Files = append(program.Files, file)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return &ast.AST{Root: program}, nil
}

func (g *GoParser) ParseFile(filePath string) (*child.File, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	lexer := NewLexer(string(content))
	p := NewParser(lexer, filePath, string(content))
	file, err := p.Parse()
	if err != nil {
		return nil, err
	}

	// Attach symbol bindings (name resolution) in a language-agnostic way.
	ast.ResolveFileSymbols(file)
	return file, nil
}

func (g *GoParser) GetLanguage() parser.Language {
	return parser.Go
}

func NewParser(l parser.Lexer, filePath string, content string) *Parser {
	p := &Parser{
		l:              l,
		filePath:       filePath,
		content:        content,
		prefixParseFns: make(map[parser.LexTokenSubCategory]prefixParseFn),
		infixParseFns:  make(map[parser.LexTokenSubCategory]infixParseFn),
		typeRegistry:   make(map[string]*declaration.TypeDeclaration),
		pendingReferences: make([]struct {
			typeName     string
			reference    *declaration.TypeReference
			ownerContext string
		}, 0),
	}

	// Register parsing functions
	p.registerPrefix(GoLexTokenSubCategory(IDENT), p.parseIdentifier)
	p.registerPrefix(GoLexTokenSubCategory(INT), p.parseIntegerLiteral)
	p.registerPrefix(GoLexTokenSubCategory(STRING), p.parseStringLiteral)
	p.registerPrefix(GoLexTokenSubCategory(BOOL_KEYWORD), p.parseBooleanLiteral) // Assuming bool keyword acts as type, but true/false are identifiers in Go? No, true/false are predeclared constants.
	// For simplicity, treating true/false as identifiers for now unless we have specific tokens for them.

	p.registerPrefix(GoLexTokenSubCategory(NOT_LOGICAL_OPERATOR), p.parsePrefixExpression)
	p.registerPrefix(GoLexTokenSubCategory(SUBTRACT_ARITHMETIC_OPERATOR), p.parsePrefixExpression)
	p.registerPrefix(GoLexTokenSubCategory(LEFT_PARENTHESIS_PUNCTUATION), p.parseGroupedExpression)

	p.registerInfix(GoLexTokenSubCategory(ADD_ARITHMETIC_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(SUBTRACT_ARITHMETIC_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(MULTIPLY_ARITHMETIC_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(DIVIDE_ARITHMETIC_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(MODULO_ARITHMETIC_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(EQUALS_COMPARSION_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(NOT_EQUALS_COMPARSION_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(LESS_THAN_COMPARSION_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(GREATER_THAN_COMPARSION_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(LESS_THAN_EQUALS_COMPARSION_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(GREATER_THAN_EQUALS_COMPARSION_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(AND_LOGICAL_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(OR_LOGICAL_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(AND_BITWISE_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(OR_BITWISE_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(XOR_BITWISE_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(SHIFT_LEFT_BITWISE_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(SHIFT_RIGHT_BITWISE_OPERATOR), p.parseInfixExpression)
	p.registerInfix(GoLexTokenSubCategory(LEFT_PARENTHESIS_PUNCTUATION), p.parseCallExpression)
	p.registerInfix(GoLexTokenSubCategory(DOT_PUNCTUATION), p.parseSelectorExpression)
	p.registerInfix(GoLexTokenSubCategory(DOT_PUNCTUATION), p.parseSelectorExpression)

	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) registerPrefix(tokenType parser.LexTokenSubCategory, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType parser.LexTokenSubCategory, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) Parse() (*child.File, error) {
	file := p.newFile()
	p.parsePackageDeclaration()
	allDeclarations := p.collectDeclarations(file)
	typeDecls := p.indexTypeDeclarations(allDeclarations)
	p.attachDeclarationsToFile(file, allDeclarations, typeDecls)

	// After parsing, resolve any pending type references and log unresolved ones
	p.resolvePendingReferences()
	return file, nil
}

func (p *Parser) newFile() *child.File {
	return &child.File{
		Id:             p.filePath,
		Name:           filepath.Base(p.filePath),
		Path:           p.filePath,
		Specifications: []*specification.Specification{},
		Declarations:   []*declaration.Declaration{},
		Contents:       "",
	}
}

func (p *Parser) parsePackageDeclaration() {
	if !p.curTokenIs("package") {
		return
	}

	p.nextToken()
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		p.packageName = p.curToken.Value()
		p.nextToken()
	}
}

func (p *Parser) collectDeclarations(file *child.File) []declaration.Declaration {
	var allDeclarations []declaration.Declaration

	for p.curToken.SubCategory() != GoLexTokenSubCategory(EOF_SPECIAL_CONSTRUCT) {
		if p.curTokenIs("import") {
			specs := p.parseImport()
			for _, s := range specs {
				// Need to create a new variable to take address of
				spec := s
				file.Specifications = append(file.Specifications, &spec)
			}
			continue
		}

		decl := p.parseDeclaration()
		if decl != nil {
			allDeclarations = append(allDeclarations, decl)
		} else {
			p.nextToken()
		}
	}

	return allDeclarations
}

func (p *Parser) indexTypeDeclarations(allDeclarations []declaration.Declaration) map[string]*declaration.TypeDeclaration {
	typeDecls := make(map[string]*declaration.TypeDeclaration)
	for _, decl := range allDeclarations {
		if typeDecl, ok := decl.(*declaration.TypeDeclaration); ok {
			typeDecls[typeDecl.Name] = typeDecl
		}
	}
	return typeDecls
}

func (p *Parser) attachDeclarationsToFile(
	file *child.File,
	allDeclarations []declaration.Declaration,
	typeDecls map[string]*declaration.TypeDeclaration,
) {
	for _, decl := range allDeclarations {
		if funcDecl, ok := decl.(*declaration.FunctionDeclaration); ok {
			// Check if it's a method (3-part ID: package.Receiver.Method)
			parts := strings.Split(funcDecl.Id, ".")
			if len(parts) == 3 {
				// It's a method - link to type, don't add to file.Declarations
				receiverType := strings.TrimPrefix(parts[1], "*")
				if targetType, exists := typeDecls[receiverType]; exists {
					targetType.Functions = append(targetType.Functions, funcDecl)
				}
				// Skip adding to file.Declarations
				continue
			}
		}

		// Not a method - add to file.Declarations
		file.Declarations = append(file.Declarations, &decl)

		// Add to specifications if applicable
		if typeDecl, ok := decl.(*declaration.TypeDeclaration); ok {
			var spec specification.Specification = typeDecl
			file.Specifications = append(file.Specifications, &spec)
		}
		if varDecl, ok := decl.(*declaration.VariableDeclaration); ok {
			var spec specification.Specification = varDecl
			file.Specifications = append(file.Specifications, &spec)
		}
	}
}

func (p *Parser) parseDeclaration() declaration.Declaration {
	switch p.curToken.Value() {
	case "func":
		return p.parseFunctionDeclaration()
	case "type":
		return p.parseTypeDeclaration()
	case "var":
		return p.parseVariableDeclaration()
	default:
		return nil
	}
}

func (p *Parser) parseFunctionDeclaration() declaration.Declaration {
	startOffset := p.curToken.Position().Offset
	p.nextToken() // skip 'func'

	funcDecl := &declaration.FunctionDeclaration{
		BaseDeclaration: &declaration.BaseDeclaration{},
	}

	receiverType := p.parseFunctionReceiver()
	p.parseFunctionName(funcDecl, receiverType)
	p.parseFunctionParameters(funcDecl)
	p.parseFunctionReturnTypes(funcDecl)
	p.parseFunctionBodyAndContents(funcDecl, startOffset)

	return funcDecl
}

// parseFunctionReceiver parses an optional method receiver and returns the
// receiver type name (if any). It assumes the current token is just after
// the 'func' keyword.
func (p *Parser) parseFunctionReceiver() string {
	var receiverType string
	if p.curTokenIs("(") {
		p.nextToken() // skip '('
		// Parse receiver components (simplified), e.g. (s *MyStruct) or (MyStruct)
		for !p.curTokenIs(")") && !p.curTokenIs("EOF") {
			if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
				// Could be name or type
				val := p.curToken.Value()
				p.nextToken()
				if p.curTokenIs("*") { // pointer receiver
					p.nextToken()
					if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
						receiverType = p.curToken.Value()
						p.nextToken()
					}
				} else if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
					// Previous was name, this is type
					receiverType = p.curToken.Value()
					p.nextToken()
				} else {
					// Previous was type (unnamed receiver)
					receiverType = val
				}
			} else if p.curTokenIs("*") {
				p.nextToken()
				if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
					receiverType = p.curToken.Value()
					p.nextToken()
				}
			} else {
				p.nextToken()
			}
		}
		p.nextToken() // skip ')'
	}
	return receiverType
}

// parseFunctionName sets the function name and Id based on the current token
// and the (optional) receiver type. It assumes the current token is at the
// identifier name.
func (p *Parser) parseFunctionName(funcDecl *declaration.FunctionDeclaration, receiverType string) {
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		funcDecl.Name = p.curToken.Value()
		if receiverType != "" {
			funcDecl.Id = fmt.Sprintf("%s.%s.%s", p.packageName, receiverType, funcDecl.Name)
		} else {
			funcDecl.Id = fmt.Sprintf("%s.%s", p.packageName, funcDecl.Name)
		}
		p.nextToken()
	}
}

// parseFunctionParameters parses the function parameter list and appends
// parameters to funcDecl. It supports grouped parameters like `x, y uint` and
// assumes the current token is at the opening '(' or already past it.
func (p *Parser) parseFunctionParameters(funcDecl *declaration.FunctionDeclaration) {
	if !p.curTokenIs("(") {
		return
	}

	p.nextToken() // skip '('
	for !p.curTokenIs(")") && !p.curTokenIs("EOF") {
		if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
			// Collect one or more parameter names: x, y, z
			var names []string
			for {
				if p.curToken.SubCategory() != GoLexTokenSubCategory(IDENT) {
					break
				}
				names = append(names, p.curToken.Value())
				p.nextToken()
				if !p.curTokenIs(",") {
					break
				}
				// Consume comma and prepare for next name or type
				p.nextToken()
				// If the next token starts a type (e.g. "[]", "chan", "map", "..."),
				// stop collecting names.
				if p.curTokenIs("[") || p.curTokenIs("chan") || p.curTokenIs("map") ||
					p.curTokenIs("...") || p.curTokenIs("interface") {
					break
				}
				if p.curToken.SubCategory() != GoLexTokenSubCategory(IDENT) {
					break
				}
			}

			// Current token should now be at the beginning of the type.
			paramType := p.parseParameterType()

			for _, name := range names {
				param := &declaration.FunctionParameter{
					Name:          name,
					ParameterType: p.determineTypeReference(paramType, funcDecl.Id+":param:"+name),
				}
				funcDecl.Parameters = append(funcDecl.Parameters, param)
			}

			if p.curTokenIs(",") {
				p.nextToken()
			}
		} else {
			// Skip unexpected tokens to avoid infinite loops.
			p.nextToken()
		}
	}
	p.nextToken() // skip ')'
}

// parseFunctionBodyAndContents parses the function body and captures the
// source contents for the function using the provided startOffset.
func (p *Parser) parseFunctionBodyAndContents(funcDecl *declaration.FunctionDeclaration, startOffset int) {
	if !p.curTokenIs("{") {
		return
	}

	p.nextToken() // skip '{'
	funcDecl.Body = p.parseBlockStatements()
	// At this point, current token SHOULD be '}' (parseBlockStatements stops at it)
	// Capture the end offset BEFORE consuming the '}'
	if p.curTokenIs("}") {
		endOffset := p.curToken.Position().Offset + len(p.curToken.Value())
		p.nextToken() // consume '}'

		if startOffset < len(p.content) && endOffset <= len(p.content) {
			funcDecl.Contents = p.content[startOffset:endOffset]
		} else {
			funcDecl.Contents = "Error capturing content"
		}
	} else {
		// No closing brace found - error case
		funcDecl.Contents = "Error: no closing brace"
	}
}

func (p *Parser) parseVariableDeclaration() declaration.Declaration {
	startOffset := p.curToken.Position().Offset
	p.nextToken() // skip 'var'
	varDecl := &declaration.VariableDeclaration{
		BaseDeclaration: &declaration.BaseDeclaration{},
		Reference:       &declaration.TypeReference{},
	}
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		varDecl.Name = p.curToken.Value()
		varDecl.Id = fmt.Sprintf("%s.%s", p.packageName, varDecl.Name)
		p.nextToken()
	}
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
		varDecl.Reference = p.determineTypeReference(p.curToken.Value(), varDecl.Id)
		p.nextToken()
	}

	// Handle initialization if present (e.g. = 123)
	if p.curTokenIs("=") {
		p.nextToken()
		p.parseExpression(LOWEST) // Consume expression
	}

	endOffset := p.curToken.Position().Offset
	// If next is EOF or newline (not handled explicitly but lexer skips whitespace),
	// we might need to be careful.
	// For now, assume end of statement is end of expression or type.

	if startOffset < len(p.content) && endOffset <= len(p.content) {
		varDecl.Contents = p.content[startOffset:endOffset]
	} else {
		varDecl.Contents = "Error capturing content"
	}

	return varDecl
}

func (p *Parser) parseBlockStatements() []statement.Statement {
	var stmts []statement.Statement

	for !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		stmt := p.parseStatement()
		if stmt != nil {
			stmts = append(stmts, stmt)
		}

		// After parsing a statement, advance to next token
		// Statement parsers should leave cursor positioned at statement end
		p.nextToken()
	}

	// When loop exits, current token SHOULD be '}' (the closing brace for THIS block)
	// Don't consume it here - let the caller handle it
	return stmts
}

func (p *Parser) parseStatement() statement.Statement {
	switch p.curToken.Value() {
	case "return":
		return p.parseReturnStatement()
	case "if":
		return p.parseIfStatement()
	case "for":
		return p.parseForStatement()
	case "switch":
		return p.parseSwitchStatement()
	case "select":
		return p.parseSelectStatement()
	case "break":
		return p.parseBreakStatement()
	case "continue":
		return p.parseContinueStatement()
	case "defer":
		return p.parseDeferStatement()
	case "goto":
		return p.parseGotoStatement()
	case "fallthrough":
		return p.parseFallthroughStatement()
	case "go":
		return p.parseGoStatement()
	case "var":
		return p.parseVariableDeclarationStatement()
	default:
		return p.parseSimpleStatement()
	}
}

func (p *Parser) parseVariableDeclarationStatement() statement.Statement {
	p.nextToken() // skip 'var'
	stmt := &statement.VariableDeclarationStatement{}

	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		stmt.Name = p.curToken.Value()
		p.nextToken()
	}

	// Type or =
	if p.curToken.Value() != "=" {
		stmt.Type = p.curToken.Value() // Simplified type parsing
		p.nextToken()
	}

	if p.curTokenIs("=") {
		p.nextToken()
		stmt.Expression = p.parseExpression(LOWEST)
	}

	return stmt
}

func (p *Parser) parseSimpleStatement() statement.Statement {
	expr := p.parseExpression(LOWEST)
	if expr == nil {
		return nil
	}

	if stmt := p.parseIncDecStatement(expr); stmt != nil {
		return stmt
	}
	if stmt := p.parseCompoundAssignmentStatement(expr); stmt != nil {
		return stmt
	}
	if stmt := p.parseShortVarDeclaration(expr); stmt != nil {
		return stmt
	}
	if stmt := p.parseSimpleAssignmentStatement(expr); stmt != nil {
		return stmt
	}

	return &statement.ExpressionStatement{Expression: expr}
}

// parseIncDecStatement handles post-increment/decrement expressions like `i++`.
func (p *Parser) parseIncDecStatement(expr expression.Expression) statement.Statement {
	if !p.peekTokenIs("++") && !p.peekTokenIs("--") {
		return nil
	}

	operator := p.peekToken.Value()
	p.nextToken() // consume operator
	return &statement.ExpressionStatement{
		Expression: &expression.InfixExpression{
			Left:     expr,
			Operator: mapBinaryOp(operator),
			Right:    nil, // Post-increment has no right operand
		},
	}
}

// parseCompoundAssignmentStatement handles operators like +=, -=, *=, /=, %=, &=, |=, ^=.
func (p *Parser) parseCompoundAssignmentStatement(expr expression.Expression) statement.Statement {
	if p.peekToken.SubCategory() != GoLexTokenSubCategory(ADD_ASSIGN_ASSIGNMENT_OPERATOR) &&
		p.peekToken.SubCategory() != GoLexTokenSubCategory(SUBTRACT_ASSIGN_ASSIGNMENT_OPERATOR) &&
		p.peekToken.SubCategory() != GoLexTokenSubCategory(MULTIPLY_ASSIGN_ASSIGNMENT_OPERATOR) &&
		p.peekToken.SubCategory() != GoLexTokenSubCategory(DIVIDE_ASSIGN_ASSIGNMENT_OPERATOR) &&
		p.peekToken.SubCategory() != GoLexTokenSubCategory(MODULO_ASSIGNMENT_OPERATOR) &&
		p.peekToken.SubCategory() != GoLexTokenSubCategory(AND_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR) &&
		p.peekToken.SubCategory() != GoLexTokenSubCategory(OR_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR) &&
		p.peekToken.SubCategory() != GoLexTokenSubCategory(XOR_BITWISE_ASSIGN_ASSIGNMENT_OPERATOR) {
		return nil
	}

	// Move to the operator token
	p.nextToken()
	op := p.curToken.Value() // "+=", "-=", "&=", etc.

	// Parse RHS expression
	p.nextToken()
	rhs := p.parseExpression(LOWEST)

	// Represent compound assignments as `a = a <op> b` with an infix RHS.
	var baseOp expression.BinaryOpKind
	switch op {
	case "+=":
		baseOp = expression.BinaryOpAdd
	case "-=":
		baseOp = expression.BinaryOpSub
	case "*=":
		baseOp = expression.BinaryOpMul
	case "/=":
		baseOp = expression.BinaryOpDiv
	case "%=":
		baseOp = expression.BinaryOpMod
	case "&=":
		baseOp = expression.BinaryOpAndBitwise
	case "|=":
		baseOp = expression.BinaryOpOrBitwise
	case "^=":
		baseOp = expression.BinaryOpXorBitwise
	default:
		baseOp = expression.BinaryOpUnknown
	}

	assignKind := statement.AssignOpUnknown
	switch op {
	case "+=":
		assignKind = statement.AssignOpAdd
	case "-=":
		assignKind = statement.AssignOpSub
	case "*=":
		assignKind = statement.AssignOpMul
	case "/=":
		assignKind = statement.AssignOpDiv
	case "%=":
		assignKind = statement.AssignOpMod
	case "&=":
		assignKind = statement.AssignOpAnd
	case "|=":
		assignKind = statement.AssignOpOr
	case "^=":
		assignKind = statement.AssignOpXor
	}

	return &statement.AssignmentStatement{
		Assignee: expr,
		Expression: &expression.InfixExpression{
			Left:     expr,
			Operator: baseOp,
			Right:    rhs,
		},
		Operator: assignKind,
	}
}

// parseShortVarDeclaration handles `x := expr` short variable declarations.
func (p *Parser) parseShortVarDeclaration(expr expression.Expression) statement.Statement {
	if !p.peekTokenIs(":=") {
		return nil
	}

	p.nextToken() // curToken becomes :=
	p.nextToken() // curToken is start of RHS
	rhs := p.parseExpression(LOWEST)

	if ident, ok := expr.(*expression.Identifier); ok {
		return &statement.VariableDeclarationStatement{
			Name:       ident.Value,
			Expression: rhs,
		}
	}
	// If complex LHS, maybe error or handle multi-assign
	return nil
}

// parseSimpleAssignmentStatement handles plain `=` assignments.
func (p *Parser) parseSimpleAssignmentStatement(expr expression.Expression) statement.Statement {
	if !p.peekTokenIs("=") {
		return nil
	}

	p.nextToken() // curToken is =
	p.nextToken() // curToken is start of RHS
	rhs := p.parseExpression(LOWEST)
	return &statement.AssignmentStatement{
		Assignee:   expr,
		Expression: rhs,
		Operator:   statement.AssignOpEq,
	}
}

func (p *Parser) parseReturnStatement() *statement.ReturnStatement {
	stmt := &statement.ReturnStatement{}
	// curToken is 'return'
	if p.peekTokenIs(";") || p.peekTokenIs("}") {
		// Bare return - don't advance, stay at 'return' keyword
		return stmt
	}
	// Return with expression(s)
	p.nextToken() // skip 'return'
	if !p.curTokenIs(";") && !p.curTokenIs("}") {
		// Parse first expression
		stmt.ReturnExpressions = append(stmt.ReturnExpressions, p.parseExpression(LOWEST))

		// Parse additional comma-separated expressions
		for p.peekTokenIs(",") {
			p.nextToken() // move to comma
			p.nextToken() // move to next expression
			expr := p.parseExpression(LOWEST)
			if expr != nil {
				stmt.ReturnExpressions = append(stmt.ReturnExpressions, expr)
			}
		}
	}
	return stmt
}

func (p *Parser) parseIfStatement() *statement.IfStatement {
	stmt := &statement.IfStatement{}
	p.nextToken() // skip 'if'
	stmt.IfExpression = p.parseExpression(LOWEST)
	p.nextToken() // advance to '{' (parseExpression leaves cursor at last token of condition)
	if p.curTokenIs("{") {
		p.nextToken() // skip '{'
		stmt.Body = p.parseBlockStatements()
		// parseBlockStatements stopped at '}'
		// IMPORTANT: Leave cursor at '}' (do NOT call nextToken)
		// This allows outer parseBlockStatements to see the '}' in its loop condition
		if p.curTokenIs("}") {
			// Cursor is at '}' - this is the last token of the if statement
			// The outer parseBlockStatements will call nextToken() to advance
		}
	}
	return stmt
}

// parseForLoopComponent parses a single component of a for loop (init or post)
// Returns a Statement (which could be ExpressionStatement or VariableDeclarationStatement)
func (p *Parser) parseForLoopComponent() statement.Statement {
	// Parse first expression
	expr := p.parseExpression(LOWEST)
	if expr == nil {
		return nil
	}

	// Check if this is a variable declaration (i := 0)
	if p.peekTokenIs(":=") {
		p.nextToken() // move to :=
		p.nextToken() // move to RHS
		rhs := p.parseExpression(LOWEST)

		if ident, ok := expr.(*expression.Identifier); ok {
			return &statement.VariableDeclarationStatement{
				Name:       ident.Value,
				Expression: rhs,
			}
		}
	}

	// Check if this is a post-increment/decrement: i++, i--
	if p.peekTokenIs("++") || p.peekTokenIs("--") {
		p.nextToken() // move to operator
		return &statement.ExpressionStatement{
			Expression: &expression.InfixExpression{
				Left:     expr,
				Operator: mapBinaryOp(p.curToken.Value()),
				Right:    nil,
			},
		}
	}

	// Otherwise just return as expression statement
	return &statement.ExpressionStatement{Expression: expr}
}

func (p *Parser) parseForStatement() *statement.ForStatement {
	stmt := &statement.ForStatement{}
	p.nextToken() // skip 'for'

	if p.parseInfiniteFor(stmt) {
		return stmt
	}
	if p.parseLeadingRangeFor(stmt) {
		return stmt
	}
	if p.parseHeaderRangeFor(stmt) {
		return stmt
	}

	p.parseClauseFor(stmt)
	return stmt
}

// parseInfiniteFor handles `for { ... }` loops.
func (p *Parser) parseInfiniteFor(stmt *statement.ForStatement) bool {
	if !p.curTokenIs("{") {
		return false
	}

	stmt.Header.Kind = statement.ForHeaderKindClause
	stmt.Header.Pre = nil
	stmt.Header.Core = nil
	stmt.Header.Post = nil
	p.nextToken() // skip '{'
	stmt.Body = p.parseBlockStatements()
	return true
}

// parseLeadingRangeFor handles the `for range x { ... }` form.
func (p *Parser) parseLeadingRangeFor(stmt *statement.ForStatement) bool {
	if !p.curTokenIs("range") {
		return false
	}

	// Range keyword at start (e.g., "for range ch")
	stmt.Header.Kind = statement.ForHeaderKindRange
	p.nextToken() // skip 'range'
	srcExpr := p.parseExpression(LOWEST)
	stmt.Header.Core = &statement.RangeHeaderStatement{Src: srcExpr}
	p.nextToken() // advance to '{'
	if p.curTokenIs("{") {
		p.nextToken() // skip '{'
		stmt.Body = p.parseBlockStatements()
	}
	return true
}

// parseHeaderRangeFor handles header-based range loops like
// `for _, x := range arr { ... }`.
func (p *Parser) parseHeaderRangeFor(stmt *statement.ForStatement) bool {
	// For range loops like "for _, x := range arr" or "for i, v := range arr",
	// we need to parse the loop variables and the range source expression.
	if !((p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) || p.curTokenIs("_")) &&
		(p.peekTokenIs(",") || p.peekTokenIs(":="))) {
		return false
	}

	stmt.Header.Kind = statement.ForHeaderKindRange

	// Parse key and optional value identifiers, then := range src
	var keyExpr, valueExpr expression.Expression

	// First identifier or "_"
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) || p.curTokenIs("_") {
		keyExpr = &expression.Identifier{Value: p.curToken.Value()}
	}

	// Optional ", value"
	if p.peekTokenIs(",") {
		p.nextToken() // move to ','
		p.nextToken() // move to value identifier
		if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) || p.curTokenIs("_") {
			valueExpr = &expression.Identifier{Value: p.curToken.Value()}
		}
	}

	// Expect :=
	for !p.curTokenIs(":=") && !p.curTokenIs("EOF") {
		p.nextToken()
	}
	if !p.curTokenIs(":=") {
		// Fallback: not actually a range loop
		stmt.Header.Kind = statement.ForHeaderKindClause
		// Let the rest of the function handle as traditional/condition-only
		return false
	}

	p.nextToken() // skip :=
	if p.curTokenIs("range") {
		p.nextToken() // skip 'range'
		srcExpr := p.parseExpression(LOWEST)
		stmt.Header.Core = &statement.RangeHeaderStatement{
			Key:   keyExpr,
			Value: valueExpr,
			Src:   srcExpr,
		}
		p.nextToken() // advance to '{'
		if p.curTokenIs("{") {
			p.nextToken() // skip '{'
			stmt.Body = p.parseBlockStatements()
		}
		return true
	}

	return false
}

// parseClauseFor handles traditional and condition-only for loops.
func (p *Parser) parseClauseFor(stmt *statement.ForStatement) {
	// Traditional for loop: for init; cond; post { ... }
	// OR condition-only loop: for cond { ... }
	stmt.Header.Kind = statement.ForHeaderKindClause

	// Parse first component as statement (formerly Initializer)
	pre := p.parseForLoopComponent()
	stmt.Header.Pre = pre

	// Check if we have semicolons (traditional for loop)
	if p.peekTokenIs(";") {
		p.nextToken() // move to ;
		p.nextToken() // move past ;

		// Parse condition into Core slot
		if !p.curTokenIs(";") {
			condExpr := p.parseExpression(LOWEST)
			if condExpr != nil {
				stmt.Header.Core = &statement.ExpressionStatement{Expression: condExpr}
			}
		}

		// Expect second semicolon
		if p.peekTokenIs(";") {
			p.nextToken() // move to ;
			p.nextToken() // move past ;

			// Parse post expression into Post slot
			if !p.curTokenIs("{") {
				postExpr := p.parseExpression(LOWEST)
				if postExpr != nil {
					stmt.Header.Post = &statement.ExpressionStatement{Expression: postExpr}
				}
			}
		}
	} else {
		// Condition-only loop: first component was condition
		if pre != nil {
			if exprStmt, ok := pre.(*statement.ExpressionStatement); ok {
				stmt.Header.Core = exprStmt
			}
		}
		stmt.Header.Pre = nil
	}

	p.nextToken() // advance to '{'
	if p.curTokenIs("{") {
		p.nextToken() // skip '{'
		stmt.Body = p.parseBlockStatements()
		// parseBlockStatements stopped at '}'
		// IMPORTANT: Leave cursor at '}' (do NOT call nextToken)
		// This allows outer parseBlockStatements to see the '}' in its loop condition
	}
}

// --- Additional control-flow statement parsers ---

// parseBreakStatement parses a bare `break` statement. Labelled breaks are
// currently parsed as bare breaks; label information can be added later if
// needed.
func (p *Parser) parseBreakStatement() *statement.BreakStatement {
	return &statement.BreakStatement{}
}

// parseContinueStatement parses a `continue` statement.
func (p *Parser) parseContinueStatement() *statement.ContinueStatement {
	return &statement.ContinueStatement{}
}

// parseFallthroughStatement parses a `fallthrough` statement inside a switch
// case. It has no payload, but is modeled as a distinct node in the AST.
func (p *Parser) parseFallthroughStatement() *statement.FallthroughStatement {
	return &statement.FallthroughStatement{}
}

// parseDeferStatement parses a `defer <call>` statement. It treats the
// following expression as the deferred call. We rely on parseBlockStatements
// to consume any trailing semicolon token.
func (p *Parser) parseDeferStatement() *statement.DeferStatement {
	stmt := &statement.DeferStatement{}
	// curToken is 'defer'
	p.nextToken() // move to start of deferred expression
	if !p.curTokenIs(";") && !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		stmt.Call = p.parseExpression(LOWEST)
	}
	return stmt
}

// parseGotoStatement parses a `goto label` statement. The label is stored as a
// simple string on the GotoStatement node.
func (p *Parser) parseGotoStatement() *statement.GotoStatement {
	stmt := &statement.GotoStatement{}
	// curToken is 'goto'
	if p.peekToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		p.nextToken()
		stmt.Label = p.curToken.Value()
	}
	return stmt
}

// parseGoStatement parses a `go <call>` statement, representing a goroutine
// launch. The launched expression is stored in the GoStatement.Call field.
func (p *Parser) parseGoStatement() *statement.GoStatement {
	stmt := &statement.GoStatement{}
	// curToken is 'go'
	p.nextToken() // move to start of call expression
	if !p.curTokenIs(";") && !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		stmt.Call = p.parseExpression(LOWEST)
	}
	return stmt
}

// parseSwitchStatement parses Go "switch" statements in a simplified but
// robust way. It supports expression switches and type switches by capturing
// the header expression best-effort and then parsing cases.
func (p *Parser) parseSwitchStatement() *statement.SwitchStatement {
	stmt := &statement.SwitchStatement{}
	// curToken is 'switch'
	p.nextToken() // move to first token after 'switch'

	// Header: best-effort parse of the switch expression; skip any trailing
	// tokens until the opening '{'. This handles both "switch x {" and more
	// complex headers like "switch v := x.(type) {".
	if p.curTokenIs("{") {
		// Expression-less switch
		// leave stmt.Expression nil
	} else {
		expr := p.parseExpression(LOWEST)
		if expr != nil {
			stmt.Expression = expr
		}
		// Advance until '{' or EOF
		for !p.curTokenIs("{") && !p.curTokenIs("EOF") {
			p.nextToken()
		}
	}

	if !p.curTokenIs("{") {
		return stmt
	}

	// Enter switch body
	p.nextToken() // skip '{'
	for !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		if p.curTokenIs("case") || p.curTokenIs("default") {
			cs := p.parseSwitchCase()
			if cs != nil {
				stmt.Cases = append(stmt.Cases, cs)
			}
			continue
		}
		// Skip unexpected tokens between cases.
		p.nextToken()
	}

	return stmt
}

// parseSwitchCase parses a single `case` or `default` clause inside a switch.
// It assumes curToken is either "case" or "default" when called.
func (p *Parser) parseSwitchCase() *statement.SwitchCaseStatement {
	cs := &statement.SwitchCaseStatement{}

	if p.curTokenIs("case") {
		// Parse comma-separated expressions until ':'
		p.nextToken() // move to first expression token
		for {
			if p.curTokenIs(":") || p.curTokenIs("EOF") {
				break
			}
			expr := p.parseExpression(LOWEST)
			if expr != nil {
				cs.Expressions = append(cs.Expressions, expr)
			}
			// After parseExpression, curToken is the last token of the expression.
			if p.peekTokenIs(",") {
				p.nextToken() // move to ','
				p.nextToken() // move to start of next expression
				continue
			}
			if p.peekTokenIs(":") {
				p.nextToken() // move to ':'
				break
			}
			// Fallback: advance one token to make progress.
			p.nextToken()
		}
	} else if p.curTokenIs("default") {
		// default clause has no expressions; skip tokens until ':'
		for !p.curTokenIs(":") && !p.curTokenIs("EOF") {
			p.nextToken()
		}
	}

	// At this point, curToken should be ':' (or EOF). Move to first body token.
	if p.curTokenIs(":") {
		p.nextToken()
	}

	// Parse case body statements until the next case/default or closing brace.
	for !p.curTokenIs("case") && !p.curTokenIs("default") && !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		stmt := p.parseStatement()
		if stmt != nil {
			cs.Body = append(cs.Body, stmt)
		}
		p.nextToken()
	}

	return cs
}

// parseSelectStatement parses Go `select` statements. Channel case headers are
// currently captured as raw header strings in a single Identifier expression
// for simplicity.
func (p *Parser) parseSelectStatement() *statement.SelectStatement {
	stmt := &statement.SelectStatement{}
	// curToken is 'select'
	p.nextToken() // move to '{' or whitespace

	// Skip to opening '{'
	for !p.curTokenIs("{") && !p.curTokenIs("EOF") {
		p.nextToken()
	}
	if !p.curTokenIs("{") {
		return stmt
	}

	p.nextToken() // skip '{'
	for !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		if p.curTokenIs("case") || p.curTokenIs("default") {
			cs := p.parseSelectCase()
			if cs != nil {
				stmt.Cases = append(stmt.Cases, cs)
			}
			continue
		}
		p.nextToken()
	}
	return stmt
}

// parseSelectCase parses a single `case` or `default` inside a select. For
// `case` clauses, we capture the entire header between `case` and `:` as a
// single Identifier.Value string; this keeps the representation simple while
// still giving the LLM enough surface form.
func (p *Parser) parseSelectCase() *statement.SelectCaseStatement {
	cs := &statement.SelectCaseStatement{}

	if p.curTokenIs("case") {
		// Collect all tokens until ':' into a single header string.
		p.nextToken() // move to first header token
		var parts []string
		for !p.curTokenIs(":") && !p.curTokenIs("EOF") {
			parts = append(parts, p.curToken.Value())
			p.nextToken()
		}
		if len(parts) > 0 {
			head := strings.Join(parts, " ")
			cs.Expressions = []expression.Expression{
				&expression.Identifier{Value: head},
			}
		}
	} else if p.curTokenIs("default") {
		// default select case – no header expressions
		for !p.curTokenIs(":") && !p.curTokenIs("EOF") {
			p.nextToken()
		}
	}

	// Move past ':' to first body token
	if p.curTokenIs(":") {
		p.nextToken()
	}

	for !p.curTokenIs("case") && !p.curTokenIs("default") && !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		stmt := p.parseStatement()
		if stmt != nil {
			cs.Body = append(cs.Body, stmt)
		}
		p.nextToken()
	}

	return cs
}
