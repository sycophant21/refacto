package go_lang

import (
	"strconv"

	"refacto/internal/domain/ast/expression"
)

func (p *Parser) parseExpression(precedence int) expression.Expression {
	prefix := p.prefixParseFns[p.curToken.SubCategory()]
	if prefix == nil {
		// Check for identifier/literal explicitly if map fails
		if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
			return p.parseIdentifier()
		}
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(";") && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.SubCategory()]
		if infix == nil {
			return leftExp
		}
		p.nextToken()
		leftExp = infix(leftExp)
	}
	return leftExp
}

func (p *Parser) parseIdentifier() expression.Expression {
	return &expression.Identifier{Value: p.curToken.Value()}
}

func (p *Parser) parseIntegerLiteral() expression.Expression {
	val, _ := strconv.Atoi(p.curToken.Value())
	return &expression.IntegerLiteral{Value: int64(val)}
}

func (p *Parser) parseStringLiteral() expression.Expression {
	return &expression.StringLiteral{Value: p.curToken.Value()}
}

func (p *Parser) parseBooleanLiteral() expression.Expression {
	return &expression.BooleanLiteral{Value: p.curToken.Value() == "true"}
}

func (p *Parser) parsePrefixExpression() expression.Expression {
	exp := &expression.PrefixExpression{
		Operator: mapUnaryOp(p.curToken.Value()),
	}
	p.nextToken()
	exp.Right = p.parseExpression(PREFIX)
	return exp
}

func (p *Parser) parseInfixExpression(left expression.Expression) expression.Expression {
	opStr := p.curToken.Value()
	precedence := p.curPrecedence()
	p.nextToken()
	right := p.parseExpression(precedence)

	// Check if this is a comparison operator
	if cmp := mapCompareOp(opStr); cmp != expression.CompareOpUnknown {
		return &expression.ComparisonExpression{
			Left:     left,
			Operator: cmp,
			Right:    right,
		}
	}

	// Otherwise return as infix expression with a BinaryOpKind
	return &expression.InfixExpression{
		Left:     left,
		Operator: mapBinaryOp(opStr),
		Right:    right,
	}
}

func mapCompareOp(op string) expression.CompareOpKind {
	switch op {
	case "<":
		return expression.CompareOpLT
	case ">":
		return expression.CompareOpGT
	case "<=":
		return expression.CompareOpLE
	case ">=":
		return expression.CompareOpGE
	case "==":
		return expression.CompareOpEQ
	case "!=":
		return expression.CompareOpNE
	default:
		return expression.CompareOpUnknown
	}
}

func mapBinaryOp(op string) expression.BinaryOpKind {
	switch op {
	case "+":
		return expression.BinaryOpAdd
	case "-":
		return expression.BinaryOpSub
	case "*":
		return expression.BinaryOpMul
	case "/":
		return expression.BinaryOpDiv
	case "%":
		return expression.BinaryOpMod
	case "&&":
		return expression.BinaryOpAndLogical
	case "||":
		return expression.BinaryOpOrLogical
	case "&":
		return expression.BinaryOpAndBitwise
	case "|":
		return expression.BinaryOpOrBitwise
	case "^":
		return expression.BinaryOpXorBitwise
	case "<<":
		return expression.BinaryOpShiftLeft
	case ">>":
		return expression.BinaryOpShiftRight
	case "++":
		// We reuse BinaryOpAdd to reconstruct `x++` as `x + 1` if needed, but
		// printing uses a special case in GetContents.
		return expression.BinaryOpAdd
	case "--":
		return expression.BinaryOpSub
	default:
		return expression.BinaryOpUnknown
	}
}

func mapUnaryOp(op string) expression.UnaryOpKind {
	switch op {
	case "-":
		return expression.UnaryOpNeg
	case "+":
		return expression.UnaryOpPlus
	case "!":
		return expression.UnaryOpNotLogical
	case "^":
		return expression.UnaryOpBitwiseNot
	case "*":
		return expression.UnaryOpDeref
	case "&":
		return expression.UnaryOpAddressOf
	default:
		return expression.UnaryOpUnknown
	}
}

func (p *Parser) parseGroupedExpression() expression.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if p.peekTokenIs(")") {
		p.nextToken()
	}
	return exp
}

func (p *Parser) parseCallExpression(function expression.Expression) expression.Expression {
	exp := &expression.CallExpression{Function: function}
	p.nextToken() // skip '('
	// Parse args
	if !p.curTokenIs(")") {
		exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))
		for p.peekTokenIs(",") {
			p.nextToken()
			p.nextToken()
			exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))
		}
	}
	if p.peekTokenIs(")") {
		p.nextToken()
	}
	return exp
}

func (p *Parser) parseSelectorExpression(left expression.Expression) expression.Expression {
	exp := &expression.SelectorExpression{Operand: left}
	p.nextToken() // skip '.'
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		exp.Selector = &expression.Identifier{Value: p.curToken.Value()}
	}
	return exp
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedenceMap[p.peekToken.SubCategory()]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedenceMap[p.curToken.SubCategory()]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curTokenIs(val string) bool {
	return p.curToken.Value() == val
}

func (p *Parser) peekTokenIs(val string) bool {
	return p.peekToken.Value() == val
}
