package projection

import (
	"fmt"

	"refacto/internal/domain/ast/expression"
	"refacto/internal/domain/ast/statement"
)

// projectStatementSlice converts a slice of Statement nodes into a slice of
// LLM-friendly maps. Nil statements are skipped.
func projectStatementSlice(stmts []statement.Statement) ([]map[string]any, error) {
	if len(stmts) == 0 {
		return nil, nil
	}
	out := make([]map[string]any, 0, len(stmts))
	for _, s := range stmts {
		if s == nil {
			continue
		}
		ps, err := ProjectStatement(s)
		if err != nil {
			return nil, err
		}
		if ps != nil {
			out = append(out, ps)
		}
	}
	if len(out) == 0 {
		return nil, nil
	}
	return out, nil
}

// parseStatementSlice converts a raw array (typically []any from json.Unmarshal)
// into a slice of Statement nodes.
func parseStatementSlice(raw any) ([]statement.Statement, error) {
	if raw == nil {
		return nil, nil
	}

	switch vv := raw.(type) {
	case []any:
		out := make([]statement.Statement, 0, len(vv))
		for _, elem := range vv {
			m, ok := elem.(map[string]any)
			if !ok {
				return nil, fmt.Errorf("parseStatementSlice: element is not object (type %T)", elem)
			}
			st, err := ParseStatement(m)
			if err != nil {
				return nil, err
			}
			if st != nil {
				out = append(out, st)
			}
		}
		return out, nil
	case []map[string]any:
		out := make([]statement.Statement, 0, len(vv))
		for _, m := range vv {
			st, err := ParseStatement(m)
			if err != nil {
				return nil, err
			}
			if st != nil {
				out = append(out, st)
			}
		}
		return out, nil
	default:
		return nil, fmt.Errorf("parseStatementSlice: unexpected type %T", vv)
	}
}

// projectOptionalStatement projects an optional Statement; nil stays nil.
func projectOptionalStatement(st statement.Statement) (map[string]any, error) {
	if st == nil {
		return nil, nil
	}
	return ProjectStatement(st)
}

// parseOptionalStatement parses an optional Statement from a raw map.
func parseOptionalStatement(raw any) (statement.Statement, error) {
	if raw == nil {
		return nil, nil
	}
	m, ok := raw.(map[string]any)
	if !ok {
		return nil, fmt.Errorf("parseOptionalStatement: not an object (type %T)", raw)
	}
	return ParseStatement(m)
}

// ProjectStatement converts an internal statement node into a generic
// LLM-friendly map representation.
func ProjectStatement(s statement.Statement) (map[string]any, error) {
	if s == nil {
		return nil, nil
	}

	switch v := s.(type) {
	case *statement.ExpressionStatement:
		return projectExpressionStatement(v)
	case *statement.ReturnStatement:
		return projectReturnStatement(v)
	case *statement.IfStatement:
		return projectIfStatement(v)
	case *statement.ElseIfStatement:
		return projectElseIfStatement(v)
	case *statement.ElseStatement:
		return projectElseStatement(v)
	case *statement.ForStatement:
		return projectForStatement(v)
	case *statement.SwitchStatement:
		return projectSwitchStatement(v)
	case *statement.SwitchCaseStatement:
		return projectSwitchCaseStatement(v)
	case *statement.SelectStatement:
		return projectSelectStatement(v)
	case *statement.SelectCaseStatement:
		return projectSelectCaseStatement(v)
	case *statement.BreakStatement:
		return projectBreakStatement(v)
	case *statement.ContinueStatement:
		return projectContinueStatement(v)
	case *statement.DeferStatement:
		return projectDeferStatement(v)
	default:
		return nil, fmt.Errorf("ProjectStatement not implemented for %T", s)
	}
}

func projectExpressionStatement(v *statement.ExpressionStatement) (map[string]any, error) {
	// { "node": "expr-stmt", "expr": <expression> }
	expr, err := ProjectExpression(v.Expression)
	if err != nil {
		return nil, err
	}
	m := map[string]any{
		"node": "expr-stmt",
	}
	if expr != nil {
		m["expr"] = expr
	}
	return m, nil
}

func projectReturnStatement(v *statement.ReturnStatement) (map[string]any, error) {
	// { "node": "return", "values": [ <expression> ] }
	m := map[string]any{"node": "return"}
	if len(v.ReturnExpressions) > 0 {
		vals := make([]map[string]any, 0, len(v.ReturnExpressions))
		for _, e := range v.ReturnExpressions {
			pe, err := ProjectExpression(e)
			if err != nil {
				return nil, err
			}
			if pe != nil {
				vals = append(vals, pe)
			}
		}
		if len(vals) > 0 {
			m["values"] = vals
		}
	}
	return m, nil
}

func projectIfStatement(v *statement.IfStatement) (map[string]any, error) {
	// { "node": "if", "cond": <expr>, "then": [ <stmts> ] }
	cond, err := ProjectExpression(v.IfExpression)
	if err != nil {
		return nil, err
	}
	thenStmts, err := projectStatementSlice(v.Body)
	if err != nil {
		return nil, err
	}
	m := map[string]any{
		"node": "if",
	}
	if cond != nil {
		m["cond"] = cond
	}
	if thenStmts != nil {
		m["then"] = thenStmts
	}
	return m, nil
}

func projectElseIfStatement(v *statement.ElseIfStatement) (map[string]any, error) {
	// { "node": "else-if", "cond": <expr>, "then": [ <stmts> ] }
	cond, err := ProjectExpression(v.ElseIfExpression)
	if err != nil {
		return nil, err
	}
	thenStmts, err := projectStatementSlice(v.Body)
	if err != nil {
		return nil, err
	}
	m := map[string]any{
		"node": "else-if",
	}
	if cond != nil {
		m["cond"] = cond
	}
	if thenStmts != nil {
		m["then"] = thenStmts
	}
	return m, nil
}

func projectElseStatement(v *statement.ElseStatement) (map[string]any, error) {
	// { "node": "else", "then": [ <stmts> ] }
	thenStmts, err := projectStatementSlice(v.Body)
	if err != nil {
		return nil, err
	}
	m := map[string]any{
		"node": "else",
	}
	if thenStmts != nil {
		m["then"] = thenStmts
	}
	return m, nil
}

func projectForStatement(v *statement.ForStatement) (map[string]any, error) {
	// Clause/while-style vs range-based for.
	if v.Header.Kind == statement.ForHeaderKindRange {
		return projectForRangeStatement(v)
	}
	return projectForClauseStatement(v)
}

func projectForRangeStatement(v *statement.ForStatement) (map[string]any, error) {
	// Range form: { "node": "for-range", "key", "value", "src", "body" }
	core, ok := v.Header.Core.(*statement.RangeHeaderStatement)
	if !ok {
		return nil, fmt.Errorf("ProjectStatement for-range: header core is %T, want *RangeHeaderStatement", v.Header.Core)
	}
	m := map[string]any{"node": "for-range"}

	if core.Key != nil {
		k, err := ProjectExpression(core.Key)
		if err != nil {
			return nil, err
		}
		if k != nil {
			m["key"] = k
		}
	}
	if core.Value != nil {
		val, err := ProjectExpression(core.Value)
		if err != nil {
			return nil, err
		}
		if val != nil {
			m["value"] = val
		}
	}
	if core.Src != nil {
		src, err := ProjectExpression(core.Src)
		if err != nil {
			return nil, err
		}
		if src != nil {
			m["src"] = src
		}
	}
	body, err := projectStatementSlice(v.Body)
	if err != nil {
		return nil, err
	}
	if body != nil {
		m["body"] = body
	}
	return m, nil
}

func projectForClauseStatement(v *statement.ForStatement) (map[string]any, error) {
	// Clause/while-style: { "node": "for", "init", "cond", "post", "body" }
	m := map[string]any{"node": "for"}

	if v.Header.Pre != nil {
		init, err := projectOptionalStatement(v.Header.Pre)
		if err != nil {
			return nil, err
		}
		if init != nil {
			m["init"] = init
		}
	}

	if v.Header.Core != nil {
		// Convention: Core is an ExpressionStatement whose Expression is the condition.
		if es, ok := v.Header.Core.(*statement.ExpressionStatement); ok && es.Expression != nil {
			cond, err := ProjectExpression(es.Expression)
			if err != nil {
				return nil, err
			}
			if cond != nil {
				m["cond"] = cond
			}
		}
	}

	if v.Header.Post != nil {
		post, err := projectOptionalStatement(v.Header.Post)
		if err != nil {
			return nil, err
		}
		if post != nil {
			m["post"] = post
		}
	}

	body, err := projectStatementSlice(v.Body)
	if err != nil {
		return nil, err
	}
	if body != nil {
		m["body"] = body
	}
	return m, nil
}

func projectSwitchStatement(v *statement.SwitchStatement) (map[string]any, error) {
	// { "node": "switch", "expr": <expr>, "cases": [ <case> ] }
	expr, err := ProjectExpression(v.Expression)
	if err != nil {
		return nil, err
	}
	cases := make([]map[string]any, 0, len(v.Cases))
	for _, c := range v.Cases {
		if c == nil {
			continue
		}
		pc, err := ProjectStatement(c)
		if err != nil {
			return nil, err
		}
		if pc != nil {
			cases = append(cases, pc)
		}
	}
	m := map[string]any{
		"node": "switch",
	}
	if expr != nil {
		m["expr"] = expr
	}
	if len(cases) > 0 {
		m["cases"] = cases
	}
	return m, nil
}

func projectSwitchCaseStatement(v *statement.SwitchCaseStatement) (map[string]any, error) {
	// { "node": "case", "values": [<expr>...], "body": [<stmts>] }
	vals := make([]map[string]any, 0, len(v.Expressions))
	for _, e := range v.Expressions {
		pe, err := ProjectExpression(e)
		if err != nil {
			return nil, err
		}
		if pe != nil {
			vals = append(vals, pe)
		}
	}
	body, err := projectStatementSlice(v.Body)
	if err != nil {
		return nil, err
	}
	m := map[string]any{
		"node": "case",
	}
	if len(vals) > 0 {
		m["values"] = vals
	}
	if body != nil {
		m["body"] = body
	}
	return m, nil
}

func projectSelectStatement(v *statement.SelectStatement) (map[string]any, error) {
	// { "node": "select", "cases": [ <select-case> ] }
	cases := make([]map[string]any, 0, len(v.Cases))
	for _, c := range v.Cases {
		if c == nil {
			continue
		}
		pc, err := ProjectStatement(c)
		if err != nil {
			return nil, err
		}
		if pc != nil {
			cases = append(cases, pc)
		}
	}
	m := map[string]any{"node": "select"}
	if len(cases) > 0 {
		m["cases"] = cases
	}
	return m, nil
}

func projectSelectCaseStatement(v *statement.SelectCaseStatement) (map[string]any, error) {
	// { "node": "select-case", "values": [<expr>...], "body": [<stmts>] }
	vals := make([]map[string]any, 0, len(v.Expressions))
	for _, e := range v.Expressions {
		pe, err := ProjectExpression(e)
		if err != nil {
			return nil, err
		}
		if pe != nil {
			vals = append(vals, pe)
		}
	}
	body, err := projectStatementSlice(v.Body)
	if err != nil {
		return nil, err
	}
	m := map[string]any{"node": "select-case"}
	if len(vals) > 0 {
		m["values"] = vals
	}
	if body != nil {
		m["body"] = body
	}
	return m, nil
}

func projectBreakStatement(_ *statement.BreakStatement) (map[string]any, error) {
	// { "node": "break" }
	return map[string]any{"node": "break"}, nil
}

func projectContinueStatement(_ *statement.ContinueStatement) (map[string]any, error) {
	// { "node": "continue" }
	return map[string]any{"node": "continue"}, nil
}

func projectDeferStatement(v *statement.DeferStatement) (map[string]any, error) {
	// { "node": "defer", "call": <expr> }
	call, err := ProjectExpression(v.Call)
	if err != nil {
		return nil, err
	}
	m := map[string]any{"node": "defer"}
	if call != nil {
		m["call"] = call
	}
	return m, nil
}

// ParseStatement reconstructs an internal statement node from a generic
// LLM-friendly map representation.
func ParseStatement(m map[string]any) (statement.Statement, error) {
	if m == nil {
		return nil, nil
	}

	node, ok := stringFromAny(m["node"])
	if !ok {
		return nil, fmt.Errorf("ParseStatement: missing or non-string node field")
	}

	switch node {
	case "expr-stmt":
		return parseExpressionStatement(m)
	case "return":
		return parseReturnStatement(m)
	case "if":
		return parseIfStatement(m)
	case "else-if":
		return parseElseIfStatement(m)
	case "else":
		return parseElseStatement(m)
	case "for":
		return parseForStatement(m)
	case "for-range":
		return parseForRangeStatement(m)
	case "switch":
		return parseSwitchStatement(m)
	case "case":
		return parseSwitchCaseStatement(m)
	case "select":
		return parseSelectStatement(m)
	case "select-case":
		return parseSelectCaseStatement(m)
	case "break":
		return &statement.BreakStatement{}, nil
	case "continue":
		return &statement.ContinueStatement{}, nil
	case "defer":
		return parseDeferStatement(m)
	default:
		return nil, fmt.Errorf("ParseStatement: unsupported node %q", node)
	}
}

func parseExpressionStatement(m map[string]any) (statement.Statement, error) {
	rawExpr, ok := m["expr"].(map[string]any)
	if !ok && m["expr"] != nil {
		return nil, fmt.Errorf("ParseStatement expr-stmt: expr is not object (type %T)", m["expr"])
	}
	var expr expression.Expression
	var err error
	if rawExpr != nil {
		expr, err = ParseExpression(rawExpr)
		if err != nil {
			return nil, err
		}
	}
	return &statement.ExpressionStatement{Expression: expr}, nil
}

func parseReturnStatement(m map[string]any) (statement.Statement, error) {
	var values []expression.Expression
	if raw, ok := m["values"]; ok && raw != nil {
		switch vv := raw.(type) {
		case []any:
			for _, elem := range vv {
				am, ok := elem.(map[string]any)
				if !ok {
					return nil, fmt.Errorf("ParseStatement return: value is not object (type %T)", elem)
				}
				expr, err := ParseExpression(am)
				if err != nil {
					return nil, err
				}
				values = append(values, expr)
			}
		case []map[string]any:
			for _, am := range vv {
				expr, err := ParseExpression(am)
				if err != nil {
					return nil, err
				}
				values = append(values, expr)
			}
		default:
			return nil, fmt.Errorf("ParseStatement return: values has unexpected type %T", vv)
		}
	}
	return &statement.ReturnStatement{ReturnExpressions: values}, nil
}

func parseIfStatement(m map[string]any) (statement.Statement, error) {
	rawCond, ok := m["cond"].(map[string]any)
	if !ok || rawCond == nil {
		return nil, fmt.Errorf("ParseStatement if: missing cond")
	}
	cond, err := ParseExpression(rawCond)
	if err != nil {
		return nil, err
	}
	body, err := parseStatementSlice(m["then"])
	if err != nil {
		return nil, err
	}
	return &statement.IfStatement{IfExpression: cond, Body: body}, nil
}

func parseElseIfStatement(m map[string]any) (statement.Statement, error) {
	rawCond, ok := m["cond"].(map[string]any)
	if !ok || rawCond == nil {
		return nil, fmt.Errorf("ParseStatement else-if: missing cond")
	}
	cond, err := ParseExpression(rawCond)
	if err != nil {
		return nil, err
	}
	body, err := parseStatementSlice(m["then"])
	if err != nil {
		return nil, err
	}
	return &statement.ElseIfStatement{ElseIfExpression: cond, Body: body}, nil
}

func parseElseStatement(m map[string]any) (statement.Statement, error) {
	body, err := parseStatementSlice(m["then"])
	if err != nil {
		return nil, err
	}
	return &statement.ElseStatement{ElseExpression: nil, Body: body}, nil
}

func parseForStatement(m map[string]any) (statement.Statement, error) {
	// Clause/while-style for
	var header statement.ForHeader
	header.Kind = statement.ForHeaderKindClause

	if rawInit, ok := m["init"]; ok && rawInit != nil {
		st, err := parseOptionalStatement(rawInit)
		if err != nil {
			return nil, err
		}
		header.Pre = st
	}

	if rawCond, ok := m["cond"].(map[string]any); ok && rawCond != nil {
		condExpr, err := ParseExpression(rawCond)
		if err != nil {
			return nil, err
		}
		if condExpr != nil {
			header.Core = &statement.ExpressionStatement{Expression: condExpr}
		}
	}

	if rawPost, ok := m["post"]; ok && rawPost != nil {
		st, err := parseOptionalStatement(rawPost)
		if err != nil {
			return nil, err
		}
		header.Post = st
	}

	body, err := parseStatementSlice(m["body"])
	if err != nil {
		return nil, err
	}
	return &statement.ForStatement{Header: header, Body: body}, nil
}

func parseForRangeStatement(m map[string]any) (statement.Statement, error) {
	// Range-based for
	var core statement.RangeHeaderStatement

	if rawKey, ok := m["key"].(map[string]any); ok && rawKey != nil {
		k, err := ParseExpression(rawKey)
		if err != nil {
			return nil, err
		}
		core.Key = k
	}
	if rawVal, ok := m["value"].(map[string]any); ok && rawVal != nil {
		v, err := ParseExpression(rawVal)
		if err != nil {
			return nil, err
		}
		core.Value = v
	}
	rawSrc, ok := m["src"].(map[string]any)
	if !ok || rawSrc == nil {
		return nil, fmt.Errorf("ParseStatement for-range: missing src")
	}
	src, err := ParseExpression(rawSrc)
	if err != nil {
		return nil, err
	}
	core.Src = src

	header := statement.ForHeader{
		Kind: statement.ForHeaderKindRange,
		Core: &core,
	}
	body, err := parseStatementSlice(m["body"])
	if err != nil {
		return nil, err
	}
	return &statement.ForStatement{Header: header, Body: body}, nil
}

func parseSwitchStatement(m map[string]any) (statement.Statement, error) {
	// Switch with cases
	rawExpr, ok := m["expr"].(map[string]any)
	if !ok || rawExpr == nil {
		return nil, fmt.Errorf("ParseStatement switch: missing expr")
	}
	expr, err := ParseExpression(rawExpr)
	if err != nil {
		return nil, err
	}
	caseStmts, err := parseStatementSlice(m["cases"])
	if err != nil {
		return nil, err
	}
	cases := make([]*statement.SwitchCaseStatement, 0, len(caseStmts))
	for _, st := range caseStmts {
		c, ok := st.(*statement.SwitchCaseStatement)
		if !ok {
			return nil, fmt.Errorf("ParseStatement switch: case has unexpected type %T", st)
		}
		cases = append(cases, c)
	}
	return &statement.SwitchStatement{Expression: expr, Cases: cases}, nil
}

func parseSwitchCaseStatement(m map[string]any) (statement.Statement, error) {
	var exprs []expression.Expression
	if raw, ok := m["values"]; ok && raw != nil {
		switch vv := raw.(type) {
		case []any:
			for _, elem := range vv {
				am, ok := elem.(map[string]any)
				if !ok {
					return nil, fmt.Errorf("ParseStatement case: value is not object (type %T)", elem)
				}
				ex, err := ParseExpression(am)
				if err != nil {
					return nil, err
				}
				exprs = append(exprs, ex)
			}
		case []map[string]any:
			for _, am := range vv {
				ex, err := ParseExpression(am)
				if err != nil {
					return nil, err
				}
				exprs = append(exprs, ex)
			}
		default:
			return nil, fmt.Errorf("ParseStatement case: values has unexpected type %T", vv)
		}
	}
	body, err := parseStatementSlice(m["body"])
	if err != nil {
		return nil, err
	}
	return &statement.SwitchCaseStatement{Expressions: exprs, Body: body}, nil
}

func parseSelectStatement(m map[string]any) (statement.Statement, error) {
	caseStmts, err := parseStatementSlice(m["cases"])
	if err != nil {
		return nil, err
	}
	cases := make([]*statement.SelectCaseStatement, 0, len(caseStmts))
	for _, st := range caseStmts {
		c, ok := st.(*statement.SelectCaseStatement)
		if !ok {
			return nil, fmt.Errorf("ParseStatement select: case has unexpected type %T", st)
		}
		cases = append(cases, c)
	}
	return &statement.SelectStatement{Cases: cases}, nil
}

func parseSelectCaseStatement(m map[string]any) (statement.Statement, error) {
	var exprs []expression.Expression
	if raw, ok := m["values"]; ok && raw != nil {
		switch vv := raw.(type) {
		case []any:
			for _, elem := range vv {
				am, ok := elem.(map[string]any)
				if !ok {
					return nil, fmt.Errorf("ParseStatement select-case: value is not object (type %T)", elem)
				}
				ex, err := ParseExpression(am)
				if err != nil {
					return nil, err
				}
				exprs = append(exprs, ex)
			}
		case []map[string]any:
			for _, am := range vv {
				ex, err := ParseExpression(am)
				if err != nil {
					return nil, err
				}
				exprs = append(exprs, ex)
			}
		default:
			return nil, fmt.Errorf("ParseStatement select-case: values has unexpected type %T", vv)
		}
	}
	body, err := parseStatementSlice(m["body"])
	if err != nil {
		return nil, err
	}
	return &statement.SelectCaseStatement{Expressions: exprs, Body: body}, nil
}

func parseDeferStatement(m map[string]any) (statement.Statement, error) {
	rawCall, ok := m["call"].(map[string]any)
	if !ok || rawCall == nil {
		return &statement.DeferStatement{}, nil
	}
	callExpr, err := ParseExpression(rawCall)
	if err != nil {
		return nil, err
	}
	return &statement.DeferStatement{Call: callExpr}, nil
}
