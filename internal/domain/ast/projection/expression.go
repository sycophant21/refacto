package projection

import (
	"fmt"

	"refacto/internal/domain/ast/expression"
)

// ProjectExpression converts an internal expression node into a generic
// LLM-friendly map representation.
func ProjectExpression(e expression.Expression) (map[string]any, error) {
	if e == nil {
		return nil, nil
	}

	switch v := e.(type) {
	case *expression.IntegerLiteral:
		return map[string]any{
			"node":  "literal",
			"type":  "int",
			"value": v.Value,
		}, nil
	case *expression.StringLiteral:
		return map[string]any{
			"node":  "literal",
			"type":  "string",
			"value": v.Value,
		}, nil
	case *expression.BooleanLiteral:
		return map[string]any{
			"node":  "literal",
			"type":  "bool",
			"value": v.Value,
		}, nil
	case *expression.Identifier:
		m := map[string]any{
			"node": "ident",
			"name": v.Value,
		}
		return m, nil
	case *expression.InfixExpression:
		left, err := ProjectExpression(v.Left)
		if err != nil {
			return nil, err
		}
		right, err := ProjectExpression(v.Right)
		if err != nil {
			return nil, err
		}
		return map[string]any{
			"node":  "binary",
			"op":    BinaryOpKindToLLM(v.Operator),
			"left":  left,
			"right": right,
		}, nil
	case *expression.ComparisonExpression:
		left, err := ProjectExpression(v.Left)
		if err != nil {
			return nil, err
		}
		right, err := ProjectExpression(v.Right)
		if err != nil {
			return nil, err
		}
		return map[string]any{
			"node":  "compare",
			"op":    CompareOpKindToLLM(v.Operator),
			"left":  left,
			"right": right,
		}, nil
	case *expression.PrefixExpression:
		right, err := ProjectExpression(v.Right)
		if err != nil {
			return nil, err
		}
		return map[string]any{
			"node": "unary",
			"op":   UnaryOpKindToLLM(v.Operator),
			"expr": right,
		}, nil
	case *expression.CallExpression:
		fn, err := ProjectExpression(v.Function)
		if err != nil {
			return nil, err
		}
		args := make([]map[string]any, 0, len(v.Arguments))
		for _, a := range v.Arguments {
			pa, err := ProjectExpression(a)
			if err != nil {
				return nil, err
			}
			args = append(args, pa)
		}
		return map[string]any{
			"node":   "call",
			"callee": fn,
			"args":   args,
		}, nil
	case *expression.SelectorExpression:
		operand, err := ProjectExpression(v.Operand)
		if err != nil {
			return nil, err
		}
		// Selector is always an Identifier; reuse its Value.
		name := ""
		if v.Selector != nil {
			name = v.Selector.Value
		}
		return map[string]any{
			"node":   "selector",
			"object": operand,
			"field":  name,
		}, nil
	default:
		return nil, fmt.Errorf("ProjectExpression not implemented for %T", e)
	}
}

// ParseExpression reconstructs an internal expression node from a generic
// LLM-friendly map representation.
func ParseExpression(m map[string]any) (expression.Expression, error) {
	if m == nil {
		return nil, nil
	}

	node, ok := stringFromAny(m["node"])
	if !ok {
		return nil, fmt.Errorf("ParseExpression: missing or non-string node field")
	}

	switch node {
	case "literal":
		typ, _ := stringFromAny(m["type"])
		switch typ {
		case "int":
			// Accept several numeric representations (int64 from IR, float64 from decoded JSON).
			var n int64
			switch v := m["value"].(type) {
			case int:
				n = int64(v)
			case int64:
				n = v
			case float64:
				n = int64(v)
			default:
				return nil, fmt.Errorf("ParseExpression literal int: value is not numeric (type %T)", v)
			}
			return &expression.IntegerLiteral{Value: n}, nil
		case "string":
			v, ok := m["value"].(string)
			if !ok {
				return nil, fmt.Errorf("ParseExpression literal string: value is not string")
			}
			return &expression.StringLiteral{Value: v}, nil
		case "bool":
			v, ok := m["value"].(bool)
			if !ok {
				return nil, fmt.Errorf("ParseExpression literal bool: value is not bool")
			}
			return &expression.BooleanLiteral{Value: v}, nil
		default:
			return nil, fmt.Errorf("ParseExpression literal: unsupported type %q", typ)
		}
	case "ident":
		name, ok := stringFromAny(m["name"])
		if !ok {
			return nil, fmt.Errorf("ParseExpression ident: missing name")
		}
		return &expression.Identifier{Value: name}, nil
	case "binary":
		opStr, ok := stringFromAny(m["op"])
		if !ok {
			return nil, fmt.Errorf("ParseExpression binary: missing op")
		}
		op, err := LLMToBinaryOpKind(opStr)
		if err != nil {
			return nil, err
		}
		leftMap, ok := m["left"].(map[string]any)
		if !ok {
			return nil, fmt.Errorf("ParseExpression binary: left is not object")
		}
		left, err := ParseExpression(leftMap)
		if err != nil {
			return nil, err
		}
		rightMap, ok := m["right"].(map[string]any)
		if !ok {
			return nil, fmt.Errorf("ParseExpression binary: right is not object")
		}
		right, err := ParseExpression(rightMap)
		if err != nil {
			return nil, err
		}
		return &expression.InfixExpression{Left: left, Operator: op, Right: right}, nil
	case "compare":
		opStr, ok := stringFromAny(m["op"])
		if !ok {
			return nil, fmt.Errorf("ParseExpression compare: missing op")
		}
		op, err := LLMToCompareOpKind(opStr)
		if err != nil {
			return nil, err
		}
		leftMap, ok := m["left"].(map[string]any)
		if !ok {
			return nil, fmt.Errorf("ParseExpression compare: left is not object")
		}
		left, err := ParseExpression(leftMap)
		if err != nil {
			return nil, err
		}
		rightMap, ok := m["right"].(map[string]any)
		if !ok {
			return nil, fmt.Errorf("ParseExpression compare: right is not object")
		}
		right, err := ParseExpression(rightMap)
		if err != nil {
			return nil, err
		}
		return &expression.ComparisonExpression{Left: left, Operator: op, Right: right}, nil
	case "unary":
		opStr, ok := stringFromAny(m["op"])
		if !ok {
			return nil, fmt.Errorf("ParseExpression unary: missing op")
		}
		op, err := LLMToUnaryOpKind(opStr)
		if err != nil {
			return nil, err
		}
		exprMap, ok := m["expr"].(map[string]any)
		if !ok {
			return nil, fmt.Errorf("ParseExpression unary: expr is not object")
		}
		expr, err := ParseExpression(exprMap)
		if err != nil {
			return nil, err
		}
		return &expression.PrefixExpression{Operator: op, Right: expr}, nil
	case "call":
		calleeMap, ok := m["callee"].(map[string]any)
		if !ok {
			return nil, fmt.Errorf("ParseExpression call: callee is not object")
		}
		callee, err := ParseExpression(calleeMap)
		if err != nil {
			return nil, err
		}
		var args []expression.Expression
		if raw, ok := m["args"]; ok && raw != nil {
			switch vv := raw.(type) {
			case []any:
				for _, ra := range vv {
					am, ok := ra.(map[string]any)
					if !ok {
						return nil, fmt.Errorf("ParseExpression call: arg is not object (type %T)", ra)
					}
					ae, err := ParseExpression(am)
					if err != nil {
						return nil, err
					}
					args = append(args, ae)
				}
			case []map[string]any:
				for _, am := range vv {
					ae, err := ParseExpression(am)
					if err != nil {
						return nil, err
					}
					args = append(args, ae)
				}
			default:
				return nil, fmt.Errorf("ParseExpression call: args has unexpected type %T", vv)
			}
		}
		return &expression.CallExpression{Function: callee, Arguments: args}, nil
	case "selector":
		objMap, ok := m["object"].(map[string]any)
		if !ok {
			return nil, fmt.Errorf("ParseExpression selector: object is not object")
		}
		obj, err := ParseExpression(objMap)
		if err != nil {
			return nil, err
		}
		field, _ := stringFromAny(m["field"])
		// Recreate Identifier for selector name.
		id := &expression.Identifier{Value: field}
		return &expression.SelectorExpression{Operand: obj, Selector: id}, nil
	default:
		return nil, fmt.Errorf("ParseExpression: unsupported node %q", node)
	}
}
