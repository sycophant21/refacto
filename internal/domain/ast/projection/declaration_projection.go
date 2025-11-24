package projection

import (
	"fmt"

	"refacto/internal/domain/ast/declaration"
)

// ProjectDeclaration converts an internal declaration node into a generic
// LLM-friendly map representation.
func ProjectDeclaration(d declaration.Declaration) (map[string]any, error) {
	if d == nil {
		return nil, nil
	}

	switch v := d.(type) {
	case *declaration.FunctionDeclaration:
		return projectFunctionDeclaration(v)
	case *declaration.VariableDeclaration:
		return projectVariableDeclaration(v)
	case *declaration.TypeDeclaration:
		return projectTypeDeclaration(v)
	default:
		return nil, fmt.Errorf("ProjectDeclaration not implemented for %T", d)
	}
}

func projectFunctionDeclaration(v *declaration.FunctionDeclaration) (map[string]any, error) {
	m := map[string]any{
		"node": "func",
		"name": v.Name,
	}

	// Receiver (optional)
	if v.Receiver != nil {
		recv := map[string]any{}
		if v.Receiver.Name != "" {
			recv["name"] = v.Receiver.Name
		}
		if v.Receiver.Type != nil {
			if rt := TypeToLLM(v.Receiver.Type); rt != nil {
				recv["type"] = rt
			}
		}
		if len(recv) > 0 {
			m["receiver"] = recv
		}
	}

	// Parameters
	if len(v.Parameters) > 0 {
		params := make([]map[string]any, 0, len(v.Parameters))
		for _, p := range v.Parameters {
			if p == nil {
				continue
			}
			pm := map[string]any{}
			if p.Name != "" {
				pm["name"] = p.Name
			}
			if p.ParameterType != nil {
				if pt := TypeToLLM(p.ParameterType); pt != nil {
					pm["type"] = pt
				}
			}
			params = append(params, pm)
		}
		if len(params) > 0 {
			m["params"] = params
		}
	}

	// Returns (as type strings only)
	if len(v.ReturnTypes) > 0 {
		rets := make([]any, 0, len(v.ReturnTypes))
		for _, r := range v.ReturnTypes {
			if r == nil || r.ParameterType == nil {
				continue
			}
			if rt := TypeToLLM(r.ParameterType); rt != nil {
				rets = append(rets, rt)
			}
		}
		if len(rets) > 0 {
			m["returns"] = rets
		}
	}

	// Body
	if len(v.Body) > 0 {
		body, err := projectStatementSlice(v.Body)
		if err != nil {
			return nil, err
		}
		if body != nil {
			m["body"] = body
		}
	}

	return m, nil
}

func projectVariableDeclaration(v *declaration.VariableDeclaration) (map[string]any, error) {
	m := map[string]any{
		"node": "var",
		"name": v.Name,
	}
	if v.Reference != nil {
		if t := TypeToLLM(v.Reference); t != nil {
			m["type"] = t
		}
	}
	// NOTE: top-level VariableDeclaration currently has no initializer expression
	// in the IR, so we do not project a "value" field here.
	return m, nil
}

func projectTypeDeclaration(v *declaration.TypeDeclaration) (map[string]any, error) {
	m := map[string]any{
		"node": "type",
		"name": v.Name,
	}
	if v.TypeKind != "" {
		m["kind"] = string(v.TypeKind)
	}
	if len(v.State) > 0 {
		fields := make([]map[string]any, 0, len(v.State))
		for _, fld := range v.State {
			if fld == nil {
				continue
			}
			fm := map[string]any{
				"name": fld.Name,
			}
			if fld.Reference != nil {
				if t := TypeToLLM(fld.Reference); t != nil {
					fm["type"] = t
				}
			}
			fields = append(fields, fm)
		}
		if len(fields) > 0 {
			m["fields"] = fields
		}
	}
	if len(v.Functions) > 0 {
		methods := make([]map[string]any, 0, len(v.Functions))
		for _, fn := range v.Functions {
			if fn == nil {
				continue
			}
			fm, err := ProjectDeclaration(fn)
			if err != nil {
				return nil, err
			}
			if fm != nil {
				methods = append(methods, fm)
			}
		}
		if len(methods) > 0 {
			m["methods"] = methods
		}
	}
	return m, nil
}

// ParseDeclaration reconstructs an internal declaration node from a generic
// LLM-friendly map representation.
func ParseDeclaration(m map[string]any) (declaration.Declaration, error) {
	if m == nil {
		return nil, nil
	}

	node, ok := stringFromAny(m["node"])
	if !ok {
		return nil, fmt.Errorf("ParseDeclaration: missing or non-string node field")
	}

	switch node {
	case "func":
		return parseFunctionDeclaration(m)
	case "var":
		return parseVariableDeclaration(m)
	case "type":
		return parseTypeDeclaration(m)
	default:
		return nil, fmt.Errorf("ParseDeclaration: unsupported node %q", node)
	}
}

func parseFunctionDeclaration(m map[string]any) (declaration.Declaration, error) {
	name, _ := stringFromAny(m["name"])
	fn := &declaration.FunctionDeclaration{
		BaseDeclaration: &declaration.BaseDeclaration{Name: name},
	}

	// Receiver
	if rawRecv, ok := m["receiver"].(map[string]any); ok {
		recv := &declaration.MethodReceiver{}
		if n, ok := stringFromAny(rawRecv["name"]); ok {
			recv.Name = n
		}
		if tv, ok := rawRecv["type"]; ok && tv != nil {
			tr, err := LLMToType(tv)
			if err != nil {
				return nil, err
			}
			recv.Type = tr
		}
		fn.Receiver = recv
	}

	// Params
	if raw, ok := m["params"]; ok && raw != nil {
		switch vv := raw.(type) {
		case []any:
			for _, elem := range vv {
				pm, ok := elem.(map[string]any)
				if !ok {
					return nil, fmt.Errorf("ParseDeclaration func: param is not object (type %T)", elem)
				}
				p := &declaration.FunctionParameter{}
				if n, ok := stringFromAny(pm["name"]); ok {
					p.Name = n
				}
				if tv, ok := pm["type"]; ok && tv != nil {
					tr, err := LLMToType(tv)
					if err != nil {
						return nil, err
					}
					p.ParameterType = tr
				}
				fn.Parameters = append(fn.Parameters, p)
			}
		case []map[string]any:
			for _, pm := range vv {
				p := &declaration.FunctionParameter{}
				if n, ok := stringFromAny(pm["name"]); ok {
					p.Name = n
				}
				if tv, ok := pm["type"]; ok && tv != nil {
					tr, err := LLMToType(tv)
					if err != nil {
						return nil, err
					}
					p.ParameterType = tr
				}
				fn.Parameters = append(fn.Parameters, p)
			}
		default:
			return nil, fmt.Errorf("ParseDeclaration func: params has unexpected type %T", vv)
		}
	}

	// Returns
	if raw, ok := m["returns"]; ok && raw != nil {
		switch vv := raw.(type) {
		case []any:
			for _, elem := range vv {
				tr, err := LLMToType(elem)
				if err != nil {
					return nil, err
				}
				fn.ReturnTypes = append(fn.ReturnTypes, &declaration.FunctionParameter{ParameterType: tr})
			}
		case []string:
			for _, s := range vv {
				tr, err := LLMToType(s)
				if err != nil {
					return nil, err
				}
				fn.ReturnTypes = append(fn.ReturnTypes, &declaration.FunctionParameter{ParameterType: tr})
			}
		default:
			return nil, fmt.Errorf("ParseDeclaration func: returns has unexpected type %T", vv)
		}
	}

	// Body
	if rawBody, ok := m["body"]; ok && rawBody != nil {
		body, err := parseStatementSlice(rawBody)
		if err != nil {
			return nil, err
		}
		fn.Body = body
	}

	return fn, nil
}

func parseVariableDeclaration(m map[string]any) (declaration.Declaration, error) {
	name, _ := stringFromAny(m["name"])
	vd := &declaration.VariableDeclaration{
		BaseDeclaration: &declaration.BaseDeclaration{Name: name},
	}
	if tv, ok := m["type"]; ok && tv != nil {
		tr, err := LLMToType(tv)
		if err != nil {
			return nil, err
		}
		vd.Reference = tr
	}
	// NOTE: we intentionally ignore any "value" field here because the IR
	// VariableDeclaration does not carry an initializer expression.
	return vd, nil
}

func parseTypeDeclaration(m map[string]any) (declaration.Declaration, error) {
	name, _ := stringFromAny(m["name"])
	td := &declaration.TypeDeclaration{
		BaseDeclaration: &declaration.BaseDeclaration{Name: name},
	}
	if kindStr, ok := stringFromAny(m["kind"]); ok && kindStr != "" {
		td.TypeKind = declaration.TypeKind(kindStr)
	}

	// Fields
	if raw, ok := m["fields"]; ok && raw != nil {
		switch vv := raw.(type) {
		case []any:
			for _, elem := range vv {
				fm, ok := elem.(map[string]any)
				if !ok {
					return nil, fmt.Errorf("ParseDeclaration type: field is not object (type %T)", elem)
				}
				name, _ := stringFromAny(fm["name"])
				var ref *declaration.TypeReference
				if tv, ok := fm["type"]; ok && tv != nil {
					tr, err := LLMToType(tv)
					if err != nil {
						return nil, err
					}
					ref = tr
				}
				vd := &declaration.VariableDeclaration{
					BaseDeclaration: &declaration.BaseDeclaration{Name: name},
					Reference:       ref,
				}
				td.State = append(td.State, vd)
			}
		case []map[string]any:
			for _, fm := range vv {
				name, _ := stringFromAny(fm["name"])
				var ref *declaration.TypeReference
				if tv, ok := fm["type"]; ok && tv != nil {
					tr, err := LLMToType(tv)
					if err != nil {
						return nil, err
					}
					ref = tr
				}
				vd := &declaration.VariableDeclaration{
					BaseDeclaration: &declaration.BaseDeclaration{Name: name},
					Reference:       ref,
				}
				td.State = append(td.State, vd)
			}
		default:
			return nil, fmt.Errorf("ParseDeclaration type: fields has unexpected type %T", vv)
		}
	}

	// Methods
	if raw, ok := m["methods"]; ok && raw != nil {
		switch vv := raw.(type) {
		case []any:
			for _, elem := range vv {
				mm, ok := elem.(map[string]any)
				if !ok {
					return nil, fmt.Errorf("ParseDeclaration type: method is not object (type %T)", elem)
				}
				dec, err := ParseDeclaration(mm)
				if err != nil {
					return nil, err
				}
				if fn, ok := dec.(*declaration.FunctionDeclaration); ok {
					td.Functions = append(td.Functions, fn)
				}
			}
		case []map[string]any:
			for _, mm := range vv {
				dec, err := ParseDeclaration(mm)
				if err != nil {
					return nil, err
				}
				if fn, ok := dec.(*declaration.FunctionDeclaration); ok {
					td.Functions = append(td.Functions, fn)
				}
			}
		default:
			return nil, fmt.Errorf("ParseDeclaration type: methods has unexpected type %T", vv)
		}
	}

	return td, nil
}
