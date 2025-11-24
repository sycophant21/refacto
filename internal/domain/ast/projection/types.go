package projection

import (
	"fmt"
	"strconv"
	"strings"

	"refacto/internal/domain/ast/declaration"
)

// TypeToLLM converts a TypeReference into an LLM-facing representation.
// For now we always emit a single canonical string (e.g. "int", "[]int",
// "map[string]int", "*MyType").
func TypeToLLM(tr *declaration.TypeReference) any {
	if tr == nil {
		return nil
	}
	return typeToLLMString(tr)
}

// typeToLLMString builds a human-readable, stable string for the given type.
func typeToLLMString(tr *declaration.TypeReference) string {
	if tr == nil {
		return ""
	}

	// Container types take precedence and wrap the element/key types.
	switch tr.Container {
	case declaration.ContainerNone:
		// No container – fall through to primitive/SystemType handling below.
		break
	case declaration.ContainerSlice:
		et := typeToLLMString(tr.ElementType)
		if et == "" {
			et = "any"
		}
		return "[]" + et
	case declaration.ContainerArray:
		// If an explicit length is available in AdditionalProperties["ArrayLen"],
		// prefer [N]T; otherwise fall back to []T.
		et := typeToLLMString(tr.ElementType)
		if et == "" {
			et = "any"
		}
		if tr.AdditionalProperties != nil {
			if v, ok := tr.AdditionalProperties["ArrayLen"]; ok {
				if n, ok := v.(int); ok {
					return fmt.Sprintf("[%d]%s", n, et)
				}
			}
		}
		return "[]" + et
	case declaration.ContainerMap:
		kt := typeToLLMString(tr.KeyType)
		if kt == "" {
			kt = "any"
		}
		et := typeToLLMString(tr.ElementType)
		if et == "" {
			et = "any"
		}
		return "map[" + kt + "]" + et
	case declaration.ContainerPointer:
		et := typeToLLMString(tr.ElementType)
		if et == "" {
			et = "any"
		}
		return "*" + et
	case declaration.ContainerChan:
		et := typeToLLMString(tr.ElementType)
		if et == "" {
			et = "any"
		}
		return "chan " + et
	}

	// Non-container primitives and user-defined types.
	if tr.Kind == declaration.Primitive {
		switch tr.PrimitiveKind {
		case declaration.Integer:
			return "int"
		case declaration.Long:
			return "long"
		case declaration.Float:
			return "float"
		case declaration.Double:
			return "double"
		case declaration.String:
			return "string"
		case declaration.Bool:
			return "bool"
		case declaration.Char:
			return "char"
		default:
			// Unknown primitive kinds fall through to SystemType/any handling below.
		}
	}

	// If we have a SystemType string, use it directly.
	if tr.SystemType != "" {
		return tr.SystemType
	}

	// Fall back to the referenced declaration's name if available.
	if tr.Type != nil {
		if td, ok := tr.Type.(*declaration.TypeDeclaration); ok {
			if td.Name != "" {
				return td.Name
			}
		}
	}

	// Last-resort fallback.
	return "any"
}

// LLMToType parses an LLM-facing type representation (usually a string) back
// into a full TypeReference.
func LLMToType(v any) (*declaration.TypeReference, error) {
	if v == nil {
		return nil, nil
	}

	s, ok := v.(string)
	if !ok {
		return nil, fmt.Errorf("LLMToType: expected string, got %T", v)
	}
	s = strings.TrimSpace(s)
	if s == "" {
		return nil, nil
	}

	tr, err := parseTypeString(s)
	if err != nil {
		return nil, err
	}
	// Always populate SystemType with the canonical string for better
	// pretty-printing via GetContents.
	tr.SystemType = s
	return tr, nil
}

// parseTypeString handles primitives, containers (slices, arrays, maps,
// pointers, chans), and user-defined types.
func parseTypeString(s string) (*declaration.TypeReference, error) {
	// Map types: map[K]V
	if strings.HasPrefix(s, "map[") {
		end := strings.Index(s, "]")
		if end < 0 || end <= len("map[") {
			return nil, fmt.Errorf("parseTypeString: malformed map type %q", s)
		}
		keyStr := s[len("map["):end]
		valStr := strings.TrimSpace(s[end+1:])
		if valStr == "" {
			return nil, fmt.Errorf("parseTypeString: map value type missing in %q", s)
		}
		keyTR, err := parseTypeString(strings.TrimSpace(keyStr))
		if err != nil {
			return nil, err
		}
		valTR, err := parseTypeString(valStr)
		if err != nil {
			return nil, err
		}
		return &declaration.TypeReference{
			Kind:        declaration.SystemDefined,
			Container:   declaration.ContainerMap,
			KeyType:     keyTR,
			ElementType: valTR,
		}, nil
	}

	// Slice: []T
	if strings.HasPrefix(s, "[]") {
		inner := strings.TrimSpace(s[2:])
		et, err := parseTypeString(inner)
		if err != nil {
			return nil, err
		}
		return &declaration.TypeReference{
			Kind:        declaration.SystemDefined,
			Container:   declaration.ContainerSlice,
			ElementType: et,
		}, nil
	}

	// Array: [N]T or []T (we'll treat empty N like slice syntactically but
	// still mark it as ContainerArray).
	if strings.HasPrefix(s, "[") {
		end := strings.Index(s, "]")
		if end > 0 {
			lenStr := strings.TrimSpace(s[1:end])
			inner := strings.TrimSpace(s[end+1:])
			if inner == "" {
				return nil, fmt.Errorf("parseTypeString: array element type missing in %q", s)
			}
			et, err := parseTypeString(inner)
			if err != nil {
				return nil, err
			}

			tr := &declaration.TypeReference{
				Kind:        declaration.SystemDefined,
				Container:   declaration.ContainerArray,
				ElementType: et,
			}
			if lenStr != "" {
				if n, err := strconv.Atoi(lenStr); err == nil {
					if tr.AdditionalProperties == nil {
						tr.AdditionalProperties = make(map[string]any)
					}
					tr.AdditionalProperties["ArrayLen"] = n
				}
			}
			return tr, nil
		}
	}

	// Pointer: *T
	if strings.HasPrefix(s, "*") {
		inner := strings.TrimSpace(s[1:])
		et, err := parseTypeString(inner)
		if err != nil {
			return nil, err
		}
		return &declaration.TypeReference{
			Kind:        declaration.SystemDefined,
			Container:   declaration.ContainerPointer,
			ElementType: et,
		}, nil
	}

	// Chan: chan T
	if strings.HasPrefix(s, "chan ") {
		inner := strings.TrimSpace(s[len("chan "):])
		et, err := parseTypeString(inner)
		if err != nil {
			return nil, err
		}
		return &declaration.TypeReference{
			Kind:        declaration.SystemDefined,
			Container:   declaration.ContainerChan,
			ElementType: et,
		}, nil
	}

	// Primitive leaf types.
	switch s {
	case "int":
		return &declaration.TypeReference{Kind: declaration.Primitive, PrimitiveKind: declaration.Integer}, nil
	case "long", "int64":
		return &declaration.TypeReference{Kind: declaration.Primitive, PrimitiveKind: declaration.Long}, nil
	case "float", "float32":
		return &declaration.TypeReference{Kind: declaration.Primitive, PrimitiveKind: declaration.Float}, nil
	case "double", "float64":
		return &declaration.TypeReference{Kind: declaration.Primitive, PrimitiveKind: declaration.Double}, nil
	case "string":
		return &declaration.TypeReference{Kind: declaration.Primitive, PrimitiveKind: declaration.String}, nil
	case "bool":
		return &declaration.TypeReference{Kind: declaration.Primitive, PrimitiveKind: declaration.Bool}, nil
	case "char", "rune":
		return &declaration.TypeReference{Kind: declaration.Primitive, PrimitiveKind: declaration.Char}, nil
	}

	// Otherwise, treat as a user-defined type; resolution to an actual
	// TypeDeclaration happens in later passes.
	return &declaration.TypeReference{
		Kind:       declaration.UserDefined,
		SystemType: s,
	}, nil
}
