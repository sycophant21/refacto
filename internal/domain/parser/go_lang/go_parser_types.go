package go_lang

import (
	"fmt"
	"strings"

	"refacto/internal/domain/ast/declaration"
	"refacto/internal/domain/ast/specification"
)

// parseParameterType parses the type part of a single function parameter and
// returns the raw type string (including any variadic or composite prefixes).
func (p *Parser) parseParameterType() string {
	var paramType string

	// Check for variadic (...)
	if p.curTokenIs("...") {
		paramType = "..."
		p.nextToken()
	}

	// If we only saw "..." and then a comma or ')', there's no type to parse
	if !(paramType != "" || !(p.curTokenIs(",") || p.curTokenIs(")"))) {
		return paramType
	}

	// Parse the type portion based on the current token kind
	switch {
	case p.curTokenIs("["):
		paramType = p.parseParamArrayOrSliceType()
	case p.curTokenIs("chan"):
		paramType = p.parseParamChanType()
	case p.curTokenIs("map"):
		paramType = p.parseParamMapType()
	case p.curTokenIs("interface"):
		paramType = p.parseParamInterfaceType()
	case p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD):
		paramType = p.parseParamSimpleOrVariadicType(paramType)
	}

	return paramType
}

// --- Parameter type helpers ---

func (p *Parser) parseParamArrayOrSliceType() string {
	// Slice or array type: [N]T or []T
	paramType := "["
	p.nextToken()
	// Skip size or leave empty for slice
	for !p.curTokenIs("]") && !p.curTokenIs("EOF") {
		paramType += p.curToken.Value()
		p.nextToken()
	}
	if p.curTokenIs("]") {
		paramType += "]"
		p.nextToken()
	}
	// Element type
	if p.curTokenIs("*") {
		paramType += "*"
		p.nextToken()
	}
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
		paramType += p.curToken.Value()
		p.nextToken()
	}
	return paramType
}

func (p *Parser) parseParamChanType() string {
	// Channel type: chan T or chan *T
	var paramType string
	p.nextToken()
	if p.curTokenIs("*") {
		p.nextToken()
		if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
			paramType = "chan *" + p.curToken.Value()
			p.nextToken()
		}
	} else if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
		paramType = "chan " + p.curToken.Value()
		p.nextToken()
	}
	return paramType
}

func (p *Parser) parseParamMapType() string {
	// Map type: map[keyType]valueType
	paramType := "map"
	p.nextToken() // skip 'map'
	if p.curTokenIs("[") {
		paramType += "["
		p.nextToken()
		// Parse key type
		for !p.curTokenIs("]") && !p.curTokenIs("EOF") {
			paramType += p.curToken.Value()
			p.nextToken()
		}
		if p.curTokenIs("]") {
			paramType += "]"
			p.nextToken()
		}
		// Parse value type
		if p.curTokenIs("*") {
			paramType += "*"
			p.nextToken()
		}
		if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
			paramType += p.curToken.Value()
			p.nextToken()
		}
	}
	return paramType
}

func (p *Parser) parseParamInterfaceType() string {
	// interface{} type
	var paramType string
	p.nextToken() // skip 'interface'
	if p.curTokenIs("{") {
		p.nextToken() // skip '{'
		if p.curTokenIs("}") {
			paramType = "interface{}"
			p.nextToken()
		}
	}
	return paramType
}

func (p *Parser) parseParamSimpleOrVariadicType(current string) string {
	// Simple identifier or variadic base type
	if current != "" && current != "..." {
		// Keep existing type
		return current
	}

	if current == "..." {
		current += p.curToken.Value()
	} else {
		current = p.curToken.Value()
	}
	p.nextToken()
	return current
}

// first token of the return type section (or '{' if there is none).
func (p *Parser) parseFunctionReturnTypes(funcDecl *declaration.FunctionDeclaration) {
	if p.curTokenIs("{") {
		return
	}

	if p.curTokenIs("(") {
		p.parseParenthesizedReturnTypes(funcDecl)
		return
	}

	p.parseSingleReturnType(funcDecl)
}

// parseParenthesizedReturnTypes handles return signatures of the form
// `(T1, T2)` or `(name T, err error)`.
func (p *Parser) parseParenthesizedReturnTypes(funcDecl *declaration.FunctionDeclaration) {
	p.nextToken() // skip '('
	for !p.curTokenIs(")") && !p.curTokenIs("EOF") {
		var paramName, paramType string

		// Could be named return: (result string, err error)
		// Or unnamed: (int, error)
		// Or mixed types: (int, *MyStruct)

		// First token could be a name or a type (or 'chan')
		firstToken := p.curToken.Value()
		firstSub := p.curToken.SubCategory()

		// Check for 'chan' keyword first
		if firstToken == "chan" {
			p.nextToken() // skip 'chan'
			// Check if this is 'chan name type' or just 'chan type'
			if p.curTokenIs("*") {
				p.nextToken()
				if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
					p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
					p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
					p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
					paramType = "chan *" + p.curToken.Value()
					p.nextToken()
				}
				// Check if next is comma or paren to know if paramType is set
				if !p.curTokenIs(",") && !p.curTokenIs(")") {
					// There's another identifier, might be a name
					if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
						paramName = paramType // shift
						paramType = p.curToken.Value()
						p.nextToken()
					}
				}
			} else if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
				p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
				p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
				p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
				firstToken2 := p.curToken.Value()
				p.nextToken()
				// Check if next is comma/paren or another identifier
				if p.curTokenIs(",") || p.curTokenIs(")") {
					paramType = "chan " + firstToken2
				} else if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
					p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
					p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
					p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
					paramName = "chan " + firstToken2
					paramType = p.curToken.Value()
					p.nextToken()
				}
			}
		} else {
			// Check if it's a type keyword or identifier
			isTypeKeyword := firstSub == GoLexTokenSubCategory(INT_KEYWORD) ||
				firstSub == GoLexTokenSubCategory(STRING_KEYWORD) ||
				firstSub == GoLexTokenSubCategory(BOOL_KEYWORD) ||
				firstSub == GoLexTokenSubCategory(IDENT)

			if isTypeKeyword {
				p.nextToken()

				// If next is comma or closing paren, then firstToken was the type
				if p.curTokenIs(",") || p.curTokenIs(")") {
					paramType = firstToken
				} else {
					// firstToken was a name, current token should be the type
					paramName = firstToken
					if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
						p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
						p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
						p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
						paramType = p.curToken.Value()
						p.nextToken()
					}
				}
			} else {
				// Unknown token - skip to avoid infinite loop
				p.nextToken()
				continue
			}
		}

		if paramType != "" {
			funcDecl.ReturnTypes = append(funcDecl.ReturnTypes, &declaration.FunctionParameter{
				Name:          paramName,
				ParameterType: p.determineTypeReference(paramType, funcDecl.Id+":return"),
			})
		}

		if p.curTokenIs(",") {
			p.nextToken()
		}
	}
	if p.curTokenIs(")") {
		p.nextToken()
	}
}

// parseSingleReturnType handles the simple form `func f() T`.
func (p *Parser) parseSingleReturnType(funcDecl *declaration.FunctionDeclaration) {
	var returnType string
	if p.curTokenIs("chan") {
		p.nextToken()
		if p.curTokenIs("*") {
			p.nextToken()
			if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
				returnType = "chan *" + p.curToken.Value()
				p.nextToken()
			}
		} else if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
			returnType = "chan " + p.curToken.Value()
			p.nextToken()
		}
	} else if p.curTokenIs("*") {
		p.nextToken()
		if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
			returnType = "*" + p.curToken.Value()
			p.nextToken()
		}
	} else if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
		returnType = p.curToken.Value()
		p.nextToken()
	}

	if returnType != "" {
		ret := &declaration.FunctionParameter{
			ParameterType: p.determineTypeReference(returnType, funcDecl.Id+":return"),
		}
		funcDecl.ReturnTypes = []*declaration.FunctionParameter{ret}
	}
}

// parseTypeDeclaration parses a `type` declaration and returns the resulting
// TypeDeclaration. It also registers the type in the parser's registry.
func (p *Parser) parseTypeDeclaration() declaration.Declaration {
	startOffset := p.curToken.Position().Offset
	p.nextToken() // skip 'type'

	typeDecl := &declaration.TypeDeclaration{
		BaseDeclaration: &declaration.BaseDeclaration{},
	}

	p.parseTypeName(typeDecl)
	lastTokenEndOffset := p.parseTypeBody(typeDecl)

	endOffset := lastTokenEndOffset
	if startOffset < len(p.content) && endOffset <= len(p.content) {
		typeDecl.Contents = p.content[startOffset:endOffset]
	} else {
		typeDecl.Contents = "Error capturing content"
	}

	// Register the type as soon as it's parsed so later references can link immediately
	p.registerTypeDeclaration(typeDecl)
	return typeDecl
}

// parseTypeName reads the identifier after the 'type' keyword and populates
// the TypeDeclaration's Name and Id.
func (p *Parser) parseTypeName(typeDecl *declaration.TypeDeclaration) {
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		typeDecl.Name = p.curToken.Value()
		typeDecl.Id = fmt.Sprintf("%s.%s", p.packageName, typeDecl.Name)
		p.nextToken()
	}
}

// parseTypeBody dispatches based on the current token (struct/interface/alias)
// and returns the end offset of the last token that belongs to this type
// declaration.
func (p *Parser) parseTypeBody(typeDecl *declaration.TypeDeclaration) int {
	var lastTokenEndOffset int

	if p.curTokenIs("struct") {
		lastTokenEndOffset = p.parseStructTypeBody(typeDecl)
	} else if p.curTokenIs("interface") {
		lastTokenEndOffset = p.parseInterfaceTypeBody(typeDecl)
	} else {
		lastTokenEndOffset = p.parseAliasTypeBody(typeDecl)
	}

	return lastTokenEndOffset
}

// parseStructTypeBody parses a struct type definition, including all of its
// fields, and returns the end offset of the closing '}'.
func (p *Parser) parseStructTypeBody(typeDecl *declaration.TypeDeclaration) int {
	typeDecl.TypeKind = declaration.StructType
	p.nextToken() // skip 'struct'

	var lastTokenEndOffset int
	if !p.curTokenIs("{") {
		return lastTokenEndOffset
	}

	p.nextToken() // skip '{'
	for !p.curTokenIs("}") && !p.curTokenIs("EOF") {
		p.parseSingleStructField(typeDecl)
	}
	if p.curTokenIs("}") {
		lastTokenEndOffset = p.curToken.Position().Offset + len(p.curToken.Value())
		p.nextToken() // Consume '}'
	}
	return lastTokenEndOffset
}

func (p *Parser) parseSingleStructField(typeDecl *declaration.TypeDeclaration) {
	if p.curToken.SubCategory() != GoLexTokenSubCategory(IDENT) {
		p.nextToken()
		return
	}

	field := &declaration.VariableDeclaration{
		BaseDeclaration: &declaration.BaseDeclaration{},
	}
	field.Name = p.curToken.Value()
	field.Id = fmt.Sprintf("%s.%s", typeDecl.Id, field.Name)
	p.nextToken()

	fieldType := p.parseStructFieldType()
	if fieldType != "" {
		field.Reference = p.determineTypeReference(fieldType, field.Id)
	}

	typeDecl.State = append(typeDecl.State, field)
}

// parseStructFieldType parses the type portion of a struct field declaration
// and returns it as a raw string (e.g. "[]string", "*MyType", "map[string]int").
func (p *Parser) parseStructFieldType() string {
	// Parse type modifiers and base type via small helpers per kind
	if p.curTokenIs("[") {
		return p.parseArrayOrSliceFieldType()
	}
	if p.curTokenIs("map") {
		return p.parseMapFieldType()
	}
	if p.curTokenIs("*") {
		return p.parsePointerFieldType()
	}
	if p.curTokenIs("chan") {
		return p.parseChanFieldType()
	}
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
		return p.parseSimpleFieldType()
	}
	return ""
}

func (p *Parser) parseArrayOrSliceFieldType() string {
	// Array or slice: [...], [size], or []
	fieldType := "["
	p.nextToken()
	// Skip array size or leave empty for slice
	for !p.curTokenIs("]") && !p.curTokenIs("EOF") {
		fieldType += p.curToken.Value()
		p.nextToken()
	}
	if p.curTokenIs("]") {
		fieldType += "]"
		p.nextToken()
	}
	// Now parse the element type
	if p.curTokenIs("*") {
		fieldType += "*"
		p.nextToken()
	}
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
		fieldType += p.curToken.Value()
		p.nextToken()
	}
	return fieldType
}

func (p *Parser) parseMapFieldType() string {
	// Map type: map[keyType]valueType
	fieldType := "map"
	p.nextToken() // skip 'map'
	if p.curTokenIs("[") {
		fieldType += "["
		p.nextToken()
		// Parse key type
		for !p.curTokenIs("]") && !p.curTokenIs("EOF") {
			fieldType += p.curToken.Value()
			p.nextToken()
		}
		if p.curTokenIs("]") {
			fieldType += "]"
			p.nextToken()
		}
		// Parse value type
		if p.curTokenIs("*") {
			fieldType += "*"
			p.nextToken()
		}
		if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
			p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
			fieldType += p.curToken.Value()
			p.nextToken()
		}
	}
	return fieldType
}

func (p *Parser) parsePointerFieldType() string {
	// Pointer type
	fieldType := "*"
	p.nextToken()
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) {
		fieldType += p.curToken.Value()
		p.nextToken()
	}
	return fieldType
}

func (p *Parser) parseChanFieldType() string {
	// Channel type
	fieldType := "chan"
	p.nextToken()
	if p.curTokenIs("*") {
		fieldType += " *"
		p.nextToken()
	}
	if p.curToken.SubCategory() == GoLexTokenSubCategory(IDENT) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(INT_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(STRING_KEYWORD) ||
		p.curToken.SubCategory() == GoLexTokenSubCategory(BOOL_KEYWORD) {
		if strings.HasSuffix(fieldType, "*") {
			fieldType += p.curToken.Value()
		} else {
			fieldType += " " + p.curToken.Value()
		}
		p.nextToken()
	}
	return fieldType
}

func (p *Parser) parseSimpleFieldType() string {
	// Simple identifier or basic type
	fieldType := p.curToken.Value()
	p.nextToken()
	return fieldType
}

// parseInterfaceTypeBody parses an interface type definition and returns the
// end offset of the closing '}'. The current implementation skips the body.
func (p *Parser) parseInterfaceTypeBody(typeDecl *declaration.TypeDeclaration) int {
	typeDecl.TypeKind = declaration.InterfaceType
	p.nextToken() // Consume 'interface'

	var lastTokenEndOffset int
	// Skip interface body for now
	if p.curTokenIs("{") {
		p.nextToken() // Consume '{'
		for !p.curTokenIs("}") && !p.curTokenIs("EOF") {
			p.nextToken()
		}
		if p.curTokenIs("}") {
			lastTokenEndOffset = p.curToken.Position().Offset + len(p.curToken.Value())
			p.nextToken() // Consume '}'
		}
	}
	return lastTokenEndOffset
}

// parseAliasTypeBody parses an alias or simple type definition and returns the
// end offset of the last token belonging to the type.
func (p *Parser) parseAliasTypeBody(typeDecl *declaration.TypeDeclaration) int {
	// Alias or simple type
	typeDecl.TypeKind = declaration.AliasType
	// Consume the type token
	lastTokenEndOffset := p.curToken.Position().Offset + len(p.curToken.Value())
	p.nextToken()
	return lastTokenEndOffset
}

func (p *Parser) parseImport() []specification.Specification {
	var specs []specification.Specification
	p.nextToken() // skip 'import'

	if p.curTokenIs("(") {
		p.nextToken() // skip '('
		for !p.curTokenIs(")") && !p.curTokenIs("EOF") {
			if p.curToken.SubCategory() == GoLexTokenSubCategory(STRING) {
				path := p.curToken.Value()
				spec := &specification.ImportSpecification{
					Id:       path, // Use path as ID for now
					Path:     path,
					Contents: fmt.Sprintf("import %s", path),
				}
				specs = append(specs, spec)
				p.nextToken()
			} else {
				p.nextToken()
			}
		}
		if p.curTokenIs(")") {
			p.nextToken()
		}
	} else {
		if p.curToken.SubCategory() == GoLexTokenSubCategory(STRING) {
			path := p.curToken.Value()
			spec := &specification.ImportSpecification{
				Id:       path,
				Path:     path,
				Contents: fmt.Sprintf("import %s", path),
			}
			specs = append(specs, spec)
			p.nextToken()
		}
	}
	return specs
}

func (p *Parser) determineTypeReference(typeName string, ownerContext string) *declaration.TypeReference {
	clean, isVariadic := normalizeTypeName(typeName)

	if ref := determineSystemTypeReference(clean, isVariadic); ref != nil {
		return ref
	}

	cleanName := stripTypePrefixes(clean)
	if ref := determinePrimitiveTypeReference(cleanName, isVariadic); ref != nil {
		return ref
	}

	return p.determineUserDefinedTypeReference(cleanName, isVariadic, ownerContext)
}

// normalizeTypeName trims whitespace and extracts a leading variadic prefix, if present.
func normalizeTypeName(typeName string) (clean string, isVariadic bool) {
	clean = strings.TrimSpace(typeName)
	if strings.HasPrefix(clean, "...") {
		isVariadic = true
		clean = strings.TrimSpace(strings.TrimPrefix(clean, "..."))
	}
	return clean, isVariadic
}

// determineSystemTypeReference builds a TypeReference for system-defined types
// like map, chan, slices, arrays, and interface{}.
func determineSystemTypeReference(clean string, isVariadic bool) *declaration.TypeReference {
	if !(strings.HasPrefix(clean, "map") ||
		strings.HasPrefix(clean, "chan") ||
		strings.HasPrefix(clean, "[]") || strings.HasPrefix(clean, "[") ||
		clean == "error" ||
		clean == "any" ||
		clean == "interface{}") {
		return nil
	}

	ref := &declaration.TypeReference{
		Kind:       declaration.SystemDefined,
		SystemType: clean,
	}

	props := map[string]interface{}{}
	if strings.HasPrefix(clean, "[]") {
		props["ElementType"] = strings.TrimPrefix(clean, "[]")
		props["IsDynamic"] = true
	} else if strings.HasPrefix(clean, "[") {
		if idx := strings.Index(clean, "]"); idx != -1 {
			sizeStr := strings.TrimPrefix(clean[:idx+1], "[")
			sizeStr = strings.TrimSuffix(sizeStr, "]")
			props["Size"] = sizeStr
			props["ElementType"] = strings.TrimSpace(clean[idx+1:])
		}
	} else if strings.HasPrefix(clean, "map[") {
		if idx := strings.Index(clean, "]"); idx != -1 {
			props["KeyType"] = strings.TrimPrefix(clean[:idx+1], "map[")
			props["ValueType"] = strings.TrimSpace(clean[idx+1:])
		}
	} else if strings.HasPrefix(clean, "chan") {
		elt := strings.TrimSpace(strings.TrimPrefix(clean, "chan"))
		if strings.HasPrefix(elt, "*") {
			props["ElementType"] = strings.TrimPrefix(elt, "*")
			props["IsPointer"] = true
		} else {
			props["ElementType"] = elt
		}
	}

	if len(props) > 0 {
		ref.AdditionalProperties = props
	}
	if isVariadic {
		if ref.AdditionalProperties == nil {
			ref.AdditionalProperties = map[string]interface{}{}
		}
		ref.IsVariadic = true
	}
	return ref
}

// stripTypePrefixes removes pointer/slice/array/map prefixes to expose the
// underlying base type name for primitive vs user-defined classification.
func stripTypePrefixes(clean string) string {
	cleanName := strings.TrimPrefix(clean, "*")
	for strings.HasPrefix(cleanName, "[]") {
		cleanName = strings.TrimPrefix(cleanName, "[]")
	}
	if strings.HasPrefix(cleanName, "[") {
		if idx := strings.Index(cleanName, "]"); idx != -1 {
			cleanName = cleanName[idx+1:]
		}
	}
	if strings.HasPrefix(cleanName, "map[") {
		if idx := strings.Index(cleanName, "]"); idx != -1 && idx+1 < len(cleanName) {
			cleanName = cleanName[idx+1:]
		}
	}
	return cleanName
}

// determinePrimitiveTypeReference returns a TypeReference for primitive types
// like int, string, bool, etc.
func determinePrimitiveTypeReference(cleanName string, isVariadic bool) *declaration.TypeReference {
	switch cleanName {
	case "int", "int8", "int16", "int32", "int64":
		tr := &declaration.TypeReference{
			Kind:          declaration.Primitive,
			PrimitiveKind: declaration.Integer,
		}
		tr.IsVariadic = isVariadic
		return tr
	case "uint", "uint8", "uint16", "uint32", "uint64", "uintptr", "byte", "rune":
		tr := &declaration.TypeReference{
			Kind:          declaration.Primitive,
			PrimitiveKind: declaration.Integer,
		}
		tr.IsVariadic = isVariadic
		return tr
	case "float32", "float64":
		tr := &declaration.TypeReference{
			Kind:          declaration.Primitive,
			PrimitiveKind: declaration.Float,
		}
		tr.IsVariadic = isVariadic
		return tr
	case "string":
		tr := &declaration.TypeReference{
			Kind:          declaration.Primitive,
			PrimitiveKind: declaration.String,
		}
		tr.IsVariadic = isVariadic
		return tr
	case "bool":
		tr := &declaration.TypeReference{
			Kind:          declaration.Primitive,
			PrimitiveKind: declaration.Bool,
		}
		tr.IsVariadic = isVariadic
		return tr
	}
	return nil
}

// determineUserDefinedTypeReference builds a TypeReference for user-defined
// types, integrating with the parser's type registry and pending references.
func (p *Parser) determineUserDefinedTypeReference(cleanName string, isVariadic bool, ownerContext string) *declaration.TypeReference {
	// Build fully-qualified name if not provided
	fq := cleanName
	if !strings.Contains(cleanName, ".") && p.packageName != "" {
		fq = p.packageName + "." + cleanName
	}

	ref := &declaration.TypeReference{Kind: declaration.UserDefined, IsVariadic: isVariadic}

	// Check if type is in registry (may be found immediately or later)
	if decl, ok := p.typeRegistry[fq]; ok && decl != nil {
		// Store as interface - TypeDeclaration implements Declaration
		ref.Type = decl
	} else {
		// Not found yet: record pending for resolution after all declarations parsed
		p.pendingReferences = append(p.pendingReferences, struct {
			typeName     string
			reference    *declaration.TypeReference
			ownerContext string
		}{typeName: fq, reference: ref, ownerContext: ownerContext})
	}
	return ref
}

// --- Type linking helpers ---
func (p *Parser) registerTypeDeclaration(typeDecl *declaration.TypeDeclaration) {
	if typeDecl == nil || typeDecl.Id == "" {
		return
	}
	p.typeRegistry[typeDecl.Id] = typeDecl
}

func (p *Parser) resolvePendingReferences() {
	if len(p.pendingReferences) == 0 {
		return
	}
	unresolved := make([]struct {
		typeName     string
		reference    *declaration.TypeReference
		ownerContext string
	}, 0)
	for _, pr := range p.pendingReferences {
		if decl, ok := p.typeRegistry[pr.typeName]; ok && decl != nil {
			// Store as interface - TypeDeclaration implements Declaration
			pr.reference.Type = decl
		} else {
			unresolved = append(unresolved, pr)
		}
	}
	p.pendingReferences = unresolved
	p.logUnresolvedReferences()
}

func (p *Parser) logUnresolvedReferences() {
	if len(p.pendingReferences) == 0 {
		return
	}
	for _, pr := range p.pendingReferences {
		logger.Error(fmt.Sprintf("Unresolved type reference: %s (owner: %s)", pr.typeName, pr.ownerContext), false)
	}
}
