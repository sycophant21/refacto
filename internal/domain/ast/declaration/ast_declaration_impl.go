package declaration

import (
	"encoding/json"
	"fmt"
	"refacto/internal/domain/ast/statement"
	"strings"
)

type (
	FunctionDeclaration struct {
		*BaseDeclaration
		Receiver   *MethodReceiver      `json:"Receiver,omitempty"`
		Parameters []*FunctionParameter `json:"Parameters,omitempty"`
		//ReturnType  *FunctionParameter    `json:"ReturnType,omitempty"` // Deprecated: use ReturnTypes
		ReturnTypes []*FunctionParameter  `json:"ReturnTypes,omitempty"`
		Body        []statement.Statement `json:"Body,omitempty"`
	}

	MethodReceiver struct {
		Name      string         `json:"Name,omitempty"`
		Type      *TypeReference `json:"Type,omitempty"`
		IsPointer bool           `json:"IsPointer,omitempty"`
	}

	FunctionParameter struct {
		Id            string         `json:"Id,omitempty"`
		Name          string         `json:"Name,omitempty"`
		ParameterType *TypeReference `json:"ParameterType,omitempty"`
	}

	FunctionParameterTypePrimitiveKind int

	TypeDeclaration struct {
		*BaseDeclaration
		TypeKind  TypeKind               `json:"TypeKind,omitempty"`
		State     []*VariableDeclaration `json:"State,omitempty"`
		Functions []*FunctionDeclaration `json:"Functions,omitempty"`
	}

	TypeKind string

	VariableDeclaration struct {
		*BaseDeclaration
		Reference *TypeReference `json:"Reference,omitempty"`
	}
	TypeReference struct {
		Kind                 ReferenceKind
		PrimitiveKind        FunctionParameterTypePrimitiveKind
		SystemType           string
		Type                 Declaration // Pointer to actual TypeDeclaration; nil if not yet resolved
		AdditionalProperties map[string]interface{}
		IsVariadic           bool // marks ...T parameters

		// Container/type-constructor information (language-agnostic).
		Container   ContainerKind  `json:"Container,omitempty"`
		ElementType *TypeReference `json:"ElementType,omitempty"`
		KeyType     *TypeReference `json:"KeyType,omitempty"`
	}
	ContainerKind int
	ReferenceKind int
)

const (
	// Integer represents signed integer primitives (e.g. int, int32).
	Integer FunctionParameterTypePrimitiveKind = iota
	// Long represents wider signed integers (e.g. long, int64) for languages
	// that distinguish them.
	Long
	// Float represents single-precision floating point values.
	Float
	// Double represents double-precision floating point values.
	Double
	// String represents text/string primitives.
	String
	// Bool represents boolean primitives.
	Bool
	// Char represents character/codepoint primitives (e.g. char, rune).
	Char
	// UnsignedInteger represents unsigned integer primitives (e.g. uint, uint32).
	UnsignedInteger
)

const (
	Primitive ReferenceKind = iota
	SystemDefined
	UserDefined
)

const (
	StructType    TypeKind = "struct"
	InterfaceType TypeKind = "interface"
	AliasType     TypeKind = "alias"
)

const (
	ContainerNone ContainerKind = iota
	ContainerSlice
	ContainerArray
	ContainerMap
	ContainerChan
	ContainerPointer
)

// BaseDeclaration methods
func (b *BaseDeclaration) GetId() string {
	return b.Id
}

func (b *BaseDeclaration) GetContents() string {
	return b.Contents
}

// Declaration type discriminators
func (f *FunctionDeclaration) GetDeclarationType() string {
	return "Function"
}

func (t *TypeDeclaration) GetDeclarationType() string {
	return "Type"
}

func (v *VariableDeclaration) GetDeclarationType() string {
	return "Variable"
}

// JSON marshaling for ContainerKind to serialize as strings
func (ck ContainerKind) MarshalJSON() ([]byte, error) {
	switch ck {
	case ContainerNone:
		return json.Marshal("None")
	case ContainerSlice:
		return json.Marshal("Slice")
	case ContainerArray:
		return json.Marshal("Array")
	case ContainerMap:
		return json.Marshal("Map")
	case ContainerChan:
		return json.Marshal("Chan")
	case ContainerPointer:
		return json.Marshal("Pointer")
	default:
		return json.Marshal("Unknown")
	}
}

// FunctionDeclaration
func (f *FunctionDeclaration) GetContents() string {
	if f.BaseDeclaration != nil && f.BaseDeclaration.Contents != "" && f.BaseDeclaration.Contents != "func" {
		return f.BaseDeclaration.Contents
	}
	var params []string
	for _, p := range f.Parameters {
		// Simplified type handling
		typeName := p.ParameterType.SystemType
		if typeName == "" {
			typeName = "int" // Default/Fallback
		}
		params = append(params, fmt.Sprintf("%s %s", p.Name, typeName))
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("func %s(%s)", f.Name, strings.Join(params, ", ")))

	sb.WriteString(" {\n")
	for _, stmt := range f.Body {
		sb.WriteString(stmt.GetContents())
		sb.WriteString("\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// TypeDeclaration
func (t *TypeDeclaration) GetContents() string {
	if t.BaseDeclaration != nil && t.BaseDeclaration.Contents != "" && t.BaseDeclaration.Contents != "type" {
		return t.BaseDeclaration.Contents
	}
	// Simplified struct/interface printing
	return fmt.Sprintf("type %s struct {}", t.Name)
}

func (t *TypeDeclaration) GetSpecificationType() string {
	return "TypeSpecification"
}

// VariableDeclaration
func (v *VariableDeclaration) GetContents() string {
	if v.BaseDeclaration != nil && v.BaseDeclaration.Contents != "" && v.BaseDeclaration.Contents != "var" {
		return v.BaseDeclaration.Contents
	}
	typeName := v.Reference.SystemType
	if typeName == "" {
		typeName = "int"
	}
	return fmt.Sprintf("var %s %s", v.Name, typeName)
}

func (v *VariableDeclaration) GetSpecificationType() string {
	return "VariableSpecification"
}

// Custom JSON marshaler for TypeReference to include Type field info
func (tr *TypeReference) MarshalJSON() ([]byte, error) {
	// Build a map for dynamic JSON output
	result := make(map[string]interface{})

	// Always include Kind
	result["Kind"] = tr.Kind

	// Include PrimitiveKind only if Kind is Primitive (even if it's 0/Integer)
	if tr.Kind == Primitive {
		result["PrimitiveKind"] = tr.PrimitiveKind
	}

	// Include SystemType only if it's not empty
	if tr.SystemType != "" {
		result["SystemType"] = tr.SystemType
	}

	// Include AdditionalProperties only if not empty
	if len(tr.AdditionalProperties) > 0 {
		result["AdditionalProperties"] = tr.AdditionalProperties
	}

	// Include IsVariadic if true
	if tr.IsVariadic {
		result["IsVariadic"] = tr.IsVariadic
	}

	// Include container info if set
	if tr.Container != ContainerNone {
		result["Container"] = tr.Container
	}
	if tr.ElementType != nil {
		result["ElementType"] = tr.ElementType
	}
	if tr.KeyType != nil {
		result["KeyType"] = tr.KeyType
	}

	// If Type is set and is a TypeDeclaration, include its ID
	if tr.Type != nil {
		if typeDecl, ok := tr.Type.(*TypeDeclaration); ok {
			result["TypeId"] = typeDecl.Id
		}
	}

	return json.Marshal(result)
}

// Custom JSON marshaling for ReferenceKind enum
func (rk ReferenceKind) MarshalJSON() ([]byte, error) {
	switch rk {
	case Primitive:
		return json.Marshal("Primitive")
	case SystemDefined:
		return json.Marshal("SystemDefined")
	case UserDefined:
		return json.Marshal("UserDefined")
	default:
		return json.Marshal("Unknown")
	}
}

func (rk *ReferenceKind) UnmarshalJSON(b []byte) error {
	var s string
	if err := json.Unmarshal(b, &s); err != nil {
		return err
	}
	switch s {
	case "Primitive":
		*rk = Primitive
	case "SystemDefined":
		*rk = SystemDefined
	case "UserDefined":
		*rk = UserDefined
	}
	return nil
}

// Custom JSON marshaling for FunctionParameterTypePrimitiveKind enum
func (pk FunctionParameterTypePrimitiveKind) MarshalJSON() ([]byte, error) {
	switch pk {
	case Integer:
		return json.Marshal("Integer")
	case Long:
		return json.Marshal("Long")
	case Float:
		return json.Marshal("Float")
	case Double:
		return json.Marshal("Double")
	case String:
		return json.Marshal("String")
	case Bool:
		return json.Marshal("Bool")
	case Char:
		return json.Marshal("Char")
	case UnsignedInteger:
		return json.Marshal("UnsignedInteger")
	default:
		return json.Marshal("Unknown")
	}
}

func (pk *FunctionParameterTypePrimitiveKind) UnmarshalJSON(b []byte) error {
	var s string
	if err := json.Unmarshal(b, &s); err != nil {
		return err
	}
	switch s {
	case "Integer":
		*pk = Integer
	case "Long":
		*pk = Long
	case "Float":
		*pk = Float
	case "Double":
		*pk = Double
	case "String":
		*pk = String
	case "Bool":
		*pk = Bool
	case "Char":
		*pk = Char
	case "UnsignedInteger":
		*pk = UnsignedInteger
	}
	return nil
}
