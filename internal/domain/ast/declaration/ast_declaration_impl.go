package declaration

import "refacto/internal/domain/ast/statement"

type (
	FunctionDeclaration struct {
		*BaseDeclaration
		Parameters []*FunctionParameter
		ReturnType *FunctionParameter
		Body       []*statement.Statement
	}

	FunctionParameter struct {
		Id            string
		Name          string
		ParameterType *TypeReference
	}

	FunctionParameterTypePrimitiveKind int

	TypeDeclaration struct {
		*BaseDeclaration
		State     []*VariableDeclaration
		Functions []*FunctionDeclaration
	}

	VariableDeclaration struct {
		*BaseDeclaration
		Reference *TypeReference
	}
	TypeReference struct {
		Kind          ReferenceKind
		PrimitiveKind FunctionParameterTypePrimitiveKind
		SystemType    string
		Type          *Declaration
	}
	ReferenceKind int
)

const (
	Integer FunctionParameterTypePrimitiveKind = iota
	Long
	Float
	Double
	String
	Bool
	Char
)

const (
	Primitive ReferenceKind = iota
	SystemDefined
	UserDefined
)
