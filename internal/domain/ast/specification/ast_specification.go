package specification

type (
	Specification interface {
		GetId() string
		GetSpecificationType() string
		GetContents() string
	}
	SpecificationType int
)
