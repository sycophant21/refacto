package controller

type (
	LLMType int
)

func (l LLMType) String() string {
	return []string{"Google", "OpenAI", "Anthropic", "Perplexity", "X"}[l]
}

const (
	Google LLMType = iota
	OpenAI
	Anthropic
	Perplexity
	X
)

type (
	Model interface {
		getUrl()
	}

	GoogleModel interface {
		Model
	}
	OpenAIModel interface {
		Model
	}
	AnthropicModel interface {
		Model
	}
	PerplexityModel interface {
		Model
	}
	XModel interface {
		Model
	}
)
