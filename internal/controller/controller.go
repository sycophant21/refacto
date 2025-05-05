package controller

import (
	"fmt"
	logg "refacto/internal/util/log"
	"reflect"
	"strings"
)

type (
	LLMController interface {
		SetApiKey(apiKey string)
		SetModel(model Model)
		//RefactorFile(fileName string)
		//RefactorFunction()
	}

	BaseController[T Model] struct {
		apiKey string
		model  T
	}

	GoogleController struct {
		*BaseController[GoogleModel]
	}

	OpenAIController struct {
		*BaseController[OpenAIModel]
	}

	AnthropicController struct {
		*BaseController[AnthropicModel]
	}

	PerplexityController struct {
		*BaseController[PerplexityModel]
	}

	XController struct {
		*BaseController[XModel]
	}
)

func (b *BaseController[T]) SetApiKey(apiKey string) {
	b.apiKey = apiKey
}

func (b *BaseController[T]) SetModel(model Model) {
	tm, ok := model.(T)
	if !ok {
		logg.Logger.Error(
			strings.Join(
				[]string{
					"Model Mismatch, expected ",
					reflect.TypeOf(b.model).String(),
					", got ",
					reflect.TypeOf(model).String(),
				}, ""),
			true)
	}
	b.model = tm
}

type Constructor func() LLMController

var constructors = map[LLMType]Constructor{
	Google:     func() LLMController { return &GoogleController{} },
	OpenAI:     func() LLMController { return &OpenAIController{} },
	Anthropic:  func() LLMController { return &AnthropicController{} },
	Perplexity: func() LLMController { return &PerplexityController{} },
	X:          func() LLMController { return &XController{} },
}

func NewController(typ LLMType) (LLMController, error) {
	ctor, ok := constructors[typ]
	if !ok {
		logg.Logger.Error(strings.Join([]string{"unsupported LLM type:", typ.String()}, " "), true)
		return nil, fmt.Errorf("unsupported LLM type: %v", typ)
	}
	return ctor(), nil
}
