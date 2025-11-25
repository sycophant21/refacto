package parser

import (
	"errors"
	"sync"
)

var (
	parsersMu sync.RWMutex
	parsers   = make(map[Language]LanguageParser)
)

func RegisterParser(parser LanguageParser) error {
	parsersMu.Lock()
	defer parsersMu.Unlock()

	lang := parser.GetLanguage()
	if _, exists := parsers[lang]; exists {
		return errors.New("parser for " + lang.String() + " already registered")
	}
	parsers[lang] = parser
	return nil
}

func GetParser(language Language) (LanguageParser, error) {
	parsersMu.RLock()
	defer parsersMu.RUnlock()

	parser, exists := parsers[language]
	if !exists {
		return nil, errors.New("no parser found for language: " + language.String())
	}
	return parser, nil
}

func ListRegisteredLanguages() []string {
	parsersMu.RLock()
	defer parsersMu.RUnlock()

	languages := make([]string, 0, len(parsers))
	for lang := range parsers {
		languages = append(languages, lang.String())
	}
	return languages
}
