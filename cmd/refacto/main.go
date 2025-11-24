package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"

	"refacto/internal/domain/ast/declaration"
	go_parser "refacto/internal/domain/parser/go_lang"
	logg "refacto/internal/util/log"
	program "refacto/internal/util/program"
	parserpkg "refacto/pkg/parser"
)

func init() {
	err := program.LoadProperties()
	if err != nil {
		log.Fatal(err)
	}
	err = logg.Initialise(10)
	if err != nil {
		log.Fatal(err)
	}

	// Register the Go parser in the global parser registry.
	if err := parserpkg.RegisterParser(go_parser.NewGoParser()); err != nil {
		log.Fatal(err)
	}
}

func main() {
	flag.Parse()
	args := flag.Args()

	var filePath string
	if len(args) > 0 {
		filePath = args[0]
	} else {
		filePath = "cmd/refacto/test_data/complex_test.go"
	}

	// Use the parser registry to obtain the Go parser.
	p, err := parserpkg.GetParser(parserpkg.Go)
	if err != nil {
		log.Fatal(err)
	}

	file, err := p.ParseFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Parsed File: %s\n", file.Name)

	// Optionally print registered languages to exercise the registry API.
	fmt.Printf("Registered languages: %v\n", parserpkg.ListRegisteredLanguages())

	// Clear Contents fields before serialization to avoid cluttering output
	for _, decl := range file.Declarations {
		switch d := (*decl).(type) {
		case *declaration.FunctionDeclaration:
			d.Contents = ""
		case *declaration.TypeDeclaration:
			d.Contents = ""
		case *declaration.VariableDeclaration:
			d.Contents = ""
		}
	}

	// Serialize to JSON
	jsonData, err := json.MarshalIndent(file, "", "  ")
	if err != nil {
		log.Fatalf("Error serializing AST to JSON: %v", err)
	}

	fmt.Println("\nAST JSON:")
	fmt.Println(string(jsonData))
}
