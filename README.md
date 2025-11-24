# refacto

> A language-agnostic, pluggable AST framework for analyzing and transforming source code — enabling AI-assisted refactoring and developer tooling across multiple programming languages.

---

## 🎯 Project Vision

**refacto** aims to build a unified, language-agnostic infrastructure for code analysis and transformation. The long-term goal is to enable seamless AI-powered refactoring, code generation, and developer tooling across any programming language through a consistent, extensible AST representation.

### Key Goals

- **Universal Code Analysis**: Parse and analyze source code from multiple languages using a single, consistent AST model
- **AI-Ready Infrastructure**: Provide LLM-friendly abstractions for code understanding and transformation
- **Zero Friction Language Addition**: Make it trivial to add support for new programming languages
- **Developer Tooling Platform**: Enable build innovative features like intelligent refactoring, code quality analysis, and automated migrations

---

## ✅ Currently Implemented

### Language Support

- **Go Parser** ✓
  - Full function and method parsing with receiver types
  - Struct and interface definitions with fields and methods
  - Type aliases and variable declarations
  - All for loop variants: traditional, range, condition-only, and infinite loops
  - Variadic parameter support with proper type marking
  - Comparison expressions as first-class AST nodes
  - Complete expression parsing (operations, calls, selectors, literals)

### Core Architecture

- **Unified AST Model** ✓
  - Consistent node types across all languages
  - Clear hierarchy: Program → File → Declarations → Statements → Expressions
  - Domain-driven design with separation of concerns
  - Type-safe node implementations

- **Extensible Parser System** ✓
  - Registry-based parser registration with thread-safe lookups
  - Standard `LanguageParser` interface for easy language addition
  - Modular lexer/parser separation following recursive descent patterns

- **Rich AST Node Types** ✓
  - **Declarations**: Functions, methods, types (structs/interfaces), variables
  - **Statements**: Returns, if/else, for loops, variable declarations, assignments, expressions
  - **Expressions**: Identifiers, literals, infix operations, comparisons, function calls, member access
  - **Specifications**: Imports and file-level metadata

- **Type System** ✓
  - TypeReference tracking with variadic parameter support
  - Method receiver types (distinguishes methods from functions)
  - Return type specifications
  - Parameter tracking with default values

---

## 🚀 Roadmap

### Phase 2: Language Expansion
- [ ] Python parser (simple grammar, high demand for AI tooling)
- [ ] TypeScript/JavaScript parser (web ecosystem integration)
- [ ] Rust parser (systems programming, growing AI use)
- [ ] Java parser (enterprise refactoring potential)

### Phase 3: LLM Integration
- [ ] Multi-provider LLM abstraction (Google, OpenAI, Anthropic, Perplexity, X)
- [ ] Context-aware code embedding generation
- [ ] Semantic code search capabilities
- [ ] AI-powered refactoring suggestions

### Phase 4: Advanced Analysis
- [ ] Symbol resolution and scope analysis
- [ ] Type inference for dynamic languages
- [ ] Control flow graph generation
- [ ] Data flow analysis

### Phase 5: Developer Tools & UI
- [ ] Web-based UI for code visualization
- [ ] Interactive refactoring playground
- [ ] Integration with popular IDEs (VSCode, IntelliJ)
- [ ] CLI tools for batch analysis and transformation
- [ ] Code migration automation

---

## 🏗️ Architecture

### Design Principles

- **Language Agnostic**: AST model independent of specific language syntax
- **Pluggable**: Add new language parsers without modifying core framework
- **Type Safe**: Leverages Go generics and interfaces for clean abstractions
- **Domain Driven**: Clear separation between parsing, AST modeling, and LLM integration

### Core Components

```
refacto/
├── internal/domain/
│   ├── parser/         # Language-specific parsers (Go, Python, TS, etc.)
│   └── ast/            # Unified AST model and node types
├── internal/controller/ # LLM provider abstraction
├── pkg/parser/         # Parser registration and extension interface
└── cmd/refacto/        # CLI and reference implementation
```

---

## 📦 Current Features

### Go Parser Capabilities

| Feature | Status |
|---------|--------|
| Functions & Methods | ✅ Complete |
| Structs & Interfaces | ✅ Complete |
| Type Aliases | ✅ Complete |
| For Loops (all variants) | ✅ Complete |
| Comparison Expressions | ✅ Complete |
| Variadic Parameters | ✅ Complete |
| Expression Parsing | ✅ Complete |
| Variable Declarations | ✅ Complete |
| Function Calls | ✅ Complete |

### Framework Features

| Feature | Status |
|---------|--------|
| Extensible Parser Registry | ✅ Complete |
| Type-Safe AST Nodes | ✅ Complete |
| JSON Serialization | ✅ Complete |
| Error Handling | ✅ Complete |
| Logging Infrastructure | ✅ Complete |

---

## 🔧 Use Cases

### Short Term (v1.0)
- **Static Code Analysis**: Understand code structure and extract metrics
- **Refactoring Tools**: Build language-agnostic refactoring utilities
- **Code Documentation**: Auto-generate documentation from AST analysis

### Medium Term (v2.0)
- **AI-Powered Suggestions**: Leverage LLMs for intelligent refactoring advice
- **Multi-Language Migration**: Facilitate code migration between languages
- **Code Quality Tools**: Detect patterns and suggest improvements

### Long Term (v3.0+)
- **Universal Developer Assistant**: AI coding assistant that understands any language
- **Automated Codebase Transformation**: Large-scale refactoring and modernization
- **Cross-Language Code Generation**: Generate code in one language from specifications in another

---

## 💡 Getting Started

### Build
```bash
go build -o refacto ./cmd/refacto
```

### Parse a File
```bash
./refacto path/to/file.go
```

### Run Tests
```bash
go test -v ./...
```

---

---

## 🚀 Getting Started

### Prerequisites

- **Go 1.18+** (for generics support)
- **Git** for version control
- **Make** (optional, for build automation)

### Installation

#### From Source

```bash
# Clone the repository
git clone https://github.com/yourusername/refacto.git
cd refacto

# Install dependencies
go mod download
go mod tidy

# Build the executable
go build -o refacto ./cmd/refacto

# Verify installation
./refacto --help
```

#### Using Go Install

```bash
go install github.com/yourusername/refacto/cmd/refacto@latest
```

### Quick Start

#### Parse a Go File

```bash
# Parse a single file and output AST as JSON
./refacto path/to/your/file.go

# Parse with custom output
./refacto ./cmd/refacto/test_data/complex_test.go > ast.json
```

#### Run Tests

```bash
# Run all tests
go test ./...

# Run with verbose output
go test -v ./...

# Run with coverage
go test -cover ./...

# Generate coverage report
go test -coverprofile=coverage.out ./... && go tool cover -html=coverage.out

# Run specific test
go test -run TestForLoop ./internal/domain/parser/go_lang
```

#### Development Setup

```bash
# Format code
go fmt ./...

# Run linter (requires golangci-lint)
golangci-lint run ./...

# Check for common issues
go vet ./...

# Organize imports
goimports -w ./...
```

---

## 🛠️ Extension Guide

### Adding a New Language Parser

Following the plugin architecture pattern, adding a new language is straightforward:

#### Step 1: Create Parser Structure

```bash
mkdir -p internal/domain/parser/new_lang
touch internal/domain/parser/new_lang/new_lang_lexer.go
touch internal/domain/parser/new_lang/new_lang_token.go
touch internal/domain/parser/new_lang/new_lang_parser.go
```

#### Step 2: Implement Lexer

Create `new_lang_lexer.go`:

```go
package new_lang

type Lexer struct {
    input        string
    position     int
    readPosition int
    ch           byte
}

func NewLexer(input string) *Lexer {
    l := &Lexer{input: input}
    l.readChar()
    return l
}

func (l *Lexer) NextToken() Token {
    // Implement tokenization logic
    // Return Token with type and literal
}

func (l *Lexer) readChar() {
    // Read next character from input
}
```

#### Step 3: Define Tokens

Create `new_lang_token.go`:

```go
package new_lang

type TokenType string
type Token struct {
    Type    TokenType
    Literal string
}

const (
    TokenEOF       TokenType = "EOF"
    TokenIdent     TokenType = "IDENT"
    TokenKeyword   TokenType = "KEYWORD"
    // Define all token types for your language
)
```

#### Step 4: Implement Parser

Create `new_lang_parser.go`:

```go
package new_lang

import (
    "refacto/internal/domain/ast"
    "refacto/internal/domain/ast/root"
    parser_pkg "refacto/pkg/parser"
)

type NewLangParser struct {
    lexer  *Lexer
    tokens []Token
}

func NewNewLangParser() parser_pkg.LanguageParser {
    return &NewLangParser{}
}

func (p *NewLangParser) GetLanguage() parser_pkg.Language {
    return parser_pkg.LanguageNewLang // Define in Language enum
}

func (p *NewLangParser) ParseFile(filePath string) (*root.File, error) {
    // Read file
    // Tokenize
    // Parse into AST
    // Return File with declarations and specifications
}

func (p *NewLangParser) ParseProject(rootDir string) (*root.Program, error) {
    // Recursively parse all files in directory
}
```

#### Step 5: Register Parser

In your init or main:

```go
func init() {
    parser := new_lang.NewNewLangParser()
    if err := parser_pkg.RegisterParser(parser); err != nil {
        log.Fatal(err)
    }
}
```

#### Step 6: Reuse AST Node Types

Wherever possible, reuse existing AST nodes from:
- `internal/domain/ast/declaration/` (FunctionDeclaration, TypeDeclaration, etc.)
- `internal/domain/ast/statement/` (ReturnStatement, IfStatement, etc.)
- `internal/domain/ast/expression/` (Identifier, Literal, CallExpression, etc.)

Only create new node types when the language has fundamentally different constructs.

---

### Adding LLM Provider Integration

The controller abstraction makes it easy to add new LLM providers:

#### Step 1: Create Provider Package

```bash
mkdir -p internal/controller/impl/provider_name
touch internal/controller/impl/provider_name/provider_controller.go
touch internal/controller/impl/provider_name/provider_client.go
```

#### Step 2: Define Model Types

In `internal/controller/types.go`:

```go
type LLMType string

const (
    LLMTypeGoogle      LLMType = "google"
    LLMTypeOpenAI      LLMType = "openai"
    LLMTypeAnthropic   LLMType = "anthropic"
    LLMTypeNewProvider LLMType = "new_provider"
)
```

#### Step 3: Implement Controller

Create `provider_controller.go`:

```go
package provider_name

import (
    "refacto/internal/controller"
)

type NewProviderModel struct {
    // Provider-specific configuration
    APIKey string
    Model  string
}

type NewProviderController struct {
    controller.BaseController[NewProviderModel]
    client *NewProviderClient
}

func NewNewProviderController(apiKey string) *NewProviderController {
    return &NewProviderController{
        client: NewNewProviderClient(apiKey),
    }
}

func (c *NewProviderController) AnalyzeCode(code string) (string, error) {
    // Use client to call provider API
    // Process response
    // Return analysis result
}

func (c *NewProviderController) RefactorSuggestions(code string) ([]string, error) {
    // Generate refactoring suggestions
}
```

#### Step 4: Update Factory

In `internal/controller/controller.go`:

```go
func NewController(llmType LLMType, config Config) (Controller, error) {
    switch llmType {
    case LLMTypeGoogle:
        return google.NewGoogleController(config.APIKey), nil
    case LLMTypeNewProvider:
        return provider_name.NewNewProviderController(config.APIKey), nil
    default:
        return nil, fmt.Errorf("unknown LLM type: %s", llmType)
    }
}
```

#### Step 5: Add Configuration

Add to `configs/application.properties`:

```properties
llm.provider=new_provider
llm.new_provider.api_key=${NEW_PROVIDER_API_KEY}
llm.new_provider.model=default-model
```

---

### Extending the AST Model

#### Adding a New Declaration Type

1. **Define interface** in `internal/domain/ast/declaration/ast_declaration.go`
2. **Create struct** implementing Declaration interface
3. **Implement methods** in `*_impl.go` files
4. **Add JSON marshaling** support if needed

Example:

```go
type InterfaceDeclaration struct {
    BaseDeclaration
    Methods []*FunctionDeclaration `json:"Methods,omitempty"`
    // Other fields
}

func (i *InterfaceDeclaration) GetDeclarationType() string {
    return "Interface"
}

func (i *InterfaceDeclaration) GetContents() string {
    return i.Contents
}
```

#### Adding a New Expression Type

Follow the pattern in `internal/domain/ast/expression/`:

```go
type MyExpression struct {
    // Fields
}

func (e *MyExpression) GetExpressionType() ExpressionType {
    return ExpressionTypeCustom
}

func (e *MyExpression) GetContents() string {
    return "expression representation"
}
```

---

## 📚 API Reference

### Parser Registry

```go
// Register a new language parser
func RegisterParser(parser LanguageParser) error

// Get parser for specific language
func GetParser(language Language) (LanguageParser, error)

// List all registered languages
func ListRegisteredLanguages() []string
```

### Language Parser Interface

```go
type LanguageParser interface {
    GetLanguage() Language
    ParseFile(filePath string) (*root.File, error)
    ParseProject(rootDir string) (*root.Program, error)
}
```

### Controller Interface

```go
type Controller interface {
    SetModel(model Model) error
    AnalyzeCode(code string) (string, error)
    RefactorSuggestions(code string) ([]string, error)
}
```

### AST Node Interfaces

```go
// Root node
type Root interface {
    GetName() string
    GetChildren() []Child
}

// File node
type Child interface {
    GetId() string
    GetName() string
    GetPath() string
    GetSpecifications() []*Specification
    GetDeclarations() []*Declaration
    GetRaw() string
}

// Declaration node
type Declaration interface {
    GetId() string
    GetDeclarationType() string
    GetContents() string
}

// Statement node
type Statement interface {
    GetStatementType() string
    GetContents() string
}

// Expression node
type Expression interface {
    GetExpressionType() ExpressionType
    GetContents() string
}
```

---

## 🏗️ Architecture Deep Dive

### Domain-Driven Design

refacto follows DDD principles with clear separation:

**Domain Layer** (`internal/domain/`)
- AST model definitions
- Language-specific parsers
- Core business logic

**Application Layer** (`internal/controller/`)
- LLM provider abstraction
- Use case orchestration
- Cross-cutting concerns

**Presentation Layer** (`cmd/refacto/`)
- CLI interface
- JSON serialization
- User-facing logic

**Infrastructure Layer** (`pkg/`)
- Parser registry
- Reusable utilities
- Extension points

### Plugin Architecture

```
┌─────────────────────────────────────────┐
│         Main Application                │
└──────────────┬──────────────────────────┘
               │
    ┌──────────┴──────────────────┐
    │                             │
┌───▼────────────────┐   ┌────────▼──────────────┐
│  Parser Registry   │   │  LLM Controller       │
│                    │   │  Factory              │
└───┬────────────────┘   └────────┬──────────────┘
    │                             │
    ├─ Go Parser                  ├─ Google Controller
    ├─ Python Parser (future)     ├─ OpenAI Controller
    └─ TS Parser (future)         └─ Custom Controllers
```

### Concurrency & Thread Safety

- Parser registry uses `sync.RWMutex` for thread-safe registration
- AST nodes are immutable after creation
- Safe for concurrent reads from multiple goroutines

---

## 🧪 Testing Strategy

### Unit Tests

```bash
# Test individual parsers
go test ./internal/domain/parser/go_lang/... -v

# Test AST nodes
go test ./internal/domain/ast/... -v

# Test controller implementations
go test ./internal/controller/impl/... -v
```

### Integration Tests

```bash
# Parse test files and verify AST structure
go test ./cmd/refacto/... -v
```

### Test Data

Test files located in `cmd/refacto/test_data/`:
- `simple1.go` - Basic function and variable declarations
- `complex_test.go` - Complex type definitions and loops
- `test_for_loop.go` - For loop variants and variadic functions

Add your own test files here when extending functionality.

### Writing Tests for New Parsers

```go
func TestNewLangParser_ParseFile(t *testing.T) {
    parser := NewNewLangParser()
    
    file, err := parser.ParseFile("test_data/example.lang")
    if err != nil {
        t.Fatalf("ParseFile failed: %v", err)
    }
    
    // Verify structure
    if len(file.Declarations) == 0 {
        t.Error("expected declarations, got none")
    }
}
```

---

## 🔐 Security Considerations

### Input Validation

- All file paths should be validated before parsing
- Implement maximum file size limits for parser input
- Consider sandboxing for untrusted code analysis

### API Key Management

```go
// Store API keys in environment variables, NOT in code
llm, err := controller.NewController(
    controller.LLMTypeGoogle,
    controller.Config{
        APIKey: os.Getenv("GOOGLE_API_KEY"),
    },
)
```

### Dependency Scanning

```bash
# Check for known vulnerabilities
go list -json -m all | nancy sleuth

# Or use standard Go tool
go list -m -u all
```

---

## 📝 Configuration

### Application Properties

Edit `configs/application.properties`:

```properties
# Logging
logging.level=INFO
logging.output=stdout

# Parser defaults
parser.max_file_size=10485760
parser.timeout=30s

# LLM Configuration
llm.provider=google
llm.google.model=gemini-pro
llm.google.max_tokens=2000
```

### Environment Variables

```bash
export GOOGLE_API_KEY="your-api-key"
export OPENAI_API_KEY="your-api-key"
export REFACTO_LOG_LEVEL="DEBUG"
refacto ./path/to/file.go
```

---

## 🐛 Troubleshooting

### Parser Returns Empty Declarations

- Verify file syntax is valid in the target language
- Check file encoding (UTF-8 recommended)
- Enable debug logging: `REFACTO_LOG_LEVEL=DEBUG`

### JSON Serialization Issues

- Ensure all struct fields have proper `json:"..."` tags
- Use `json:"-"` to exclude sensitive fields (like Contents)
- Check for circular references in AST structures

### LLM Integration Fails

- Verify API keys are correctly set
- Check network connectivity to LLM provider
- Review rate limiting and quota usage
- Enable verbose logging for API responses

---

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make your changes
4. Add tests for new functionality
5. Run `go fmt ./...` and `golangci-lint run ./...`
6. Submit a pull request with description

### Contribution Areas

- **New Language Parsers**: Python, TypeScript, Rust, Java
- **LLM Providers**: Anthropic, Perplexity, X (xAI)
- **Analysis Tools**: Symbol resolution, type inference, data flow
- **UI/Frontend**: Web dashboard for AST visualization
- **Documentation**: Examples, guides, API documentation

---

## 📄 License

This project is licensed under the MIT License - see LICENSE file for details.

---

## 📖 Learn More

**Detailed Documentation:**
- [WARP.md](WARP.md) - Architecture, structure, and development guidelines
- [IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md) - Recent parser enhancements and changes

**External Resources:**
- [Go Generics Guide](https://go.dev/doc/tutorial/generics)
- [Recursive Descent Parsing](https://en.wikipedia.org/wiki/Recursive_descent_parser)
- [Abstract Syntax Trees](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
- [Domain-Driven Design](https://martinfowler.com/bliki/DomainDrivenDesign.html)

**Examples & Samples:**
- Go parser test data: `cmd/refacto/test_data/`
- Full AST output: Run `./refacto cmd/refacto/test_data/complex_test.go`

---

## 📞 Support & Community

- **Issues**: Report bugs and request features on GitHub Issues
- **Discussions**: Join discussions on GitHub Discussions
- **Email**: For security issues, email security@refacto.dev

---

## 🎯 Project Status

**Current Version**: 0.1.0 (Alpha)

**Stability**: Experimental - API may change

**Production Ready**: Not yet - use for development/evaluation only

**Planned 1.0 Release**: Q2 2026 (Tentative)
