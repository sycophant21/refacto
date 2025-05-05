package program

import (
	"bufio"
	"bytes"
	_ "embed"
	"errors"
	"fmt"
	"refacto"
	"strings"
)

type PropertiesConstant struct {
	key   string
	value any
}

var propertiesConstants = make(map[string]PropertiesConstant)

func newProperty(key string, value any) (PropertiesConstant, error) {
	return PropertiesConstant{key, value}, nil
}

func addProperty(key string, value any) error {
	if _, exists := propertiesConstants[key]; exists {
		return errors.New(fmt.Sprint("Key ", key, " already exists."))
	}
	val, err := newProperty(key, value)
	if err != nil {
		return err
	}
	propertiesConstants[key] = val
	return nil
}

func GetProperty(key string) (any, error) {
	value, exists := propertiesConstants[key]
	if !exists {
		return nil, errors.New(fmt.Sprint("Key ", key, " doesn't exist."))
	}
	return value.value, nil
}

const (
	DatasourceEngineName   = "datasource.engine"
	DatasourceUsername     = "datasource.username"
	DatasourcePassword     = "datasource.password"
	DatasourceUrl          = "datasource.url"
	DatasourceDatabaseName = "datasource.database.name"
	LoggingLevel           = "logging.level.root"
)

func LoadProperties() error {
	fileBytes, err := embeded.PropertiesFile.ReadFile("configs/application.properties")
	if err != nil {
		return err
	}
	reader := bytes.NewReader(fileBytes)
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		parts := strings.SplitN(line, "=", 2)
		if len(parts) != 2 {
			return fmt.Errorf("invalid line: %s", line)
		}
		key := strings.TrimSpace(parts[0])
		value := strings.TrimSpace(parts[1])
		err := addProperty(key, value)
		if err != nil {
			return err
		}
	}

	if err := scanner.Err(); err != nil {
		return err
	}

	return nil
}
