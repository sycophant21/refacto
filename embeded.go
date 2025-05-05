package embeded

import (
	"embed"
)

//go:embed configs
var PropertiesFile embed.FS
