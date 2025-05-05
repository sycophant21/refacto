package error

import "refacto/internal/util/log"

var logger = log.Logger

func Should[T any](_ T, err error) {
	if err != nil {
		logger.Error(err.Error(), true)
	}
}

func Must[T any](t T, err error) T {
	if err != nil {
		logger.Error(err.Error(), false)
	}
	return t
}
