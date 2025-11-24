package error

import logg "refacto/internal/util/log"

var logger = logg.Logger

// Should unwraps a (T, error) pair, logging a fatal error and terminating
// the program via the shared logger if err is non-nil. Otherwise it returns t.
//
// This is intended for unrecoverable paths at the program boundary; internal
// code should generally propagate errors instead of calling Should directly.
func Should[T any](t T, err error) T {
	if err != nil {
		logger.Error(err.Error(), true)
	}
	return t
}

// Must is a convenience for fatal-error handling when only an error is
// returned. If err is non-nil, it logs the error as fatal and terminates
// the program via the shared logger.
func Must(err error) {
	if err != nil {
		logger.Error(err.Error(), true)
	}
}
