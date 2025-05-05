package log

import (
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	programUtils "refacto/internal/util/program"
	"runtime"
	"strconv"
	"sync"
	"time"
)

type LoggingLevel string

const (
	ERROR LoggingLevel = "ERROR"
	WARN  LoggingLevel = "WARN"
	INFO  LoggingLevel = "INFO"
	DEBUG LoggingLevel = "DEBUG"
)

var loggingLevels = map[LoggingLevel]uint8{
	ERROR: 0,
	WARN:  1,
	INFO:  2,
	DEBUG: 3,
}

var Logger *stdLogger = &stdLogger{}

type entry struct {
	Level   LoggingLevel
	Message string
	File    string
}

type stdLogger struct {
	logger       *log.Logger
	loggingLevel LoggingLevel
	pid          int

	entries chan entry
	wg      sync.WaitGroup
}

func Initialise(bufferSize int) error {
	lvl, err := parseLogLevel()
	if err != nil {
		return err
	}
	logDir, err := resolveLogDir("Refacto")
	if err != nil {
		return err
	}
	writer, err := openLogFile(logDir, "app.log")
	if err != nil {
		return err
	}
	newStdLogger([]io.Writer{writer, os.Stdout}, lvl, os.Getpid(), bufferSize)
	return nil
}

func parseLogLevel() (LoggingLevel, error) {
	val, err := programUtils.GetProperty(programUtils.LoggingLevel)
	if err != nil {
		return INFO, err
	}
	lvl := LoggingLevel(val.(string))
	if _, ok := loggingLevels[lvl]; !ok {
		log.Printf("invalid logging level %q, defaulting to INFO", lvl)
		lvl = INFO
	}
	return lvl, nil
}

func resolveLogDir(appName string) (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	path := filepath.Join(home, "Library", "Logs", appName)
	if err := os.MkdirAll(path, 0o700); err != nil {
		return "", err
	}
	return path, nil
}

func openLogFile(dir, name string) (io.Writer, error) {
	path := filepath.Join(dir, name)
	f, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o666)
	if err != nil {
		return nil, err
	}
	return f, nil
}

func newStdLogger(writers []io.Writer, level LoggingLevel, pid, bufferSize int) {
	multi := io.MultiWriter(writers...)
	base := log.New(multi, "", 0)
	Logger.logger = base
	Logger.loggingLevel = level
	Logger.pid = pid
	Logger.entries = make(chan entry, bufferSize)
	Logger.wg.Add(1)
	go Logger.runWorker()
	//Logger.Close()
}

func (l *stdLogger) runWorker() {
	defer l.wg.Done()
	for e := range l.entries {
		l.write(e)
	}
}

func (l *stdLogger) Error(msg string, panicApp bool) {
	_, file, _, ok := runtime.Caller(1)
	if !ok {
		log.Fatal("unable to fetch caller information")
	}
	l.enqueue(ERROR, msg, file)
	if panicApp {
		l.Close()
		os.Exit(1)
	}
}

func (l *stdLogger) Warn(msg string) {
	_, file, _, ok := runtime.Caller(1)
	if !ok {
		log.Fatal("unable to fetch caller information")
	}
	l.enqueue(WARN, msg, file)
}

func (l *stdLogger) Info(msg string) {
	_, file, _, ok := runtime.Caller(1)
	if !ok {
		log.Fatal("unable to fetch caller information")
	}
	l.enqueue(INFO, msg, file)
}

func (l *stdLogger) Debug(msg string) {
	_, file, _, ok := runtime.Caller(1)
	if !ok {
		log.Fatal("unable to fetch caller information")
	}
	l.enqueue(DEBUG, msg, file)
}

func (l *stdLogger) enqueue(level LoggingLevel, msg, file string) {
	l.entries <- entry{Level: level, Message: msg, File: file}
}

func (l *stdLogger) write(e entry) {
	if loggingLevels[e.Level] > loggingLevels[l.loggingLevel] {
		return
	}
	file := e.File
	if len(file) > 40 {
		file = file[len(file)-40:]
	}
	prefix := time.Now().Format("2006-01-02T15:04:05-07:00") + " " + string(e.Level) + " " + strconv.Itoa(l.pid) + " "
	l.logger.SetPrefix(prefix)
	l.logger.Println("--- [" + fmt.Sprintf("%15s", "main") + "] " + fmt.Sprintf("%-40s", file) + " : " + e.Message)
}

func (l *stdLogger) Close() {
	close(l.entries)
	l.wg.Wait()
}
