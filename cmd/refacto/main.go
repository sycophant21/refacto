package main

import (
	"errors"
	"fmt"
	"log"
	"refacto/internal/util/error"
	logg "refacto/internal/util/log"
	program "refacto/internal/util/program"
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
}

func main() {
	defer logg.Logger.Close()
	str := error.Must("abc", errors.New("invalid data type"))
	fmt.Println(str)
}
