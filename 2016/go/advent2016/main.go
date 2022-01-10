package main

import (
	"fmt"
	"os"
)

func main() {
	day := os.Args[1]
	switch day {
	case "05":
		day05(os.Args[2:])
	case "14":
		day14(os.Args[2:])
	default:
		fmt.Errorf("unknown puzzle: %s\n", day)
	}
}
