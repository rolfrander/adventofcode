package main

import (
	"os"
)

func main() {
	day := os.Args[1]
	switch day {
	case "05":
		day05(os.Args[2:])
	}
}
