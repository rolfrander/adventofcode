package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

const empty byte = 0
const facingEast byte = 1
const facingSouth byte = 2

func ParseLine(in string) []uint8 {
	l := len(in)
	r := make([]uint8, l)
	for pos, char := range in {
		if char == '.' {
			r[pos] = empty
		} else if char == '>' {
			r[pos] = facingEast
		} else if char == 'v' {
			r[pos] = facingSouth
		}
	}
	return r
}

func ReadData(input io.Reader) [][]byte {
	image := make([][]uint8, 0, 256)
	scanner := bufio.NewScanner(input)

	for scanner.Scan() {
		line := scanner.Text()
		image = append(image, ParseLine(line))
	}
	return image
}

func MoveEast(input, output [][]byte) ([][]byte, [][]byte, int) {
	cnt := 0
	h := len(input)
	w := len(input[0])

	for row := 0; row < h; row++ {
		for col := 0; col < w; col++ {
			if input[row][col] == facingEast && input[row][(col+1)%w] == empty {
				output[row][col] = empty
				output[row][(col+1)%w] = facingEast
				cnt++
				col++
			} else {
				output[row][col] = input[row][col]
			}

		}
	}

	return output, input, cnt
}

func MoveSouth(input, output [][]byte) ([][]byte, [][]byte, int) {
	cnt := 0
	h := len(input)
	w := len(input[0])

	for col := 0; col < w; col++ {
		for row := 0; row < h; row++ {
			if input[row][col] == facingSouth && input[(row+1)%h][col] == empty {
				output[row][col] = empty
				output[(row+1)%h][col] = facingSouth
				cnt++
				row++
			} else {
				output[row][col] = input[row][col]
			}

		}
	}

	return output, input, cnt
}

func Step(input, output [][]byte) ([][]byte, [][]byte, int) {
	var cnt1, cnt2 int
	input, output, cnt1 = MoveEast(input, output)
	input, output, cnt2 = MoveSouth(input, output)
	return input, output, cnt1 + cnt2
}

func PrintBoard(input [][]byte) {
	for _, row := range input {
		for _, element := range row {
			var c string
			switch element {
			case empty:
				c = "."
			case facingEast:
				c = ">"
			case facingSouth:
				c = "v"
			default:
				c = "?"
			}
			fmt.Print(c)
		}
		fmt.Println()
	}
}

func intArg(argno int, name string) int {
	val, err := strconv.ParseInt(os.Args[argno], 10, 32)
	if err != nil {
		fmt.Printf("Error parsing %s %s as int\n", name, os.Args[argno])
		os.Exit(1)
	}
	return int(val)
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: switch file iterations")
	} else {
		filename := os.Args[1]
		//iterations := intArg(2, "iterations")
		input, err := os.Open(filename)
		if err != nil {
			fmt.Printf("Error opening %s\n", filename)
			os.Exit(1)
		}
		defer input.Close()

		data := ReadData(input)
		tmp := make([][]byte, 0, len(data))
		for _, row := range data {
			tmp = append(tmp, make([]byte, len(row)))
		}

		//PrintBoard(data)
		cnt := 1
		iterations := 0
		for i := 0; cnt != 0; i++ {
			data, tmp, cnt = Step(data, tmp)
			iterations++
		}
		fmt.Printf("after %d iterations, %d moves\n", iterations, cnt)
		//PrintBoard(data)
	}
}
