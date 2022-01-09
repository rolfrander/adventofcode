package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

func ParseLine(in string, padding int) []uint8 {
	l := len(in)
	r := make([]uint8, l+2*padding)
	for pos, char := range in {
		if char == '#' {
			r[pos+padding] = 1
		}
	}
	return r
}

func ReadData(input io.Reader, padding int) ([]byte, [][]byte) {
	var filter []uint8
	image := make([][]uint8, 0, 256)
	scanner := bufio.NewScanner(input)
	w := 0

	for scanner.Scan() {
		line := scanner.Text()
		if filter == nil {
			filter = ParseLine(line, 0)
		} else {
			l := len(line)
			if l > 0 {
				if w == 0 {
					w = l + 2*padding
					for i := 0; i < padding; i++ {
						image = append(image, make([]uint8, w))
					}
				}
				image = append(image, ParseLine(line, padding))
			}
		}
	}
	for i := 0; i < padding; i++ {
		image = append(image, make([]uint8, w))
	}
	return filter, image
}

func CountPixels(input [][]byte, padding int) int {
	h := len(input)
	w := len(input[0])
	cnt := 0
	for _, row := range input[padding : h-padding] {
		for _, val := range row[padding : w-padding] {
			cnt += int(val)
		}
	}
	return cnt
}

func PrintImage(input [][]byte, padding int) {
	h := len(input)
	w := len(input[0])
	for _, row := range input[padding : h-padding] {
		for _, val := range row[padding : w-padding] {
			if val == 1 {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func PrintEnhancement(enhancement []byte) {
	for i, val := range enhancement {
		if val == 1 {
			fmt.Print("#")
		} else {
			fmt.Print(".")
		}
		if (i % 74) == 73 {
			fmt.Println()
		}
	}
	fmt.Println()
	fmt.Println()
}

func Convolute(enhancement []byte, input, output [][]byte) ([][]byte, [][]byte) {
	delta := []int{-1, 0, 1}
	h := len(input)
	w := len(input[0])
	for y, row := range input {
		if y > 0 && y < (h-1) {
			for x, _ := range row {
				if x > 0 && x < (w-1) {
					conv := 0
					for _, dy := range delta {
						for _, dx := range delta {
							conv = (conv << 1) | int(input[y+dy][x+dx])
						}
					}
					output[y][x] = enhancement[conv]
				}
			}
		}
	}
	return output, input
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

	if len(os.Args) != 5 {
		fmt.Println("Usage: convolute file iterations image-padding count-padding")
	} else {
		filename := os.Args[1]
		iterations := intArg(2, "iterations")
		imgPadding := intArg(3, "image read padding")
		cntPadding := intArg(4, "pixel count padding")

		input, err := os.Open(filename)
		if err != nil {
			fmt.Printf("Error opening %s\n", filename)
			os.Exit(1)
		}
		defer input.Close()

		enhancement, image := ReadData(input, imgPadding)
		fmt.Printf("Pixelcount before enhancement: %d\n", CountPixels(image, cntPadding))
		tmpImage := make([][]byte, 0, len(image))

		for _, row := range image {
			tmpImage = append(tmpImage, make([]byte, len(row)))
		}

		for i := 0; i < iterations; i++ {
			image, tmpImage = Convolute(enhancement, image, tmpImage)
		}
		PrintImage(image, cntPadding)
		fmt.Printf("Pixelcount after enhancement: %d\n", CountPixels(image, cntPadding))
	}
}
