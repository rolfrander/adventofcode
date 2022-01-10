package main

import (
	"crypto/md5"
	"fmt"
	"os"
	"strconv"
)

func formatNumber(n int, target []byte) int {
	pos := len(target) - 1
	if n == 0 {
		target[pos] = '0'
		return pos
	}
	for ; n > 0; pos-- {
		digit := n % 10
		n = n / 10
		target[pos] = byte(digit + '0')
	}
	return pos + 1
}

func byte2rune(b byte) rune {
	if b < 0x0a {
		return rune(b) + '0'
	} else {
		return rune(b) - 0x0a + 'a'
	}
}

func day05(args []string) {
	if len(args) != 2 {
		fmt.Errorf("Expected argument: door-id password-length")
	}

	data := make([]byte, 64)
	counter := make([]byte, 32)
	pwdLength, err := strconv.ParseInt(args[1], 10, 32)
	if err != nil {
		fmt.Printf("Error parsing %s as int\n", args[1])
		os.Exit(1)
	}

	pwd := make([]rune, pwdLength)
	pwd2 := make([]rune, pwdLength)
	curPwdLength := int64(0)
	curPwdLength2 := int64(0)

	copy(data, []byte(args[0]))
	dataLength := len(args[0])

	for i := 0; (i < 100000000) && ((curPwdLength < pwdLength) || (curPwdLength2 < pwdLength)); i++ {
		pos := formatNumber(i, counter)
		copy(data[dataLength:], counter[pos:])

		newlen := dataLength + len(counter) - pos
		md5sum := md5.Sum(data[0:newlen])

		if (md5sum[0] == 0) && (md5sum[1] == 0) && (md5sum[2] <= 0x0f) {
			if curPwdLength < pwdLength {
				pwd[curPwdLength] = byte2rune(md5sum[2])
				curPwdLength++
			}
			position := int64(md5sum[2])
			if (position < pwdLength) && (pwd2[position] == 0) {
				curPwdLength2++
				pwd2[position] = byte2rune(md5sum[3] >> 4)
			}
			fmt.Printf("%10d %x\n", i, md5sum)
		}
	}

	fmt.Printf("task 1: %s\n", string(pwd))
	fmt.Printf("task 2: %s\n", string(pwd2))
}
