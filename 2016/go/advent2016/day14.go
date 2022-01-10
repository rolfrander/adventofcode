package main

import (
	"crypto/md5"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"
)

func nInARow(n int, hash []byte) int {
	var nibble [16]int
	for i, _ := range nibble {
		nibble[i] = -1
	}

	for nc := 0; nc < 32; nc++ {
		if (nc % 2) == 0 {
			nibble[nc%n] = (int(hash[nc>>1]) >> 4) & 0xf
		} else {
			nibble[nc%n] = int(hash[nc>>1]) & 0xf
		}
		for j := 1; nibble[0] == nibble[j]; j++ {
			if j == n-1 {
				return nibble[0]
			}
		}
	}
	return -1
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func calcKey(input []byte, stretch int) []byte {
	md5sum := md5.Sum(input)
	for j := 0; j < stretch; j++ {
		md5string := fmt.Sprintf("%x", md5sum)
		md5sum = md5.Sum([]byte(md5string))
	}
	return md5sum[:]
}

func hashProducer(salt string, start, end, interval, stretch int, c chan []byte) {
	data := make([]byte, 64)
	counter := make([]byte, 32)

	copy(data, []byte(salt))
	dataLength := len(salt)

	for i := start; i < end; i += interval {
		pos := formatNumber(i, counter)
		copy(data[dataLength:], counter[pos:])

		newlen := dataLength + len(counter) - pos
		md5sum := calcKey(data[0:newlen], stretch)
		c <- md5sum
	}
	close(c)
}

const maxiter = 5000000
const threadcount = 60

func day14(args []string) {
	if len(args) != 2 {
		fmt.Printf("Expected argument: salt stretching\n")
		os.Exit(1)
	}

	salt := args[0]
	stretch64, err := strconv.ParseInt(args[1], 10, 32)
	if err != nil {
		fmt.Printf("Error parsing %s as int\n", args[1])
		os.Exit(1)
	}
	stretch := int(stretch64)
	keycnt := 0
	var c [threadcount]chan []byte

	fmt.Printf("starting %v threads, GOMAXPROCS=%v\n", threadcount, runtime.GOMAXPROCS(0))
	for i, _ := range c {
		c[i] = make(chan []byte, 5)
		go hashProducer(salt, i, maxiter, threadcount, stretch, c[i])
	}

	var lastSeen3 [16][]int
	for i, _ := range lastSeen3 {
		lastSeen3[i] = make([]int, 0, 64)
	}

	maxKeyHash := 0
	i := 0
	start := time.Now()
	for ; (i < maxiter) && (keycnt < 64); i++ {
		md5sum := <-c[i%threadcount]
		triplet := nInARow(3, md5sum[:])
		if triplet > -1 {
			lastSeen3[triplet] = append(lastSeen3[triplet], i)
			for (i - lastSeen3[triplet][0]) > 1000 {
				lastSeen3[triplet] = lastSeen3[triplet][1:]
			}
			//fmt.Printf("triplet %x at %d %x\n", triplet, i, md5sum)
			quintuplet := nInARow(5, md5sum[:])
			if quintuplet > -1 {
				for len(lastSeen3[quintuplet]) > 0 && (i-lastSeen3[quintuplet][0]) > 0 && keycnt < 64 {
					//fmt.Printf("i=%d, quintuplet=%x, triplets at %v, keycnt=%d, data=%s\n",
					//	i, quintuplet, lastSeen3[quintuplet], keycnt, string(data[0:newlen]))
					maxKeyHash = max(maxKeyHash, lastSeen3[quintuplet][0])
					lastSeen3[quintuplet] = lastSeen3[quintuplet][1:]
					keycnt++
				}
			}
		}
	}
	/*
		for i, l := range lastSeen3 {
			fmt.Printf("lastSeen %x: %d\n", i, l)
		}
	*/
	elapsed := time.Now().Sub(start)
	fmt.Printf("task-1: %d, i=%d, keycnt=%d, elapsed time=%v\n", maxKeyHash, i, keycnt, elapsed)
}
