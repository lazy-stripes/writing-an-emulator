// This file is provided as example code to illustrate the following article:
// https://blog.tigris.fr/2019/07/09/writing-an-emulator-the-first-steps/
// Feel free to do whatever with it!
package main

import (
	"fmt"
	"io/ioutil"
)

type CPU struct {
	A, F uint8
	B, C uint8
	D, E uint8
	H, L uint8
	SP   uint16
	PC   uint16
}

func main() {
	memory, err := ioutil.ReadFile("./dmg-rom.bin")
	if err != nil {
		panic(err)
	}

	cpu := CPU{}
	for {
		opcode := memory[cpu.PC]
		cpu.PC++

		switch opcode {
		// ... other opcodes

		case 0x31: // LD SP,d16

			// Here, Go requires some type conversion to fit two
			// uint8 into one uint16 and also to avoid grossly
			// overflowing an uint8 by shifting it left 8 bits,
			// which would just make it zero and yield 0x00fe
			// instead of 0xfffe. That was a fun crash to debug!
			cpu.SP = uint16(memory[cpu.PC]) | uint16(memory[cpu.PC+1])<<8
			cpu.PC += 2

		// ... other opcodes

		default:
			fmt.Printf("Unknown opcode: %#2x\n", opcode)
			fmt.Printf("cpu.PC=0x%04x\n", cpu.PC)
			fmt.Printf("cpu.SP=0x%04x\n", cpu.SP)
			return
		}
	}
}
