// This file is provided as example code to illustrate the following article:
// https://blog.tigris.fr/2019/07/28/writing-an-emulator-memory-management/
// Feel free to do whatever with it!
package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
)

// CPU structure embedding memory to prepare for memory management.
type CPU struct {
	A, F uint8
	B, C uint8
	D, E uint8
	H, L uint8
	SP   uint16
	PC   uint16

	memory []byte
}

// String returns a human-readable representation of the CPU's current state.
func (c *CPU) String() string {
	var b bytes.Buffer
	fmt.Fprintf(&b, "A: %#02x - F: %#02x\n", c.A, c.F)
	fmt.Fprintf(&b, "B: %#02x - C: %#02x\n", c.B, c.C)
	fmt.Fprintf(&b, "D: %#02x - E: %#02x\n", c.D, c.E)
	fmt.Fprintf(&b, "H: %#02x - L: %#02x\n", c.H, c.L)
	fmt.Fprintf(&b, "         SP: %#04x\n", c.SP)
	fmt.Fprintf(&b, "         PC: %#04x\n", c.PC)
	return b.String()
}

// HL returns the value of registers H and L as if reading from a single
// 16-bit register. Doing this for all pairs of registers is left as an
// exercise to the reader.
func (c *CPU) HL() uint16 {
	return uint16(c.H)<<8 | uint16(c.L)
}

// SetHL stores a 16-bit value into H and L as if writing to a single 16-bit
// register. Doing this for all pairs of registers is left as an exercise to
// the reader.
func (c *CPU) SetHL(value uint16) {
	c.L = uint8(value & 0x00ff)
	c.H = uint8((value & 0xff00) >> 8)
}

//
// CPU Instructions
//

// Supported instructions, in a mapping because we're only implementing a few
// of them. In the final version, we'll use a proper 256-entry array.
// Each supported opcode is mapped to a function taking a pointer to the CPU
// and responsible for updating that CPU accordingly.
var instructions = map[byte]func(*CPU){
	0x21: ldhld16,
	0x31: ldspd16,
	0x32: ldhlda,
	0xaf: xora,
}

//
// Generic instructions used to group common code.
//
// r 	single (8-bit) register (A, F, B C, D, E, H or L)
// rr	double (16-bit) register (AF, BC, DE or HL -- SP and PC are handled
// 	separately because I made them uint16s and I stand by that decision)
// d8	8-bit (unsigned) parameter (1 byte) after opcode
// d16	16-bit (unsigned) parameter (2 bytes, little-endian) after opcode
// r8	8-bit (signed) parameter (1 byte) after opcode

// LD rr,d16 - Copy d16 in double register rr (pretty easy thanks to having
// separate single registers)
func ldrrd16(c *CPU, high, low *uint8) {
	*low = c.memory[c.PC]
	*high = c.memory[c.PC+1]
	c.PC += 2
}

// XOR r (even though the boot ROM only contains XOR A which means A=0)
func xorr(c *CPU, reg uint8) {
	c.A ^= reg
}

// LD (addr),r
func ldaddrr(c *CPU, addr uint16, reg uint8) {
	c.memory[addr] = reg
}

//
// Individual instructions in the order they appear in the boot ROM.
//

// LD SP,d16 (not calling LD rr,d16 because we made SP a single uint16)
func ldspd16(c *CPU) {
	c.SP = uint16(c.memory[c.PC]) | uint16(c.memory[c.PC+1])<<8
	c.PC += 2
}

// XOR A (we could just set A to zero but we'll need XOR r later anyway)
func xora(c *CPU) {
	xorr(c, c.A)
}

// LD HL,d16
func ldhld16(c *CPU) {
	ldrrd16(c, &c.H, &c.L)
}

// LD (HL-),A
func ldhlda(c *CPU) {
	// Using extension methods to treat HL as a single 16-bit register.
	ldaddrr(c, c.HL(), c.A)
	c.SetHL(c.HL() - 1)
}

//
// Main loop running code in memory from address 0 on our CPU.
// When an unknown opcode is encountered, quit and show CPU state.
//
func main() {
	bootROM, err := ioutil.ReadFile("./dmg-rom.bin")
	if err != nil {
		panic(err)
	}

	cpu := CPU{memory: bootROM}
	for {
		// The next opcode to execute is the byte at the exact address
		// pointed to by PC.
		opcode := cpu.memory[cpu.PC]
		cpu.PC++

		// Try finding a corresponding instruction in the instructions
		// mapping. Keys that don't have a value will return 'nil'.
		if instruction := instructions[opcode]; instruction != nil {
			instruction(&cpu)
		} else {
			fmt.Printf("Unknown opcode: %#2x\n", opcode)
			fmt.Println(&cpu)
			return
		}
	}
}
