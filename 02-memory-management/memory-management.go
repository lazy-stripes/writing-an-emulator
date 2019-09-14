// This file is provided as example code to illustrate the following article:
// https://blog.tigris.fr/2019/07/28/writing-an-emulator-memory-management/
// It is supposed to show that the Game Boy's ROM code loops at some point.
// Feel free to do whatever with it!
package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
)

//
// Memory-related code. You'd probably want to put that in a 'memory' package.
//

// Addressable interface provides functions to read/write bytes in a given
// 16-bit address space.
type Addressable interface {
	// Contains returns true if the Addressable can handle the address.
	Contains(addr uint16) bool
	// Read returns the value stored at the given address.
	Read(addr uint16) uint8
	// Write stores the given value at the given address (if writable).
	Write(addr uint16, value uint8)
}

// MMU manages an arbitrary number of ordered address spaces. It also satisfies
// the Addressable interface.
type MMU struct {
	Spaces []Addressable
}

// Returns the first address space that can handle the requested address or nil.
func (m *MMU) space(addr uint16) Addressable {
	for _, space := range m.Spaces {
		if space.Contains(addr) {
			return space
		}
	}
	return nil
}

// Contains returns whether one of the address spaces known to the MMU contains
// the given address. The first address space in the internal list containing a
// given address will shadow any other that may contain it.
func (m *MMU) Contains(addr uint16) bool {
	return m.space(addr) != nil
}

// Read finds the first address space compatible with the given address and
// returns the value at that address. If no space contains the requested
// address, it returns 0xff (emulates black bar on boot).
func (m *MMU) Read(addr uint16) uint8 {
	if space := m.space(addr); space != nil {
		return space.Read(addr)
	}
	return 0xff
}

// Write finds the first address space compatible with the given address and
// attempts writing the given value to that address.
func (m *MMU) Write(addr uint16, value uint8) {
	if space := m.space(addr); space != nil {
		space.Write(addr, value)
	}
}

// RAM as an arbitrary long list of R/W bytes at addresses starting from a
// given offset.
type RAM struct {
	bytes []uint8
	start uint16
}

// NewRAM instantiates a zeroed slice of the given size to represent RAM.
func NewRAM(start, size uint16) *RAM {
	return &RAM{make([]uint8, size), start}
}

// Read returns the byte at the given address (adjusting for offset).
func (r RAM) Read(addr uint16) uint8 {
	return r.bytes[addr-r.start]
}

// Write stores the given value at the given address (adjusting for offset).
func (r RAM) Write(addr uint16, value uint8) {
	r.bytes[addr-r.start] = value
}

// Contains returns true as long as the given address fits in the slice.
func (r RAM) Contains(addr uint16) bool {
	return addr >= r.start && addr < r.start+uint16(len(r.bytes))
}

// Boot address space translating memory access to Boot ROM and the BOOT
// register at address 0xff50.
type Boot struct {
	rom      RAM   // The contents of the boot ROM.
	register uint8 // BOOT register at address 0xff50.
	disabled bool  // Writing to 0xff50 will disable boot ROM access.
}

// NewBoot returns a new address space containing the boot ROM and the BOOT
// register.
func NewBoot(filename string) *Boot {
	rom, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	return &Boot{rom: RAM{rom, 0}}
}

// Contains returns true if the given address is that of the BOOT register,
// or if the boot ROM is not disabled and contains the address.
func (b *Boot) Contains(addr uint16) bool {
	return addr == 0xff50 || (!b.disabled && b.rom.Contains(addr))
}

// Read returns the value stored at the given address in ROM or BOOT register.
// If the boot ROM was disabled, Contains() should ensure this method will
// only be called with address 0xff50.
func (b *Boot) Read(addr uint16) uint8 {
	if addr == 0xff50 {
		return b.register
	}
	return b.rom.Read(addr)
}

// Write is only supported for the BOOT register and disables the boot ROM.
func (b *Boot) Write(addr uint16, value uint8) {
	if addr == 0xff50 {
		b.register = value
		b.disabled = true
	} // Writing to the boot ROM itself is obviously not allowed.
}

//
// CPU-related code. You'd probably want to put that in a 'cpu' package.
//

// CPU's register F stores the values of four flags: Z, N, H and C.
// The values below represent those flag's bit position in F.
// There is a nicer way to define these. See https://golang.org/ref/spec#Iota
const (
	FlagC uint8 = 0x10
	FlagH uint8 = 0x20
	FlagN uint8 = 0x40
	FlagZ uint8 = 0x80
)

// CPU structure with embedded Memory Management Unit.
type CPU struct {
	A, F uint8
	B, C uint8
	D, E uint8
	H, L uint8
	SP   uint16
	PC   uint16

	mmu MMU
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

// DE returns the value of registers D and E as if reading from a single
// 16-bit register.
func (c *CPU) DE() uint16 {
	return uint16(c.D)<<8 | uint16(c.E)
}

// SetDE stores a 16-bit value into D and E as if writing to a single 16-bit
// register.
func (c *CPU) SetDE(value uint16) {
	c.E = uint8(value & 0x00ff)
	c.D = uint8((value & 0xff00) >> 8)
}

// HL returns the value of registers H and L as if reading from a single
// 16-bit register.
func (c *CPU) HL() uint16 {
	return uint16(c.H)<<8 | uint16(c.L)
}

// SetHL stores a 16-bit value into H and L as if writing to a single 16-bit
// register.
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
var instructions = map[uint8]func(*CPU){
	0x04: incb,
	0x05: decb,
	0x06: ldbd8,
	0x0c: incc,
	0x0d: decc,
	0x0e: ldcd8,
	0x11: ldded16,
	0x13: incde,
	0x17: rla,
	0x18: jrr8,
	0x1a: ldade,
	0x1e: lded8,
	0x20: jrnzr8,
	0x21: ldhld16,
	0x22: ldhlia,
	0x23: inchl,
	0x28: jrzr8,
	0x2e: ldld8,
	0x31: ldspd16,
	0x32: ldhlda,
	0x3d: deca,
	0x3e: ldad8,
	0x4f: ldca,
	0x57: ldda,
	0x67: ldha,
	0x77: ldhla,
	0x7b: ldae,
	0xaf: xora,
	0xc1: popbc,
	0xc5: pushbc,
	0xc9: ret,
	0xcd: call,
	0xe0: ldffd8a,
	0xe2: ldffca,
	0xea: ldd16a,
	0xf0: ldaffd8,
	0xfe: cpd8,
}

// Extended instruction set (two-byte opcodes starting with 0xcb)
var extendedInstructions = map[uint8]func(*CPU){
	0x11: rlc,
	0x7c: bit7h,
}

//
// Generic instructions used to group common code.
//
// r 	single (8-bit) register (A, F, B C, D, E, H or L)
// rr	double (16-bit) register (AF, BC, DE or HL, SP and PC are handled separately)
// d8	8-bit (unsigned) parameter (1 byte) after opcode
// d16	16-bit (unsigned) parameter (2 bytes, little-endian) after opcode
// r8	8-bit (signed) parameter (1 byte) after opcode

// LD rr,d16 - Copy d16 in double register rr (pretty easy thanks to having
// separate single registers)
func ldrrd16(c *CPU, high, low *uint8) {
	*low = c.mmu.Read(c.PC)
	*high = c.mmu.Read(c.PC + 1)
	c.PC += 2
}

// XOR r (even though the boot ROM only contains XOR A which means A=0)
func xorr(c *CPU, reg uint8) {
	c.A ^= reg
}

// LD (addr),r - Store the value in r at memory address addr.
func ldaddrr(c *CPU, addr uint16, reg uint8) {
	c.mmu.Write(addr, reg)
}

// BIT n,r - Set CPU's Z flag to the value of bit n in register r.
// The documentation also specifies that this instruction must:
//  * set flag N to 0
//  * set flag H to 1
//  * keep flag C to its current value
func bitnr(c *CPU, bit, reg uint8) {
	if reg&(1<<bit) == 0 {
		c.F = (c.F & ^FlagN) | FlagZ | FlagH
	} else {
		c.F = (c.F & ^(FlagN | FlagZ)) | FlagH
	}
}

// JR [<condition>],r8 - Relative jump to the given offset if condition is
// true (or if there is no condition).
// In other words, add r8 (which can be positive or negative) to PC.
func jr(c *CPU, condition bool) {
	// Read() returns an unsigned 8-bit value. Casting it to a signed
	// 8-bit value does what we expect (i.e. values over 127 will
	// be converted to their signed equivalent between -128 and -1).
	// Note that casting it to an int16 directly would not work, since
	// values over 127 can still be represented by a positive number
	// when using 16 bits.
	offset := int8(c.mmu.Read(c.PC))
	c.PC++
	if condition {
		// Now we need a cast to int16 for the potential subtraction
		// between two 16-bit values.
		c.PC = uint16(int16(c.PC) + int16(offset))
	}
}

// LD r,d8 - Store the given 8-bit value in register r.
func ldrd8(c *CPU, reg *uint8) {
	*reg = c.mmu.Read(c.PC)
	c.PC++
}

// LD (HL),r - Store the value in register r to memory at address HL.
func ldhlr(c *CPU, reg uint8) {
	c.mmu.Write(c.HL(), reg)
}

// PUSH rr - Store the value of a 16-bit register at the memory address in SP.
// Handling two 8-bit parameters instead of a single 16-bit value because most
// of the time we'll use our 8-bit registers as arguments anyway.
// We decrement SP because the stack grows down from the top of the RAM.
func push(c *CPU, high, low uint8) {
	c.SP -= 2
	c.mmu.Write(c.SP, low)
	c.mmu.Write(c.SP+1, high)
}

// POP rr - Store the 16-bit value at the memory address in SP in the given
// 16-bit register. Handling two 8-bit parameters instead of a single 16-bit
// value because most of the time we'll use our 8-bit registers as arguments
// anyway.
func pop(c *CPU, high, low *uint8) {
	*low = c.mmu.Read(c.SP)
	*high = c.mmu.Read(c.SP + 1)
	c.SP += 2
}

// RL r - "Rotate left through Carry". Rotate a register's value left,
// storing its former leftmost bit in the Carry flag, and setting its new
// rightmost bit to the value previously in the Carry flag.
// Essentially this shifts the concatenation of the Carry flag bit and r left
// and wraps around.
func rl(c *CPU, reg *uint8) {
	result := *reg << 1 & 0xff
	if c.F&FlagC > 0 {
		result |= 1
	}
	// Flags z 0 0 c
	c.F = 0x00
	if result == 0 {
		c.F |= FlagZ
	}
	if *reg&(1<<7) > 0 {
		c.F |= FlagC
	}
	*reg = result
}

// INC r - Decrement a single register and set flags accordingly.
func incr(c *CPU, reg *byte) {
	// Flags z 1 h -
	c.F &= FlagC
	c.F |= FlagN
	// If incrementing will overflow the lower nibble, set H flag.
	if *reg&0x0f == 0x0f {
		c.F |= FlagH
	}
	*reg++
	if *reg == 0 {
		c.F |= FlagZ
	}
}

// DEC r - Decrement a single register and set flags accordingly.
func decr(c *CPU, reg *byte) {
	// Flags z 1 h -
	c.F &= FlagC
	c.F |= FlagN
	// If decrementing will wrap around the lower nibble, set H flag.
	if *reg&0x0f == 0 {
		c.F |= FlagH
	}
	*reg--
	if *reg == 0 {
		c.F |= FlagZ
	}
}

// INC rr - Increment a 16-bit register. Easier than a, 8-bit one because no
// flag is modified.
func incrr(c *CPU, high, low *uint8) {
	if *low == 0xff {
		*high++
	}
	*low++
}

// CP r/d8 - Compare a value with A. Equivalent to doing a subtraction and
// setting the CPU flags accordingly, but without keeping the result.
// Returns the result of the operation so sub() can use it.
func cp(c *CPU, value uint8) uint8 {
	// Flags: z 1 h c
	c.F = FlagN
	if value&0xf > c.A&0xf {
		c.F |= FlagH
	}
	if value > c.A {
		c.F |= FlagC
	}
	result := c.A - value
	if result == 0 {
		c.F |= FlagZ
	}
	return result
}

//
// Individual instructions in the order they appear in the boot ROM.
//

// LD SP,d16 (not calling LD rr,d16 because we made SP a single uint16)
func ldspd16(c *CPU) {
	c.SP = uint16(c.mmu.Read(c.PC)) | uint16(c.mmu.Read(c.PC+1))<<8
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
	// Using getters/setters to treat HL as a single 16-bit register.
	ldaddrr(c, c.HL(), c.A)
	c.SetHL(c.HL() - 1)
}

// BIT 7,H
func bit7h(c *CPU) {
	bitnr(c, 7, c.H)
}

// JR NZ,r8
func jrnzr8(c *CPU) {
	jr(c, c.F&FlagZ == 0)
}

// LD C,d8
func ldcd8(c *CPU) {
	ldrd8(c, &c.C)
}

// LD A,d8
func ldad8(c *CPU) {
	ldrd8(c, &c.A)
}

// LD (FF00+C),A
func ldffca(c *CPU) {
	c.mmu.Write(0xff00+uint16(c.C), c.A)
}

// INC C
func incc(c *CPU) {
	incr(c, &c.C)
}

// LD (HL),A
func ldhla(c *CPU) {
	ldhlr(c, c.A)
}

// LD (FF00+d8),A
func ldffd8a(c *CPU) {
	c.mmu.Write(0xff00+uint16(c.mmu.Read(c.PC)), c.A)
	c.PC++
}

// LD DE,d16
func ldded16(c *CPU) {
	ldrrd16(c, &c.D, &c.E)
}

// LD A,(DE)
func ldade(c *CPU) {
	c.A = c.mmu.Read(c.DE())
}

// CALL d16
func call(c *CPU) {
	// Advance PC before pushing its new value (i.e. the address of the
	// next instruction to return to) to the stack.
	addr := uint16(c.mmu.Read(c.PC)) | uint16(c.mmu.Read(c.PC+1))<<8
	c.PC += 2
	push(c, uint8(c.PC>>8), uint8(c.PC&0xff))
	c.PC = addr
}

// LD C,A
func ldca(c *CPU) {
	// LD r,r is trivial enough not to need a helper function.
	c.C = c.A
}

// LD B,d8
func ldbd8(c *CPU) {
	ldrd8(c, &c.B)
}

// PUSH BC
func pushbc(c *CPU) {
	push(c, c.B, c.C)
}

// RL C - this is used by the boot ROM's "graphic routine" and it does
// something pretty cool that I hope to illustrate soonish.
func rlc(c *CPU) {
	rl(c, &c.C)
}

// RLA - Distinct from RL A (same opcode, but in the extended instruction set).
// This makes no difference right now, but when we start taking timings into
// account, the difference between one and two memory reads will be relevant.
func rla(c *CPU) {
	rl(c, &c.A)
}

// POP BC
func popbc(c *CPU) {
	pop(c, &c.B, &c.C)
}

// DEC B
func decb(c *CPU) {
	decr(c, &c.B)
}

// LD (HL+),A
func ldhlia(c *CPU) {
	ldaddrr(c, c.HL(), c.A)
	c.SetHL(c.HL() + 1)
}

// INC HL
func inchl(c *CPU) {
	incrr(c, &c.H, &c.L)
}

// RET
func ret(c *CPU) {
	// Simulate POP PC for consistency.
	var P, C uint8
	pop(c, &P, &C)
	c.PC = uint16(P)<<8 | uint16(C)
}

// INC DE
func incde(c *CPU) {
	incrr(c, &c.D, &c.E)
}

// LD A,E
func ldae(c *CPU) {
	c.A = c.E
}

// CP d8
func cpd8(c *CPU) {
	val := c.mmu.Read(c.PC)
	c.PC++
	cp(c, val) // Ignore result, we only want to set CPU flags.
}

// LD (d16),A
func ldd16a(c *CPU) {
	addr := uint16(c.mmu.Read(c.PC)) | uint16(c.mmu.Read(c.PC+1))<<8
	c.PC += 2
	c.mmu.Write(addr, c.A)
}

// DEC A
func deca(c *CPU) {
	decr(c, &c.A)
}

// JR Z,r8
func jrzr8(c *CPU) {
	jr(c, c.F&FlagZ != 0)
}

// LD H,A
func ldha(c *CPU) {
	c.H = c.A
}

// LD D,A
func ldda(c *CPU) {
	c.D = c.A
}

// INC B
func incb(c *CPU) {
	incr(c, &c.B)
}

// DEC C
func decc(c *CPU) {
	decr(c, &c.C)
}

// LD L,d8
func ldld8(c *CPU) {
	ldrd8(c, &c.L)
}

// JR r8
func jrr8(c *CPU) {
	jr(c, true)
}

// LD E,d8
func lded8(c *CPU) {
	ldrd8(c, &c.E)
}

// LD A,(FF00+d8)
func ldaffd8(c *CPU) {
	c.A = c.mmu.Read(0xff00 + uint16(c.mmu.Read(c.PC)))
	c.PC++
}

//
// Main loop running code in memory from address 0 on our CPU.
// When an unknown opcode is encountered, quit and show CPU state.
//
func main() {
	boot := NewBoot("./dmg-rom.bin")     // Covers 0x0000→0x00ff and 0xff50
	ram := NewRAM(0x8000, 0xffff-0x8000) // Covers 0x8000→0xffff

	// MMU looking up addresses in boot ROM or BOOT register first,
	// then in RAM. So even if the ram objects technically contains
	// address 0xff50, the boot object will take precedence.
	mmu := MMU{[]Addressable{boot, ram}}

	cpu := CPU{mmu: mmu}

	fmt.Println("Starting state:")
	fmt.Println(&cpu)

	fmt.Println("Endless loop begins:")
	// Only display a few iterations of the loop where we get stuck.
	loopsLeft := 3

	for {
		// The next opcode to execute is the byte at the exact address
		// pointed to by PC.
		opcode := cpu.mmu.Read(cpu.PC)
		cpu.PC++

		// Choose in which instruction set (base or extended) we'll
		// look up the opcode.
		extended := false
		instructionSet := instructions
		if opcode == 0xcb {
			// Extended instruction set, opcode is one more byte.
			opcode = cpu.mmu.Read(cpu.PC)
			cpu.PC++
			instructionSet = extendedInstructions
			extended = true
		}

		// Try finding a corresponding instruction in the instructions
		// mapping. Keys that don't have a value will return 'nil'.
		if instruction := instructionSet[opcode]; instruction != nil {
			instruction(&cpu)
		} else {
			if extended {
				fmt.Printf("Unknown extended opcode: 0xcb %#02x\n", opcode)
			} else {
				fmt.Printf("Unknown opcode: %#02x\n", opcode)
			}
			fmt.Println(&cpu)
			return
		}

		// Without a PPU, this program will just keep running forever.
		// Show where it's getting stuck and exit.
		if cpu.PC >= 0x0064 && cpu.PC <= 0x0068 {
			fmt.Println(&cpu)
		}

		if cpu.PC == 0x0068 {
			loopsLeft--
			if loopsLeft == 0 {
				fmt.Println("... and so on. We're stuck now, see you next article!")
				return
			}
		}
	}
}
