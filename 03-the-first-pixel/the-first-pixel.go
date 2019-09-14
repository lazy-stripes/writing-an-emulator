// This file is provided as example code to illustrate the following article:
// https://blog.tigris.fr/2019/09/15/writing-an-emulator-the-first-pixel/
// Feel free to do whatever with it!
package main

import (
	"bytes"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
)

////////////////////////////////////////////////////////////////////////////////
//
// Generic code. You'd probably want to put that in some 'utils' package.
//
////////////////////////////////////////////////////////////////////////////////

// FIFO structure for shifting out pixels to the display or enqueue CPU
// micro-operations. We use interface{} as a placeholder for any type.
// We also make it a fixed size that works for CPU operations as well as PPU
// pixels.
type FIFO struct {
	fifo [16]interface{} // Array of values in the FIFO.
	out  int             // Current index of the tail (output) of the FIFO.
	in   int             // Current index of the head (input) of the FIFO.
	len  int             // Current length of the FIFO.
}

// Pre-defined errors to only instantiate them once.
var errFIFOOverflow = errors.New("FIFO buffer overflow")
var errFIFOUnderrun = errors.New("FIFO buffer underrun")

// Push an item to the FIFO.
func (f *FIFO) Push(item interface{}) error {
	if f.len == len(f.fifo) {
		return errFIFOOverflow
	}
	f.fifo[f.in] = item
	f.in = (f.in + 1) % len(f.fifo)
	f.len++
	return nil
}

// Pop an item out of the FIFO.
func (f *FIFO) Pop() (item interface{}, err error) {
	if f.len == 0 {
		return 0, errFIFOUnderrun
	}
	item = f.fifo[f.out]
	f.out = (f.out + 1) % len(f.fifo)
	f.len--
	return item, nil
}

// Size returns the current amount of items in the FIFO.
func (f *FIFO) Size() int {
	return f.len
}

// Clear resets internal indexes, effectively clearing out the FIFO.
func (f *FIFO) Clear() {
	f.in, f.out, f.len = 0, 0, 0
}

////////////////////////////////////////////////////////////////////////////////
//
// Display-related code. You'd probably want to put that in a 'display' package.
//
////////////////////////////////////////////////////////////////////////////////

// Display interface supporting pixel output and palettes.
type Display interface {
	// Enable turns the display on. By default, nothing is displayed.
	Enable()

	// Enabled returns whether the display is on.
	Enabled() bool

	// Disable turns the display off. Should only be called during VBlank.
	Disable()

	// Write outputs a pixel (defined as a color number) to the display.
	Write(color uint8)

	// HBlank is called whenever all pixels in a scanline have been output.
	HBlank()

	// HBlank is called whenever a full frame has been output.
	VBlank()
}

// Console display shifting pixels out to standard output.
type Console struct {
	Palette [4]rune
	enabled bool
}

// NewConsole returns a Console display with dark-themed unicode pixels.
// If your terminal is light-themed reverse the order of the Palette
// array below.
func NewConsole() *Console {
	return &Console{Palette: [4]rune{'█', '▒', '░', ' '}}
}

// Enable turns on the display. "Pixels" will be printed out the moment they're
// written to the display.
func (c *Console) Enable() {
	c.enabled = true
}

// Enabled returns whether the display is enabled or not (as part of the
// Display interface).
func (c *Console) Enabled() bool {
	return c.enabled
}

// Disable turns off the display. No output will occur.
func (c *Console) Disable() {
	c.enabled = false
}

// Write prints out a pixel from our rune palette if display is enabled.
func (c *Console) Write(colorIndex uint8) {
	if c.enabled {
		fmt.Printf("%[1]c%[1]c", c.Palette[colorIndex])
	}
}

// HBlank prints a newline to set up the console for the next scanline.
func (c *Console) HBlank() {
	if c.enabled {
		fmt.Print("\n")
	}
}

// VBlank prints a separation between each console screen frame.
func (c *Console) VBlank() {
	if c.enabled {
		fmt.Print("\n === VBLANK ===\n")
		// If you want to clear the screen instead, you can use the code below:
		//fmt.Print("\033[2J")
	}
}

// HDConsole uses ANSI foreground/background colors and the '▀' block character
// to fit two pixels per output character instead of one. Not only does this
// take less space on screen, it also gives pixels a much more natural ratio.
// It requires a 256-color terminal.
type HDConsole struct {
	Console // Embed Console type, we'll only redefine Write() and HBlank()

	// Override the embedded Console type's Palette property.
	Palette [4]uint8 // ANSI 256-color codes for each color index

	// Buffer holding pixel data for two 160-pixel scanlines. First pass will
	// set the top pixel color, second pass will set the lower pixel color.
	buffer [160][2]byte

	pos  uint // Current position in line buffer
	line uint // Current buffered line being written to (zero is the top line).
}

// NewHDConsole returns a Console display with dark-themed ANSI pixels.
func NewHDConsole() *HDConsole {
	return &HDConsole{Palette: [4]uint8{254, 245, 240, 236}}
}

// Write a pixel to the current line in the buffer (if the display is enabled).
// We buffer two lines worth of pixels before printing them out.
func (c *HDConsole) Write(colorIndex uint8) {
	if c.enabled {
		c.buffer[c.pos][c.line] = colorIndex
		c.pos++
	}
}

// HBlank either updates the internal line index in our buffer, or prints out
// the last two buffered scanlines in a single string ending with a carriage
// return.
func (c *HDConsole) HBlank() {
	if c.enabled {
		if c.line == 1 {
			// We're just done filling up the second buffered line, we can now
			// print the whole buffer.

			// Read each pair of pixel colors. We use a half-block character
			// with distinct colors for the foreground and background to
			// represent the top and bottom pixel together.
			for _, pair := range c.buffer {
				topColor := pair[0]
				bottomColor := pair[1]

				// See https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
				// Note: we could compare the two pixels and only output a full
				// block character in a single color if they match. This is left
				// as an exercise to the reader.
				fmt.Printf("\x1b[38;5;%d;48;5;%dm▀\x1b[0m", c.Palette[topColor],
					c.Palette[bottomColor])
			}
			fmt.Print("\n")

			// Reset line position to buffer the next two scanlines.
			c.line = 0
		} else {
			// We're done buffering the top scanline. Do the bottom one next.
			c.line++
		}
		// Also reset position in line for the next scanline.
		c.pos = 0
	}
}

// FetcherState is an enum-like type to define all admissible states for the
// PPU's fetcher.
type FetcherState uint8

// Possible Fetcher states. Values don't really matter, they start from zero.
const (
	ReadTileID FetcherState = iota
	ReadTileData0
	ReadTileData1
	PushToFIFO
)

// Fetcher reading tile data from VRAM and pushing pixels to a FIFO queue.
type Fetcher struct {
	FIFO     FIFO         // Pixel FIFO that the PPU will read.
	mmu      *MMU         // Reference to the global MMU.
	ticks    int          // Clock cycle counter for timings.
	state    FetcherState // Current state of our state machine.
	mapAddr  uint16       // Start address of BG/Windows map row.
	dataAddr uint16       // Start address of Sprite/BG tile data.
	tileLine uint8        // Y offset (in pixels) in the tile.

	// Index of the tile to read in the current row of the background map.
	tileIndex uint8

	tileID   uint8    // Tile number in the tilemap.
	tileData [8]uint8 // Pixel data for one row of the fetched tile.
}

// Start fetching a line of pixels starting from the given tile address in the
// background map. Here, tileLine indicates which row of pixels to pick from
// each tile we read.
func (f *Fetcher) Start(mapAddr uint16, tileLine uint8) {
	f.tileIndex = 0
	f.mapAddr = mapAddr
	f.tileLine = tileLine
	f.state = ReadTileID

	// Clear FIFO between calls, as it may still contain leftover tile data
	// from the very end of the previous scanline.
	f.FIFO.Clear()
}

// Tick advances the fetcher's state machine one step.
func (f *Fetcher) Tick() {
	// The Fetcher runs at half the speed of the PPU (every 2 clock cycles).
	f.ticks++
	if f.ticks < 2 {
		return
	}
	f.ticks = 0 // Reset tick counter and execute next state.

	switch f.state {
	case ReadTileID:
		// Read the tile's number from the background map. This will be used
		// in the next states to find the address where the tile's actual pixel
		// data is stored in memory.
		f.tileID = f.mmu.Read(f.mapAddr + uint16(f.tileIndex))
		f.state = ReadTileData0

	case ReadTileData0:
		f.ReadTileLine(0, f.tileID, f.tileLine, &f.tileData)
		f.state = ReadTileData1

	case ReadTileData1:
		f.ReadTileLine(1, f.tileID, f.tileLine, &f.tileData)
		f.state = PushToFIFO

	case PushToFIFO:
		if f.FIFO.Size() <= 8 {
			// We stored pixel bits from least significant (rightmost) to most
			// (leftmost) in the data array, so we must push them in reverse
			// order.
			for i := 7; i >= 0; i-- {
				f.FIFO.Push(f.tileData[i])
			}
			// Advance to the next tile in the map's row.
			f.tileIndex++
			f.state = ReadTileID
		}
	}
}

// ReadTileLine updates the fetcher's internal pixel buffer with tile data
// depending on the current state. Each pixel needs 2 bits of information,
// which are read in two separate steps.
func (f *Fetcher) ReadTileLine(bitPlane uint8, tileID uint8, tileLine uint8,
	data *[8]uint8) {
	// A tile's graphical data takes 16 bytes (2 bytes per row of 8 pixels).
	// Tile data starts at address 0x8000 so we first compute an offset to
	// find out where the data for the tile we want starts.
	offset := 0x8000 + (uint16(tileID) * 16)

	// Then, from that starting offset, we compute the final address to read
	// by finding out which of the 8-pixel rows of the tile we want to display.
	addr := offset + (uint16(tileLine) * 2)

	// Finally, read the first or second byte of graphical data depending on
	// what state we're in.
	pixelData := f.mmu.Read(addr + uint16(bitPlane))
	for bitPos := uint(0); bitPos <= 7; bitPos++ {
		// Separate each bit fom the data byte we just read. Each of these bits
		// is half of a pixel's color value.
		if bitPlane == 0 {
			// Least significant bit, replace the previous value.
			data[bitPos] = (pixelData >> bitPos) & 1
		} else {
			// Most significant bit, update the previous value.
			data[bitPos] |= ((pixelData >> bitPos) & 1) << 1
		}
	}
}

// PPUState is an enum-like type to define all admissible states for the PPU.
type PPUState uint8

// Possible PPU states. Values don't really matter, they start from zero.
const (
	OAMSearch PPUState = iota
	PixelTransfer
	HBlank
	VBlank
)

// LCD Control register bits (see https://golang.org/ref/spec#Iota)
const (
	// Bit 0 - BG/Window Display/Priority     (0=Off, 1=On)
	LCDCBGDisplay uint8 = 1 << iota
	// Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
	LCDCSpriteDisplayEnable
	// Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
	LCDCSpriteSize
	// Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
	LCDCBGTileMapDisplayeSelect
	// Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
	LCDCBGWindowTileDataSelect
	// Bit 5 - Window Display Enable          (0=Off, 1=On)
	LCDCWindowDisplayEnable
	// Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
	LCDCWindowTileMapDisplayeSelect
	// Bit 7 - LCD Display Enable             (0=Off, 1=On)
	LCDCDisplayEnable
)

// PPU that continuously scans video RAM, enqueues pixels to display in a FIFO
// and pops them out to an arbitrary display. This version only implements the
// few registers required to display the background map (no scrolling). For
// synchronicity with the CPU, the PPU is implemented as a state machine whose
// Tick() method is called every clock cycle.
// Since it holds hardware registers, the PPU also implements the Addressable
// interface.
type PPU struct {
	LCDC uint8 // LCD Control register
	LY   uint8 // Number of the scanline currently being displayed.

	// Fetcher runs at half the PPU's speed and fetches pixel data from the
	// background map's tiles, according to the current scanline. It also holds
	// the FIFO pixel queue that we will be writing out to the display.
	Fetcher Fetcher

	// Screen object implementing our Display interface above. Just a text
	// console for now, but we can swap it for a proper window later.
	Screen Display

	state PPUState // Current state of the state machine.
	ticks uint     // Clock ticks counter for the current line.
	x     uint8    // Current amount of pixels already output for the current line.
}

// NewPPU returns an instance of PPU using the given display object.
func NewPPU(screen Display) *PPU {
	ppu := PPU{Screen: screen}
	return &ppu
}

// Contains return true if the requested address is the LCDC or LY register.
func (p *PPU) Contains(addr uint16) bool {
	return addr == 0xff40 || addr == 0xff44
}

// Read returns the current value in LCDC or LY resgister.
func (p *PPU) Read(addr uint16) uint8 {
	switch addr {
	case 0xff40:
		return p.LCDC
	case 0xff44:
		return p.LY
	}
	panic("invalid PPU read address")
}

// Write updates the value in LCDC and ignore writes to LY.
func (p *PPU) Write(addr uint16, value uint8) {
	switch addr {
	case 0xff40:
		p.LCDC = value
	case 0xff44:
		// Documentation is unclear as to what happens when writing to LY.
		// The Pan Docs say "Writing will reset the counter." but list
		// the register as read-only. For now we'll go read-only.
	default:
		panic("invalid PPU write address")
	}
}

// Tick advances the PPU state one step.
func (p *PPU) Tick() {
	// Check if the screen should be turned on or off depending on LCDC value.
	if !p.Screen.Enabled() {
		if p.LCDC&LCDCDisplayEnable != 0 {
			// Turn screen on.
			p.Screen.Enable()
			p.state = OAMSearch
		} else {
			// Screen is off, PPU remains idle.
			return
		}
	} else {
		if p.LCDC&LCDCDisplayEnable == 0 {
			// Turn screen off and reset PPU state machine.
			p.LY = 0
			p.x = 0
			p.Screen.Disable()
			return
		}
	}

	// Screen is on, keep counting ticks.
	p.ticks++

	switch p.state {
	case OAMSearch:
		// In this state, the PPU would scan the OAM (Objects Attribute Memory)
		// from 0xfe00 to 0xfe9f to mix sprite pixels in the current line later.
		// This always takes 40 clock ticks.

		//
		// OAM search will happen here (when implemented).
		//

		if p.ticks == 40 {
			// Move to Pixel Transfer state. Initialize the fetcher to start
			// reading background tiles from VRAM. We don't do scrolling yet
			// and the boot ROM does nothing fancy with map addresses, so we
			// just give the fetcher the base address of the row of tiles we
			// need in video RAM:
			//
			// - The background map is 32×32 tiles big.
			// - The viewport starts at the top left of that map when LY is 0.
			// - Each tile is 8×8 pixels.
			//
			// In the present case, we only need to figure out in which row of
			// the background map our current line (at position LY) is. Then we
			// start fetching pixels from that row's address in VRAM, and for
			// each tile, we can tell which 8-pixel line to fetch by computing
			// LY modulo 8.
			p.x = 0
			tileLine := p.LY % 8
			tileMapRowAddr := 0x9800 + (uint16(p.LY/8) * 32)
			p.Fetcher.Start(tileMapRowAddr, tileLine)

			p.state = PixelTransfer
		}

	case PixelTransfer:
		// Fetch pixel data into our pixel FIFO.
		p.Fetcher.Tick()

		// Stop here if the FIFO isn't holding at least 8 pixels. This will
		// be used to mix in sprite data when we implement these. It also
		// guarantees the FIFO will always have data to Pop() later.
		if p.Fetcher.FIFO.Size() <= 8 {
			return
		}

		// Put a pixel from the FIFO on screen.
		pixelColor, _ := p.Fetcher.FIFO.Pop()
		p.Screen.Write(pixelColor.(uint8))

		// Check when the scanline is complete (160 pixels).
		p.x++
		if p.x == 160 {
			// Switch to HBlank state. For our console screen, that means
			// printing a carriage return.
			p.Screen.HBlank()
			p.state = HBlank
		}

	case HBlank:
		// Nothing much to do here but wait the proper number of clock cycles.
		// A full scanline takes 456 clock cycles to complete. At the end of a
		// scanline, the PPU goes back to the initial OAM Search state.
		// When we reach line 144, we switch to VBlank state instead.
		if p.ticks == 456 {
			p.ticks = 0
			p.LY++
			if p.LY == 144 {
				p.Screen.VBlank()
				p.state = VBlank
			} else {
				p.state = OAMSearch
			}
		}

	case VBlank:
		// Nothing much to do here either. VBlank is when the CPU is supposed to
		// do stuff that takes time. It takes as many cycles as would be needed
		// to keep displaying scanlines up to line 153.
		if p.ticks == 456 {
			p.ticks = 0
			p.LY++
			if p.LY == 153 {
				// End of VBlank, back to initial state.
				p.LY = 0
				p.state = OAMSearch
			}
		}
	}
}

////////////////////////////////////////////////////////////////////////////////
//
// Memory-related code. You'd probably want to put that in a 'memory' package.
//
////////////////////////////////////////////////////////////////////////////////

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
func (r *RAM) Read(addr uint16) uint8 {
	return r.bytes[addr-r.start]
}

// Write stores the given value at the given address (adjusting for offset).
func (r *RAM) Write(addr uint16, value uint8) {
	r.bytes[addr-r.start] = value
}

// Contains returns true as long as the given address fits in the slice.
func (r *RAM) Contains(addr uint16) bool {
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

////////////////////////////////////////////////////////////////////////////////
//
// CPU-related code. You'd probably want to put that in a 'cpu' package.
//
////////////////////////////////////////////////////////////////////////////////

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

// Tick advances the CPU's state machine one step. This is oversimplified for
// the sake of the example. Executing a CPU instruction normally takes a lot
// longer, and all operations don't complete in the same number of cycles. But
// it's Good Enough™ for now.
func (c *CPU) Tick() {
	// The next opcode to execute is the byte at the exact address
	// pointed to by PC.
	opcode := c.mmu.Read(c.PC)
	c.PC++

	// Choose in which instruction set (base or extended) we'll
	// look up the opcode.
	extended := false
	instructionSet := instructions
	if opcode == 0xcb {
		// Extended instruction set, opcode is one more byte.
		opcode = c.mmu.Read(c.PC)
		c.PC++
		instructionSet = extendedInstructions
		extended = true
	}

	// Try finding a corresponding instruction in the instructions
	// mapping. Keys that don't have a value will return 'nil'.
	if instruction := instructionSet[opcode]; instruction != nil {
		instruction(c)
	} else {
		if extended {
			fmt.Printf("Unknown extended opcode: 0xcb %#02x\n", opcode)
		} else {
			fmt.Printf("Unknown opcode: %#02x\n", opcode)
		}
		fmt.Println(c)
		os.Exit(1)
	}
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

// Generic instructions used to regroup common code. The following placeholders
// are used in names and descriptions:
//
// r 	single (8-bit) register (A, F, B, C, D, E, H or L)
// rr	double (16-bit) register (AF, BC, DE or HL)
// d8	8-bit (unsigned) parameter (1 byte) after opcode
// d16	16-bit (unsigned) parameter (2 bytes, little-endian) after opcode
// r8	8-bit (signed) parameter (1 byte) after opcode

// LD rr,d16 - Copy d16 in double register rr (pretty easy thanks to having
// separate single registers).
func ldrrd16(c *CPU, high, low *uint8) {
	*low = c.mmu.Read(c.PC)
	*high = c.mmu.Read(c.PC + 1)
	c.PC += 2
}

// XOR r (even though the boot ROM only contains XOR A which means A=0).
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
// We use two 8-bit parameters instead of a single 16-bit value because most
// of the time we'll use our 8-bit registers as arguments anyway.
// We decrement SP because the stack grows down from the top of the RAM.
func push(c *CPU, high, low uint8) {
	c.SP -= 2
	c.mmu.Write(c.SP, low)
	c.mmu.Write(c.SP+1, high)
}

// POP rr - Store the 16-bit value at the memory address in SP in the given
// 16-bit register. We use two 8-bit parameters instead of a single 16-bit
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
// Essentially this shifts the concatenation of the Carry flag bit and r left,
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

// INC r - Increment a single register and set flags accordingly.
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

// INC rr - Increment a 16-bit register. Easier than an 8-bit one because no
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

// RL C - This is used by the boot ROM's "graphic routine" and it does
// something pretty cool that I hope to illustrate in some future article.
func rlc(c *CPU) {
	rl(c, &c.C)
}

// RLA - Distinct from RL A (same opcode, but in the extended instruction set).
// This makes no difference right now, but when we start taking timings into
// account, the difference between one and two instruction bytes (and thus two
// memory reads) will be relevant.
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

////////////////////////////////////////////////////////////////////////////////
//
// Main loop running code in memory from address 0 on our CPU.
// When an unknown opcode is encountered, quit and show CPU state.
//
////////////////////////////////////////////////////////////////////////////////
func main() {
	boot := NewBoot("./dmg-rom.bin")     // Covers 0x0000→0x00ff and 0xff50
	ram := NewRAM(0x8000, 0xffff-0x8000) // Covers 0x8000→0xffff

	// Set up display using a text-based output.
	//screen := NewHDConsole() // Try this if your terminal supports 256 colors
	screen := NewConsole()
	ppu := NewPPU(screen) // Covers 0xff40 and 0xff44

	// MMU looking up addresses in boot ROM or BOOT register first,
	// then in the PPU, then in RAM. So even if the RAM object technically
	// contains addresses shadowing the BOOT, LCDC or LY registers, the boot
	// or ppu objects will take precedence.
	mmu := MMU{[]Addressable{boot, ppu, ram}}
	ppu.Fetcher.mmu = &mmu
	cpu := CPU{mmu: mmu}

	for {
		// Now we have each component executing in parallel perform one tick's
		// worth of work in one iteration of our loop.
		cpu.Tick()
		ppu.Tick()
	}
}
