// This file is provided as example code to illustrate the following article:
// https://blog.tigris.fr/2021/07/28/writing-an-emulator-timing-is-key/
// Feel free to do whatever with it!
package main

// The comments below are used by Golang's C pseudo-package, which is used to
// interface with external C code. As we're doing low-level data transfers
// between Go pointers and C, we have to use this special syntax. This is way
// out of scope, but if you're curious, see: https://golang.org/cmd/cgo/
//
// The point is that the C-like comments below will make the Uint8 SDL type
// and our callback function usable as if they were part of a "C" package.

// typedef unsigned char Uint8;
// void mainLoopCallback(void *userdata, Uint8 *stream, int len);
import "C"

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"image/color"
	"io/ioutil"
	"os"
	"reflect"
	"unsafe"

	"github.com/faiface/mainthread"
	"github.com/veandco/go-sdl2/sdl"
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

	// Enable turns the display off. Should only be called during VBlank.
	Disable()

	// Write outputs a pixel (defined as a color number) to the display.
	Write(color uint8)

	// HBlank is called whenever all pixels in a scanline have been output.
	HBlank()

	// HBlank is called whenever a full frame has been output.
	VBlank()
}

// SDL display shifting pixels out to a single texture.
type SDL struct {
	// Palette will contain our R, G, B and A components for each of the four
	// potential colors the Game Boy can display.
	Palette [4]color.RGBA

	// The following fields store pointers to the Window, Renderer and Texture
	// objects used by the SDL display.
	window   *sdl.Window
	renderer *sdl.Renderer
	texture  *sdl.Texture

	// The texture buffer for the current frame, and the current offset where
	// new pixel data should be written into that buffer.
	buffer []byte
	offset int

	enabled bool
}

// DefaultPalette represents the selectable colors in the DMG. We use a greenish
// set of colors and Alpha is always 0xff since we won't use transparency.
var DefaultPalette = [4]color.RGBA{
	color.RGBA{0xe0, 0xf0, 0xe7, 0xff}, // White
	color.RGBA{0x8b, 0xa3, 0x94, 0xff}, // Light gray
	color.RGBA{0x55, 0x64, 0x5a, 0xff}, // Dark gray
	color.RGBA{0x34, 0x3d, 0x37, 0xff}, // Black
}

// Screen dimensions that will be used in several different places.
const (
	ScreenWidth  = 160
	ScreenHeight = 144
)

// NewSDL returns an SDL display with a greenish palette.
func NewSDL() *SDL {
	// Create the window where the Game Boy pixels will be drawn.
	window, err := sdl.CreateWindow("Timing is key",
		sdl.WINDOWPOS_UNDEFINED, // Don't specify any X or Y position
		sdl.WINDOWPOS_UNDEFINED,
		ScreenWidth*2, ScreenHeight*2, // 2× zoom
		sdl.WINDOW_SHOWN) // Make sure window is visible at creation time

	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create window: %s\n", err)
		return nil
	}

	// Create the renderer that will serve as an abstraction for the GPU and
	// output to the window we just created.
	renderer, err := sdl.CreateRenderer(window,
		-1,                       // Auto-select what rendering driver to use
		sdl.RENDERER_ACCELERATED) // Use hardware acceleration if possible

	if err != nil {
		window.Destroy()
		fmt.Fprintf(os.Stderr, "Failed to create renderer: %s\n", err)
		return nil
	}

	// Try to update our texture at the same rate our screen is refreshing.
	// This ought to slow down scrolling, but isn't the most reliable because
	// your screen's refresh rate may vary, your screen's refres rate may not
	// match the original Game Boy's, and also some GPU drivers just can't
	// sync to your screen anyway.
	//
	// We first try "adaptive vsync" where SDL will try skipping frames if
	// our emulator is too slow, but that mode may not be available so we fall
	// back on synchronizing to your physical screen's VBlank period which may
	// induce lag.
	// if err = sdl.GLSetSwapInterval(-1); err != nil {
	// 	fmt.Printf("Can't set adaptive vsync: %s\n", sdl.GetError())
	// 	// Try 'just' syncing to vblank then.
	// 	if err = sdl.GLSetSwapInterval(1); err != nil {
	// 		fmt.Printf("Can't sync to vblank: %s\n", sdl.GetError())
	// 	}
	// }

	// Create the texture that will be used to draw the Game Boy's screen.
	texture, err := renderer.CreateTexture(
		// We want to store RGBA color values in our texture buffer.
		uint32(sdl.PIXELFORMAT_RGBA32),

		// Tell the GPU this texture will change frequently, since we'll update
		// it once every frame.
		sdl.TEXTUREACCESS_STREAMING,

		// The texture itself is exactly the size of the Game Boy screen,
		// 160×144 pixels. The renderer will stretch it as needed to fit our
		// window's actual size.
		ScreenWidth, ScreenHeight)

	if err != nil {
		renderer.Destroy()
		window.Destroy()
		fmt.Fprintf(os.Stderr, "Failed to create texture: %s\n", err)
		return nil
	}

	// 160×144 pixels stored using 4 bytes each (as per RGBA32) give us the
	// exact size we need for the texture buffer.
	bufLen := ScreenWidth * ScreenHeight * 4
	buffer := make([]byte, bufLen)

	sdl := SDL{Palette: DefaultPalette, renderer: renderer, texture: texture,
		buffer: buffer}

	return &sdl
}

// Close frees all resources created by SDL.
func (s *SDL) Close() {
	s.texture.Destroy()
	s.renderer.Destroy()
	s.window.Destroy()
}

// Enable turns on the display. Pixels will be drawn to our texture and shown
// at VBlank time.
func (s *SDL) Enable() {
	s.enabled = true
}

// Enabled returns whether the display is enabled or not (as part of the
// Display interface).
func (s *SDL) Enabled() bool {
	return s.enabled
}

// Disable turns off the display. The screen texture won't be updated.
func (s *SDL) Disable() {
	s.offset = 0
	s.enabled = false
}

// Write adds a new pixel (a mere index into a palette) to the texture buffer.
func (s *SDL) Write(colorIndex uint8) {
	if s.enabled {
		color := s.Palette[colorIndex]
		s.buffer[s.offset+0] = color.R
		s.buffer[s.offset+1] = color.G
		s.buffer[s.offset+2] = color.B
		s.buffer[s.offset+3] = color.A
		s.offset += 4
	}
}

// HBlank is only there as part of the Display interface and has no use in this
// context.
func (s *SDL) HBlank() {}

// VBlank is called when the PPU reaches VBlank state. At this point, our SDL
// buffer should be ready to display.
func (s *SDL) VBlank() {
	if s.enabled {
		s.texture.Update(
			nil,           // Rectangle to update (here, the whole texture)
			s.buffer,      // Our up-to-date texture buffer
			ScreenWidth*4, // The texture buffer's "pitch" in bytes
		)
		s.renderer.Copy(s.texture, nil, nil)
		s.offset = 0
	}
	s.renderer.Present()
}

//
// Fetcher code.
//

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
	Registers // Embeds a mapping of addresses to register variables.

	LCDC uint8 // LCD Control register.
	LY   uint8 // Number of the scanline currently being displayed.
	BGP  uint8 // Background map tiles palette.
	SCY  uint8 // Y-scrolling (from the top of the screen).

	// Fetcher runs at half the PPU's speed and fetches pixel data from the
	// background map's tiles, according to the current scanline. It also holds
	// the FIFO pixel queue that we will be writing out to the display.
	Fetcher Fetcher

	// Screen object implementing our Display interface above. Just a text
	// console for now, but we can swap it for a proper window later.
	Screen Display

	state PPUState // Current state of the state machine.
	ticks uint     // Clock ticks counter for the current line.
	x     uint8    // Current count of pixels already output in a scanline.

	lastSystemTick uint32 // SDL CPU tick count used for rough timing.
}

// NewPPU returns an instance of PPU using the given display object.
func NewPPU(screen Display) *PPU {
	// Pre-instantiate the PPU object so we can refer to its registers.
	ppu := PPU{Screen: screen}

	// Associate addresses with the corresponding register variables.
	ppu.Registers = Registers{
		0xff40: &ppu.LCDC,
		0xff42: &ppu.SCY,
		0xff44: &ppu.LY,
		0xff47: &ppu.BGP,
	}

	return &ppu
}

// Write overrides Registers.Write to make sure we don't write to LY.
func (p *PPU) Write(addr uint16, value uint8) {
	if addr != 0xff44 {
		p.Registers.Write(addr, value)
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
			// reading background tiles from VRAM. The boot ROM does nothing
			// fancy with map addresses, so we just give the fetcher the base
			// address of the row of tiles we need in video RAM, adjusted with
			// the value in our vertical scrolling register.
			//
			// In the present case, we only need to figure out in which row of
			// the background map our current line (at position Y) is. Then we
			// start fetching pixels from that row's address in VRAM, and for
			// each tile, we can tell which 8-pixel line to fetch by computing
			// Y modulo 8.
			p.x = 0
			y := p.SCY + p.LY // Real Y value taking scrolling into account

			// The following is almost identical to the non-scrolling version,
			// substituting our computed Y value instead of only using LY.
			tileLine := y % 8
			tileMapRowAddr := 0x9800 + (uint16(y/8) * 32)
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

		// Put a pixel from the FIFO on screen. We take a value between 0 and 3
		// and use it to look up an actual color (yet another value between 0
		// and 3 where 0 is the lightest color and 3 the darkest) in the BGP
		// register.
		pixelColor, _ := p.Fetcher.FIFO.Pop()

		// BGP contains four consecutive 2-bit values. We take the one whose
		// index is given by pixelColor by shifting those 2-bit values right
		// that many times and only keeping the rightmost 2 bits. I initially
		// got the order wrong and fixed it thanks to coffee-gb.
		paletteColor := (p.BGP >> (pixelColor.(uint8) * 2)) & 3
		p.Screen.Write(paletteColor)

		// Check when the scanline is complete (160 pixels).
		p.x++
		if p.x == 160 {
			// Switch to HBlank state.
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
				// Run VBlank() on the main thread so it can update the GPU.
				mainthread.Call(p.Screen.VBlank)
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

// Add appends a new address space at the end of the MMU's internal list. This
// can be used to optionally add spaces to the MMU, like a cartridge's data if
// any is provided.
func (m *MMU) Add(space Addressable) {
	m.Spaces = append(m.Spaces, space)
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

// Cartridge type acting like a read-only extension of our RAM type, initialized
// with a file just like the BootROM type. This type directly embeds RAM so the
// Read() and Contains() methods are already implemented. We can just add an
// empty Write() method to make it fully read-only.
type Cartridge struct {
	RAM // Also embeds `bytes` and `start` properties.
}

// NewCartridge returns a new address space containing the cartridge's content
// and starting from zero. Returns nil in case of error, so that if there is
// no cartridge file in the current folder, we silently ignore it.
func NewCartridge(filename string) *Cartridge {
	cart, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Println(err)
		return nil
	}
	return &Cartridge{RAM{cart, 0}}
}

// Write does nothing for our cartridge. Some actual cartridges with extra chips
// in them actually do some specific stuff when you write to them, but that is
// way beyond the scope of this program.
func (c *Cartridge) Write(addr uint16, val uint8) {
	// Ignore all writes to this address space.
}

// Registers type allows mapping a 16-bit address to an 8-bit register variable.
// It also implements the Addressable interface.
type Registers map[uint16]*uint8

// Contains returns true if the given address corresponds to a known register.
func (r Registers) Contains(addr uint16) bool {
	return r[addr] != nil
}

// Read returns the byte in the register at the given address.
func (r Registers) Read(addr uint16) uint8 {
	if regPtr := r[addr]; regPtr != nil {
		return *regPtr
	}
	panic("invalid register read address")
}

// Write sets the value of the register at the given address. If you need some
// extra checks (for read-only registers for instance), you can just override
// this method in types embedding it.
func (r Registers) Write(addr uint16, value uint8) {
	if regPtr := r[addr]; regPtr != nil {
		*regPtr = value
	} else {
		panic("invalid register write address")
	}
}

////////////////////////////////////////////////////////////////////////////////
//
// CPU-related code. You'd probably want to put that in a 'cpu' package.
//
////////////////////////////////////////////////////////////////////////////////

// CPUState is an enum-like type to define all admissible states for the CPU.
type CPUState uint8

// Possible PPU states. Values don't really matter, they start from zero.
const (
	FetchOpcode CPUState = iota
	Execute
)

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

	extendedSet bool // Current instruction set (basic or extended)

	ops        FIFO  // Micro-operations queue
	tmp1, tmp2 uint8 // Storage for micro-op parameters

	state CPUState // FetchOpcode by default
	ticks uint
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
	// Any CPU operation takes 4 ticks.
	c.ticks++
	if c.ticks < 4 {
		return
	}
	c.ticks = 0

	switch c.state {
	case FetchOpcode:
		// The next opcode to execute is the byte at the exact address
		// pointed to by PC.
		opcode := c.mmu.Read(c.PC)
		c.PC++

		// Choose in which instruction set (base or extended) we'll
		// look up the opcode.
		if opcode == 0xcb && !c.extendedSet {
			// Extended instruction set, opcode is one more byte, don't update
			// current state and keep fetching.
			c.extendedSet = true
			return
		}

		// Try finding a corresponding instruction in the instructions
		// mapping. Keys that don't have a value will return 'nil'.
		var instruction func(*CPU)
		if c.extendedSet {
			instruction = extendedInstructions[opcode]
		} else {
			instruction = instructions[opcode]
		}
		if instruction == nil {
			if c.extendedSet {
				fmt.Printf("Unknown extended opcode: 0xcb %#02x\n", opcode)
			} else {
				fmt.Printf("Unknown opcode: %#02x\n", opcode)
			}
			fmt.Println(c)
			fmt.Println("Press <Enter> or <Return> to quit...")
			bufio.NewReader(os.Stdin).ReadBytes('\n')
			os.Exit(1)
		}

		// By this point we got the instruction we want, reset the extended set
		// bit for the next instruction.
		c.extendedSet = false

		// Create as many micro-operations as needed to complete this CPU
		// instruction in the proper number of cycles. Note that some
		// instructions complete "instantaneously" (i.e. within the 4 cycles it
		// takes to fetch and read the opcode). In that case, we just keep
		// fetching opcodes.
		instruction(c)

		// If we have more micro-operations to perform, do that in a separate
		// state.
		if c.ops.Size() > 0 {
			c.state = Execute
		}

	case Execute:
		// Extract the next micro-operation from our FIFO and execute it.
		opItem, _ := c.ops.Pop()
		op := opItem.(func(c *CPU)) // Convert FIFO generic item into proper type
		op(c)                       // Perform operation

		// When we run out of operations, go back to fetching opcodes.
		if c.ops.Size() == 0 {
			c.state = FetchOpcode
		}
	}
}

//
// CPU Instructions
//

// Custom type to save some typing and look clearer in code.
type instruction func(c *CPU)

// Same as above.
type instructionSet map[uint8]instruction

// Supported instructions, in a mapping because we're only implementing a few
// of them. In the final version, we'll use a proper 256-entry array.
// Each supported opcode is mapped to a function taking a pointer to the CPU
// and responsible for updating that CPU accordingly.
var instructions = instructionSet{
	0x04: incb,
	0x05: decb,
	0x06: ldbd8,
	0x0c: incc,
	0x0d: decc,
	0x0e: ldcd8,
	0x11: ldded16,
	0x13: incde,
	0x15: decd,
	0x16: lddd8,
	0x17: rla,
	0x18: jrr8,
	0x1a: ldade,
	0x1d: dece,
	0x1e: lded8,
	0x20: jrnzr8,
	0x21: ldhld16,
	0x22: ldhlia,
	0x23: inchl,
	0x24: inch,
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
	0x7c: ldah,
	0x90: subb,
	0xaf: xora,
	0xbe: cphl,
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
var extendedInstructions = map[uint8]instruction{
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
// separate single registers). Takes 12 ticks (4 fetching the opcode, and 4
// for loading each byte coming after the opcode).
func ldrrd16(c *CPU, high, low *uint8) {
	// Read the first byte into the lower register.
	c.ops.Push(func(c *CPU) {
		*low = c.mmu.Read(c.PC)
		c.PC++
	})

	// Read the second byte into the higher register.
	c.ops.Push(func(c *CPU) {
		*high = c.mmu.Read(c.PC)
		c.PC++
	})
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
// This takes 4 ticks to read the parameter, then it's either done if the
// condition is false, or takes another 4 ticks to update PC.
func jr(c *CPU, condition bool) {
	// Here, we want to keep the value we just read for the next micro-operation
	// so we store it in a temporary register on the CPU. This is remotely
	// similar to the W/Z registers in a Z80.
	c.ops.Push(func(c *CPU) {
		c.tmp1 = c.mmu.Read(c.PC)
		c.PC++
	})

	// Only do the actual jump if the condition was true. Takes 4 more ticks.
	if condition {
		c.ops.Push(func(c *CPU) {
			// Read() returned an unsigned 8-bit value. Casting it to a signed
			// 8-bit value does what we expect (i.e. values over 127 will
			// be converted to their signed equivalent between -128 and -1).
			// Note that casting it to an int16 directly would not work, since
			// values over 127 can still be represented by a positive number
			// when using 16 bits.
			// Now we need a cast to int16 for the potential subtraction
			// between two 16-bit values.
			c.PC = uint16(int16(c.PC) + int16(int8(c.tmp1)))
		})
	}
}

// LD r,d8 - Store the given 8-bit value in register r. Takes 8 ticks: 4 to read
// the opcode, 4 to read the value.
func ldrd8(c *CPU, reg *uint8) {
	c.ops.Push(func(c *CPU) {
		*reg = c.mmu.Read(c.PC)
		c.PC++
	})
}

// LD (HL),r - Store the value in register r to memory at address HL. Takes 8
// ticks: 4 to read the opcode, 4 to write the desired register value.
func ldhlr(c *CPU, reg uint8) {
	c.ops.Push(func(c *CPU) {
		c.mmu.Write(c.HL(), reg)
	})
}

// POP rr - Store the 16-bit value at the memory address in SP in the given
// 16-bit register. We use two 8-bit parameters instead of a single 16-bit
// value because most of the time we'll use our 8-bit registers as arguments
// anyway. Takes 8 ticks.
func pop(c *CPU, high, low *uint8) {
	c.ops.Push(func(c *CPU) {
		*low = c.mmu.Read(c.SP)
		c.SP++
	})

	c.ops.Push(func(c *CPU) {
		*high = c.mmu.Read(c.SP)
		c.SP++
	})
}

// RL r - "Rotate left through Carry". Rotate a register's value left,
// storing its former leftmost bit in the Carry flag, and setting its new
// rightmost bit to the value previously in the Carry flag.
// Essentially this shifts the concatenation of the Carry flag bit and r left,
// and wraps around. Completes within the 4 ticks needed to fetch the opcode.
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

// INC r - Increment a single register and set flags accordingly. Completes
// within the 4 ticks needed to fetch the opcode.
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

// DEC r - Decrement a single register and set flags accordingly. Completes
// within the 4 ticks needed to fetch the opcode.
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

// SUB r - Subtract value contained in register r from A.
func subr(c *CPU, value uint8) {
	c.A = cp(c, value)
}

//
// Individual instructions in the order they appear in the boot ROM.
//

// LD SP,d16 (12 ticks)
func ldspd16(c *CPU) {
	// Read the first address byte into SP's lower 8 bits. This will also
	// clear SP's higher 8 bits for the next operation.
	c.ops.Push(func(c *CPU) {
		c.SP = uint16(c.mmu.Read(c.PC))
		c.PC++
	})

	// Read the second address byte into SP's higher 8 bits.
	c.ops.Push(func(c *CPU) {
		c.SP |= uint16(c.mmu.Read(c.PC)) << 8
		c.PC++
	})
}

// XOR A (4 ticks)
func xora(c *CPU) {
	xorr(c, c.A)
}

// LD HL,d16 (12 ticks)
func ldhld16(c *CPU) {
	ldrrd16(c, &c.H, &c.L)
}

// LD (HL-),A (8 ticks)
func ldhlda(c *CPU) {
	c.ops.Push(func(c *CPU) {
		// Using getters/setters to treat HL as a single 16-bit register.
		ldaddrr(c, c.HL(), c.A)
		c.SetHL(c.HL() - 1)
	})
}

// BIT 7,H (8 ticks for fetching both opcode bytes)
func bit7h(c *CPU) {
	bitnr(c, 7, c.H)
}

// JR NZ,r8 (8 ticks if condition is false, 12 otherwise)
func jrnzr8(c *CPU) {
	jr(c, c.F&FlagZ == 0)
}

// LD C,d8 (8 ticks)
func ldcd8(c *CPU) {
	ldrd8(c, &c.C)
}

// LD A,d8 (8 ticks)
func ldad8(c *CPU) {
	ldrd8(c, &c.A)
}

// LD (FF00+C),A (8 ticks)
func ldffca(c *CPU) {
	c.ops.Push(func(c *CPU) {
		c.mmu.Write(0xff00+uint16(c.C), c.A)
	})
}

// INC C (4 ticks)
func incc(c *CPU) {
	incr(c, &c.C)
}

// LD (HL),A (8 ticks)
func ldhla(c *CPU) {
	ldhlr(c, c.A)
}

// LD (FF00+d8),A (12 ticks)
func ldffd8a(c *CPU) {
	// Read value from memory (4 ticks).
	c.ops.Push(func(c *CPU) {
		c.tmp1 = c.mmu.Read(c.PC)
		c.PC++
	})

	// Write A to computed memory address (4 ticks).
	c.ops.Push(func(c *CPU) {
		c.mmu.Write(0xff00+uint16(c.tmp1), c.A)
	})
}

// LD DE,d16 (12 ticks)
func ldded16(c *CPU) {
	ldrrd16(c, &c.D, &c.E)
}

// LD A,(DE) (8 ticks)
func ldade(c *CPU) {
	c.ops.Push(func(c *CPU) {
		c.A = c.mmu.Read(c.DE())
	})
}

// CALL d16 (24 ticks)
func call(c *CPU) {
	// We need PC to read the two address bytes following the opcode. This is
	// where we need two temporary registers.

	// Read the first (low) address byte (4 ticks).
	c.ops.Push(func(c *CPU) {
		c.tmp1 = c.mmu.Read(c.PC)
		c.PC++
	})

	// Read the second (high) address byte (4 ticks).
	c.ops.Push(func(c *CPU) {
		c.tmp2 = c.mmu.Read(c.PC)
		c.PC++
	})

	// Push current PC address (the next instruction to execute) to the stack.
	// For some reason that push operation incurs an extra 4-tick delay.
	c.ops.Push(func(c *CPU) {
		// Do nothing, the CPU will just wait an extra 4 ticks.
	})

	// Push high byte to stack.
	c.ops.Push(func(c *CPU) {
		c.SP--
		c.mmu.Write(c.SP, uint8(c.PC>>8))
	})

	// Push low byte to stack and update PC in the same operation.
	c.ops.Push(func(c *CPU) {
		c.SP--
		c.mmu.Write(c.SP, uint8(c.PC&0xff))
		c.PC = uint16(c.tmp2)<<8 | uint16(c.tmp1)
	})
}

// LD C,A (4 ticks)
func ldca(c *CPU) {
	// LD r,r is trivial enough not to need a helper function.
	c.C = c.A
}

// LD B,d8 (8 ticks)
func ldbd8(c *CPU) {
	ldrd8(c, &c.B)
}

// PUSH BC (16 ticks)
func pushbc(c *CPU) {
	// For some reason the push operation incurs an extra 4-tick delay.
	c.ops.Push(func(c *CPU) {
		// Do nothing, the CPU will just wait an extra 4 ticks.
	})

	// Push high register to stack.
	c.ops.Push(func(c *CPU) {
		c.SP--
		c.mmu.Write(c.SP, c.B)
	})

	// Push low register to stack.
	c.ops.Push(func(c *CPU) {
		c.SP--
		c.mmu.Write(c.SP, c.C)
	})
}

// RL C (8 ticks) - This is used by the boot ROM's "graphic routine" and it does
// something pretty cool that I hope to illustrate in some future article.
func rlc(c *CPU) {
	rl(c, &c.C)
}

// RLA - Distinct from RL A (same opcode, but in the extended instruction set).
// By using the one-byte instruction, this only takes 4 ticks instead of 8 for
// the RL A extended instruction.
func rla(c *CPU) {
	rl(c, &c.A)
}

// POP BC (12 ticks)
func popbc(c *CPU) {
	pop(c, &c.B, &c.C)
}

// DEC B (4 ticks)
func decb(c *CPU) {
	decr(c, &c.B)
}

// LD (HL+),A (8 ticks)
func ldhlia(c *CPU) {
	c.ops.Push(func(c *CPU) {
		ldaddrr(c, c.HL(), c.A)
		c.SetHL(c.HL() + 1)
	})
}

// INC HL (4 ticks)
func inchl(c *CPU) {
	incrr(c, &c.H, &c.L)
}

// RET (16 ticks)
func ret(c *CPU) {
	// Use tmp1 and tmp2 to simulate a POP PC of sorts.
	pop(c, &c.tmp1, &c.tmp2)

	// Update PC with the popped value.
	c.ops.Push(func(c *CPU) {
		c.PC = uint16(c.tmp1)<<8 | uint16(c.tmp2)
	})
}

// INC DE (8 ticks)
func incde(c *CPU) {
	c.ops.Push(func(c *CPU) {
		incrr(c, &c.D, &c.E)
	})
}

// LD A,E (4 ticks)
func ldae(c *CPU) {
	c.A = c.E
}

// CP d8 (8 ticks)
func cpd8(c *CPU) {
	c.ops.Push(func(c *CPU) {
		val := c.mmu.Read(c.PC)
		c.PC++
		cp(c, val) // Ignore result, we only want to set CPU flags.
	})
}

// LD (d16),A (16 ticks)
func ldd16a(c *CPU) {
	// Read high byte of destination address.
	c.ops.Push(func(c *CPU) {
		c.tmp1 = c.mmu.Read(c.PC)
		c.PC++
	})

	// Read low byte of destination address.
	c.ops.Push(func(c *CPU) {
		c.tmp2 = c.mmu.Read(c.PC)
		c.PC++
	})

	// Write A to destination address.
	c.ops.Push(func(c *CPU) {
		addr := uint16(c.tmp1) | uint16(c.tmp2)<<8
		c.mmu.Write(addr, c.A)
	})
}

// DEC A (4 ticks)
func deca(c *CPU) {
	decr(c, &c.A)
}

// JR Z,r8 (8 ticks if condition is false, 12 otherwise)
func jrzr8(c *CPU) {
	jr(c, c.F&FlagZ != 0)
}

// LD H,A (4 ticks)
func ldha(c *CPU) {
	c.H = c.A
}

// LD D,A (4 ticks)
func ldda(c *CPU) {
	c.D = c.A
}

// INC B (4 ticks)
func incb(c *CPU) {
	incr(c, &c.B)
}

// DEC C (4 ticks)
func decc(c *CPU) {
	decr(c, &c.C)
}

// LD L,d8 (8 ticks)
func ldld8(c *CPU) {
	ldrd8(c, &c.L)
}

// JR r8 (12 ticks)
func jrr8(c *CPU) {
	jr(c, true)
}

// LD E,d8 (8 ticks)
func lded8(c *CPU) {
	ldrd8(c, &c.E)
}

// LD A,(FF00+d8) (12 ticks)
func ldaffd8(c *CPU) {
	// Read value from memory (4 ticks).
	c.ops.Push(func(c *CPU) {
		c.tmp1 = c.mmu.Read(c.PC)
		c.PC++
	})

	// Read computed memory address into A (4 ticks).
	c.ops.Push(func(c *CPU) {
		c.A = c.mmu.Read(0xff00 + uint16(c.tmp1))
	})
}

// DEC E (4 ticks)
func dece(c *CPU) {
	decr(c, &c.E)
}

// INC H (4 ticks)
func inch(c *CPU) {
	incr(c, &c.H)
}

// LD A,H (4 ticks)
func ldah(c *CPU) {
	c.A = c.H
}

// SUB B (4 ticks)
func subb(c *CPU) {
	subr(c, c.B)
}

// DEC D (4 ticks)
func decd(c *CPU) {
	decr(c, &c.D)
}

// LD D,d8 (8 ticks)
func lddd8(c *CPU) {
	ldrd8(c, &c.D)
}

// CP (HL) (8 ticks)
func cphl(c *CPU) {
	c.ops.Push(func(c *CPU) {
		val := c.mmu.Read(c.HL())
		cp(c, val) // Ignore result, we only want to set CPU flags.
	})
}

////////////////////////////////////////////////////////////////////////////////
//
// Sound-related code. You'd probably want to put that in a 'sound' package.
//
////////////////////////////////////////////////////////////////////////////////

// The APU contains sound channels that look similar but not enough to easily
// group common functionality here. I'm going to try, but there may be some
// copy-pasting going on. (Sound is complicated.)

// Audio settings for SDL.
// Constant values you can tweak to see their effect on the produced sound.
const (
	SamplingRate    = 22050 // How many sample frames to send per second.
	FramesPerBuffer = 512   // Number of sample frames fitting the audio buffer.
)

// GameBoyRate is the main CPU frequence to be used in divisions.
const GameBoyRate = 4 * 1024 * 1024 // 4194304Hz or 4MiHz

// SoundOutRate represents CPU cycles to wait before producing one sample frame.
const SoundOutRate = GameBoyRate / SamplingRate

// DutyCycles represents available duty patterns. For any given frequency,
// we'll internally split one period of that frequency in 8, and for each
// of those slices, this will specify whether the signal should be on or off.
var DutyCycles = [4][8]bool{
	{false, false, false, false, false, false, false, true}, // 00000001, 12.5%
	{true, false, false, false, false, false, false, true},  // 10000001, 25%
	{true, false, false, false, false, true, true, true},    // 10000111, 50%
	{false, true, true, true, true, true, true, false},      // 01111110, 75%
}

// VolumeEnvelope structure that will act as a state machine only managing the
// current volume envelope for a Square or Noise signal generator.
type VolumeEnvelope struct {
	// The properties below can be set by the APU itself.
	Initial  uint8 // NRx2 bits 7-4
	Decrease bool  // NRx2 bit 3
	Sweep    uint8 // NRx2 bits 2-0
	Volume   uint8 // Current calculated volume.

	enabled bool
	ticks   uint // Clock ticks counter.
}

// Enable is called whenever the corresponding channel is triggered.
func (v *VolumeEnvelope) Trigger() {
	v.Volume = v.Initial
	v.enabled = true
	v.ticks = 0
}

// Tick advances the volume envelope one step. It will adjust the volume value
// every <sweep>×(1/64) seconds or <sweep>×(<cpuFreq>/64) clocks.
// Source: https://gbdev.gg8.se/wiki/articles/Sound_Controller about NR12.
func (v *VolumeEnvelope) Tick() {
	if !v.enabled {
		return
	}

	// Volume must always stay in the 0-15 range.
	if (v.Volume == 0 && v.Decrease) || (v.Volume == 15 && !v.Decrease) {
		v.enabled = false
		return
	}

	// Only update volume every <sweep>×(<cpuFreq>/64) clocks.
	v.ticks++
	if v.ticks < uint(v.Sweep)*(GameBoyRate/64) {
		return
	}
	v.ticks = 0

	if v.Decrease {
		v.Volume -= 1
	} else {
		v.Volume += 1
	}
}

// SquareWave structure implementing sound sample generation for one of the four
// possible sounds the Game Boy can produce at once. A.k.a Sound1.
type SquareWave struct {
	NR11 uint8 // Pattern duty and sound length
	NR12 uint8 // Volume envelope
	NR13 uint8 // Frequency's lower 8 bits
	NR14 uint8 // Control and frequency' higher 3 bits

	enabled bool

	// Duty-related variables.
	dutyType int  // Index into DutyCycles to know what duty type to use.
	dutyStep int  // Sub-index into DutyCycles to set the signal high or low.
	ticks    uint // Clock ticks counter for advancing duty step.

	// Volume envelope.
	envelope VolumeEnvelope
}

// SetNRx2 is called whenever the value in NR12 or NR22 was changed, so that we
// can update the volume envelope's state machine.
func (s *SquareWave) SetNRx2(value uint8) {
	s.envelope.Initial = value >> 4
	s.envelope.Decrease = (value&0x08 == 0)
	s.envelope.Sweep = value & 7
}

// SetNRx4 is called whenever the value in NR14 or NR24 was changed, so that we
// can reset the volume envelope and start sound output.
func (s *SquareWave) SetNRx4(value uint8) {
	if value&0x80 != 0 {
		s.envelope.Trigger()
		s.enabled = true
	}
}

// Tick produces a sample of the signal to generate based on the current value
// in the signal generator's registers. We use a named return value, which is
// conveniently set to zero (silence) by default.
func (s *SquareWave) Tick() (sample uint8) {
	// Only play if enabled.
	if !s.enabled {
		return
	}

	// Update volume envelope.
	s.envelope.Tick()

	// With `x` the 11-bit value in NR13/NR14, frequency is 131072/(2048-x) Hz.
	rawFreq := ((uint(s.NR14) & 7) << 8) | uint(s.NR13)
	freq := 131072 / (2048 - rawFreq)

	// Advance duty step every 1/(8f) where f is the sound's real frequency.
	if s.ticks++; s.ticks >= GameBoyRate/(freq*8) {
		s.dutyStep = (s.dutyStep + 1) % 8
		s.ticks = 0
	}

	// Use envelope's current volume (and an amplification factor).
	if DutyCycles[s.dutyType][s.dutyStep] {
		sample = s.envelope.Volume * 4
	}

	return
}

// APU structure grouping all sound signal generators and keeping track of when
// to actually output a sample for the sound card to play. For now we only use
// two generators for stereo sound, but in time, we'll mix the output of four of
// those and the stereo channel they'll go to will be configurable as well.
type APU struct {
	Registers

	Square1 SquareWave
	Square2 SquareWave

	ticks uint // Clock ticks counter for mixing samples
}

// NewAPU instantiates an APU structure and properly associates register
// addresses with the relevant structure members.
func NewAPU() *APU {
	// Pre-instantiate the APU object so we can refer to its registers.
	apu := APU{}

	// Associate addresses with the corresponding register variables.
	apu.Registers = Registers{
		0xff11: &apu.Square1.NR11,
		0xff12: &apu.Square1.NR12,
		0xff13: &apu.Square1.NR13,
		0xff14: &apu.Square1.NR14,
		0xff16: &apu.Square2.NR11,
		0xff17: &apu.Square2.NR12,
		0xff18: &apu.Square2.NR13,
		0xff19: &apu.Square2.NR14,
	}
	return &apu
}

// Write overrides Registers.Write for debugging and masking purposes.
func (a *APU) Write(addr uint16, value uint8) {
	a.Registers.Write(addr, value)
	//fmt.Printf("APU %04x=%02x\n", addr, value)

	// After writing the value, see if we must enable or update something.
	switch addr {
	case 0xff12: // NR12
		a.Square1.SetNRx2(value) // Update envelope
	case 0xff14: // NR14
		a.Square1.SetNRx4(value) // Trigger channel
	case 0xff17: // NR22
		a.Square2.SetNRx2(value) // Update envelope
	case 0xff19: // NR24
		a.Square2.SetNRx4(value) // Trigger channel
	}
}

// Tick advances the state machine of all signal generators to produce a single
// stereo sample for the sound card. This sample is only actually sent to the
// sound card at the chosen sampling rate.
func (a *APU) Tick() (left, right uint8, play bool) {
	// Advance all signal generators a step. Right now we only have two but
	// if we were to implement all four, we'd actually mix all their outputs
	// together here (with various per-generator parameters to account for).

	// For now, we'll mix everything and send the mixed audio on both channels.
	left = a.Square1.Tick() + a.Square2.Tick()
	right = left

	// We're ticking as fast as the Game Boy CPU goes, but our sound sample rate
	// is much lower than that so we only need to yield an actual sample every
	// so often.
	// Yes I'm probably missing a very obvious way to optimize this all.
	if a.ticks++; a.ticks >= SoundOutRate {
		a.ticks = 0
		play = true
	}

	return
}

////////////////////////////////////////////////////////////////////////////////
//
// Main loop running code in memory from address 0 on our CPU.
// When an unknown opcode is encountered, quit and show CPU state.
//
////////////////////////////////////////////////////////////////////////////////

// Quick and dirty gameboy structure just so we can access CPU, PPU and APU
// from the audio callback.
type GameBoy struct {
	CPU *CPU
	PPU *PPU
	APU *APU
}

// I have given up on using the callback's `data` pointer for now. We'll just
// make sure this is initialized before we enable audio playback.
var gb *GameBoy

// Audio callback function that SDL will call at a regular interval that
// should be roughly <sampling rate> / (<audio buffer size> / <channels>).
// Here SDL expects you to copy audio data from your program into the requesting
// audio buffer (buf), exactly as much as the requested length (len) which
// we know is <channels> × <sample frames per buffer>.
// Instead of copying WAV samples from an existing file, we generate WAV data
// on the fly from our APU. Ain't that cool?
//
// The comment line below is part of cgo and binds C and Go code. Do not
// modify it, not even to add a leading space. Trust me, I tried.
//
//export mainLoopCallback
func mainLoopCallback(data unsafe.Pointer, buf *C.Uint8, len C.int) {
	// We've reached the limits of the Go bindings. In order to access the
	// audio buffer, we have to jump through rather ugly conversion hoops
	// between C and Go. Note that the three lines of code below were in the
	// SDL example program. I couldn't have come up with that myself.
	n := int(len)
	hdr := reflect.SliceHeader{Data: uintptr(unsafe.Pointer(buf)), Len: n, Cap: n}
	buffer := *(*[]C.Uint8)(unsafe.Pointer(&hdr))

	// Tick everything as many times as needed to fill the audio buffer. See
	// how we now actually tick the whole emulator (CPU, PPU and APU) but only
	// update the audio buffer when the APU itself produces a sample frame.
	for i := 0; i < n; {
		gb.CPU.Tick()
		gb.PPU.Tick()
		left, right, play := gb.APU.Tick()
		if play {
			//fmt.Printf("CPU.PC=%04x\n", gb.CPU.PC)
			buffer[i] = C.Uint8(left)
			buffer[i+1] = C.Uint8(right)
			i += 2
		}
	}
}

func run() {
	// Run the main logic on the main thread so SDL resources are allocated
	// there. Keep the endless loop out of it, otherwise the current function
	// will return immediately and main() will exit.
	mainthread.Call(func() {
		boot := NewBoot("./dmg-rom.bin")     // Covers 0x0000→0x00ff and 0xff50
		ram := NewRAM(0x8000, 0xffff-0x8000) // Covers 0x8000→0xffff

		// Create window and set is as the PPU's display.
		screen := NewSDL()
		ppu := NewPPU(screen) // Covers 0xff40, 0xff42, 0xff44 and 0xff47

		// Audio Processing Unit that will generate samples.
		apu := NewAPU() // Covers 0xff11→0xff19

		// MMU looking up addresses in boot ROM or BOOT register first,
		// then in the PPU, then in RAM, then in the cartridge (if any).
		// So even if the RAM object technically contains addresses shadowing the
		// BOOT, LCDC, LY or SCY registers, the boot or ppu objects will take
		// precedence.
		mmu := MMU{[]Addressable{boot, ppu, apu, ram}}

		// If a cartridge file is given as parameter, try to load it and add it to
		// our MMU. Otherwise, the emulator will still behave as if no game was
		// inserted.
		if len(os.Args) == 2 {
			if cart := NewCartridge(os.Args[1]); cart != nil {
				mmu.Add(cart)
			}
		}

		ppu.Fetcher.mmu = &mmu
		cpu := &CPU{mmu: mmu}

		// Group all these components in a single structure that the audio callback
		// can access directly.
		gb = &GameBoy{cpu, ppu, apu}

		// With the latest CPU instructions implemented, and without an official
		// cartridge, the emulator will loop forever at the end of the boot process.
		fmt.Println("Press CTRL+C to quit...")

		// An AudioSpec structure containing our parameters. After calling
		// OpenAudio, it will also contain some values initialized by SDL itself,
		// such as the audio buffer size.
		spec := sdl.AudioSpec{
			Freq:     SamplingRate,
			Format:   sdl.AUDIO_U8,
			Channels: 2,
			Samples:  FramesPerBuffer,
			Callback: sdl.AudioCallback(C.mainLoopCallback),
		}

		// We're asking SDL to honor our parameters exactly, or fail. You can ask
		// SDL to return what it can do if it can't handle your parameters, and
		// work with that instead.
		if err := sdl.OpenAudio(&spec, nil); err != nil {
			panic(err)
		}

		// We can check that SDL allocated an audio buffer that is exactly
		// (<number of samples per buffer> × <number of channels>) bytes long.
		fmt.Printf("Values obtained from OpenAudio:\n")
		fmt.Printf("Frequency: %d Hz\n", spec.Freq)
		fmt.Printf("Channels: %d\n", spec.Channels)
		fmt.Printf("Samples per buffer: %d\n", spec.Samples)
		fmt.Printf("Buffer size: %d bytes\n\n", spec.Size)

		// Now the magic starts: we enable audio playback... and then we need to
		// wait, indefinitely. Normally, the audio callback would have some kind of
		// mechanism to detect when the program quits (such as handling the "close
		// window" event).
		sdl.Delay(1000) // Reduce crackling at startup
		sdl.PauseAudio(false)
	})

	// Do nothing here, the callback will take care of everything.
	for {
		sdl.Delay(1000)
	}
}

func main() {
	// Enable mainthread package and run in a separate goroutine.
	mainthread.Run(run)
}
