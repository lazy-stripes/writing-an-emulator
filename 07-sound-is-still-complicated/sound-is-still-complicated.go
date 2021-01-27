// This file is provided as example code to illustrate the following article:
// https://blog.tigris.fr/2021/01/26/writing-an-emulator-sound-is-still-complicated/
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
// void squareWaveCallback(void *userdata, Uint8 *stream, int len);
import "C"

import (
	"fmt"
	"reflect"
	"unsafe"

	"github.com/veandco/go-sdl2/sdl"
)

// Constant values you can tweak to see their effect on the produced sound.
const (
	Tone            = 440   // "A" note at 440 Hz.
	SamplingRate    = 22050 // How many sample frames to send per second.
	FramesPerBuffer = 512   // Number of sample frames fitting the audio buffer.
	Volume          = 63    // 25% volume for unsigned 8-bit samples.
	Duration        = 3     // Desired duration of our note, in seconds.
)

// Work variables. I'm using global variables here because the program is so
// small. When we work with sound for real, we will store these in some
// structure whose address will be passed as user data each time our callback
// function is invoked.

var sampleFrames uint // Total count of sample frames sent to the sound card.
var quit bool         // Used by the callback to tell the main function to quit.

// List of timestamps (in millisecond elapsed since the beginning of the
// program's execution) at each invokation of our callback function. We'll use
// these to compute how often the callback was invoked in the end.
var ticksInCallback []int

// Not sure how I'm supposed to pass this to SDL. Go doesn't allow Go pointers
// use in C code but we're calling a Go callback... I'm still working on this.
var apu *APU

// Audio callback function that SDL will call at a regular interval that
// should be roughly <sampling rate> / (<audio buffer size> / <channels>).
// Here SDL expects you to copy audio data from your program into the requesting
// audio buffer (buf), exactly as much as the requested length (len) which
// we know is <channels> × <sample frames per buffer>.
// Instead of copying WAV samples from an existing file, we generate WAV data
// on the fly as a simple square wave.
//
// The comment line below is part of cgo and binds C and Go code. Do not
// modify it, not even to add a leading space. Trust me, I tried.
//
//export squareWaveCallback
func squareWaveCallback(data unsafe.Pointer, buf *C.Uint8, len C.int) {
	// We've reached the limits of the Go bindings. In order to access the
	// audio buffer, we have to jump through rather ugly conversion hoops
	// between C and Go. Note that the three lines of code below were in the
	// SDL example program. I couldn't have come up with that myself.
	n := int(len)
	hdr := reflect.SliceHeader{Data: uintptr(unsafe.Pointer(buf)), Len: n, Cap: n}
	buffer := *(*[]C.Uint8)(unsafe.Pointer(&hdr))

	// Tick the APU as many times as needed to fill the audio buffer. In the
	// end we'll actually tick the whole emulator here (CPU, PPU and APU) but
	// not every tick will produce a sample.
	for i := 0; i < n; {
		left, right, play := apu.Tick()
		if play {
			buffer[i] = C.Uint8(left)
			buffer[i+1] = C.Uint8(right)
			i += 2
		}
	}

	// Count sample frames to know when the desired number of seconds has
	// elapsed, then tell the main function we want to quit. Given our audio
	// buffer size, and the fact we're working with two channels, the number of
	// sample frames we just output is half the buffer size.
	sampleFrames += uint(n / 2)
	if sampleFrames >= SamplingRate*Duration {
		quit = true
	}

	// Store current ticks count. We'll analyze all of them later.
	ticksInCallback = append(ticksInCallback, int(sdl.GetTicks()))
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

// SquareWave structure implementing sound sample generation for one of the four
// possible sounds the Game Boy can produce at once. A.k.a Sound1.
type SquareWave struct {
	NR11 uint8 // Pattern duty and sound length
	NR12 uint8 // Volume envelope
	NR13 uint8 // Frequency's lower 8 bits
	NR14 uint8 // Control and frequency' higher 3 bits

	// Duty-related variables.
	dutyType int  // Index into DutyCycles to know what duty type to use.
	dutyStep int  // Sub-index into DutyCycles to set the signal high or low.
	ticks    uint // Clock ticks counter for advancing duty step.
}

// Tick produces a sample of the signal to generate based on the current value
// in the signal generator's registers. We use a named return value, which is
// conveniently set to zero (silence) by default.
func (s *SquareWave) Tick() (sample uint8) {
	// With `x` the 11-bit value in NR13/NR14, frequency is 131072/(2048-x) Hz.
	rawFreq := ((uint(s.NR14) & 7) << 8) | uint(s.NR13)
	freq := 131072 / (2048 - rawFreq)

	// Advance duty step every 1/(8f) where f is the sound's real frequency.
	if s.ticks++; s.ticks >= GameBoyRate/(freq*8) {
		s.dutyStep = (s.dutyStep + 1) % 8
		s.ticks = 0
	}

	// TODO: Compute volume for real. Cheat for now.
	if DutyCycles[s.dutyType][s.dutyStep] {
		sample = Volume // Arbitrary volume for testing
	}

	return
}

// APU structure grouping all sound signal generators and keeping track of when
// to actually output a sample for the sound card to play. For now we only use
// two generators for stereo sound, but in time, we'll mix the output of four of
// those and the stereo channel they'll go to will be configurable as well.
type APU struct {
	ToneSweep1 SquareWave
	ToneSweep2 SquareWave

	// We could add even more generators for the fun of it!
	ToneSweep3 SquareWave

	ticks uint // Clock ticks counter for mixing samples
}

// Tick advances the state machine of all signal generators to produce a single
// stereo sample for the sound card. This sample is only actually sent to the
// sound card at the chosen sampling rate.
func (a *APU) Tick() (left, right uint8, play bool) {
	// Advance all signal generators a step. Right now we only have two but
	// if we were to implement all four, we'd actually mix all their outputs
	// together here (with various per-generator parameters to account for).

	// Here we also cheat a bit and force generator1 output to the left channel
	// and generator2 to the right.
	//left = a.ToneSweep1.Tick()
	//right = a.ToneSweep2.Tick()

	// Another way to use the generated sound is to mix it and send the result
	// to both stereo channels. I'm pretty sure there are limitations to this
	// but in the mean time, we could also do this:
	left = a.ToneSweep1.Tick() + a.ToneSweep2.Tick() + a.ToneSweep3.Tick()
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

func main() {
	// An AudioSpec structure containing our parameters. After calling
	// OpenAudio, it will also contain some values initialized by SDL itself,
	// such as the audio buffer size.
	spec := sdl.AudioSpec{
		Freq:     SamplingRate,
		Format:   sdl.AUDIO_U8,
		Channels: 2,
		Samples:  FramesPerBuffer,
		Callback: sdl.AudioCallback(C.squareWaveCallback),
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

	// Use global variable for the callback for now. What I want to do may not
	// be possible here. I have a plan B. It's fine.
	apu = &APU{}

	// Simulate sound being enabled. Again, I only want to see if sound comes
	// out for now. We need to store our tone's frequency as 131072/(2048-x)
	// in 11-bits. Tone obviously cannot be zero. We can also pick a duty type
	// (default is 12.5%).

	// Output the tone defined in our constants to the left channel.
	toneLeft := Tone
	rawFreq := (2048 * (toneLeft - 64)) / toneLeft

	// Lowest 8 bits of calculated frequency in NR13.
	apu.ToneSweep1.NR13 = uint8(rawFreq & 0x00ff)

	// Highest 3 bits of calculated frequency. Note that bit 6 is still zero,
	// so we're not using NR11 to control the length of that note. This won't
	// really matter until next article.
	apu.ToneSweep1.NR14 = uint8(rawFreq & 0x0700 >> 8)
	apu.ToneSweep1.dutyType = 2 // 50% duty

	// Output tone/3 to the right channel. This should sound okay. You can set
	// the new tone to the original toner ×2 or divide it by 2, or play around
	// with other values, change the duty type, etc.
	// I have to admit that when I finally got to this point I gleefully tried
	// many truly horrific combinations.
	toneRight := toneLeft / 3
	rawFreq = (2048 * (toneRight - 64)) / toneRight
	apu.ToneSweep2.NR13 = uint8(rawFreq & 0x00ff)
	apu.ToneSweep2.NR14 = uint8(rawFreq & 0x0700 >> 8)
	apu.ToneSweep2.dutyType = 2

	// Why stop here? We can add yet another harmonic if we feel like it! Even
	// use a different duty pattern.
	toneExtra := toneLeft / 4
	rawFreq = (2048 * (toneExtra - 64)) / toneExtra
	apu.ToneSweep3.NR13 = uint8(rawFreq & 0x00ff)
	apu.ToneSweep3.NR14 = uint8(rawFreq & 0x0700 >> 8)
	apu.ToneSweep3.dutyType = 3

	// Start playing sound. Not sure why we un-pause it instead of starting it.
	sdl.PauseAudio(false)

	// Run an endless loop with a tiny delay in it to avoid hogging 100% of CPU
	// time, until we detect from the audio callback that we've been running
	// for the required amount of time.
	for !quit { // The `quit` variable is set in our callback.
		sdl.Delay(10)
	}
	sdl.CloseAudio()

	// Just to get a feel for it, print all recorded timestamp values. They
	// should be incremental and close to one another.
	fmt.Printf("The callback was invoked %d times\n", len(ticksInCallback))

	// Now turn that list of increasing timestamps into a list of intervals
	// between each two consecutive values. This should yield a list of
	// numbers that are much closer in value.
	var intervals []int
	for i := 0; i < len(ticksInCallback)-1; i++ {
		intervals = append(intervals, ticksInCallback[i+1]-ticksInCallback[i])
	}

	// And finally compute the average value for those intervals (I've only
	// computed the arithmetic mean, further analysis is left as an exercise to
	// the reader.)
	average := 0
	for _, interval := range intervals {
		average += interval
	}

	average /= len(intervals)

	fmt.Printf("\nAverage interval: %dms\n", average)
}
