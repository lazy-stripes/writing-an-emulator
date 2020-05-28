// Quick and dirty attempt at generating any sound with SDL.

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
	tone            = 440   // "A" note at 440 Hz.
	samplingRate    = 22050 // How many sample frames to send per second.
	framesPerBuffer = 512   // Number of sample frames fitting the audio buffer.
	volume          = 127   // 50% volume for unsigned 8-bit samples.
	duration        = 3     // Desired duration of our note, in seconds.
)

// Work variables. I'm using global variables here because the program is so
// small. When we work with sound for real, we will store these in some
// structure whose address will be passed as user data each time our callback
// function is invoked.

var sampleFrames uint // Total count of sample frames sent to the sound card.
var duty uint         // Number of samples sent since last signal variation.

var isHigh = true // True for the "on" phase of our signal.
var quit bool     // Used by the callback to tell the main function to quit.

// List of timestamps (in millisecond elapsed since the beginning of the
// program's execution) at each invocation of our callback function. We'll use
// these to compute how often the callback was invoked in the end.
var ticksInCallback []int

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

	// We know we're working with 2 channels, so we have to duplicate our sample
	// value. Producing a different sound on each channel is left as an exercise
	// to the reader.
	for i := 0; i < n; i += 2 {
		if isHigh {
			buffer[i] = volume
			buffer[i+1] = volume
		} else {
			buffer[i] = 0
			buffer[i+1] = 0
		}

		// Toggle the signal's volume between "on" and "off" when we have sent
		// as many samples as there are in one duty cycle. We are using a 50%
		// duty cycle here so this is (<sampling rate> / <note frequency>) / 2
		// or <sampling rate> / (2 × <note frequency>).
		if duty++; duty >= samplingRate/(tone*2) {
			isHigh = !isHigh // Toggle our signal "on" or "off".
			duty = 0
		}
	}

	// Count sample frames to know when the desired number of seconds has
	// elapsed, then tell the main function we want to quit. Given our audio
	// buffer size, and the fact we're working with two channels, the number of
	// sample frames we just output is half the buffer size.
	sampleFrames += uint(n / 2)
	if sampleFrames >= samplingRate*duration {
		quit = true
	}

	// Store current ticks count. We'll analyze all of them later.
	ticksInCallback = append(ticksInCallback, int(sdl.GetTicks()))
}

func main() {
	// An AudioSpec structure containing our parameters. After calling
	// OpenAudio, it will also contain some values initialized by SDL itself,
	// such as the audio buffer size.
	spec := sdl.AudioSpec{
		Freq:     samplingRate,
		Format:   sdl.AUDIO_U8,
		Channels: 2,
		Samples:  framesPerBuffer,
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
	fmt.Printf("The callback was invoked %d times:\n", len(ticksInCallback))
	fmt.Println(ticksInCallback)

	// Now turn that list of increasing timestamps into a list of intervals
	// between each two consecutive values. This should yield a list of
	// numbers that are much closer in value.
	var intervals []int
	for i := 0; i < len(ticksInCallback)-1; i++ {
		intervals = append(intervals, ticksInCallback[i+1]-ticksInCallback[i])
	}
	fmt.Printf("\nIntervals between consecutive callback invocations:\n")
	fmt.Println(intervals)

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
