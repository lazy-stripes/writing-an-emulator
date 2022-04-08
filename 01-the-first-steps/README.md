So! I’ve read an enlightening blog post. I’ve set up a fresh new git repo. I’ve tracked down a dump of the Game Boy’s boot ROM. I’m pumped as fuck!

...

Now what?

# Setting the bar (low?)

The task ahead, exciting as it is, seems a little daunting at first, so let’s take a small, reasonably attainable step. Implementing a CPU from scratch isn’t so scary, there are good ways to get started with that: the Synacor Challenge is one, the Advent of Code another. It’s basically writing a loop in which we’ll map the next value in a memory array to some function, call said function, increment our index in the memory array and repeat the whole thing ad infinitum — or until you run out of memory to read from.

So my first milestone will be to write a simple command-line program that:

    Defines a CPU object representing the Game Boy
    Reads the boot ROM in memory
    Executes the first few bytes of that ROM
    Displays the current state of the CPU when it’s done

All of that could reasonably be done in one day with a single array, a mapping and a for loop. I'm only going to have to interpret 256[mfn]Actually only 200 since the ROM contains 56 bytes worth of tile data[/mfn] bytes after all. To help even further, the commented assembly code for the boot ROM is also available online, and I can easily pick out the first couple machine instructions I'll need to implement.

# All right, I can do this, let's get started

Let's get a couple things out of the way right now: I'm writing this emulator in Go because it's fun and has become my favorite language. Also because it's low-level enough for my purpose and I'll comment on this now and then. I'm also writing the code in [Visual Studio Code](https://en.wikipedia.org/wiki/Visual_Studio_Code) because it's got good support for Go — especially the debugger — and runs great on Linux. That's it, your mileage may vary.

Now, what's a CPU anyway? Structurally, it's a bunch of data (8-bit) registers to use as storage when operating on numbers and a couple address (16-bit) registers used to refer to the next instruction to execute in memory, or the bottom of the stack. This is a terrible oversimplification, but it turns out the LRxxx CPU in the Game Boy isn't much more complicated than that. At any rate, this will be a good enough definition to get started.

// A CPU implementation of the DMG-01's
type CPU struct {
        A, F   uint8
        B, C   uint8
        D, E   uint8
        H, L   uint8
        SP     uint16
        PC     uint16
}

If you haven't spent any time in your childhood reading up on [Z80](https://en.wikipedia.org/wiki/Zilog_Z80) assembly (you missed out), those letters may not look familiar. They just represent boxes into which the CPU can temporarily store 8-bit values (0 to 255) or 16-bit addresses (0 to 65535). Those pairs of 8-bit registers can also be used, in some circumstances, as four 16-bit registers (AF, BC, DE and HL) but at this stage, this isn't too relevant.

The first register we need to look at to get started is PC (for Program Counter), which will tell the CPU where to read the next instruction in memory. It conveniently starts at 0 so we already know to look at the very beginning of the boot ROM to see what is the very first CPU instruction we should implement. We could just open the boot ROM with an hexadecimal editor, see that the first byte is 0x31 and look up what CPU instruction it is, either in the [Pan Docs](http://bgb.bircd.org/pandocs.htm#cpuinstructionset) or in the [instruction set tables](http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html) available online. I found the latter to be easier to use, but the former has more information.

Either way, 0x31 is the opcode for LD SP,d16 a somewhat barbaric way to say "store (LoaD) into SP the value stored in the next two bytes in memory". Which we could have seen right away from the disassembled boot code:

LD SP,$fffe		; $0000  Setup Stack
XOR A			; $0003  Zero the memory from $8000-$9FFF (VRAM)
LD HL,$9fff		; $0004

The very first thing a Game Boy does when turned on is to set its CPU's SP register to the value 0xfffe. Easy! Obviously, I still need a tiny bit of logic to extract that 0xfffe value from memory after reading the instruction, which implies implementing memory first. I'll get there in a moment. For the time being, I'll assume that, somewhere in that infinite loop I was mentioning in the introduction, there will be a bit of code reading a little like:

for {
	opcode := memory[cpu.PC]
	cpu.PC++

	// ...
	if opcode == 0x31 {
		cpu.SP = memory[cpu.PC] | (memory[cpu.PC+1] << 8)
		cpu.PC += 2
	}
	// ...
}

Now this assumes that we have a large memory array which contains, from index 0 onwards, the individual bytes of the boot ROM code. Still, wasn't that quick? And I've already consumed three bytes from the ROM, only 197 to go! Again, it's not entirely as bad as it might sound[mfn]Provided it sounded bad to you. I won't hold it against you if it did![/mfn] since many instructions will repeat throughout the boot ROM, only with different parameters. For instance, LD SP,$fffe is very similar to the LD HL,$9fff instruction two lines below, or, a bit further down, to LD HL,$ff26. I could just implement a generic function that would take a 16-bit register as parameter and store the next two bytes in memory in it.

In fact, removing all numeric parameters from the boot ROM's assembly code, using a quick and dirty combination of grep, sort and uniq, it turns out only 49 different opcodes are used. This means I could technically run the whole boot sequence by only implementing the 48 remaining CPU instructions and never have to look at the rest of the instruction set!

ADD (HL)
BIT 7,H
CALL d16
CP d8
CP (HL)
DEC A
DEC B
DEC C
DEC D
DEC E
INC B
INC C
INC DE
INC H
INC HL
JR r8
JR NZ, r8
JR Z, r8
LD ($FF00+C),A
LD ($FF00+d8),A
LD A,($FF00+d8)
LD A,B
LD A,d8
LD A,(DE)
LD A,E
LD A,H
LD A,L
LD B,d8
LD C,A
LD C,d8
LD (d16),A
LD D,A
LD D,d8
LD DE,d16
LD E,d8
LD H,A
LD (HL),A
LD (HL+),A
LD (HL-),A
LD HL,d16
LD L,d8
LD SP,d16
POP BC
PUSH BC
RET
RLA
RL C
SUB B
XOR A

This is similar to the way those opcodes are listed on the page linked above. Here d8 represents an 8-bit parameter value read from memory just after the opcode, d16 a 16-bit parameter or address and r8 a signed 8-bit number to add to or substract from PC (for relative jumps). And you'll even notice that many of these distinct opcodes actually have a very similar purpose. All these `LD register,value` could definitely be described by even more generic functions. Let's group together all instructions working on single registers, all those working on double registers, all those working with addresses, etc. Now we're down to 25 functions, that sounds totally manageable!

ADD (address)
BIT n,r
CALL address
CP d8
CP (HL)
DEC r
INC r
INC rr
JR condition,address
LD (address),A
LD A,(address)
LD r,r
LD r,d8
LD r,(address)
LD (address),r
LD rr,d16
LD (HL+),A
LD (HL-),A
POP rr
PUSH rr
RET
RLA
RL r
SUB r
XOR r

Incidentally, this is how the CPU instruction set is organized in the Pan Docs and that definitely helps.


 (such as A for the [accumulator](https://en.wikipedia.org/wiki/Accumulator_(computing)), F for flags, H for the High byte of an address, L for the Low byte, you get it)

# Raising the bar with timings

One thing that always comes back in pretty much any resource about the Game Boy — or emulation in general — is the timing, expressed in cycles. Usually clock cycles. Or, depending on context, machine cycles. Sometimes they don’t specify and just say “cycles” and you don’t know if they mean 1 or 4 and it’s a little frustrating.

Regardless, the idea is that things in an actual Game Boy — things like reading a byte from memory, shifting a pixel out to the LCD display,
