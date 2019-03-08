# eprisc
A RISC microprocessor architecture

This repository contains the epRISC ISA, a reference implementation, and sample code.

---

## What is it?
epRISC is a 32-bit RISC microprocessor architecture. It is designed to be easy to program for and easy to implement.

## What can it do?
Check out `documentation/isa_v5` for more information on the architecture itself. 

The reference impementation has modules for an SDRAM controller, a text-mode video card, a three-wire UART, SPI, and a fast parallel bus called sysX (which is very similar to Quad SPI). These modules are *mostly* working as of today and will synthesize without much trouble on Altera MAX 10 devices. 

The reference impementation also has epRISC code for driving all of these modules, a ROM with helper routines for things like strings and SD cards, and a machine-level monitor application. There is preliminary work being done on a Forth interpreter as well.

## How can I play with it?
Go fetch some more repos:

[spasm](http://github.com/JCLemme/spasm) is the assembler.

[flail](http://github.com/JCLemme/flail) is the emulator.

The assembler is reasonably complete and bug-free, and there are plenty of commented examples of epRISC programs in the `software` directory (although no formal spec sheet yet). The emulator is still being developed, but will work for testing most programs that don't rely on any hardware but the serial port.

For the time being, you might want to clone [this repository](http://github.com/jclemme/eprisc-open-computer) instead. I've only just started moving things to separate repositories and there are bound to be issues with uncorrected file paths.

## Media

![The most current system running a demo program](https://youtu.be/55KR43LEU_E)

![The original system showing off the monitor](https://youtu.be/6Ez0p0I0oQU)
