; epRISC Interactive Firmware
; copyright 2018 by John C Lemme, jclemme (at) proportionallabs (dot) com

; Layout of the dictionary
; Word 0    Defined[0Empty,1Code,2Variable,3Constant],Precedence[0Norm,1Immd],Pad[2bit],NameSize[1byte]
; Word 1..n Packed name, zero padded
; Word n+1  Next definition
; Word n+2  Code pointer
; Word n+3  Data...n

; Layout of memory
; 0000      Interpreter
; 1000      Data stack
; 1200      Return stack
; 1400      Input buffer
; 1600      Word buffer
; 1700      Call stack
; 1800      Current base
; 1801      Current word address
; 1802      Current word length
; 1803      ?
; 1804      --Reserved--
; 2000      Beginning of dictionary
; 4000      Beginning of heap

; Layout of registers

; Xw        Return stack pointer (RSP)
; Xw        Data stack pointer (DSP)
; Xx        Data DSP
; Xy        Data DSP+1
;
; Yw        Input address
; Yx        Num of characters entered
; Yy        Word address
; Yz        Word that we're working with

; Zw        Temporary (Temp)
; Zx        Character in word

; =============
; Instruction Pointer and Entry

!ip             h00004000
!def            BUS_BASE_ADDRESS    h2000

:entry          brch.a  a:main

; =============
; BIOS Routines

!include    "../rom/bios_bus.asm"
!include    "../rom/bios_uart_fast.asm"
!include    "../rom/bios_spi.asm"
!include    "../rom/bios_string.asm"
!include    "../rom/bios_sdcard.asm"
!include    "../rom/bios_debug.asm"

; =============
; Defines

!def            DSP         Xw          
!def            RSP         Xx   
!def            StackWorkA  Xy
!def            StackWorkB  Xz

!def            InptAddr    Yw          
!def            InptOffs    Yx          
!def            DictAddr    Yy
!def            HeapAddr    Yz

!def            Temp        Zw  
!def            AuxOutput   Zy        
!def            Output      Zz   

!def            MemBase     1800
!def            MemLastAddr 1801
!def            MemLastLen  1802
       
; =============
; Strings

!zone           ifirm_str

:.welcome       !str "epRISC Interactive Firmware v0.3\n\n"
:.okprompt      !str "ok\n"
:.tmpmatch      !str "match "
:.wordhit       !str "Word "

!zone           ifirm_errstr

:.setleft       !str ">>>>"
:.setright      !str "<<<<\n"
:.setstack      !str "Current stack ([dsp-2], [dsp-1], [dsp]):\n"

:.generic       !str "Generic/undefined error"
:.overflow      !str "Stack overflow"
:.underflow     !str "Stack underflow"
:.undefined     !str "Undefined word"

:.lookup        !data $ifirm_errstr.generic $ifirm_errstr.overflow $ifirm_errstr.underflow $ifirm_errstr.undefined


; =============
; Subroutine - Push Number to Data Stack
; Pushes a number to the data stack. Uses %AuxOutput for parameters - maybe bad design?

:sub_pushds     stor.o  s:%AuxOutput r:%DSP o:#h00                  ; Save data to the stack
                addr.v  d:%DSP a:%DSP v:#h01                        ; Increment DSP
                rtrn.s                                              ; Return

; =============
; Subroutine - Pop Number from Data Stack
; Pops a number from the data stack. Outputs to %Output

:sub_popsds     subr.v  d:%DSP a:%DSP v:#h01                        ; Decrement DSP

                cmpr.v  a:%DSP v:#h10 s:#h04    
                brch.a  c:%LST a:.underflowd                        ; Report underflow errors
                
                load.o  d:%Output r:%DSP o:#h00                     ; Load data from the stack
                rtrn.s                                              ; Return
                
:.underflowd    addr.v  d:%DSP a:%DSP v:#h01                        ; Fix stack
                move.v  d:%Temp v:#h02
                push.r  s:%Temp
                call.s  a:sub_callerror                             ; Report the failure

; =============
; Subroutine - Push Number to Return Stack
; Pushes a number to the return stack. Uses %AuxOutput for parameters - maybe bad design?

:sub_pushrs     stor.o  s:%AuxOutput r:%RSP o:#h00                  ; Save data to the stack
                addr.v  d:%RSP a:%RSP v:#h01                        ; Increment RSP
                rtrn.s                                              ; Return

; =============
; Subroutine - Pop Number from Return Stack
; Pops a number from the return stack. Outputs to %Output

:sub_popsrs     subr.v  d:%RSP a:%RSP v:#h01                        ; Decrement RSP

                cmpr.v  a:%RSP v:#h12 s:#h04    
                brch.a  c:%LST a:.underflowd                        ; Report underflow errors
                
                load.o  d:%Output r:%RSP o:#h00                     ; Load data from the stack
                rtrn.s                                              ; Return
                
:.underflowr    addr.v  d:%RSP a:%RSP v:#h01                        ; Fix stack
                move.v  d:%Temp v:#h02
                push.r  s:%Temp
                call.s  a:sub_callerror                             ; Report the failure
                
; =============
; Subroutine - Tokenizer
; Extracts the first token from a string, breaking on the first instance of the token or a null terminator
         
!zone           ifirm_sub_tokenize
!def            CurrAddr    Yw
!def            TokenBrk    Yx
!def            StrLen      Zy
!def            StrAddr     Zz

:sub_tokenize   push.r  s:%CurrAddr
                push.r  s:%TokenBrk
                subr.v  d:%SP a:%SP v:#h03
                pops.r  d:%TokenBrk
                pops.r  d:%CurrAddr                                 
                addr.v  d:%SP a:%SP v:#h05                          ; Grab parameters from the stack
                
                move.v  d:%StrLen v:#h00                            ; The string has no length right now
                move.v  d:%StrAddr v:#h00                           ; And it doesn't exist in memory yet
                
:.leadloop      load.o  d:%Temp r:%CurrAddr                         ; Get the character

                cmpr.r  a:%Temp b:%TokenBrk                         ; Is it the token character?
                brch.a  c:%NEQ a:.subword                           ; If not, exit
                
                cmpr.v  a:%Temp v:#h00                              ; Is it the null terminator?
                brch.a  c:%EQL a:.cleanup                           ; If so, there isn't a word in here at all and we should leave 
                
                addr.v  d:%CurrAddr a:%CurrAddr v:#h01             
                brch.a  a:.leadloop                                 ; Else, keep on truckin
                
:.subword       move.r  d:%StrAddr s:%CurrAddr                      ; This character is the first character in the string
                
:.wordloop      load.o  d:%Temp r:%CurrAddr                         ; Get the character

                cmpr.r  a:%Temp b:%TokenBrk                         ; Is it the token character?
                brch.a  c:%EQL a:.tokenfound                        ; If so, exit
                
                cmpr.v  a:%Temp v:#h00                              ; Is it the null terminator?
                brch.a  c:%EQL a:.tokenfound                        ; If so, behave like you found the token character
                
                addr.v  d:%CurrAddr a:%CurrAddr v:#h01             
                addr.v  d:%StrLen a:%StrLen v:#h01                  ; The string is ONE CHARACTER LONGER
                brch.a  a:.wordloop                                 ; Else, keep on truckin
                
:.tokenfound    ; I don't remember if we need to do something here, so I'll leave the label
                
:.cleanup       pops.r  d:%TokenBrk
                pops.r  d:%CurrAddr
                rtrn.s                                              ; Restore and exit

; =============
; Subroutine - Dictionary Lookup
; Finds a dictionary entry that matches a given name

!zone           ifirm_sub_dictlookup
!def            StrAddr     Yw
!def            StrLen      Yx
!def            DictAddr    Yy
!def            EntryLen    Zy
!def            EntryAddr   Zz

:sub_dictlookup push.r  s:%StrAddr
                push.r  s:%StrLen
                subr.v  d:%SP a:%SP v:#h03
                pops.r  d:%StrLen
                pops.r  d:%StrAddr                                 
                addr.v  d:%SP a:%SP v:#h05                          ; Grab parameters from the stack
        
                move.v  d:%DictAddr v:#h6000                        ; Reset the dictionary pointer
                move.v  d:%HeapAddr v:#h7000                        ; Reset the heap pointer
                
:.searchloop    load.o  d:%Temp r:%DictAddr                         ; Get the description word of the entry
                
                cmpr.v  d:%Temp a:%Temp v:#h00                      ; Is the word zero?
                brch.a  c:%EQL a:.noentry                           ; If so, we ran out of entries
                
                masr.v  d:%Temp a:%Temp v:#hFF s:#h0A               ; Extract the word length
                
                cmpr.r  a:%StrLen b:%Temp                           ; Do the word lengths match?
                brch.a  c:%NEQ a:.nolength                          ; If not, loop down a few
                
                push.r  s:%StrAddr
                push.r  s:%Temp
                push.r  s:%DictAddr                                 ; This will save us from hard math later on
                
                addr.v  d:%DictAddr a:%DictAddr v:#h01              ; String begins one word after definition
                
:.nameloop      push.r  s:%DictAddr
                push.r  s:%StrAddr
                
                load.o  d:%DictAddr r:%DictAddr
                load.o  d:%StrAddr r:%StrAddr                       ; A particularly stupid way of saving a couple of registers

                cmpr.r  a:%DictAddr b:%StrAddr                      ; Match?
                brch.a  c:%NEQ a:.nomatch                           ; If not, abort
                
                pops.r  d:%StrAddr
                pops.r  d:%DictAddr
                
                addr.v  d:%StrAddr a:%StrAddr v:#h01
                addr.v  d:%DictAddr a:%DictAddr v:#h01
                subr.v  d:%Temp a:%Temp v:#h01                      ; Advance da pointers
                
                cmpr.v  a:%Temp v:#h00
                brch.a  c:%NEQ a:.nameloop                          ; Loop back up if that word matched
                
:.match         pops.r  d:%EntryAddr
                pops.r  d:%EntryLen
                pops.r  d:%StrAddr                                  ; Combo of cleanup and saving the dictionary entry
                brch.a  a:.cleanup
                
:.nomatch       pops.r  d:%StrAddr
                pops.r  d:%DictAddr                                 ; Cleanup
                
                pops.r  d:%DictAddr
                pops.r  d:%Temp
                pops.r  d:%StrAddr                                  ; Restore the variables we saved
                
:.nolength      addr.v  d:%Temp a:%Temp v:#h01
                addr.r  d:%DictAddr a:%DictAddr b:%Temp
                load.o  d:%DictAddr r:%DictAddr                     ; Get the address of the next entry
                brch.a  a:.searchloop

:.noentry       move.v  d:%EntryAddr v:#h00
                move.v  d:%EntryLen v:#h00                          ; There was no match, so set everything to zero
                
:.cleanup       pops.r  d:%StrLen
                pops.r  d:%StrAddr
                rtrn.s                                              ; Restore and exit
                
; =============
; Subroutine - Number Converter
; Converts a Forth-style number into a value

!zone           ifirm_sub_numconvert
!def            StrAddr     Yw
!def            StrLen      Yx
!def            Base        Yy
!def            Status      Zy
!def            Value       Zz

:sub_numconvert push.r  s:%StrAddr
                push.r  s:%StrLen
                push.r  s:%Base
                subr.v  d:%SP a:%SP v:#h04
                pops.r  d:%StrLen
                pops.r  d:%StrAddr                                 
                addr.v  d:%SP a:%SP v:#h06                          ; Grab parameters from the stack
                
                move.v  d:%Base v:#h0A                              ; The default base is 10
                move.v  d:%Status v:#h00                            ; Clear the status register
                
:.modloop       load.o  d:%Temp r:%StrAddr                          ; Load the character

:.chkpremature  cmpr.v  a:%Temp v:#h00                              ; Is it empty?
                brch.a  c:%NEQ a:.chknegative                       ; If not, loop down
                brch.a  a:.numberfail                               ; Abort - there is no number here
                
:.chknegative   cmpr.v  a:%Temp v:#c'-'                             ; Is it negative?
                brch.a  c:%NEQ a:.chkpound                          ; If not, loop down
                orbt.v  d:%Status a:%Status v:#h01                  ; Record that the value should be negative
                brch.a  a:.nextmod                                  ; Move on
                
:.chkpound      cmpr.v  a:%Temp v:#c'#'                             ; Is it decimal?
                brch.a  c:%NEQ a:.chkampersand                      ; If not, loop down
                move.v  d:%Base v:#h0A                              ; Set the base to 10
                brch.a  a:.nextmod                                  ; Move on
                
:.chkampersand  cmpr.v  a:%Temp v:#c'&'                             ; Is it decimal?
                brch.a  c:%NEQ a:.chkdollar                         ; If not, loop down
                move.v  d:%Base v:#h0A                              ; Set the base to 10
                brch.a  a:.nextmod                                  ; Move on
                
:.chkdollar     cmpr.v  a:%Temp v:#c'$'                             ; Is it hex?
                brch.a  c:%NEQ a:.chkpercent                        ; If not, loop down
                move.v  d:%Base v:#h10                              ; Set the base to 16
                brch.a  a:.nextmod                                  ; Move on
                
:.chkpercent    cmpr.v  a:%Temp v:#c'%'                             ; Is it binary?
                brch.a  c:%NEQ a:.itsanumber                        ; If not, loop down
                move.v  d:%Base v:#h02                              ; Set the base to 2
               
:.nextmod       addr.v  d:%StrAddr a:%StrAddr v:#h01
                subr.v  d:%StrLen a:%StrLen v:#h01                  ; Next character please
                
                cmpr.v  a:%StrLen v:#h00                            ; Are we out of characters?
                brch.a  c:%NEQ a:.modloop                           ; If not, keep looking
                
:.itsanumber    move.v  d:%Value v:#h00                             ; Clear the accumulator
                
:.numberloop    load.o  d:%Temp r:%StrAddr                          ; Load the character
                
                cmpr.v  a:%Temp v:#h00                              ; Is it empty?
                brch.a  c:%NEQ a:.notfinished                       ; If not, loop down
                brch.a  a:.finish                                   ; Return the number since we're done here
                
:.notfinished   orbt.v  d:%Temp a:%Temp v:#h20
                subr.v  d:%Temp a:%Temp v:#h30                      ; Force lowercase and shrink down to number range
                
                cmpr.v  a:%Temp v:#h0A                              ; Is the digit part of the alphabet?
                brch.a  c:%LST a:.checkrange                        ; If not, skip this next bit
                subr.v  d:%Temp a:%Temp v:#h27                      ; Drop the ASCII lowercase alphabet range (starting at 0x61) to 0xA 
                      
:.checkrange    cmpr.r  a:%Temp b:%Base                             ; Is the number outside of the acceptable range for the base?
                brch.a  c:%GET a:.numberfail                        ; If so, abort
                
                push.r  s:%Base                                     ; Make a backup of the base
                push.r  s:%StrLen                                   ; Need another register
                move.v  d:%StrLen v:#h00                            ; (Our temporary accumulator)
                
:.multiply      addr.r  d:%StrLen a:%StrLen b:%Value                ; Add another value
                subr.v  d:%Base a:%Base v:#h01                      ; This method doesn't work for base == 0, but that doesn't exist so whatever
                cmpr.v  a:%Base v:#h00                              ; Done?
                brch.a  c:%NEQ a:.multiply                          ; If not, better keep going
                
                move.r  d:%Value s:%StrLen                          ; Save the multiplied value
                pops.r  d:%StrLen
                pops.r  d:%Base                                     ; And restore working registers
                
                addr.r  d:%Value a:%Value b:%Temp                   ; Add the last place
                
:.nextdigit     addr.v  d:%StrAddr a:%StrAddr v:#h01
                subr.v  d:%StrLen a:%StrLen v:#h01                  ; Next character please
                
                cmpr.v  a:%StrLen v:#h00                            ; Are we out of characters?
                brch.a  c:%NEQ a:.numberloop                        ; If not, keep looking
                
:.finish        cmpr.v  a:%Status v:#h01                            ; Is the number negative?
                brch.a  c:%NEQ a:.finishstat                        ; If not, skip the next thing
                notb.r  d:%Value a:%Value
                addr.v  d:%Value a:%Value v:#h01                    ; Make it negative
:.finishstat    move.v  d:%Status v:#h00                            ; Clear the status register
                brch.a  a:.cleanup                                  ; And exit
                
:.numberfail    move.v  d:%Status v:#hFF                            ; A positive status indicates that the string isn't a number
                                  
:.cleanup       pops.r  d:%Base
                pops.r  d:%StrLen
                pops.r  d:%StrAddr
                rtrn.s                                              ; Restore and exit
                
; =============
; Subroutine - Number Output
; Outputs a number in the current base.

!zone           ifirm_sub_numoutput
!def            Value       Yw
!def            Base        Yy
!def            Divided     Zy
!def            Records     Zz

:sub_numoutput  push.r  s:%Value
                push.r  s:%Base
                subr.v  d:%SP a:%SP v:#h03
                pops.r  d:%Value                                 
                addr.v  d:%SP a:%SP v:#h04                          ; Grab parameters from the stack
                
                move.v  d:%Base v:#h0A                              ; Just decimal for right now
                move.v  d:%Records v:#h00                           ; Print no characters yet
                
                cmpr.v  a:%Value v:#h00                             ; Is the value negative?
                brch.a  c:%GET a:.calcloop                          ; If not, keep on goin
                
                notb.r  d:%Value a:%Value
                addr.v  d:%Value a:%Value v:#h01                    ; Make positive
                
                move.v  d:%Records v:#c'-'
                push.r  s:%Records
                move.v  d:%Records v:#h01                           ; Quick and dirty negative hack
                
:.calcloop      move.v  d:%Divided v:#h00                           ; Clear divided count

:.divideloop    cmpr.r  a:%Value b:%Base                            ; Is there a remainder?
                brch.a  c:%LST a:.exitdivide                        ; Not yet? Then keep it up
                addr.v  d:%Divided a:%Divided v:#h01                ; Record the new number
                subr.r  d:%Value a:%Value b:%Base                   ; Subtract out a base
                brch.a  a:.divideloop                               ; Loop it
                
:.exitdivide    push.r  s:%Value                                    ; Save the digit
                move.r  d:%Value s:%Divided                         ; Keep the transfer
                addr.v  d:%Records a:%Records v:#h01                ; And note how many digits we kept
                
                cmpr.v  a:%Value v:#h00                             ; Are there more digits to copy?
                brch.a  c:%NEQ a:.calcloop                          ; If so, keep chuggin
                
:.printloop     pops.r  d:%Value                                    ; Get the digit back
                addr.v  d:%Value a:%Value v:#h30                    ; Push it up to ASCII digit range
                
                cmpr.v  a:%Value v:#h3A                             ; Does this need to be a letter?
                brch.a  c:%LST a:.display                           ; If not, don't make it a letter
                addr.v  d:%Value a:%Value v:#h07                    ; Push it a little higher to ASCII uppercase range
                
:.display       push.r  s:%Records
                push.r  s:%Value
                call.s  a:ser_send
                pops.r  d:%Value                                    
                pops.r  d:%Records                                  ; Print the character, keeping in mind that Records will be zapped
                
                subr.v  d:%Records a:%Records v:#h01
                cmpr.v  a:%Records v:#h00                           ; We done?
                brch.a  c:%NEQ a:.printloop                         ; If not, keep going
                
                move.v  d:%Value v:#h20
                push.r  s:%Value
                call.s  a:ser_send
                pops.r  d:%Value                                    ; Print the extra space
                
:.cleanup       pops.r  d:%Base
                pops.r  d:%Value
                rtrn.s                                              ; Restore and exit
              
; =============
; Subroutine - Error Display
; Displays an error message and the current top of stack. Returns to interpreter, aborting execution.

!zone           ifirm_sub_callerror
!def            WordAddr    Yw
!def            WordLen     Yx
!def            TypeError   Yy
!def            AuxOutput   Zy
!def            Output      Zz

:sub_callerror  subr.v  d:%SP a:%SP v:#h01
                pops.r  d:%TypeError
                addr.v  d:%SP a:%SP v:#h02                          ; Grab parameters from the stack. No need to save them
                
                move.v  d:%Temp v:#h0A  
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print '\n'
                
                ; STEP ONE - Print the error that was thrown
                
                move.v  d:%Temp v:#h3A
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print ':'
                
                move.v  d:%Temp v:#h20
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print ' '
                
                push.r  s:%TypeError
                call.s  a:sub_numoutput
                pops.r  d:%TypeError                                ; Print the error number
                
                move.v  d:%Temp v:#h3A
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print ':'
                
                move.v  d:%Temp v:#h20
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print ' '
                
                move.v  d:%Temp v:ifirm_errstr.lookup
                addr.r  d:%Temp a:%Temp b:%TypeError
                load.o  d:%Temp  r:%Temp                            ; Grab address of string
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; And print the error
                
                move.v  d:%Temp v:#h0A  
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print '\n'
                
                ; STEP TWO - Print the word that fucked us
                
                move.v  d:%Temp v:ifirm_errstr.setleft
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; Print ">>>>"
                
                load.o  d:%WordAddr r:%GL o:#hMemLastAddr
                load.o  d:%WordLen r:%GL o:#hMemLastLen             ; Get the name of the word
                
:.nameloop      load.o  d:%Temp r:%WordAddr 
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp
                addr.v  d:%WordAddr a:%WordAddr v:#h01
                subr.v  d:%WordLen a:%WordLen v:#h01
                cmpr.v  a:%WordLen v:#h00
                brch.a  c:%NEQ a:.nameloop                          ; Print the bad word
                
                move.v  d:%Temp v:ifirm_errstr.setright
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; Print "<<<<\n"

                ; STEP THREE - Print the stack dump. This is badness

                move.v  d:%Temp v:ifirm_errstr.setstack
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; Print stack layout message
                
                subr.v  d:%Temp a:%DSP v:#h03
                load.o  d:%Temp r:%Temp
                push.r  s:%Temp
                call.s  a:sub_numoutput
                pops.r  d:%Temp                                     ; DSP - 2
                
                subr.v  d:%Temp a:%DSP v:#h02
                load.o  d:%Temp r:%Temp
                push.r  s:%Temp
                call.s  a:sub_numoutput
                pops.r  d:%Temp                                     ; DSP - 1
                
                subr.v  d:%Temp a:%DSP v:#h01
                load.o  d:%Temp r:%Temp
                push.r  s:%Temp
                call.s  a:sub_numoutput
                pops.r  d:%Temp                                     ; DSP
                
                move.v  d:%Temp v:#h0A  
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print '\n'
                
                brch.a  a:errreentry                                ; Branch back to the interpreter
           
                
                
; =============
; Machine Setup
; Initializes the machine. Not necessary when running as a program but good to have anyway

!zone           ifirm_code

:main           move.v  d:%GL v:#h00                                ; The global register is mostly used for branches
                move.v  d:%SP v:#h1700                              ; Set up the call stack
                move.v  d:%RSP v:#h1200                             ; Set up the return stack
                move.v  d:%DSP v:#h1000                             ; Set up the data stack
                move.v  d:%InptAddr v:#h1400                        ; Set up the command buffer
                move.v  d:%InptOffs v:#00                           ; Set up the command buffer offset
                
                move.v  d:%Temp v:#h0A
                stor.o  s:%Temp r:%GL o:#hMemBase                   ; Store the default numerical base - decimal
                
                call.s  a:ioc_init
                move.v  d:%Temp v:#h7FFF
                push.r  s:%Temp
                move.v  d:%Temp v:#h00
                push.r  s:%Temp
                call.s  a:ioc_send
                pops.r  d:%Temp
                pops.r  d:%Temp                                     ; Reset the I/O controller
                
:.welcome       move.v  d:%Temp v:ifirm_str.welcome
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; Print the welcome message
       
                brch.a  a:cmdentry                                  ; Jump down to the command handler
                
; =============
; Command Buffer
; Gets commands from the user
         
!zone           ifirm_cmdbuffer

:errreentry     move.v  d:%SP v:#h1700                              ; Set up the call stack

:cmdreentry     move.v  d:%InptAddr v:#h1400                        ; Reset the input pointer

                move.v  d:%Temp v:ifirm_str.okprompt
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; Print the "ok" prompt
                
:cmdentry       move.v  d:%InptOffs v:#00                           ; Set up the command buffer offset
                
:.cmdloop       call.s  a:ser_srcv                                  ; Get a character from the terminal
                
                cmpr.v  a:%Output v:#h08                            ; Is character a backspace?
                brch.a  c:%NEQ a:.notback                           ; If not, loop down a few
                
                addr.v  d:%InptOffs a:%InptOffs v:#h01              ; Decrement the buffer offset
                
                move.v  d:%Output v:#h08    
                push.r  s:%Output
                call.s  a:ser_send
                pops.r  d:%Output                                   ; Echo a backspace
                
                brch.a  a:.cmdloop                                  ; And loop back around
                
:.notback       cmpr.v  a:%Output v:#h0D                            ; Is character an enter?
                brch.a  c:%NEQ a:.notenter                          ; If not, loop down a few
                
                addr.r  d:%Temp a:%InptAddr b:%InptOffs
                stor.o  s:%GL r:%Temp                               ; Store a zero
                
                move.v  d:%Temp v:#h20
                push.r  s:%Temp
                call.s  a:ser_send
                pops.r  d:%Temp                                     ; Print the "I'm processing" space

                brch.a  a:intpentry                                 ; And jump to the interpreter
                
:.notenter      cmpr.v  a:%InptOffs v:#h10 s:#h03                   ; Is the command buffer full?
                brch.a  c:%LET a:.notfull                           ; If not, loop down a few

                brch.a  a:.cmdloop                                  ; Loop back around, since we can't do anything anymore
                
:.notfull       addr.r  d:%Temp a:%InptAddr b:%InptOffs
                stor.o  s:%Output r:%Temp                           ; Store the character
                addr.v  d:%InptOffs a:%InptOffs v:#h01              ; Increment the buffer offset
                
                push.r  s:%Output
                call.s  a:ser_send
                pops.r  d:%Output                                   ; Echo the character
                
                brch.a  a:.cmdloop                                  ; And loop back around

; =============
; Interpreter 
; Starts executing the provided words

!zone           ifirm_interpreter

:intpentry      move.v  d:%InptAddr v:#h1400                        ; Reset the input pointer
                move.v  d:%DictAddr v:#h6000                        ; Reset the dictionary pointer

:.runloop       push.r  s:%InptAddr
                move.v  d:%Temp v:#h20
                push.r  s:%Temp
                call.s  a:sub_tokenize                              
                pops.r  d:%Temp                                     
                pops.r  d:%Temp                                     ; Get a space-delimited string from the input     
                
                cmpr.v  a:%AuxOutput v:#h00                         ; Is the string's length zero?
                brch.a  c:%EQL a:.runfinish                         ; If so, there are no more words to execute
                
                push.r  s:%Output
                move.v  d:%Temp v:ifirm_str.wordhit
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; Print the hit message
                pops.r  d:%Output                                   ; Keep in mind that other routines generally clobber Zz
                
                stor.o  s:%AuxOutput r:%GL o:#hMemLastLen
                stor.o  s:%Output r:%GL o:#hMemLastAddr             ; Store the last word called
                
                push.r  s:%AuxOutput
                push.r  s:%Output                                   ; Save these so we know where to go next
                
                push.r  s:%Output   
                push.r  s:%AuxOutput
                call.s  a:sub_dictlookup
                pops.r  d:%Temp
                pops.r  d:%Temp                                     ; See if there's a dictionary entry matching the word
                
                cmpr.v  a:%Output v:#h00                            ; Is the entry's address zero?
                brch.a  c:%EQL a:.notfound                          ; If so, the word is invalid
                
                addr.v  d:%AuxOutput a:%AuxOutput v:#h02
                addr.r  d:%Output a:%Output b:%AuxOutput            ; Calculate the code address
                load.o  d:%Output r:%Output                         ; Load it
                
                push.r  s:%Output
                move.v  d:%Temp v:ifirm_str.tmpmatch
                push.r  s:%Temp
                call.s  a:str_puts
                pops.r  d:%Temp                                     ; Print the error message
                pops.r  d:%Output
                
                brch.o  r:%Output l:%SVSK                           ; And run the word
                
                pops.r  d:%Output
                pops.r  d:%AuxOutput                                ; Restore the buffer pointer and length
                
                addr.r  d:%InptAddr a:%Output b:%AuxOutput          ; Move the buffer pointer to the next word
                brch.a  a:.runloop                                  ; And loop back up to run another word
                
:.notfound      pops.r  d:%Output
                pops.r  d:%AuxOutput                                ; Clear the stack
                
                push.r  s:%AuxOutput
                push.r  s:%Output                                   ; Save these so we know where to go next
                
                push.r  s:%Output
                push.r  s:%AuxOutput                                
                call.s  a:sub_numconvert
                pops.r  d:%Temp
                pops.r  d:%Temp                                     ; See if the result was a number
                
                cmpr.v  a:%AuxOutput v:#h00                         ; Was it a number
                brch.a  c:%NEQ a:.notnumber                         ; If not, abort
                
                move.r  d:%AuxOutput s:%Output
                call.s  a:sub_pushds                                ; Onto the stack it goes
                
                pops.r  d:%Output
                pops.r  d:%AuxOutput                                ; Restore the buffer pointer and length
                
                addr.r  d:%InptAddr a:%Output b:%AuxOutput          ; Move the buffer pointer to the next word
                brch.a  a:.runloop
                
:.notnumber     move.v  d:%Temp v:#h03
                push.r  s:%Temp
                call.s  a:sub_callerror
                pops.r  d:%Temp                                     ; This pop will never be called but whatever
                
:.runfinish     brch.a  a:cmdreentry                                ; Not sure if something else needs to be done here, so I'll make it separate
                
                
                
                
                
                
; =============
; Dictionary
; Predefined words

!ip             h00006000

:dict_restart   !data   h40700000 
                !wstr   "restart"
                !data   $dict_period 
                !data   $entry                                      ; IT'S A HACK AGAIN
               
               
:dict_period    !data   h40100000
                !wstr   "."
                !data   $dict_add
                !data   $dictc_period
                
:dictc_period   call.s  a:sub_popsds                                ; Retrieve the number
                push.r  s:%Output
                call.s  a:sub_numoutput
                pops.r  d:%Output                                   ; Print it
                rtrn.s                                              ; And return
                
                
:dict_add       !data   h40100000
                !wstr   "+"
                !data   $dict_subtract
                !data   $dictc_add
                
:dictc_add      call.s  a:sub_popsds                              
                move.r  d:%StackWorkA s:%Output
                call.s  a:sub_popsds                                
                move.r  d:%StackWorkB s:%Output                     ; Get the terms
                
                addr.r  d:%AuxOutput a:%StackWorkB b:%StackWorkA    ; Add em
                call.s  a:sub_pushds                                ; And put back on the data stack

                rtrn.s                                              ; And return
                
                
:dict_subtract  !data   h40100000
                !wstr   "-"
                !data   $dict_create
                !data   $dictc_subtract
                
:dictc_subtract call.s  a:sub_popsds                              
                move.r  d:%StackWorkA s:%Output
                call.s  a:sub_popsds                                
                move.r  d:%StackWorkB s:%Output                     ; Get the terms
                
                subr.r  d:%AuxOutput a:%StackWorkB b:%StackWorkA    ; Subtract em
                call.s  a:sub_pushds                                ; And put back on the data stack

                rtrn.s                                              ; And return
                
                
:dict_create    !data   h40600000
                !wstr   "create"
                !data   $dict_nullentry
                !data   $dictc_create
                
:dictc_create   call.s  a:sub_popsds                              
                move.r  d:%StackWorkA s:%Output
                call.s  a:sub_popsds                                
                move.r  d:%StackWorkB s:%Output                     ; Get the terms
                
                subr.r  d:%AuxOutput a:%StackWorkB b:%StackWorkA    ; Subtract em
                call.s  a:sub_pushds                                ; And put back on the data stack

                rtrn.s                                              ; And return
                
               
                
; =============
; Heap
; Data and code words
                
!ip             h00007000

:dict_nullentry !data h00000000
















