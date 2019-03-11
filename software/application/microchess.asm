; epRISC development platform - MicroChess
;
; modified by John C. Lemme, jclemme (at) proportionallabs (dot) com
; this file is *not* part of the epRISC project, but rather an open-source project that is included here as part of the baseline system - see "license.txt" for details.
;
; John Lemme's port of Daryl Rictor's port of Peter Jenning's MicroChess. 


;***********************************************************************
;
;  MicroChess (c) 1996-2002 Peter Jennings, peterj@benlo.com 
;
;***********************************************************************
; I have been given permission to distribute this program by the
; author and copyright holder, Peter Jennings.  Please get his 
; permission if you wish to re-distribute a modified copy of 
; this file to others.  He specifically requested that his 
; copyright notice be included in the source and binary images.
; Thanks! 
;
; When life gives you lemons, squeeze them back into life's eyes - Elmer
; modified by Daryl Rictor to work over a serial terminal connection, August 2002.


; 6551 I/O Port Addresses
;
ACIADat	= 	$7F70
ACIASta	=	$7F71
ACIACmd	=	$7F72
ACIACtl	=	$7F73
;
; page zero variables
;
BOARD   =	$50 
BK      =	$60 
PIECE   =	$B0 
SQUARE  =	$B1 
SP2     =	$B2 
SP1     =	$B3 
INCHEK  =	$B4 
STATE   =	$B5 
MOVEN   =	$B6 
REV	=       $B7
OMOVE   =	$DC 
WCAP0   =	$DD 
COUNT   =	$DE 
BCAP2   =	$DE 
WCAP2   =	$DF 
BCAP1   =	$E0 
WCAP1   =	$E1 
BCAP0   =	$E2 
MOB     =	$E3 
MAXC    =	$E4 
CC      =	$E5 
PCAP    =	$E6 
BMOB    =	$E3 
BMAXC   =	$E4 
BMCC    =	$E5 		; was BCC (TASS doesn't like it as a label)
BMAXP   =	$E6 
XMAXC   =	$E8 
WMOB    =	$EB 
WMAXC   =	$EC 
WCC     =	$ED 
WMAXP   =	$EE 
PMOB    =	$EF 
PMAXC   =	$F0 
PCC     =	$F1 
PCP     =	$F2 
OLDKY   =	$F3 
BESTP   =	$FB 
BESTV   =	$FA 
BESTM   =	$F9 
DIS1    =	$FB 
DIS2    =	$FA 
DIS3    =	$F9 
temp    =   $FC

!ip h00004000                                       ; load into RAM @ $1000-$15FF (really @ h4000)

:entry      move.v  d:%GL v:#h5000                  ; (This is to emulate zero page behavior)
            move.v  d:%Xw v:#h00                    ; LDA   #$00		        ; REVERSE TOGGLE
		    stor.o  r:%GL s:%Xw o:REV               ; STA   REV
            call.s  a:chess_ioinit                  ; JSR   Init_6551
:chess		                                        ; CLD			            ; INITIALIZE (not applicable for epRISC)
		    move.v  d:%SP v:#h1800                  ; LDX   #$FF		        ; TWO STACKS (just make sure stack is OK in case bootloader effed up)
		                                            ; TXS	
		    move.v  d:%Xx v:#hC8                    ; LDX	#$C8
		    stor.o  r:%GL s:%Xx o:SP2               ; STX	SP2
		
                                                                                ; ROUTINES TO LIGHT LED
                                                                                ; DISPLAY AND GET KEY
                                                                                ; FROM KEYBOARD
		
:out		call.s  a:pout                          ; JSR	pout		        ; DISPLAY AND
		    call.s  a:kin                           ; JSR	KIN		            ; GET INPUT   *** my routine waits for a keypress
                                                    ; CMP	OLDKY		        ; KEY IN ACC  *** no need to debounce
                                                    ; BEQ	OUT		            ; (DEBOUNCE)
                                                    ; STA	OLDKY
                                                    		
		    cmpr.v  a:%Xw v:#h43                    ; CMP	#$43		        ; [C]
		    brch.a  c:%NEQ a:noset                  ; BNE	NOSET		        ; SET UP
		    move.v  d:%Xx v:#h1F                    ; LDX	#$1F		        ; BOARD
:whset		move.v  d:%Xz v:SETW                    
            addr.r  d:%Xz a:%Xz b:%Xx
            load.o  r:%Xz d:%Xw                     ; LDA	SETW,X		        ; FROM
		    move.v  d:%Xz v:BOARD                   
		    addr.r  d:%Xz a:%Xz b:%Xx 
		    stor.o  r:%Xz s:%Xw                     ; STA	BOARD,X		        ; SETW
		    subr.v  d:%Xx a:%Xx v:#h01              ; DEX	
		    cmpr.v  a:%Xw v:#h00
		    brch.a  c:%POS a:whset                  ; BPL	WHSET
		    move.v  d:%Xx v:#h1B                    ; LDX	#$1B		        ; *ADDED
		    stor.o  r:%GL s:%Xx o:OMOVE             ; STX	OMOVE		        ; INITS TO $FF
		    move.v  d:%Xw v:#hCC                    ; LDA	#$CC		        ; Display CCC
		    cmpr.v  a:%Xw v:#h00    
		    brch.a  c:%NEQ a:cldsp                  ; BNE	CLDSP
		
:noset		cmpr.v  a:%Xw v:#h45                    ; CMP	#$45		        ; [E]
		    brch.a  c:%NEQ a:norev                  ; BNE	NOREV		        ; REVERSE
		    call.s  a:reverse                       ; JSR	REVERSE        	    ; BOARD IS
		                                            ; SEC                       ; (pretty pretty sure I don't need this)
		    move.v  d:%Xw v:#h01                    ; LDA	#$01
		    load.o  r:%GL d:%Xz o:REV
		    subr.r  d:%Xw a:%Xw b:%Xz               ; SBC	REV
		    stor.o  r:%GL s:%Xw o:REV               ; STA	REV		            ; TOGGLE REV FLAG
		    move.v  d:%Xw v:#hEE                    ; LDA	#$EE                ; IS
		    cmpr.v  a:%Xw v:#h00                    
		    brch.a  c:%NEQ a:cldsp                  ; BNE	CLDSP
	
:norev		cmpr.v  a:%Xw v:#h40			        ; [P]
		    brch.a  c:%NEQ a:nogo                   ; BNE	NOGO           	    ; PLAY CHESS
		    call.s  a:go                            ; JSR	GO
:cldsp		stor.o  r:%GL s:%Xw o:DIS1              ; STA	DIS1          	    ; DISPLAY
		    stor.o  r:%GL s:%Xw o:DIS2              ; STA	DIS2         	    ; ACROSS
		    stor.o  r:%GL s:%Xw o:DIS3              ; STA	DIS3          	    ; DISPLAY
		    cmpr.v  a:%Xw v:#h00
		    brch.a  c:%NEQ a:chess                  ; BNE	CHESS
		
:nogo		cmpr.v  a:%Xw v:#h0D                    ; CMP	#$0D                ; [Enter]
		    brch.a  c:%NEQ a:nomv                   ; BNE	NOMV          	    ; MOVE MAN
		    call.s  a:move                          ; JSR	MOVE          	    ; AS ENTERED
		    brch.a  a:disp                          ; JMP	DISP
:nomv		cmpr.v  a:%Xw v:#h41                    ; CMP   #$41		        ; [Q] ***Added to allow game exit***
		    brch.a  c:%EQL a:done                   ; BEQ   DONE		        ; quit the game, exit back to system.  
		    brch.a  a:input                         ; JMP	INPUT		        ; process move
:done		move.v  d:%GL v:#h00
            brch.o  r:%GL                           ; JMP   $FF00		        ; *** MUST set this to YOUR OS starting address (in this case, zero)

                                                                                ; THE ROUTINE JANUS DIRECTS THE
                                                                                ; ANALYSIS BY DETERMINING WHAT
                                                                                ; SHOULD OCCUR AFTER EACH MOVE
                                                                                ; GENERATED BY GNM

:janus		load.o  r:%GL d:%Xx o:STATE             ; LDX	STATE
		    cmpr.v  a:%Xx v:#h00
		    brch.a  c:%NEG a:nocount                ; BMI	NOCOUNT
		
                                                                                ; THIS ROUTINE COUNTS OCCURRENCES
                                                                                ; IT DEPENDS UPON STATE TO INDEX
                                                                                ; THE CORRECT COUNTERS
		
:counts		load.o  r:%GL d:%Xw o:PIECE             ; LDA	PIECE
		    cmpr.v  a:%Xw v:#h00
		    brch.a  c:%EQL a:over                   ; BEQ	OVER           	    ; IF STATE=8
		    cmpr.v  a:%Xx v:#h08                    ; CPX 	#$08           	    ; DO NOT COUNT
		    brch.a  c:%NEQ a:over                   ; BNE	OVER          	    ; BLK MAX CAP
		    load.o  r:%GL d:%Xz o:BMAXP
		    cmpr.r  a:%Xw b:%Xz                     ; CMP	BMAXP        	    ; MOVES FOR
		    brch.a  c:%EQL a:xrt                    ; BEQ	XRT           	    ; WHITE
 		
:over       move.v  d:%Xz v:MOB
		    addr.r  d:%Xz a:%Xz b:%Xx
            load.o  r:%Xz d:%Yz
            addr.v  d:%Yz a:%Yz v:#h01      
            stor.o  r:%Xz s:%Yz                     ; INC	MOB,X          	    ; MOBILITY
		    cmpr.v  a:%Xw v:#h01                    ; CMP 	#$01           	    ;  + QUEEN
		    brch.a  c:%NEQ a:noq                    ; BNE	NOQ           	    ; FOR TWO
		    move.v  d:%Xz v:MOB
		    addr.r  d:%Xz a:%Xz b:%Xx
            load.o  r:%Xz d:%Yz
            addr.v  d:%Yz a:%Yz v:#h01      
            stor.o  r:%Xz s:%Yz                     ; INC	MOB,X

:noq		cmpr.v  a:%Xz v:#h80                
            brch.a  c:%HOS a:nocap                  ; BVC	NOCAP               ; (since 6502 is 8-bit, manually checks for overflow)
		    move.v  d:%Xy v:#h0F                    ; LDY	#$0F           	    ; CALCULATE
		    load.o  r:%GL d:%Xw o:SQUARE            ; LDA	SQUARE       	    ; POINTS
:eloop		load.o  r:%GL d:%Xz o:BK
            addr.r  d:%Xz a:%Xz b:%Xy       
            cmpr.r  a:%Xw b:%Xz                     ; CMP	BK,Y           	    ; CAPTURED
		    brch.a  c:%EQL a:foun                   ; BEQ	FOUN          	    ; BY THIS
		    subr.v  d:%Xy a:%Xy v:#h01              ; DEY			            ; MOVE
		    cmpr.v  a:%Xy v:#h00            
		    brch.a  c:%POS a:eloop                  ; BPL	ELOOP
:foun		move.v  d:%Xz v:POINTS
            addr.v  d:%Xz a:%Xz v:%Xy
            load.o  r:%Xz d:%Xw                     ; LDA	POINTS,Y
		    move.v  d:%Xz v:MAXC
		    addr.v  d:%Xz a:%Xz b:%Xx
		    load.o  r:%Xz d:%Xz
		    cmpr.r  a:%Xw b:%Xz                     ; CMP	MAXC,X
		    brch.a  c:%LOW a:less                   ; BCC	LESS          	    ; SAVE IF
            move.v  d:%Xz v:PCAP
		    addr.r  d:%Xz a:%Xz b:%Xx
            stor.o  r:%Xz s:%Xy                     ; STY	PCAP,X         	    ; BEST THIS
            move.v  d:%Xz v:MAXC
		    addr.r  d:%Xz a:%Xz b:%Xx
            stor.o  r:%Xz s:%Xy                     ; STA	MAXC,X         	    ; STATE
		
:.less		                                        ; CLC	                    ; (don't need this)
		                                            ; PHP			            ; ADD TO (nor this)
            move.v  d:%Xz v:CC
		    addr.r  d:%Xz a:%Xz b:%Xx
            load.o  r:%Xz d:%Yz
            addr.v  d:%Xw a:%Xw b:%Yz               ; ADC	CC,X           	    ; CAPTURE
		    move.v  d:%Xz v:CC
		    addr.r  d:%Xz a:%Xz b:%Xx
            stor.o  r:%Xz s:%Xw                     ; STA	CC,X           	    ; COUNTS
		                                            ; PLP                       ; (nope)	
		
NOCAP		CPX	#$04
		BEQ	ON4
		BMI	TREE          	;(=00 ONLY)
XRT		RTS	
;		
;      GENERATE FURTHER MOVES FOR COUNT
;      AND ANALYSIS	
;		
ON4		LDA	XMAXC        	; SAVE ACTUAL 
		STA	WCAP0         	; CAPTURE
		LDA	#$00           	; STATE=0
		STA	STATE
		JSR	MOVE          	; GENERATE
		JSR	REVERSE       	; IMMEDIATE
		JSR	GNMZ         	; REPLY MOVES  
		JSR	REVERSE
;		
		LDA	#$08       	; STATE=8
		STA	STATE        	; GENERATE
;		JSR	OHM          	; CONTINUATION
		JSR	UMOVE         	; MOVES
;		
		JMP	STRATGY       	; FINAL EVALUATION
NOCOUNT	CPX	#$F9
		BNE	TREE
;		
;      DETERMINE IF THE KING CAN BE
;      TAKEN, USED BY CHKCHK
;		
		LDA	BK           	; IS KING
		CMP	SQUARE       	; IN CHECK?
		BNE	RETJ          	; SET INCHEK=0
		LDA	#$00           	; IF IT IS
		STA	INCHEK
RETJ		RTS	
;		
;      IF A PIECE HAS BEEN CAPTURED BY
;      A TRIAL MOVE, GENERATE REPLIES &
;      EVALUATE THE EXCHANGE GAIN/LOSS
;		
TREE		BVC	RETJ          	; NO CAP
		LDY	#$07           	; (PIECES)
		LDA	SQUARE
LOOPX		CMP	BK,Y
		BEQ	FOUNX
		DEY	
		BEQ	RETJ          	; (KING)
		BPL	LOOPX         	; SAVE
FOUNX		LDA	POINTS,Y       	; BEST CAP
		CMP	BCAP0,X        	; AT THIS
		BCC	NOMAX         	; LEVEL
		STA	BCAP0,X
NOMAX		DEC	STATE
		LDA	#$FB           	; IF STATE=FB
		CMP	STATE        	; TIME TO TURN
		BEQ	UPTREE        	; AROUND
		JSR	GENRM         	; GENERATE FURTHER
UPTREE		INC	STATE        	; CAPTURES
		RTS	
;		
;      THE PLAYER'S MOVE IS INPUT
;		
INPUT		CMP	#$08           	; NOT A LEGAL
		BCS	ERROR      	; SQUARE #
		JSR	DISMV
DISP		LDX	#$1F
SEARCH		LDA	BOARD,X
		CMP	DIS2
		BEQ	HERE          	; DISPLAY
		DEX			; PIECE AT    
		BPL	SEARCH        	; FROM
HERE		STX	DIS1         	; SQUARE
		STX	PIECE
ERROR		JMP	CHESS
;		
;      GENERATE ALL MOVES FOR ONE
;      SIDE, CALL JANUS AFTER EACH
;      ONE FOR NEXT STE?
;		
;
GNMZ		LDX	#$10            ; CLEAR
GNMX		LDA	#$00            ; COUNTERS
CLEAR		STA	COUNT,X
		DEX	
		BPL	CLEAR
;		
GNM		LDA	#$10            ; SET UP
		STA	PIECE        	; PIECE
NEWP		DEC	PIECE        	; NEW PIECE
		BPL	NEX           	; ALL DONE?
		RTS			; #NAME?
;		
NEX		JSR	RESET        	; READY
		LDY	PIECE        	; GET PIECE
		LDX	#$08
		STX	MOVEN        	; COMMON START
		CPY	#$08            ; WHAT IS IT?
		BPL	PAWN          	; PAWN
		CPY	#$06
		BPL	KNIGHT        	; KNIGHT
		CPY	#$04
		BPL	BISHOP       	; BISHOP
		CPY	#$01
		BEQ	QUEEN         	; QUEEN
		BPL	ROOK          	; ROOK
;		
KING		JSR	SNGMV         	; MUST BE KING!
		BNE	KING          	; MOVES
		BEQ	NEWP          	; 8 TO 1
QUEEN		JSR	LINE
		BNE	QUEEN         	; MOVES
		BEQ	NEWP          	; 8 TO 1
;		
ROOK		LDX	#$04
		STX	MOVEN        	; MOVES
AGNR		JSR	LINE          	; 4 TO 1
		BNE	AGNR
		BEQ	NEWP
;		
BISHOP		JSR	LINE
		LDA	MOVEN        	; MOVES
		CMP	#$04           	; 8 TO 5
		BNE	BISHOP
		BEQ	NEWP
;		
KNIGHT		LDX	#$10
		STX	MOVEN        	; MOVES
AGNN		JSR	SNGMV         	; 16 TO 9
		LDA	MOVEN
		CMP	#$08
		BNE	AGNN
		BEQ	NEWP
;		
PAWN		LDX	#$06
		STX	MOVEN
P1		JSR	CMOVE         	; RIGHT CAP?
		BVC	P2
		BMI	P2
		JSR	JANUS         	; YES
P2		JSR	RESET
		DEC	MOVEN        	; LEFT CAP?
		LDA	MOVEN
		CMP	#$05
		BEQ	P1
P3		JSR	CMOVE         	; AHEAD
		BVS	NEWP          	; ILLEGAL
		BMI	NEWP
		JSR	JANUS
		LDA	SQUARE       	; GETS TO
		AND	#$F0           	; 3RD RANK?
		CMP	#$20
		BEQ	P3            	; DO DOUBLE
		JMP	NEWP
;		
;      CALCULATE SINGLE STEP MOVES
;      FOR K,N	
;		
SNGMV		JSR	CMOVE        	; CALC MOVE
		BMI	ILL1           	; -IF LEGAL
		JSR	JANUS           ; -EVALUATE
ILL1		JSR	RESET
		DEC	MOVEN
		RTS	
;		
;     CALCULATE ALL MOVES DOWN A
;     STRAIGHT LINE FOR Q,B,R
;		
LINE		JSR	CMOVE         	; CALC MOVE
		BCC	OVL            	; NO CHK
		BVC	LINE		; NOCAP       
OVL		BMI	ILL             ; RETURN
		PHP	
		JSR	JANUS         	; EVALUATE POSN
		PLP	
		BVC	LINE          	; NOT A CAP
ILL		JSR	RESET         	; LINE STOPPED
		DEC	MOVEN         	; NEXT DIR
		RTS	
;		
;      EXCHANGE SIDES FOR REPLY
;      ANALYSIS	
;		
REVERSE		LDX	#$0F
ETC		SEC	
		LDY	BK,X           	; SUBTRACT
		LDA 	#$77           	; POSITION
		SBC  	BOARD,X        	; FROM 77
		STA  	BK,X
		STY  	BOARD,X         ; AND
		SEC	
		LDA	#$77           	; EXCHANGE
		SBC 	BOARD,X        	; PIECES
		STA  	BOARD,X
		DEX	
		BPL   	ETC
		RTS	
;		
;        CMOVE CALCULATES THE TO SQUARE
;        USING SQUARE AND THE MOVE
;       TABLE  FLAGS SET AS FOLLOWS:
;       N#NAME?	MOVE
;       V#NAME?	(LEGAL UNLESS IN CR)
;       C#NAME?	BECAUSE OF CHECK
;       [MY &THANKS TO JIM BUTTERFIELD
;        WHO WROTE THIS MORE EFFICIENT
;        VERSION OF CMOVE)
;		
CMOVE		LDA	SQUARE       	; GET SQUARE
		LDX	MOVEN       	; MOVE POINTER
		CLC	
		ADC	MOVEX,X        	; MOVE LIST
		STA	SQUARE       	; NEW POS'N
		AND	#$88
		BNE	ILLEGAL       	; OFF BOARD
		LDA	SQUARE
;			
		LDX	#$20
LOOP		DEX			; IS TO
		BMI	NO            	; SQUARE
		CMP  	BOARD,X        	; OCCUPIED?
		BNE   	LOOP
;			
		CPX	#$10            ; BY SELF?
		BMI   	ILLEGAL
;			
		LDA	#$7F		; MUST BE CAP!
		ADC	#$01            ; SET V FLAG
		BVS   	SPX 	        ; (JMP)
;			
NO		CLV			; NO CAPTURE
;			
SPX		LDA  	STATE         	; SHOULD WE
		BMI   	RETL           	; DO THE
		CMP	#$08 	        ; CHECK CHECK?
		BPL  	RETL
;			
;        CHKCHK REVERSES SIDES
;       AND LOOKS FOR A KING
;       CAPTURE TO INDICATE
;       ILLEGAL MOVE BECAUSE OF
;       CHECK  SINCE THIS IS
;       TIME CONSUMING, IT IS NOT
;       ALWAYS DONE	
;		
CHKCHK	PHA				; STATE  #392
		PHP	
		LDA	#$F9
		STA	STATE         	; GENERATE
		STA	INCHEK        	; ALL REPLY
		JSR	MOVE          	; MOVES TO
		JSR	REVERSE       	; SEE IF KING
		JSR	GNM           	; IS IN
		JSR	RUM           	; CHECK
		PLP	
		PLA	
		STA	STATE
		LDA	INCHEK
		BMI	RETL           	; NO - SAFE
		SEC			; YES - IN CHK
		LDA	#$FF
		RTS	
;		
RETL		CLC			; LEGAL
		LDA	#$00            ; RETURN
		RTS	
;		
ILLEGAL	LDA	#$FF
		CLC			; ILLEGAL
		CLV			; RETURN
		RTS	
;		
;       REPLACE PIECE ON CORRECT SQUARE
;		
RESET		LDX	PIECE      	; GET LOGAT
		LDA	BOARD,X        	; FOR PIECE
		STA	SQUARE       	; FROM BOARD
		RTS	
;		
;		
;		
GENRM		JSR	MOVE          	; MAKE MOVE
GENR2		JSR	REVERSE      	; REVERSE BOARD
		JSR	GNM          	; GENERATE MOVES
RUM		JSR	REVERSE   	; REVERSE BACK
;		
;       ROUTINE TO UNMAKE A MOVE MADE BY
;	  MOVE
;		
UMOVE		TSX			; UNMAKE MOVE
		STX	SP1
		LDX	SP2           	; EXCHANGE
		TXS			; STACKS
		PLA			; MOVEN
		STA	MOVEN
		PLA			; CAPTURED
		STA	PIECE        	; PIECE
		TAX	
		PLA			; FROM SQUARE
		STA	BOARD,X
		PLA			; PIECE
		TAX	
		PLA			; TO SOUARE
		STA	SQUARE
		STA	BOARD,X
		JMP	STRV
;		
;       THIS ROUTINE MOVES PIECE
;       TO SQUARE, PARAMETERS
;       ARE SAVED IN A STACK TO UNMAKE
;       THE MOVE LATER
;		
MOVE		TSX	
		STX	SP1          	; SWITCH
		LDX	SP2          	; STACKS
		TXS	
		LDA	SQUARE
		PHA			; TO SQUARE
		TAY	
		LDX	#$1F
CHECK		CMP	BOARD,X        	; CHECK FOR
		BEQ	TAKE          	; CAPTURE
		DEX	
		BPL	CHECK
TAKE		LDA	#$CC
		STA	BOARD,X
		TXA			; CAPTURED
		PHA			; PIECE
		LDX	PIECE
		LDA	BOARD,X
		STY	BOARD,X        	; FROM
		PHA			; SQUARE
		TXA	
		PHA			; PIECE
		LDA	MOVEN
		PHA			; MOVEN
STRV		TSX	
		STX	SP2           	; SWITCH
		LDX	SP1           	; STACKS
		TXS			; BACK
		RTS	
;			
;       CONTINUATION OF SUB STRATGY
;       -CHECKS FOR CHECK OR CHECKMATE
;       AND ASSIGNS VALUE TO MOVE
;		
CKMATE	LDY	BMAXC         		; CAN BLK CAP
		CPX	POINTS       	; MY KING?
		BNE	NOCHEK	
		LDA	#$00           	; GULP!
		BEQ	RETV          	; DUMB MOVE!
;		
NOCHEK	LDX	BMOB         		; IS BLACK
		BNE	RETV          	; UNABLE TO
		LDX	WMAXP        	; MOVE AND
		BNE	RETV          	; KING IN CH?
		LDA	#$FF           	; YES! MATE
;		
RETV		LDX	#$04            ; RESTORE
		STX	STATE        	; STATE=4
;		
;       THE VALUE OF THE MOVE (IN ACCU)
;       IS COMPARED TO THE BEST MOVE AND
;       REPLACES IT IF IT IS BETTER
;		
PUSH		CMP	BESTV         	; IS THIS BEST
		BCC	RETP          	; MOVE SO FAR?
		BEQ	RETP
		STA	BESTV        	; YES!
		LDA	PIECE        	; SAVE IT
		STA	BESTP
		LDA	SQUARE
		STA	BESTM        	; FLASH DISPLAY
RETP		LDA	#"."		; print ... instead of flashing disp
		Jmp	syschout	; print . and return
;		
;       MAIN PROGRAM TO PLAY CHESS
;       PLAY FROM OPENING OR THINK
;		
GO		LDX	OMOVE        	; OPENING?
		BMI	NOOPEN          ; -NO   *ADD CHANGE FROM BPL
		LDA	DIS3         	; -YES WAS
		CMP	OPNING,X        ; OPPONENT'S
		BNE	END            	; MOVE OK?
		DEX	
		LDA	OPNING,X       	; GET NEXT
		STA	DIS1         	; CANNED
		DEX			; OPENING MOVE
		LDA	OPNING,X
		STA	DIS3         	; DISPLAY IT
		DEX	
		STX	OMOVE        	; MOVE IT
		BNE	MV2            	; (JMP)
;			
END		LDA     #$FF		; *ADD - STOP CANNED MOVES
		STA	OMOVE        	; FLAG OPENING
NOOPEN	LDX	#$0C            	; FINISHED
		STX	STATE        	; STATE=C
		STX	BESTV        	; CLEAR BESTV
		LDX	#$14           	; GENERATE P
		JSR	GNMX          	; MOVES
;		
		LDX	#$04           	; STATE=4
		STX	STATE        	; GENERATE AND
		JSR	GNMZ          	; TEST AVAILABLE
;
;	MOVES
;		
		LDX	BESTV        	; GET BEST MOVE
		CPX	#$0F           	; IF NONE
		BCC	MATE          	; OH OH!
;		
MV2		LDX	BESTP        	; MOVE
		LDA	BOARD,X         ; THE
		STA	BESTV        	; BEST
		STX	PIECE        	; MOVE
		LDA	BESTM
		STA	SQUARE       	; AND DISPLAY
		JSR	MOVE           	; IT
		JMP	CHESS
;		
MATE		LDA	#$FF           	; RESIGN
		RTS			; OR STALEMATE
;		
;       SUBROUTINE TO ENTER THE
;       PLAYER'S MOVE
;		
DISMV		LDX	#$04           	; ROTATE
DROL		ASL	DIS3          	; KEY
		ROL	DIS2         	; INTO
		DEX			; DISPLAY
		BNE	DROL		;
		ORA	DIS3
		STA	DIS3
		STA	SQUARE
		RTS	
;		
;       THE FOLLOWING SUBROUTINE ASSIGNS
;       A VALUE TO THE MOVE UNDER
;       CONSIDERATION AND RETURNS IT IN
;	THE ACCUMULATOR
;		

STRATGY	CLC	
		LDA	#$80
		ADC	WMOB         	; PARAMETERS
		ADC	WMAXC        	; WITH WHEIGHT
		ADC	WCC          	; OF O25
		ADC	WCAP1
		ADC	WCAP2
		SEC	
		SBC	PMAXC
		SBC	PCC
		SBC	BCAP0
		SBC	BCAP1
		SBC	BCAP2
		SBC	PMOB
		SBC	BMOB
		BCS	POS           	; UNDERFLOW
		LDA	#$00           	; PREVENTION
POS		LSR	
		CLC			; **************
		ADC	#$40
		ADC	WMAXC       	; PARAMETERS
		ADC	WCC         	; WITH WEIGHT
		SEC			; OF 05
		SBC	BMAXC
		LSR			; **************
		CLC	
		ADC	#$90
		ADC	WCAP0       	; PARAMETERS
		ADC	WCAP0       	; WITH WEIGHT
		ADC	WCAP0       	; OF 10
		ADC	WCAP0
		ADC	WCAP1
		SEC			; [UNDER OR OVER-
		SBC	BMAXC        	; FLOW MAY OCCUR
		SBC	BMAXC        	; FROM THIS
		SBC	BMCC          	; SECTION]
		SBC	BMCC
		SBC	BCAP1
		LDX	SQUARE      	; ***************
		CPX	#$33
		BEQ	POSN         	; POSITION
		CPX	#$34           	; BONUS FOR
		BEQ	POSN         	; MOVE TO
		CPX	#$22           	; CENTRE
		BEQ	POSN           	; OR
		CPX	#$25           	; OUT OF
		BEQ	POSN         	; BACK RANK
		LDX	PIECE
		BEQ	NOPOSN
		LDY	BOARD,X
		CPY	#$10
		BPL	NOPOSN
POSN		CLC
		ADC	#$02
NOPOSN	JMP	CKMATE       		; CONTINUE


;-----------------------------------------------------------------
; The following routines were added to allow text-based board
; display over a standard RS-232 port.
;
POUT        	jsr 	pout9		; print CRLF
		jsr     pout13		; print copyright
		JSR	POUT10		; print column labels
		LDY   	#$00		; init board location
		JSR	POUT5		; print board horz edge
POUT1		lDA   	#"|"		; print vert edge
		JSR   	syschout	; PRINT ONE ASCII CHR - SPACE
		LDX   	#$1F
POUT2		TYA			; scan the pieces for a location match
            	CMP	BOARD,X		; match found?
            	BEQ   	POUT4		; yes; print the piece's color and type
            	DEX			; no
            	BPL	POUT2		; if not the last piece, try again
		tya			; empty square	
		and	#$01		; odd or even column?				
		sta   	temp		; save it
		tya  			; is the row odd or even
		lsr			; shift column right 4 spaces 
		lsr			;
		lsr			;
		lsr			;
		and   	#$01		; strip LSB  
		clc			; 
		adc   	temp		; combine row & col to determine square color  
		and   	#$01		; is board square white or blk?
		bne	pout25 		; white, print space
		lda   	#"*"		; black, print *
		.byte	$2c		; used to skip over LDA #$20
POUT25		LDA   	#$20		; ASCII space
		JSR   	syschout	; PRINT ONE ASCII CHR - SPACE
		JSR   	syschout	; PRINT ONE ASCII CHR - SPACE
POUT3		INY			; 
	        TYA			; get row number
            	AND   	#$08		; have we completed the row?	 
            	BEQ   	POUT1		; no, do next column
		LDA   	#"|"		; yes, put the right edge on
		JSR   	syschout	; PRINT ONE ASCII CHR - |             
		jsr	pout12		; print row number
		JSR   	POUT9		; print CRLF
            	JSR   	POUT5		; print bottom edge of board
		CLC			; 
		TYA			; 
		ADC	#$08		; point y to beginning of next row
		TAY			;
		CPY   	#$80		; was that the last row?
		BEQ   	POUT8		; yes, print the LED values
		BNE   	POUT1		; no, do new row

POUT4		LDA   	REV		; print piece's color & type
		BEQ   	POUT41		;
		LDA	cpl+16,X	;
		BNE	POUT42		;
POUT41		LDA   	cpl,x		;
POUT42		JSR	syschout	;
		lda	cph,x		;
		jsr   	syschout	; 
		BNE	POUT3		; branch always

POUT5       	TXA			; print "-----...-----<crlf>"
		PHA
		LDX	#$19
		LDA	#"-"
POUT6		JSR   	syschout	; PRINT ONE ASCII CHR - "-"
		DEX
		BNE	POUT6
		PLA
		TAX
		JSR	POUT9
		RTS	 		

POUT8		jsr	pout10		; 
		LDA   	$FB
		JSR   	syshexout	; PRINT 1 BYTE AS 2 HEX CHRS	
        	LDA   	#$20
		JSR   	syschout	; PRINT ONE ASCII CHR - SPACE
        	LDA   	$FA
		JSR   	syshexout	; PRINT 1 BYTE AS 2 HEX CHRS	
        	LDA   	#$20
		JSR   	syschout	; PRINT ONE ASCII CHR - SPACE
        	LDA   	$F9
		JSR   	syshexout	; PRINT 1 BYTE AS 2 HEX CHRS	

POUT9      	LDA   	#$0D
		JSR   	syschout	; PRINT ONE ASCII CHR - CR
        	LDA   	#$0A
		JSR   	syschout	; PRINT ONE ASCII CHR - LF
                RTS 

pout10		ldx   	#$00		; print the column labels
POUT11		lda	#$20		; 00 01 02 03 ... 07 <CRLF>
		jsr   	syschout
		txa
		jsr	syshexout
		INX
		CPX   	#$08
		BNE	POUT11
		BEQ	POUT9
POUT12		TYA
		and 	#$70
		JSR 	syshexout
		rts

Pout13		ldx   	#$00		; Print the copyright banner
Pout14		lda   	banner,x
		beq   	POUT15
		jsr   	syschout
		inx
		bne   	POUT14
POUT15		rts         

KIN        	LDA   	#"?"
		JSR   	syschout	; PRINT ONE ASCII CHR - ?
		JSR   	syskin		; GET A KEYSTROKE FROM SYSTEM
            	AND   	#$4F            ; MASK 0-7, AND ALPHA'S
            	RTS
;
; 6551 I/O Support Routines
;
;
Init_6551      lda   #$1F               ; 19.2K/8/1
               sta   ACIActl            ; control reg 
               lda   #$0B               ; N parity/echo off/rx int off/ dtr active low
               sta   ACIAcmd            ; command reg 
               rts                      ; done
;
; input chr from ACIA1 (waiting)
;
syskin         lda   ACIASta            ; Serial port status             
               and   #$08               ; is recvr full
               beq   syskin             ; no char to get
               Lda   ACIAdat            ; get chr
               RTS                      ;
;
; output to OutPut Port
;
syschout       PHA                      ; save registers
ACIA_Out1      lda   ACIASta            ; serial port status
               and   #$10               ; is tx buffer empty
               beq   ACIA_Out1          ; no
               PLA                      ; get chr
               sta   ACIAdat            ; put character to Port
               RTS                      ; done

syshexout      PHA                     ;  prints AA hex digits
               LSR                     ;  MOVE UPPER NIBBLE TO LOWER
               LSR                     ;
               LSR                     ;
               LSR                     ;
               JSR   PrintDig          ;
               PLA                     ;
PrintDig       PHY                     ;  prints A hex nibble (low 4 bits)
               AND   #$0F              ;
               TAY                     ;
               LDA   Hexdigdata,Y      ;
               PLY                     ;
               jmp   syschout          ;

Hexdigdata	.byte	"0123456789ABCDEF"
banner		.byte	"MicroChess (c) 1996-2002 Peter Jennings, peterj@benlo.com"
		.byte	$0d, $0a, $00
cpl		.byte	"WWWWWWWWWWWWWWWWBBBBBBBBBBBBBBBBWWWWWWWWWWWWWWWW"
cph		.byte	"KQCCBBRRPPPPPPPPKQCCBBRRPPPPPPPP"
		.byte	$00
;
; end of added code
;
; BLOCK DATA
		*= $1580
SETW		.byte 	$03, $04, $00, $07, $02, $05, $01, $06
        	.byte 	$10, $17, $11, $16, $12, $15, $14, $13
        	.byte 	$73, $74, $70, $77, $72, $75, $71, $76
	 	.byte	$60, $67, $61, $66, $62, $65, $64, $63

MOVEX   	.byte 	$00, $F0, $FF, $01, $10, $11, $0F, $EF, $F1
		.byte	$DF, $E1, $EE, $F2, $12, $0E, $1F, $21

POINTS  	.byte 	$0B, $0A, $06, $06, $04, $04, $04, $04
		.byte 	$02, $02, $02, $02, $02, $02, $02, $02

OPNING  	.byte 	$99, $25, $0B, $25, $01, $00, $33, $25
		.byte	$07, $36, $34, $0D, $34, $34, $0E, $52
        	.byte 	$25, $0D, $45, $35, $04, $55, $22, $06
		.byte	$43, $33, $0F, $CC

;
;
; end of file
;

!include    "../rom/bios_bus.asm"
!include    "../rom/bios_uart.asm"
!include    "../rom/bios_string.asm"
!include    "../rom/bios_video.asm"
!include    "../rom/bios_debug.asm"
