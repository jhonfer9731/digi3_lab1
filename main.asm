;*******************************************************************
;* This stationery serves as the framework for a user application. *
;* For a more comprehensive program that demonstrates the more     *
;* advanced functionality of this processor, please see the        *
;* demonstration applications, located in the examples             *
;* subdirectory of the "Freescale CodeWarrior for HC08" program    *
;* directory.                                                      *
;*******************************************************************
; Include derivative-specific definitions
            INCLUDE 'derivative.inc'
         
;
            XDEF _Startup
            ABSENTRY _Startup ;
            
            ;def values

;MASK            EQU %00111111 ; No existe en memoria
					
					
					
                ORG    Z_RAMStart
Operand1:        DS.B   1           ; Contador, DS, Define store 
Operand2:        DS.B   1 
Contador3:        DS.B   1
signoNegativo:		DS.B 1
Result:			DS.B	1
decimal1:		DS.W	1
;
; Secci�n para definir variables en memoria RAM, por fuera de la p�gina cero
;
            ;ORG    RAMStart

; Secci�n de c�digo del programa. 
		ORG   ROMStart
 _Startup:
		; Apagar WatchDOG
		LDA   #$20
		STA   SOPT1
		
		; Takes the SP to the last position of RAM
		LDHX #RAMEnd+1; charge HX with RAMEnd + 1
		TXS 		; move the value of HX to the stack pointer - 1
		
		
		CLRA		; clear A register
		CLRX		; clear X register
		CLRH		; clear H register
		CLR Operand1 ; clear reserved memory positions
		CLR Operand2
		CLR Contador3
		CLR signoNegativo
		CLR Result
		CLR decimal1
		MOV #$00,PTAD ; byte with the start (bit 0),Capture_Operand1 (bit 1) and Capture_Operand2 (bit2) signals 
		MOV #$00,PTADD ; set as input port
		MOV #$00,PTBD ; port assigned to Operand value
		MOV #$00, PTBDD ; set as input port
		MOV #$00,PTCD ; port assigned to the Operation name
		MOV #$00, PTCDD; set as input port
		MOV #$00,PTDD; port to show the result
		MOV #$FF, PTDDD; set as output port
		MOV #$00,PTED; port assigned to flags
		MOV #$FF, PTEDD; set as output port
; program body

Inicio: 
; The capture of the 1st operand is performed


Capture:			
			BRCLR	1,PTAD,Delay	; branch if the bit 1 of PTAD is cleared (0)
			BRA		Capture
			
Delay:		LDHX	#127; load the inmediate into H:X register
SigaDelay:	AIX		#-1; Decrement H:X register
			CPHX	#0; Compare if it is 0
			BNE		SigaDelay; If HX is not 0 continue with de loop
			BRCLR	1,PTAD,Cap2Operand; verify if the bit 1 of PTAD is still cleared
			BRA		Capture; ; if it is not cleared now, return to Capture and start again
			

Cap2Operand:
			MOV		PTBD,Operand1	


; The capture of the 2nd operand is performed

		
CapLoop2:
			BRCLR	2,PTAD,Delay2 ; branch if the bit 2 of PTAD is cleared (0)
			BRA		CapLoop2
Delay2:
			LDHX	#127; load the inmediate into H:X register		
SigaDelay2:	AIX		#-1; Decrement H:X register
			CPHX	#0; Compare if it is 0
			BNE		SigaDelay2; If HX is not 1 continue with de loop
			BRCLR	2,PTAD,selOperation; verify if the bit 1 of PTAD is still cleared
			BRA		CapLoop2; ; if it is not cleared now, return to Capture and start again

; The capture of the operation is performed


selOperation:
			MOV		PTBD,Operand2
LoopOperation:
			BRCLR	0,PTAD,Delay3 ; branch if the bit 0 of PTAD is cleared (0)
			BRA		LoopOperation
Delay3:
			LDHX	#127; load the inmediate into H:X register		
SigaDelay3:	AIX		#-1; Decrement H:X register
			CPHX	#0; Compare if it is 0
			BNE		SigaDelay3; If HX is not 1 continue with de loop
			BRCLR	0,PTAD,OpType; verify if the bit 1 of PTAD is still cleared
			BRA		LoopOperation; ; if it is not cleared now, return to Capture and start again


; Choose the type of the operation according to the bits 0 and 1 of the port PTCD	
OpType:
			LDA		PTCD ;Load in A the content of port c where the opType is stored
			AND		#%00000011 ;mask to take only the bits of operation
			CMP		#3
			BEQ		division
			CMP		#2
			BEQ		multiplication
			CMP		#1
			BEQ		subtract
			JMP		sum
			

subtract:
			LDA		Operand1	
			SUB		Operand2		; Subtract Op1-Op2
			TAX						;Save value of A in X
			TPA						;Load A with the status register
			AND		#%10000000		;Mask for Overflow bit in CCR
			BMI		SUB_Overflow	;if z=1 exit
			STX		Result			; if not, store the result of the subtraction
			MOV		Result,PTDD		
			BCLR	0,PTED;			;flag: no overflow error 
			JMP 	End_Program ; it is temporal

SUB_Overflow:
			CLR 	Result
			BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error
			JMP 	End_Program ;



multiplication:
			CLR Contador3
			LDA		Operand1
			BMI		change_sign1
			BRA		go_mult
change_sign1:
			NEGA
			INC 	Contador3
go_mult:	
			LDX 	Operand2
			BMI		change_sign2
			BRA		next_mult		
change_sign2:
			NEGX
			INC		Contador3
	
next_mult:	BRSET	0,Contador3,sign_mult
			MOV		#0,signoNegativo
cont_mult:	MUL							;Multiply X * A
			PSHX						; X to stack
			PULH						;Load H with X (through stack)
			TAX							;X<--A
			STHX	Result				; save result
			MOV		Result,PTDD			
			BCLR	0,PTED				; no flags
			BRA 	End_Program
sign_mult:
			MOV		#1,signoNegativo
			BRA cont_mult



division:			; it verifies if there are special cases
					CLRH
					LDA		Operand2
					CMP		#1
					BNE		verificarSi0
					MOV		Operand1,Result
					MOV		Operand1,PTDD
					BRA 	End_Program ; it is temporal
verificarSi0:		CMP		#0
					BNE		OperandoSinE
					BSET	1,PTED ; send a flag to the LSB of the port noticing about zero division
					BRA 	End_Program ; it is temporal
					
				; it executes if there is no special cases
OperandoSinE:		EOR		Operand1
					AND		#%10000000
					BEQ		empezarDiv; Branch if the signs are equal in both operands
					MOV		#1,signoNegativo
empezarDiv:
					BRCLR	7,Operand1,comprobar2 
					NEG		Operand1		; Change the operand to positive number if it is negative
comprobar2:
					BRCLR	7,Operand2,seguirDiv
					NEG		Operand2		; Change the operand to positive number if it is negative
seguirDiv:
					LDX		Operand2  	;Charge operand2 to X
					LDA		Operand1	;Charge operand2 to A
					CMP		#-128  ; verify the case of -128/-1
					BNE		dividir
					CPX		#-1
					BNE		dividir
					BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error
					BRA 	End_Program ; it is temporal
dividir:
					DIV
					STA		Result
					CLRA	; clear the 8 LSB of the dividend
					DIV		; divide the remainder which is located in the H register , A will have the 8-bits binary fraction of the operation
					;STA		decimal
					;convert binary fraction to decimal
					
					LDX		#10
					MUL
					STX		decimal1  ;Take the 8 most significant bits, they have the first decimal of the operation
					LDX		#10
					MUL		;Multiply the 8 least significant bits that are contained in A with #10
					STX		decimal1 + 1  ; Take the most significant bits, they have the second decimal of the operation
					
					
					CLRX	;Clear X register
					CLRH	;Clear H register					
					MOV		Result,PTDD
					BCLR	0,PTED ; clear the overflow flag
					BRA 	End_Program ; it is temporal
	
	
									
			
sum:
			LDA		Operand1
			ADD		Operand2; Add to check the overflow flag only
			;flag for checking status
			BLT		verificarNegativo ; N xor V = 1, desechar el caso donde N =1 y V = 0
			BPL		operacionCorrecta
			BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error (because of big positive numbers)
			BRA 	End_Program ; it is temporal
					
verificarNegativo:
			BMI		operacionCorrecta
			BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error (because of big negative numbers)
			BRA 	End_Program ; it is temporal
operacionCorrecta:
			LDA		Operand1
			ADD		Operand2
			STA		Result; it modifies the CCR register
			MOV		Result,PTDD
			BCLR	0,PTED;
			BRA 	End_Program ; it is temporal
			

			
			
End_Program:

			BRA 	End_Program

;*                 Interrupt Vectors                          *
;**************************************************************

            ORG	$FFFE
			DC.W  _Startup			; Reset
