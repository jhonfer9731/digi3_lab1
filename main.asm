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

RESULTADO_FIN           EQU  $00C0; No existe en memoria
					
					
					
                ORG    Z_RAMStart
Operand1:        DS.B   1           ; Contador, DS, Define store 
Operand2:        DS.B   1 
Contador3:        DS.B   1
signoNegativo:		DS.B 1
Result:			DS.B	1
cocient:		DS.B	1
contadorChar:	DS.B	1
contadorAlerta: DS.W	1		;pointer to ROM address of alert message
contadorMensaje:DS.W 	1		;pointer to RAM address of the display message
ResultM:		DS.W	1
pointerBCD:		DS.W	1

			ORG RESULTADO_FIN
numeroBCD:		DS.B	6
puntoDecimal:	DS.B	1
decimal1:		DS.B	2
;
; Secci�n para definir variables en memoria RAM, por fuera de la p�gina cero
;
            ORG    RAMStart
mensajeAlerta:		DS.B	50

; Seccion de codigo del programa. 
		ORG   ROMStart
 _Startup:
		; Apagar WatchDOG
		LDA   #$20
		STA   SOPT1
		
		; Takes the SP to the last position of RAM
		LDHX #RAMEnd+1; charge HX with RAMEnd + 1
		TXS 		; move the value of HX to the stack pointer - 1
		
StartAllAgain:

			MOV	#64,contadorChar
			LDHX	#RESULTADO_FIN
			CLR decimal1
			CLR decimal1+1
repLimpiar:	LDA  	#32
			STA		,X
			INCX
			DBNZ contadorChar,repLimpiar
			
				MOV	#64,contadorChar
				LDHX 	#RAMStart
repLimpiar2:	LDA  	#32
				STA		,X
				INCX
				DBNZ contadorChar,repLimpiar2
			
			
		CLR		contadorChar
		CLRA		; clear A register
		CLRX		; clear X register
		CLRH		; clear H register
		
		CLR Operand1 ; clear reserved memory positions
		CLR Operand2
		CLR Contador3
		CLR signoNegativo
		CLR Result
		CLR contadorAlerta
		MOV	#' ',puntoDecimal
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
			TXA
			JMP 	setSign

SUB_Overflow:
			CLR 	Result
			BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error
			JMP 	ErrorMessage ;



multiplication:
			CLR Contador3				;Cont to know if + (2 or 0) or - (1)
			LDA		Operand1		
			BMI		change_sign1		;Change the sign if negative
			CMP		#0
			BNE 	go_mult		
			JMP 	Display			;if 0 finish 
change_sign1:
			NEGA
			INC 	Contador3
go_mult:	
			LDX 	Operand2
			BMI		change_sign2		;verify sign second operand
			CPX		#0
			BNE 	next_mult		
			JMP 	Display			;if 0 finish 
change_sign2:
			NEGX
			INC		Contador3
	
next_mult:	BRSET	0,Contador3,sign_mult
			MOV		#0,signoNegativo	;if Contador=0 or 2 is positive
cont_mult:	MUL							;Multiply X * A
			PSHX						; X to stack (second operand)
			PULH						;Load H with X (through stack)
			TAX							;X<--A
			STHX	ResultM				; save result (16 bits)
			MOV		ResultM,PTDD			
			BCLR	0,PTED				; no flags, ok
			JMP 	Display
sign_mult:
			MOV		#1,signoNegativo	;;if Contador=1 is negative
			BRA cont_mult



division:			; it verifies if there are special cases
					CLRH
					LDA		Operand2
					CMP		#1
					BNE		verificarSi0
					MOV		Operand1,Result
					MOV		Operand1,PTDD
					JMP 	setSign ; it is temporal
verificarSi0:		CMP		#0
					BNE		verificarCasoOF
					BSET	1,PTED ; send a flag to the LSB of the port noticing about zero division
					JMP 	ErrorMessage2 
					
verificarCasoOF:	CMP		#-1
					BNE		OperandoSinE
					LDX		Operand1
					CPX		#-128
					BNE		OperandoSinE; if it is equal, there is overflow in the case -128/-1
					BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error
					JMP 	ErrorMessage ; it is temporal
						
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
dividir:
					DIV
					STA		Result
					CLRA	; clear the 8 LSB of the dividend
					DIV		; divide the remainder which is located in the H register , A will have the 8-bits binary fraction of the operation
					
					;convert binary fraction to decimal
					MOV	#'.',puntoDecimal
					LDX		#10
					MUL
					STX		decimal1  ;Take the 8 most significant bits, they have the first decimal of the operation
					LDX		#10
					MUL		;Multiply the 8 least significant bits that are contained in A with #10
					STX		decimal1 + 1  ; Take the most significant bits, they have the second decimal of the operation
					LDA		decimal1
					ADD		#48
					STA		decimal1
					LDA		decimal1 +1
					ADD		#48
					STA		decimal1 +1
					
					
					CLRX	;Clear X register
					CLRH	;Clear H register					
					MOV		Result,PTDD
					BCLR	0,PTED ; clear the overflow flag
					BRA 	Display 
	
	
									
			
sum:
			LDA		Operand1
			ADD		Operand2; Add to check the overflow flag only
			;flag for checking status
			BLT		verificarNegativo ; N xor V = 1, desechar el caso donde N =1 y V = 0
			BPL		operacionCorrecta
			BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error (because of big positive numbers)
			JMP 	ErrorMessage ; it is temporal 
					
verificarNegativo:
			BMI		operacionCorrecta
			BSET	0,PTED ; send a flag to the LSB of the port noticing about overflow error (because of big negative numbers)
			JMP 	ErrorMessage ; it is temporal
operacionCorrecta:
			LDA		Operand1
			ADD		Operand2
			STA		Result; it modifies the CCR register
			MOV		Result,PTDD
			BCLR	0,PTED;
setSign:
			AND #%10000000 
			BEQ	Display
			LDA Result
			NEGA
			MOV	#1,signoNegativo
			STA	Result
				
;BCD DISPLAY
Display:		
				LDA		Result							
				CLRH						; from here all numbers are unsigned
				MOV 	#2, Contador3		; initial number of times to iterate into go here
				LDHX	#numeroBCD+5		;set the pointer with the numeroBCD address + 5
				STHX	pointerBCD
				LDA		PTCD				;check if the performed operation is multiplication
				AND		#%00000011
				CMP		#2
				BEQ 	Res_Multi			; if multi go to special case of display(16 bits)
				LDA		Result				;the result of any other operation is inside Result and has 8 bits
				CMP		#100
				BPL		RepeatBCD			;if sum,subtract or division do less operations to display
				MOV		#1,Contador3		; if the range is between 0 and 99, Contador3 change from 2 to 1
				BRA 	RepeatBCD

Res_Multi:		LDHX	ResultM				;result for multi (16 bits) doesnt fit in A
				CPHX	#100				;check if the number bigger than 100
				BMI		verify
				TXA							;if not, start from the beggining
				LDX 	#100
				BRA		LoopDispMult
				
verify:
				TXA							;if it is not bigger than 100 check if less than 10 to save immediately
				CMP		#10
				BMI		Exit
				MOV		#1, Contador3		;if its bigger than 10 and less than 100 do less loops to save the result
				BRA		Rep
				
LoopDispMult:	
				DIV
				PSHA		;save first quotient
				PSHH		;transfer the  first remainder to A
				PULA
				CLRH
Rep:			LDX		#10				
				DIV			;divide by 10 the first remainder 
				PSHA		;save quotient
				PSHH		;transfer actual remainder to A
				PULA	
goHere:			LDHX	pointerBCD		;pointer to save the ans
				ADD		#48
				STA		,X 		; store the remainder to the address pointed by pointerBCD
				DEC		pointerBCD+1
				PULA			;load the next quotients from Stack onto A		
				DBNZ 	Contador3,goHere		
				;finished saving the least significant units of the result
				;Continue with the others if they are
				MOV		#2,Contador3
				CMP		#100		;check if the quotient(of the very first DIV) is bigger than 100
				BPL		RepeatBCD
				CMP		#10			;if not check if its less than 10 go and save result
				BMI		Exit
				MOV		#1,Contador3	;if its between 10 and 100 do less iterations
								
				;Second part: here is the general display (less iterations for not MUL functions)
RepeatBCD:		CLRH
				LDX		#10
				DIV			; it divides A / #10
				PSHA		;save quotient onto Stack.We are going to pull later
				PSHH		; transfer remainder to A
				PULA

				LDHX	pointerBCD
				ADD		#48
				STA		,X 		; store the remainder to the address pointed by pointerBCD
				DEC		pointerBCD+1; pointer has 16 bits, then it is decremented the LSBs part
				PULA			;take the next value from stack. In this case the quotient
				DBNZ		Contador3,RepeatBCD
;finish display
				
Exit:			LDHX	pointerBCD
				ADD		#48		
				STA		,X				;store the last quotient
				DEC		pointerBCD+1
				LDA		signoNegativo	;load the flag for negative sign
				AND		#%00000001
				BEQ		CorrectOperation	;if its positive finish
				LDHX	pointerBCD
				LDA		#45
				STA		,X				;if not store the negative '-' in ascii before the number 
				
				
CorrectOperation: 	LDHX 	#CorrectOpMessage
					STHX	contadorAlerta
					BRA		RAMPointerAlert

ErrorMessage:		LDHX 	#AlertaOverFlow
					STHX	contadorAlerta
					BRA		RAMPointerAlert
					
ErrorMessage2:		LDHX 	#AlertaOperacionCero
					STHX	contadorAlerta
					BRA		RAMPointerAlert

RAMPointerAlert:
					LDHX	#mensajeAlerta
					STHX	contadorMensaje
					
LoopMessage:		LDHX	contadorAlerta
					LDA		,X	; load in A the data in address located at H:X register
					CMP		#0	; compare if the end-string byte ('\0')
					BEQ 	End_Program
					AIX		#1; increment 1 to X (address of alertaOverflow)
					STHX	contadorAlerta
					LDHX	contadorMensaje
					STA		,X
					AIX		#1
					STHX	contadorMensaje	
					BRA		LoopMessage
			
End_Program:			

			JMP 	StartAllAgain
			


AlertaOverFlow: 	DC.B "Error !! Sobrecarga en la operacion realizada"
FinalCadena:		DC.B 0
AlertaOperacionCero: DC.B "Error !! division por cero no permitida"
FinalCadena2:		DC.B 0
CorrectOpMessage: DC.B		"Operacion Correcta"
FinalCadena3:		DC.B 0


;*                 Interrupt Vectors                          *
;**************************************************************

            ORG	$FFFE
			DC.W  _Startup			; Reset
