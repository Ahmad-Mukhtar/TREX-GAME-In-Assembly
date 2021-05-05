[org 0x0100]
jmp start
sc: db 'score: ' ;printing score in left corner
val: dw 2078	;starting point of hurdle2
vall: dw 1918	;starting point of hurdle2
hurdval: dw 2078	;starting point of hurdle1
hurdval1: dw 1918	;starting point of hurdle1
hurdcount: dw 1		;for counting of 1st hurdle
hurdcount2: dw 1    ;for counting of hurdle 2
collision: dw 1920	;variable to check collision value of hurdle 1
collision1: dw 1920 ;variable to check collision value of hurdle 2
shapeofhurd1: dw 0x06B3 ;saving hurdleshape
shapeofhurd1a: dw 0x06DB 
shapeofhurd2: dw 0x06DB
shapeofhurd2a: dw 0x06DB
tickcount: dw 0 ;for storing score
gameover: dw 'Game over Your score is ' ;to print the game over
scoreindex: dw 308 ;saving index for printing score
oldisr: dd  0  ;storing old value of int9
oldisr1: dd 0  
countmovhurd: dw 0		;to move hurdle with time
checkingtoexit: dw 0 ;check to exit
speakerrr:
pusha
MOV     DX,6000          ; Number of times to repeat whole routine.

MOV     BX,1             ; Frequency value.

MOV     AL, 10110110B    ; The Magic Number (use this binary number only)
OUT     43H, AL          ; Send it to the initializing port 43H Timer 2.

NEXT_FREQUENCY:          ; This is were we will jump back to 2000 times.

MOV     AX, BX           ; Move our Frequency value into AX.

OUT     42H, AL          ; Send LSB to port 42H.
MOV     AL, AH           ; Move MSB into AL  
OUT     42H, AL          ; Send MSB to port 42H.

IN      AL, 61H          ; Get current value of port 61H.
OR      AL, 00000011B    ; OR AL to this value, forcing first two bits high.
OUT     61H, AL          ; Copy it to port 61H of the PPI Chip
                         ; to turn ON the speaker.

MOV     CX, 100          ; Repeat loop 100 times
DELAY_LOOP:              ; Here is where we loop back too.
LOOP    DELAY_LOOP       ; Jump repeatedly to DELAY_LOOP until CX = 0


INC     BX               ; Incrementing the value of BX lowers 
                         ; the frequency each time we repeat the
                         ; whole routine

DEC     DX               ; Decrement repeat routine count

CMP     DX, 0            ; Is DX (repeat count) = to 0
JNZ     NEXT_FREQUENCY   ; If not jump to NEXT_FREQUENCY
                         ; and do whole routine again.

                         ; Else DX = 0 time to turn speaker OFF

IN      AL,61H           ; Get current value of port 61H.
AND     AL,11111100B     ; AND AL to this value, forcing first two bits low.
OUT     61H,AL           ; Copy it to port 61H of the PPI Chip
                         ; to turn OFF the speaker.

popa
ret
clr: 
	push es
	push ax
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov di, 0 ; point di to top left column
nextloc: mov word [es:di], 0x0720 ; clear next char on screen
	add di, 2 ; move to next screen location
	cmp di, 4000 ; has the whole screen cleared
	jne nextloc ; if no clear next position
	pop di
	pop ax
	pop es
ret 
;for setting color Green in the background
setbackground:
	pusha
	mov ax,0xb800
	mov es,ax
	mov di,2240 ;starting point of game bar
	mov bx,4000 ;adding end limit
	mov al,0xDB
	mov ah,02h
dra:
	mov word [es:di],ax
	add di,2
	cmp di,bx
	jne dra
	popa
ret
;print the floor
draw:
	pusha
	mov ax,0xb800
	mov es,ax
	mov di,2080 ;starting point of game bar
	mov bx,di
	add bx,160  ;ending points
	mov al,0x2D
	mov ah,0Eh
draw_bar:
	mov word [es:di],ax
	add di,2
	cmp di,bx
	jne draw_bar
	popa
ret

score: ;print score : on left of screen
	cld  
	mov ax,0xb800
	mov es,ax
	mov di,290
	mov si,sc ;starting pointing of string score
	mov cx,7
	mov ah,0EH
printchr:
	lodsb
	stosw
	loop printchr
ret
;print dinasour on the ground
dinasour:
	pusha
	mov ax,0xb800
	mov es,ax
	mov di,800
	mov al,' '
	mov ah,07h
	mov word [es:di],ax			;printing space at jump location of dinasour's head
	mov di,1600
	mov al,01h
	mov ah,0EH
	mov word [es:di],ax
	mov di,960
	mov al,' '
	mov ah,07h
	mov word [es:di],ax
	mov di,1760
	mov al,0xB3
	mov ah,0Eh
	mov word [es:di],ax
	mov di,1120
	mov al,' '
	mov ah,07h
	mov word [es:di],ax
	mov di,1920
	mov al,0x13
	mov ah,0Eh
	mov word [es:di],ax
	popa
ret
;printing dinsour  above the ground
jumpdino:
	call speakerrr  ;sound of jump
	pusha 
	mov word [collision],1120
	mov word  [collision1],1120
	mov ax,0xb800 ;pointing to screen
	mov es,ax
	mov di,1600  
	mov al,' '
	mov ah,07H
	mov word [es:di],ax  ;printing space at previous location of dinasour's head
	mov di,800
	mov al,01h
	mov ah,0EH
	mov word [es:di],ax ;printing head of dinasour above at previous location of dinasour
	mov di,1760
	mov al,' '
	mov ah,07H
	mov word [es:di],ax		;printing space at previous location of dinasour's body
	mov di,960
	mov al,0xB3
	mov ah,0Eh
	mov word [es:di],ax     ;printing body of dinasour above at previous location of dinasour
	mov di,1920
	mov al,' '
	mov ah,07H
	mov word [es:di],ax		;printing space at previous location of dinasour's legs
	mov di,1120
	mov al,0x13
	mov ah,0Eh
	mov word [es:di],ax		;printing body of dinasour above at previous location of dinasour
	call movinghurdles
	call delayy
	call dinasour	;printing dinasour on the ground
	popa 
ret
;delay for dinasour jump
delayy:
	pusha
	pushf
	mov cx,1000
	mydelay:
	mov bx,600     ;; increase this number if you want to add more delay, and decrease this number if you want to reduce delay.
	mydelay1:
	dec bx
	jnz mydelay1
	loop mydelay
	call timer
	popf
	popa
ret
;delay for hurdles movement
delay:
		pusha
		mov  cx, 200
		
again:
		mov di, 200
	
nested:		
		sub di, 1
		cmp di, 0
		jne nested
		loop again
		popa
ret
;print and move hurdles
movinghurdles:
		mov word bp,[shapeofhurd1]
		mov word bx,[shapeofhurd1a]
		mov word cx,[shapeofhurd2]
		mov word dx,[shapeofhurd2a]
		mov di,[hurdval]   ;point of hurdles
		mov si,[hurdval1]  ;point of hurdles	
		mov word[es:di],bp   ;printing the hurdles
		mov word[es:si],bx		;printing the hurdles
		cmp di,[collision]		;check if dinasour collided or not with the hurdle
		je outofrange			;if yes exit
		cmp di, 1920         	;cmp with the ending point of hurdle
		je set					;yes,then change the value to their starting point
		cmp word [val],di		;cmp with di of first hurdle if less then keep moving the second hurdle
		jl second				
		cmp di,2000				;cmp with the di of first hurdle if in the middle then bring out the next hurdle
		jnl move				;else  move only the first hurdle
		second:
		mov word [hurdval1],si	;saving the index of hurdle1 
		mov word [hurdval],di	;saving the index of hurdle1 
		mov di,[val]			;getting the index of second hurdle
		mov si,[vall]			;getting the index of second hurdle
		call delay
		mov word[es:di], 0x0720	;print space on last value
		mov word[es:si],0x0720	;print space on last value
		sub si, 2
		sub di,2
		mov word[es:di],cx      ;print the hurdle
		mov word[es:si],dx		;print the hurdle
		mov word [vall],si		;storing the new index of second hurdle
		mov word [val],di		;storing the new index of second hurdle
		cmp di,[collision1]		;check the collision of hurdle 2
		je outofrange			;if yes then exit
		cmp di,1920				;cmp with the ending point
		je erroravoid				;change the value to start
vom:
		mov di,[hurdval]   		;storing back the value of hurdle1
		mov si,[hurdval1]  		;storing back the value of hurdle1
		move:
		call delay
		mov word[es:di], 0x0720  ;print space on previous position
		mov word[es:si],0x0720	 ;print space on previous position
		sub si,2				 ;updating next point
		mov word [hurdval1],si	 ;storing back the new value of hurdl1
		sub di,2				 ;updating next point
		mov word [hurdval],di	;storing back the new value of hurdl1
		outofrange:				;avoiding short jump error
		jmp ter		;for terminate after moving the hurdle
erroravoid:		
		jmp set2
set:
		mov word[es:di], 0x0720		;print space on previous position oif hurdle1
		mov word[es:si],0x0720		;print space on previous position oif hurdle1
		mov word [hurdval],2078		;moving again the starting point of hurdle1
		mov word [hurdval1],1918	;moving again the starting point of hurdle1
		mov di,[hurdval]   ;storing the strating point of hurdles
		mov si,[hurdval1]  ;storing the strating point of hurdles
		add word [hurdcount],1	;increment the hurdle1 count
		mov ax,[hurdcount]			
		mov bl,2
		div bl
		cmp ah,0				;if hurdle count is even then change the hurdleshape
		jz setthurd
		mov word [shapeofhurd1],0x06B3			;storing the shapes value
		mov word [shapeofhurd1a],0x06DB
		call dinasour			;to avoid the dissappearing of dinasour if hurdle collide
		jmp ter	
setthurd:
		mov word [shapeofhurd1],0x06DB			;change the value to change hurdle shape
		mov word [shapeofhurd1a],0x06DF			;change the value to change hurdle shape
		mov word [hurdval1],2080;as the shape is changed so changing the index to print
		mov si,[hurdval1]        ;same as above
		call dinasour			;to avoid the dissappearing of dinasour if hurdle collide
		jmp ter
set2:
		mov word[es:di], 0x0720		;print space on previos value
		mov word[es:si],0x0720		;print space on previos value
		mov word [val],2078			;moving the starting point of hurdle2
		mov word [vall],1918		;moving the starting point of hurdle2
		add word [hurdcount2],1		;increment hurdlecount of hurdle2
		push bx						;saving the values
		push bp
		mov ax,[hurdcount2]			
		mov bl,2
		div bl
		cmp ah,0				;if hurdlecount is even then change its shape else not
		jz setthurd2
		pop bp					;poping the save values above
		pop bx
		mov word [shapeofhurd2],0x06DB			;value of hurdle shape
        mov word [shapeofhurd2a],0x06DB					;value of hurdle shape
		call dinasour
jmp vom
setthurd2:
	mov word [shapeofhurd2],0x06DB		;changing value of hurdle shape
	mov word [shapeofhurd2a],0x06DE		;changing value of hurdle shape
	pop bp				;poping above stored value
	pop bx
	call dinasour		;to avoid the dissappearing of dinasour if hurdle collide
	jmp vom
ter:
ret
kbisr:
	in al, 0x60 ; read a char from keyboard port
	cmp al, 0x39 ; is the key space 
	jne exit ; no goto exit
	call jumpdino
exit:
	mov al, 0x20
	out 0x20, al ; send EOI to PIC
	iret 
;code for printing the number score taken from book
printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
nextdigit: 
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di,[scoreindex] ; saving index to print score
nextpos: 
	pop dx ; remove a digit from the stack
	mov dh, 0x0E ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	pop bp
	ret 2
;function to increses the score count and printing it
timer: 
	pusha
	push word [tickcount]
	call printnum ; print tick count
	add word [tickcount],1
	popa
ret 
maingame:
		mov ax, 0xb800
		mov es, ax ; point es to video memory
moving:
		inc word [countmovhurd]  ;to move hurdle with time
		cmp word [countmovhurd],2
		jne quit
		call timer			;print score 
		call movinghurdles	;move hurdles
		mov word [countmovhurd],0
		cmp di,[collision]	;compare the hurdle1 position value with collision if same then game over
		je changeee
		cmp di,[collision1]	;compare the hurdle2 position value with collision if same then game over
		je changeee
		mov word [collision],1920			;storing the value of collision f dinasour jumps the they will be chenged so reverting them to previos value
		mov word  [collision1],1920
		jmp quit ;exit the isr
changeee:
mov word [checkingtoexit],1
quit:
	mov al, 0x20
	out 0x20, al 
	iret
start:
call clr
call hide_cursor	;to hide cursor from screen
call show_title		;print menu taken from internet changed by printing trex
call clr			;clear the menu
call setbackground 	;set background colour
call draw			;print ground
call dinasour		;print dinasour
call score			;print the string score
call timer			;print value of score
xor ax, ax		
mov es, ax 
mov ax, [es:9*4]
mov [oldisr], ax 	;saving the isr value in a variable
mov ax, [es:9*4+2]
mov [oldisr+2], ax 	;saving the isr value in a variable
mov ax, [es:8*4]
mov [oldisr1],ax
mov ax, [es:8*4+2]
mov [oldisr1+2],ax
cli					;disable interrupts
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs 	; store segment at n*4+2
mov word [es:8*4],maingame ; store offset at n*4
mov [es:8*4+2], cs ; store segment at n*4+
sti ; enable interrupts					;enable interupts

infi:
cmp word [checkingtoexit],1  ;check if collided then exit
jne infi

call hide_cursor	;if collided then clear the screen
call clr			;for clearing the screen
cld    
mov ax,0xb800
mov es,ax
mov di,1970
mov si,gameover 	;starting point of string  gameover
mov cx,24   		;size of string
mov ah,0EH			;colour of the string
prin:				;print the string 
lodsb
stosw
loop prin
mov word [scoreindex],di ;storing the value to print the final score in the middle
sub word [tickcount],1;as final score is increased by one after the function is last called so subtracting one to get the right value
call timer
xor ax,ax			
mov es,ax
mov ax, [oldisr] 		; read old offset
 mov bx, [oldisr+2] 	
mov cx,[oldisr1]
mov dx,[oldisr1+2]
cli 					; disable interrupts
mov [es:9*4], ax		;restore old offset
mov [es:9*4+2], bx 	
mov [es:8*4],cx
mov [es:8*4+2],dx
sti
mov ah,0  				;wait for key so that the user can see the final score as long as he wants
int 16h					;interupt for getting key
call clr				;clear the screen oso the final score screen is cleared
mov ax,0x4c00			;exiting the program
int 21h
  
sleep:
			mov ah, 0
			int 1ah
			mov bx, dx
		.wait:
			mov ah, 0
			int 1ah
			sub dx, bx
			cmp dx, si
			jl .wait
			ret

	hide_cursor:
			mov ah, 02h
			mov bh, 0
			mov dh, 25
			mov dl, 0
			int 10h
			ret

	clear_keyboard_buffer:
			mov ah, 1
			int 16h
			jz .end
			mov ah, 0h ; retrieve key from buffer
			int 16h
			jmp clear_keyboard_buffer
		.end:
			ret

	exit_process:
			mov ah, 4ch
			int 21h
			ret

	buffer_clear:
			mov bx, 0
		.next:	
			mov byte [buffer + bx], ' '
			inc bx
			cmp bx, 2000
			jnz .next
			ret
		
	; in:
	;	bl = char
	;	cx = col
	;	dl = row
	buffer_write:
		mov di, buffer
		mov al, 80
		mul dl
		add ax, cx
		add di, ax
		mov byte [di], bl
		ret
	
	; in:
	;	cx = col
	;	dx = row
	; out: 
	;	bl = char
	buffer_read:
		mov di, buffer
		mov al, 80
		mul dl
		add ax, cx
		add di, ax
		mov bl, [di]
		ret
	
	; in:
	;	si = string address
	;	di = buffer destination offset
	buffer_print_string:
		.next:
			mov al, [si]
			cmp al, 0
			jz .end
			mov byte [buffer + di], al
			inc di
			inc si
			jmp .next
		.end:
			ret
	buffer_render:
			mov ax, 0b800h
			mov es, ax
			mov di, buffer
			mov si, 0
		.next:
			mov bl, [di]
			cmp bl, 8
			jz .is_snake
			cmp bl, 4
			jz .is_snake
			cmp bl, 2
			jz .is_snake
			cmp bl, 1
			jz .is_snake
			jmp .write
		.is_snake:
			mov bl, 219
		.write:
			mov byte [es:si], bl
			inc di
			add si, 2
			cmp si, 4000
			jnz .next
			ret

	show_title:
			call buffer_clear
			call buffer_render
			mov si, 18
			call sleep
			mov si, 0
		.next:
			mov bx, [.title + si]
			mov byte [buffer + bx], 219
			push si
			call buffer_render
			mov si, 1
			call sleep
			pop si
			add si, 2
			cmp si, 220
		   jl .next
		   
		   
		   
		   
		   
		   
			mov si, .text_1
			mov di, 1620
			call buffer_print_string
			mov si, .text_2
			mov di, 1781
			call buffer_print_string
			call clear_keyboard_buffer
		.wait_for_key:
			mov si, .text_4
			mov di, 1385
			call buffer_print_string
			call buffer_render
			mov si, 5
			call sleep
			mov ah, 1
			int 16h
			jnz .continue
			mov si, .text_3
			mov di, 1385
			call buffer_print_string
			call buffer_render
			mov si, 10
			call sleep
			mov ah, 1
			int 16h
			jz .wait_for_key
		.continue:
			mov ah, 0
			int 16h
			ret
		.title:
			dw 0343,0342, 0341, 0340, 0339, 0338, 0337, 0336, 0335
			dw 0419,0499,579,659,739,819,899,979
			dw 0985, 0905, 0825, 0745, 0665, 0585, 0505, 0425, 0345
			dw 0346,0347,0348,0349,0350,0351
			dw  0592,0512,0432,0352,0666,0667,0668,0669,0670,0671,0672
			dw  0746,0747,828,0829,0910,0911,0992,0993
			dw 0355,435,515,595,675,755,835,915,995
			dw 0356, 0357, 0358, 0359, 0360,0361,0362
			dw 0996,0997,0998,0999,1000,1001,1002
			dw 0676, 0677, 0678, 0679, 0680, 0681
			dw 0373,0452,0531,0610,0689,0767,0846,0925,1004
			dw 0364,445,526,607,688,770,0851,0932,1013
						
		.text_1:
			db "DEVELOPED BY Chaudhry&Ahmad (C) 2019", 0
		.text_2:
			db "WRITTEN IN ASSEMBLY 8086 LANGUAGE :)", 0
		.text_3:
			db "PRESS ANY KEY TO START", 0
		.text_4:
			db "                      ", 0

section .bss
	
		buffer resb 2000




; cx should be median of array
;mov ax, 0x4c00
;int 0x21
