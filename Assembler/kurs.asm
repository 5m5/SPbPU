%define world_r 30
%define world_c 70
%define key_buf_size 512
%define pl_ground 26
%define max_jump_h 10

segment .data
	; 2d array dimintions
	wld_r equ world_r
	wld_c equ world_c
	wld_a equ (world_c * world_r)	

	; path to keyboard node
	k_input db "/dev/input/event3", 0x0

	; player cords
	pl_x db 30
	pl_y db pl_ground
	; player jump boolean
	pl_jump db 1
	; obstical col val <- every byte = one obs <- last byte is always 0x0
	obs_c db 30, 50, 0x0 ; <- only used to setup obs in .bss
	obs_c_size equ $-obs_c
	; fixed row pos for all obsticals
	obs_row equ 24
	; starting pos for obs
	obs_start equ world_c - 15

	; game characters
	return db 0xA
	player db '@'
	wall db '~'
	space db ' '
	ob_c db '#'
	
	; bash stuff for clearing screen
	c_term db 27, "[H", 27, "[2J" 
	c_term_len equ $-c_term

	time_struct:
		sec dd 0
		usec dd 0

segment .bss
	; 2d array world_r * world_c <- note 2d arrays r a lie 
	world resb wld_a
	; random adress for storing things in regesters
	_dword resd 1
	; buffer for key presses
	key_buf resb key_buf_size
	; obstical col val <- every byte = one obs <- last byte is always 0x0
	obs resb obs_c_size
	
segment .text
	global main

_init_world:
	mov eax, -1
	mov ecx, -1
	jmp loop_r

	exit:
		ret
	
	loop_c:
		; place walls around perimeter
		cmp ecx, 0
		jz place_wall
		
		mov ebx, wld_c
		sub ebx, 1
		cmp ecx, ebx
		jz place_wall
	
		cmp eax, 0
		jz place_wall

		mov ebx, wld_r
		sub ebx, 1
		cmp eax, ebx
		jz place_wall

		; place spaces
		jmp place_space	
	
		place_space:
			; get world 2d array mem offset in register ebx
			push eax
			; calculate off set in regester edx and place ascii block in mem
			mov edx, wld_c
			mul edx	
			mov ebx, world
			add ebx, eax
			add ebx, ecx
			pop eax	
			; put char in array at ebx
			mov edx, [space]
			mov [ebx], edx
			jmp iterate
		
		place_player:
			; get world 2d array mem offset in register ebx
			push eax
			; calculate off set in regester edx and place ascii block in mem
			mov edx, wld_c
			mul edx	
			mov ebx, world
			add ebx, eax
			add ebx, ecx
			pop eax	
			; put char in array at ebx
			mov edx, [player]
			mov [ebx], edx
			jmp iterate

		place_wall:
			; get world 2d array mem offset in register ebx
			push eax
			; calculate off set in regester edx and place ascii block in mem
			mov edx, wld_c
			mul edx	
			mov ebx, world
			add ebx, eax
			add ebx, ecx
			pop eax	
			; put char in array at ebx
			mov edx, [wall]
			mov [ebx], edx
			jmp iterate
			
		iterate:	
			add ecx, 1
			cmp ecx, wld_c
			jnz loop_c	
			jmp loop_r
	
	loop_r:
		add eax, 1
		mov ecx, 0
		cmp eax, wld_r
		jnz loop_c
		jmp exit

; draw player at pl_x, pl_y
_place_player:
	; empty regesters
	mov eax, 0
	mov edx, 0
	; draw player
	mov al, byte [pl_y]
	mov dl, byte [pl_x]
	mov ch, '*'
	call _change_map
	add eax, 1
	call _change_map
	sub eax, 1
	add edx, 1
	call _change_map
	add eax, 2
	call _change_map
	sub eax, 1
	sub edx, 2
	call _change_map
	sub edx, 1
	call _change_map
	sub edx, 1
	add eax, 1
	call _change_map
	ret

; draw obsticals
_draw_obs:
	; loop through bytes in obs until 0x0
	; ebx hold current pos in obs
	mov ebx, -1
	; enter loop
	jmp _d_o_iterate	

	; Adress for returning after a cmp	
	_d_o_ret:
		ret

	_d_o_iterate:
		; increment ebx
		add ebx, 1
		
		; check to see if loop has reached end of obsticals
		mov al, 0x0
		cmp al, [obs + ebx]
		jz _d_o_ret

		; draw obstical at pos
		push eax
		push ebx
		push ecx
		push edx

		mov ecx, 0
		mov edx, 0
		mov ch, [ob_c]
		mov eax, obs_row
		mov dl, [obs + ebx]
		call _change_map
		add edx, 1
		add eax, 1
		call _change_map
		add eax, 1
		call _change_map
		add edx, 1
		call _change_map
		add edx, 1
		sub eax, 1
		call _change_map
		sub edx, 4
		add eax, 1
		call _change_map
		add edx, 1
		add eax, 1
		call _change_map
		add edx, 1
		add eax, 1
		call _change_map

		pop edx
		pop ecx
		pop ebx
		pop eax

		jmp _d_o_iterate	


; set byte in array via row and col
; regesters eax = row, edx = col, and ch = new byte
_change_map:
	; get adress of byte
	; to replace bassed of row and col
	mov ebx, world
	push eax
	push edx
	mov edx, wld_c
	mul edx
	add ebx, eax
	pop edx
	pop eax
	add ebx, edx
	; replace byte
	mov [ebx], ch
	ret
	
_print_world:
	mov ecx, -1
	jmp loop_row
	print_row:
		; get to row
		mov eax, wld_c
		mul ecx
		mov ebx, world
		add ebx, eax
		mov [_dword], ebx
		; print row
		push ecx
		mov eax, 4
		mov ebx, 1
		mov ecx, [_dword]
		mov edx, wld_c
		int 0x80
		; goto new line
		mov eax, 4
		mov ebx, 1
		mov ecx, return
		mov edx, 1
		int 0x80
		pop ecx
		; go back to loop	
		jmp loop_row
	loop_row:
		add ecx, 1
		cmp ecx, wld_r
		jnz print_row
		ret

; game logic stuff
_game_l:
	; move all objects closer
	mov eax, -1
	call _game_l_iter
	; jump if jump is true
	mov ebx, 1
	cmp ebx, pl_jump
	; let player move 
	call _game_l_move
	; sleep 
	call _sleep_p
	; read key input
	call _game_l_readk_input
	ret
	
	; read key input from device node	
	_game_l_readk_input:
		mov eax, 5
		mov ebx, k_input
		mov ecx, 0
		int 0x80

		mov eax, 3
		mov ebx, eax
		mov ecx, key_buf
		mov edx, key_buf_size
		int 0x80

		mov eax, 4
		mov ebx, 1
		mov ecx, key_buf
		mov edx, key_buf_size
		int 0x80
		ret

	; move player
	_game_l_move:
		; jump if jump is true
		mov ebx, 0
		mov bl, 1
		cmp bl, [pl_jump]
		jz _game_l_move_up
		jmp _game_l_move_down

		_game_l_move_up:
			sub [pl_y], byte 1
			jmp _game_l_move_ret

		_game_l_move_down:
			; make sure player is not bellow pl_ground before falling
			mov eax, 0
			mov al, byte [pl_y]
			cmp eax, pl_ground
			jge _game_l_move_ret
			; if player y is less than pl_ground then fall!
			add [pl_y], byte 1
			jmp _game_l_move_ret

		_game_l_move_ret:
			ret

	_game_l_iter_ret:
		ret

	_game_l_reset_obs:
		mov [ebx], byte obs_start
		jmp _game_l_iter

	_game_l_iter:
		add eax, 1
		mov ecx, 0
		mov cl, 0x0
		cmp cl, [obs + eax]
		jz _game_l_iter_ret
		; move object close to player
		mov ebx, obs
		add ebx, eax
		sub [ebx], byte 1
		; if object is at 10 move back to right side of array
		mov ecx, 0
		mov cl, 10
		cmp cl, [obs + eax]
		jz _game_l_reset_obs
		; next iteration of loop
		jmp _game_l_iter			


; setup game and allocate data
_setup_game:
	mov eax, -1
	; set sleep time in time_struct
	mov [sec], dword 0
	mov [usec], dword 350000000
	; set up game objects
	call _setup_game_obs_iter
	;ret _setup_game
	ret

	_s_g_obs_ret:
		mov ebx, obs
		add ebx, obs_c_size
		mov [ebx], byte 0x0
		ret

	_setup_game_obs_iter:
		add eax, 1
		mov cl, 0x0
		cmp cl, [obs_c + eax]
		jz _s_g_obs_ret

		mov ebx, obs
		add ebx, eax

		mov edx, [obs_c + eax]	
		mov [ebx], edx

		jmp _setup_game_obs_iter

; sleep process
_sleep_p:
	mov eax, 162
	mov ebx, time_struct
	mov ecx, 0
	int 0x80
	
; clear console
_clear_con:
	mov eax, 4
	mov ebx, 1
	mov ecx, c_term
	mov edx, c_term_len
	int 0x80
	ret
		
; end proc
_exit: 
	mov eax, 1
	mov ebx, 0
	int 0x80

_game_loop:
	; clear 2d array
	call _init_world
	; draw player on array
	call _place_player
	; draw obsticals on array
	call _draw_obs
	; clear console
	call _clear_con
	; print array to console
	call _print_world
	; apply game logic
	call _game_l
	; repeat
	jmp _game_loop
	ret
main:
	call _setup_game
	call _game_loop
	; exit
	jmp _exit
	