global start
extern printf

section .text

start:
	; init stack
	mov rax, 9 ; mmap
	mov rdi, 0 ; page aligned
	mov rsi, 8388608 ; 8 MiB
	mov rdx, 3 ; rw
	mov r10, 34 ; map type = memory
	mov r8, -1 ; not a fd
	mov r9, 0 ; offset
	syscall
	mov rbp, rsp
	mov r15, done ; setup return address

mov RSP , R15
mov RAX , 24878
jmp RSP
consequence$115:
mov RAX , 318
jmp RBX
alternative$116:
mov RAX , RAX
add RAX , RCX
jmp RBX
continuation$117:
cmp RSP , 6
jne consequence$115
jmp alternative$116
consequence$118:
mov RSP , 6
jmp continuation$117
alternative$119:
mov RSP , 14
jmp continuation$117
continuation$120:
cmp RSP , 6
jne consequence$118
jmp alternative$119
consequence$121:
mov RSP , 14
jmp continuation$120
alternative$122:
mov RSP , 6
jmp continuation$120
consequence$123:
mov RAX , 62
jmp RBX
alternative$124:
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$121
jmp alternative$122
continuation$125:
cmp RSP , 6
jne consequence$123
jmp alternative$124
consequence$126:
mov RSP , 6
jmp continuation$125
alternative$127:
mov RSP , 14
jmp continuation$125
continuation$128:
cmp RSP , 6
jne consequence$126
jmp alternative$127
consequence$129:
mov RSP , 14
jmp continuation$128
alternative$130:
mov RSP , 6
jmp continuation$128
add$0:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$129
jmp alternative$130
consequence$131:
mov RAX , 4414
jmp RBX
alternative$132:
mov RAX , RAX
sub RAX , RCX
jmp RBX
continuation$133:
cmp RSP , 6
jne consequence$131
jmp alternative$132
consequence$134:
mov RSP , 6
jmp continuation$133
alternative$135:
mov RSP , 14
jmp continuation$133
continuation$136:
cmp RSP , 6
jne consequence$134
jmp alternative$135
consequence$137:
mov RSP , 14
jmp continuation$136
alternative$138:
mov RSP , 6
jmp continuation$136
consequence$139:
mov RAX , 4158
jmp RBX
alternative$140:
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$137
jmp alternative$138
continuation$141:
cmp RSP , 6
jne consequence$139
jmp alternative$140
consequence$142:
mov RSP , 6
jmp continuation$141
alternative$143:
mov RSP , 14
jmp continuation$141
continuation$144:
cmp RSP , 6
jne consequence$142
jmp alternative$143
consequence$145:
mov RSP , 14
jmp continuation$144
alternative$146:
mov RSP , 6
jmp continuation$144
sub$3:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$145
jmp alternative$146
consequence$147:
mov RAX , 8510
jmp RBX
alternative$148:
mov RSP , RAX
shr RSP , 3
mov RAX , RSP
imul RAX , RCX
jmp RBX
continuation$149:
cmp RSP , 6
jne consequence$147
jmp alternative$148
consequence$150:
mov RSP , 6
jmp continuation$149
alternative$151:
mov RSP , 14
jmp continuation$149
continuation$152:
cmp RSP , 6
jne consequence$150
jmp alternative$151
consequence$153:
mov RSP , 14
jmp continuation$152
alternative$154:
mov RSP , 6
jmp continuation$152
consequence$155:
mov RAX , 8254
jmp RBX
alternative$156:
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$153
jmp alternative$154
continuation$157:
cmp RSP , 6
jne consequence$155
jmp alternative$156
consequence$158:
mov RSP , 6
jmp continuation$157
alternative$159:
mov RSP , 14
jmp continuation$157
continuation$160:
cmp RSP , 6
jne consequence$158
jmp alternative$159
consequence$161:
mov RSP , 14
jmp continuation$160
alternative$162:
mov RSP , 6
jmp continuation$160
mul$6:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$161
jmp alternative$162
consequence$163:
mov RAX , 14
jmp RBX
alternative$164:
mov RAX , 6
jmp RBX
consequence$165:
mov RAX , 12606
jmp RBX
alternative$166:
cmp RCX , RAX
jl consequence$163
jmp alternative$164
continuation$167:
cmp RSP , 6
jne consequence$165
jmp alternative$166
consequence$168:
mov RSP , 6
jmp continuation$167
alternative$169:
mov RSP , 14
jmp continuation$167
continuation$170:
cmp RSP , 6
jne consequence$168
jmp alternative$169
consequence$171:
mov RSP , 14
jmp continuation$170
alternative$172:
mov RSP , 6
jmp continuation$170
consequence$173:
mov RAX , 12350
jmp RBX
alternative$174:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$171
jmp alternative$172
continuation$175:
cmp RSP , 6
jne consequence$173
jmp alternative$174
consequence$176:
mov RSP , 6
jmp continuation$175
alternative$177:
mov RSP , 14
jmp continuation$175
continuation$178:
cmp RSP , 6
jne consequence$176
jmp alternative$177
consequence$179:
mov RSP , 14
jmp continuation$178
alternative$180:
mov RSP , 6
jmp continuation$178
lt$9:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$179
jmp alternative$180
consequence$181:
mov RAX , 14
jmp RBX
alternative$182:
mov RAX , 6
jmp RBX
consequence$183:
mov RAX , 16702
jmp RBX
alternative$184:
cmp RCX , RAX
jg consequence$181
jmp alternative$182
continuation$185:
cmp RSP , 6
jne consequence$183
jmp alternative$184
consequence$186:
mov RSP , 6
jmp continuation$185
alternative$187:
mov RSP , 14
jmp continuation$185
continuation$188:
cmp RSP , 6
jne consequence$186
jmp alternative$187
consequence$189:
mov RSP , 14
jmp continuation$188
alternative$190:
mov RSP , 6
jmp continuation$188
consequence$191:
mov RAX , 16446
jmp RBX
alternative$192:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$189
jmp alternative$190
continuation$193:
cmp RSP , 6
jne consequence$191
jmp alternative$192
consequence$194:
mov RSP , 6
jmp continuation$193
alternative$195:
mov RSP , 14
jmp continuation$193
continuation$196:
cmp RSP , 6
jne consequence$194
jmp alternative$195
consequence$197:
mov RSP , 14
jmp continuation$196
alternative$198:
mov RSP , 6
jmp continuation$196
gt$12:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$197
jmp alternative$198
consequence$199:
mov RAX , 14
jmp RBX
alternative$200:
mov RAX , 6
jmp RBX
consequence$201:
mov RAX , 20798
jmp RBX
alternative$202:
cmp RCX , RAX
je consequence$199
jmp alternative$200
continuation$203:
cmp RSP , 6
jne consequence$201
jmp alternative$202
consequence$204:
mov RSP , 6
jmp continuation$203
alternative$205:
mov RSP , 14
jmp continuation$203
continuation$206:
cmp RSP , 6
jne consequence$204
jmp alternative$205
consequence$207:
mov RSP , 14
jmp continuation$206
alternative$208:
mov RSP , 6
jmp continuation$206
consequence$209:
mov RAX , 20542
jmp RBX
alternative$210:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$207
jmp alternative$208
continuation$211:
cmp RSP , 6
jne consequence$209
jmp alternative$210
consequence$212:
mov RSP , 6
jmp continuation$211
alternative$213:
mov RSP , 14
jmp continuation$211
continuation$214:
cmp RSP , 6
jne consequence$212
jmp alternative$213
consequence$215:
mov RSP , 14
jmp continuation$214
alternative$216:
mov RSP , 6
jmp continuation$214
eq$15:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$215
jmp alternative$216
consequence$217:
mov RAX , 14
jmp RBX
alternative$218:
mov RAX , 6
jmp RBX
consequence$219:
mov RAX , 24894
jmp RBX
alternative$220:
cmp RCX , RAX
jle consequence$217
jmp alternative$218
continuation$221:
cmp RSP , 6
jne consequence$219
jmp alternative$220
consequence$222:
mov RSP , 6
jmp continuation$221
alternative$223:
mov RSP , 14
jmp continuation$221
continuation$224:
cmp RSP , 6
jne consequence$222
jmp alternative$223
consequence$225:
mov RSP , 14
jmp continuation$224
alternative$226:
mov RSP , 6
jmp continuation$224
consequence$227:
mov RAX , 24638
jmp RBX
alternative$228:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$225
jmp alternative$226
continuation$229:
cmp RSP , 6
jne consequence$227
jmp alternative$228
consequence$230:
mov RSP , 6
jmp continuation$229
alternative$231:
mov RSP , 14
jmp continuation$229
continuation$232:
cmp RSP , 6
jne consequence$230
jmp alternative$231
consequence$233:
mov RSP , 14
jmp continuation$232
alternative$234:
mov RSP , 6
jmp continuation$232
lte$18:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$233
jmp alternative$234
consequence$235:
mov RAX , 14
jmp RBX
alternative$236:
mov RAX , 6
jmp RBX
consequence$237:
mov RAX , 28990
jmp RBX
alternative$238:
cmp RCX , RAX
jge consequence$235
jmp alternative$236
continuation$239:
cmp RSP , 6
jne consequence$237
jmp alternative$238
consequence$240:
mov RSP , 6
jmp continuation$239
alternative$241:
mov RSP , 14
jmp continuation$239
continuation$242:
cmp RSP , 6
jne consequence$240
jmp alternative$241
consequence$243:
mov RSP , 14
jmp continuation$242
alternative$244:
mov RSP , 6
jmp continuation$242
consequence$245:
mov RAX , 28734
jmp RBX
alternative$246:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$243
jmp alternative$244
continuation$247:
cmp RSP , 6
jne consequence$245
jmp alternative$246
consequence$248:
mov RSP , 6
jmp continuation$247
alternative$249:
mov RSP , 14
jmp continuation$247
continuation$250:
cmp RSP , 6
jne consequence$248
jmp alternative$249
consequence$251:
mov RSP , 14
jmp continuation$250
alternative$252:
mov RSP , 6
jmp continuation$250
gte$21:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$251
jmp alternative$252
consequence$253:
mov RAX , 14
jmp RBX
alternative$254:
mov RAX , 6
jmp RBX
consequence$255:
mov RAX , 33086
jmp RBX
alternative$256:
cmp RCX , RAX
jne consequence$253
jmp alternative$254
continuation$257:
cmp RSP , 6
jne consequence$255
jmp alternative$256
consequence$258:
mov RSP , 6
jmp continuation$257
alternative$259:
mov RSP , 14
jmp continuation$257
continuation$260:
cmp RSP , 6
jne consequence$258
jmp alternative$259
consequence$261:
mov RSP , 14
jmp continuation$260
alternative$262:
mov RSP , 6
jmp continuation$260
consequence$263:
mov RAX , 32830
jmp RBX
alternative$264:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$261
jmp alternative$262
continuation$265:
cmp RSP , 6
jne consequence$263
jmp alternative$264
consequence$266:
mov RSP , 6
jmp continuation$265
alternative$267:
mov RSP , 14
jmp continuation$265
continuation$268:
cmp RSP , 6
jne consequence$266
jmp alternative$267
consequence$269:
mov RSP , 14
jmp continuation$268
alternative$270:
mov RSP , 6
jmp continuation$268
neq$24:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$269
jmp alternative$270
consequence$271:
mov RAX , 14
jmp RBX
alternative$272:
mov RAX , 6
jmp RBX
isInt$27:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 7
cmp RSP , 0
je consequence$271
jmp alternative$272
consequence$273:
mov RAX , 14
jmp RBX
alternative$274:
mov RAX , 6
jmp RBX
isBool$29:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 6
je consequence$273
jmp alternative$274
consequence$275:
mov RAX , 14
jmp RBX
alternative$276:
mov RAX , 6
jmp RBX
isEmpty$31:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 22
je consequence$275
jmp alternative$276
consequence$277:
mov RAX , 14
jmp RBX
alternative$278:
mov RAX , 6
jmp RBX
isVoid$33:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 30
je consequence$277
jmp alternative$278
consequence$279:
mov RAX , 14
jmp RBX
alternative$280:
mov RAX , 6
jmp RBX
isChar$35:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 62
je consequence$279
jmp alternative$280
consequence$281:
mov RAX , 6
jmp RSP
alternative$282:
mov RAX , 14
jmp RSP
not$37:
mov RSP , R15
mov RAX , RDI
cmp RAX , 6
jne consequence$281
jmp alternative$282
done:
	mov r8, 0b111 ; Int tag
	and r8, rax
	cmp r8, 0
	je print_int
	mov r8, 0xFF
	and r8, rax
	cmp r8, 0b1110 ; True tag
	je print_true
	cmp r8, 0b110 ; False tag
	je print_false
	cmp r8, 0b10110 ; Empty tag
	je print_empty
	cmp r8, 0b11110 ; Void tag
	je print_void
	cmp r8, 0b101110 ; Char tag
	je print_char
	
print_int:
	shr rax, 3
	mov r8, 10 ; we need to div by 10
	mov r10, msg ; r10 becomes pointer into msg that we traverse backwards
	add r10, len
	xor r11, r11 ; r11 becomes the counter for the string length
	cmp rax, 0
	jl rax_negative
rax_positive:
print_rax_positive:
	xor rdx, rdx
	dec r10 
	inc r11
	div r8
	add rdx, '0'
	mov [r10], dl
	cmp rax, 0
	jne print_rax_positive
	jmp print
rax_negative:
	neg rax
print_rax_negative:
	xor rdx, rdx
	dec r10 
	inc r11
	div r8
	add rdx, '0'
	mov [r10], dl
	cmp rax, 0
	jne print_rax_negative
	dec r10
	inc r11
	mov BYTE [r10], '-'
	jmp print
print_true:
	mov r10, true
	mov r11, trueLen
	jmp print
print_false:
	mov r10, false
	mov r11, trueLen
	jmp print
print_empty:
	mov r10, empty
	mov r11, emptyLen
	jmp print
print_void:
	jmp exit
print_char:
	mov r10, msg
	dec r10
	mov BYTE [r10], `\'`
	dec r10
	shr rax, 8
	mov [r10], al
	dec r10
	mov BYTE [r10], `\'`
	mov r11, 4
	jmp print
print:
	mov rax, 1
	mov rdi, 1
	mov rsi, r10
	mov rdx, r11
	syscall
print_newline:
	mov rax, 1
	mov rdi, 1
	mov rsi, nl
	mov rdx, 1
	syscall
exit:
	mov rax, 60
	mov rdi, 0
	syscall

section .data

len: equ 20
msg: times len dw 0
trueLen: equ 5
true: db 'true', 0
falseLen: equ 6
false: db 'false', 0
emptyLen: equ 3
empty: db '[]', 0
nl: db 0x0a

