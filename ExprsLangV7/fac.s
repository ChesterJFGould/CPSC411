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
mov RDI , 80
mov R15 , RSP
jmp fac$39
consequence$121:
mov RAX , 318
jmp RBX
alternative$122:
mov RAX , RAX
add RAX , RCX
jmp RBX
continuation$123:
cmp RSP , 6
jne consequence$121
jmp alternative$122
consequence$124:
mov RSP , 6
jmp continuation$123
alternative$125:
mov RSP , 14
jmp continuation$123
continuation$126:
cmp RSP , 6
jne consequence$124
jmp alternative$125
consequence$127:
mov RSP , 14
jmp continuation$126
alternative$128:
mov RSP , 6
jmp continuation$126
consequence$129:
mov RAX , 62
jmp RBX
alternative$130:
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$127
jmp alternative$128
continuation$131:
cmp RSP , 6
jne consequence$129
jmp alternative$130
consequence$132:
mov RSP , 6
jmp continuation$131
alternative$133:
mov RSP , 14
jmp continuation$131
continuation$134:
cmp RSP , 6
jne consequence$132
jmp alternative$133
consequence$135:
mov RSP , 14
jmp continuation$134
alternative$136:
mov RSP , 6
jmp continuation$134
add$0:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$135
jmp alternative$136
consequence$137:
mov RAX , 4414
jmp RBX
alternative$138:
mov RAX , RAX
sub RAX , RCX
jmp RBX
continuation$139:
cmp RSP , 6
jne consequence$137
jmp alternative$138
consequence$140:
mov RSP , 6
jmp continuation$139
alternative$141:
mov RSP , 14
jmp continuation$139
continuation$142:
cmp RSP , 6
jne consequence$140
jmp alternative$141
consequence$143:
mov RSP , 14
jmp continuation$142
alternative$144:
mov RSP , 6
jmp continuation$142
consequence$145:
mov RAX , 4158
jmp RBX
alternative$146:
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$143
jmp alternative$144
continuation$147:
cmp RSP , 6
jne consequence$145
jmp alternative$146
consequence$148:
mov RSP , 6
jmp continuation$147
alternative$149:
mov RSP , 14
jmp continuation$147
continuation$150:
cmp RSP , 6
jne consequence$148
jmp alternative$149
consequence$151:
mov RSP , 14
jmp continuation$150
alternative$152:
mov RSP , 6
jmp continuation$150
sub$3:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$151
jmp alternative$152
consequence$153:
mov RAX , 8510
jmp RBX
alternative$154:
mov RSP , RAX
shr RSP , 3
mov RAX , RSP
imul RAX , RCX
jmp RBX
continuation$155:
cmp RSP , 6
jne consequence$153
jmp alternative$154
consequence$156:
mov RSP , 6
jmp continuation$155
alternative$157:
mov RSP , 14
jmp continuation$155
continuation$158:
cmp RSP , 6
jne consequence$156
jmp alternative$157
consequence$159:
mov RSP , 14
jmp continuation$158
alternative$160:
mov RSP , 6
jmp continuation$158
consequence$161:
mov RAX , 8254
jmp RBX
alternative$162:
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$159
jmp alternative$160
continuation$163:
cmp RSP , 6
jne consequence$161
jmp alternative$162
consequence$164:
mov RSP , 6
jmp continuation$163
alternative$165:
mov RSP , 14
jmp continuation$163
continuation$166:
cmp RSP , 6
jne consequence$164
jmp alternative$165
consequence$167:
mov RSP , 14
jmp continuation$166
alternative$168:
mov RSP , 6
jmp continuation$166
mul$6:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$167
jmp alternative$168
consequence$169:
mov RAX , 14
jmp RBX
alternative$170:
mov RAX , 6
jmp RBX
consequence$171:
mov RAX , 12606
jmp RBX
alternative$172:
cmp RCX , RAX
jl consequence$169
jmp alternative$170
continuation$173:
cmp RSP , 6
jne consequence$171
jmp alternative$172
consequence$174:
mov RSP , 6
jmp continuation$173
alternative$175:
mov RSP , 14
jmp continuation$173
continuation$176:
cmp RSP , 6
jne consequence$174
jmp alternative$175
consequence$177:
mov RSP , 14
jmp continuation$176
alternative$178:
mov RSP , 6
jmp continuation$176
consequence$179:
mov RAX , 12350
jmp RBX
alternative$180:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$177
jmp alternative$178
continuation$181:
cmp RSP , 6
jne consequence$179
jmp alternative$180
consequence$182:
mov RSP , 6
jmp continuation$181
alternative$183:
mov RSP , 14
jmp continuation$181
continuation$184:
cmp RSP , 6
jne consequence$182
jmp alternative$183
consequence$185:
mov RSP , 14
jmp continuation$184
alternative$186:
mov RSP , 6
jmp continuation$184
lt$9:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$185
jmp alternative$186
consequence$187:
mov RAX , 14
jmp RBX
alternative$188:
mov RAX , 6
jmp RBX
consequence$189:
mov RAX , 16702
jmp RBX
alternative$190:
cmp RCX , RAX
jg consequence$187
jmp alternative$188
continuation$191:
cmp RSP , 6
jne consequence$189
jmp alternative$190
consequence$192:
mov RSP , 6
jmp continuation$191
alternative$193:
mov RSP , 14
jmp continuation$191
continuation$194:
cmp RSP , 6
jne consequence$192
jmp alternative$193
consequence$195:
mov RSP , 14
jmp continuation$194
alternative$196:
mov RSP , 6
jmp continuation$194
consequence$197:
mov RAX , 16446
jmp RBX
alternative$198:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$195
jmp alternative$196
continuation$199:
cmp RSP , 6
jne consequence$197
jmp alternative$198
consequence$200:
mov RSP , 6
jmp continuation$199
alternative$201:
mov RSP , 14
jmp continuation$199
continuation$202:
cmp RSP , 6
jne consequence$200
jmp alternative$201
consequence$203:
mov RSP , 14
jmp continuation$202
alternative$204:
mov RSP , 6
jmp continuation$202
gt$12:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$203
jmp alternative$204
consequence$205:
mov RAX , 14
jmp RBX
alternative$206:
mov RAX , 6
jmp RBX
consequence$207:
mov RAX , 20798
jmp RBX
alternative$208:
cmp RCX , RAX
je consequence$205
jmp alternative$206
continuation$209:
cmp RSP , 6
jne consequence$207
jmp alternative$208
consequence$210:
mov RSP , 6
jmp continuation$209
alternative$211:
mov RSP , 14
jmp continuation$209
continuation$212:
cmp RSP , 6
jne consequence$210
jmp alternative$211
consequence$213:
mov RSP , 14
jmp continuation$212
alternative$214:
mov RSP , 6
jmp continuation$212
consequence$215:
mov RAX , 20542
jmp RBX
alternative$216:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$213
jmp alternative$214
continuation$217:
cmp RSP , 6
jne consequence$215
jmp alternative$216
consequence$218:
mov RSP , 6
jmp continuation$217
alternative$219:
mov RSP , 14
jmp continuation$217
continuation$220:
cmp RSP , 6
jne consequence$218
jmp alternative$219
consequence$221:
mov RSP , 14
jmp continuation$220
alternative$222:
mov RSP , 6
jmp continuation$220
eq$15:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$221
jmp alternative$222
consequence$223:
mov RAX , 14
jmp RBX
alternative$224:
mov RAX , 6
jmp RBX
consequence$225:
mov RAX , 24894
jmp RBX
alternative$226:
cmp RCX , RAX
jle consequence$223
jmp alternative$224
continuation$227:
cmp RSP , 6
jne consequence$225
jmp alternative$226
consequence$228:
mov RSP , 6
jmp continuation$227
alternative$229:
mov RSP , 14
jmp continuation$227
continuation$230:
cmp RSP , 6
jne consequence$228
jmp alternative$229
consequence$231:
mov RSP , 14
jmp continuation$230
alternative$232:
mov RSP , 6
jmp continuation$230
consequence$233:
mov RAX , 24638
jmp RBX
alternative$234:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$231
jmp alternative$232
continuation$235:
cmp RSP , 6
jne consequence$233
jmp alternative$234
consequence$236:
mov RSP , 6
jmp continuation$235
alternative$237:
mov RSP , 14
jmp continuation$235
continuation$238:
cmp RSP , 6
jne consequence$236
jmp alternative$237
consequence$239:
mov RSP , 14
jmp continuation$238
alternative$240:
mov RSP , 6
jmp continuation$238
lte$18:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$239
jmp alternative$240
consequence$241:
mov RAX , 14
jmp RBX
alternative$242:
mov RAX , 6
jmp RBX
consequence$243:
mov RAX , 28990
jmp RBX
alternative$244:
cmp RCX , RAX
jge consequence$241
jmp alternative$242
continuation$245:
cmp RSP , 6
jne consequence$243
jmp alternative$244
consequence$246:
mov RSP , 6
jmp continuation$245
alternative$247:
mov RSP , 14
jmp continuation$245
continuation$248:
cmp RSP , 6
jne consequence$246
jmp alternative$247
consequence$249:
mov RSP , 14
jmp continuation$248
alternative$250:
mov RSP , 6
jmp continuation$248
consequence$251:
mov RAX , 28734
jmp RBX
alternative$252:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$249
jmp alternative$250
continuation$253:
cmp RSP , 6
jne consequence$251
jmp alternative$252
consequence$254:
mov RSP , 6
jmp continuation$253
alternative$255:
mov RSP , 14
jmp continuation$253
continuation$256:
cmp RSP , 6
jne consequence$254
jmp alternative$255
consequence$257:
mov RSP , 14
jmp continuation$256
alternative$258:
mov RSP , 6
jmp continuation$256
gte$21:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$257
jmp alternative$258
consequence$259:
mov RAX , 14
jmp RBX
alternative$260:
mov RAX , 6
jmp RBX
consequence$261:
mov RAX , 33086
jmp RBX
alternative$262:
cmp RCX , RAX
jne consequence$259
jmp alternative$260
continuation$263:
cmp RSP , 6
jne consequence$261
jmp alternative$262
consequence$264:
mov RSP , 6
jmp continuation$263
alternative$265:
mov RSP , 14
jmp continuation$263
continuation$266:
cmp RSP , 6
jne consequence$264
jmp alternative$265
consequence$267:
mov RSP , 14
jmp continuation$266
alternative$268:
mov RSP , 6
jmp continuation$266
consequence$269:
mov RAX , 32830
jmp RBX
alternative$270:
mov RSP , RAX
and RSP , 7
cmp RSP , 0
je consequence$267
jmp alternative$268
continuation$271:
cmp RSP , 6
jne consequence$269
jmp alternative$270
consequence$272:
mov RSP , 6
jmp continuation$271
alternative$273:
mov RSP , 14
jmp continuation$271
continuation$274:
cmp RSP , 6
jne consequence$272
jmp alternative$273
consequence$275:
mov RSP , 14
jmp continuation$274
alternative$276:
mov RSP , 6
jmp continuation$274
neq$24:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
mov RSP , RCX
and RSP , 7
cmp RSP , 0
je consequence$275
jmp alternative$276
consequence$277:
mov RAX , 14
jmp RBX
alternative$278:
mov RAX , 6
jmp RBX
isInt$27:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 7
cmp RSP , 0
je consequence$277
jmp alternative$278
consequence$279:
mov RAX , 14
jmp RBX
alternative$280:
mov RAX , 6
jmp RBX
isBool$29:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 6
je consequence$279
jmp alternative$280
consequence$281:
mov RAX , 14
jmp RBX
alternative$282:
mov RAX , 6
jmp RBX
isEmpty$31:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 22
je consequence$281
jmp alternative$282
consequence$283:
mov RAX , 14
jmp RBX
alternative$284:
mov RAX , 6
jmp RBX
isVoid$33:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 30
je consequence$283
jmp alternative$284
consequence$285:
mov RAX , 14
jmp RBX
alternative$286:
mov RAX , 6
jmp RBX
isChar$35:
mov RBX , R15
mov RSP , RDI
mov RSP , RSP
and RSP , 255
cmp RSP , 62
je consequence$285
jmp alternative$286
consequence$287:
mov RAX , 6
jmp RSP
alternative$288:
mov RAX , 14
jmp RSP
not$37:
mov RSP , R15
mov RAX , RDI
cmp RAX , 6
jne consequence$287
jmp alternative$288
continuation$289:
add RBP , 16
mov RCX , QWORD [ RBP - 0 ]
mov RBX , QWORD [ RBP - 8 ]
mov RSP , RAX
mov RDI , RBX
mov RSI , RSP
mov R15 , RCX
jmp mul$6
continuation$290:
add RBP , 16
mov RCX , QWORD [ RBP - 0 ]
mov RBX , QWORD [ RBP - 8 ]
mov RSP , RAX
mov RDI , RSP
mov QWORD [ RBP - 0 ] , RCX
mov QWORD [ RBP - 8 ] , RBX
sub RBP , 16
mov R15 , continuation$289
jmp fac$39
consequence$291:
mov RAX , 8
jmp RCX
alternative$292:
mov RDI , RBX
mov RSI , 8
mov QWORD [ RBP - 0 ] , RCX
mov QWORD [ RBP - 8 ] , RBX
sub RBP , 16
mov R15 , continuation$290
jmp sub$3
continuation$293:
add RBP , 16
mov RCX , QWORD [ RBP - 0 ]
mov RBX , QWORD [ RBP - 8 ]
mov RSP , RAX
cmp RSP , 6
jne consequence$291
jmp alternative$292
fac$39:
mov RCX , R15
mov RBX , RDI
mov RDI , RBX
mov RSI , 8
mov QWORD [ RBP - 0 ] , RCX
mov QWORD [ RBP - 8 ] , RBX
sub RBP , 16
mov R15 , continuation$293
jmp lt$9
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
	cmp r8, 0b111110 ; Error tag
	je print_error
print_int:
	sar rax, 3
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
	add r10, len
	dec r10
	mov BYTE [r10], `\'`
	dec r10
	shr rax, 8
	mov [r10], al
	dec r10
	mov BYTE [r10], `\'`
	mov r11, 4
	jmp print
print_error:
	shr rax, 8
	jmp error
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
error:
	mov rdi, rax
	mov rax, 60
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

