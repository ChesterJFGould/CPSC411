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
	; init heap
	mov rax, 9 ; mmap
	mov rdi, 0 ; page aligned
	mov rsi, 8388608 ; 8 MiB
	mov rdx, 3 ; rw
	mov r10, 34 ; map type = memory
	mov r8, -1 ; not a fd
	mov r9, 0 ; offset
	syscall
	sub rsp, 8388608
	mov r15, done ; setup return address

mov RAX , R15
mov RDI , 8
mov RSI , 16
mov R15 , RAX
jmp eq$22
consequence$169:
mov RCX , RAX
mov R10 , 18446744073709551608
and RCX , R10
mov RAX , RSI
mov R10 , 3
shrx RAX , RAX , R10
mov RAX , RAX
mov R10 , 1
add RAX , R10
mov RAX , RAX
mov R10 , 8
imul RAX , R10
mov [ RCX + RAX ] , RBX
mov RAX , 30
jmp R8
alternative$170:
mov RAX , 61758
jmp R8
continuation$171:
mov R11 , 6
cmp RCX , R11
jne consequence$169
jmp alternative$170
consequence$172:
mov RCX , 14
jmp continuation$171
alternative$173:
mov RCX , 6
jmp continuation$171
consequence$174:
mov RCX , RAX
mov R10 , 18446744073709551600
and RCX , R10
mov R11 , 0
mov RCX , [ RCX + R11 ]
cmp RSI , RCX
jl consequence$172
jmp alternative$173
alternative$175:
mov RAX , 61758
jmp R8
continuation$176:
mov R11 , 6
cmp RCX , R11
jne consequence$174
jmp alternative$175
consequence$177:
mov RCX , 14
jmp continuation$176
alternative$178:
mov RCX , 6
jmp continuation$176
consequence$179:
mov R11 , 0
cmp RSI , R11
jge consequence$177
jmp alternative$178
alternative$180:
mov RAX , 61758
jmp R8
continuation$181:
mov R11 , 6
cmp RCX , R11
jne consequence$179
jmp alternative$180
consequence$182:
mov RCX , 14
jmp continuation$181
alternative$183:
mov RCX , 6
jmp continuation$181
consequence$184:
mov RCX , RSI
mov R10 , 7
and RCX , R10
mov R11 , 0
cmp RCX , R11
je consequence$182
jmp alternative$183
alternative$185:
mov RAX , 61502
jmp R8
continuation$186:
mov R11 , 6
cmp RCX , R11
jne consequence$184
jmp alternative$185
consequence$187:
mov RCX , 14
jmp continuation$186
alternative$188:
mov RCX , 6
jmp continuation$186
setvec$0:
mov R8 , R15
mov RAX , RDI
mov RSI , RSI
mov RBX , RDX
mov RCX , RAX
mov R10 , 7
and RCX , R10
mov R11 , 3
cmp RCX , R11
je consequence$187
jmp alternative$188
consequence$189:
mov RBX , RAX
mov R10 , 18446744073709551608
and RBX , R10
mov RAX , RDX
mov R10 , 3
shrx RAX , RAX , R10
mov RAX , RAX
mov R10 , 1
add RAX , R10
mov RAX , RAX
mov R10 , 8
imul RAX , R10
mov RAX , [ RBX + RAX ]
jmp RCX
alternative$190:
mov RAX , 57662
jmp RCX
continuation$191:
mov R11 , 6
cmp RBX , R11
jne consequence$189
jmp alternative$190
consequence$192:
mov RBX , 14
jmp continuation$191
alternative$193:
mov RBX , 6
jmp continuation$191
consequence$194:
mov RBX , RAX
mov R10 , 18446744073709551600
and RBX , R10
mov R11 , 0
mov RBX , [ RBX + R11 ]
cmp RDX , RBX
jl consequence$192
jmp alternative$193
alternative$195:
mov RAX , 57662
jmp RCX
continuation$196:
mov R11 , 6
cmp RBX , R11
jne consequence$194
jmp alternative$195
consequence$197:
mov RBX , 14
jmp continuation$196
alternative$198:
mov RBX , 6
jmp continuation$196
consequence$199:
mov R11 , 0
cmp RDX , R11
jge consequence$197
jmp alternative$198
alternative$200:
mov RAX , 57662
jmp RCX
continuation$201:
mov R11 , 6
cmp RBX , R11
jne consequence$199
jmp alternative$200
consequence$202:
mov RBX , 14
jmp continuation$201
alternative$203:
mov RBX , 6
jmp continuation$201
consequence$204:
mov RBX , RDX
mov R10 , 7
and RBX , R10
mov R11 , 0
cmp RBX , R11
je consequence$202
jmp alternative$203
alternative$205:
mov RAX , 57406
jmp RCX
continuation$206:
mov R11 , 6
cmp RBX , R11
jne consequence$204
jmp alternative$205
consequence$207:
mov RBX , 14
jmp continuation$206
alternative$208:
mov RBX , 6
jmp continuation$206
vecRef$4:
mov RCX , R15
mov RAX , RDI
mov RDX , RSI
mov RBX , RAX
mov R10 , 7
and RBX , R10
mov R11 , 3
cmp RBX , R11
je consequence$207
jmp alternative$208
consequence$209:
mov RAX , RDX
add RAX , RCX
jmp RBX
alternative$210:
mov RAX , 4414
jmp RBX
continuation$211:
mov R11 , 6
cmp RAX , R11
jne consequence$209
jmp alternative$210
consequence$212:
mov RAX , 14
jmp continuation$211
alternative$213:
mov RAX , 6
jmp continuation$211
consequence$214:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$212
jmp alternative$213
alternative$215:
mov RAX , 4158
jmp RBX
continuation$216:
mov R11 , 6
cmp RAX , R11
jne consequence$214
jmp alternative$215
consequence$217:
mov RAX , 14
jmp continuation$216
alternative$218:
mov RAX , 6
jmp continuation$216
add$7:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$217
jmp alternative$218
consequence$219:
mov RAX , RDX
sub RAX , RCX
jmp RBX
alternative$220:
mov RAX , 8510
jmp RBX
continuation$221:
mov R11 , 6
cmp RAX , R11
jne consequence$219
jmp alternative$220
consequence$222:
mov RAX , 14
jmp continuation$221
alternative$223:
mov RAX , 6
jmp continuation$221
consequence$224:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$222
jmp alternative$223
alternative$225:
mov RAX , 8254
jmp RBX
continuation$226:
mov R11 , 6
cmp RAX , R11
jne consequence$224
jmp alternative$225
consequence$227:
mov RAX , 14
jmp continuation$226
alternative$228:
mov RAX , 6
jmp continuation$226
sub$10:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$227
jmp alternative$228
consequence$229:
mov RAX , RDX
mov R10 , 3
shrx RAX , RAX , R10
mov RAX , RAX
imul RAX , RCX
jmp RBX
alternative$230:
mov RAX , 12606
jmp RBX
continuation$231:
mov R11 , 6
cmp RAX , R11
jne consequence$229
jmp alternative$230
consequence$232:
mov RAX , 14
jmp continuation$231
alternative$233:
mov RAX , 6
jmp continuation$231
consequence$234:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$232
jmp alternative$233
alternative$235:
mov RAX , 12350
jmp RBX
continuation$236:
mov R11 , 6
cmp RAX , R11
jne consequence$234
jmp alternative$235
consequence$237:
mov RAX , 14
jmp continuation$236
alternative$238:
mov RAX , 6
jmp continuation$236
mul$13:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$237
jmp alternative$238
consequence$239:
mov RAX , 14
jmp RBX
alternative$240:
mov RAX , 6
jmp RBX
consequence$241:
cmp RDX , RCX
jl consequence$239
jmp alternative$240
alternative$242:
mov RAX , 16702
jmp RBX
continuation$243:
mov R11 , 6
cmp RAX , R11
jne consequence$241
jmp alternative$242
consequence$244:
mov RAX , 14
jmp continuation$243
alternative$245:
mov RAX , 6
jmp continuation$243
consequence$246:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$244
jmp alternative$245
alternative$247:
mov RAX , 16446
jmp RBX
continuation$248:
mov R11 , 6
cmp RAX , R11
jne consequence$246
jmp alternative$247
consequence$249:
mov RAX , 14
jmp continuation$248
alternative$250:
mov RAX , 6
jmp continuation$248
lt$16:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$249
jmp alternative$250
consequence$251:
mov RAX , 14
jmp RBX
alternative$252:
mov RAX , 6
jmp RBX
consequence$253:
cmp RDX , RCX
jg consequence$251
jmp alternative$252
alternative$254:
mov RAX , 20798
jmp RBX
continuation$255:
mov R11 , 6
cmp RAX , R11
jne consequence$253
jmp alternative$254
consequence$256:
mov RAX , 14
jmp continuation$255
alternative$257:
mov RAX , 6
jmp continuation$255
consequence$258:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$256
jmp alternative$257
alternative$259:
mov RAX , 20542
jmp RBX
continuation$260:
mov R11 , 6
cmp RAX , R11
jne consequence$258
jmp alternative$259
consequence$261:
mov RAX , 14
jmp continuation$260
alternative$262:
mov RAX , 6
jmp continuation$260
gt$19:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$261
jmp alternative$262
consequence$263:
mov RAX , 14
jmp RBX
alternative$264:
mov RAX , 6
jmp RBX
eq$22:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
cmp RCX , RAX
je consequence$263
jmp alternative$264
consequence$265:
mov RAX , 14
jmp RBX
alternative$266:
mov RAX , 6
jmp RBX
consequence$267:
cmp RDX , RCX
jle consequence$265
jmp alternative$266
alternative$268:
mov RAX , 24894
jmp RBX
continuation$269:
mov R11 , 6
cmp RAX , R11
jne consequence$267
jmp alternative$268
consequence$270:
mov RAX , 14
jmp continuation$269
alternative$271:
mov RAX , 6
jmp continuation$269
consequence$272:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$270
jmp alternative$271
alternative$273:
mov RAX , 24638
jmp RBX
continuation$274:
mov R11 , 6
cmp RAX , R11
jne consequence$272
jmp alternative$273
consequence$275:
mov RAX , 14
jmp continuation$274
alternative$276:
mov RAX , 6
jmp continuation$274
lte$25:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$275
jmp alternative$276
consequence$277:
mov RAX , 14
jmp RBX
alternative$278:
mov RAX , 6
jmp RBX
consequence$279:
cmp RDX , RCX
jge consequence$277
jmp alternative$278
alternative$280:
mov RAX , 28990
jmp RBX
continuation$281:
mov R11 , 6
cmp RAX , R11
jne consequence$279
jmp alternative$280
consequence$282:
mov RAX , 14
jmp continuation$281
alternative$283:
mov RAX , 6
jmp continuation$281
consequence$284:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$282
jmp alternative$283
alternative$285:
mov RAX , 28734
jmp RBX
continuation$286:
mov R11 , 6
cmp RAX , R11
jne consequence$284
jmp alternative$285
consequence$287:
mov RAX , 14
jmp continuation$286
alternative$288:
mov RAX , 6
jmp continuation$286
gte$28:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$287
jmp alternative$288
consequence$289:
mov RAX , 14
jmp RBX
alternative$290:
mov RAX , 6
jmp RBX
neq$31:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
cmp RCX , RAX
jne consequence$289
jmp alternative$290
cons$34:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RSP
mov R10 , 16
add RSP , R10
mov R11 , 0
mov [ RAX + R11 ] , RDX
mov R11 , 8
mov [ RAX + R11 ] , RCX
mov RAX , RAX
mov R10 , 18446744073709551608
and RAX , R10
mov RAX , RAX
mov R10 , 1
or RAX , R10
jmp RBX
consequence$291:
mov RAX , 14
jmp RBX
alternative$292:
mov RAX , 6
jmp RBX
isInt$37:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$291
jmp alternative$292
consequence$293:
mov RAX , 14
jmp RCX
alternative$294:
mov RAX , 6
jmp RCX
continuation$295:
mov R11 , 6
cmp RAX , R11
jne consequence$293
jmp alternative$294
consequence$296:
mov RAX , 14
jmp continuation$295
alternative$297:
mov RAX , 6
jmp continuation$295
consequence$298:
mov RAX , 14
jmp RCX
alternative$299:
mov R11 , 6
cmp RBX , R11
je consequence$296
jmp alternative$297
continuation$300:
mov R11 , 6
cmp RAX , R11
jne consequence$298
jmp alternative$299
consequence$301:
mov RAX , 14
jmp continuation$300
alternative$302:
mov RAX , 6
jmp continuation$300
isBool$39:
mov RCX , R15
mov RAX , RDI
mov RBX , RAX
mov R11 , 14
cmp RBX , R11
je consequence$301
jmp alternative$302
consequence$303:
mov RAX , 14
jmp RBX
alternative$304:
mov RAX , 6
jmp RBX
isEmpty$41:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R10 , 255
and RAX , R10
mov R11 , 22
cmp RAX , R11
je consequence$303
jmp alternative$304
consequence$305:
mov RAX , 14
jmp RBX
alternative$306:
mov RAX , 6
jmp RBX
isVoid$43:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R10 , 255
and RAX , R10
mov R11 , 30
cmp RAX , R11
je consequence$305
jmp alternative$306
consequence$307:
mov RAX , 14
jmp RBX
alternative$308:
mov RAX , 6
jmp RBX
isChar$45:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R10 , 255
and RAX , R10
mov R11 , 46
cmp RAX , R11
je consequence$307
jmp alternative$308
consequence$309:
mov RAX , 14
jmp RBX
alternative$310:
mov RAX , 6
jmp RBX
isError$47:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R10 , 255
and RAX , R10
mov R11 , 62
cmp RAX , R11
je consequence$309
jmp alternative$310
consequence$311:
mov RAX , 14
jmp RBX
alternative$312:
mov RAX , 6
jmp RBX
isPair$49:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R10 , 7
and RAX , R10
mov R11 , 1
cmp RAX , R11
je consequence$311
jmp alternative$312
consequence$313:
mov RAX , 6
jmp RBX
alternative$314:
mov RAX , 14
jmp RBX
not$51:
mov RBX , R15
mov RAX , RDI
mov R11 , 6
cmp RAX , R11
jne consequence$313
jmp alternative$314
consequence$315:
mov RAX , 14
jmp RBX
alternative$316:
mov RAX , 6
jmp RBX
isVector$53:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R10 , 7
and RAX , R10
mov R11 , 3
cmp RAX , R11
je consequence$315
jmp alternative$316
consequence$317:
mov RAX , RCX
mov R10 , 18446744073709551608
and RAX , R10
mov R11 , 0
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$318:
mov RAX , 36926
jmp RBX
continuation$319:
mov R11 , 6
cmp RAX , R11
jne consequence$317
jmp alternative$318
consequence$320:
mov RAX , 14
jmp continuation$319
alternative$321:
mov RAX , 6
jmp continuation$319
car$55:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 1
cmp RAX , R11
je consequence$320
jmp alternative$321
consequence$322:
mov RAX , RCX
mov R10 , 18446744073709551608
and RAX , R10
mov R11 , 8
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$323:
mov RAX , 41022
jmp RBX
continuation$324:
mov R11 , 6
cmp RAX , R11
jne consequence$322
jmp alternative$323
consequence$325:
mov RAX , 14
jmp continuation$324
alternative$326:
mov RAX , 6
jmp continuation$324
cdr$57:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 1
cmp RAX , R11
je consequence$325
jmp alternative$326
consequence$327:
mov RAX , RBX
mov R10 , 1
add RAX , R10
mov RAX , RAX
mov R10 , 8
imul RAX , R10
mov RBX , RSP
add RSP , RAX
mov RAX , RBX
mov R10 , 18446744073709551608
and RAX , R10
mov RAX , RAX
mov R10 , 3
or RAX , R10
jmp RCX
alternative$328:
mov RAX , 45118
jmp RCX
continuation$329:
mov R11 , 6
cmp RAX , R11
jne consequence$327
jmp alternative$328
consequence$330:
mov RAX , 14
jmp continuation$329
alternative$331:
mov RAX , 6
jmp continuation$329
makeVector$59:
mov RCX , R15
mov RBX , RDI
mov RAX , RBX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$330
jmp alternative$331
consequence$332:
mov RAX , RCX
mov R10 , 18446744073709551600
and RAX , R10
mov R11 , 0
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$333:
mov RAX , 49214
jmp RBX
continuation$334:
mov R11 , 6
cmp RAX , R11
jne consequence$332
jmp alternative$333
consequence$335:
mov RAX , 14
jmp continuation$334
alternative$336:
mov RAX , 6
jmp continuation$334
vectorLength$61:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 3
cmp RAX , R11
je consequence$335
jmp alternative$336
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

