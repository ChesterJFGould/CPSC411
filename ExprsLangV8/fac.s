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
	mov r15, done ; setup return address

mov RAX , R15
mov RDI , 80
mov R15 , RAX
jmp fac$63
consequence$175:
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
alternative$176:
mov RAX , 61758
jmp R8
continuation$177:
mov R11 , 6
cmp RCX , R11
jne consequence$175
jmp alternative$176
consequence$178:
mov RCX , 14
jmp continuation$177
alternative$179:
mov RCX , 6
jmp continuation$177
consequence$180:
mov RCX , RAX
mov R10 , 18446744073709551600
and RCX , R10
mov R11 , 0
mov RCX , [ RCX + R11 ]
cmp RSI , RCX
jl consequence$178
jmp alternative$179
alternative$181:
mov RAX , 61758
jmp R8
continuation$182:
mov R11 , 6
cmp RCX , R11
jne consequence$180
jmp alternative$181
consequence$183:
mov RCX , 14
jmp continuation$182
alternative$184:
mov RCX , 6
jmp continuation$182
consequence$185:
mov R11 , 0
cmp RSI , R11
jge consequence$183
jmp alternative$184
alternative$186:
mov RAX , 61758
jmp R8
continuation$187:
mov R11 , 6
cmp RCX , R11
jne consequence$185
jmp alternative$186
consequence$188:
mov RCX , 14
jmp continuation$187
alternative$189:
mov RCX , 6
jmp continuation$187
consequence$190:
mov RCX , RSI
mov R10 , 7
and RCX , R10
mov R11 , 0
cmp RCX , R11
je consequence$188
jmp alternative$189
alternative$191:
mov RAX , 61502
jmp R8
continuation$192:
mov R11 , 6
cmp RCX , R11
jne consequence$190
jmp alternative$191
consequence$193:
mov RCX , 14
jmp continuation$192
alternative$194:
mov RCX , 6
jmp continuation$192
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
je consequence$193
jmp alternative$194
consequence$195:
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
alternative$196:
mov RAX , 57662
jmp RCX
continuation$197:
mov R11 , 6
cmp RBX , R11
jne consequence$195
jmp alternative$196
consequence$198:
mov RBX , 14
jmp continuation$197
alternative$199:
mov RBX , 6
jmp continuation$197
consequence$200:
mov RBX , RAX
mov R10 , 18446744073709551600
and RBX , R10
mov R11 , 0
mov RBX , [ RBX + R11 ]
cmp RDX , RBX
jl consequence$198
jmp alternative$199
alternative$201:
mov RAX , 57662
jmp RCX
continuation$202:
mov R11 , 6
cmp RBX , R11
jne consequence$200
jmp alternative$201
consequence$203:
mov RBX , 14
jmp continuation$202
alternative$204:
mov RBX , 6
jmp continuation$202
consequence$205:
mov R11 , 0
cmp RDX , R11
jge consequence$203
jmp alternative$204
alternative$206:
mov RAX , 57662
jmp RCX
continuation$207:
mov R11 , 6
cmp RBX , R11
jne consequence$205
jmp alternative$206
consequence$208:
mov RBX , 14
jmp continuation$207
alternative$209:
mov RBX , 6
jmp continuation$207
consequence$210:
mov RBX , RDX
mov R10 , 7
and RBX , R10
mov R11 , 0
cmp RBX , R11
je consequence$208
jmp alternative$209
alternative$211:
mov RAX , 57406
jmp RCX
continuation$212:
mov R11 , 6
cmp RBX , R11
jne consequence$210
jmp alternative$211
consequence$213:
mov RBX , 14
jmp continuation$212
alternative$214:
mov RBX , 6
jmp continuation$212
vecRef$4:
mov RCX , R15
mov RAX , RDI
mov RDX , RSI
mov RBX , RAX
mov R10 , 7
and RBX , R10
mov R11 , 3
cmp RBX , R11
je consequence$213
jmp alternative$214
consequence$215:
mov RAX , RDX
add RAX , RCX
jmp RBX
alternative$216:
mov RAX , 4414
jmp RBX
continuation$217:
mov R11 , 6
cmp RAX , R11
jne consequence$215
jmp alternative$216
consequence$218:
mov RAX , 14
jmp continuation$217
alternative$219:
mov RAX , 6
jmp continuation$217
consequence$220:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$218
jmp alternative$219
alternative$221:
mov RAX , 4158
jmp RBX
continuation$222:
mov R11 , 6
cmp RAX , R11
jne consequence$220
jmp alternative$221
consequence$223:
mov RAX , 14
jmp continuation$222
alternative$224:
mov RAX , 6
jmp continuation$222
add$7:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$223
jmp alternative$224
consequence$225:
mov RAX , RDX
sub RAX , RCX
jmp RBX
alternative$226:
mov RAX , 8510
jmp RBX
continuation$227:
mov R11 , 6
cmp RAX , R11
jne consequence$225
jmp alternative$226
consequence$228:
mov RAX , 14
jmp continuation$227
alternative$229:
mov RAX , 6
jmp continuation$227
consequence$230:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$228
jmp alternative$229
alternative$231:
mov RAX , 8254
jmp RBX
continuation$232:
mov R11 , 6
cmp RAX , R11
jne consequence$230
jmp alternative$231
consequence$233:
mov RAX , 14
jmp continuation$232
alternative$234:
mov RAX , 6
jmp continuation$232
sub$10:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$233
jmp alternative$234
consequence$235:
mov RAX , RDX
mov R10 , 3
shrx RAX , RAX , R10
mov RAX , RAX
imul RAX , RCX
jmp RBX
alternative$236:
mov RAX , 12606
jmp RBX
continuation$237:
mov R11 , 6
cmp RAX , R11
jne consequence$235
jmp alternative$236
consequence$238:
mov RAX , 14
jmp continuation$237
alternative$239:
mov RAX , 6
jmp continuation$237
consequence$240:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$238
jmp alternative$239
alternative$241:
mov RAX , 12350
jmp RBX
continuation$242:
mov R11 , 6
cmp RAX , R11
jne consequence$240
jmp alternative$241
consequence$243:
mov RAX , 14
jmp continuation$242
alternative$244:
mov RAX , 6
jmp continuation$242
mul$13:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$243
jmp alternative$244
consequence$245:
mov RAX , 14
jmp RBX
alternative$246:
mov RAX , 6
jmp RBX
consequence$247:
cmp RAX , RCX
jl consequence$245
jmp alternative$246
alternative$248:
mov RAX , 16702
jmp RBX
continuation$249:
mov R11 , 6
cmp RAX , R11
jne consequence$247
jmp alternative$248
consequence$250:
mov RAX , 14
jmp continuation$249
alternative$251:
mov RAX , 6
jmp continuation$249
consequence$252:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$250
jmp alternative$251
alternative$253:
mov RAX , 16446
jmp RBX
continuation$254:
mov R11 , 6
cmp RAX , R11
jne consequence$252
jmp alternative$253
consequence$255:
mov RAX , 14
jmp continuation$254
alternative$256:
mov RAX , 6
jmp continuation$254
lt$16:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RAX , RAX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$255
jmp alternative$256
consequence$257:
mov RAX , 14
jmp RBX
alternative$258:
mov RAX , 6
jmp RBX
consequence$259:
cmp RAX , RCX
jg consequence$257
jmp alternative$258
alternative$260:
mov RAX , 20798
jmp RBX
continuation$261:
mov R11 , 6
cmp RAX , R11
jne consequence$259
jmp alternative$260
consequence$262:
mov RAX , 14
jmp continuation$261
alternative$263:
mov RAX , 6
jmp continuation$261
consequence$264:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$262
jmp alternative$263
alternative$265:
mov RAX , 20542
jmp RBX
continuation$266:
mov R11 , 6
cmp RAX , R11
jne consequence$264
jmp alternative$265
consequence$267:
mov RAX , 14
jmp continuation$266
alternative$268:
mov RAX , 6
jmp continuation$266
gt$19:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RAX , RAX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$267
jmp alternative$268
consequence$269:
mov RAX , 14
jmp RBX
alternative$270:
mov RAX , 6
jmp RBX
eq$22:
mov RBX , R15
mov RAX , RDI
mov RAX , RSI
cmp RAX , RAX
je consequence$269
jmp alternative$270
consequence$271:
mov RAX , 14
jmp RBX
alternative$272:
mov RAX , 6
jmp RBX
consequence$273:
cmp RAX , RCX
jle consequence$271
jmp alternative$272
alternative$274:
mov RAX , 24894
jmp RBX
continuation$275:
mov R11 , 6
cmp RAX , R11
jne consequence$273
jmp alternative$274
consequence$276:
mov RAX , 14
jmp continuation$275
alternative$277:
mov RAX , 6
jmp continuation$275
consequence$278:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$276
jmp alternative$277
alternative$279:
mov RAX , 24638
jmp RBX
continuation$280:
mov R11 , 6
cmp RAX , R11
jne consequence$278
jmp alternative$279
consequence$281:
mov RAX , 14
jmp continuation$280
alternative$282:
mov RAX , 6
jmp continuation$280
lte$25:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RAX , RAX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$281
jmp alternative$282
consequence$283:
mov RAX , 14
jmp RBX
alternative$284:
mov RAX , 6
jmp RBX
consequence$285:
cmp RAX , RCX
jge consequence$283
jmp alternative$284
alternative$286:
mov RAX , 28990
jmp RBX
continuation$287:
mov R11 , 6
cmp RAX , R11
jne consequence$285
jmp alternative$286
consequence$288:
mov RAX , 14
jmp continuation$287
alternative$289:
mov RAX , 6
jmp continuation$287
consequence$290:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$288
jmp alternative$289
alternative$291:
mov RAX , 28734
jmp RBX
continuation$292:
mov R11 , 6
cmp RAX , R11
jne consequence$290
jmp alternative$291
consequence$293:
mov RAX , 14
jmp continuation$292
alternative$294:
mov RAX , 6
jmp continuation$292
gte$28:
mov RBX , R15
mov RAX , RDI
mov RCX , RSI
mov RAX , RAX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$293
jmp alternative$294
consequence$295:
mov RAX , 14
jmp RBX
alternative$296:
mov RAX , 6
jmp RBX
neq$31:
mov RBX , R15
mov RAX , RDI
mov RAX , RSI
cmp RAX , RAX
jne consequence$295
jmp alternative$296
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
consequence$297:
mov RAX , 14
jmp RBX
alternative$298:
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
je consequence$297
jmp alternative$298
consequence$299:
mov RAX , 14
jmp RBX
alternative$300:
mov RAX , 6
jmp RBX
continuation$301:
mov R11 , 6
cmp RAX , R11
jne consequence$299
jmp alternative$300
consequence$302:
mov RAX , 14
jmp continuation$301
alternative$303:
mov RAX , 6
jmp continuation$301
consequence$304:
mov RAX , 14
jmp RBX
alternative$305:
mov R11 , 6
cmp RAX , R11
je consequence$302
jmp alternative$303
continuation$306:
mov R11 , 6
cmp RAX , R11
jne consequence$304
jmp alternative$305
consequence$307:
mov RAX , 14
jmp continuation$306
alternative$308:
mov RAX , 6
jmp continuation$306
isBool$39:
mov RBX , R15
mov RAX , RDI
mov RAX , RAX
mov R11 , 14
cmp RAX , R11
je consequence$307
jmp alternative$308
consequence$309:
mov RAX , 14
jmp RBX
alternative$310:
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
je consequence$309
jmp alternative$310
consequence$311:
mov RAX , 14
jmp RBX
alternative$312:
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
je consequence$311
jmp alternative$312
consequence$313:
mov RAX , 14
jmp RBX
alternative$314:
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
je consequence$313
jmp alternative$314
consequence$315:
mov RAX , 14
jmp RBX
alternative$316:
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
je consequence$315
jmp alternative$316
consequence$317:
mov RAX , 14
jmp RBX
alternative$318:
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
je consequence$317
jmp alternative$318
consequence$319:
mov RAX , 6
jmp RBX
alternative$320:
mov RAX , 14
jmp RBX
not$51:
mov RBX , R15
mov RAX , RDI
mov R11 , 6
cmp RAX , R11
jne consequence$319
jmp alternative$320
consequence$321:
mov RAX , 14
jmp RBX
alternative$322:
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
je consequence$321
jmp alternative$322
consequence$323:
mov RAX , RCX
mov R10 , 18446744073709551608
and RAX , R10
mov R11 , 0
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$324:
mov RAX , 36926
jmp RBX
continuation$325:
mov R11 , 6
cmp RAX , R11
jne consequence$323
jmp alternative$324
consequence$326:
mov RAX , 14
jmp continuation$325
alternative$327:
mov RAX , 6
jmp continuation$325
car$55:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 1
cmp RAX , R11
je consequence$326
jmp alternative$327
consequence$328:
mov RAX , RCX
mov R10 , 18446744073709551608
and RAX , R10
mov R11 , 8
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$329:
mov RAX , 41022
jmp RBX
continuation$330:
mov R11 , 6
cmp RAX , R11
jne consequence$328
jmp alternative$329
consequence$331:
mov RAX , 14
jmp continuation$330
alternative$332:
mov RAX , 6
jmp continuation$330
cdr$57:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 1
cmp RAX , R11
je consequence$331
jmp alternative$332
consequence$333:
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
alternative$334:
mov RAX , 45118
jmp RCX
continuation$335:
mov R11 , 6
cmp RAX , R11
jne consequence$333
jmp alternative$334
consequence$336:
mov RAX , 14
jmp continuation$335
alternative$337:
mov RAX , 6
jmp continuation$335
makeVector$59:
mov RCX , R15
mov RBX , RDI
mov RAX , RBX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$336
jmp alternative$337
consequence$338:
mov RAX , RCX
mov R10 , 18446744073709551600
and RAX , R10
mov R11 , 0
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$339:
mov RAX , 49214
jmp RBX
continuation$340:
mov R11 , 6
cmp RAX , R11
jne consequence$338
jmp alternative$339
consequence$341:
mov RAX , 14
jmp continuation$340
alternative$342:
mov RAX , 6
jmp continuation$340
vectorLength$61:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 3
cmp RAX , R11
je consequence$341
jmp alternative$342
continuation$343:
mov R10 , 16
add RBP , R10
mov RCX , [ RBP - 0 ]
mov RBX , [ RBP - 8 ]
mov RAX , RAX
mov RDI , RBX
mov RSI , RAX
mov R15 , RCX
jmp mul$13
continuation$344:
mov R10 , 16
add RBP , R10
mov RCX , [ RBP - 0 ]
mov RBX , [ RBP - 8 ]
mov RAX , RAX
mov RDI , RAX
mov [ RBP - 0 ] , RCX
mov [ RBP - 8 ] , RBX
mov R10 , 16
sub RBP , R10
mov R15 , continuation$343
jmp fac$63
consequence$345:
mov RAX , 8
jmp RCX
alternative$346:
mov RDI , RBX
mov RSI , 8
mov [ RBP - 0 ] , RCX
mov [ RBP - 8 ] , RBX
mov R10 , 16
sub RBP , R10
mov R15 , continuation$344
jmp sub$10
continuation$347:
mov R10 , 16
add RBP , R10
mov RCX , [ RBP - 0 ]
mov RBX , [ RBP - 8 ]
mov RAX , RAX
mov R11 , 6
cmp RAX , R11
jne consequence$345
jmp alternative$346
fac$63:
mov RCX , R15
mov RBX , RDI
mov RDI , RBX
mov RSI , 8
mov [ RBP - 0 ] , RCX
mov [ RBP - 8 ] , RBX
mov R10 , 16
sub RBP , R10
mov R15 , continuation$347
jmp lt$16
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

