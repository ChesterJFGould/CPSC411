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
	sub rsp, 128
	mov r15, done ; setup return address

mov RBX , R15
mov RDI , 32
mov RSI , 22
mov [ RBP - 0 ] , RBX
mov R10 , 8
sub RBP , R10
mov R15 , continuation$357
jmp cons$34
consequence$180:
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
mov RCX , RAX
mov R10 , 18446744073709551600
and RCX , R10
mov R11 , 0
mov RCX , [ RCX + R11 ]
cmp RSI , RCX
jl consequence$183
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
mov R11 , 0
cmp RSI , R11
jge consequence$188
jmp alternative$189
alternative$191:
mov RAX , 61758
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
consequence$195:
mov RCX , RSI
mov R10 , 7
and RCX , R10
mov R11 , 0
cmp RCX , R11
je consequence$193
jmp alternative$194
alternative$196:
mov RAX , 61502
jmp R8
continuation$197:
mov R11 , 6
cmp RCX , R11
jne consequence$195
jmp alternative$196
consequence$198:
mov RCX , 14
jmp continuation$197
alternative$199:
mov RCX , 6
jmp continuation$197
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
je consequence$198
jmp alternative$199
consequence$200:
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
mov RBX , RAX
mov R10 , 18446744073709551600
and RBX , R10
mov R11 , 0
mov RBX , [ RBX + R11 ]
cmp RDX , RBX
jl consequence$203
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
mov R11 , 0
cmp RDX , R11
jge consequence$208
jmp alternative$209
alternative$211:
mov RAX , 57662
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
consequence$215:
mov RBX , RDX
mov R10 , 7
and RBX , R10
mov R11 , 0
cmp RBX , R11
je consequence$213
jmp alternative$214
alternative$216:
mov RAX , 57406
jmp RCX
continuation$217:
mov R11 , 6
cmp RBX , R11
jne consequence$215
jmp alternative$216
consequence$218:
mov RBX , 14
jmp continuation$217
alternative$219:
mov RBX , 6
jmp continuation$217
vecRef$4:
mov RCX , R15
mov RAX , RDI
mov RDX , RSI
mov RBX , RAX
mov R10 , 7
and RBX , R10
mov R11 , 3
cmp RBX , R11
je consequence$218
jmp alternative$219
consequence$220:
mov RAX , RDX
add RAX , RCX
jmp RBX
alternative$221:
mov RAX , 4414
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
consequence$225:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$223
jmp alternative$224
alternative$226:
mov RAX , 4158
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
add$7:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$228
jmp alternative$229
consequence$230:
mov RAX , RDX
sub RAX , RCX
jmp RBX
alternative$231:
mov RAX , 8510
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
consequence$235:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$233
jmp alternative$234
alternative$236:
mov RAX , 8254
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
sub$10:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$238
jmp alternative$239
consequence$240:
mov RAX , RDX
mov R10 , 3
shrx RAX , RAX , R10
mov RAX , RAX
imul RAX , RCX
jmp RBX
alternative$241:
mov RAX , 12606
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
consequence$245:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$243
jmp alternative$244
alternative$246:
mov RAX , 12350
jmp RBX
continuation$247:
mov R11 , 6
cmp RAX , R11
jne consequence$245
jmp alternative$246
consequence$248:
mov RAX , 14
jmp continuation$247
alternative$249:
mov RAX , 6
jmp continuation$247
mul$13:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$248
jmp alternative$249
consequence$250:
mov RAX , 14
jmp RBX
alternative$251:
mov RAX , 6
jmp RBX
consequence$252:
cmp RDX , RCX
jl consequence$250
jmp alternative$251
alternative$253:
mov RAX , 16702
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
consequence$257:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$255
jmp alternative$256
alternative$258:
mov RAX , 16446
jmp RBX
continuation$259:
mov R11 , 6
cmp RAX , R11
jne consequence$257
jmp alternative$258
consequence$260:
mov RAX , 14
jmp continuation$259
alternative$261:
mov RAX , 6
jmp continuation$259
lt$16:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$260
jmp alternative$261
consequence$262:
mov RAX , 14
jmp RBX
alternative$263:
mov RAX , 6
jmp RBX
consequence$264:
cmp RDX , RCX
jg consequence$262
jmp alternative$263
alternative$265:
mov RAX , 20798
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
consequence$269:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$267
jmp alternative$268
alternative$270:
mov RAX , 20542
jmp RBX
continuation$271:
mov R11 , 6
cmp RAX , R11
jne consequence$269
jmp alternative$270
consequence$272:
mov RAX , 14
jmp continuation$271
alternative$273:
mov RAX , 6
jmp continuation$271
gt$19:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$272
jmp alternative$273
consequence$274:
mov RAX , 14
jmp RBX
alternative$275:
mov RAX , 6
jmp RBX
eq$22:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
cmp RCX , RAX
je consequence$274
jmp alternative$275
consequence$276:
mov RAX , 14
jmp RBX
alternative$277:
mov RAX , 6
jmp RBX
consequence$278:
cmp RDX , RCX
jle consequence$276
jmp alternative$277
alternative$279:
mov RAX , 24894
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
consequence$283:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$281
jmp alternative$282
alternative$284:
mov RAX , 24638
jmp RBX
continuation$285:
mov R11 , 6
cmp RAX , R11
jne consequence$283
jmp alternative$284
consequence$286:
mov RAX , 14
jmp continuation$285
alternative$287:
mov RAX , 6
jmp continuation$285
lte$25:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$286
jmp alternative$287
consequence$288:
mov RAX , 14
jmp RBX
alternative$289:
mov RAX , 6
jmp RBX
consequence$290:
cmp RDX , RCX
jge consequence$288
jmp alternative$289
alternative$291:
mov RAX , 28990
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
consequence$295:
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$293
jmp alternative$294
alternative$296:
mov RAX , 28734
jmp RBX
continuation$297:
mov R11 , 6
cmp RAX , R11
jne consequence$295
jmp alternative$296
consequence$298:
mov RAX , 14
jmp continuation$297
alternative$299:
mov RAX , 6
jmp continuation$297
gte$28:
mov RBX , R15
mov RDX , RDI
mov RCX , RSI
mov RAX , RDX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$298
jmp alternative$299
consequence$300:
mov RAX , 14
jmp RBX
alternative$301:
mov RAX , 6
jmp RBX
neq$31:
mov RBX , R15
mov RCX , RDI
mov RAX , RSI
cmp RCX , RAX
jne consequence$300
jmp alternative$301
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
consequence$302:
mov RAX , 14
jmp RBX
alternative$303:
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
je consequence$302
jmp alternative$303
consequence$304:
mov RAX , 14
jmp RCX
alternative$305:
mov RAX , 6
jmp RCX
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
consequence$309:
mov RAX , 14
jmp RCX
alternative$310:
mov R11 , 6
cmp RBX , R11
je consequence$307
jmp alternative$308
continuation$311:
mov R11 , 6
cmp RAX , R11
jne consequence$309
jmp alternative$310
consequence$312:
mov RAX , 14
jmp continuation$311
alternative$313:
mov RAX , 6
jmp continuation$311
isBool$39:
mov RCX , R15
mov RAX , RDI
mov RBX , RAX
mov R11 , 14
cmp RBX , R11
je consequence$312
jmp alternative$313
consequence$314:
mov RAX , 14
jmp RBX
alternative$315:
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
je consequence$314
jmp alternative$315
consequence$316:
mov RAX , 14
jmp RBX
alternative$317:
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
je consequence$316
jmp alternative$317
consequence$318:
mov RAX , 14
jmp RBX
alternative$319:
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
je consequence$318
jmp alternative$319
consequence$320:
mov RAX , 14
jmp RBX
alternative$321:
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
je consequence$320
jmp alternative$321
consequence$322:
mov RAX , 14
jmp RBX
alternative$323:
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
je consequence$322
jmp alternative$323
consequence$324:
mov RAX , 6
jmp RBX
alternative$325:
mov RAX , 14
jmp RBX
not$51:
mov RBX , R15
mov RAX , RDI
mov R11 , 6
cmp RAX , R11
jne consequence$324
jmp alternative$325
consequence$326:
mov RAX , 14
jmp RBX
alternative$327:
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
je consequence$326
jmp alternative$327
consequence$328:
mov RAX , RCX
mov R10 , 18446744073709551608
and RAX , R10
mov R11 , 0
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$329:
mov RAX , 36926
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
car$55:
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
mov RAX , RCX
mov R10 , 18446744073709551608
and RAX , R10
mov R11 , 8
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$334:
mov RAX , 41022
jmp RBX
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
cdr$57:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 1
cmp RAX , R11
je consequence$336
jmp alternative$337
consequence$338:
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
alternative$339:
mov RAX , 45118
jmp RCX
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
makeVector$59:
mov RCX , R15
mov RBX , RDI
mov RAX , RBX
mov R10 , 7
and RAX , R10
mov R11 , 0
cmp RAX , R11
je consequence$341
jmp alternative$342
consequence$343:
mov RAX , RCX
mov R10 , 18446744073709551600
and RAX , R10
mov R11 , 0
mov RAX , [ RAX + R11 ]
jmp RBX
alternative$344:
mov RAX , 49214
jmp RBX
continuation$345:
mov R11 , 6
cmp RAX , R11
jne consequence$343
jmp alternative$344
consequence$346:
mov RAX , 14
jmp continuation$345
alternative$347:
mov RAX , 6
jmp continuation$345
vectorLength$61:
mov RBX , R15
mov RCX , RDI
mov RAX , RCX
mov R10 , 7
and RAX , R10
mov R11 , 3
cmp RAX , R11
je consequence$346
jmp alternative$347
continuation$348:
mov R10 , 16
add RBP , R10
mov RDX , [ RBP - 0 ]
mov RBX , [ RBP - 8 ]
mov RAX , RAX
mov RDI , RBX
mov RSI , RAX
mov R15 , RDX
jmp add$7
continuation$349:
mov R10 , 16
add RBP , R10
mov RDX , [ RBP - 0 ]
mov RBX , [ RBP - 8 ]
mov RAX , RAX
mov RDI , RAX
mov [ RBP - 0 ] , RDX
mov [ RBP - 8 ] , RBX
mov R10 , 16
sub RBP , R10
mov R15 , continuation$348
jmp sum$63
continuation$350:
mov R10 , 16
add RBP , R10
mov RDX , [ RBP - 0 ]
mov RCX , [ RBP - 8 ]
mov RBX , RAX
mov RDI , RCX
mov [ RBP - 0 ] , RDX
mov [ RBP - 8 ] , RBX
mov R10 , 16
sub RBP , R10
mov R15 , continuation$349
jmp cdr$57
consequence$351:
mov RAX , 0
jmp RDX
alternative$352:
mov RDI , RCX
mov [ RBP - 0 ] , RDX
mov [ RBP - 8 ] , RCX
mov R10 , 16
sub RBP , R10
mov R15 , continuation$350
jmp car$55
continuation$353:
mov R10 , 16
add RBP , R10
mov RDX , [ RBP - 0 ]
mov RCX , [ RBP - 8 ]
mov RAX , RAX
mov R11 , 6
cmp RAX , R11
jne consequence$351
jmp alternative$352
sum$63:
mov RDX , R15
mov RCX , RDI
mov RDI , RCX
mov [ RBP - 0 ] , RDX
mov [ RBP - 8 ] , RCX
mov R10 , 16
sub RBP , R10
mov R15 , continuation$353
jmp isEmpty$41
continuation$354:
mov R10 , 8
add RBP , R10
mov RBX , [ RBP - 0 ]
mov RAX , RAX
mov RDI , RAX
mov R15 , RBX
jmp sum$63
continuation$355:
mov R10 , 8
add RBP , R10
mov RBX , [ RBP - 0 ]
mov RAX , RAX
mov RDI , 8
mov RSI , RAX
mov [ RBP - 0 ] , RBX
mov R10 , 8
sub RBP , R10
mov R15 , continuation$354
jmp cons$34
continuation$356:
mov R10 , 8
add RBP , R10
mov RBX , [ RBP - 0 ]
mov RAX , RAX
mov RDI , 16
mov RSI , RAX
mov [ RBP - 0 ] , RBX
mov R10 , 8
sub RBP , R10
mov R15 , continuation$355
jmp cons$34
continuation$357:
mov R10 , 8
add RBP , R10
mov RBX , [ RBP - 0 ]
mov RAX , RAX
mov RDI , 24
mov RSI , RAX
mov [ RBP - 0 ] , RBX
mov R10 , 8
sub RBP , R10
mov R15 , continuation$356
jmp cons$34
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

