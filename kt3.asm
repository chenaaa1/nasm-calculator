 segment   code              ;声明段code开始。
        org    100H                 ;现在以100H作为段内偏移
        JMP    begin                ;跳转到begin处
 		x dw 0            ;第一个数
        y dw 0            ;第二个数
        z dw 0            ; 运算结果
        remainder dw 0 ;余数
        temp dw 0    ; 暂时存储单个数字
        ten  dw 0ah
        op db 0             ;操作符
        symbol db '+'       ; 正负号标志
        strtemp dw 0        ; 暂时存储多个数字
        strshow dw 0        ; 存储结果地址
        ; 用7个message变量存储初始界面变量
        messagebefore db '[x] and [y] is positive whole number',0ah,0dh,'$'
        message db 'Please input x [op] y',0ah,0dh,'$'
        message2 db 'input error,please input again',0ah,0dh,'$'
        message3 db '...........................................',0ah,0dh,'$'
		message4 db 'In operation......','$'
        message5 dw 'remainder=','$'
        message6 db 'Continue typing or no(y/n)','$'
		message7 db 'Please do not exceed 65535', 0ah,0dh,'$'
        message8 db '[+] additive operation :','$'
        message9 db '[-] subtraction operation :','$'
        message10 db '[*] multiply operation :','$'
        message11 db '[/] division operation :','$'
; 初始界面打印提示信息
begin:
        MOV    AX, CS
        MOV    DS, AX               ;数据段同代码段
        MOV    ES, AX               ;附加段同代码段

        ;依次调用对应的字符串来显示初始界面
        ; pleaes input ··· 语句
	  mov dx,message
	  mov ah,09h;显示字符串
	  int 21h 
        ; 分隔符语句·····
      mov dx,message3
	  mov ah,09h;显示字符串
	  int 21h
        ;提示x,y为正整数
      mov dx,messagebefore
	  mov ah,09h;显示字符串
	  int 21h
      mov dx,message3
	  mov ah,09h;显示字符串
	  int 21h        
      mov dx,message7 ;提示结果不要超过65535
	  mov ah,09h
	  int  21h
	  call begin2
	  

; 读入输入的字符
begin2:

         ; strtemp是一个被初始化的内存空间
	  mov dword [strtemp],0

         ; 调用do_before函数来读入第一位数字x
	  call do_before
         ; 把运算符存入[op]中,al中存的为运算符
	  mov [op],al
         ; 把读到的x值移入[x]中,strtemp存的为数字的值
	  mov ax,[strtemp]
	  mov [x],ax
         ; 恢复strtemp
	  mov dword [strtemp],0
         
         ; 读入第二位数字y
	  call do_before
	  mov ax,[strtemp]
	  mov [y],ax 

; 运算并输出结果函数
wihledo:
         ; 进入运算函数do_op
	  call do_op

         ; 将x的值输出到界面,注意这里可以理解为字符串拼接
         mov bx,[x]
         mov [strshow],bx
	  call show

         ; 将运算符输出到界面
         mov dl,[op]
         mov ah,02h
         int 21H

         ; 将y的值输出到界面
         mov bx,[y]
         mov [strshow],bx
         call show

         ; 将'='输出到界面
         mov dl,'='
         mov ah,02h
         int 21H

         ; 判断输出结果是否带负号
         mov dl,[symbol]    ; symbol初值为'+',如果减法出现负数那么改为'-'
         cmp dl,'-'
         jne lan     ; 不为'-'的话进入lan函数
         mov ah,02h  ; 是'-'号的话就输出,注意这个是负号不是减号
         int 21H
       
       ; 把运算结果输出到界面
       lan:
         mov bx,[z]
         mov [strshow],bx
	  call show
         ;
       ; 判断运算符是否为除法,不是的话转入do_move函数
       mov bl,[op]
       cmp bl,'/'
       jne do_move

       ; 是除法的话就在加上余数'remainder='的输出
       call crlf     ; 换行
       mov dx,message5      ; 输出'remainder='
	mov ah,09h;显示字符串
	int 21h 

       ; 在'remainder='后面加上计算得到的余数
       mov bx,[remainder]
       mov [strshow],bx
       call show


; 让用户继续选择输入或结束函数
do_move:
       call crlf
       mov dx,message3      ; message3是分隔符······
	mov ah,09h;显示字符串
	int 21h
       mov dx,message6      ; message6是Continue type···
	mov ah,09h;显示字符串
	int 21h
       ;
       mov ah,1h
       int 21h
       
       ; 保证用户输入时大小写y,n都可以
       cmp al,'y'
       je lan2       ; lan2是进行二次运算函数
       cmp al,'Y'
       je lan2
       cmp al,'n'
       je over       ; over是终止主程序函数
       cmp al,'N'
       je over


; 二次运算函数
lan2:
       call crlf
       ; 把上一次运算的结果输出到界面上
       mov bx,[z]
       mov [strshow],bx
	call show

       ; 读入下一次的运算符
	mov ah,1h
       int 21h          ;调用DOS功能1H号,读入一位数字
	; 判断运算符是什么符号
	cmp al,'+' 
	je done6
	cmp al,'-' 
	je done6
	cmp al,'*'
	je done6
	cmp al,'/' 
	je done6
       ; 比较是否位回车键,回车键ascil码为13
       cmp al,13
       je done6
       ; 如果输入的不是正确的运算符的话就转入报错函数
       call do_error
       ; 奇偶位清除则跳转
       jmp over


done6:
       ; 读入运算符
       mov [op],al
       mov  ax,[z]
       ; 把上次运算结果z记为x
	mov [x],ax

       ; 准备再次读入新的y值
	mov dword [strtemp],0
       call do_before
       mov ax,[strtemp]
	mov [y],ax
       
       ; 跳到运算总函数whiledo
       jmp wihledo


; 终止主程序函数
over:
	 ;
       MOV    AH, 4CH
       INT    21H                  ;结束程序，返回操作系统


;..............................
; 判断读入字符类型函数
do_before:
          mov ah,1h
          int 21h          ;调用DOS功能1H号,读入一位数字
		; 判断是否为加减乘除符号
		cmp al,'+' 
		je done       ; done函数的作用是返回主函数
		cmp al,'-' 
		je done 
		cmp al,'*'
		je done  
		cmp al,'/' 
		je done
              cmp al,13     ; 判断是否为回车键,说明此时为运算结尾
              je done 

          ; 如果一开始输入的不是数字的话直接重新输入
          mov dword [temp],0
          sub al,'0'       ;将数字转化为ASCII码代表的数字
          cmp al,0       ;判断输入的是否是数字(小于0)
          jl do_error       ;不是数字的话结束输入
          cmp al,9      ;判断输入的是否是数字(大于9)
          jg do_error      ;不是数字的话结束输入

          xor ah,ah     ;高位清零
          mov [temp],ax     ; 保留ax原来的值,temp存储一个数字,比如9
          mov ax,[strtemp]  ; strtemp相当于整个数字,比如999

          mov bx,10  ; 给bx赋10相当于十进制进位
          ; mul bx相当于ax * bx,然后就让ax中的值完成了进位
          mul  bx    ; ten 中存储的是10

          add ax,[temp]     ; 进位后在加上原来的数
          mov [strtemp],ax  ; 把得到的多个数字存到strtemp中
          ; 数字没存完的话就再循环来存,直到遇到运算符或结尾符(回车符)
          jmp do_before  ; 输入未结束，循环调用

       ; 返回主程序函数
       done:		
          ret

; 报错函数(输入非法字符时报错)
do_error:
	call crlf
	mov dx,message2 ; 打印出错误提示 'input error,please input again'
	mov ah ,09h
	int 21h
       ;清零al,返回begin2函数
	   xor al,al
       jmp begin2


;..............................
; 运算起始函数，也可以充当加法函数
do_op:
          call crlf       ;调用输出回车子程序
          mov ah,09h
          mov dx,message4   ;输出'In operation......(表示计算中)'
          int 21h
          call crlf ; 换行符
		; 将运算符和x的值移入寄存器中
		mov bl,[op]
              mov ax,[x]

		; 先判断运算符是否为'+'
		cmp bl,'+'
              ; 运算符“不等于” '+'的话跳转到done1函数(判断是否为'-')
		jne done1

              ;如果运算符为'+'的话就进行加法运算
		add ax,[y]
		push ax
		mov dx,message8
	  	mov ah,09h;显示字符串
	  	int 21h
		pop ax
		JMP done4     ; done4函数为输出结果函数
		;

; 减法函数
done1:
              ; 判断运算符是否为'-'号
		cmp bl,'-'
              ; 不为'-'号的话，再跳转到判断'*'号函数done2中
		jne done2 
              
              ; 做减法前要判断是否会产生负数，再跳入不同函数处理
              cmp ax,[y] ;[x]和[y]进行比较
              ; 有符号大于等于则跳转到"无负号减法do_sub函数"
              jge do_sub

              ; x小于y的话就进行有负号减法运算
              mov byte [symbol],'-'
              mov bx,[y]
              sub bx,ax
              mov ax,bx
			push ax
			mov dx,message9
	  		mov ah,09h;显示字符串
	  		int 21h
			pop ax
              jmp done4     ; 输出结果
       ; 无负号减法函数
       do_sub:
		sub ax,[y]
		push ax
		mov dx,message9
	  	mov ah,09h;显示字符串
	  	int 21h
		pop ax
		JMP done4
		;

; 乘法函数
done2:
              ; 判断是否为乘法符号
		cmp bl,'*'
              ; 如果不是乘法符号的话就跳转到done3除法函数
		jne done3

              ; 是乘法符号的话就进行乘法运算
		mul dword [y]
		push ax
		mov dx,message10
		mov ah,09h;显示字符串
	  	int 21h
		pop ax
		JMP done4     ; 输出结果

; 除法函数
done3:
              ; 判断是否为除法符号
		cmp bl,'/' 
              ; 因为前面三个符号已经检测过了
              ; 所以这里还非除法符号的话就转入报错函数
		jne done5

              ; 是除号的话就进行除法运算
              mov bx,[y]
              mov bh,0
		div  bl
              ; 把余数存入[remainder]
              mov [remainder],ah
              mov ah,0
		push ax
		mov dx,message11
	 	mov ah,09h;显示字符串
		int 21h
		pop ax
		jmp done4     ; 输出结果
		;

; 非法运算符报错函数(当用户输入非加减乘除符号时运行)
done5:
              call crlf     ; 换行函数
		mov dx,message2 ; 输出  ’错误‘'input error',0ah,0dh,'$'
		mov ah ,09h
		int 21h
		ret ; 恢复断点，返回主程序

; 保存结果函数
done4:
		mov [z],ax  ;将结果送入z中
              ret ; 恢复断点，返回子程序
 ;..............................
 ; 在计算完后显示输出结果


; 显示结果并结束函数
; 思路：①给bx赋值10，为下一步的div指令做铺垫
; ②用div指令将结果每次除以10，然后把余数压入栈
; ③判断商是否小于0，这是作为判断是否除到了最后一位数的依据
; ④如果商小于0，则把余数和商都压入栈
; 如果商大于0，则重复②③的操作以保证整串数字都压入栈
; ⑤将数字依次出栈并打印至界面
show:
            mov ax,[strshow]            ;strshow中存储要输出的整数
            ; 这里给bx赋值10,方便输出
            ; 例如要输出93,那么后面div bx时商为9存在ax,余数为3存在dx
            ; 然后再将数字压入栈依次输出,最后就能输出strshow中存储的数字
            mov bx,0ah           ;给bx寄存器中赋10,方便结果输出
            mov cx,0            ;cx寄存器作计数器

     step1:  mov dx,0
            div  bx             ;执行div指令后商在ax寄存器中,余数在dx寄存器中
            cmp ax,0ah          ;判断商是否小于10，小于0则说明已到最后一位数字
            ; 若商小于10跳转到n1段函数
            jl n1
       
            ; 若商>=10,则把余数压入栈，然后再算一次
            push dx             ;将余数压入栈
            inc cx              ;计数器加1,cx代表栈中数字的数量
            jmp step1         ;若商不小于10,则一直进行压栈、判断的循环

     ; 将余数和商压入栈
     n1:    push dx           ;将最后一位余数压入栈
            inc cx
            push ax           ;最后一位小于10的商入栈
            inc cx
            jmp step2       ; 跳入step2函数将结果输出到界面

     ; 将栈中数字依次输出并拼接
     step2:  pop dx          ;将栈中数字依次输出
            add dl,'0'      ; 因为mov ah,2h输出的为dl寄存器,所以这里给dl加'0'
                            ;可以理解为 DL 的值加上30H。
                            ; DL 里原先可能是一个 0--9 的数， 加上30H 后 就变成了ASCII 码。
                            ; 如： DL=3, 加上30H后 DL=33H (也就是 DL='3' ) 变成了字符。 
            mov ah,2h       ; 输出单个字符
            int 21h
            loop step2      ; 重复操作至字符串输出完成
            ret


;..............................
; 换行函数
crlf:      ;回车换行
            mov ah,2
            mov dl,0DH
            int 21H
            mov dl,0AH
            int 21H
            ret                