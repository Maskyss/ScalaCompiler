DATA SEGMENT
	decem1 DD .028
	decem2 DQ 0.025
	decem3 DD 13493
	des1 DD 4567d
	des2 DQ 16788855d
DATA ENDS

cs:DATA, ds:CODE
CODE SEGMENT
	Finit
	Fadd dword ptr ds:decem2[eax]
	Fcom st(5)
	Fadd dword ptr cs:decem1[edi]
	Faddp st(1), st(0)
	Fadd dword ptr des1[ebx]
	Faddp st(7), st(0)
	Fadd qword ptr es:des2[edx]
	Fadd qword ptr  es: des2[esp]
	Fcom st(1)
	Fadd qword ptr es:des2[esp]
	Faddp st(1), st(0)
	Fadd dword ptr ss:decem3[ebx]
	Fadd dword ptr des1[ebp]
	Fcom st(7)
	Fadd dword ptr  fs:decem1[esi]
	Faddp st(2), st(0)
		

CODE ENDS
END