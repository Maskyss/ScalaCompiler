DATA SEGMENT
	decem1 DD .028
	decem2 DQ 0.025
	decem4 DQ -0.025
	decem3 DD 3.14
	pr1  DD -3.14 
	des1 DD 4567d
	des2 DQ 12345678
	des3 DQ -12345678
DATA ENDS


CODE SEGMENT
	finit
	fadd dword ptr decem2[eax]
	fcom st(5)
	Fadd DWORD ptr cs:decem1[edi]
	Faddp st(1), st(0)
	Fadd dword ptr des1[ebx]
	Faddp st(7), st(0)
	Fadd qword ptr es:des2[edx]
	Fadd qword ptr ES:des2[esp]
	Fcom ST(1)
	Fadd qword ptr des2[esp]
	Faddp st(0), st(0)
	Fadd dword ptr ss:decem3[ebx]
	Fadd dword ptr ss:decem3[esp]
	Fadd dword ptr gs:des3[ecx]
	Fadd dword ptr gs:decem4[esi]
	Fadd dword ptr ds:pr1[ebb]
	Fadd dword ptr des[ebp]
	Fcom st(7)
	Fadd dword ptr  fs:decem1[esi]
	Faddp st(2), st(0)


CODE ENDS
END