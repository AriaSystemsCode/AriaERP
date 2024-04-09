<%
Function GetFormat(strSource,strFormat)
	Dim intpos1
	Dim intPos2
	Dim intTotLegth
	Dim intOldPos
	Dim strOutPut
	
	intTotLegth = Len(strFormat)
	intOldPos = 1
	intPos1 = 1
	strOutPut = ""
	intpos = 1
	Do while Not intPos1 = 0
		intpos1 = instr(intOldPos, strFormat, "-",1)
		intpos2 = instr(intOldPos, strFormat, "/",1)
		IF intpos1 > intpos2 OR intpos1 = 0 Then
			intpos1 = intpos2
		End IF
		IF intpos1 = 0 Then
			strOutPut = strOutPut & mid(strSource, intpos, intTotLegth)
		Else
			strOutPut = strOutPut & mid(strSource, intpos, intpos1 - intOldPos)
			strOutPut = strOutPut & Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1)
		End if
		intpos = intpos + intpos1 - intOldPos
		intOldPos = intpos1 + 1
	Loop
	GetFormat = strOutPut
	'return GetFormat
End Function
%>