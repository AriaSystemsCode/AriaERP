<%@ LANGUAGE="VBSCRIPT"%>
<%Response.Buffer=true

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	strFile = "reb"
End IF

Session("ConnectionString") = Application("DataConnectionString")
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open(Session("ConnectionString"))
Set RSOtsPrd = Conn.Execute("Select Setups.mData_Def From Setups Where cFld_Name='M_OTSPRIOD'")
Set RSSysTyp = Conn.Execute("Select Setups.mData_Def From Setups Where cFld_Name='M_SYSTYPE'")
%>
<html>
<head>
<title>Style Open To Sell</title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
</head>
<body >

<%
' trace yasser
strSQL  ="Select * From Style Where Style = '"&Request("StyNo")&"'"
' end trace
Dim lcOtsPrd , lnOpenCtPo
lcOtsPrd = UCASE(TRIM(RSOtsPrd("mData_Def")))
lnOpenCtPo = 0
Set RSStyOts  = Conn.Execute("Select * From Style Where Style = '" & Request("StyNo") & "' and status+cstygroup like 'A%'")

'If Not UCASE(Session("Ware")) = "ALL" And Not Len(Session("Ware")) = 0 Then 
if trim(application("WareCode")) <> "" then
  Set RSStyDye = Conn.Execute("Select * From StyDye Where cwarecode+style+dyelot Like '"  & UCASE(application("WareCode"))& Request("StyNo") & "%'")
  strsql = "Select Dist OrdHdr.Status,OrdLine.* From OrdHdr,OrdLine Where "
  strsql = strsql & " Ordline.style+DTOS(Ordline.complete)+Ordline.cordtype+Ordline.order+Ordline.store+STR(Ordline.lineno,6) like '" & Request("StyNo") & "%'"
  strsql = strsql & " And OrdHdr.cOrdType+OrdHdr.Order=OrdLine.cOrdType+OrdLine.Order And OrdHdr.cOrdType+OrdHdr.order like 'O%' and OrdHdr.Status $ 'OH' And OrdHdr.cWareCode = '" & UCASE(application("WareCode")) & "'"
  
  Set RSOrdOts  = server.CreateObject("ADODB.recordset")
  RSOrdOts.Open strsql,conn
Else
  Set RSStyDye = Conn.Execute("Select * From StyDye Where style+cwarecode+dyelot like '" & Request("StyNo") & "%'")
  Set RSOrdOts  = Conn.Execute("Select Dist OrdHdr.Status,OrdLine.* From OrdHdr,OrdLine Where Ordline.Style='" & Request("StyNo") & "' And OrdHdr.cOrdType+OrdHdr.Order=OrdLine.cOrdType+OrdLine.Order And OrdHdr.cOrdType='O' and OrdHdr.Status $ 'OH'")
End If
'Response.Write "<font size=2>" & RSStyDye.eof
'Response.End 
Set RSScale  = Conn.Execute("SELECT * FROM Scale WHERE Type+scale+prepak = 'S"&RSStyOts("Scale")&"'") %><% Set RSShip   = Conn.Execute("SELECT * FROM ShpmtHdr WHERE ShipNo = ''") %>
<% If application("WareCode")<> "" And CLng(RSStyOts("Stk1"))=0 And CLng(RSStyOts("Stk2"))=0 And CLng(RSStyOts("Stk3"))=0 And CLng(RSStyOts("Stk4"))=0 And CLng(RSStyOts("Stk5"))=0 And CLng(RSStyOts("Stk6"))=0 And CLng(RSStyOts("Stk7"))=0 And CLng(RSStyOts("Stk8"))=0 And CLng(RSStyOts("Wip1"))=0 And CLng(RSStyOts("Wip2"))=0 And CLng(RSStyOts("Wip3"))=0 And CLng(RSStyOts("Wip4"))=0 And CLng(RSStyOts("Wip5"))=0 And CLng(RSStyOts("Wip6"))=0 And CLng(RSStyOts("Wip7"))=0 And CLng(RSStyOts("Wip8"))=0 And CLng(RSStyOts("Ord1"))=0 And CLng(RSStyOts("Ord2"))=0 And CLng(RSStyOts("Ord3"))=0 And CLng(RSStyOts("Ord4"))=0 And CLng(RSStyOts("Ord5"))=0 And CLng(RSStyOts("Ord6"))=0 And CLng(RSStyOts("Ord7"))=0 And CLng(RSStyOts("Ord8"))=0 Then %>
	<p>&nbsp;&nbsp;&nbsp; No open to sell information found..</p>
<% Else 
	If Not RSStyOts.Eof Then 
			If RSStyOts("Make") = True Then
				If application("WareCode") <> ""Then 
					Set RSCtPo  = Conn.Execute("Select Dist Cuttktl.*,CuttktH.Complete From Cuttktl,CutTktH Where Cuttktl.CutTkt = CutTktH.CutTkt And Cuttktl.Style='" & Request("StyNo") & "' And CutTktH.Status $ 'OAH' And CutTktL.cWareCode='" & UCASE(application("WareCode")) & "'") 
			  Else 
					Set RSCtPo  = Conn.Execute("Select Dist Cuttktl.*,CuttktH.Complete From Cuttktl,CutTktH Where Cuttktl.CutTkt = CutTktH.CutTkt And Cuttktl.Style='" & Request("StyNo") & "' And CutTktH.Status $ 'OAH'")
				End If 
			Else 
				If application("WareCode") <> "" Then 
					Set RSCtPo  = Conn.Execute("Select Dist PosHdr.Status,Posln.*,PosHdr.Complete,PosHdr.Available From Posln,PosHdr Where PosLn.cStyType+PosLn.Po=PosHdr.cStyType+PosHdr.Po And PosLn.Style='" & Request("StyNo") & "' And PosHdr.Status $ 'OH' And PosHdr.cStyType<>'C' And PosLn.TranCd<>'6' And PosLn.cWareCode='" & UCASE(application("WareCode")) & "'")
				Else 
					Set RSCtPo  = Conn.Execute("Select Dist PosHdr.Status,Posln.*,PosHdr.Complete,PosHdr.Available From Posln,PosHdr Where PosLn.cStyType+PosLn.Po=PosHdr.cStyType+PosHdr.Po And PosLn.Style='" & Request("StyNo") & "' And PosHdr.Status $ 'OH' And PosHdr.cStyType<>'C' And PosLn.TranCd<>'6'")
				End If 
			End If 
			lnOpenCtPo = 1	
			If Not CLng(RSStyOts("Stk1")) = 0 or Not CLng(RSStyOts("Stk2")) = 0 or Not CLng(RSStyOts("Stk3")) = 0 or Not CLng(RSStyOts("Stk4")) = 0 or Not CLng(RSStyOts("Stk5")) = 0 or Not CLng(RSStyOts("Stk6")) = 0 or Not CLng(RSStyOts("Stk7")) = 0 or Not CLng(RSStyOts("Stk8")) = 0 or Not CLng(RSStyOts("Wip1")) = 0 or Not CLng(RSStyOts("Wip2")) = 0 or Not CLng(RSStyOts("Wip3")) = 0 or Not CLng(RSStyOts("Wip4")) = 0 or Not CLng(RSStyOts("Wip5")) = 0 or Not CLng(RSStyOts("Wip6")) = 0 or Not CLng(RSStyOts("Wip7")) = 0 or Not CLng(RSStyOts("Wip8")) = 0 or Not CLng(RSStyOts("Ord1")) = 0 or Not CLng(RSStyOts("Ord2")) = 0 or Not CLng(RSStyOts("Ord3")) = 0 or Not CLng(RSStyOts("Ord4")) = 0 or Not CLng(RSStyOts("Ord5")) = 0 or Not CLng(RSStyOts("Ord6")) = 0 or Not CLng(RSStyOts("Ord7")) = 0 or Not CLng(RSStyOts("Ord8")) = 0 Then %><% 
				Dim lcThisYr , lcThisMn , lcThisDy , laNoOfDays(11) , lcPrdMonth , lnPrdMonth , lcPrdYear , lnPrdNum
    
    lcThisYr = Year(Date())
    lcThisMn = Month(Date())
    lcThisDy = INT(CLng(Day(Date())))
    
    laNoOfDays(0)  = "31"
    laNoOfDays(1)  = "28"
    laNoOfDays(2)  = "31"
    laNoOfDays(3)  = "30"
    laNoOfDays(4)  = "31"
    laNoOfDays(5)  = "30"
    laNoOfDays(6)  = "31"
    laNoOfDays(7)  = "31"
    laNoOfDays(8)  = "30"
    laNoOfDays(9)  = "31"
    laNoOfDays(10) = "30"
    laNoOfDays(11) = "31"

    lcPrdMonth = lcThisMn
    lnPrdMonth = INT(CLng(lcThisMn))
    lcPrdYear  = lcThisYr
    Dim laDtPeriod()
    lnPrdNum = 1
    If lcOtsPrd = "E" AND lcThisDy <=15 Then
      lnPrdNum = 11
	  ReDim laDtPeriod(10,2)
    Else
      lnPrdNum = 10
	  ReDim laDtPeriod(9,2)
	End If
    If lcOtsPrd = "W" Then 
      If lcThisDy >= 8 AND lcThisDy <= 15 Then
        lnPrdNum = 11
        ReDim laDtPeriod(10,2)
      ElseIf lcThisDy >= 16 AND lcThisDy <= 22 Then
        lnPrdNum = 12
        ReDim laDtPeriod(11,2)
      ElseIf lcThisDy >= 22 AND lcThisDy <= 31 Then
        lnPrdNum = 13
        ReDim laDtPeriod(12,2)
      Else
        lnPrdNum = 10
  	    ReDim laDtPeriod(9,2)
	  End If
    Else
      lnPrdNum = 10
	  ReDim laDtPeriod(9,2)
	End If
    lnPrdNum = INT(CLng(lnPrdNum))
    Dim I , lcPrdCnt , lcDatToSt
	lcDatToSt = lcPrdMonth & "/01/" & lcPrdYear
    For I=0 TO lnPrdNum-1
      lcPrdCnt = CInt(I+1)
      If Len(lcPrdCnt) = 1 Then 
        lcPrdCnt = " " & lcPrdCnt
      End If
	  Select Case lcOtsPrd
		Case "E"
		  If lcPrdCnt = " 1" or lcPrdCnt = " 3" or lcPrdCnt = " 5" or lcPrdCnt = " 7" or lcPrdCnt = " 9" or lcPrdCnt = "11" Then
			lcDatToSt = lcPrdMonth & "/01/" & lcPrdYear
			laDtPeriod(I,0) = DateAdd("d",0,lcDatToSt)
            
			lcDatToSt = lcPrdMonth & "/15/" & lcPrdYear
			laDtPeriod(I,1) = DateAdd("d",0,lcDatToSt)
            
			laDtPeriod(I,2) = MonthName(INT(CLng(lcPrdMonth)) , True) & ". 01-15"
          Else
			lcDatToSt = lcPrdMonth & "/16/" & lcPrdYear
			laDtPeriod(I,0) = DateAdd("d",0,lcDatToSt)
			
			If INT(CLng(lcPrdYear)) MOD 4 = 0 Then
			  laNoOfDays(1)   = "29"
			Else
			  laNoOfDays(1)   = "28"
		    End If
            lcDatToSt  = lcPrdMonth & "/" & TRIM(laNoOfDays(lnPrdMonth-1)) & "/" & lcPrdYear
			laDtPeriod(I,1) = DateAdd("d",0,lcDatToSt)
            
			laDtPeriod(I,2) = MonthName(INT(CLng(lcPrdMonth)) , True) & ". 16-" & TRIM(laNoOfDays(lnPrdMonth-1))
            If lnPrdMonth = 12 Then
	          lcPrdYear  = CStr(INT(CLng(lcPrdYear)) + 1)
              lnPrdMonth = 1
              lcPrdMonth = 1
            Else
              lnPrdMonth = lnPrdMonth + 1
              lcPrdMonth = lcPrdMonth + 1
			End If
		  End If
		Case "W"
		  If lcPrdCnt = " 1" or lcPrdCnt = " 5" or lcPrdCnt = " 9" or lcPrdCnt = "13" Then
              lcDatToSt = lcPrdMonth & "/01/" & lcPrdYear
			  laDtPeriod(I,0) = DateAdd("d",0,lcDatToSt)

              lcDatToSt = lcPrdMonth & "/07/" &lcPrdYear
			  laDtPeriod(I,1) = DateAdd("d",0,lcDatToSt)

			  laDtPeriod(I,2) = MonthName(INT(CLng(lcPrdMonth)) , True) & ". 01-07"
		  ElseIf lcPrdCnt = " 2" or lcPrdCnt = " 6" or lcPrdCnt = "10" Then
			  lcDatToSt = lcPrdMonth & "/08/" & lcPrdYear
			  laDtPeriod(I,0) = DateAdd("d",0,lcDatToSt)

			  lcDatToSt = lcPrdMonth & "/15/" & lcPrdYear
			  laDtPeriod(I,1) = DateAdd("d",0,lcDatToSt)

              laDtPeriod(I,2) = MonthName(INT(CLng(lcPrdMonth)) , True) & ". 08-15"
		  ElseIf lcPrdCnt = " 3" or lcPrdCnt = " 7" or lcPrdCnt = "11" Then
			  lcDatToSt = lcPrdMonth & "/16/" & lcPrdYear
			  laDtPeriod(I,0) = DateAdd("d",0,lcDatToSt)

			  lcDatToSt = lcPrdMonth & "/22/" & lcPrdYear
			  laDtPeriod(I,1) = DateAdd("d",0,lcDatToSt)

              laDtPeriod(I,2) = MonthName(INT(CLng(lcPrdMonth)) , True) & ". 16-22"
		  ElseIf lcPrdCnt = " 4" or lcPrdCnt = " 8" or lcPrdCnt = "12" Then
			  lcDatToSt = lcPrdMonth & "/23/" & lcPrdYear
			  laDtPeriod(I,0) = DateAdd("d",0,lcDatToSt)
			  
			  If INT(CLng(lcPrdYear)) MOD 4 = 0 Then
                laNoOfDays(1)   = "29"
			  Else
			    laNoOfDays(1)   = "28"
			  End If
			  lcDatToSt = lcPrdMonth & "/" & TRIM(laNoOfDays(lnPrdMonth-1)) & "/" & lcPrdYear
			  laDtPeriod(I,1) = DateAdd("d",0,lcDatToSt)
              
			  laDtPeriod(I,2) = MonthName(INT(CLng(lcPrdMonth)) , True) & ". 23-" & TRIM(laNoOfDays(lnPrdMonth-1))
              If lnPrdMonth = 12 Then
  	            lcPrdYear  = CStr(INT(CLng(lcPrdYear)) + 1)
                lnPrdMonth = 1
			    lcPrdMonth = 1
              Else
                lnPrdMonth = lnPrdMonth + 1
			    lcPrdMonth = lcPrdMonth + 1
			  End If
		  End If
		Case "M"  
          lcDatToSt = lcPrdMonth & "/01/" & lcPrdYear
		  laDtPeriod(I,0) = DateAdd("d",0,lcDatToSt)

		  If INT(CLng(lcPrdYear)) MOD 4 = 0 Then
            laNoOfDays(1)   = "29"
		  Else
		    laNoOfDays(1)   = "28"
		  End If 
		  lcDatToSt = lcPrdMonth & "/" & TRIM(laNoOfDays(lnPrdMonth-1)) & "/" & lcPrdYear
		  laDtPeriod(I,1) = DateAdd("d",0,lcDatToSt)
          
		  laDtPeriod(I,2) = MonthName(INT(CLng(lcPrdMonth)))
          If lnPrdMonth = 12 Then
            lcPrdYear  = CStr(INT(CLng(lcPrdYear)) + 1)
            lnPrdMonth = 1
			lcPrdMonth = 1
          Else
            lnPrdMonth = lnPrdMonth + 1
		    lcPrdMonth = lcPrdMonth + 1
		  End If
      End Select
    Next  
	
	For I=0 TO lnPrdNum-1
	  lcStrDate = DateAdd("d",0,laDtPeriod(I,0))
	  lcEndDate = DateAdd("d",0,laDtPeriod(I,1))
	  If Date() > lcStrDate And Date() > lcEndDate Then
	    lnCurrent = INT(CLng(I))
	  End If
	Next
    lnCurrent = lnCurrent + 1
	For I = 0 TO lnPrdNum - 1
	  If lnCurrent > (lnPrdNum-1) Then
	    Exit For 
      End If
      laDtPeriod(I,0) = laDtPeriod(lnCurrent,0)
	  laDtPeriod(I,1) = laDtPeriod(lnCurrent,1)
	  laDtPeriod(I,2) = laDtPeriod(lnCurrent,2)
      lnCurrent = lnCurrent + 1
	Next
    
    laDtPeriod(0,0) = "1/1/1901"
    laDtPeriod(0,0) = DateAdd("d" , 0 , laDtPeriod(0,0))
	laDtPeriod(0,2) = "Current"
	
	Dim laOTS(7,11)
	If trim(application("WareCode")) <> "" Then 
	  For I = 0 TO 7
	    lcI = CStr(I+1)
	    laOts(I,0)  = INT(CLng(RSStyDye("Stk" & lcI)))
	    laOts(I,11) = INT(CLng(RSStyDye("Ord" & lcI))) + INT(CLng(RSStyDye("Shp" & lcI)))
	  Next 
	Else
	  For I = 0 TO 7
		lcI = CStr(I+1)
	    laOts(I,0)  = INT(CLng(RSStyOts("Stk" & lcI)))
	    laOts(I,11) = INT(CLng(RSStyOts("Ord" & lcI))) + INT(CLng(RSStyOts("Shp" & lcI)))
	  Next 
	End If
	Dim lcCompDt , lnSign , lnSign1 , lnSign2
	Dim lcCrSMon , lcRest , lcCrSDay , lcCrSYer , lcFtEMon , lcFtEDay , lcFtEYer 
	If Not RSCtPo.Eof Then
	  Do While Not RSCtPo.Eof
        If RSStyOts("Make") = True Then
		  'lcCompDt = CSTR(RSCtPo("Complete"))
		  lcCompDt = RSCtPo("Complete")
		  If RSCtPo("TranCd")="1" Then
            lnSign = 1
          Else
            lnSign = -1
          End If
		Else
	      If InStr(CSTR(RSCtPo("Available")),":") = 0 Then
			lcCompDt = RSCtPo("Available")
		  Else
			lcCompDt = RSCtPo("Complete")
		  End If
		  lnSign1 = 1
		  lnSign2 = 1
		  If RSCtPo("TranCd") = "1" Then
		    lnSign1 = 1
		  Else
		    lnSign1 = -1
          End If
		  If RSCtPo("cStyType") = "P" Or RSCtPo("cStyType") = "N" Then
		    lnSign2 = 1
		  Else
		    lnSign2 = -1
		  End If
		  lnSign = lnSign1 * lnSign2
		End If
		If lcCompDt >= laDtPeriod(0,0) And lcCompDt <= laDtPeriod(9,1) Then 
		  For I = 0 To 7
            lcI = CStr(I+1)
            For J = 1 To 9
			  If lcCompDt >= laDtPeriod(J,0) And lcCompDt <= laDtPeriod(J,1) Then 
			    laOts(I,J) = laOts(I,J) + (INT(CLng(RSCtPo("Qty" & lcI))) * lnSign)
      		    Exit For
			  End If
		    Next
		  Next
	    Else
		  If lcCompDt > laDtPeriod(9,1) Then
			For I = 0 To 7
			  lcI = CStr(I+1)
			  laOts(I,9) = laOts(I,9) + (INT(CLng(RSCtPo("Qty" & lcI))) * lnSign)
            Next
          Else
            FOR I = 0 To 7
              lcI = CStr(I+1)
			  laOts(I,0) = laOts(I,0) + (INT(CLng(RSCtPo("Qty" & lcI))) * lnSign)
            Next
		  End If 
		End If
		If RSCtPo("TranCd") = "3" Then
		  Set RSShip  = Conn.Execute("SELECT * FROM ShpmtHdr WHERE ShipNo = '" & RSCtPo("ShipNo") & "'")
		  If Not RSShip.Eof Then
			lcCompDt = RSShip("Eta")
		  Else
			lcCompDt = RSCtPo("Complete")
		  End If
		  If lcCompDt >= laDtPeriod(0,0) And lcCompDt <= laDtPeriod(9,1) Then 
			For I = 0 To 7
              lcI = CStr(I+1)
              For J = 1 To 9
				If lcCompDt >= laDtPeriod(J,0) And lcCompDt <= laDtPeriod(J,1) Then 
				  laOts(I,J) = laOts(I,J) + INT(CLng(RSCtPo("Qty" & lcI)))
      		      Exit For
			    End If
		     Next
		    Next
	      Else
			If lcCompDt > laDtPeriod(9,1) Then
			  For I = 0 To 7
			    lcI = CStr(I+1)
			    laOts(I,9) = laOts(I,9) + INT(CLng(RSCtPo("Qty" & lcI)))
              Next
            Else
              FOR I = 0 To 7
                lcI = CStr(I+1)
			    laOts(I,0) = laOts(I,0) + INT(CLng(RSCtPo("Qty" & lcI)))
              Next
		    End If 
		  End If
		End If
	    RSCtPo.MoveNext
	  Loop
	End If 
	If Not RSOrdOts.Eof Then
	  Do While Not RSOrdOts.Eof
		lcCompDt = RSOrdOts("Start")
		If lcCompDt >= laDtPeriod(0,0) And lcCompDt <= laDtPeriod(9,1) Then 
          For I = 0 To 7
            lcI = CStr(I+1)
            For J = 0 To 9
			  If lcCompDt >= laDtPeriod(J,0) And lcCompDt <= laDtPeriod(J,1) Then
			    laOts(I,J) = laOts(I,J) + (INT(CLng(RSOrdOts("Qty" & lcI))) * -1)
      		    Exit For
			  End If
		    Next
		  Next
	    Else
		  If lcCompDt > laDtPeriod(9,1) Then
		    For I = 0 To 7
			  lcI = CStr(I+1)
			  laOts(I,9) = laOts(I,9) + (INT(CLng(RSOrdOts("Qty" & lcI))) * -1)
            Next
          Else
            FOR I = 0 To 7
              lcI = CStr(I+1)
			  laOts(I,0) = laOts(I,0) + (INT(CLng(RSOrdOts("Qty" & lcI))) * -1)
            Next
		  End If 
		End If
		RSOrdOts.MoveNext
      Loop
	End If
    
	Dim lcOnHand
    lcOnHand = 0
    For lnSz = 0 TO 7
      For I=0 TO 9
	    If laOts(lnSz,I) < 0 Then
    	  lcOnHand = laOts(lnSz,I)
	      laOts(lnSz,I) = 0
	      If I = 0 Then
	        For J = 0 TO 9
	          lcOnHand =  lcOnHand + laOts(lnSz,J)
	          If lcOnHand <= 0 Then
	      	    laOts(lnSz,J) = 0
	          Else
                laOts(lnSz,J) = lcOnHand
                lcOnHand = 0
                Exit For
	          End If
	        Next
          Else
            For J = I-1 To 0 Step -1
              lcOnHand =  lcOnHand + laOts(lnSz,J)
              If lcOnHand <= 0 Then
                laOts(lnSz,J) = 0
              Else
          	    laOts(lnSz,J) = lcOnHand
                lcOnHand = 0
                Exit For
              End If
            Next
            If (lcOnHand < 0) AND (I < 9) Then
              For J = I+1 To 9
                lcOnHand =  lcOnHand + laOts(lnSz,J)
                If lcOnHand <= 0 Then
            	  laOts(lnSz,J) = 0
                Else
            	  laOts(lnSz,J) = lcOnHand
            	  lcOnHand = 0
            	  Exit For
                End If
              Next
            End If
          End If
        End If
      Next
      If lcOnHand < 0 Then
	    laOts(lnSz,9) = lcOnHand
      End If
      lcOnHand = 0
    Next
    For lnSz = 0 TO 7
      For I = 0 TO 9
	    laOts(lnSz,10) = laOts(lnSz,10) + laOts(lnSz,I)
      Next
    Next
%>
<Table width=100% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<tr>
<td height=72>
<iframe src="../Banner.asp" width =100% height = 90 border=0 frameborder=0 scrolling=no ></iframe> 
</td>
</tr>
</table>

<Table width=100% align=center><TR><TD align=center>
    <b><font face="Arial" size="2">Style Open To Sell For Multiple Periods</font></p></b>
 </TD></TR></Table>
<div align="center">
  <center>
<table border="1" width=100% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
     <tr>
    
        <td Align="left" valign="center" width="7%" class=dark_cell><strong>Size</strong></font></td>
        <% For I = 0 To 8 %>
	      <td Align="right" width="7%" class=dark_cell><strong><% Response.Write(laDtPeriod(I,2)) %></strong></font>&nbsp;</td>
        <% Next %>
        <td Align="right" width="7%" class=dark_cell><strong>Future</strong></font></td>
        <td Align="right" width="7%" class=dark_cell><strong>Total Available</strong></font></td>
        <td Align="right" width="7%" class=dark_cell><strong>Total Sold YTD</strong></font></td>
    </tr>
    <% Dim lnScalCnt %><% lnScalCnt = INT(CLng(RSScale("Cnt"))) - 1  %><% For I = 0 To lnScalCnt %>
    <tr>
        <% lcI = CStr(I+1) %><td Align="left" valign="center" width="7%" class=dark_cell><strong><% Response.Write(RSScale("SZ" & lcI)) %></strong></font>&nbsp;</td>
        <% For J = 0 To 11 %><td Align="right" valign="center" width="7%" class=light_cell><% Response.Write(laOts(I,J)) %></font>&nbsp;</td>
        <% Next %>
    </tr>
    <% Next %>
    <tr>
        <td Align="left" valign="center" width="7%" class=dark_cell><strong>Total</strong></font></td>
        <% Dim lnTotal %><% For I = 0 To 11 %><% lnTotal = 0 %><% For J = 0 To lnScalCnt %><% lnTotal = lnTotal + laOts(J,I) %><% Next %><td Align="right" valign="center" width="7%"><font color="navy" size="2" face="Arial"><% Response.Write(lnTotal) %></font>&nbsp;</td>
        <% Next %>
    </tr>
</table>
  </center>
</div>
<br>
<Table width=100% align=center><TR><TD align=center>
<input TYPE="BUTTON" VALUE="Close" onClick="window.close()" id="BUTTON1" name="BUTTON1">
</TD></TR></Table>
<% Else %>
<p>No open to sell information found..</p>
<% End If %><% Else %>
<p>No open to sell information found.. <% End If %> <% End If %> </p>
</body>
</html>
<% RSOtsPrd.Close %><% RSSysTyp.Close %><% RSStyOts.Close %><% RSOrdOts.Close %><% RSShip.Close %><% Conn.Close %><% Set RSOtsPrd = Nothing %><% Set RSSysTyp = Nothing %><% Set RSStyOts  = Nothing %><% Set RSOrdOts  = Nothing %><% Set RSShip   = Nothing %><% If lnOpenCtPo = 1 Then %><% Set RSCtPo  = Nothing %><% End If %>