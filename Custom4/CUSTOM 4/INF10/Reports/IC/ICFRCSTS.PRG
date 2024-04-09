*:***************************************************************************
*: Program Name  : ICFRCSTS
*: Program desc. : Forecast the style qty to be ordered
*: For Report    : (INF10 - Infinity Classics Inc.)
*: System        : Aria Advantage Series.4XP
*: Module        : Invetory Control (IC)
*: Developer     : Mariam Mazhar (MMT)
*: Date          : 08/03/2006 (C132519)
*!*************************************************************
*! Modifications :
*! B608160,1 WAM 07/12/2007 Fix calculating last year sales
*************************************************************************
lcStTime = TIME()
DIMENSION loOgScroll.laCRParams[5,2]
 
IF EMPTY(ldRpStrtDat)
  =gfModalGen('TRM00250B00000','DIALOG', 'Calucatations Start Date') 
  RETURN .F.
ENDIF 
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'ForeCast for the Style QTY to be Ordered'

loOgScroll.laCRParams[2,1] = 'Period '
loOgScroll.laCRParams[2,2] = lcRpPeriod

loOgScroll.laCRParams[3,1] = 'StartDate'
loOgScroll.laCRParams[3,2] = ldRpStrtDat

loOgScroll.laCRParams[4,1] = 'lnMajLen'
loOgScroll.laCRParams[4,2] = lnMajorLen

loOgScroll.laCRParams[5,1] = 'lcRpStyle'
loOgScroll.laCRParams[5,2] = lcRpStyle

IF llRpSize
  loOgScroll.lcOGLastForm = 'ICFRCSTS'
ELSE
  loOgScroll.lcOGLastForm = 'ICFRCSSA'
ENDIF 

loOgScroll.cCRorientation = 'L'
IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data........" NOWAIT 
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    = lfStyTemp()  && Create temporary cursor.
  ENDIF
  lfCollect()
ELSE
  IF FILE(oAriaApplication.WorkDir +  lcWorkFile + ".DBF") AND !USED(lcWorkFile)
    USE oAriaApplication.WorkDir +  lcWorkFile + ".DBF" IN 0 
  ENDIF 
ENDIF 
SELECT(lcWorkFile)
IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkFile + ".DBF"
SELECT(lcWorkFile)
USE 
gfDispRe()
RETURN 
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/03/2006 
*! Purpose   : When function of OG
*!*************************************************************
FUNCTION lfwRepWhen
loDBFStyle    = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))
loDBFPOSLN    = CreateObject("RemoteTable","POSLN","POSLNS",'POSLN',SET("DATASESSION"))
loDBFPOSHDR   = CreateObject("RemoteTable","POSHDR","POSHDR",'POSHDR',SET("DATASESSION"))
*B608160,1 WAM 07/12/2007 Fix calculating last year sales
*loDBFOrdline  = CreateObject("RemoteTable","Ordline","Ordline",'Ordline',SET("DATASESSION"))
loDBFOrdline  = CreateObject("RemoteTable","Ordline","ORDLINES",'Ordline',SET("DATASESSION"))
*B608160,1 WAM 07/12/2007 (End)

loDBFordhdr   = CreateObject("RemoteTable","ordhdr","ordhdr",'ordhdr',SET("DATASESSION"))

*!*************************************************************
*! Name      : lfStyTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/03/2006 
*! Purpose   : Create temporary cursor.
*!*************************************************************
FUNCTION lfStyTemp
DIMENSION laTempStru[29,4]
STORE '' TO laTempStru

laTempStru[1,1] = 'Style'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 19
laTempStru[1,4] = 0

laTempStru[2,1] = 'Stk1'
laTempStru[2,2] = 'N'
laTempStru[2,3] = 7
laTempStru[2,4] = 0

laTempStru[3,1] = 'Stk2'
laTempStru[3,2] = 'N'
laTempStru[3,3] = 7
laTempStru[3,4] = 0

laTempStru[4,1] = 'Stk3'
laTempStru[4,2] = 'N'
laTempStru[4,3] = 7
laTempStru[4,4] = 0

laTempStru[5,1] = 'Stk4'
laTempStru[5,2] = 'N'
laTempStru[5,3] = 7
laTempStru[5,4] = 0

laTempStru[6,1] = 'Stk5'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 7
laTempStru[6,4] = 0

laTempStru[7,1] = 'Stk6'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 7
laTempStru[7,4] = 0

laTempStru[8,1] = 'Stk7'
laTempStru[8,2] = 'N'
laTempStru[8,3] = 7
laTempStru[8,4] = 0

laTempStru[9,1] = 'Stk8'
laTempStru[9,2] = 'N'
laTempStru[9,3] = 7
laTempStru[9,4] = 0

laTempStru[10,1] = 'TotStk'
laTempStru[10,2] = 'N'
laTempStru[10,3] = 9
laTempStru[10,4] = 0

laTempStru[11,1] = 'WIP1'
laTempStru[11,2] = 'N'
laTempStru[11,3] = 7
laTempStru[11,4] = 0

laTempStru[12,1] = 'WIP2'
laTempStru[12,2] = 'N'
laTempStru[12,3] = 7
laTempStru[12,4] = 0

laTempStru[13,1] = 'WIP3'
laTempStru[13,2] = 'N'
laTempStru[13,3] = 7
laTempStru[13,4] = 0

laTempStru[14,1] = 'WIP4'
laTempStru[14,2] = 'N'
laTempStru[14,3] = 7
laTempStru[14,4] = 0

laTempStru[15,1] = 'WIP5'
laTempStru[15,2] = 'N'
laTempStru[15,3] = 7
laTempStru[15,4] = 0

laTempStru[16,1] = 'WIP6'
laTempStru[16,2] = 'N'
laTempStru[16,3] = 7
laTempStru[16,4] = 0

laTempStru[17,1] = 'WIP7'
laTempStru[17,2] = 'N'
laTempStru[17,3] = 7
laTempStru[17,4] = 0

laTempStru[18,1] = 'WIP8'
laTempStru[18,2] = 'N'
laTempStru[18,3] = 7
laTempStru[18,4] = 0

laTempStru[19,1] = 'TotWIP'
laTempStru[19,2] = 'N'
laTempStru[19,3] = 9
laTempStru[19,4] = 0

laTempStru[20,1] = 'LSTYRS1'
laTempStru[20,2] = 'N'
laTempStru[20,3] = 7
laTempStru[20,4] = 0

laTempStru[21,1] = 'LSTYRS2'
laTempStru[21,2] = 'N'
laTempStru[21,3] = 7
laTempStru[21,4] = 0

laTempStru[22,1] = 'LSTYRS3'
laTempStru[22,2] = 'N'
laTempStru[22,3] = 7
laTempStru[22,4] = 0

laTempStru[23,1] = 'LSTYRS4'
laTempStru[23,2] = 'N'
laTempStru[23,3] = 7
laTempStru[23,4] = 0

laTempStru[24,1] = 'LSTYRS5'
laTempStru[24,2] = 'N'
laTempStru[24,3] = 7
laTempStru[24,4] = 0

laTempStru[25,1] = 'LSTYRS6'
laTempStru[25,2] = 'N'
laTempStru[25,3] = 7
laTempStru[25,4] = 0

laTempStru[26,1] = 'LSTYRS7'
laTempStru[26,2] = 'N'
laTempStru[26,3] = 7
laTempStru[26,4] = 0

laTempStru[27,1] = 'LSTYRS8'
laTempStru[27,2] = 'N'
laTempStru[27,3] = 7
laTempStru[27,4] = 0

laTempStru[28,1] = 'TOTLSTYRS'
laTempStru[28,2] = 'N'
laTempStru[28,3] = 9
laTempStru[28,4] = 0

laTempStru[29,1] = 'nCaseQty'
laTempStru[29,2] = 'N'
laTempStru[29,3] = 5
laTempStru[29,4] = 0

 = gfCrtTmp(lcWorkFile ,@laTempStru,"Style" ,lcWorkFile,.F.)

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*! Date      : 08/03/2006 
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 

CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/03/2006 
*! Purpose   : Collect data 
*!*************************************************************
FUNCTION lfCollect 
llUseStyle = .F.
llUseGrp   = .F.

ldStartDate = ldRpStrtDat
ldEndDate = {}
DO CASE

  CASE  lcRpPeriod= "THREE"  
    ldEndDate = GOMONTH(ldRpStrtDat,3)
  CASE  lcRpPeriod= "SIX"      
    ldEndDate = GOMONTH(ldRpStrtDat,6)  
  CASE  lcRpPeriod= "NINE"      
    ldEndDate = GOMONTH(ldRpStrtDat,9)  
  CASE  lcRpPeriod= "TWLV"      
    ldEndDate = GOMONTH(ldRpStrtDat,12)  
ENDCASE 

ldStartDateLast  = GOMONTH(ldStartDate,-12)
ldEndDateLast  = {}
DO CASE

  CASE lcRpPeriod =  "THREE"  
    ldEndDateLast   = GOMONTH(ldStartDateLast,3)
  CASE  lcRpPeriod= "SIX"      
    ldEndDateLast   = GOMONTH(ldStartDateLast,6)  
  CASE  lcRpPeriod= "NINE"      
    ldEndDateLast   = GOMONTH(ldStartDateLast,9)  
  CASE  lcRpPeriod= "TWLV"      
    ldEndDateLast   = GOMONTH(ldStartDateLast,12)  
ENDCASE 


*--Style
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.STYLE'),1)
IF lnPosition > 0
  lcStylFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
IF llUseStyle 
  SELECT(lcStylFile)
  LOCATE 
  IF EOF()
    llUseStyle = .F.
  ENDIF 
ENDIF 
*--Style Group
lnGrpPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'STYLE.CSTYGROUP'),1)
IF lnGrpPos > 0
  lcGrpStr = LOOGSCROLL.laOGFXFlt[lnGrpPos,6]
  lcGrpFile = loOGScroll.gfTempName()
  llUseGrp = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcGrpFile)
ENDIF

IF llUseStyle && case user selected a style range
  SELECT(lcStylFile)
  SCAN 
    loDBFStyle.Seek(&lcStylFile..Style,'Style') 
    SELECT  STYLE 
    SCAN REST WHILE STYLE = &lcStylFile..Style FOR IIF(llUseGrp ,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.)
      m.Style = STYLE.Style
     *--Stk
      m.Stk1 = Style.Stk1
      m.Stk2 = Style.Stk2
      m.Stk3 = Style.Stk3
      m.Stk4 = Style.Stk4
      m.Stk5 = Style.Stk5
      m.Stk6 = Style.Stk6
      m.Stk7 = Style.Stk7
      m.Stk8 = Style.Stk8
      m.TotStk = Style.TotStk
      m.nCaseQty = Style.nCaseQty
      m.wip1 = 0
      m.wip2 = 0
      m.wip3 = 0
      m.wip4 = 0
      m.wip5 = 0
      m.wip6 = 0
      m.wip7 = 0
      m.wip8 = 0
      m.totwip = 0
      *--Wip
      IF loDBFPOSLN.Sqlrun("Select Posln.*,POSHDR.COMPLETE AS PO_Comp,POSHDR.AVAILABLE AS Po_Avl FROM Posln(INDEX = poslns)"+;
                          " INNER JOIN poshdr(INDEX = poshdr) ON posln.cbusdocu = poshdr.cbusdocu "+;
                          " and posln.cstytype = poshdr.cstytype and posln.po = poshdr.po WHERE Posln.Cstytype in ('A','D','P','U','N') AND posln.cinvtype = '0001'"+;
                          " and POSLN.STYLE = '"+&lcStylFile..Style+"' "+;
                          " AND POSHDR.Status IN ('H','O') AND posln.Trancd <> '6'")

        lfGetWIP()
	  ENDIF
	  *--last year sales 
	  m.LSTYRS1 = 0
	  m.LSTYRS2 = 0
	  m.LSTYRS3 = 0
	  m.LSTYRS4 = 0
	  m.LSTYRS5 = 0
	  m.LSTYRS6 = 0
	  m.LSTYRS7 = 0
	  m.LSTYRS8 = 0
	  m.TOTLSTYRS = 0	  	  
	  IF loDBFOrdline.Seek(&lcStylFile..Style,'ORDLINES')
	    SELECT Ordline
	    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = &lcStylFile..Style ;
	         FOR BETWEEN(ordline.Start,ldStartDateLast,ldEndDateLast) AND ;
	             loDBFordhdr.Seek('O'+Ordline.ORDER) AND ordhdr.Status <> 'X' 
		  m.LSTYRS1 = m.LSTYRS1 + Ordline.book1
		  m.LSTYRS2 = m.LSTYRS2 + Ordline.book2
		  m.LSTYRS3 = m.LSTYRS3 + Ordline.book3
		  m.LSTYRS4 = m.LSTYRS4 + Ordline.book4
		  m.LSTYRS5 = m.LSTYRS5 + Ordline.book5
		  m.LSTYRS6 = m.LSTYRS6 + Ordline.book6
		  m.LSTYRS7 = m.LSTYRS7 + Ordline.book7
		  m.LSTYRS8 = m.LSTYRS8 + Ordline.book8
		  m.TOTLSTYRS = m.TOTLSTYRS + Ordline.totbook
	    ENDSCAN 
	  ENDIF 
	  INSERT INTO (lcWorkFile) FROM MEMVAR 
  ENDSCAN 
ENDSCAN   
ELSE
    SELECT  STYLE 
    SCAN FOR IIF(llUseGrp ,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.)
      m.Style = STYLE.Style
      *--Stk
      m.Stk1 = Style.Stk1
      m.Stk2 = Style.Stk2
  	  m.Stk3 = Style.Stk3
      m.Stk4 = Style.Stk4
      m.Stk5 = Style.Stk5
      m.Stk6 = Style.Stk6
      m.Stk7 = Style.Stk7
      m.Stk8 = Style.Stk8
      m.TotStk = Style.TotStk
      m.nCaseQty = Style.nCaseQty
      m.wip1 = 0
      m.wip2 = 0
      m.wip3 = 0
      m.wip4 = 0
      m.wip5 = 0
      m.wip6 = 0
      m.wip7 = 0
      m.wip8 = 0
      m.totwip = 0
      *--Wip
      IF loDBFPOSLN.Sqlrun("Select Posln.*,POSHDR.COMPLETE AS PO_Comp,POSHDR.AVAILABLE AS Po_Avl FROM Posln(INDEX = poslns)"+;
                          " INNER JOIN poshdr(INDEX = poshdr) ON posln.cbusdocu = poshdr.cbusdocu "+;
                          " and posln.cstytype = poshdr.cstytype and posln.po = poshdr.po WHERE Posln.Cstytype in ('A','D','P','U','N') AND posln.cinvtype = '0001'"+;
                          " and POSLN.STYLE = '"+Style.Style+"' "+;
                          " AND POSHDR.Status IN ('H','O') AND posln.Trancd <> '6'")

*      IF loDBFPOSLN.Sqlrun("Select Posln.*,POSHDR.COMPLETE AS PO_Comp,POSHDR.AVAILABLE AS Po_Avl FROM Posln(INDEX = poslns)"+;
                          " INNER JOIN poshdr(INDEX = poshdr) ON posln.cbusdocu = poshdr.cbusdocu "+;
                          " and posln.cstytype = poshdr.cstytype and posln.po = poshdr.po WHERE Posln.Cstytype = 'P'AND posln.cinvtype = '0001'"+;
                          " and POSLN.STYLE = '"+Style.Style+"' AND POSLN.CBUSDOCU IN ('P','U')"+;
                          " AND POSHDR.Status IN ('H','O','A') AND posln.Trancd <> '6'")


        lfGetWIP()
	  ENDIF
	  *--last year sales 
	  m.LSTYRS1 = 0
	  m.LSTYRS2 = 0
	  m.LSTYRS3 = 0
	  m.LSTYRS4 = 0
	  m.LSTYRS5 = 0
	  m.LSTYRS6 = 0
	  m.LSTYRS7 = 0
	  m.LSTYRS8 = 0
	  m.TOTLSTYRS = 0	  	  
	  IF loDBFOrdline.Seek(Style.Style,'ORDLINES')
	    SELECT Ordline
	    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = Style.Style;
	         FOR  BETWEEN(ordline.Start,ldStartDateLast,ldEndDateLast) AND ;
	         	  loDBFordhdr.Seek('O'+Ordline.ORDER) AND ordhdr.Status <> 'X'  
		  m.LSTYRS1 = m.LSTYRS1 + Ordline.book1
		  m.LSTYRS2 = m.LSTYRS2 + Ordline.book2
		  m.LSTYRS3 = m.LSTYRS3 + Ordline.book3
		  m.LSTYRS4 = m.LSTYRS4 + Ordline.book4
		  m.LSTYRS5 = m.LSTYRS5 + Ordline.book5
		  m.LSTYRS6 = m.LSTYRS6 + Ordline.book6
		  m.LSTYRS7 = m.LSTYRS7 + Ordline.book7
		  m.LSTYRS8 = m.LSTYRS8 + Ordline.book8
		  m.TOTLSTYRS = m.TOTLSTYRS + Ordline.totbook
	    ENDSCAN 
	  ENDIF 
	  INSERT INTO (lcWorkFile) FROM MEMVAR 
  ENDSCAN 
ENDIF 
*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/06/2006
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfGetWIP
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/06/2006
*! Purpose   : get WIP
*!*************************************************************
FUNCTION lfGetWIP
PRIVATE lcPo_no
*-- Define aray with dimention of style size count
DIMENSION lnQty[8]
lnQty   = 0

lcPo_no = SPACE(8)

*-- Define 2 arrays to check if there is over rescive or not[Begin]
DIMENSION laPOVlu[8],laOverResv[8]
laPOVlu    = 0
laOverResv = .F.
SELECT Posln 
SCAN FOR cStyType $ "ADPUN" AND TranCd <> '6' AND BETWEEN(IIF(EMPTY(POSLN.Po_Avl) or (!EMPTY(POSLN.Po_Avl) and TTOd(POSLN.Po_Avl) = DATE(1900,1,1)),TTOD(POSLN.PO_Comp),TTOD(POSLN.Po_Avl)),ldStartDate ,ldEndDate )
	*BETWEEN(IIF(EMPTY(POSLN.Po_Avl),POSLN.PO_Comp,POSLN.Po_Avl),ldStartDate ,ldEndDate ) 
  lcTrSign = IIF(Trancd='1',1,-1) * IIF(cBusDocu $ 'R', -1, 1)
  llArrQty = .F.
  IF lcPo_no = cBusDocu+cStyType+PO
    IF Trancd $ '245'
      llArrQty = .T.
      *-- Loop to check the over receive qty.
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        *-- If there is over receive in the current qty., add zero to the qty. array.
        IF laOverResv[lnCount]
          lnQty[lnCount] = 0
        *-- If not over receive the current qty.
        ELSE
          *-- If the current qty. greater than the budget qty. & the budget qty. greater than zero
          IF Qty&lcCount. > laPOVlu[lnCount] AND laPOVlu[lnCount] > 0
            *-- Save budget qty.
            lnQty[lnCount]      = laPOVlu[lnCount]
            *-- Set the flag of over recive to .T.
            laOverResv[lnCount] = .T.
          ELSE
            *-- Save current Budget qty.
            lnQty[lnCount]      = Qty&lcCount. 
            laPOVlu[lnCount]    = laPOVlu[lnCount] - Qty&lcCount. 
            IF laPOVlu[lnCount] = 0
              *-- Set the flag of over recive to .T.
              laOverResv[lnCount] = .T.
            ENDIF
          ENDIF 
        ENDIF  
      ENDFOR
    ENDIF
  *-- If the PO has been changed
  ELSE
    *-- Assign the PO # to the PO variable
    lcPo_no = cBusDocu+cStyType+PO
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      *-- Save the current qty. in the qty. array.
      lnQty[lnCount] = Qty&lcCount.
      *-- Save the budget qty. of the PO line.
      laPOVlu[lnCount] = Qty&lcCount.
      *-- Set the flag of over recive to .F.
      laOverResv = .F.
    ENDFOR
  ENDIF
ENDSCAN 
m.wip1   = laPOVlu[1]
m.wip2   = laPOVlu[2]
m.wip3   = laPOVlu[3]
m.wip4   = laPOVlu[4]
m.wip5   = laPOVlu[5]
m.wip6   = laPOVlu[6]
m.wip7   = laPOVlu[7]
m.wip8   = laPOVlu[8]
m.totwip = laPOVlu[1]+laPOVlu[2]+laPOVlu[3]+laPOVlu[4]+laPOVlu[5]+laPOVlu[6]+laPOVlu[7]+laPOVlu[8]

RETURN


