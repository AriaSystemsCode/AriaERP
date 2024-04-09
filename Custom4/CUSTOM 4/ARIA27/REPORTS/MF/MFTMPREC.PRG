*:**************************************************************************
*: Program file  : MFTMPREC
*: Program desc. : Open To Sell Report for 
*: Date          : 11/08/1999
*: System        : Aria Advantage Series.
*: Module        : MANUFACTURING (MF)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : E500311
*:**************************************************************************
*: Calls :  
*:       Procedures : lpPrintHdr, lpPrintRep,
*:       Functions  : lfvTmpRecv(), lfBrowTmp(), lfwRepWhen(), lfClearRep(),
*:                  : lfNonMaj()
*:          Globals : gfModalGen(), gfItemMask(), AriaBrow()
*:**************************************************************************
*: This Program is due to E500311 ...
*:**************************************************************************
*
lcRpExp  = STRTRAN(lcRpExp,"CTKTRCVH.","")

*-- Set CtktRcvL to 1st order used
SELECT CtKtRcvL
SET ORDER TO TAG CtKtRcvc

SELECT CtKtRcvH
lcRpExp = [cType = 'M' AND ] + lcRpExp
SET FILTER TO &lcRpExp

GO TOP
*-- IF no records after filteration
IF EOF()
  *Message --> There are no records to display.
  *Buttons --> '             <OK>             '
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE   && there is records proceed with printing
  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT

  DO lpPrintRep      && print the report
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpPrintRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/1999
*! Purpose   : Prints the report
*!**************************************************************************
*! Example   : DO lpPrintRep
*!**************************************************************************
*
PROCEDURE lpPrintRep
*-- this program loops around CtKtRcvH 2 times to print same data with
*-- different layout
ROW     = 99
PAGENO  = 0
R_WIDTH = 'W'
R_TITLE = "Receiving Report"

*-- Headers for first Receiving Report (1st layout)
lcHeader1 = 'Session Description                    Status   Receive Date'
lcHeader2 = '       CutTkt ' + PADR(lcMajTtl,12) + ' ' + PADR(lcNonMajT,7) + ;
             IIF(llDyelot,'Dyelot     ','Reference  ')+;
             'Type       Qty1   Qty2   Qty3   Qty4   Qty5   Qty6   Qty7   Qty8  TotQty'

*-- Initializing variables
STORE 0 TO lnTotalQty,lnGrTotQty
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  lnTotal&lcI   = 0
  lnGrndTot&lcI = 0
ENDFOR

lcTmpRcvNo = TmpRcvNum      && Save Temporay receive number
*-- Scan around the temporary receiving header file (1st loop) [Begin]
SCAN WHILE INKEY() <> 32
  WAIT WINDOW 'Print temporary receiving number ' + TmpRcvNum + ' - <SPACE BAR> to abort' NOWAIT
  *-- If Row exceeded page lines (54)
  IF ROW >= 54
    DO lpPrintHdr WITH 'H'      &&Print 1st Header
  ENDIF

  *-- If Temp Receiving Number is changed
  IF TmpRcvNum <> lcTmpRcvNo
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,000 SAY 'SubTotal of Session :'+lcTmpRcvNo

    lnColumn = 54
    *-- For loop to print Total Quantity for (each) size for (same) Temp Receving No.
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      @ ROW,lnColumn SAY lnTotal&lcI PICTURE '999999'
      *-- accumulate Total Quantity for each size for all Temp Receiving no
      lnGrndTot&lcI = lnGrndTot&lcI + lnTotal&lcI
      lnTotal&lcI = 0
      lnColumn = lnColumn + 7
    ENDFOR
    *-- EndFor to print Total Quantity for (each) size for (same) Temp Receving No.

    *-- print Total Quantity for (all) sizes for (same) Temp Receving No.    
    @ ROW,110 SAY lnTotalQty PICTURE '9999999'
    *-- accumulate Total Quantity for all sizes for all Temp Receiving no
    lnGrTotQty = lnGrTotQty + lnTotalQty
    lnTotalQty = 0
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
  ENDIF
  *-- EndIf of Temp Receiving Number is changed
  
  *-- Print Temporary Receiving header
  @ ROW,000 SAY TmpRcvNum + '  ' + SUBSTR(Cdesc,1,30) + ' ' + ;
                IIF(cStatus='O','Open    ',IIF(cStatus='P','Posted','Approved'))+'   '+DTOC(dDate)
  lcTmpRcvNo = TmpRcvNum

  ROW = ROW + 1
  SELECT CtKtRcvL
  SEEK 'M'+CtKtRcvH.TmpRcvNum
  lcCarton = SPACE(3)
  lcCuttkt = SPACE(6)
  lcStyle  = SPACE(19)
  lcDyelot = SPACE(10)

  *-- loop around the temporary receiving lines (1st loop) [Begin]
  SCAN WHILE cType + TmpRcvNum = 'M' + CtKtRcvH.TmpRcvNum
    IF ROW >= 54
      DO lpPrintHdr WITH 'H'
    ENDIF

    *-- If Carton + Cuttkt no + Style changed
    IF cCarton+Cuttkt+Style <> lcCarton+lcCuttkt+lcStyle
      lcCarton = cCarton
      lcCuttkt = Cuttkt
      lcStyle  = Style

      @ ROW,001 SAY cCarton
      @ ROW,007 SAY Cuttkt
      @ ROW,014 SAY SUBSTR(Style,1,lnMajLen)
      @ ROW,027 SAY SUBSTR(Style,lnClrPo,lnColorLen)

      *-- If company support Dyelot
      IF llDyelot
        IF cCarton+Cuttkt+Style+Dyelot <> lcCarton+lcCuttkt+lcStyle+lcDyelot
          lcDyelot = Dyelot
          @ ROW,034 SAY Dyelot
        ENDIF
      ELSE  && Else Company Doesn't support Deylot
        @ ROW,034 SAY SUBSTR(Reference,1,10)
      ENDIF   
      *-- EndIf of company support Dyelot

    ENDIF
    *-- EndIf of Carton + Cuttkt no + Style changed

    lnColumn = 54
    @ ROW,045 SAY IIF(TranCd='2','Received',IIF(TranCd='3','Others','Cancelled'))

    *-- If Tran code is 2nd Quality Or Damaged we need to accumulate its lines
    *-- and print its total directly
    *-- If TranCd = '3'
    IF TranCd = '3'
	  DIMENSION laOther[8]
	  TotOther = 0
	  laOther  = 0
      SCAN REST WHILE TranCd = '3'
		TotOther = TotOther + TotQty
        *-- For loop to accumulate the Quantites for (each) size for both (2nd Quality and Damaged)
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
		  laOther[lnI] = laOther[lnI] + Qty&lcI
        ENDFOR
        *-- EndFor to accumulate the Quantites for each (size) for both (2nd Quality and Damaged)
      ENDSCAN
	  SKIP -1
	  	
      *-- For loop to print Quantity for (each) size in case of Other (2nd Quality & Damaged)
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        @ ROW,lnColumn SAY laOther[lnI] PICTURE '999999'
        lnTotal&lcI = lnTotal&lcI + laOther[lnI]
        lnColumn = lnColumn + 7
      ENDFOR
      *-- EndFor to print Total Quantity for (each) size in case of Other (2nd Quality & Damaged)

      @ ROW,110 SAY TotOther PICTURE '9999999'   && print total of other (2nd Quality & Damaged)
      lnTotalQty = lnTotalQty + TotOther
    
    ELSE   && Else TranCd not equal '3'

      *-- For loop to print Quantity for (each) size in case (1st Quality Or Cancelled)
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        @ ROW,lnColumn SAY Qty&lcI
        lnTotal&lcI = lnTotal&lcI + Qty&lcI
        lnColumn = lnColumn + 7
      ENDFOR
      *-- EndFor to print Quantity for (each) size in case (1st Quality Or Cancelled)
 
      @ ROW,110 SAY TotQty     && Print Total Quantity of 1st Quality Or Cancelled
      lnTotalQty = lnTotalQty + TotQty
    ENDIF
    *-- EndIf of TranCd equal '3'
    
    ROW = ROW + 1
  ENDSCAN
  *-- loop around the temporary receiving lines (1st loop) [End]

ENDSCAN
*-- Scan around the temporary receiving header file (1st loop)[End]

*-- Subtotal of last session
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,000 SAY 'SubTotal of Session :' + lcTmpRcvNo

*-- Print Temporary receive Totals for all quantities
lnColumn = 54

*-- For loop to print Total Quantity for (each) size for (last) Temp Receving No.
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  @ ROW,lnColumn SAY lnTotal&lcI PICTURE '999999'
  *-- accumulate Total Quantity for (each) size for (all) Temp Receiving no
  lnGrndTot&lcI = lnGrndTot&lcI + lnTotal&lcI
  lnTotal&lcI = 0
  lnColumn = lnColumn + 7
ENDFOR
*-- EndFor to print Total Quantity for (each) size for (last) Temp Receving No.

*-- print Total Quantity for (all) sizes for (last) Temp Receving No.
@ ROW,110 SAY lnTotalQty PICTURE '9999999'
lnGrTotQty = lnGrTotQty + lnTotalQty    
lnTotalQty = 0

ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1

*-- Print Grand Totals for all Temporary receiving quantities.
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW + 1
@ ROW,000 SAY 'Grand Total'

lnColumn = 54
*-- For loop to print Grand Total Quantity for (each) size for (all) Temp Receving No.
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  @ ROW,lnColumn SAY lnGrndTot&lcI PICTURE '999999'
  lnColumn = lnColumn + 7
ENDFOR
*-- EndFor to print Grand Total Quantity for (each) size for (all) Temp Receving No.

*-- print Grand Totals for (all) sizes for (all) Temp Receving No.
@ ROW,110 SAY lnGrTotQty PICTURE '9999999'  
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)

*-- Changing order to temp. index to print Second layout sorted by Style
SELECT CtKtRcvL
SET ORDER TO TAG (lcTmpIndex)

*-- Printing the Receiving Report Recap (2nd layout)
SELECT CtKtRcvH
GO TOP
ROW = 99
R_TITLE = "Receiving Report Recap by style"

*-- header for the 2nd layout
lcHeader1 = PADR(lcMajTtl,12) + ' ' + PADR(lcNonMajT,8) + ' ' + ;
            'Qty1   Qty2   Qty3   Qty4   Qty5   Qty6   Qty7   Qty8  TotQty'

*--Initializing Variables
STORE 0 TO lnTotQty,lnTotalQty
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  lnQty&lcI   = 0
  lnTotal&lcI = 0
ENDFOR

lcTmpRcvNo = TmpRcvNum
*-- Scan around the temporary receiving header file (2nd loop) [Begin]
SCAN
  WAIT WINDOW 'Print temporary receiving recap number ' + TmpRcvNum + ' - <SPACE BAR> to abort' NOWAIT
  IF ROW >= 54
    DO lpPrintHdr WITH ''
  ENDIF

  *-- If Temp Receiving Number is changed 
  IF TmpRcvNum <> lcTmpRcvNo
    lnColumn = 20
    *-- For loop to print Total Quantities for (each) size 
    *-- for (all) Cuttkts in (same) Temp Receiving No
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      @ ROW,lnColumn SAY lnQty&lcI PICTURE '999999'
      lnQty&lcI = 0
      lnColumn = lnColumn + 7
    ENDFOR
    *-- EndFor to print Total Quantities for (each) size for (each) cuttkt

    *-- Print Total Quantity for (all) sizes for (all) cuttkts in (same) Temp Receiving No
    @ ROW,076 SAY lnTotQty PICTURE '9999999'  
    lnTotQty = 0
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,000 SAY 'SubTotal'

    lnColumn = 20
    *-- For loop to print Total Quantities for (each) size 
    *-- for (all) Cuttkts in (same) Temp Receiving No
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      @ ROW,lnColumn SAY lnTotal&lcI PICTURE '999999'
      lnTotal&lcI = 0
      lnColumn = lnColumn + 7
    ENDFOR
    *-- EndFor to print Total Quantities for (each) size for (all) Cuttkts
    
    *-- Print Total Quantity for (all) sizes for (all) cuttkts in (same) Temp Receiving No
    @ ROW,076 SAY lnTotalQty PICTURE '9999999'
    lnTotalQty = 0

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
  ENDIF
  *-- If Temp Receiving Number is changed 

  @ ROW,000 SAY 'Session : ' + TmpRcvNum
  lcTmpRcvNo = TmpRcvNum
  ROW = ROW + 1
  
  SELECT CtKtRcvL
  SEEK 'M'+CtKtRcvH.TmpRcvNum
  lcStyle = Style
  @ ROW,000 SAY SUBSTR(Style,1,lnMajLen)
  @ ROW,013 SAY SUBSTR(Style,lnClrPo,lnColorLen)
  
  *-- loop around the temporary receiving lines (2nd loop) [Begin]
  SCAN WHILE TmpRcvNum = CtKtRcvH.TmpRcvNum
    IF Style <> lcStyle
      lnColumn = 20
      *-- For loop to print Total Quantities for (each) size 
      *-- for (each) Cuttkt in (same) Temp Receiving No
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        @ ROW,lnColumn SAY lnQty&lcI PICTURE '999999'
        lnQty&lcI = 0
        lnColumn = lnColumn + 7
      ENDFOR
      *-- EndFor to print Total Quantities for (each) size for (each) Cuttkt

      *-- Print Total Quantity for (all) sizes for (all) cuctkts in (same) Temp Receiving No
      @ ROW,076 SAY lnTotQty PICTURE '9999999'
      lnTotQty = 0

      ROW = ROW + 1
      IF ROW >= 54
        DO lpPrintHdr WITH 'H'
      ENDIF

      @ ROW,000 SAY SUBSTR(Style,1,lnMajLen)
      @ ROW,013 SAY SUBSTR(Style,lnClrPo,lnColorLen)
      lcStyle = Style
    ENDIF

    *-- For loop to collect (each) Quantity for (each) size 
    *-- for (all) Cuttkts in (same) Temp Receiving no
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      lnQty&lcI   = lnQty&lcI   + Qty&lcI
      lnTotal&lcI = lnTotal&lcI + Qty&lcI
    ENDFOR
    *-- EndFor to collect (each) Quantity for (each) size 
    
    lnTotQty   = lnTotQty   + TotQty
    lnTotalQty = lnTotalQty + TotQty
  ENDSCAN
  *-- loop around the temporary receiving lines (2nd loop) [End]

ENDSCAN
*-- Scan around the temporary receiving header file (2nd loop) [End]

*-- Subtotal of last session
lnColumn = 20

*-- For loop to print Total Quantities for (each) size 
*-- for (each) Cuttkt in (last) Temp Receiving No
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  @ ROW,lnColumn SAY lnQty&lcI PICTURE '999999'
  lnQty&lcI = 0
  lnColumn = lnColumn + 7
ENDFOR
*-- EndFor to print Total Quantities for (each) size for (each) cuttkt

*-- Print Total Quantity for (all) sizes for (each) cuttkts in (last) Temp Receiving No
@ ROW,076 SAY lnTotQty PICTURE '9999999'
lnTotQty = 0

ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,000 SAY 'SubTotal'
lnColumn = 20

*-- For loop to print Total Quantities for (each) size 
*-- for (all) Cuttkts in (last) Temp Receiving No
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  @ ROW,lnColumn SAY lnTotal&lcI PICTURE '999999'
  lnTotal&lcI = 0
  lnColumn = lnColumn + 7
ENDFOR
*-- EndFor to print Total Quantities for (each) size for (all) Cuttkts

*-- Print Total Quantity for (all) sizes for (all) cuttkts in (last) Temp Receiving No
@ ROW,076 SAY lnTotalQty PICTURE '9999999'
lnTotalQty = 0

ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1
*-- End of lpPrintRep.

*!**************************************************************************
*! Name      : lpPrintHdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/1999
*! Purpose   : Print header for Both Receving and Receiving Recap by Style
*!**************************************************************************
*! Parameters : 'H' additional header in case of printing Receiving report
*!**************************************************************************
*! Example   : DO lpPrintHdr WITH 'H'
*!**************************************************************************
*
PROCEDURE lpPrintHdr
PARAMETER lcFirstHdr
PAGENO = PAGENO + 1
DO RPT_HDR WITH 'MFTMPRECV','',R_WIDTH
ROW = 05
@ ROW,00 SAY lcHeader1
IF lcFirstHdr = 'H' 
  ROW = ROW + 1
  @ ROW,00 SAY lcHeader2
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW + 1
*-- End of lpPrintHdr.

*!**************************************************************************
*! Name      : lfvTmpRecv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/1999
*! Purpose   : Validate the entered temp receive numbers.
*!**************************************************************************
*! Example   : = lfvTmpRecv()
*!**************************************************************************
*
FUNCTION lfvTmpRecv
PRIVATE lcCurVar, lnAlias, lcOldVal
lnAlias  = SELECT()

lcCurVar = SYS(18)
SELECT CtKtRcvH
IF !EMPTY(&lcCurVar) AND !SEEK('M'+&lcCurVar)
  lcRetVal = lfBrowTmp()
  &lcCurVar = lcRetVal
ENDIF

SELECT (lnAlias)
*-- End of lfvTmpRecv.

*!**************************************************************************
*! Name      : lfBrowTmp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/1999
*! Purpose   : Browse the temp receive header file.
*!**************************************************************************
*! Example   : = lfBrowTmp()
*!**************************************************************************
*
FUNCTION lfBrowTmp
PRIVATE lcBrFields,laBrowVal
DIMENSION laBrowVal[1]
laBrowVal = SPACE(6)
        
lcBrFields = [TmpRcvNum:9:H="Receive#",cDesc:H="Description",cStatus:H="Status",;
              Carton:H="Carton",NtotStk:H="Stock",NTotDam:H="Damaged",NTotCan:H=;
              "Cancelled",Shipno:H="ShipNo",dDate:H="Date"]

IF AriaBrow([FOR cType = "M"],"Temporary Receiving Numbers",;
             gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,;
             .F.,.F.,"CtKtRcvH.TmpRcvNum","laBrowVal")
ENDIF
RETURN laBrowVal[1]
*-- End of lfBrowTmp.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/1999
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
*-- Create the Index expression on Master file
SELECT CtKtRcvL
INDEX ON cType+TmpRcvNum+Style+STR(RECNO(),6) TAG (lcTmpIndex) OF ;
         (gcWorkDir+lcTmpIndex+'.CDX')
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/1999
*! Purpose   : Close any opened files if user press OG <Close> Button
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
SELECT CtKtRcvL
CLOSE INDEXES
IF FILE(gcWorkDir+lcTmpIndex+'.CDX')
  ERASE (gcWorkDir+lcTmpIndex+'.CDX')
ENDIF
RETURN
*-- End of lfClearRep.

*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/1999
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
*-- End of lfNonMaj.
