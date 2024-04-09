****************************************************************************
*: Program file      : POSTYPGM  (Copied form STY681Z.PRG FOR GMA) (C#101806)
*: Program desc.     : PRINT STYLE PURCHASE ORDER FOR GMA.
*:                   : Converted from Aria26 to Aria27.
*: System            : Aria Apparel System (A27).
*: Module            : Style Purchase Order (PO)
*: Developer         : Khalid Mohi El-Din Mohamed KHM
*: Date              : 03/22/2000
*:**************************************************************************
*:Modifications
*B803599,1 ABD Fix the following bugs:-
*B803599,1     1-The form doesn't print the address correctly. It prints it hared code.
*B803599,1     2-The form is very slow it tacks 5 times 2.6. To print one PO.
*B803685,1 ABD Fix the following bugs:-
*B803685,1     1)The amount and the total amt doesn't correct. Form picks the total 
*B803685,1       amount for whole the PO and put its cost as a cost for one piece,and 
*B803685,1       recalculate the total.
*B803685,1     2)The form doesn't print the whole description for the style.
*B803870,1 KHM 11/29/2000 Fix the bug of printing empty pages when there are more
*B803870,1                than 9 lines.
*C102190,1 AMH 01/28/2001 Remove the lines after grand total and move the whole form
*C102190,1                two lines down.
*B604307,1 HBG 03/20/2001 Print the "SHip TO" Name from the "Company name" instead
*B604307,1                of Warehouse name.
*B604652,1 KHM 07/10/2001 Printing the company name instead of the hard coded one
*B605651,1 MHM 05/03/2002 Fix bug of "Subscript out of bounds" 
*C200508,1 TMI 03/06/2003 Add the field "Final Destination" to the report
*C125092,1 BWA 11/18/2004 Add new field in the FRX which is the Status field.
*B126738,1 ASH 03/14/2005 Don't print status if the PO is Hold
*:***********************************************************************************
*:           

*-- Variables to hold the non major segments.
STORE 0 TO lnClrPo,lnColorLen,PAGENO,Row
STORE '' TO lcColor,XCLR_DESC,lcCLrDesc
lcTmpPoH = gfTempName()
*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))
*-- To get the non major segments.
=lfNonMaj()

*-- To get the color long name
DIMENSION laClrLName[1,2]
laClrLName[1,1] = 'CLRLNAME'      && Array to get the Color long name
laClrLName[1,2] = 'lcCLrDesc'

*-- To get the lables of the PO items.
DIMENSION laLables[10,2]
laLables[1,1]  = 'M_CISLBL1'
laLables[2,1]  = 'M_CISLBL2'
laLables[3,1]  = 'M_CISLBL3'
laLables[4,1]  = 'M_CISLBL4'
laLables[5,1]  = 'M_CISLBL5'
laLables[6,1]  = 'M_CITYPE1'
laLables[7,1]  = 'M_CITYPE2'
laLables[8,1]  = 'M_CITYPE3'
laLables[9,1]  = 'M_CITYPE4'
laLables[10,1] = 'M_CITYPE5'

=gfGetMemVar(@laLables,gcAct_Comp)
FOR lnArryCnt = 1 TO ALEN(laLables,1)
  lcArryCnt = ALLTRIM(STR(lnArryCnt))
  IF lnArryCnt <= 5
    lcISLbl&lcArryCnt = laLables[lnArryCnt,2]
  ENDIF  
  IF lnArryCnt >= 6
    lcIType&lcArryCnt = laLables[lnArryCnt,2]
  ENDIF  
ENDFOR

*-- To get the company address.
*B803599,1 ABD The form doesn't print the address correctly. It prints it hared code. [Begin]
*HLine1 = "G.M.A. ACCESSORIES INC."
*HLINE2 = laCompAdd[2]
*HLINE3 = laCompAdd[3]
*HLINE4 = laCompAdd[4]
*HLINE5 = laCompAdd[5]
IF EMPTY(laCompAdd[5])
 
 *B604652,1 KHM 07/10/2001 (Begin) Printing the company name instead of the
 *B604652,1 KHM 07/10/2001  hard coded one.
 *HLINE1 = "G.M.A. ACCESSORIES INC."
 HLINE1 = ALLTRIM(lcCompName)
 *B604652,1 KHM 07/10/2001 (End)
 
 HLINE2 = laCompAdd[1]
 HLINE3 = laCompAdd[2]
 HLINE4 = laCompAdd[3]
 HLINE5 = laCompAdd[4]
ELSE

 *B604652,1 KHM 07/10/2001 (Begin) Printing the company name instead of the
 *B604652,1 KHM 07/10/2001  hard coded one.
 *HLINE1 = "G.M.A. ACCESSORIES INC."
 HLINE1 = ALLTRIM(lcCompName)
 *B604652,1 KHM 07/10/2001 (End)
  
 HLINE2 = laCompAdd[1]
 HLINE3 = laCompAdd[2]
 HLINE4 = laCompAdd[3]
 HLINE5 = laCompAdd[5]
ENDIF
*B803599,1 ABD  [End]

COL1=(ROUND((80-LEN(HLINE1))/2,0))
COL2=(ROUND((80-LEN(HLINE2))/2,0))
COL3=(ROUND((80-LEN(HLINE3))/2,0))
COL4=(ROUND((80-LEN(HLINE4))/2,0))
COL5=(ROUND((80-LEN(HLINE5))/2,0))

*-- Copying the PosHdr's structure to a temporary file. 
SELECT PosHdr
COPY STRUCTURE TO (gcWorkDir+lcTmpPoH)
=gfOpenFile (gcWorkDir+lcTmpPoH,' ','EX')
INDEX ON Po TAG (lcTmpPoH)

*B803599,1 ABD Get the postion of the POSHDR file in the Option Grid. [Begin]
lnPoPos   = lfItmPos('POSHDR.PO')

*- change the filter and collect the data from the cursour if you browse 
*- in range and select POs that you select only to spped up the performance.
IF "POSHDR.PO" $ lcRpExp
  PRIVATE lcPoExp , lcBrowPos
  lcPoExp = ""

  *-- Copy all laOGVrFlt to another array to save the old value.
  DIMENSION laTempExpr[1] , laBrTmpFlt[1]

  *B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
  =ACOPY(laOGFxFlt,laTempExpr)
  *B605651,1 MHM  [End]
  
  =ACOPY(laBrFldFlt,laBrTmpFlt)

  *B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
  *DIMENSION laOGVrFlt[1,7] , laBrFldFlt[1,5]
  *STORE "" To laOGVrFlt , laBrFldFlt
  *=ACOPY(laTempExpr,laOGVrFlt,AELEMENT(laTempExpr,lnPoPos,1),7)
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  STORE "" To laOGFxFlt , laBrFldFlt
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnPoPos,1),7)
  *B605651,1 MHM  [End]
  
  *-- Convert numeric position to string
  lcBrowPos = ALLTRIM(STR(lnPoPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
  
    *B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
    *IF "laOGVrFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
    *B605651,1 MHM [End]

      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
      EXIT
    ENDIF
  ENDFOR
  
  *-- Generate InList expression for transaction Number.

  *B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
  *lcPoExp = gfGenFlt('laOGVrFlt',.T.,.T.)
  lcPoExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  *B605651,1 MHM [End]
  
  *- Replace the Inlist in Poshdr.po withe .T.
  lcRpExp = STRTRAN(lcRpExp,lcPoExp,".T.")
  
  *-- Restore original laOGVrFlt , laBrFldFlt

  *B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
  *DIMENSION laOGVrFlt[1] , laBrFldFlt[1]
  *=ACOPY(laTempExpr,laOGVrFlt)    && restore Original array
  DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
  =ACOPY(laTempExpr,laOGFxFlt)    && restore Original array
  *B605651,1 MHM [End]

  =ACOPY(laBrTmpFlt,laBrFldFlt)
ENDIF

*- If you select PO in the in browse you will get this file from the Option Grid.

*B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
*lcTPoFile = laOGVrFlt[lnPoPos,6]
lcTPoFile = laOGFxFlt[lnPoPos,6]
*B605651,1 MHM [End]

llWorkDeal = !EMPTY(lcTPoFile) AND USED(lcTPoFile) AND RECCOUNT(lcTPoFile) > 0


*-- Remove the set skip to To speed up the performance that he don't need it
*-- It need it in the FRX only not at at Say reports.
SELECT  POSHDR
SET SKIP TO

*-- Selecting the required Po according to the selection criteria
IF llWorkDeal
  SELECT (lcTPoFile)
  SET RELATION TO 'P'+PO INTO PoSHdr ADDITIVE
  GOTO TOP
  SCAN REST WHILE !EOF()
  lcNewPo = PO
    IF !SEEK(Po,lcTmpPoH)
      SELECT PoSHdr
        SCAN REST WHILE !EOF() FOR  &lcRpEXp .AND. PoSHdr.PO = lcNewPo
          SCATTER MEMVAR MEMO
          INSERT INTO (lcTmpPoH) FROM MEMVAR
      ENDSCAN  
     ENDIF
  ENDSCAN
  SELECT (lcTPoFile)
  SET RELATION TO 
ELSE
  *B803599,1 ABD [End]
  SELECT PoSHdr
  SCAN FOR &lcRpExp 
    IF !SEEK(Po,lcTmpPoH)
      SCATTER MEMVAR
      INSERT INTO (lcTmpPoH) FROM MEMVAR
    ENDIF
  ENDSCAN
  *B803599,1 ABD End For If Statement .[Begin]
ENDIF
*B803599,1 ABD [End]

SELECT (lcTmpPoH)
GOTO TOP
IF EOF()  
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  RETURN
ENDIF

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

*-- Printing the selected POs
SELECT (lcTmpPoH)
SCAN FOR Flag <> 'N' AND INKEY() <> 32
  XPO     = PO
  =SEEK('P'+XPo,'PosHdr')  

  *-- To get the requried Codes,Company address, and vendor address.
  =lfGetCodes()

  *-- This is the vendor address.
  XVENDOR = PosHdr.Vendor
  XVNAME  = lcVenName
  XVADDR1 = laVenAdr[1]
  XVADDR2 = laVenAdr[2]
  XVADDR3 = laVenAdr[3]
  XVADDR4 = laVenAdr[4]

  *-- This is the shipto address.
  *B604307,1 HBG 03/20/2001 Print the "SHip TO" Name from the "Company name" instead
  *B604307,1                of Warehouse name. [Begin]
  *XSTNAME  = lcShpname
  XSTNAME = gcCom_Name
  *B604307,1 [End]
  
  XSTADDR1 = lashpadr[1]
  XSTADDR2 = lashpadr[2]
  XSTADDR3 = SUBSTR(ALLTRIM(lashpadr[3]),1,15)+' '+;
             SUBSTR(ALLTRIM(lashpadr[4]),1,3)+' '+;
             SUBSTR(ALLTRIM(lashpadr[5]),1,10)
  =SEEK(PosHdr.cwarecode,'WAREHOUS')
  XSTADDR4 = ALLTRIM(WareHous.cAddress6)

  *-- The term code.
  XTERM_DATA = lcTerms

  *-- The ShipVia
  XSHIP_DATA = lcShipVia

  *-- The Division Code
  XDIVI_DATA = lcDivDesc
  
  *B604652,1 KHM 07/10/2001 (Begin) Printing the company name instead of the
  *B604652,1 KHM 07/10/2001  hard coded one.
  *HLine1 = "G.M.A. ACCESSORIES INC."
  HLine1 = ALLTRIM(lcCompName)
  *B604652,1 KHM 07/10/2001 (End)
  
  *-- Foreign Currency
  xCurr = IIF(llRpFc,cPriceCur,gcBaseCurr)

  *-- Estimated Cost.
  lnICost2 = NICOST2
  lnICost3 = NICOST3
  lnICost4 = NICOST4
  lnICost5 = NICOST5

  *-- If print price in foreign currency.
  IF llRpFc
    lnICost2 = IIF(lcIType7  = 'D',lnICost2*nDutyRat,lnICost2)
    lnICost3 = IIF(lcIType8  = 'D',lnICost3*nDutyRat,lnICost3)
    lnICost4 = IIF(lcIType9  = 'D',lnICost4*nDutyRat,lnICost4)
    lnICost5 = IIF(lcIType10 = 'D',lnICost5*nDutyRat,lnICost5)
  ENDIF
  STORE 0 TO XTOTQTY,XTOTORD,XGTOTORD,XGTOTQTY
  lcOldScale = ' '

  *-- To print the header of the form.
  =lfPrnHdr()

  IF SEEK('P'+XPo,'PosLn')
    SELECT PosLn
    DO WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'P'+xPo              

      *B803870,1 KHM 11/29/2000 (Begin) Re-print the header after printing 
      *B803870,1                the continue word.
      *=IIF(Row >= 55,lfContinue(),.F.)
      IF Row >= 55
        =lfContinue()
        =lfPrnHdr()
      ENDIF  
      *B803870,1 KHM 11/29/2000 (End)

      lcStyle = SUBSTR(Style,1,lnMajLen)        
      =SEEK(Style,'Style')
      XSTYDESC = Style.Desc1
      @ ROW,02 SAY lcStyle
      *B803685,1 ABD The form doesn't print the whole description for the style [Begin]
      *@ ROW,19 SAY SUBSTR(XSTYDESC,1,20)
       @ ROW,19 SAY XSTYDESC
      *B803685,1 ABD [END]
      ROW=ROW+1

      *B803870,1 KHM 11/29/2000 (Begin) Re-print the header after printing 
      *B803870,1                the continue word.
      *=IIF(Row >= 55,lfContinue(),.F.)
      IF Row >= 55
        =lfContinue()
        =lfPrnHdr()
      ENDIF  
      *B803870,1 KHM 11/29/2000 (End)
      
      *-- If print style's notepad
      IF llrpPrtSn
        =lfPrnNotPd('F',lcStyle)
      ENDIF

      *-- To print the Po lines.
      =lfPrnDetal()

      *-- To print the subtotal of each style.
      =lfPrSubTot()
    ENDDO
    *-- If print Po's notepad
    IF llrpPrtPn
      =lfPrnNotPd('P',xPo)
    ENDIF      
  ENDIF
  *-- To print the grand total for each po  
  =lfPrGrdTot()
ENDSCAN
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To print the PO header
*!*************************************************************
*! Example     : = lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO +1
*C102190,1 AMH move the whole form two lines down [Start]
*@ 01,04 SAY 'PO # : '+xPO
*@ 01,COL1 SAY HLINE1
*@ 01,60 SAY 'DATE  :'
*@ 01,68 SAY PosHdr.ENTERED
*@ 02,COL2 SAY HLINE2
*@ 02,60 SAY 'PAGE #: '
*@ 02,68 SAY PAGENO PICTURE '999'
*@ 03,COL3 SAY HLINE3
*@ 04,COL4 SAY HLINE4
*@ 05,COL5 SAY HLINE5
*@ 07,26 SAY 'P U R C H A S E     O R D E R'
*@ 09,04 SAY '........ VENDOR .........'
*@ 09,47 SAY '........ SHIP TO ........'
*@ 10,04 SAY XVNAME
*@ 10,47 SAY XSTNAME
*@ 11,04 SAY XVADDR1
*@ 11,47 SAY XSTADDR1
*@ 12,04 SAY XVADDR2
*@ 12,47 SAY XSTADDR2
*@ 13,04 SAY XVADDR3
*@ 13,47 SAY XSTADDR3
*@ 14,04 SAY XVADDR4
*@ 14,47 SAY XSTADDR4
*@ 15,04 SAY 'ATTN: '+PosHdr.CONTACT
*@ 17,04 SAY 'DIVISION     :'+SUBSTR(XDIVI_DATA,1,15)
*@ 17,47 SAY 'SHIP VIA   :'+SUBSTR(XSHIP_DATA,1,15)
*@ 18,04 SAY 'COMPLETE     :'
*@ 18,18 SAY PosHdr.COMPLETE
*@ 18,47 SAY 'QUOTA CATG.:'+PosHdr.QUOTACAT
*@ 19,04 SAY 'TERMS        :'+SUBSTR(XTERM_DATA,1,15)
*@ 19,47 SAY 'ORIGIN     :'+PosHdr.ORIGIN
*@ 20,04 SAY 'F.O.B.       :'+PosHdr.cFOB
*@ 20,47 SAY 'INSURANCE  :'+PosHdr.INSURANCE
*@ 21,04 SAY 'LETTER CREDIT:'+PosHdr.LC
*@ 22,00 SAY REPLICATE('*',80)
*@ 23,00 SAY '  STYLE           DESCRIPTION'

*B803685,1 ABD The amount and the total amountt must not print if print price None. [BEGIN]
*IF lcRpPrice = 'N'
*  @ 24,00 SAY '  COLOR   COLOR DESCRIPTION             TOT QTY  '
*ELSE
*  @ 24,00 SAY '  COLOR   COLOR DESCRIPTION             TOT QTY  CURR      PRICE         AMOUNT'
*ENDIF
*B803685,1 ABD [End]

*@ 25,00 SAY REPLICATE('*',80)
*ROW      = 26
@ 03,04 SAY 'PO # : '+xPO
@ 03,COL1 SAY HLINE1
@ 03,60 SAY 'DATE  :'
@ 03,68 SAY PosHdr.ENTERED

*C125092,1 BWA 11/18/2004 Add new field in the FRX which is the Status field.[START]
*B126738,1 ASH 03/14/2005 (Begin) Don't print status if the PO is Hold
*@ 04,04 SAY 'Status : ' + IIF(Poshdr.Status = "O" , "Open" , IIF(Poshdr.Status = "H" , "Hold" , IIF(Poshdr.Status = "C" , "Complete" , IIF(Poshdr.Status = "S" , "Closed" , IIF(Poshdr.Status = "X" , "Cancelled" , "Bid")))))
IF Poshdr.Status <> 'H'
  @ 04,04 SAY 'Status : ' + IIF(Poshdr.Status = "O" , "Open" , IIF(Poshdr.Status = "H" , "" , IIF(Poshdr.Status = "C" , "Complete" , IIF(Poshdr.Status = "S" , "Closed" , IIF(Poshdr.Status = "X" , "Cancelled" , "Bid")))))
ENDIF
*B126738,1 ASH 03/14/2005 (End)
*C125092,1 BWA 11/18/2004.[END]

@ 04,COL2 SAY HLINE2
@ 04,60 SAY 'PAGE #: '
@ 04,68 SAY PAGENO PICTURE '999'
@ 05,COL3 SAY HLINE3
@ 06,COL4 SAY HLINE4
@ 07,COL5 SAY HLINE5
@ 09,26 SAY 'P U R C H A S E     O R D E R'
@ 11,04 SAY '........ VENDOR .........'
@ 11,47 SAY '........ SHIP TO ........'
@ 12,04 SAY XVNAME
@ 12,47 SAY XSTNAME
@ 13,04 SAY XVADDR1
@ 13,47 SAY XSTADDR1
@ 14,04 SAY XVADDR2
@ 14,47 SAY XSTADDR2
@ 15,04 SAY XVADDR3
@ 15,47 SAY XSTADDR3
@ 16,04 SAY XVADDR4
@ 16,47 SAY XSTADDR4
@ 17,04 SAY 'ATTN: '+PosHdr.CONTACT
@ 19,04 SAY 'DIVISION     :'+SUBSTR(XDIVI_DATA,1,15)
@ 19,47 SAY 'SHIP VIA   :'+SUBSTR(XSHIP_DATA,1,15)
@ 20,04 SAY 'COMPLETE     :'
@ 20,18 SAY PosHdr.COMPLETE
@ 20,47 SAY 'QUOTA CATG.:'+PosHdr.QUOTACAT
@ 21,04 SAY 'TERMS        :'+SUBSTR(XTERM_DATA,1,15)
@ 21,47 SAY 'ORIGIN     :'+PosHdr.ORIGIN
@ 22,04 SAY 'F.O.B.       :'+PosHdr.cFOB
*C200508,1 TMI [Start] Add the field final destination to the report layout under the Origin field
*@ 22,47 SAY 'INSURANCE  :'+PosHdr.INSURANCE
@ 22,47 SAY 'FINAL DEST.:'+PADR(gfCodDes(PosHdr.CFNLDST,'CFNLDST'),20)
*C200508,1 TMI [End  ] 
@ 23,04 SAY 'LETTER CREDIT:'+PosHdr.LC
*C200508,1 TMI [Start] 
@ 23,47 SAY 'INSURANCE  :'+PosHdr.INSURANCE
*C200508,1 TMI [End  ] 
@ 24,00 SAY REPLICATE('*',80)
@ 25,00 SAY '  STYLE           DESCRIPTION'

IF lcRpPrice = 'N'
  @ 26,00 SAY '  COLOR   COLOR DESCRIPTION             TOT QTY  '
ELSE
  @ 26,00 SAY '  COLOR   COLOR DESCRIPTION             TOT QTY  CURR      PRICE         AMOUNT'
ENDIF

@ 27,00 SAY REPLICATE('*',80)
ROW      = 28
*C102190,1 AMH [End]

*!*************************************************************
*! Name      : lfPrnDetal
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To print the detail of each po
*!*************************************************************
*! Example     : = lfPrnDetal()
*!*************************************************************
FUNCTION lfPrnDetal

SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd='P'+xPo+lcStyle;
          FOR TranCd = '1' AND TotQty > 0              

  *B803870,1 KHM 11/29/2000 (Begin) Re-print the header after printing 
  *B803870,1                the continue word.
  *=IIF(Row >= 55,lfContinue(),.F.)
  IF Row >= 55
    =lfContinue()
    =lfPrnHdr()
  ENDIF  
  *B803870,1 KHM 11/29/2000 (End)
  
  lcColor   = SUBSTR(Style,lnClrPo,lnColorLen)
  XCLR_DESC = gfCodDes(lcColor,'COLOR     ')
  =gfRltFld(lcColor , @laClrLName ,'COLOR     ')
  XCVENSTY = SUBSTR(CVENSTY,1,lnMajLen)
  IF SCALE <> lcOldScale
    lcOldScale = SCALE
    IF SEEK('S'+lcOldScale,'Scale')        
      lnSclCol = 1
      FOR lnScaleCnt = 1 TO Scale.Cnt
        lcScaleCnt = STR(lnScaleCnt,1)
        lcSize = Scale.Sz&lcScaleCnt
        lcSize = lcSize+SPACE(1)
        @ ROW,(lnSclCol*6)+19 SAY lcSize
        lnSclCol = lnSclCol + 1
      ENDFOR
      ROW = ROW + 1

      *B803870,1 KHM 11/29/2000 (Begin) Re-print the header after printing 
      *B803870,1                the continue word.
      *=IIF(Row >= 55,lfContinue(),.F.)
      IF Row >= 55
        =lfContinue()
        =lfPrnHdr()
      ENDIF  
      *B803870,1 KHM 11/29/2000 (End)
      
    ENDIF
  ENDIF	   
  @ ROW,02 SAY lcColor
  @ ROW,10 SAY SUBSTR(XCLR_DESC,1,15)
  FOR lnQtyCnt = 1 TO 8
    lcQtyCnt = STR(lnQtyCnt,1)
    *B803685,1 ABD Remark next line and identify the layout. [Begin]   
    *@ ROW,(lnQtyCnt*6)+20 SAY QTY&lcQtyCnt  PICTURE '@Z 99999'
     @ ROW,(lnQtyCnt*6)+19 SAY QTY&lcQtyCnt  PICTURE '@Z 99999'    
   *B803685,1 ABD [End]
  ENDFOR
  ROW = ROW+1

  *B803870,1 KHM 11/29/2000 (Begin) Re-print the header after printing 
  *B803870,1                the continue word.
  *=IIF(Row >= 55,lfContinue(),.F.)
  IF Row >= 55
    =lfContinue()
    =lfPrnHdr()
  ENDIF  
  *B803870,1 KHM 11/29/2000 (End)
  
  IF !EMPTY(lcCLrDesc)
    @ ROW,02 SAY SUBSTR(lcCLrDesc,1,30)
  ENDIF
  @ ROW,41 SAY TOTQTY PICTURE '9999999'

  *B803685,1 ABD The amount and the total amountt must not print if print price None
  *B803685,1 ABD And fix bug that print uncorrect price and amount [Begin] 
  *@ ROW,55 SAY IIF(llRpFc,nfCost1,nCost1) PICTURE '999999.99'
  *XAMOUNT = IIF(llRpFc,TotQty*nfCost1,TotQty*nCost1)
  lnPriValue = 0
  IF lcRpPrice # 'N'
    IF lcRpPrice = 'C'
      lnPriValue = IIF(llRpFc,Posln.ncost1,Posln.NeCost1)
    ELSE
      lnPriValue = ROUND(Style.nSugRetPri,2)
    ENDIF
    @ ROW,50 SAY XCURR    
    @ ROW,55 SAY lnPriValue        PICTURE '999999.99'
    XAMOUNT = lnPriValue*TotQty
    @ ROW,66 SAY XAMOUNT PICTURE '9999999999.99'    
	  ENDIF
  *B803685,1 ABD [End]

  XTOTQTY  = XTOTQTY+ TOTQTY
 
  *B803685,1 ABD The amount and the total amountt must not print if print price None. [Begin]
  IF lcRpPrice # 'N'
    XTOTORD  = XTOTORD+ XAMOUNT
  ENDIF  
  *B803685,1 ABD [End]
  
  IF !EMPTY(XCVENSTY)
    ROW = ROW+1
    
    *B803870,1 KHM 11/29/2000 (Begin) Re-print the header after printing 
    *B803870,1                the continue word.
    *=IIF(Row >= 55,lfContinue(),.F.)
    IF Row >= 55
      =lfContinue()
      =lfPrnHdr()
    ENDIF  
    *B803870,1 KHM 11/29/2000 (End)
    
	@ ROW,02 SAY "VENDOR STYLE :"
    @ ROW,18 SAY XCVENSTY
  ENDIF
  ROW = ROW+1
ENDSCAN

*!*************************************************************
*! Name      : lfPrSubTot
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To print the sub total for each style.
*!*************************************************************
*! Example     : = lfPrSubTot()
*!*************************************************************
FUNCTION lfPrSubTot

@ ROW,10 SAY '*** SUB TOTAL ***'
*B803685,1 ABD Remark next line and identify the layout. [Begin]   
*@ ROW,40 SAY XTOTQTY PICTURE '9999999'
@ ROW,41 SAY XTOTQTY PICTURE '9999999'
*B803685,1 ABD [END]

*B803685,1 ABD The amount and the total amountt must not print if print price None. [Begin]
*@ ROW,66 SAY XTOTORD PICTURE '9999999999.99'
@ ROW,66 SAY IIF(XTOTORD = 0 ,'',XTOTORD) PICTURE '9999999999.99'
*B803685,1 ABD [End]

ROW=ROW+2
XGTOTQTY=XGTOTQTY+XTOTQTY
XGTOTORD=XGTOTORD+XTOTORD
STORE 0 TO XTOTQTY,XTOTORD

*!*************************************************************
*! Name      : lfPrGrdTot
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To print the grand total for each PO.
*!*************************************************************
*! Example     : = lfPrGrdTot()
*!*************************************************************
FUNCTION lfPrGrdTot

@ 55,00 SAY REPLICATE('-',80)
@ 56,10 SAY 'TOTAL  =>'
@ 56,40 SAY XGTOTQTY  PICTURE '9999999'
@ 56,49 SAY 'PIECES'

*B803685,1 ABD The amount and the total amountt must not print if print price None. [Begin]
*@ 56,66 SAY XGTOTORD  PICTURE '9999999999.99'
@ 56,66 SAY IIF(XGTOTORD = 0 ,'',XGTOTORD)  PICTURE '9999999999.99'
*B803685,1 ABD [End]

ROW = 57
*C102190,1 AMH Remove the lines after grand total [Start]
*IF lnICost2  <> 0
*  @ ROW,10 SAY 'ESTIMATED '+ lcISLbl2
*  @ ROW,69 SAY lnICost2 PICTURE '9999999.999'
*  ROW=ROW+1
*ENDIF

*IF lnICost3  <> 0
*  @ ROW,10 SAY 'ESTIMATED ' +  lcISLbl3
*  @ ROW,69 SAY lnICost3 PICTURE '9999999.999'
*  ROW=ROW+1
*ENDIF

*IF lnICost4  <> 0
*  @ ROW,10 SAY 'ESTIMATED '+ lcISLbl4
*  @ ROW,69 SAY lnICost4 PICTURE '9999999.999'
*  ROW=ROW+1
*ENDIF

*IF lnICost5  <> 0
*  @ ROW,10 SAY 'ESTIMATED '+ lcISLbl5
*  @ ROW,69 SAY lnICost5 PICTURE '9999999.999'
*  ROW=ROW+1
*ENDIF
XGAMT=XGTOTORD+lnICost2+lnICost3+lnICost4+lnICost5

*B803685,1 ABD The amount and the total amountt must not print if print price None. [Begin]
*@ ROW,66 SAY XGAMT PICTURE '9999999999.999'
*IF lcRpPrice # 'N'
*  @ ROW,10 SAY 'TOTAL PURCHASE ORDER AMOUNT'
*  @ ROW,50 SAY XCURR
*  @ ROW,66 SAY IIF(XGAMT = 0 ,'',XGAMT) PICTURE '9999999999.999'
*ENDIF
*B803685,1 ABD [End]
*C102190,1 AMH [End]

ROW = ROW + 1

*!*************************************************************
*! Name      : lfContinue
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To page footer.
*!*************************************************************
*! Example     : = lfContinue()
*!*************************************************************
FUNCTION lfContinue

@ 56,00 SAY REPLICATE('-',80)
@ 57,13 SAY 'C O N T I N U E D ...'   
@ 57,69    SAY '**********'
@ 58,00 SAY REPLICATE('-',80)

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To print the PO & style notepad
*!*************************************************************
*! Example     : = lfPrnNotPd()
*!*************************************************************
FUNCTION lfPrnNotPd
PARAMETERS lcTranKey,lcType
PRIVATE lnAlias

lnAlias = SELECT()
IF SEEK(lcTranKey+lcType,'NotePad')
  SELECT NOTEPAD
  
  lnNotLine = 1
  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnNotLine <= lnMemLins
    IF ROW >= 55
      =lfContinue()
      =lfPrnHdr()
    ENDIF      
    IF  '*' <> LEFT(MLINE(MNOTES,lnNotLine),1)  
       @ ROW,04 SAY MLINE(MNOTES,lnNotLine)
       ROW    = ROW+1
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO
  ROW    = ROW+1
  SET MEMOWIDTH TO lnOldMemW
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnNonMajPo = 0
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      llStopConc = .T.
    ENDIF
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/22/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

*B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
*lnItmPos = ASCAN(laOGVrFlt,lcItmInFlt)
lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
*B605651,1 MHM [End]

IF lnItmPos > 0

  *B605651,1 MHM 05/03/2002 change variable filter to fixed one [Start]
  *lnItmPos = ASUBSCRIPT(laOGVrFlt,lnItmPos,1)
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
  *B605651,1 MHM [End]

ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*!*************************************************************
