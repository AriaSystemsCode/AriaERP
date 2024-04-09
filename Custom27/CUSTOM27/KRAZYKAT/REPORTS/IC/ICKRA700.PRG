*:**************************************************************************
*: Program file  : ICKRA700
*: Program desc. : Open To Sell Report for (Krazy Kat)
*: Date          : 09/23/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh Saiid (SSE)
*: Reference     : C101640
*:**************************************************************************
*: Calls :  
*:       Procedures : lpCollData, lpPrintHdr, lpPrnNotPd, lpPrintRep,
*:                  : lpPrintDet, lpRptHdr
*:
*:       Functions  : lfwRepWhen(), lfCollTime(), lfvRpSelct(), lfItmPos(),
*:                  : lfCreatFil(), lfNonMaj(), lfSRVSty(), lfStySum(),
*:                  : lfvStyle(), lfvFabric(), lfRpSetup(), lfvSortBy(),
*:                  : lfGetReplc(), lfFillAray(), lfvDateRng(), lfvpbOk(),
*:                  : lfvDir(), lfVAccpOk()
*:          Globals : gfModalGen(), gfOpenFile(), gfItemMask(), gfStyBrw(),
*:                  : FaBrow()
*:**************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR Krazy Kat (101640) 
*:**************************************************************************
*

*-- If ItPoShip file found in lcMCompDir [Begin]
IF FILE(lcMCompDir+'ITPoShip.DBF') AND FILE(lcMCompDir+'ITSetup.DBF')
  *--- Open  used  Files.
  =gfOpenFile(lcMCompDir+"ITPOSHIP",lcMCompDir+"Style","SH")
  =gfOpenFile(lcMCompDir+"ITSETUP","","SH")
  
ELSE   && this file is not found
  *-- Message : M42197 => 'Cannot open the file ITPOSHIP'
  *-- Button  : B00000 => '             <Ok>            '
  = gfModalGen('TRM42203B00000','DIALOG','ITPOSHIP or ITSETUP') 
  RETURN
ENDIF
*-- EndIf of ItPoShip file found in lcMCompDir [Begin]

lcStTime = TIME()       && To store the current time
lcRpExp  = STRTRAN(lcRpExp,"STYLE.","")

IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcTempFile) AND RECCOUNT(lcTempFile) > 0
    = lfCreatFil()
  ENDIF

  *-- make the following relations [Begin]
  SELECT OrdLine
  SET RELATION TO cOrdType + Order INTO OrdHdr ADDITIVE
  *-- OrdLine
  *--       |___[cOrdType + Order]
  *--                            OrdHdr 
  SELECT ItPoShip
  SET RELATION TO 'P'+Po INTO Poshdr ADDITIVE
  SET FILTER TO IIF(EMPTY(lcDateRang),.T., &lcDateRang)
  *-- ItPoShip
  *--        |___
  *--            ['P'+Po]
  *--                    PosHdr
  *-- make the following relations [End]

  *-- forming the OTS expression [Begin]
  lcOTSExpr = ''
  IF lcRpSelect <> 'A'
    DO CASE
      CASE lcRpSelect = 'P'
        lcOTSExpr = [(TotStk + TotWip - TotOrd)>=IIF(lnMinQty>0,lnMinQty,1)]
      CASE lcRpSelect = 'N'
        lcOTSExpr = [(TotStk + TotWip - TotOrd)<=IIF(lnMinQty*(-1)<0,lnMinQty,-1)]
      CASE lcRpSelect = 'B'
        lcOTSExpr = [(TotStk + TotWip - TotOrd)<>0]
    ENDCASE
  ENDIF
  
  IF !EMPTY(lcOTSExpr)
    IF !EMPTY(lcRpExp)
      lcRpExp = lcRpExp + [ AND ]
    ENDIF
    lcRpExp = lcRpExp + lcOTSExpr
  ENDIF
  *-- forming the OTS expression [End]

  *-- make the Style File match the criteria selected
  SELECT Style
  SET FILTER TO &lcRpExp
  GO TOP
    
  *-- Collect data for report 
  DO lpCollData

  *-- break the relations which is used during collecting data
  SELECT OrdLine
  SET RELATION TO 

  SELECT ItPoShip
  SET RELATION TO
ENDIF

SELECT (lcTempFile)
GO TOP
*-- If no records in temp file (empty)
IF RECCOUNT(lcTempFile) = 0
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN

ELSE  

  *-- Reindexing file if user change sort by [Begin.]
  GO TOP
  lcFirstRec = EVALUATE(lcSortKey)
  IF !SEEK(lcFirstRec)
    *-- Replace key field with new values.
    REPLACE ALL cTempKey WITH EVALUATE(lcSortKey)
  ENDIF
  *-- Reindexing file if user change sort by [End.]

  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT
  lnStyleNo = 1      && Variable to count the total styles number
  DO lpPrintRep      && print the report 
  *-- Calculate spent time in collecting data.
  lcEdTime = TIME()  && Time in which we finish collect data.
  lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(lnStyleNo)) + ' ' + lcMajTtl + ; 
              ' in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
  DO ENDREPORT       
  SET DEVICE TO SCREEN
ENDIF
*-- EndIf of no records in temp file (empty)
****************************************************************************
*************************** *-- End of Report--* ***************************
****************************************************************************

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Collect data for the temporary file
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
*-- Notes : we have to store the style in a variable named lcStyle because  
*-- we don't know cLinkType field in ItSetup carries ARIA26 or ARIA27 link
*-- so if link is ARIA26 we use both Style/Color fields in ITPOSHIP file 
*-- otherwise link is ARIA27 we use Style field only

*-- Scan Loop for Master Style file 
SCAN WHILE INKEY() <> 32
  WAIT WINDOW 'Selecting ' + lcMajTtl + ' ' + Style + ;
              ' ...<Space Bar> to abort' NOWAIT                
  lcIndexExp = EVALUATE(lcSortKey)
  lcStyle    = IIF(ItSetup.cLinkType='ARIA26',PADR(SUBSTR(Style.Style,1,lnMajLen),19)+PADR(SUBSTR(Style.Style,lnClrPo,lnColorLen),6),Style.Style)
  lcColor    = SUBSTR(Style,lnClrPo,lnColorLen)
  lnTotQty   = 0
  
  *-- Collect total Open Order(s) quantities for this style if found [Begin]
  IF SEEK(Style,'OrdLine')  
    SELECT OrdLine  
    SUM TotQty TO lnTotQty REST WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) =;
                                      Style.Style FOR OrdHdr.Status = "O"
  ENDIF
  *-- Collect total Order quantity for this style if found [End]
  
  *-- add data into temporary file [Begin]
  INSERT INTO (lcTempFile) (Style, nPriceA, nOpnToSell, mNotePad, cScale,;
               cPrePak, cStyGroup, Pattern, nTotalOrd, cTempKey) VALUES ;
              (Style.Style, Style.PriceA, Style.TotStk+Style.TotWip-Style.TotOrd,;
               IIF(SEEK ('F'+Style.Style,'NotePad'),NotePad.mNotes, ''),;
               Style.Scale, Style.PrePak, Style.cStyGroup, Style.Pattern,;
               lnTotQty, lcIndexExp)
  *-- add data into temporary file [End]

  *-- Complete filling temporary file with ItPoship file data [Begin]
  IF SEEK(lcStyle,'ItPoShip')
    SELECT (lcTempFile)
    *-- Adding the Quota Category 
    REPLACE cCatalog WITH PoSHdr.QuotaCat
    SELECT ItPoShip
  
    *-- Scan Loop for ItPoShip
    SCAN REST WHILE Style + Color = lcStyle
      SELECT (lcTempFile)
      DO CASE
        *-- case purchase order date less than or equal system date
        CASE PoSHdr.Available <= gdSysDate
          *-- this means Po total quantity is now in the Stock
          REPLACE nInStock WITH nInStock + ItPoShip.TotQty

        *-- case purchase order date greater than system date
        CASE PoSHdr.Available > gdSysDate
          *-- if this po date is found in temp file 
          IF SEEK(PADR(lcIndexExp,35)+lcColor+DTOS(PoSHdr.Available))
            *-- add this Total quantity to the total order 
            REPLACE nTotalOrd WITH nTotalOrd + ItPoShip.TotQty
          ELSE   && Else not found
            *-- add this Total quantity as a new line 
            INSERT INTO (lcTempFile) (Style,cCatalog,dDate,nTotalOrd,cTempKey) ;
            VALUES (Style.Style,PoSHdr.QuotaCat,PoSHdr.Available,ItPoShip.TotQty,lcIndexExp)
          ENDIF
          *-- Endif this po date is found in temp file 
      ENDCASE
    ENDSCAN
    *-- EndScan Loop for ItPoShip
  ENDIF
  *-- Complete filling temporary file with ItPoship file data [End]
  
  *-- update Total order in temporary file [Begin]
  *-- object of this method is to remove accumulated Total Order from the preceeding lines
  *-- i.e. Calculate Batch quantities.
  *-- Order1 From start to Order1
  *-- Order2 From Order1 to Order2
  *-- ===========================
  *-- ===========================
  *-- ===========================
  *-- ===========================
  *-- Order(n) from Order(n-1) to Order(n)
  *--
  SELECT (lcTempFile)
  =SEEK(lcIndexExp)       && get first line for this style
  *-- Reduce Total order with the amount in the Stock
  REPLACE nTotalOrd WITH MAX(nTotalOrd - nInStock,0)
  *-- If still nTotalOrd greater than zero
  IF nTotalOrd > 0
    lnTotalOrd = nTotalOrd   && store total order qty in lnTotalOrd Var.
    SKIP                     && move to next record

    *-- Scan and reduce accumulated total order from lnTotalOrd var.
    SCAN REST WHILE cTempKey = lcIndexExp AND lnTotalOrd > 0

      *-- IF Total Order less than lnTotalOrd var.  (no need to continue)
      IF nTotalOrd < lnTotalOrd
        lnTotalOrd = lnTotalOrd - nTotalOrd
        REPLACE nTotalOrd WITH 0
      ELSE  && Else Total Order still greater than lnTotalOrd var. 
        REPLACE nTotalOrd  WITH MAX(nTotalOrd-lnTotalOrd,0)
        lnTotalOrd = 0
      ENDIF      
    ENDSCAN
  ENDIF
  *-- EndIf of nTotalOrd greater than zero
  *-- update Total order in temporary file [End]
  
ENDSCAN
*-- Scan Loop for Master Style file 
WAIT CLEAR
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Get the time for collecting data
*!**************************************************************************
*! Called from : Main Prg
*!**************************************************************************
*! Returns     : time For collecting data
*!**************************************************************************
* 
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

****************************************************************************
********************* *-- Report Printing Functions --* ********************
****************************************************************************

*!**************************************************************************
*! Name      : lpPrintRep
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Prints the report
*!**************************************************************************
*! Example   : DO lpPrintRep
*!**************************************************************************
*
PROCEDURE lpPrintRep
SELECT (lcTmpCatlg)
ZAP
SELECT (lcTempFile)
GOTO TOP
STORE 0 TO PAGENO , lnTotOTS , lnOpnToSel
ROW     = 99
R_WIDTH = 'W'
R_TITLE = 'Customized open to sell report'
lcText  = IIF(lcRpSortBy='S',lcMajTTl + ' Total ',IIF(lcRpSortBy = 'G',;
             'Total group of: ','Total pattern of: '))
STORE '' TO lcStyle,lcStyMajor

DO WHILE !EOF() .AND. INKEY() <> 32
  WAIT WINDOW 'Printing ' + lcMajTtl + ' ' + Style + ;
              ' ...<Space Bar> to abort' NOWAIT

  IF SUBSTR(Style,1,lnMajLen) <> lcStyMajor
    lnStyleNo = lnStyleNo + 1
  ENDIF
  lcStyMajor = SUBSTR(Style,1,lnMajLen)
              
  lcIndexExp = EVALUATE(lcSortKey)
  lcColor    = SUBSTR(Style,lnClrPo,lnColorLen)
  
  IF ROW > 60      && MAXROW = 60
    DO lpPrintHdr
  ENDIF
  
  lcStyle = Style
  @ ROW,0 SAY SUBSTR(Style,1,lnMajLen) + '->'  
  ROW = ROW + 1

  *-- Print the Scale sizes [Begin]
  =SEEK('S'+cScale,'Scale')
  @ ROW,00 SAY "Scale" + " [" + cScale + "]: "
  lnScaleCnt = Scale.Cnt
  lnCol = 13  
  FOR lnCounter = 1 TO lnScaleCnt
    lcCounter = STR(lnCounter,1)
    @ ROW,lnCol SAY PADL(ALLTRIM(Scale.Sz&lcCounter),5)
    lnCol = lnCol + 7
  ENDFOR
  ROW = ROW + 1
  *-- Print the Scale sizes [End]
  
  *-- Print the Prepak [Begin]
  IF SEEK('P'+PADR(cScale,3)+cPrePak,'Scale')    
    @ ROW,00 SAY "PrePak" + "[" + cPrepak + "]: "
    lnCol = 15
    FOR lnCounter = 1 TO lnScaleCnt
      lcCounter = STR(lnCounter,1)
      @ ROW,lnCol SAY Scale.PP&lcCounter PICTURE "999"
      lnCol = lnCol + 7
    ENDFOR
    ROW = ROW + 1
  ENDIF
  *-- Print the Prepak [End]
  
  *-- print NotePad
  DO lpPrnNotPd
  ROW = ROW + 1
  
  *-- Initialize grand total Open to Sell
  DO WHILE !EOF() .AND. cTempKey = PADR(lcIndexExp,35)
    lcColor  = SUBSTR(Style,lnClrPo,lnColorLen)
    lcOldKey = IIF(lcRpSortBy='S',Style,IIF(lcRpSortBy='G',cStyGroup,Pattern))
    IF ROW > 60      && MAXROW = 60
      DO lpPrintHdr
    ENDIF

    *-- Print the Color Code, PriceA, and the OTS numbers.
    @ ROW,14 SAY SUBSTR(Style,lnClrPo,lnColorLen)
    @ ROW,29 SAY nPriceA
    @ ROW,49 SAY nOpnToSell
    IF nInStock > 0
      @ ROW,72 SAY 'In Stock ' 
      @ ROW,82 SAY nInStock      
    ENDIF
    ROW = ROW + 1
    
    *-- Accumulating the OTS for each style
    lnOpnToSel = lnOpnToSel + nOpnToSell
    *-- Accumulating the OTS for each category
    
    SELECT (lcTmpCatlg)
    IF !SEEK(&lcTempFile..cCatalog)
      INSERT INTO (lcTmpCatlg) (cCatalog) VALUES (&lcTempFile..cCatalog)
    ENDIF
    REPLACE nOpnToSell WITH nOpnToSell + &lcTempFile..nOpnToSell

    SELECT (lcTempFile)
    SKIP
    SCAN REST WHILE cTempKey+SUBSTR(Style,lnClrPo,lnColorLen) = PADR(lcIndexExp,35)+lcColor
      IF ROW > 60      && MAXROW = 60
        DO lpPrintHdr
      ENDIF
      *-- Print details under condition that total order greater than 0.
      IF nTotalOrd > 0
        @ ROW,72 SAY dDate
        @ ROW,82 SAY nTotalOrd     
        ROW = ROW + 1
      ENDIF
    ENDSCAN
  ENDDO
  
  IF ROW > 60      && MAXROW = 60
    DO lpPrintHdr
  ENDIF
  *-- Print the Total for the Style
  lcNewKey = IIF(lcRpSortBy='S',Style,IIF(lcRpSortBy='G',cStyGroup,Pattern))
  IF lcRpSortBy = 'S' OR lcOldKey <> lcNewKey
    DO lpPrintDet WITH IIF(lnOpnToSel > 0, lnOpnToSel, 0) , "-"
    lnTotOTS   = lnTotOTS + lnOpnToSel
    lnOpnToSel = 0
  ENDIF
ENDDO

IF ROW > 60      && MAXROW = 60
  DO lpPrintHdr
ENDIF
DO lpPrintDet WITH IIF(lnTotOTS > 0, lnTotOTS, 0) , "*"

SELECT (lcTmpCatlg)
SCAN
  IF ROW > 60      && MAXROW = 60
    DO lpPrintHdr
  ENDIF
  @ ROW,00 SAY 'Total for Category :' + cCatalog
  @ ROW,47 SAY nOpnToSell PICTURE '9999999999'

  ROW = ROW + 1
ENDSCAN  
WAIT CLEAR
@ ROW,00 SAY REPLICATE('*',132)
*-- End of lpPrintRep.

*!**************************************************************************
*! Name      : lpPrintHdr
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : To print the second header of the report
*!**************************************************************************
*! Example   : DO lpPrintHdr
*!**************************************************************************
PROCEDURE lpPrintHdr
PAGENO = PAGENO + 1
DO lpRptHdr WITH 'ICKRA700',lcRpOpTitl,R_WIDTH
ROW = 7        
@ ROW,00 SAY PADR(ALLTRIM(lcMajTtl)+' #',13) + ' ' + ;
             PADR(ALLTRIM(lcNonMajT)+' Code',16) + ;
             ' Price         Open-To-Sell'
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('*',132)
ROW = ROW + 1
*-- End of lpPrintHdr.

*!**************************************************************************
*! Name      : lpPrnNotPd
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : To get the notepad of the style and print it
*!**************************************************************************
*! Example   : DO lpPrnNotPd
*!**************************************************************************
PROCEDURE lpPrnNotPd
PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine

lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 72

SELECT NotePad
IF SEEK ('F' + &lcTempFile..Style)
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnNotLine <= lnMemLins
    IF ROW > 60      && MAXROW = 60
      DO lpPrintHdr
    ENDIF
    @ ROW,15 SAY MLINE(MNOTES,lnNotLine)
    ROW = ROW + 1
    lnNotLine = lnNotLine + 1
  ENDDO     
ENDIF  
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)
*-- End of lpPrnNotPd.

*!**************************************************************************
*! Name      : lpRptHdr
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : To print the first report header
*!**************************************************************************
*! Example   : DO lpRptHdr
*!**************************************************************************
PROCEDURE lpRptHdr
*-- this function , I think needs to be replaced with RptHdr directly
PARAMETER XPROG,XRPTNAME,XTYPE

PRIVATE ALL LIKE X*
XRPTNAME = TRIM(XRPTNAME)
R_TITLE  = TRIM(R_TITLE)
X1 = ((130 - (LEN(TRIM(QCOMPANY))))/2)
X2 = ((130 - (LEN( R_TITLE  )))/2)
X3 = ((130 - (LEN( XRPTNAME )))/2)

@ 01,000 SAY XPROG
@ 01,X1  SAY QCOMPANY
@ 01,120 SAY DATE()
@ 01,129 SAY '~'   
@ 02,000 SAY TIME()
@ 02,X2  SAY R_TITLE 
@ 02,120 SAY 'PAGE#'
@ 02,126 SAY STR(PAGENO,4)
lcScdTit = 'AS OF ' + DTOC(Date())
lnCol1   = ((130 - (LEN(lcScdTit)))/2)
@ 03,lnCol1  SAY lcScdTit
lcTrdTit = 'Delivery dates between ' +DTOC(ldRpFrom)+ ' AND ' +DTOC(ldRpTo)
lnCol2   = ((130 - (LEN(lcTrdTit)))/2)
@ 04, lnCol2 SAY lcTrdTit
@ 05,X3  SAY XRPTNAME
@ 06,00 SAY REPLICATE('*',132)
*-- End of lpRptHdr.

*!**************************************************************************
*! Name      : lpPrintDet
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : To print the style subtotal and grandtotal
*!**************************************************************************
*! Example   : DO lpPrintDet
*!**************************************************************************
PROCEDURE lpPrintDet
PARAMETERS lnCol, lcType

*-- This to draw "-" for subtotals, and "*" for the grandtotals
@ ROW,00 SAY REPLICATE(lcType,132)
ROW = ROW + 1
@ ROW,00 SAY IIF(lcType="*",'Grand Total',lcText+IIF(lcRpSortBy='G',;
                 SUBSTR(&lcSortKey,1,2),STRTRAN(&lcSortKey,lcStyle)))

@ ROW,49 SAY lnCol PICTURE '99999999'
ROW = ROW + 1

IF !(lcType = "-" AND EOF())
  @ ROW,00 SAY REPLICATE(lcType,132)
  ROW = ROW + 1
ENDIF
*-- End of lpPrintDet.

****************************************************************************
*************************** *-- OG Functions --* ***************************
****************************************************************************

*!**************************************************************************
*! Name      : lfRpSetup
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : to call link company screen.
*!**************************************************************************
*! Example   : = lfRpSetup()
*!**************************************************************************
*
FUNCTION lfRpSetup
IF !FILE(gcDataDir+'KRASETUP.MEM')
  DIMENSION laComp[1,2]
  STORE '' TO laComp , lcMCompDir , lcDir
  DO (gcScrDir+'KRASETUP.SPX')  && Run The Linked Directry and Selected Company.
ENDIF

IF FILE(gcDataDir+'KRASETUP.MEM')
  RESTORE FROM (gcDataDir+'KRASETUP.MEM') ADDITIVE
ELSE
  llOgTrmnat = .T.
  CLEAR READ
ENDIF  
*-- End of lfRpSetup.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfItmPos() , lfCreatFil()
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnStatPos = lfItmPos('STYLE.STATUS')  && store the Style Status position
laOGFxFlt[lnStatPos,6] = 'A'          && make status target defaulted to "A"

*-- fill the lcDateRang variable with date range 
= lfDateRang()

*-- Call the function which create the main Temp file 
= lfCreatFil()
=lfvSortBy()
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
*! Called from : Main report prg
*!**************************************************************************
*! Returns   : Position
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfCreatFil
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Create temporary file structure.
*!**************************************************************************
*! Called from : OG When function. OR Main PRG
*!**************************************************************************
*! Example   : = lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
*-- If files are used we have to close it first [Begin]
IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF

IF USED(lcTmpCatlg)
  USE IN (lcTmpCatlg)
ENDIF
*-- If files are used we have to close it first [End]

*-- Create the Temp Files [Begin]
CREATE TABLE (gcWorkDir+lcTempFile); 
             (Style C(19),mNotePad M,nPriceA N(7,2),nInStock N(8),;
             nOpnToSell N(8),nTotalOrd N(8),dDate D,cCatalog C(4),cScale C(1),;
             cPrePak C(1),cStyGroup C(6),Pattern C(10),cTempKey C(35))
INDEX ON cTempKey+SUBSTR(Style,lnClrPo,lnColorLen)+DTOS(dDate) ;
      TAG (lcTempFile) OF (gcWorkDir+lcTempFile+'.CDX')

CREATE TABLE (gcWorkDir+lcTmpCatlg) (cCatalog C(4),nOpnToSell N(10))
INDEX ON cCatalog TAG (lcTmpCatlg) OF (gcWorkDir+lcTmpCatlg+'.CDX')
*-- Create the Temp Files [End]

*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfvSortBy
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : change index flag to reindex temp cursor.
*!**************************************************************************
*! Example   : =lfvSortBy()
*!**************************************************************************
FUNCTION lfvSortBy
DO CASE
  CASE lcRpSortBy = 'S'
    lcSortKey  = [SUBSTR(Style,1,lnMajLen)]
  CASE lcRpSortBy = 'G'
    lcSortKey = [cStyGroup+Pattern+SUBSTR(Style,1,lnMajLen)]
  CASE lcRpSortBy = 'P'
    lcSortKey = [Pattern+SUBSTR(Style,1,lnMajLen)]
ENDCASE
*-- End of lfvSortBy.
 
*!**************************************************************************
*! Name      : lfvRpSelct
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Validate Select (Positive/Negative/Both/All(Pos,Neg or Zero))
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfvRpSelct()
*!**************************************************************************
* 
FUNCTION lfvRpSelct
*-- to update the sign (+ve or -ve)
lcMinQtPic = IIF(lcRpSelect = 'N','-9999999999','9999999999') 
llOGFltCh = .T.
CLEAR READ
*-- End of lfvRpSelct.

*!**************************************************************************
*! Name      : lfFillAray
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Fill Sort By Array
*!**************************************************************************
*! Example   : = lfFillAray()
*!**************************************************************************
*
FUNCTION lfFillAray
DIMENSION laSortDesc[3,1] , laSortVal[3,1]
laSortDesc[1,1] = PROPER(lcMajTtl)
laSortDesc[2,1] = PROPER(lcMajTtl)+' Group'+' + Pattern + '+PROPER(lcMajTtl)
laSortDesc[3,1] = 'Pattern + ' + PROPER(lcMajTtl)

laSortVal[1,1]  = 'S'
laSortVal[2,1]  = 'G'
laSortVal[3,1]  = 'P'
*-- End of lfFillAray.

*!**************************************************************************
*! Name      : lfvDateRng
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Shows Date range screen for Po sold date range
*!**************************************************************************
*! Calls     : DateRng.spr
*!**************************************************************************
*! Example   : = lfvDateRng()
*!**************************************************************************
*
FUNCTION lfvDateRng
lcTitle = 'Date range'
DO (gcRepHome + gcAct_Appl+'\ICKRA700.SPX') && Run The Date Range screen 
*-- End of lfvDateRng.

*!**************************************************************************
*! Name      : lfvpbOk
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Validate date range screen's OK button
*!**************************************************************************
*! Example   : =lfvpbOk()
*!**************************************************************************
*
FUNCTION lfvpbOk
IF ldRpFrom > ldRpTo
  *-- Text : Date RANGE FROM must be less than or equal TO Date range to  
  =gfModalGen('TRM00272B00000','DIALOG','Date "'+"|"+'" must be less than or equal'+"|"+'Date " range to " ') 
  _CUROBJ = OBJNUM(ldRpFrom)
ELSE
  = lfDateRang()
  CLEAR READ
ENDIF
*-- End of lfvpbOk.

*!**************************************************************************
*! Name      : lfDateRang
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : fill the Date range variable
*!**************************************************************************
*! Example   : = lfDateRang()
*!**************************************************************************
*
FUNCTION lfDateRang
lcDateRang = ''
*-- Fill the lcDateRang variable according to the grid options.
DO CASE
  CASE  EMPTY(ldRpFrom) .AND. !EMPTY(ldRpTo)
    lcDateRang = "PoSHdr.Available <= ldRpTo"
  CASE !EMPTY(ldRpFrom) .AND. !EMPTY(ldRpTo)
    lcDateRang = "BETWEEN(PoSHdr.Available,ldRpFrom,ldRpTo)"
ENDCASE
*-- End of lfDateRang.

*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
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
*-- Compute Free/Color Items in Style Structure. [Begin]
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
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.

*!**************************************************************************
*! Name      : lfVDir
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Validate the Push Button Directory in ICKRASETUP Screen
*!**************************************************************************
*! Example   : = lfVDir()
*!**************************************************************************
*
FUNCTION lfvDir
lnCount = 0
lcDir = GETDIR()
lcDir = SUBSTR(lcDir,1,AT('\',lcDir,2))
SHOW GET lcDir
IF EMPTY(lcDir)
  RETURN
ENDIF
IF FILE(lcDir+'SYSFILES\SYCCOMP.DBF')
  USE IN (IIF(USED(lcAliasN),lcAliasN,0))
  *=gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',lcAliasN)
  =gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',@lcAliasN,.T.)
  SELECT (lcAliasN)
  COUNT TO lcCompNo FOR !EMPTY(CCOMP_id)
  DIMENSION laComp[lcCompNo,2]
  laComp = ''
  SCAN
    lnCount = lnCount + 1
    laComp[lnCount,1] = cComp_Id + '  ' + ALLTRIM(cCom_Name)
    laComp[lnCount,2] = ccom_ddir
  ENDSCAN  
  SHOW GET PbComp
ENDIF
IF lnCount > 0
  SHOW GET PbComp ENABLE
  SHOW GET PbOk   ENABLE
ENDIF
*-- End of lfvDir.

*!**************************************************************************
*! Name        : lfVAccpOk
*! Developer   : Sameh Saiid (SSE)
*! Date        : 09/23/1999
*! Purpose     : Validate push button accept in ICKRASETUP Screen
*!**************************************************************************
*! Example     : = lfVAccpOk()
*!**************************************************************************
*
FUNCTION lfVAccpOk
IF !EMPTY(lcDir)
  lcMcompdir =  laComp[PbComp,2]
  lcMcompdir = ALLTRIM(lcMcompdir)
  IF FILE(lcMcompdir+'ITPOSHIP.DBF') .AND. FILE(lcMcompdir+'ITHBILLT.DBF')
     CLEAR READ
     SAVE ALL LIKE lcMcompdir TO (gcDataDir+'KRASETUP.MEM')
  ELSE
    *-- TEXT : Company files can not be Found
    =gfModalGen('TRM00113B00000','DIALOG','found') 
    SET DEVICE TO SCREEN
    laComp = ''
    SHOW GET pbComp
    SHOW GET PbComp DISABLE
    SHOW GET PbOk   DISABLE
    RETURN
  ENDIF
ENDIF
*-- End of lfVAccpOk.

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : validate style
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfStyBrw()
*!**************************************************************************
*! Example     : = lfvStyle()
*!**************************************************************************
* 
FUNCTION lfvStyle
lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE
*--End of lfvStyle.

*!**************************************************************************
*! Name      : lfvFabric
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : validate fabric
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : FaBrow()
*!**************************************************************************
*! Example     : = lfvFabric()
*!**************************************************************************
* 
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Parameters  : lcParm
*!**************************************************************************
*! Example     : =lfsrvSty()
*!**************************************************************************
*! Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid (SSE)
*! Date      : 09/23/1999
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns     : Calculated field value.
*!**************************************************************************
*! Example     : = lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  

  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.
