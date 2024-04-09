*:************************************************************************
*: Program file  : ICMEZ600
*: Program desc. : CUT & SOLD REPORT CUSTOMIZED FOR MEZ.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 05/02/1999
*: Refer to      : C(101477)
*:*************************************************************************
*: C100914,1
*:*************************************************************************

*---Adjust the filter expression
*--Get the invoice date,cutting ticket entered date, and order completion date.
STORE {} TO ldFOrdDt,ldTOrdDt,ldFInvDt,ldTInvDt,ldFCutDt,ldTCutDt
*--Set century on
SET CENTURY ON
FOR lnDateNo = 1 TO 3
   lcDateNo = STR(lnDateNo,1)
   lcDateExp = IIF(lnDateNo = 1,'ORDLINE.COMPLETE',IIF(lnDateNo = 2,'INVLINE.INVDATE','CUTTKTH.ENTERED'))
   lnPos     = ASCAN(laOGFxFlt,lcDateExp)
   IF lnPos <> 0
     *--Get the row.
     lnDatePos = ASUBSCRIPT(laOGFxFlt,lnPos,1)
     *--Getb the separator pos.
     lnPPos    = AT("|",laOgFxflt[lnDatePos,6])
     DO CASE
       CASE lnDateNo = 1
         *-- Order completion date.
         ldFOrdDt = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,lnPPos-1))
         ldTOrdDt = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],lnPPos+1))
         *--Clear the order completion date from the expression as the filter will be set on style file.
         IF lcDateExp $ lcRpExp
           lnDateP = AT(lcDateExp,lcRpExp)
           *-- Clear the expression from "BETWEEN(.." TO the end of this date expession.
           lcRpExp = STRTRAN(lcRpExp,SUBSTR(lcRpExp,lnDateP-13,95),'.T.')
         ENDIF  
       CASE lnDateNo = 2
         *-- Invoice date.
         ldFInvDt = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,lnPPos-1))
         ldTInvDt = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],lnPPos+1))
         *--Clear the order completion date from the expression as the filter will be set on style file.
         IF lcDateExp $ lcRpExp
           lnDateP = AT(lcDateExp,lcRpExp)
           *-- Clear the expression from "BETWEEN(.." TO the end of this date expession.
           lcRpExp = STRTRAN(lcRpExp,SUBSTR(lcRpExp,lnDateP-13,95),'.T.')
         ENDIF  
       OTHERWISE
         *-- Cutting ticket entered date.
         ldFCutDt = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,lnPPos-1))
         ldTCutDt = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],lnPPos+1))
         *--Clear the order completion date from the expression as the filter will be set on style file.
         IF lcDateExp $ lcRpExp
           lnDateP = AT(lcDateExp,lcRpExp)
           *-- Clear the expression from "BETWEEN(.." TO the end of this date expession.
           lcRpExp = STRTRAN(lcRpExp,SUBSTR(lcRpExp,lnDateP-13,95),'.T.')
         ENDIF  
     ENDCASE     
   ENDIF
ENDFOR
*--Open SYCCOMP to get the company name.
=gfOpenFile(gcsyshome+'SYCCOMP',gcsyshome+'CCOMP_id','SH')
SEEK gcAct_Comp
lcCompName = cCom_Name             && Variable to hold the Company Name
*-- Add the make filter
IF lcRpMakBuy <> 'B'
  lcRpExp = lcRpExp +IIF(lcRpMakBuy = 'D','.AND. STYLE.MAKE','.AND. !STYLE.MAKE')
ENDIF
*-- Copy the record that match the selection criteria from the style file to a temp file
*-- and sort the temp. file by the season.
lcStyTmp = gfTempName()
SELECT STYLE
WAIT WINDOW 'Collecting data. Please wait.......' NOWAIT
COPY TO (gcWorkDir+lcStyTmp) FOR &lcRpExp
=gfOpenFile(gcWorkDir+lcStyTmp,'','EX')
INDEX ON Season +Style TAG (lcStyTmp)
*--If no records selected.
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*--Initilize the needed variable.
llMultiWH = (gfGetMemVar('M_WareHouse')  = 'Y')
*--Initializing the following variables in order to calculate the positive OTS qty and OTS amount. 
STORE 0 TO lnPosOTS,lnPosAmt
R_WIDTH = 'W'

IF gcDevice='PRINTER'
  *--LandScape ESC sequence.
  lcLandEsc=[&l1O&l14]
  @ PROW(),PCOL() SAY ' '
  @ PROW(),PCOL() SAY lcLandEsc 
  @ PROW(),PCOL() SAY ' '
ENDIF
R_TITLE = 'CUT & SOLD'
Row=1
lnPageNo=1
STORE 0 TO lnTSold,lnTSoldA,lnTCut,lnTCutA,lnTIss,lnTOnH,lnTShip,;
           lnTShipA,lnTToSh,lnTToSl,lnTToSlA
*-- Added to var. the first one to hold the season and the second one is a 
*-- flage that indicate if we'll print the season or not.
lcSeason = &lcStyTmp..Season
llPrnSeas = .T.
*---------------*
* Start printing
*---------------*
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*--Print the header information.
DO lpHeader
*--Changed to read from a temp. file
SELECT (lcStyTmp)
STORE 0 TO lnSubSldQ,lnSubSldA,lnSubCutQ,lnSubCutA,lnSubIssQ,lnSubOnHQ,lnSubShpQ,;
           lnSubShpA,lnSubToSh,lnSubToSl,lnSubToSlA
DO WHILE !EOF() .AND. INKEY() <> 32
  WAIT WINDOW 'PRINTING.... - <Space Bar> TO ABORT' NOWAIT           
  IF Season <> lcSeason 
    @ Row,01 SAY REPLICATE('-',126) 
    Row=Row+1
    @ Row,10 SAY lcSeason +' : SUBTOTAL'
    @ Row,27 SAY lnSubSldQ  PICTURE '99999'
    @ Row,43 SAY lnSubCutQ  PICTURE '999999'   
    @ Row,60 SAY lnSubIssQ  PICTURE '99999'    
    @ Row,66 SAY lnSubOnHQ  PICTURE '9999999'  
    @ Row,74 SAY lnSubShpQ  PICTURE '9999999'  
    @ Row,92 SAY lnSubToSh  PICTURE '9999999'  
    @ Row,100 SAY lnSubToSl PICTURE '9999999'   
    Row=Row+1
    @ Row,32 SAY lnSubSldA  PICTURE '9999999.99'
    @ Row,49 SAY lnSubCutA  PICTURE '9999999.99'
    @ Row,81 SAY lnSubShpA  PICTURE '9999999.99'
    @ Row,108 SAY lnSubToSlA PICTURE '9999999.99'
    Row=Row+1
    @ Row,01 SAY REPLICATE('-',126)
    STORE 0 TO lnSubSldQ,lnSubSldA,lnSubCutQ,lnSubCutA,lnSubIssQ,lnSubOnHQ,lnSubShpQ,;
               lnSubShpA,lnSubToSh,lnSubToSl,lnSubToSlA
    lcSeason = Season
    lnPageNo = lnPageNo+1
    DO lpHeader
  ENDIF 
  lcStyle = Style
  STORE 0 TO lnSldQty,lnSldAmt,lnCutQty,lnCutAmt,lnIssQty,lnOnHQty,lnShpQty,;
             lnShpAmt
  SELECT OrdLine
  IF SEEK(lcStyle)
    lcExp = IIF (!EMPTY(ldTOrdDt),'Complete>=ldFOrdDt AND Complete<=ldTOrdDt','1=1')
    SCAN WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle FOR (&lcExp AND !(OrdHdr.Status $ 'X'))
      lnSldQty=lnSldQty+TotQty+InvLine.TotQty
      lnSldAmt=lnSldAmt+((TotQty+InvLine.TotQty)*Price) 
    ENDSCAN
  ENDIF
  lnSubSldQ=lnSubSldQ+lnSldQty
  lnSubSldA= lnSubSldA+lnSldAmt  
  lnTSold=lnTSold+lnSldQty
  lnTSoldA=lnTSoldA+lnSldAmt
  SELECT CutTktL
  IF SEEK(lcStyle)
    lcExp=IIF (!EMPTY(ldTCutDt),'CutTktH.Entered>=ldFCutDt AND CutTktH.Entered<=ldTCutDt','1=1')
    SCAN WHILE style+cuttkt+trancd = lcStyle FOR(&lcExp AND CutTktH.Status $ 'OHAC')
      IF TRANCD='1'
        lnCutQty=lnCutQty+TotQty
      ENDIF
    ENDSCAN
    lnCutAmt=lnCutQty*&lcStyTmp..PriceA
  ENDIF
  lnSubCutQ = lnSubCutQ+lnCutQty
  lnSubCutA = lnSubCutA+lnCutAmt
  lnTCut    = lnTCut+lnCutQty
  lnTCutA   = lnTCutA+lnCutAmt
  SELECT CutTktL
  IF SEEK(lcStyle)
    lcExp=IIF (!EMPTY(ldTCutDt),'CutTktH.Entered>=ldFCutDt AND CutTktH.Entered<=ldTCutDt','1=1')
    SCAN WHILE style+cuttkt+trancd = lcStyle FOR(&lcExp AND CutTktH.Status $ 'OH')
      DO CASE
        CASE TRANCD='1'
          lnIssQty=lnIssQty+TotQty
        CASE TRANCD='2'
          lnIssQty=lnIssQty-TotQty
        CASE TRANCD='3'
          lnIssQty=lnIssQty-TotQty
        CASE TRANCD='4'
          lnIssQty=lnIssQty-TotQty
      ENDCASE
    ENDSCAN
    lnSubIssQ=lnSubIssQ+lnIssQty
  ENDIF
  lnTIss    = lnTIss+lnIssQty
  lnOnHQty  = &lcStyTmp..TotStk
  lnTOnH    = lnTOnH+lnOnHQty
  lnSubOnHQ = lnSubOnHQ+lnOnHQty
  SELECT InvLine
  SET ORDER TO InvLineS
  IF SEEK(lcStyle)
    lcExp=IIF (!(EMPTY(ldFInvDt) AND EMPTY(ldTInvDt)),'InvDate>=ldFInvDt AND InvDate<=ldTInvDt','1=1')
    SCAN WHILE style+invoice+STR(lineno,6) = lcStyle FOR &lcExp
      lnShpQty=lnShpQty+TotQty
      lnShpAmt=lnShpAmt+(TotQty*Price)
    ENDSCAN
  ENDIF
  lnSubShpQ  = lnSubShpQ+lnShpQty
  lnSubShpA  = lnSubShpA+lnShpAmt
  lnTShip    = lnTShip+lnShpQty
  lnTShipA   = lnTShipA+lnShpAmt
  SET ORDER TO InvLineO
  lnToShip   = lnSldQty-lnShpQty
  lnTToSh    = lnTToSh+lnToShip
  lnSubToSh  = lnSubToSh+lnToShip
  lnToSell   = lnOnHQty-lnToShip
  lnTToSl    = lnTToSl+lnToSell
  lnSubToSl  = lnSubToSl+lnToSell
  SELECT (lcStyTmp)
  @ Row,01 SAY Style
  @ Row,21 SAY SUBSTR(Desc,1,15)
  @ Row,37 SAY lnSldQty PICTURE '9999'
  @ Row,42 SAY lnSldAmt PICTURE '999999.99' 
  @ Row,52 SAY lnCutQty PICTURE '999999'    
  @ Row,59 SAY lnCutAmt PICTURE '999999.99' 
  @ Row,69 SAY lnIssQty PICTURE '99999'     
  @ Row,75 SAY lnOnHQty PICTURE '9999999'   
  @ Row,83 SAY lnShpQty PICTURE '9999999'   
  @ Row,91 SAY lnShpAmt PICTURE '999999.99' 
  @ Row,101 SAY lnToShip PICTURE '9999999'   
  @ Row,109 SAY lnToSell PICTURE '9999999'  
  @ Row,117 SAY lnToSell*PriceA PICTURE '9999999.99' 
  lnPosOTS = lnPosOTS + IIF(lnToSell > 0,lnToSell,0)
  lnPosAmt = lnPosAmt + IIF(lnToSell*PriceA > 0, lnToSell*PriceA, 0)
  lnTToSlA=lnTToSlA+(lnToSell*PriceA)
  lnSubToSlA=lnSubToSlA+(lnToSell*PriceA)
  SKIP
  IF SUBSTR(Style,1,lnMajorLen) = SUBSTR(lcStyle,1,lnMajorLen)
    Row=Row+1
  ELSE
    Row=Row+2
  ENDIF
  IF EOF()
    =lfPrntHdr()
    @ Row,01 SAY REPLICATE('-',126) 
    Row=Row+1
    =lfPrntHdr()
    @ Row,10 SAY lcSeason +' : SUBTOTAL'
    @ Row,36 SAY lnSubSldQ  PICTURE '99999'
    @ Row,52 SAY lnSubCutQ  PICTURE '999999'   
    @ Row,69 SAY lnSubIssQ  PICTURE '99999'    
    @ Row,75 SAY lnSubOnHQ  PICTURE '9999999'  
    @ Row,83 SAY lnSubShpQ  PICTURE '9999999'  
    @ Row,101 SAY lnSubToSh  PICTURE '9999999'  
    @ Row,109 SAY lnSubToSl PICTURE '9999999'   
    Row=Row+1
    =lfPrntHdr()
    @ Row,41 SAY lnSubSldA  PICTURE '9999999.99'
    @ Row,58 SAY lnSubCutA  PICTURE '9999999.99'
    @ Row,90 SAY lnSubShpA  PICTURE '9999999.99'
    @ Row,116 SAY lnSubToSlA PICTURE '99999999.99'
    Row=Row+1
    =lfPrntHdr()
    @ Row,01 SAY REPLICATE('-',126)
    Row=Row+1
    =lfPrntHdr()
    @ Row,10 SAY 'TOTAL'
    @ Row,36 SAY lnTSold  PICTURE '99999'        
    @ Row,52 SAY lnTCut   PICTURE '999999'
    @ Row,69 SAY lnTIss   PICTURE '99999'    
    @ Row,75 SAY lnTOnH   PICTURE '9999999'  
    @ Row,83 SAY lnTShip  PICTURE '9999999'  
    @ Row,101 SAY lnTToSh  PICTURE '9999999'  
    @ Row,109 SAY lnTToSl PICTURE '9999999'   
    Row=Row+1
    =lfPrntHdr()
    @ Row,41 SAY lnTSoldA PICTURE '9999999.99'
    @ Row,58 SAY lnTCutA  PICTURE '9999999.99'
    @ Row,90 SAY lnTShipA PICTURE '9999999.99' 
    @ Row,116 SAY lnTToSlA PICTURE '99999999.99'
    Row=Row+1
    =lfPrntHdr()
    @ Row,01 SAY REPLICATE('-',126)
    Row = Row + 1
    =lfPrntHdr()
    @ Row,10 SAY 'Total without negative OTS'
    @ Row,100 SAY lnPosOTS PICTURE '9999999'
    Row = Row + 1
    =lfPrntHdr()
    @ Row,107 SAY lnPosAmt PICTURE '99999999.99'
  ENDIF
  IF Row>38 AND !EOF()
    lnPageNo=lnPageNo+1
    DO lpHeader
  ENDIF
ENDDO
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lpHeader
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/02/1999
*! Purpose   : Print the header.
*!*************************************************************
*! Example            :  DO lpPrint.
*!*************************************************************
PROCEDURE lpHeader

lcCol=INT(114-LEN(ALLTRIM(lcCompName)))/2
@ 03,lcCol SAY lcCompName
@ 04,01 SAY 'DATE:'
@ 04,07 SAY DATE()
@ 04,17 SAY 'TIME:'
@ 04,23 SAY TIME()
@ 04,47 SAY 'CUT AND SOLD REPORT'
@ 04,104 SAY 'PAGE #:'
@ 04,111 SAY lnPageNo PICTURE '99'
@ 05,01 SAY 'Season : ' + &lcStyTmp..Season
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3
* STYLE  CLR DESCRIPTION     SOLD   AMOUNT    CUT   AMOUNT ISSUE ON-HAND SHIPPED   AMOUNT TO SHIP TO SELL   AMOUNT
* ------ --- --------------- ---- -------- ------ -------- ----- ------- ------- -------- ------- ------- --------
@ 06,123 SAY 'TO'
@ 07,122 SAY 'SELL'
@ 08,01 SAY '&lcMajTitle'+SPACE(8)+'&lcNonMajTl DESCRIPTION     SOLD    AMOUNT    CUT    AMOUNT ISSUE ON-HAND SHIPPED    AMOUNT TO SHIP TO SELL     AMOUNT'
@ 09,01 SAY '------       -----  --------------- ---- --------- ------  -------- ----- ------- -------  -------- ------- -------   --------'
Row=10

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/02/1999
*! Purpose   : To draw the header in case of row > 38
*!*************************************************************
*! Example            :  lfPrntHdr()
*!*************************************************************
FUNCTION lfPrntHdr

IF Row > 38
  lnPageNo=lnPageNo+1
  DO lpHeader
ENDIF


FUNCTION lfwRepWhen


****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 05/13/1999
* Refer To : (C101477)
****************************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 05/1/03/1999
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 01/27/99
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
* Refer To : (C101477)
*!*************************************************************
FUNCTION lfEvalSegs

**Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.      
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]

****************************************************************************
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 05/03/1999
* Refer To : (C101477)
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

