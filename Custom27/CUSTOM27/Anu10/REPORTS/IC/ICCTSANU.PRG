*:************************************************************************
*: Program file  : ICCTSANU.PRG
*: Program desc. : CUT & SOLD REPORT CUSTOMIZED FOR ANU100 (CONVERTED FROM 2.6 TO 2.7)
*:         System: ARIA27
*:      Developer: TAREK MOHAMED IBRAHIM
*:           Date: 06/07/2001
*:       Entry   : C#102316
*:*************************************************************************
*:Modifications
*:*************************************************************************
llMultiWH  = gfGetMemVar('M_WareHouse')='Y'

=gfOpenFile(gcDataDir+'Style','Style','SH')
=gfOpenFile(gcDataDir+'Scale','Scale','SH')
=gfOpenFile(gcDataDir+'Codes' ,'Codes','SH')
=gfOpenFile(gcDataDir+'Warehous','Warehous','SH')

qTopic = 'CUT & SOLD REPORT'

*---------------------------------------------------------------------
* NOTE: All these fields must be initialized by the calling program
*---------------------------------------------------------------------
STORE SPACE(30) TO XTITLE
STORE SPACE(30) TO XOPT_TITLE
STORE SPACE(12) TO LSTYLE, HSTYLE, XFABRIC
STORE SPACE(02) TO XSEASON, XDIVISION, XGROUP
STORE SPACE(01) TO XMAKE, XSTATUS, TSEA, TDIV, TCLR
STORE SPACE(10) TO XPATTERN
STORE SPACE(01) TO XSORT
STORE SPACE(01) TO XBOMPRT                    
XRPT_CD='C'
IF llMultiWH
  STORE 'N'  TO lcLoc   
ENDIF
*--------------------------------------------------------------
STORE SPACE(20) TO HPDESC
XFILTER = ' '

*Initializing the following variables
*in order to calculate the positive OTS qty and 
*OTS amount.
STORE 0 TO lnPosOTS,lnPosAmt

*--Clear temp file if it is found
=lfClearRep()
  
SELECT STYLE
COPY TO (gcWorkDir+lcStyTmp) FOR &lcRpExp

*-- If no styles copied then exit
IF _TALLY=0
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

select 0
=gfOpenFile(gcWorkDir+lcStyTmp,'','SH')
*INDEX ON Season +Style + Color TAG &lcStyTmp
INDEX ON Season+Style TAG &lcStyTmp

STORE {} TO ldFOrdDt,ldTOrdDt,ldFInvDt,ldTInvDt,ldFCutDt,ldTCutDt
*--Get values setted from Option Grid
ldFOrdDt=ldRpOrdFDt
ldTOrdDt=ldRpOrdTDt
ldFInvDt=ldRpInvFDt
ldTInvDt=ldRpInvTDt
ldFCutDt=ldRpCttFDt
ldTCutDt=ldRpCttTDt

=gfOpenFile(gcDataDir+'InvHdr' ,'InvHdr','SH')
=gfOpenFile(gcDataDir+'InvLine','InvLineO','SH')
=gfOpenFile(gcDataDir+'OrdHdr' ,'OrdHdr','SH')
=gfOpenFile(gcDataDir+'OrdLine','OrdLineS','SH')
=gfOpenFile(gcDataDir+'CutTktH','CutTktH','SH')
=gfOpenFile(gcDataDir+'CutTktL','CutTktLS','SH')

R_WIDTH = 'XW'
SET DEVICE TO PRINT

*IF gcDevice='PRINTER'
*  *--LandScape ESC sequence.
*  lcLandEsc=[&l1O&l14]
*  @ PROW(),PCOL() SAY ' '
*  @ PROW(),PCOL() SAY lcLandEsc 
*  @ PROW(),PCOL() SAY ' '
*ENDIF

*--Set relations without additive clause
SELECT OrdLine
*C#102316,4 TMI [START] Wrong expression in the relation  .i.e. 'O'+Ordline.Order instead of Ordline.Order
*SET RELATION TO OrdLine.Order + STR(OrdLine.LineNo,6) INTO InvLine,;
                Ordline.Order INTO OrdHdr
SET RELATION TO OrdLine.Order + STR(OrdLine.LineNo,6) INTO InvLine,;
                'O'+Ordline.Order INTO OrdHdr
*C#102316,4 TMI [End  ] 

SELECT CutTktL
SET RELATION TO CutTktL.CutTkt INTO CutTktH

R_TITLE = 'CUT & SOLD'
Row=1
lnPageNo=1
STORE 0 TO lnTSold,lnTSoldA,lnTCut,lnTCutA,lnTIss,lnTOnH,lnTShip,;
           lnTShipA,lnTToSh,lnTToSl,lnTToSlA

lcSeason = &lcStyTmp..Season
llPrnSeas = .T.

DO lpHeader

SELECT &lcStyTmp
STORE 0 TO lnSubSldQ,lnSubSldA,lnSubCutQ,lnSubCutA,lnSubIssQ,lnSubOnHQ,;
           lnSubShpQ,lnSubShpA,lnSubToSh,lnSubToSl,lnSubToSlA
           
DO WHILE !EOF() .AND. INKEY() <> 32
  IF Season <> lcSeason 
    @ Row,01 SAY REPLICATE('-',126) 
    Row=Row+1
    @ Row,10 SAY lcSeason +' : SUBTOTAL'
    @ Row,37 SAY lnSubSldQ  PICTURE '99999'
    @ Row,52 SAY lnSubCutQ  PICTURE '999999'   
    @ Row,69 SAY lnSubIssQ  PICTURE '99999'    
    @ Row,75 SAY lnSubOnHQ  PICTURE '9999999'  
    @ Row,83 SAY lnSubShpQ  PICTURE '9999999'  
    @ Row,101 SAY lnSubToSh PICTURE '9999999'  
    @ Row,109 SAY lnSubToSl PICTURE '9999999'   
    Row=Row+1
    
    @ Row,41 SAY lnSubSldA   PICTURE '9999999.99'
    @ Row,58 SAY lnSubCutA   PICTURE '9999999.99'
    @ Row,90 SAY lnSubShpA   PICTURE '9999999.99'
    @ Row,117 SAY lnSubToSlA PICTURE '9999999.99'
    
    Row=Row+1
    @ Row,01 SAY REPLICATE('-',126)
    STORE 0 TO lnSubSldQ,lnSubSldA,lnSubCutQ,lnSubCutA,lnSubIssQ,lnSubOnHQ,;
               lnSubShpQ,lnSubShpA,lnSubToSh,lnSubToSl,lnSubToSlA
    lcSeason = Season
    lnPageNo = lnPageNo+1
    DO lpHeader
  ENDIF 
  
  lcStyle=Style
  lcStyMaj=CSTYMAJOR
  STORE 0 TO lnSldQty,lnSldAmt,lnCutQty,lnCutAmt,lnIssQty,lnOnHQty,lnShpQty,lnShpAmt
  
  SELECT OrdLine  
  IF SEEK(lcStyle)
    lcExp=IIF (!( EMPTY(ldFOrdDt) AND EMPTY(ldTOrdDt) ),'Complete>=ldFOrdDt AND Complete<=ldTOrdDt','1=1')    
    SCAN WHILE Style=lcStyle FOR (&lcExp AND !(OrdHdr.Status $ 'X'))
      lnSldQty=lnSldQty+TotQty+InvLine.TotQty
      lnSldAmt=lnSldAmt+((TotQty+InvLine.TotQty)*Price)
    ENDSCAN
  ENDIF
  lnSubSldQ=lnSubSldQ+lnSldQty
  lnSubSldA=lnSubSldA+lnSldAmt  

  lnTSold  =lnTSold +lnSldQty
  lnTSoldA =lnTSoldA+lnSldAmt
    
  SELECT CutTktL
  IF SEEK(lcStyle)
    lcExp=IIF (!( EMPTY(ldFCutDt) AND EMPTY(ldTCutDt) ),'CutTktH.Entered>=ldFCutDt AND CutTktH.Entered<=ldTCutDt','1=1')
    SCAN WHILE Style=lcStyle FOR(&lcExp AND CutTktH.Status $ 'OHAC')
      IF TRANCD='1'
        lnCutQty=lnCutQty+TotQty
      ENDIF
      *DO CASE
      *  CASE TRANCD='1'
      *    lnCutQty=lnCutQty+TotQty
      *  CASE TRANCD='2'
      *    lnCutQty=lnCutQty-TotQty
      *  CASE TRANCD='3'
      *    lnCutQty=lnCutQty-TotQty
      *  CASE TRANCD='4'
      *    lnCutQty=lnCutQty-TotQty
      *ENDCASE
    ENDSCAN
    lnCutAmt=lnCutQty*&lcStyTmp..PriceA
  ENDIF
  lnSubCutQ=lnSubCutQ+lnCutQty
  lnSubCutA=lnSubCutA+lnCutAmt
  lnTCut   =lnTCut   +lnCutQty
  lnTCutA  =lnTCutA  +lnCutAmt

  SELECT CutTktL
  IF SEEK(lcStyle)
    lcExp=IIF (!( EMPTY(ldFCutDt) AND EMPTY(ldTCutDt) ),'CutTktH.Entered>=ldFCutDt AND CutTktH.Entered<=ldTCutDt','1=1')
    SCAN WHILE Style=lcStyle FOR(&lcExp AND CutTktH.Status $ 'OH')
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
  
  lnTIss   =lnTIss+lnIssQty

  lnOnHQty =&lcStyTmp..TotStk
  lnSubOnHQ=lnSubOnHQ+lnOnHQty
  lnTOnH   =lnTOnH   +lnOnHQty
  
  SELECT InvLine
  SET ORDER TO InvLineS
  IF SEEK(lcStyle)
    lcExp=IIF (!( EMPTY(ldFInvDt) AND EMPTY(ldTInvDt) ),'InvDate>=ldFInvDt AND InvDate<=ldTInvDt','1=1')
    SCAN WHILE Style=lcStyle FOR &lcExp
      lnShpQty=lnShpQty+TotQty
      lnShpAmt=lnShpAmt+(TotQty*Price)
    ENDSCAN
  ENDIF
  lnSubShpQ=lnSubShpQ+lnShpQty
  lnSubShpA=lnSubShpA+lnShpAmt
  lnTShip  =lnTShip  +lnShpQty
  lnTShipA =lnTShipA +lnShpAmt
  SET ORDER TO InvLineO
    
  *--To ship
  lnToShip =lnSldQty -lnShpQty
  lnSubToSh=lnSubToSh+lnToShip
  lnTToSh  =lnTToSh  +lnToShip

  *--To sell
  lnToSell =lnCutQty -lnSldQty
  lnSubToSl=lnSubToSl+lnToSell
  lnTToSl  =lnTToSl  +lnToSell
  
  *-- collecting data wait window 
  WAit WINDOW lcStyle NOWAIT

  SELECT &lcStyTmp
  @ Row,01  SAY SUBSTR(Style,1,19)      
  @ Row,21  SAY SUBSTR(Desc,1,15)
  @ Row,37  SAY lnSldQty PICTURE '9999'
  @ Row,42  SAY lnSldAmt PICTURE '999999.99' 
  @ Row,52  SAY lnCutQty PICTURE '999999'    
  @ Row,59  SAY lnCutAmt PICTURE '999999.99'
  @ Row,69  SAY lnIssQty PICTURE '99999'     
  @ Row,75  SAY lnOnHQty PICTURE '9999999'   
  @ Row,83  SAY lnShpQty PICTURE '9999999'   
  @ Row,91  SAY lnShpAmt PICTURE '999999.99' 
  @ Row,101 SAY lnToShip PICTURE '9999999'   
  @ Row,109 SAY lnToSell PICTURE '9999999'  
  @ Row,117 SAY lnToSell*PriceA PICTURE '9999999.99' 

  lnPosOTS = lnPosOTS + IIF(lnToSell > 0,lnToSell,0)
  lnPosAmt = lnPosAmt + IIF(lnToSell*PriceA > 0, lnToSell*PriceA, 0)
  
  lnTToSlA=lnTToSlA+(lnToSell*PriceA)
  lnSubToSlA=lnSubToSlA+(lnToSell*PriceA)
  SKIP
  IF &lcStyTmp..CSTYMAJOR = lcStyMaj
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
    @ Row,101 SAY lnSubToSh PICTURE '9999999'  
    @ Row,109 SAY lnSubToSl PICTURE '9999999'   
    Row=Row+1

    *--Adding this function to print the header
    *--in case of numbers of rows exceed 38. and moving
    *--the following totals 1 chr to the left.
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
    @ Row,101 SAY lnTToSh PICTURE '9999999'  
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
    @ Row,109 SAY lnPosOTS PICTURE '9999999'
    Row = Row + 1
    =lfPrntHdr()
    @ Row,116 SAY lnPosAmt PICTURE '99999999.99'
    
  ENDIF

  IF Row>38 AND !EOF()
    lnPageNo=lnPageNo+1
    DO lpHeader
  ENDIF

ENDDO

DO ENDREPORT

*--Reset DEVICE to Screen
SET DEVICE TO SCREEN

*:**************************************************************************
*:* Name        : lpHeader
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/07/2001
*:* Purpose     : Print the header.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpHeader()
*:***************************************************************************
PROCEDURE lpHeader
PRIVATE lnI
lnI = 0
lcCol=INT(123-LEN(ALLTRIM(QCompany)))/2
@ 03,lcCol SAY QCompany
@ 04,01 SAY 'DATE:'
@ 04,07 SAY DATE()
@ 04,17 SAY 'TIME:'
@ 04,23 SAY TIME()
@ 04,52 SAY 'CUT AND SOLD REPORT'
@ 04,118 SAY 'PAGE #:'
@ 04,125 SAY lnPageNo PICTURE '99'
lnCol=INT(123-LEN(ALLTRIM(lcRPTitle)))/2
lnI = IIF(!EMPTY(lcRPTitle),1,0)
IF lnI=1
  @ 05,lnCol SAY ALLTRIM(lcRPTitle)
ENDIF  
@ 05+lnI,01 SAY 'Season : ' + &lcStyTmp..Season
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3
* STYLE               DESCRIPTION     SOLD   AMOUNT    CUT   AMOUNT ISSUE ON-HAND SHIPPED   AMOUNT TO SHIP TO SELL   AMOUNT
* ------------------- --------------- ---- -------- ------ -------- ----- ------- ------- -------- ------- ------- --------
@ 06+lnI,123 SAY 'TO'  
@ 07+lnI,122 SAY 'SELL'
@ 08+lnI,01 SAY 'STYLE               DESCRIPTION     SOLD    AMOUNT    CUT    AMOUNT ISSUE ON-HAND SHIPPED    AMOUNT TO SHIP TO SELL     AMOUNT'
@ 09+lnI,01 SAY '------------------- --------------- ---- --------- ------  -------- ----- ------- -------  -------- ------- -------   --------'
Row=10+lnI

*:**************************************************************************
*:* Name        : lfPrntHdr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/07/2001
*:* Purpose     : To draw the header in case of row > 38
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPrntHdr()
*:***************************************************************************
FUNCTION lfPrntHdr
IF Row > 38
  lnPageNo=lnPageNo+1
  DO lpHeader
ENDIF


*:*************************************************************************:*
*:***************       OPTION GRID FUNCTIONS       ***********************:*
*:*************************************************************************:*

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : When funciton for option grid.
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRepWhen()
*:***************************************************************************
FUNCTION lfwRepWhen

DIMENSION laRPPrnItm[14]
llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')
lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

lnFreSgPo = 0
lnArrPos  = ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)')
IF lnArrPos>0
  lnFreSgPo = ASUBSCRIPT(laOGVrFlt,lnArrPos,1)
ENDIF


*-- Disable/enable Only This colors, Free Segment. [begin]
*DO CASE
*  CASE lcFreeClr = 'C'
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
*  CASE lcFreeClr = 'F'
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
*  OTHERWISE
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
*   laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
*ENDCASE
*-- Disable/enable Only This colors, Free Segment. [end]

*-- end of lfwRepWhen

*:**************************************************************************
*:* Name        : lfvDate
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : Valid function for dates
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvDate()
*:***************************************************************************
FUNCTION lfvDate
PARAMETERS lcFDt,lcTDt
PRIVATE ldFrom,ldTo,lcCent
STORE {} TO ldFrom,ldTo
IF !EMPTY(&lcTDt) 
  ldFrom = &lcFDt
  ldTo   = &lcTDt
ENDIF
lcCent = SET('CENTU')
SET CENTU ON
lcTitle = 'Dates'
DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
SET CENTU &lcCent

*:**************************************************************************
*:* Name        : lfvpbOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : Ok button valid functiont
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvpbOk()
*:***************************************************************************
FUNCTION lfvpbOk
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
   &lcFDt = ldFrom
   &lcTDt = ldTo
   CLEAR READ
ENDIF

*:**************************************************************************
*:* Name        : lfMajTtGet
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : To get the style major segement title
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMajTtGet()
*:***************************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*:**************************************************************************
*:* Name        : lfMajPic
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*!* Purpose     : Get major seg. picture
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMajPic()
*:***************************************************************************
FUNCTION lfMajPic
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
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

*:**************************************************************************
*:* Name        : lfSetSty  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : Set-valid -reset  valid function for style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSetSty  ()
*:***************************************************************************
FUNCTION lfSetSty  
PARAMETERS lcParam
PRIVATE lcSlct
lcSlct = SELECT(0)

SELECT STYLE
IF lcParam='S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   LOCATE
ELSE   
   SET ORDER TO TAG STYLE IN STYLE    
ENDIF

SELECT (lcSlct)
*-- end of lfSetSty.

*:**************************************************************************
*:* Name        : lfvFabric
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : Validate fabric
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvFabric()
*:***************************************************************************
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
*-- end of lfvFabric.

*:**************************************************************************
*:* Name        : lfClearRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/11/2001
*:* Purpose     : Closes opened temp files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfClearRep()
*:***************************************************************************
FUNCTION lfClearRep
llOGFltCh = .T.  
IF USED(lcStyTmp)
  USE IN (lcStyTmp)
  ERASE &gcWorkDIr.&lcStyTmp..DBF
  ERASE &gcWorkDIr.&lcStyTmp..CDX
ENDIF
*-- end of lfClearRep.