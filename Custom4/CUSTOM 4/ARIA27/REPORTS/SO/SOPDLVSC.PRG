*:***************************************************************************
*: Program file  : SODELVSC.prg
*: Program desc. : Sales Order Delivery Schedule
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : TAREK MOHAMED IBRAHIM - TMI
*: REFER TO      : N000381,1
*:***************************************************************************
*: Calls :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:Modifications  :
*:************************************************************************************
*--First check Completion Dates and start dates[start]
llNoRecs = .T.

llCutTkt = ('MF' $ gcCmpModules )

llPo = ('PO' $ gcCmpModules ) OR ('PS' $ gcCmpModules )

IF lcBuckBy == "C"
  IF EMPTY(laOGFxFlt[lnStartPos,6]) .OR. LEN(ALLTRIM(laOGFxFlt[lnStartPos,6])) < 8
    WAIT WINDOW 'You have to enter at least one Start Date !'
    SET DEVICE TO SCREEN
    llNoRecs = .T.
    RETURN
  ELSE
    IF OCCURS('|',laOGFxFlt[lnStartPos,6]) > 1
      WAIT WINDOW 'Only 2 Start Dates will be printed ...'
    ENDIF
  ENDIF
ELSE
  IF EMPTY(laOGFxFlt[lnCompPos,6]) .OR. LEN(ALLTRIM(laOGFxFlt[lnCompPos,6])) < 8
    WAIT WINDOW 'You have to enter at least one Complete Date !'
    SET DEVICE TO SCREEN
    llNoRecs = .T.
    RETURN
  ELSE
    IF OCCURS('|',laOGFxFlt[lnCompPos,6]) > 1
      WAIT WINDOW 'Only 2 Complete Dates will be printed ...'
    ENDIF
  ENDIF
ENDIF

IF lcBuckBy == "C"
  IF EMPTY(ldRpCmp)
    WAIT WINDOW 'No Bucket Completion date found, Cannot proceed!'
    RETURN
  ENDIF
ELSE
  IF EMPTY(ldRpSrt)
    WAIT WINDOW 'No Bucket Starting date found, Cannot proceed!'
    RETURN
  ENDIF
ENDIF
PRIVATE lcCentSt
lcCentSt = SET('CENTURY')
SET CENTURY ON

ldEntrDate = IIF(lcBuckBy == "C", ldRpCmp , ldRpSrt )   && Get entered date
XDATE0 = ldEntrDate - DOW(ldEntrDate) 
XDATE1 = XDATE0 + 7                    
XDATE2 = XDATE0 + 14                   
XDATE3 = XDATE0 + 21                   
XDATE4 = XDATE0 + 28                  
STORE XDATE0 + 1 TO ldMinComp,ldMinStrt
STORE XDATE4     TO ldMaxComp,ldMaxStrt

IF lcBuckBy == "C"
  lcRpExp = lcRpExp + '.AND. BETWEEN(DTOS(OrdLine.COMPLETE),DTOS({' + DTOC(ldMinComp) + '}),DTOS({' + DTOC(ldMaxComp) + '}))'
ELSE
  lcRpExp = lcRpExp + '.AND. BETWEEN(DTOS(OrdLine.START),DTOS({' + DTOC(ldMinStrt) + '}),DTOS({' + DTOC(ldMaxStrt) + '}))'
ENDIF  

SET CENTURY &lcCentSt

IF lcMake <> 'A'
  lcMakeExp = "STYLE.MAKE = IIF(lcMake='N',.F.,.T.)"
  lcRpExp = lcRpExp + ' AND ' + lcMakeExp
ENDIF

*-- lcRepNmTtl hold the header of the non major segment for the frx
STORE SPACE(0) TO WORKTEMP,CUTTTEMP,POTEMP,ORDTEMP,XPRTWIP,XWIPRPT,XWIPRPT,;
  XPRTORD,XORDRPT,XORDRPT,XSTAT
STORE .F. TO llNoRecs
lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
XBYCOLOR   = lcRpByClr

lnStartPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)

lnCompPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.COMPLETE'),1)

DECLARE laCmpRange[4]

STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
  XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
  XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
  XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
  XWIP1,XWIP2,XWIP3,XWIP4

SELECT ORDLINE

llNoRecs = .F.

DO lpPrepPrnt

*:**************************************************************************
*:* Name        : lfMajTtGet
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMajTtGet()
*:***************************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*:**************************************************************************
*:* Name        : lfNonMaj
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : Non major part of style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfNonMaj()
*:***************************************************************************
FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F'
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
      lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
      lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
    ENDIF
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : When function of the OG
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRepWhen()
*:***************************************************************************
FUNCTION lfwRepWhen

SET ORDER TO SEGVAL IN ICSEGVAL
SET ORDER TO SCALE IN SCALE
STORE 1 TO lnMajPos

*-- if it is the default case
IF lnOgSeting = 1

  IF lcBuckBy="C"
    ldRpCmp  = LDOM(DATE())
  ELSE
    ldRpSrt  = LDOM(DATE())
  ENDIF

ENDIF

IF lcBuckBy == "C"
  lnStartPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)
ELSE
  lnCompPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.COMPLETE'),1)
ENDIF

IF lcBuckBy == "S"
  IF EMPTY(laOGFxFlt[lnCompPos,6])
    laOGFxFlt[lnCompPos,6] = DTOC(DATE()) + '|' + DTOC(DATE())
  ENDIF
ELSE
  IF EMPTY(laOGFxFlt[lnStartPos,6])
    laOGFxFlt[lnStartPos,6] = DTOC(DATE()) + '|' + DTOC(DATE())
  ENDIF
ENDIF

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
*--Get Major Length
IF lnMajSeg > 1
  FOR lnLoop = 1 TO lnMajSeg
    lnMajLen = lnMajLen + LEN(laMajSeg[lnLoop,3])
  ENDFOR
  *--Take Care of seprators
  lnMajLen = lnMajLen + lnMajSeg - 1
ELSE
  lnMajLen = LEN(laMajSeg[lnMajSeg,3])
ENDIF


*!*********************************************************************

*:**************************************************************************
*:* Name        : lfAdjPrnArr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAdjPrnArr()
*:***************************************************************************
FUNCTION lfAdjPrnArr

DIMENSION laRPPrnDsp[2,1],laRPPrnRet[2,1]

laRPPrnDsp[1,1]=lcMajTtl
laRPPrnDsp[2,1]=lcNonMajT

laRPPrnRet[1,1]='S'
laRPPrnRet[2,1]='C'

*:**************************************************************************
*:* Name        : lfMajPic
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMajPic()
*:***************************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************

*:**************************************************************************
*:* Name        : lfvStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : Valid Fn. of Styles
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvStyle()
*:***************************************************************************
FUNCTION lfvStyle
lcAlias = Alias()
lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE
SELECT STYLE
LOCATE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style')
    &lcStyle = PADR(STYLE.CSTYMAJOR,19)
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE
SELECT (lcAlias)

*:**************************************************************************
*:* Name        : lfvFabric
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
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

****************************************************************************
PROCEDURE lpPrepPrnt
**lcMake    &&

XTITLE    = lcRPTitle
XMAKE     = lcMake   &&lcRPDomImp
ROW     = 99
DIMENSION XPREPAK[8]
STORE 0 TO XPREPAK

R_TITLE = 'Delivery Schedule'
R_WIDTH = 'W'
PAGENO     = 00

WAIT WINDOW 'PRINTING ... <Space Bar> to Abort' NOWAIT
SELECT STYLE
SET FILTER TO &lcRPExp
LOCATE

SET DEVICE TO PRINTER

DO lpPrint

IF llNoRecs = .F.
  DO ENDREPORT
ELSE
  WAIT WINDOW   'No records selected to print.. !'
  SET DEVICE TO SCREEN
  RETURN
ENDIF

IF USED('WorkTemp')
  USE IN ('WorkTemp')
ENDIF

IF USED('ORDTEMP')
  USE IN ('ORDTEMP')
ENDIF


SET DEVICE TO SCREEN

********************** Calculations and Printing ***************************
PROCEDURE lpPrint

SELECT ORDLINE
LOCATE

SELECT 0
*-----------------------------------
WorkTemp = gfTempName()
ORDTEMP  = gfTempName()
CREATE TABLE (gcWorkDir + ORDTEMP)(STYLE  C(19)      ,;
  COLOR  C(6)       ,;
  O_ORD1 N(10)       ,;
  O_ORD2 N(10)       ,;
  O_ORD3 N(10)       ,;
  O_ORD4 N(10)       ,;
  O_ORDAMT1 N(13,2) ,;
  O_ORDAMT2 N(13,2) ,;
  O_ORDAMT3 N(13,2) ,;
  O_ORDAMT4 N(13,2) ,;
  H_ORD1 N(10)       ,;
  H_ORD2 N(10)       ,;
  H_ORD3 N(10)       ,;
  H_ORD4 N(10)       ,;
  H_ORDAMT1 N(13,2) ,;
  H_ORDAMT2 N(13,2) ,;
  H_ORDAMT3 N(13,2) ,;
  H_ORDAMT4 N(13,2) ,;
  WIP1 N(10)         ,;
  WIP2 N(10)         ,;
  WIP3 N(10)         ,;
  WIP4 N(10)         ,;
  STK1 N(10)         ,;
  STK2 N(10)         ,;
  STK3 N(10)         ,;
  STK4 N(10)         ,;
  TOTSTK N(10)       ,;
  TOTORD N(12)       ,;
  TOTWIP N(10,2)    ,;
  H_TOTORDA N(13,2) ,;
  O_TOTORDA N(13,2) ,;
  PYSHIP1 N(7)      ,;
  PYSHIP2 N(7)      ,;
  PYSHIP3 N(7)      ,;
  PYSHIP4 N(7)      ,;
  PNSHIP1 N(7)      ,;
  PNSHIP2 N(7)      ,;
  PNSHIP3 N(7)      ,;
  PNSHIP4 N(7)      ,;
  AYSHIP1 N(7)      ,;
  AYSHIP2 N(7)      ,;
  AYSHIP3 N(7)      ,;
  AYSHIP4 N(7)      ,;
  ANSHIP1 N(7)      ,;
  ANSHIP2 N(7)      ,;
  ANSHIP3 N(7)      ,;
  ANSHIP4 N(7))
*-----------------------------------
*Table alrady created ..HDM.
*// COPY STRUCTURE TO &QWD.&ORDTEMP

IF XBYCOLOR <> 'Y'
  COPY STRUCTURE TO (gcWorkDir + WorkTemp)
  USE (gcWorkDir + WorkTemp) IN 0
  SELECT &WorkTemp
  INDEX ON STYLE TAG &WorkTemp
ENDIF
SELECT 0
SELECT &ORDTEMP
INDEX ON STYLE TAG &ORDTEMP
SET ORDER TO TAG &ORDTEMP

IF llCutTkt
  SELECT CUTTKTL
  SET RELATION TO CUTTKT INTO CUTTKTH
ENDIF
IF llPo
  SELECT POSLN
  SET RELATION TO 'P' + PO INTO POSHDR
ENDIF
SELECT ORDLINE
SET RELATION TO 'O' + ORDER INTO ORDHDR

*--Establish relation between ordline and style file according to report style
SELECT STYLE
SET ORDER TO TAG Style
SELECT ORDLINE
SET RELATION TO Ordline.style INTO Style ADDITIVE

XSTYLE = SPACE(19)
XCOLOR = SPACE(6)
HSTYLE = SPACE(6)
SELECT ORDLINE
LOCATE
llNoRecs = .T.

IF !EMPTY(lcRpEdiFlt)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcRpEdiFlt
ENDIF

llNoRecs = .F.

lcLopedSty = ''

PRIVATE lnflag
lnflag = 0
IF USED(laOgFxFLt[1,6])
  SELECT &laOgFxFLt[1,6]
  LOCATE
  IF !EOF()
    lnflag = 1
  ENDIF 
ENDIF

IF lnflag = 1

  SCAN
    
    =SEEK(LEFT(cStyMajor,lnMajLen),"ORDLINE")
    IF FOUND("ORDLINE")

      SELECT OrdLine
      SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(lineno,6) =;
                      LEFT(&laOgFxFLt[1,6]..cStyMajor,lnMajLen);
                FOR &lcRpExp AND !(IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) $ lcLopedSty)

        llNorecs = .F.
        STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
          XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
          XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
          XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
          XWIP1,XWIP2,XWIP3,XWIP4
        SELECT STYLE
        XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
        IF EOF()
          EXIT
        ENDIF
        IF !EMPTY(HSTYLE) .AND. STYLE > HSTYLE
          EXIT
        ENDIF
        IF IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) <> XSTYLE
          STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
            XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
            XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
            XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
            XWIP1,XWIP2,XWIP3,XWIP4
        ENDIF
  
        IF lcRpByClr = 'Y'
          XSTYLE = STYLE
        ELSE
          XSTYLE = PADR(SUBSTR(STYLE,1,lnMajLen),19)
        ENDIF
  
        lcLopedSty = lcLopedSty + '|' + XSTYLE

        WAIT WINDOW 'Calculating Cut&Sold info for ' + lcStyTitle + ' ' + XSTYLE NOWAIT

        XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
        XTOTSTK = 0
        XKEY    = STYLE
        XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
        *-------------------------
        * LOOP ORDER LINE FILE
        *---------------------- ---
        SELECT &ORDTEMP
        IF !SEEK( XKEY )
          APPEND BLANK
          REPLACE STYLE  WITH XSTYLE,;
            COLOR  WITH XCOLOR
        ENDIF
        REPLACE TOTSTK WITH TOTSTK + XTOTSTK
        
        SELECT ORDLINE

        lnLineRec = RECNO()
  
        SELECT ORDLINE
        lnRecNo = IIF(EOF(),0,RECNO())
        SCAN REST WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
  
          lnRecNo = IIF(EOF(),0,RECNO())
          IF ORDHDR->STATUS $ 'CX' .OR. TOTQTY <= 0
            LOOP
          ENDIF
          XORDER = ORDER

          IF lcBuckBy == "C"
            DO CASE
              CASE BETWEEN(ORDHDR.COMPLETE,XDATE0 + 1 , XDATE1)
                Z='1'
              CASE BETWEEN(ORDHDR.COMPLETE,XDATE1 + 1 , XDATE2)
                Z='2'
              CASE BETWEEN(ORDHDR.COMPLETE,XDATE2 + 1 , XDATE3)
                Z='3'
              CASE BETWEEN(ORDHDR.COMPLETE,XDATE3 + 1 , XDATE4)
                Z='4'
              OTHE
                Z= '*'
            ENDCASE
          ELSE
            DO CASE
              CASE BETWEEN(ORDHDR.START,XDATE0 + 1 , XDATE1)              
                Z='1'
              CASE BETWEEN(ORDHDR.START,XDATE1 + 1 , XDATE2)
                Z='2'
              CASE BETWEEN(ORDHDR.START,XDATE2 + 1 , XDATE3)
                Z='3'
              CASE BETWEEN(ORDHDR.START,XDATE3 + 1 , XDATE4)
                Z='4'
              OTHE
                Z= '*'
            ENDCASE
          ENDIF
             
          IF Z<>'*'
            IF (ORDHDR->STATUS='O')
              XO_ORD&Z     =XO_ORD&Z   + TOTQTY
              XO_ORDAMT&Z  =XO_ORDAMT&Z+ (PRICE * TOTQTY)
            ELSE
              XH_ORD&Z     =XH_ORD&Z   + TOTQTY
              XH_ORDAMT&Z  =XH_ORDAMT&Z+ (PRICE * TOTQTY)
            ENDIF
          ENDIF
        ENDSCAN
        SELECT ORDLINE
        IF lnRecNo > 0
          GO (lnRecNo)
        ENDIF
  
        SELECT &ORDTEMP
        REPLACE O_ORD1     WITH O_ORD1+XO_ORD1;
          O_ORDAMT1  WITH O_ORDAMT1+XO_ORDAMT1;
          H_ORD1     WITH H_ORD1+XH_ORD1;
          H_ORDAMT1  WITH H_ORDAMT1+XH_ORDAMT1;
          O_ORD2     WITH O_ORD2+XO_ORD2;
          O_ORDAMT2  WITH O_ORDAMT2+XO_ORDAMT2;
          H_ORD2     WITH H_ORD2+XH_ORD2;
          H_ORDAMT2  WITH H_ORDAMT2+XH_ORDAMT2;
          O_ORD3     WITH O_ORD3+XO_ORD3;
          O_ORDAMT3  WITH O_ORDAMT3+XO_ORDAMT3;
          H_ORD3     WITH H_ORD3+XH_ORD3;
          H_ORDAMT3  WITH H_ORDAMT3+XH_ORDAMT3;
          O_ORD4     WITH O_ORD4+XO_ORD4;
          O_ORDAMT4  WITH O_ORDAMT4+XO_ORDAMT4;
          H_ORD4     WITH H_ORD4+XH_ORD4;
          H_ORDAMT4  WITH H_ORDAMT4+XH_ORDAMT4
        ***
        ****** [PO] PROCESS P/O FILE
        ***
        
        IF lcRpBsPrjn $ 'WA'

          IF STYLE->MAKE
  
            IF llCutTkt
              SELE CUTTKTL
              LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
              SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR CUTTKTH->STATUS $ 'OH'
                IF lcBuckBy == "C"
                  DO CASE
                    CASE BETWEEN(CUTTKTH.COMPLETE, XDATE0 + 1 , XDATE1)
                      XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(CUTTKTH.COMPLETE, XDATE1 + 1 , XDATE2)
                      XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(CUTTKTH.COMPLETE , XDATE2 + 1 , XDATE3 )
                      XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(CUTTKTH.COMPLETE , XDATE3 + 1 , XDATE4)
                      XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
                  ENDCASE
                ELSE
                  DO CASE
                    CASE BETWEEN(CUTTKTH.ENTERED, XDATE0 + 1 , XDATE1)
                      XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(CUTTKTH.ENTERED, XDATE1 + 1 , XDATE2)
                      XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(CUTTKTH.ENTERED , XDATE2 + 1 , XDATE3 )
                      XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(CUTTKTH.ENTERED , XDATE3 + 1 , XDATE4)
                      XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
                  ENDCASE
                ENDIF
              ENDSCAN
            ENDIF
            
            
          ELSE  && Not STYLE->MAKE
          
            IF llPo
        
              SELE POSLN
              lcSvORd = SET('ORDER')
              SET ORDER TO Poslns
              LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
  
              IF lcBuckBy == "C"
                SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
                   .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),;
                   DTOS(ldMinComp),DTOS((ldMaxComp)))
      
                  XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,;
                    POSHDR.AVAILABLE)
                  DO CASE
                    CASE BETWEEN(XDATE,XDATE0+1,XDATE1)
                      XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                      XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                      XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                      XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
                  ENDCASE
                ENDSCAN
              ELSE
                SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
                  .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),;
                  DTOS(ldMinStrt),DTOS((ldMaxStrt)))
          
                  XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,;
                  POSHDR.AVAILABLE)
                  DO CASE
                    CASE BETWEEN(XDATE,XDATE0+1,XDATE1)
                      XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                      XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                      XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                    CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                      XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
                  ENDCASE
                ENDSCAN
              ENDIF
              SET ORDER TO &lcSvORd
            ENDIF
  
          ENDIF  && STYLE->MAKE
  
          XWorP1 = XWIP1
          XWorP2 = XWIP2
          XWorP3 = XWIP3
          XWorP4 = XWIP4

        ENDIF
        
        =lfUpdWorPn()
  
        SELECT STYLE
        XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
      ENDSCAN

    ENDIF

  ENDSCAN

ELSE

  SELECT OrdLine
  SCAN FOR &lcRpExp AND !(IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) $ lcLopedSty)
    llNorecs = .F.
    STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
      XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
      XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
      XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
      XWIP1,XWIP2,XWIP3,XWIP4

    SELECT STYLE
    XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
    IF EOF()
      EXIT
    ENDIF
    IF !EMPTY(HSTYLE) .AND. STYLE > HSTYLE
      EXIT
    ENDIF
    IF IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) <> XSTYLE
      STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
        XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
        XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
        XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
        XWIP1,XWIP2,XWIP3,XWIP4
    ENDIF
  
    IF lcRpByClr = 'Y'
      XSTYLE = STYLE
    ELSE
      XSTYLE = PADR(SUBSTR(STYLE,1,lnMajLen),19)
    ENDIF
  
    lcLopedSty = lcLopedSty + '|' + XSTYLE

    WAIT WINDOW 'Calculating Cut&Sold info for ' + lcStyTitle + ' ' + XSTYLE NOWAIT

    XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
    XTOTSTK = 0
    XKEY    = STYLE
    XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
    *-------------------------
    * LOOP ORDER LINE FILE
    *---------------------- ---
    SELECT &ORDTEMP
    IF !SEEK( XKEY )
      APPEND BLANK
      REPLACE STYLE  WITH XSTYLE,;
        COLOR  WITH XCOLOR
    ENDIF
    REPLACE TOTSTK WITH TOTSTK + XTOTSTK
    
    SELECT ORDLINE

    lnLineRec = RECNO()
  
    SELECT ORDLINE
    lnRecNo = IIF(EOF(),0,RECNO())
    SCAN REST WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
  
      lnRecNo = IIF(EOF(),0,RECNO())
      IF ORDHDR->STATUS $ 'CX' .OR. TOTQTY <= 0
        LOOP
      ENDIF
      XORDER = ORDER

      IF lcBuckBy == "C"
        DO CASE

          CASE BETWEEN(ORDHDR.COMPLETE, XDATE0 + 1 , XDATE1)
            Z='1'
          CASE BETWEEN(ORDHDR.COMPLETE,XDATE1 + 1 , XDATE2)
            Z='2'
          CASE BETWEEN(ORDHDR.COMPLETE,XDATE2 + 1 , XDATE3)
            Z='3'
          CASE BETWEEN(ORDHDR.COMPLETE,XDATE3 + 1 , XDATE4)
            Z='4'
          OTHE
            Z= '*'
        ENDCASE
        
      ELSE
      
        DO CASE
          CASE BETWEEN(ORDHDR.START,XDATE0 + 1 , XDATE1)
            Z='1'
          CASE BETWEEN(ORDHDR.START,XDATE1 + 1 , XDATE2)
            Z='2'
          CASE BETWEEN(ORDHDR.START,XDATE2 + 1 , XDATE3)
            Z='3'
          CASE BETWEEN(ORDHDR.START,XDATE3 + 1 , XDATE4)
            Z='4'
          OTHE
            Z= '*'
        ENDCASE
      ENDIF
    
      IF Z<>'*'
        IF (ORDHDR->STATUS='O')
          XO_ORD&Z     =XO_ORD&Z   + TOTQTY
          XO_ORDAMT&Z  =XO_ORDAMT&Z+ (PRICE * TOTQTY)
        ELSE
          XH_ORD&Z     =XH_ORD&Z   + TOTQTY
          XH_ORDAMT&Z  =XH_ORDAMT&Z+ (PRICE * TOTQTY)
        ENDIF
      ENDIF
    ENDSCAN
    SELECT ORDLINE
    IF lnRecNo > 0
      GO (lnRecNo)
    ENDIF
  
    SELECT &ORDTEMP
    REPLACE O_ORD1     WITH O_ORD1+XO_ORD1;
      O_ORDAMT1  WITH O_ORDAMT1+XO_ORDAMT1;
      H_ORD1     WITH H_ORD1+XH_ORD1;
      H_ORDAMT1  WITH H_ORDAMT1+XH_ORDAMT1;
      O_ORD2     WITH O_ORD2+XO_ORD2;
      O_ORDAMT2  WITH O_ORDAMT2+XO_ORDAMT2;
      H_ORD2     WITH H_ORD2+XH_ORD2;
      H_ORDAMT2  WITH H_ORDAMT2+XH_ORDAMT2;
      O_ORD3     WITH O_ORD3+XO_ORD3;
      O_ORDAMT3  WITH O_ORDAMT3+XO_ORDAMT3;
      H_ORD3     WITH H_ORD3+XH_ORD3;
      H_ORDAMT3  WITH H_ORDAMT3+XH_ORDAMT3;
      O_ORD4     WITH O_ORD4+XO_ORD4;
      O_ORDAMT4  WITH O_ORDAMT4+XO_ORDAMT4;
      H_ORD4     WITH H_ORD4+XH_ORD4;
      H_ORDAMT4  WITH H_ORDAMT4+XH_ORDAMT4
    ***
    ****** [PO] PROCESS P/O FILE
    ***
 
    IF lcRpBsPrjn $ 'WA'

      IF STYLE->MAKE
      
        IF llCutTkt
          SELE CUTTKTL
          LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
          SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR CUTTKTH->STATUS $ 'OH'
            IF lcBuckBy == "C"
              DO CASE
                CASE BETWEEN(CUTTKTH.COMPLETE, XDATE0 + 1 , XDATE1)
                  XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(CUTTKTH.COMPLETE, XDATE1 + 1 , XDATE2)
                  XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(CUTTKTH.COMPLETE , XDATE2 + 1 , XDATE3 )
                  XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(CUTTKTH.COMPLETE , XDATE3 + 1 , XDATE4)
                  XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
              ENDCASE
            ELSE
              DO CASE
                CASE BETWEEN(CUTTKTH.ENTERED, XDATE0 + 1 , XDATE1)
                  XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(CUTTKTH.ENTERED, XDATE1 + 1 , XDATE2)
                  XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(CUTTKTH.ENTERED , XDATE2 + 1 , XDATE3 )
                  XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(CUTTKTH.ENTERED , XDATE3 + 1 , XDATE4)
                  XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
              ENDCASE
            ENDIF
          ENDSCAN
        ENDIF
        
        
      ELSE
        
        
        IF llPo
        
          SELE POSLN
          lcSvORd = SET('ORDER')
          SET ORDER TO Poslns
          LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
          IF lcBuckBy == "C"
            SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
                .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),;
                DTOS(ldMinComp),DTOS((ldMaxComp)))
        
              XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,;
                POSHDR.AVAILABLE)
              DO CASE
                CASE BETWEEN(XDATE,XDATE0+1,XDATE1)
                  XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                  XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                  XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )              
                CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                  XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
              ENDCASE
            ENDSCAN
  
          ELSE
  
            SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
                .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),;
                DTOS(ldMinStrt),DTOS((ldMaxStrt)))
        
              XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,;
              POSHDR.AVAILABLE)
              DO CASE
                CASE BETWEEN(XDATE,XDATE0+1,XDATE1)
                  XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                  XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                  XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                  XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
              ENDCASE
            ENDSCAN
          ENDIF
          SET ORDER TO &lcSvORd
        
        ENDIF
      ENDIF  && STYLE->MAKE

      XWorP1 = XWIP1
      XWorP2 = XWIP2
      XWorP3 = XWIP3
      XWorP4 = XWIP4
      
    ENDIF    
        
    =lfUpdWorPn()    

    SELECT STYLE
    XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
  ENDSCAN

ENDIF

*-----------------------------
* $$$ PRINT REPORT
*-----------------------------

A = PADR(ALLTRIM(laMajSeg[lnCPos, 5]),18) + 'PIECES     AMOUNT       '+;
                                            'PIECES     AMOUNT       '+;
                                            'PIECES     AMOUNT       '+;
                                            'PIECES     AMOUNT      '+;
                                            'PIECES      AMOUNT'
SELECT &ORDTEMP
SET RELATION TO STYLE INTO STYLE

LOCATE
IF EOF()
  llNoRecs = .T.
  return
ENDIF

XBREAK = STYLE
PAGENO = 0
ROW    = 99


STORE 0    TO YPIECE1,YPIECE2,YPIECE3,YPIECE4,YPIECE
STORE 0    TO NPIECE1,NPIECE2,NPIECE3,NPIECE4,NPIECE
STORE 0.00 TO YAMT1,YAMT2,YAMT3,YAMT4,YAMT
STORE 0.00 TO NAMT1,NAMT2,NAMT3,NAMT4,NAMT


R_WIDTH = 'W'
R_TITLE = 'DELIVERY SCHEDULE'
XREPORT = 'SODELVSC'

SELECT &ORDTEMP
llRecExst = .F.
SCAN
  REPLACE TOTORD  WITH O_ORD1+H_ORD1+O_ORD2+H_ORD2+O_ORD3+H_ORD3+O_ORD4+H_ORD4
  REPLACE O_TOTORDA WITH O_ORDAMT1+O_ORDAMT2+O_ORDAMT3+O_ORDAMT4
  REPLACE H_TOTORDA WITH H_ORDAMT1+H_ORDAMT2+H_ORDAMT3+H_ORDAMT4
  REPLACE TOTWIP  WITH WIP1+WIP2+WIP3+WIP4

  REPLACE STK1 WITH  TOTSTK
  REPLACE STK2 WITH  STK1 + WIP1 - (O_ORD1+H_ORD1)
  REPLACE STK3 WITH  STK2 + WIP2 - (O_ORD2+H_ORD2)
  REPLACE STK4 WITH  STK3 + WIP3 - (O_ORD3+H_ORD3)
    
  IF STK2<0
    STK2=0
  ENDIF
  IF STK3<0
    STK3=0
  ENDIF
  IF STK4<0
    STK4=0
  ENDIF
  FOR I=1 TO 4
    Z=STR(I,1)
    AVL&Z = STK&Z + WIP&Z
    IF AVL&Z >= ( H_ORD&Z + O_ORD&Z )
      REPLACE PYSHIP&Z WITH ( H_ORD&Z + O_ORD&Z )
      REPLACE PNSHIP&Z WITH 0
    ELSE   &&-AVAILABEL < ORDERS
      REPLACE PYSHIP&Z WITH AVL&Z
      REPLACE PNSHIP&Z WITH AVL&Z - ( H_ORD&Z + O_ORD&Z )
    ENDIF
  ENDFOR
  IF TOTSTK+TOTORD+TOTWIP = 0
    LOOP
  ELSE
    llRecExst = .T.
  ENDIF
ENDSCAN

SELECT &ORDTEMP
LOCATE

IF XBYCOLOR <> 'Y'
  llFrstTme=.T.
  DO WHILE .T.
    lcStyle = Style
    XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
    SEEK lcStyle
    SUM REST WHILE Style = lcStyle TO ARRAY laFields
    SELECT &WorkTemp
    IF llFrstTme
      =ACOPY(laFields,laTemp)
      DIME LAFIELDS[ALEN(laFields,1)+2]
      llFrstTme=.F.
    ENDIF
    =AINS(laFields,1)
    =AINS(laFields,1)
    laFields[1]=lcStyle
    laFields[2]=SPACE(6)
    APPEND BLANK
    GATHER FROM laFields
    SELECT &ORDTEMP
    IF EOF()
      EXIT
    ENDIF
  ENDDO
ENDIF

Work=IIF(XBYCOLOR = 'Y',ORDTEMP,WORKTEMP)
SELECT &Work

LOCATE

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

DO WHILE .T. &&INKEY() <>32

  IF EOF()
    EXIT
  ENDIF

  ORDO = O_ORD1 + O_ORD2 + O_ORD3 + O_ORD4    &&-TOTAL PIECES OPEN
  ORDH = H_ORD1 + H_ORD2 + H_ORD3 + H_ORD4    &&-TOTAL PIECES HOLD
  XCOLOR  = COLOR
  AVGPRICE=0.00
  IF TOTORD<>0
    AVGPRICE = ROUND( (O_TOTORDA+H_TOTORDA)/TOTORD,2)
  ENDIF

  =IIF(ROW>=53,lfPrnHdr(),.F.)
  
  REPLACE STK1 WITH  TOTSTK
  REPLACE STK2 WITH  STK1 + WIP1 - (O_ORD1+H_ORD1)
  REPLACE STK3 WITH  STK2 + WIP2 - (O_ORD2+H_ORD2)
  REPLACE STK4 WITH  STK3 + WIP3 - (O_ORD3+H_ORD3)

  IF STK2<0
    STK2=0
  ENDIF
  IF STK3<0
    STK3=0
  ENDIF
  IF STK4<0
    STK4=0
  ENDIF
  ORD1 = O_ORD1 + H_ORD1
  ORD2 = O_ORD2 + H_ORD2
  ORD3 = O_ORD3 + H_ORD3
  ORD4 = O_ORD4 + H_ORD4

  IF STYLE <> XBREAK
    XBREAK = STYLE
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW+1

    =IIF(ROW>=53,lfPrnHdr(),.F.)

    @ ROW,00  SAY 'STYLE: '
    @ ROW,07  SAY XBREAK
    @ ROW,25  SAY ' ' + STYLE->DESC                        &&TAK02/28/94
    @ ROW,60  SAY ' ' + 'AVG PRICE: '                      &&TAK02/28/94
    @ ROW,72  SAY AVGPRICE       PICTURE '99999.99'  &&TAK02/28/94
    ROW=ROW+2

    =IIF(ROW>=53,lfPrnHdr(),.F.)

  ENDIF

  IF XBYCOLOR='Y'

    @ ROW,000 SAY XCOLOR      && HDM   COLOR
  ENDIF

  @ ROW,000 SAY '(+)STOCK'
  @ ROW,012 SAY STK1        PICTURE '99999999'
  @ ROW,036 SAY STK2        PICTURE '99999999'
  @ ROW,060 SAY STK3        PICTURE '99999999'
  @ ROW,084 SAY STK4        PICTURE '99999999'
  @ ROW,110 SAY TOTSTK      PICTURE '99999999'
  ROW=ROW+1
  
  DO CASE
    CASE lcRpBsPrjn = 'W'
      lcWIPorPln = '(+)WIP'
    CASE lcRpBsPrjn = 'P'
      lcWIPorPln = '(+)PLAN'
    CASE lcRpBsPrjn = 'A'
      lcWIPorPln = '(+)WIP/PLAN'
  ENDCASE      

  @ ROW,000 SAY lcWIPorPln
  @ ROW,012 SAY WIP1        PICTURE '99999999'
  @ ROW,036 SAY WIP2        PICTURE '99999999'
  @ ROW,060 SAY WIP3        PICTURE '99999999'
  @ ROW,084 SAY WIP4        PICTURE '99999999'
  @ ROW,110 SAY TOTWIP      PICTURE '99999999'
  ROW=ROW+1


  AVL1 = STK1 + WIP1
  AVL2 = STK2 + WIP2
  AVL3 = STK3 + WIP3
  AVL4 = STK4 + WIP4
  AVLTOT = STK1+WIP1+WIP2+WIP3+WIP4

  @ ROW,000 SAY 'AVAILABLE'
  @ ROW,012 SAY AVL1        PICTURE '99999999'
  @ ROW,036 SAY AVL2        PICTURE '99999999'
  @ ROW,060 SAY AVL3        PICTURE '99999999'
  @ ROW,084 SAY AVL4        PICTURE '99999999'
  @ ROW,110 SAY AVLTOT      PICTURE '99999999'
  ROW=ROW+1

  @ ROW,000 SAY '(-)ORDERS:'
  ROW=ROW+1
  @ ROW,002 SAY 'OPEN'
  @ ROW,012 SAY O_ORD1        PICTURE '99999999'
  @ ROW,021 SAY O_ORDAMT1     PICTURE '9999999999999'
  @ ROW,036 SAY O_ORD2        PICTURE '99999999'
  @ ROW,045 SAY O_ORDAMT2     PICTURE '9999999999999'
  @ ROW,060 SAY O_ORD3        PICTURE '99999999'
  @ ROW,069 SAY O_ORDAMT3     PICTURE '9999999999999'
  @ ROW,084 SAY O_ORD4        PICTURE '99999999'
  @ ROW,093 SAY O_ORDAMT4     PICTURE '9999999999999'
  @ ROW,110 SAY ORDO         PICTURE '99999999'
  @ ROW,117 SAY O_TOTORDA      PICTURE '9999999999999'
  ROW=ROW+1
  =IIF(ROW>=53,lfPrnHdr(),.F.)

  @ ROW,002 SAY 'HOLD'
  @ ROW,012 SAY H_ORD1        PICTURE '99999999'
  @ ROW,021 SAY H_ORDAMT1     PICTURE '9999999999999'
  @ ROW,036 SAY H_ORD2        PICTURE '99999999'
  @ ROW,045 SAY H_ORDAMT2     PICTURE '9999999999999'
  @ ROW,060 SAY H_ORD3        PICTURE '99999999'
  @ ROW,069 SAY H_ORDAMT3     PICTURE '9999999999999'
  @ ROW,084 SAY H_ORD4        PICTURE '99999999'
  @ ROW,093 SAY H_ORDAMT4     PICTURE '9999999999999'
  @ ROW,110 SAY ORDH         PICTURE '99999999'
  @ ROW,117 SAY H_TOTORDA      PICTURE '9999999999999'
  ROW=ROW+1
  
  =IIF(ROW>=53,lfPrnHdr(),.F.)

  @ ROW,013 SAY '----------------------'
  @ ROW,037 SAY '----------------------'
  @ ROW,061 SAY '----------------------'
  @ ROW,085 SAY '----------------------'
  @ ROW,110 SAY '----------------------'
  ROW=ROW+1

  =IIF(ROW>=53,lfPrnHdr(),.F.)

  *-----------------------------------------
  * CAN/CAN'T SHIP TOTALS
  *-----------------------------------------
  
  *CAN/CAN'T SHIP amounts depends on fields on file.
  AYSHIP1 = ROUND(PYSHIP1 * AVGPRICE,0)
  ANSHIP1 = ROUND(PNSHIP1 * AVGPRICE,0)
  AYSHIP2 = ROUND(PYSHIP2 * AVGPRICE,0)
  ANSHIP2 = ROUND(PNSHIP2 * AVGPRICE,0)
  AYSHIP3 = ROUND(PYSHIP3 * AVGPRICE,0)
  ANSHIP3 = ROUND(PNSHIP3 * AVGPRICE,0)
  AYSHIP4 = ROUND(PYSHIP4 * AVGPRICE,0)
  ANSHIP4 = ROUND(PNSHIP4 * AVGPRICE,0)

  PYTOT = PYSHIP1 + PYSHIP2 + PYSHIP3 + PYSHIP4
  AYTOT = AYSHIP1 + AYSHIP2 + AYSHIP3 + AYSHIP4
  PNTOT = PNSHIP1 + PNSHIP2 + PNSHIP3 + PNSHIP4
  ANTOT = ANSHIP1 + ANSHIP2 + ANSHIP3 + ANSHIP4

  @ ROW,000 SAY 'CAN   SHIP'
  @ ROW,012 SAY PYSHIP1     PICTURE '99999999'
  @ ROW,021 SAY AYSHIP1     PICTURE '9999999999999'
  @ ROW,036 SAY PYSHIP2     PICTURE '99999999'
  @ ROW,045 SAY AYSHIP2     PICTURE '9999999999999'
  @ ROW,060 SAY PYSHIP3     PICTURE '99999999'
  @ ROW,069 SAY AYSHIP3     PICTURE '9999999999999'
  @ ROW,084 SAY PYSHIP4     PICTURE '99999999'
  @ ROW,093 SAY AYSHIP4     PICTURE '9999999999999'
  @ ROW,110 SAY PYTOT       PICTURE '99999999'
  @ ROW,117 SAY AYTOT       PICTURE '9999999999999'

  ROW=ROW+1

  =IIF(ROW>=53,lfPrnHdr(),.F.)

  @ ROW,000 SAY "CAN'T SHIP"
  @ ROW,012 SAY PNSHIP1     PICTURE '99999999'
  @ ROW,021 SAY ANSHIP1     PICTURE '9999999999999'
  @ ROW,036 SAY PNSHIP2     PICTURE '99999999'
  @ ROW,045 SAY ANSHIP2     PICTURE '9999999999999'
  @ ROW,060 SAY PNSHIP3     PICTURE '99999999'
  @ ROW,069 SAY ANSHIP3     PICTURE '9999999999999'
  @ ROW,084 SAY PNSHIP4     PICTURE '99999999'
  @ ROW,093 SAY ANSHIP4     PICTURE '9999999999999'
  @ ROW,110 SAY PNTOT       PICTURE '99999999'
  @ ROW,117 SAY ANTOT       PICTURE '9999999999999'

  ROW=ROW+1

  =IIF(ROW>=53,lfPrnHdr(),.F.)

  OTS1 = MAX(AVL1 - (O_ORD1+H_ORD1), 0)
  OTS2 = MAX(AVL2 - (O_ORD2+H_ORD2), 0)
  OTS3 = MAX(AVL3 - (O_ORD3+H_ORD3), 0)
  OTS4 = MAX(AVL4 - (O_ORD4+H_ORD4), 0)
  
  @ ROW,000 SAY "OPN-TO-SEL"
  @ ROW,012 SAY OTS1        PICTURE '99999999'
  @ ROW,036 SAY OTS2        PICTURE '99999999'
  @ ROW,060 SAY OTS3        PICTURE '99999999'
  @ ROW,084 SAY OTS4        PICTURE '99999999'
  ROW = ROW+2

  =IIF(ROW>=53,lfPrnHdr(),.F.)

  *-------------------------------
  * CUM GRAND TOTALS
  *-------------------------------
  
  YPIECE1 = YPIECE1 + PYSHIP1
  YPIECE2 = YPIECE2 + PYSHIP2
  YPIECE3 = YPIECE3 + PYSHIP3
  YPIECE4 = YPIECE4 + PYSHIP4

  NPIECE1 = NPIECE1 + PNSHIP1
  NPIECE2 = NPIECE2 + PNSHIP2
  NPIECE3 = NPIECE3 + PNSHIP3
  NPIECE4 = NPIECE4 + PNSHIP4

  YAMT1   = YAMT1   + AYSHIP1
  YAMT2   = YAMT2   + AYSHIP2
  YAMT3   = YAMT3   + AYSHIP3
  YAMT4   = YAMT4   + AYSHIP4

  NAMT1   = NAMT1   + ANSHIP1
  NAMT2   = NAMT2   + ANSHIP2
  NAMT3   = NAMT3   + ANSHIP3
  NAMT4   = NAMT4   + ANSHIP4

  SELECT &Work

  SKIP
ENDDO

*----------------------
* PRINT GRAND TOTALS
*----------------------
ROW=ROW+2
@ ROW,00 SAY REPLICATE("=", 132)
ROW = ROW+1
=IIF(ROW>=53,lfPrnHdr(),.F.)

YPIECE = YPIECE1+YPIECE2+YPIECE3+YPIECE4
NPIECE = NPIECE1+NPIECE2+NPIECE3+NPIECE4
YAMT   = YAMT1+YAMT2+YAMT3+YAMT4
NAMT   = NAMT1+NAMT2+NAMT3+NAMT4

@ ROW,000 SAY '*'
@ ROW,002 SAY 'CAN   SHIP'
@ ROW,012 SAY YPIECE1     PICTURE '99999999'
@ ROW,021 SAY YAMT1       PICTURE '99999999'
@ ROW,036 SAY YPIECE2     PICTURE '99999999'
@ ROW,045 SAY YAMT2       PICTURE '99999999'
@ ROW,060 SAY YPIECE3     PICTURE '99999999'
@ ROW,069 SAY YAMT3       PICTURE '99999999'
@ ROW,084 SAY YPIECE4     PICTURE '99999999'
@ ROW,093 SAY YAMT4       PICTURE '99999999'
@ ROW,110 SAY YPIECE      PICTURE '99999999'
@ ROW,117 SAY YAMT        PICTURE '99999999'
ROW=ROW+1

=IIF(ROW>=53,lfPrnHdr(),.F.)

@ ROW,000 SAY '*'
@ ROW,002 SAY "CAN'T SHIP"
@ ROW,012 SAY NPIECE1     PICTURE '99999999'
@ ROW,021 SAY NAMT1       PICTURE '99999999'
@ ROW,036 SAY NPIECE2     PICTURE '99999999'
@ ROW,045 SAY NAMT2       PICTURE '99999999'
@ ROW,060 SAY NPIECE3     PICTURE '99999999'
@ ROW,069 SAY NAMT3       PICTURE '99999999'
@ ROW,084 SAY NPIECE4     PICTURE '99999999'
@ ROW,093 SAY NAMT4       PICTURE '99999999'
@ ROW,110 SAY NPIECE      PICTURE '99999999'
@ ROW,117 SAY NAMT        PICTURE '99999999'

ROW=ROW+1


*:**************************************************************************
*:* Name        : lfPrnHdr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPrnHdr()
*:***************************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO+1

lcType = IIF(lcBuckBy = "S",'By Start Date','By Completion Date')
DO RPT_HDR WITH XREPORT,lcType,R_WIDTH

@ 05,00  SAY 'STYLE: '
@ 05,07  SAY STYLE
@ 05,25  SAY ' '+STYLE->DESC
@ 05,60  SAY 'AVG PRICE: '
@ 05,72  SAY AVGPRICE     PICTURE '99999.99'

DIMENSION laWeeks[4,2]
laWeeks = ''
=lfGetWeek()
@ 07,23  SAY laWeeks[1,1]+laWeeks[2,1]+laWeeks[3,1]+laWeeks[4,1]
@ 08,18  SAY laWeeks[1,2]+laWeeks[2,2]+laWeeks[3,2]+laWeeks[4,2]+'TOTAL'
@ 09,00  SAY A
ROW=10

*-----------------------
*   END ORD940.PRG
*-----------------------
*:**************************************************************************
*:* Name        : lfAdjSeg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*! Purpose   : Get the style code segments information.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAdjSeg()
*:***************************************************************************
FUNCTION lfAdjSeg

STORE 0 TO lnFPos , lnDPos , lnZPos   , lnGPos , lnCPos , lnOPos , lnTPos , ;
           lnQPos , lnSPos , lnMajPos
STORE 0 TO lnMajLen
STORE 1 TO lnMajPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.


DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
*--Get Major Length

FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it,
  *-- display the style major instead (style major will browse from the
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1
    LOOP
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'F'
      *-- If there are more than one "FREE" segment , get first one only
      lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style'
      ENDIF
    CASE laMajSeg[lnC,1] = 'D'
      lnDPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Division'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Z'
      lnZPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Season'
      ENDIF
    CASE laMajSeg[lnC,1] = 'G'
      lnGPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style Group'
      ENDIF
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Color'
      ENDIF
    CASE laMajSeg[lnC,1] = 'O'
      *-- If there are more than one "OTHER" segment , get first one only
      lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Other'
      ENDIF
    CASE laMajSeg[lnC,1] = 'T'
      lnTPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Make'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Q'
      lnQPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Quality'
      ENDIF
    CASE laMajSeg[lnC,1] = 'S'
      lnSPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Scale'
      ENDIF
  ENDCASE

ENDFOR

*:**************************************************************************
*:* Name        : lfSRVFab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*! Purpose   : control browsing primary fabric and validate
*!           : selecting it in inlist function.
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSRVFab()
*:***************************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    LOCATE
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*:**************************************************************************
*:* Name        : lfsrvSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*! Purpose   : To set relation on or off when running the in range function
*!             in the option grid.
*!*************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfsrvSty()
*:***************************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

*:**************************************************************************
*:* Name        : lfFabSum
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*! Purpose   : sum a specific field for the current fabric in fabric file
*! Called from : Option Grid,fabric browse calculated fields.
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFabSum()
*:***************************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
PROCEDURE LDOM

PARAMETER XDATE
PRIVATE X                       && ADDED BY FAR ON 07/28/93

XNEXTM   = IIF( MONTH(XDATE) = 12 , 1, MONTH(XDATE) + 1 )

FOR X=1 TO 32
  XDATE = XDATE + 1
  IF MONTH(XDATE) = XNEXTM
    EXIT
  ENDIF

ENDFOR
XDATE = XDATE - 1

RETURN(XDATE)

*:**************************************************************************
*:* Name        : lfvEdiOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvEdiOrd()
*:***************************************************************************
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
*-- end of lfvEdiOrd.

*:**************************************************************************
*:* Name        : lfvClear
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvClear()
*:***************************************************************************
FUNCTION lfvClear
CLEAR READ
*-- end of lfvClear.


*:**************************************************************************
*:* Name        : lfUpdWorPn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/29/02
*:* Purpose     : Calc Plan qty's and update the temp file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdWorPn()
*:***************************************************************************
FUNCTION lfUpdWorPn
PRIVATE XPLAN1,XPLAN2,XPLAN3,XPLAN4,lcSty

IF lcRpBsPrjn $ 'PA'
  STORE 0 TO XPLAN1,XPLAN2,XPLAN3,XPLAN4
  IF lcRpByClr = 'Y'
    lcSty = XSTYLE
  ELSE
    lcSty = SUBSTR(XSTYLE,1,lnMajLen)
  ENDIF
      
  SELECT FORCAST
  *-- Key :  STYLE+STR(NYEAR,4)+STR(NWEEK,2)
  FOR lnX = 1 TO 4
    lcX = STR(lnX,1)
    lcYear = STR(YEAR(XDATE&lcX),4)
    ld1stDay = CTOD('1/1/'+lcYear)
    ld1stDay = ld1stDay - DOW(ld1stDay) 
    lnWeek =  (XDATE&lcX-DOW(XDATE&lcX) - ld1stDay)/7 + 1
    IF lcRpByClr = 'Y'
      IF SEEK(lcSty+lcYear+STR(lnWeek,2),'FORCAST')
        XPLAN&lcX = FORCAST.NFORTOTQTY
      ENDIF
    ELSE
      =SEEK(lcSty,'FORCAST')
      SCAN REST WHILE STYLE+STR(NYEAR,4)+STR(NWEEK,2) = lcSty ;
                FOR STR(NYEAR,4)+STR(NWEEK,2)=lcYear+STR(lnWeek,2)
        XPLAN&lcX = XPLAN&lcX + FORCAST.NFORTOTQTY
      ENDSCAN          
    ENDIF
  ENDFOR          
  *--
  XWorP1 = XPLAN1
  XWorP2 = XPLAN2
  XWorP3 = XPLAN3
  XWorP4 = XPLAN4
            
ENDIF
    
IF lcRpBsPrjn = 'A'
  XWorP1 = MAX( XWIP1 , XPLAN1 )
  XWorP2 = MAX( XWIP2 , XPLAN2 )
  XWorP3 = MAX( XWIP3 , XPLAN3 )
  XWorP4 = MAX( XWIP4 , XPLAN4 )
ENDIF
   
SELECT &ORDTEMP
REPLACE WIP1 WITH WIP1+XWorP1,;
        WIP2 WITH WIP2+XWorP2,;
        WIP3 WITH WIP3+XWorP3,;
        WIP4 WITH WIP4+XWorP4

*-- end of lfUpdWorPn.

*:**************************************************************************
*:* Name        : lfGetWeek
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/29/2002
*:* Purpose     : Fill laWeeks array
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetWeek()
*:***************************************************************************
FUNCTION lfGetWeek
PRIVATE lcYear,ld1stDay,lnWeek,lnI,lcI
FOR lnI = 1 TO 4
  lcI = STR(lnI,1)
  lcYear = STR(YEAR(XDATE&lcI),4)
  ld1stDay = CTOD('1/1/'+lcYear)
  ld1stDay = ld1stDay - DOW(ld1stDay) 
  lnWeek =  (XDATE&lcI-DOW(XDATE&lcI) - ld1stDay)/7 + 1
  Day1 = XDATE&lcI - DOW(XDATE&lcI) + 1
  laWeeks[lnI,1] =  'Week ' + STR(lnWeek,2) + SPACE(17)
  laWeeks[lnI,2] =  DTOC(Day1) + '-' + DTOC(Day1+6) + SPACE(7)
ENDFOR
*-- end of lfGetWeek.