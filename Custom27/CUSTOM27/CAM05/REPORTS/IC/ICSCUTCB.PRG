*:***************************************************************************
*: Program file  : ICSCUTCB.PRG
*: Program desc. : CUSTOMIZED SPECIAL CUT & SOLD FOR COMBIO.
*! Date          : 29/05/2002
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102614
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSCUTCB
*:***************************************************************************
*: Modifications :
*: B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.
*: B606533,1 BWA 10/13/2002 Fix the bug of not printing all the dates bucket data.
*:***************************************************************************

*-- lcRepNmTtl variable that hold the header of the non major segment for the frx
*-- llDontPrn  variable that showes that no records match report criteria
*-- PAGENO     variable that hold Page No.
*-- ROW        variable that hold Row No.
*-- XTIME      variable that hold time
*-- R_WIDTH    variable that hold report width
*-- R_TITLE    variable that hold report title
*-- WORKTEMP   variable that hold the name of work temp. file
*-- CUTTTEMP   variable that hold the name of cuttkt temp. file
*-- POTEMP     variable that hold the name of PO temp. file
*-- ORDTEMP    variable that hold the name of ORDHDR temp. file
*-- XPRTWIP    variable that showes print WIP Summary or Detail
*-- XBYSIZE    variable that showes print by size
*-- XSLDAT1    variable that hold from sales order comp. date
*-- XSHDAT1    variable that hold to   sales order comp. date
*-- XPLDAT1    variable that hold from Prod. order comp. date
*-- XPHDAT1    variable that hold to   Prod. order comp. date
*-- XWIPSORT   variable that showes if it is sorted by WIP or not
*-- XPRTORD    variable that showes print Sales Summary or Detail
*-- XORDSORT   variable that hold the sales order Sort
*-- XSTATHLD   variable that showes if include Orders on hold
*-- XALLOCAT   variable that showes if include Allocated Qty. or All Qty.
*-- XTITLE     variable that hold optional title
*-- XFILTER    variable that hold report filter expression

llDontPrn = .F.
PAGENO    = 0
ROW       = 0
XTIME     = TIME()
R_WIDTH   = ''
R_TITLE   = ''

STORE SPACE(0) TO WORKTEMP , CUTTTEMP , POTEMP  , ORDTEMP , XPRTWIP ,;
                  XWIPRPT  , XPRTORD  , XORDRPT , XORDRPT , XSTAT

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
XBYSIZE    = IIF(llRPBySize,'Y','N')
XSLDAT1    = ldRPSLDate
XSHDAT1    = ldRPSHDate
XPLDAT1    = ldRPPLDate
XPHDAT1    = ldRPPHDate
XPRTWIP    = lcRPWIPSta
XWIPSORT   = lcRPWIPSor
XPRTORD    = lcRPSalSta
XORDSORT   = lcRPSALSor
XSTATHLD   = IIF(llRPHolOrd,'Y','N')
XALLOCAT   = lcRPAllQty
XTITLE     = lcRPTitle

*--The new variables to hold the other 3 dates.
XSHDatC = IIF(XSHDAT1 = CTOD('  /  /    ') , XSLDAT1 , XSHDAT1)
XPHDATC = IIF(XPHDAT1 = CTOD('  /  /    ') , XPLDAT1 , XPHDAT1)

*--SO DATES
XSLDAT2 = ldRPSLDat2
XSHDAT2 = ldRPSHDat2
XSHDatC = IIF(XSHDAT2 = CTOD('  /  /    ') , IIF(XSLDAT2 = CTOD('  /  /    ')  , XSHDatC , XSLDAT2), XSHDAT2)

XSLDAT3 = ldRPSLDat3
XSHDAT3 = ldRPSHDat3
XSHDatC = IIF(XSHDAT3 = CTOD('  /  /    ') , IIF(XSLDAT3 = CTOD('  /  /    ')  , XSHDatC , XSLDAT3), XSHDAT3)

XSLDAT4 = ldRPSLDat4
XSHDAT4 = ldRPSHDat4
XSHDatC = IIF(XSHDAT4 = CTOD('  /  /    ') , IIF(XSLDAT4 = CTOD('  /  /    ')  , XSHDatC , XSLDAT4), XSHDAT4)

*--PO DATES
XPLDAT2  = ldRPPLDat2
XPHDAT2  = ldRPPHDat2
XPHDatC  = IIF(XPHDAT2 = CTOD('  /  /    ') , IIF(XPLDAT2 = CTOD('  /  /    ')  , XPHDatC , XPLDAT2), XPHDAT2)

XPLDAT3  = ldRPPLDat3
XPHDAT3  = ldRPPHDat3
XPHDatC  = IIF(XPHDAT3 = CTOD('  /  /    ') , IIF(XPLDAT3 = CTOD('  /  /    ')  , XPHDatC , XPLDAT3), XPHDAT3)

XPLDAT4  = ldRPPLDat4
XPHDAT4  = ldRPPHDat4
XPHDatC  = IIF(XPHDAT4 = CTOD('  /  /    ') , IIF(XPLDAT4 = CTOD('  /  /    ')  , XPHDatC , XPLDAT4), XPHDAT4)

*--Check the fields of the SO date if it's empty or not.
lnDatCount = 0
FOR lnLop1 = 1 TO 4
  lnTime = ALLTRIM(STR(lnLop1))

  *B606533,1 BWA 10/13/2002 Fix the bug of not printing all the dates bucket data.[START]
  *lnDatCount = lnDatCount + IIF( EMPTY(XSLDAT&lnTime) OR EMPTY(XSHDAT&lnTime) , 0 , 1 )
  lnDatCount = lnDatCount + IIF( EMPTY(XSLDAT&lnTime) AND EMPTY(XSHDAT&lnTime) , 0 , 1 )
  *B606533,1 BWA 10/13/2002.[END]

ENDFOR

*--Check the fields of the PO date if it's empty or not.
lnDatContP = 0
FOR lnLop2 = 1 TO 4
  lnTime = ALLTRIM(STR(lnLop2))

  *B606533,1 BWA 10/13/2002 Fix the bug of not printing all the dates bucket data.[START]
  *lnDatContP = lnDatContP + IIF( EMPTY(XPLDAT&lnTime) OR EMPTY(XPHDAT&lnTime) , 0 , 1 )
  lnDatContP = lnDatContP + IIF( EMPTY(XPLDAT&lnTime) AND EMPTY(XPHDAT&lnTime) , 0 , 1 )
  *B606533,1 BWA 10/13/2002.[END]
ENDFOR

*--Variable hold the total maximum number of the dates entered.
IF (lnDatCount > 0 AND lnDatContP > 0 ) OR (lnDatCount # 0 OR lnDatContP # 0)
  lnTowDat = IIF( lnDatCount > lnDatContP ,  lnDatCount , lnDatContP )
ELSE
  IF lnDatCount = 0 AND lnDatContP = 0
    lnTowDat = 0
  ENDIF
ENDIF

PRIVATE lcCustAcnt
STORE "" TO lcCustAcnt
llMore24  = .F.
lnDatPost = 0

lnDataPos = ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
  IF AT('INLIST(CUSTOMER.TYPE',LCRPEXP) > 0
    lnDatPost = AT('INLIST(CUSTOMER.TYPE',LCRPEXP)
  ELSE
    IF AT('BETWEEN(CUSTOMER.TYPE',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(CUSTOMER.TYPE',LCRPEXP)
      llMore24 = .T.
    ENDIF
  ENDIF

  IF lnDatPost > 0
    IF llMore24
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost) , 2)
    ELSE
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost))
    ENDIF

    IF lnPos1 > 0
      lcCustAcnt = SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
      lcRPExp    = STRTRAN(lcRPExp, lcCustAcnt , " .T. ")
      lcCustAcnt = "AND " + lcCustAcnt
    ELSE
      lcCustAcnt = SUBSTR(lcRpExp ,lnDatPost)
      lcRPExp    = STRTRAN(lcRPExp, lcCustAcnt , " .T. )")
      lcCustAcnt = "AND (" + lcCustAcnt
    ENDIF
  ENDIF
ENDIF
XFILTER = lcRPExp

*--Section of collecting the data and printing.
WAIT WINDOW 'Collecting data....' NOWAIT
DO lpCollecData
WAIT CLEAR

IF !llDontPrn
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

*B606533,1 BWA 10/13/2002 Fix the bug of not printing all the dates bucket data.[START]
DIMENSION laFileName[4]
laFileName[1] = WORKTEMP
laFileName[2] = CUTTTEMP
laFileName[3] = POTEMP
laFileName[4] = ORDTEMP

*--Function of deleting the temprory files.
=lfBasToClr(@laFileName , 'A')
*B606533,1 BWA 10/13/2002.[END]

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : To get the style nonmajor segement structure.
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
*-- Compute Free/Color Items in Style Structure.

RETURN ''

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- Check the cost access
DIMENSION laRPPrnItm[14]
llCostAccs = gfUserPriv('IC','ICSTYLE')
lnClrSgPo  = ASUBSCRIPT(laOGVrFlt,;
             ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
lnFreSgPo  = ASUBSCRIPT(laOGVrFlt,;
             ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment.
DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')

  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')

  OTHERWISE
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
ENDCASE

*-- Disable/enable Only This colors, Free Segment.
=lfvWIPSuDt()
=lfvSalSuDt()

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Get major Segment picture.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfvSODate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Validate sales order date range.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSODate()
*!*************************************************************
FUNCTION lfvSODate
PARAMETERS lnCall

PRIVATE ldFrom,ldTo
STORE 0 TO lnSoDat1 , lnSoDat2 , lnSoDat3 , lnSoDat4
lnFrmSoPo = "SALES"

DO CASE
  CASE lnCall = '1'
    IF EMPTY(ldRPSLDate) .AND. EMPTY(ldRPSHDate)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPSLDate
      ldTo   = ldRPSHDate
    ENDIF
    lnSoDat1 = 1
    lcTitle  = 'Sales order comp. date No1'

  CASE lnCall = '2'
    IF EMPTY(ldRPSLDat2) .AND. EMPTY(ldRPSHDat2)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPSLDat2
      ldTo   = ldRPSHDat2
    ENDIF
    lnSoDat2 = 2
    lcTitle  = 'Sales order comp. date No2'

  CASE lnCall = '3'
    IF EMPTY(ldRPSLDat3) .AND. EMPTY(ldRPSHDat3)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPSLDat3
      ldTo   = ldRPSHDat3
    ENDIF
    lnSoDat3 = 3
    lcTitle  = 'Sales order comp. date No3'

  CASE lnCall = '4'
    IF EMPTY(ldRPSLDat4) .AND. EMPTY(ldRPSHDat4)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPSLDat4
      ldTo   = ldRPSHDat4
    ENDIF
    lnSoDat4 = 4
    lcTitle  = 'Sales order comp. date No4'

ENDCASE

*--Call the date screen.
DO DateRng.Spx

DO CASE
  CASE lnCall = '1'
    ldRPSLDate = ldFrom
    ldRPSHDate = ldTo

  CASE lnCall = '2'
    ldRPSLDat2 = ldFrom
    ldRPSHDat2 = ldTo

  CASE lnCall = '3'
    ldRPSLDat3 = ldFrom
    ldRPSHDat3 = ldTo

  CASE lnCall = '4'
    ldRPSLDat4 = ldFrom
    ldRPSHDat4 = ldTo

ENDCASE

*--End of lfvSODate.
*!*************************************************************
*! Name      : lfvPODate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Validate production order date range.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvPODate()
*!*************************************************************
FUNCTION lfvPODate
PARAMETERS lnPcall

PRIVATE ldFrom,ldTo
STORE 0 TO lnPoDat1 , lnPoDat2 , lnPoDat3 , lnPoDat4
lnFrmSoPo = "PARCHES"

DO CASE
  CASE lnPcall = '1'
    IF EMPTY(ldRPPLDate) .AND. EMPTY(ldRPPHDate)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPPLDate
      ldTo   = ldRPPHDate
    ENDIF
    lnPoDat1 = 1
    lcTitle  = 'Prod. order comp. date No1'

  CASE lnPcall = '2'
    IF EMPTY(ldRPPLDat2) .AND. EMPTY(ldRPPHDat2)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPPLDat2
      ldTo   = ldRPPHDat2
    ENDIF
    lnPoDat2 = 2
    lcTitle  = 'Prod. order comp. date No2'

  CASE lnPcall = '3'
    IF EMPTY(ldRPPLDat3) .AND. EMPTY(ldRPPHDat3)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPPLDat3
      ldTo   = ldRPPHDat3
    ENDIF
    lnPoDat3 = 3
    lcTitle  = 'Prod. order comp. date No3'

  CASE lnPcall = '4'
    IF EMPTY(ldRPPLDat4) .AND. EMPTY(ldRPPHDat4)
      STORE {} TO ldFrom,ldTo
    ELSE
      ldFrom = ldRPPLDat4
      ldTo   = ldRPPHDat4
    ENDIF
    lnPoDat4 = 4
    lcTitle  = 'Prod. order comp. date No4'

ENDCASE

*--Call the date screen.
DO DateRng.Spx

DO CASE
  CASE lnPcall = '1'
    ldRPPLDate = ldFrom
    ldRPPHDate = ldTo

  CASE lnPcall = '2'
    ldRPPLDat2 = ldFrom
    ldRPPHDat2 = ldTo

  CASE lnPcall = '3'
    ldRPPLDat3 = ldFrom
    ldRPPHDat3 = ldTo

  CASE lnPcall = '4'
    ldRPPLDat4 = ldFrom
    ldRPPHDat4 = ldTo

ENDCASE

*-End of lfvPODate.
*!*************************************************************
*! Name      : lfvpbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Validate OK button.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvpbOk()
*!*************************************************************
FUNCTION lfvpbOk
PRIVATE llinSec , llOutCod
llinSec  = .F.
llOutCod = .T.

*--Case the user select the PO dates.
IF lnFrmSoPo == "SALES"
  IF lnSoDat1 = 1
    STORE {} TO ldRPSLDat2 , ldRPSHDat2 , ldRPSLDat3 , ldRPSHDat3 , ldRPSLDat4 , ldRPSHDat4
  ENDIF

  IF lnSoDat2 > 0 OR lnSoDat3 > 0 OR lnSoDat4 > 0
    IF   (lnSoDat2 > 0 AND !EMPTY(ldFrom))                       ;
      OR (lnSoDat2 > 0 AND EMPTY(ldFrom) AND ldTO <= ldRPSHDate) ;
      OR (lnSoDat2 > 0 AND EMPTY(ldFrom) AND EMPTY(ldTO))        ;
      OR (lnSoDat2 > 0 AND EMPTY(ldFrom) AND ldTO > ldRPSHDate)

      IF EMPTY(ldFrom) AND ldTO > ldRPSHDate
        ldFrom = ldRPSHDate + 1
      ENDIF
      IF EMPTY(ldFrom) AND EMPTY(ldTO)
        llOutCod = .F.
      ENDIF
      IF EMPTY(ldFrom) AND ldTO <= ldRPSHDate AND llOutCod
        WAIT WINDOW "Sales order comp. date No2 TO must be greater than Sales order comp. date No1 TO ." NOWAIT
        _CUROBJ = OBJNUM(ldTo)
        RETURN
      ENDIF
      IF EMPTY(ldRPSHDate) AND llOutCod OR (ldFrom <= ldRPSHDate AND !EMPTY(ldFrom)) AND llOutCod
        IF EMPTY(ldRPSHDate)
          WAIT WINDOW "Sales order comp. date No1 must not be empty." NOWAIT
          STORE {} TO ldFrom , ldTo
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          RETURN
        ENDIF
        IF ldFrom <= ldRPSHDate
          WAIT WINDOW "Sales order comp. date No2 must be greater than Sales order comp. date No1 ." NOWAIT
          STORE {} TO ldFrom
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          llinSec = .T.
        ENDIF
      ELSE
        llinSec = .T.
        IF ldFrom > ldTo
          WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
          _CUROBJ = OBJNUM(ldFrom)
        ELSE
          CLEAR READ
        ENDIF
      ENDIF
    ENDIF

    IF   (lnSoDat3 > 0 AND !EMPTY(ldFrom))                       ;
      OR (lnSoDat3 > 0 AND EMPTY(ldFrom) AND ldTO <= ldRPSHDat2) ;
      OR (lnSoDat3 > 0 AND EMPTY(ldFrom) AND EMPTY(ldTO))        ;
      OR (lnSoDat3 > 0 AND EMPTY(ldFrom) AND ldTO > ldRPSHDat2)

      IF EMPTY(ldFrom) AND ldTO > ldRPSHDat2
        ldFrom = ldRPSHDat2 + 1
      ENDIF
      IF EMPTY(ldFrom) AND EMPTY(ldTO)
        llOutCod = .F.
      ENDIF
      IF EMPTY(ldFrom) AND ldTO <= ldRPSHDat2 AND llOutCod
        WAIT WINDOW "Sales order comp. date No3 TO must be greater than Sales order comp. date No2 TO ." NOWAIT
        _CUROBJ = OBJNUM(ldTo)
        RETURN
      ENDIF
      IF EMPTY(ldRPSHDat2) AND llOutCod OR (ldFrom <= ldRPSHDat2 AND !EMPTY(ldFrom)) AND llOutCod
        IF EMPTY(ldRPSHDat2)
          WAIT WINDOW "Sales order comp. date No2 must not be empty." NOWAIT
          STORE {} TO ldFrom , ldTo
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          RETURN
        ENDIF
        IF ldFrom <= ldRPSHDat2
          WAIT WINDOW "Sales order comp. date No3 must be greater than Sales order comp. date No2 ." NOWAIT
          STORE {} TO ldFrom
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          llinSec = .T.
        ENDIF
      ELSE
        llinSec = .T.
        IF ldFrom > ldTo
          WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
          _CUROBJ = OBJNUM(ldFrom)
        ELSE
          CLEAR READ
        ENDIF
      ENDIF
    ENDIF

    IF   (lnSoDat4 > 0 AND !EMPTY(ldFrom))                       ;
      OR (lnSoDat4 > 0 AND EMPTY(ldFrom) AND ldTO <= ldRPSHDat3) ;
      OR (lnSoDat4 > 0 AND EMPTY(ldFrom) AND EMPTY(ldTO))        ;
      OR (lnSoDat4 > 0 AND EMPTY(ldFrom) AND ldTO > ldRPSHDat3)

      IF EMPTY(ldFrom) AND ldTO > ldRPSHDat3
        ldFrom = ldRPSHDat3 + 1
      ENDIF
      IF EMPTY(ldFrom) AND EMPTY(ldTO)
        llOutCod = .F.
      ENDIF
      IF EMPTY(ldFrom) AND ldTO <= ldRPSHDat3 AND llOutCod
        WAIT WINDOW "Sales order comp. date No4 TO must be greater than Sales order comp. date No3 TO ." NOWAIT
        _CUROBJ = OBJNUM(ldTo)
        RETURN
      ENDIF
      IF EMPTY(ldRPSHDat3) AND llOutCod OR (ldFrom <= ldRPSHDat3 AND !EMPTY(ldFrom)) AND llOutCod
        IF EMPTY(ldRPSHDat3)
          WAIT WINDOW "Sales order comp. date No3 must not be empty." NOWAIT
          STORE {} TO ldFrom , ldTo
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          RETURN
        ENDIF
        IF ldFrom <= ldRPSHDat3
          WAIT WINDOW "Sales order comp. date No4 must be greater than Sales order comp. date No3 ." NOWAIT
          STORE {} TO ldFrom
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          llinSec = .T.
        ENDIF
      ELSE
        llinSec = .T.
        IF ldFrom > ldTo
          WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
          _CUROBJ = OBJNUM(ldFrom)
        ELSE
          CLEAR READ
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !llinSec
    IF ldFrom > ldTo
      WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
      _CUROBJ = OBJNUM(ldFrom)
    ELSE
      CLEAR READ
    ENDIF
  ENDIF
ENDIF

PRIVATE llinSec , llOutCod
llinSec = .F.
llOutCod = .T.

*--Case the user select the PO dates.
IF lnFrmSoPo == "PARCHES"
  IF lnPoDat1 = 1
    STORE {} TO ldRPPLDat2 , ldRPPHDat2 , ldRPPLDat3 , ldRPPHDat3 , ldRPPLDat4 , ldRPPHDat4
  ENDIF

  IF lnPoDat2 > 0 OR lnPoDat3 > 0 OR lnPoDat4 > 0
    IF   (lnPoDat2 > 0 AND !EMPTY(ldFrom))                       ;
      OR (lnPoDat2 > 0 AND EMPTY(ldFrom) AND ldTO <= ldRPPHDate) ;
      OR (lnPoDat2 > 0 AND EMPTY(ldFrom) AND EMPTY(ldTO))        ;
      OR (lnPoDat2 > 0 AND EMPTY(ldFrom) AND ldTO > ldRPPHDate)

      IF EMPTY(ldFrom) AND ldTO > ldRPPHDate
        ldFrom = ldRPPHDate + 1
      ENDIF
      IF EMPTY(ldFrom) AND EMPTY(ldTO)
        llOutCod = .F.
      ENDIF
      IF EMPTY(ldFrom) AND ldTO <= ldRPPHDate AND llOutCod
        WAIT WINDOW "Prod. order comp. date No2 TO must be greater than Prod. order comp. date No1 TO ." NOWAIT
        _CUROBJ = OBJNUM(ldTo)
        RETURN
      ENDIF
      IF EMPTY(ldRPPHDate) AND llOutCod OR (ldFrom <= ldRPPHDate AND !EMPTY(ldFrom)) AND llOutCod
        IF EMPTY(ldRPPHDate)
          WAIT WINDOW "Prod. order comp. date No1 must not be empty." NOWAIT
          STORE {} TO ldFrom , ldTo
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          RETURN
        ENDIF
        IF ldFrom <= ldRPPHDate
          WAIT WINDOW "Prod. order comp. date No2 must be greater than Prod. order comp. date No1 ." NOWAIT
          STORE {} TO ldFrom
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          llinSec = .T.
        ENDIF
      ELSE
        llinSec = .T.
        IF ldFrom > ldTo
          WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
          _CUROBJ = OBJNUM(ldFrom)
        ELSE
          CLEAR READ
        ENDIF
      ENDIF
    ENDIF

    IF   (lnPoDat3 > 0 AND !EMPTY(ldFrom))                       ;
      OR (lnPoDat3 > 0 AND EMPTY(ldFrom) AND ldTO <= ldRPPHDat2) ;
      OR (lnPoDat3 > 0 AND EMPTY(ldFrom) AND EMPTY(ldTO))        ;
      OR (lnPoDat3 > 0 AND EMPTY(ldFrom) AND ldTO > ldRPPHDat2)

      IF EMPTY(ldFrom) AND ldTO > ldRPPHDat2
        ldFrom = ldRPPHDat2 + 1
      ENDIF
      IF EMPTY(ldFrom) AND EMPTY(ldTO)
        llOutCod = .F.
      ENDIF
      IF EMPTY(ldFrom) AND ldTO <= ldRPPHDat2 AND llOutCod
        WAIT WINDOW "Prod. order comp. date No3 TO must be greater than Prod. order comp. date No2 TO ." NOWAIT
        _CUROBJ = OBJNUM(ldTo)
        RETURN
      ENDIF
      IF EMPTY(ldRPPHDat2) AND llOutCod OR (ldFrom <= ldRPPHDat2 AND !EMPTY(ldFrom)) AND llOutCod
        IF EMPTY(ldRPPHDat2)
          WAIT WINDOW "Prod. order comp. date No2 must not be empty." NOWAIT
          STORE {} TO ldFrom , ldTo
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          RETURN
        ENDIF
        IF ldFrom <= ldRPPHDat2
          WAIT WINDOW "Prod. order comp. date No3 must be greater than Prod. order comp. date No2 ." NOWAIT
          STORE {} TO ldFrom
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          llinSec = .T.
        ENDIF
      ELSE
        llinSec = .T.
        IF ldFrom > ldTo
          WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
          _CUROBJ = OBJNUM(ldFrom)
        ELSE
          CLEAR READ
        ENDIF
      ENDIF
    ENDIF

    IF   (lnPoDat4 > 0 AND !EMPTY(ldFrom))                       ;
      OR (lnPoDat4 > 0 AND EMPTY(ldFrom) AND ldTO <= ldRPPHDat3) ;
      OR (lnPoDat4 > 0 AND EMPTY(ldFrom) AND EMPTY(ldTO))        ;
      OR (lnPoDat4 > 0 AND EMPTY(ldFrom) AND ldTO > ldRPPHDat3)

      IF EMPTY(ldFrom) AND ldTO > ldRPPHDat3
        ldFrom = ldRPPHDat3 + 1
      ENDIF
      IF EMPTY(ldFrom) AND EMPTY(ldTO)
        llOutCod = .F.
      ENDIF
      IF EMPTY(ldFrom) AND ldTO <= ldRPPHDat3 AND llOutCod
        WAIT WINDOW "Prod. order comp. date No4 TO must be greater than Prod. order comp. date No3 TO ." NOWAIT
        _CUROBJ = OBJNUM(ldTo)
        RETURN
      ENDIF
      IF EMPTY(ldRPPHDat3) AND llOutCod OR (ldFrom <= ldRPPHDat3 AND !EMPTY(ldFrom)) AND llOutCod
        IF EMPTY(ldRPPHDat3)
          WAIT WINDOW "Prod. order comp. date No3 must not be empty." NOWAIT
          STORE {} TO ldFrom , ldTo
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          RETURN
        ENDIF
        IF ldFrom <= ldRPPHDat3
          WAIT WINDOW "Prod. order comp. date No4 must be greater than Prod. order comp. date No3 ." NOWAIT
          STORE {} TO ldFrom
          SHOW GETS
          _CUROBJ = OBJNUM(ldFrom)
          llinSec = .T.
        ENDIF
      ELSE
        llinSec = .T.
        IF ldFrom > ldTo
          WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
          _CUROBJ = OBJNUM(ldFrom)
        ELSE
          CLEAR READ
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !llinSec
    IF ldFrom > ldTo
      WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
      _CUROBJ = OBJNUM(ldFrom)
    ELSE
      CLEAR READ
    ENDIF
  ENDIF
ENDIF

*--End of lfvpbOk.
*!*************************************************************
*! Name      : lfvWIPSuDt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Validate print WIP Summary or Detail option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvWIPSuDt()
*!*************************************************************
FUNCTION lfvWIPSuDt

IF ('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  lcWIPSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPWIPSor'),1)
  laOGObjCnt[lcWIPSorPo] = lcRPWIPSta = 'D'
  = lfOGShowGet('lcRPWIPSor')
ENDIF

*--End of lfvWIPSuDt.
*!*************************************************************
*! Name      : lfvSalSuDt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Validate print Sales Summary or Detail option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSalSuDt()
*!*************************************************************
FUNCTION lfvSalSuDt

lcSalSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPSalSor'),1)
laOGObjCnt[lcSalSorPo] = lcRPSalSta = 'D'
= lfOGShowGet('lcRPSalSor')

*--End of lfvSalSuDt.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Validate Style.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag   = ORDER('STYLE')
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
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Validate Fabric.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

lcFabObj  = VARREAD()
lcFab     = &lcFabObj
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

*--End of lfvFabric.
*!*************************************************************
*! Name      : lpCollecData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Collecting data in Temp. File.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollecData
*!*************************************************************
PROCEDURE lpCollecData

IF XPRTWIP = 'D'
  XWIPRPT  = .T.
ELSE
  XWIPRPT  = .F.
ENDIF

IF XPRTORD = 'D'
  XORDRPT  = .T.
ELSE
  XORDRPT  = .F.
ENDIF

XSTAT = IIF(XSTATHLD = 'N','O','HO')

IF XSLDAT1 <> CTOD('  /  /    ')
  XSHDatC = XSHDatC
ENDIF

IF XPLDAT1 <> CTOD('  /  /    ')
  XPHDatC = XPHDatC
ENDIF

SELECT STYLE
SET FILTER TO
XFILTER = XFILTER + '.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'
SET FILTER TO &XFILTER
LOCATE

IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  llDontPrn = .T.
  RETURN
ENDIF

********  EXTRACT RECORDS FROM CUTTING TICKET, PO, AND ORDERS **************

WORKTEMP = gfTEMPNAME()
CUTTTEMP = gfTEMPNAME()
POTEMP   = gfTEMPNAME()
ORDTEMP  = gfTEMPNAME()

*-- If the "Manufacturing" module is installed
IF 'MF' $ gcComp_Mdl
  SELECT CUTTKTL
  SET ORDER TO CUTTKTLS
  SET RELATION TO CUTTKT INTO CUTTKTH
  COPY STRU TO &gcWorkDir.&CUTTTEMP
  USE &gcWorkDir.&CUTTTEMP IN 0 EXCLUSIVE
  SELECT (CUTTTEMP)
  SET RELATION TO CUTTKT INTO CUTTKTH

  SET ORDER TO TktOper IN MFGOPRHD
  SELECT CUTTKTH
  SET RELATION TO 'M' + CutTkt INTO MFGOPRHD

  SELECT (CUTTTEMP)
  DO CASE
    CASE XBYSIZE = 'N' .AND. XPRTWIP = 'S'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + CUTTKT + TRANCD + STR(RECNO(),7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XPRTWIP = 'S'
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO(),7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")

    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + DTOS(CUTTKTH.COMPLETE) + CUTTKT + TRANCD + STR(RECNO(),7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")

    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'F'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + MFGOPRHD.cContCode + CUTTKT + TRANCD + STR(RECNO(),7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")

    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + CUTTKT + TRANCD + STR(RECNO(),7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'D'
      INDEX ON STYLE + DTOS(CUTTKTH.COMPLETE) + CUTTKT + TRANCD + STR(RECNO(),7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'F'
      INDEX ON STYLE + MFGOPRHD.cContCode + CUTTKT + TRANCD + STR(RECNO() , 7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'P'
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO() , 7) TAG (CUTTTEMP) OF (gcWorkDir + CUTTTEMP + ".CDX")
  ENDCASE
ENDIF

*-- If the "Style Purchase Order" or the "Point of Sale" Modules is installed.
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  SELECT POSHDR
  SELECT POSLN
  SET RELATION TO cStyType + PO INTO POSHDR
  COPY STRU TO &gcWorkDir.&POTEMP
  USE &gcWorkDir.&POTEMP IN 0 EXCLUSIVE

  SELECT (POTEMP)
  SET RELATION TO cStyType + PO INTO POSHDR

  DO CASE
    CASE XBYSIZE = 'N' .AND. XPRTWIP = 'S'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + PO + TRANCD + STR(RECNO(),7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XPRTWIP = 'S'
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")

    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + DTOS(POSHDR.COMPLETE) + PO + TRANCD + STR(RECNO(),7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")

    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'F'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + VENDOR + PO + TRANCD + STR(RECNO(),7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")

    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen) + PO + TRANCD + STR(RECNO(),7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'D'
      INDEX ON STYLE + DTOS(POSHDR.COMPLETE) + PO + TRANCD + STR(RECNO() , 7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'F'
      INDEX ON STYLE + VENDOR + PO + TRANCD + STR(RECNO() , 7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")

    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'P'
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG (POTEMP) OF (gcWorkDir + POTEMP + ".CDX")
  ENDCASE
ENDIF

SELECT ORDLINE
SET RELATION TO cOrdType + Order INTO ORDHDR
COPY STRU TO &gcWorkDir.&ORDTEMP
USE &gcWorkDir.&ORDTEMP IN 0 EXCLUSIVE

SELECT (ORDTEMP)
SET RELATION TO cOrdType + Order INTO ORDHDR

DO CASE
  CASE XBYSIZE = 'N' .AND. XPRTORD = 'S'
    INDEX ON SUBSTR(STYLE,1,lnMajLen) + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO,6) TAG (ORDTEMP) OF (gcWorkDir + ORDTEMP + ".CDX")

  CASE XBYSIZE = 'Y' .AND. XPRTORD = 'S'
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO , 6) TAG (ORDTEMP)  OF (gcWorkDir + ORDTEMP + ".CDX")

  CASE XBYSIZE = 'N' .AND. XORDSORT = 'D'
    INDEX ON SUBSTR(STYLE,1,lnMajLen) + DTOS(COMPLETE) + ORDER + STORE + STR(LINENO,6) TAG (ORDTEMP)  OF (gcWorkDir + ORDTEMP + ".CDX")

  CASE XBYSIZE = 'N' .AND. XORDSORT = 'A'
    INDEX ON SUBSTR(STYLE,1,lnMajLen) + ACCOUNT + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO,6) TAG (ORDTEMP)  OF (gcWorkDir + ORDTEMP + ".CDX")

  CASE XBYSIZE = 'N' .AND. XORDSORT = 'O'
    INDEX ON SUBSTR(STYLE,1,lnMajLen) + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO,6) TAG (ORDTEMP)  OF (gcWorkDir + ORDTEMP + ".CDX")

  CASE XBYSIZE = 'Y' .AND. XORDSORT = 'D'
    INDEX ON STYLE + DTOS(COMPLETE) + ORDER + STORE + STR(LINENO , 6) TAG (ORDTEMP)  OF (gcWorkDir + ORDTEMP + ".CDX")

  CASE XBYSIZE = 'Y' .AND. XORDSORT = 'A'
    INDEX ON STYLE + ACCOUNT + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO , 6) TAG (ORDTEMP)  OF (gcWorkDir + ORDTEMP + ".CDX")

  CASE XBYSIZE = 'Y' .AND. XORDSORT = 'O'
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO , 6) TAG (ORDTEMP)  OF (gcWorkDir + ORDTEMP + ".CDX")
ENDCASE

*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to OrdLine according to the CutOrd index.
  =gfOpenFile(gcDataDir+'CutPick','CutOrd','SH')
  SELECT ORDLINE
  SET RELATION TO '2'+ ORDER + STR(LineNo,6) INTO CUTPICK ADDITIVE
ENDIF

*-- If the "Style Purchase Order" or the "Point of Sale" modules is installed.
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to PosLn according,
  *-- to the CutPick index in another work area.
  SELECT 0
  USE '&gcDataDir.CUTPICK' AGAIN ALIAS CUTPICK2
  SET ORDER TO CUTPICK
  SELECT POSLN
  SET RELATION TO '2' + PO + Style INTO CUTPICK2 ADDITIVE
ENDIF

*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to OrdLine according ,
  *-- to the CutOrd index in another work area to check if the order ,
  *-- was allocated by cuttkt not by PO.
  SELECT 0
  USE '&gcDataDir.CUTPICK' AGAIN ALIAS CUTPICK3
  SET ORDER TO CUTORD
  SELECT ORDLINE
  SET RELATION TO '1' + ORDER + STR(LineNo,6) INTO CUTPICK3 ADDITIVE
ENDIF

SELECT STYLE
LOCATE

XDESC  = SPACE(20)
XSTYLE = SPACE(19)
XCOLOR = SPACE(6)

*** SAVE OFF RECORDS INTO THER RESPECTIVE TEMPORARY FILES.
SCAN
  IF INKEY() = 32
    RETURN
  ENDIF

  xStyle = Style
  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl

    *** Store off Records Into First Temp File CUTTTEMP.
    SELECT CUTTKTL
    SEEK XSTYLE
    IF FOUND()
      IF XPLDAT1 <> CTOD('  /  /    ')
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE) = (XSTYLE) FOR (BETWEEN(CUTTKTH.COMPLETE , XPLDAT1 , XPHDatC) .AND.CUTTKTH.STATUS <> 'X' .AND. CUTTKTH.STATUS <> 'C' .AND. TOTQTY <> 0)
      ELSE
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE) = (XSTYLE) FOR (CUTTKTH.STATUS <> 'X' .AND. CUTTKTH.STATUS <> 'C' .AND. TOTQTY <> 0)
      ENDIF
      SELECT (CUTTTEMP)
      APPEND FROM &gcWorkDir.&WORKTEMP
    ENDIF
  ENDIF

  *-- If the " Style Purchase Order " or the "Point of Sale" Modules is installed.
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

    *** Store off Records Into Second Temp File POTEMP
    SELECT POSLN
    SEEK XSTYLE
    IF FOUND()
      IF !('POSLN.ACCOUNT INTO CUSTOMER' $ SET('RELATION'))
        SET RELATION TO "M" + POSLN.ACCOUNT INTO CUSTOMER ADDITIVE
      ENDIF

      XADD = ''
      DO CASE
        CASE XALLOCAT = 'L'
          XADD = ' .AND.  CutPick2.CtktNo = PosLn.Po'
        CASE XALLOCAT = 'N'
          XADD = ' .AND.  CutPick2.CtktNo <> PosLn.Po'
      ENDCASE

      IF XPLDAT1 <> CTOD('  /  /    ')
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE) = (XSTYLE) ;
             FOR (BETWEEN(POSHDR.COMPLETE,XPLDAT1,XPHDatC) .AND.   ;
             POSHDR.STATUS <> 'X' .AND. POSHDR.STATUS <> 'C' .AND. ;
             TOTQTY <> 0 &XADD &lcCustAcnt)
      ELSE
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE)      ;
             FOR (POSHDR.STATUS <> 'X' .AND. POSHDR.STATUS<>'C' .AND. ;
             TOTQTY <> 0 &XADD &lcCustAcnt)
      ENDIF

      SELECT POSLN
      IF 'POSLN.ACCOUNT INTO CUSTOMER' $ SET('RELATION')
        SET RELATION OFF INTO CUSTOMER
      ENDIF

      SELECT (POTEMP)
      APPEND FROM &gcWorkDir.&WORKTEMP
    ENDIF
  ENDIF

  *** Store off Records Into Third Temp File ORDTEMP.
  SELECT ORDLINE
  SEEK XSTYLE
  IF FOUND()
    IF !('ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION'))
      SET RELATION TO "M" + ORDLINE.ACCOUNT INTO CUSTOMER ADDITIVE
    ENDIF

    XADDORD = ''
    IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
      DO CASE
        CASE XALLOCAT = 'L'
          XADDORD = ' .AND.  (OrdLine.Order = CutPick.Order .OR. ' + ;
                    ' OrdLine.Order = CutPick3.Order) '

        CASE XALLOCAT = 'N'
          XADDORD = ' .AND.  (OrdLine.Order <> CutPick.Order .AND.' + ;
                    ' OrdLine.Order <> CutPick3.Order) '
      ENDCASE
    ENDIF

    IF XSLDAT1 <> CTOD('  /  /    ')
      COPY REST TO &gcWorkDir.&WORKTEMP                       ;
          WHILE Style = xStyle                                ;
            FOR BETWEEN(Complete , xSLDat1 , XSHDatC)   .AND. ;
                ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
    ELSE
      COPY REST TO &gcWorkDir.&WORKTEMP                       ;
          WHILE Style = xStyle                                ;
            FOR ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
    ENDIF

    SELECT ORDLINE
    IF 'ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION')
      SET RELATION OFF INTO CUSTOMER
    ENDIF

    SELECT (ORDTEMP)
    APPEND FROM &gcWorkDir.&WORKTEMP
  ENDIF
  SELECT STYLE
ENDSCAN

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF

IF USED('CUTPICK2')
  USE IN CUTPICK2
ENDIF

IF USED('CUTPICK3')
  USE IN CUTPICK3
ENDIF

PAGENO  = 0
ROW     = 99
XTIME   = TIME()
R_WIDTH ='W'
IF XWIPRPT .OR. XORDRPT
  DO CASE
    CASE XALLOCAT = 'L'
      R_TITLE = 'SPECIAL CUT & SOLD - ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'N'
      R_TITLE = 'SPECIAL CUT & SOLD - NOT ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'A'
      R_TITLE = 'SPECIAL CUT & SOLD - DETAIL REPORT'
  ENDCASE
ELSE
  DO CASE
    CASE XALLOCAT = 'L'
      R_TITLE = 'SPECIAL CUT & SOLD - ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'N'
      R_TITLE = 'SPECIAL CUT & SOLD - NOT ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'A'
      R_TITLE = 'SPECIAL CUT & SOLD - SUMMARY REPORT'
  ENDCASE
ENDIF

*--End of lpCollecData.
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Print report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint

IF XBYSIZE = 'N'
  DO STY961                                 && No Breakdown In Size
ELSE
  DO STY962                                 && Breakdown In Size
ENDIF

*--End of lpPrint.
*!*************************************************************
*! Name      : STY961
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : REPORT WITHOUT SIZE BREAKDOWN.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO STY961
*!*************************************************************
PROCEDURE STY961

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL330              123456789012345678901234567890 - SPECIAL CUT & SOLD REPORT          MM/DD/YY
*PAGE: 123                                                                               HH:MM:SS
*
*------------------------------------------------------------------------------------------------
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*XXXXXXX XXXXXXXXXXXXXXXXXXXX       XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX     TOTAL
*PO #   VENDOR COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     P.O. Subtotals ........  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*CUTTKT CONTR1 COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     Cuttkt Subtotals ......  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*ORDER  ACCT#  COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     Orders Subtotals ......  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*            STOCK  .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            W.I.P  .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            ORDERS .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            =====================================================================================================================
*            O.T.S. .........  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===
*                            GRAND TOTALS:  STOCK: 12345678   W.I.P: 12345678   ORDERS: 12345678   O.T.S: 12345678
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===

PRIVATE LLPRNTCB
lnHowMany = 1
llCretArry = .F.

IF (lnDatCount > 0 AND lnDatContP > 0 ) OR (lnDatCount # 0 OR lnDatContP # 0)
  lnHowMany = IIF( lnDatCount > lnDatContP ,  lnDatCount , lnDatContP )
ELSE
  IF lnDatCount = 0 AND lnDatContP = 0
    lnHowMany = 0
  ENDIF
ENDIF

*B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
*FOR lnLopD = 1 TO IIF( lnHowMany = 0 , 1 , lnHowMany)
*  lnNum = ALLTRIM(STR(lnLopD))

  *** Declare & Initialize Variables & Array
*  STORE SPACE(6)  TO XCUTTKT, XPO, XORDER , XCOLOR
*  STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
*  STORE CTOD('  /  /    ') TO XDATE
*  STORE 0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD
  STORE 0 TO XGRDSTK, XGRDWIP, XGRDORD

*  DIMENSION XCOLOR(10) , XCLR(10) , XSTK(10)    , XCUT(10) , XSUBCUT(10), XPOS(10)  ,;
*            XSUBPOS(10) , XORD(10) , XSUBORD(10) , XOTS(10) , XSTY(10)   , XSUBSTY(10)
*B606147,1 BWA 06/24/2002.[END]

  *** Determine Which Index, Seek, and Scan To Use (WIP)
  DO CASE
    CASE XWIPSORT = 'D'                          && Sort by Completion Date
      XCUTSCAN = '(CUTTKTH.COMPLETE = XDATE .AND. CUTTKT = XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XPOSCAN  = '(POSHDR.COMPLETE  = XDATE .AND. PO = XPO) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XCUTHDR  = 'COMPLETE CUTTKT CONTR1'
      XPOHDR   = 'COMPLETE PO #   VENDOR'

    CASE XWIPSORT = 'F'                          && Sort by Contractor
      XCUTSCAN = '(MFGOPRHD.cContCode = XCONTR1 .AND. CUTTKT = XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XPOSCAN  = '(POSHDR.VENDOR = XVENDOR .AND. PO = XPO) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XCUTHDR  = 'CONTR1 CUTTKT COMPLETE'
      XPOHDR   = 'VENDOR PO #   COMPLETE'

    OTHERWISE                                 && Sort by Cutting Ticket/PO
      XCUTSCAN = '(CUTTKTH.CUTTKT = XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XPOSCAN  = '(PO = XPO) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XCUTHDR  = 'CUTTKT CONTR1 COMPLETE'
      XPOHDR   = 'PO #   VENDOR COMPLETE'
  ENDCASE

  *** Determine Which Index, Seek, and Scan To Use (ORDERS)
  DO CASE
    CASE XORDSORT = 'D'                         && Sort by Completion Date
      XORDSCAN = '(COMPLETE = XDATE .AND. ORDER = XORDER) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XORDHDR  = 'COMPLETE ORDER   ACCT#'

    CASE XORDSORT = 'A'                         && Sort by Account
      XORDSCAN = '(ACCOUNT = XACCOUNT .AND. ORDER = XORDER) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XORDHDR  = 'ACCT# ORDER   COMPLETE'

    OTHERWISE                                 && Sort by Orders
      XORDSCAN = '(ORDER = XORDER) FOR SUBSTR(STYLE,1,lnMajLen) = XSTYLE'
      XORDHDR  = 'ORDER  ACCT#  COMPLETE'
  ENDCASE

  *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
  *IF lnLopD = 1 AND lnHowMany # 0
  *  llCretArry = .T.
  *  lnDimOts = 0
  *  llNewStyCb = .F.
  *ENDIF
  *B606147,1 BWA 06/24/2002.[END]

  SELECT STYLE
  LOCATE

  DO WHILE !EOF()

    *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
    FOR lnLopD = 1 TO IIF( lnHowMany = 0 , 1 , lnHowMany)
      lnNum = ALLTRIM(STR(lnLopD))

      *** Declare & Initialize Variables & Array
      STORE SPACE(6)  TO XCUTTKT, XPO, XORDER , XCOLOR
      STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
      STORE CTOD('  /  /    ') TO XDATE
      STORE 0 TO XCLRCNT, XCNT

      DIMENSION XCOLOR(10) , XCLR(10) , XSTK(10)    , XCUT(10) , XSUBCUT(10), XPOS(10)  ,;
               XSUBPOS(10) , XORD(10) , XSUBORD(10) , XOTS(10) , XSTY(10)   , XSUBSTY(10)

      IF lnLopD = 1 AND lnHowMany # 0
        llCretArry = .T.
        lnDimOts   = 0
        llNewStyCb = .F.
      ENDIF
      *B606147,1 BWA 06/24/2002.[END]

      *B606147,1 BWA 06/24/2002 Fix the bug creat the array due to the number of colors.[START]
      *IF llCretArry
      *  IF lnDimOts # 0
      *    llNewStyCb = IIF( ASCAN(XADDOTS,STYLE.STYLE) > 0 , .F. , .T.)
      *  ENDIF
      *  IF lnDimOts = 0 .OR. llNewStyCb
      *    lnDimOts = lnDimOts + 1
      *    DIMENSION XADDOTS[lnDimOts,9]
      *    XADDOTS[lnDimOts , 1 ] = STYLE.STYLE
      *  ENDIF
      *ENDIF
      *B606147,1 BWA 06/24/2002.[END]

      *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
      SELECT STYLE
      PRIVATE lcCurntRec
      lcCurntRec = EVAL(KEY())
      *B606147,1 BWA 06/24/2002.[END]

      XSTYLE = SUBSTR(STYLE,1,lnMajLen)
      XDESC  = DESC
      STORE SPACE(5) TO XCLR
      STORE SPACE(6) TO XCOLOR
      STORE 0 TO XSTK    , XCUT    , XSUBCUT , XTOTCUT , XPOS , XSUBPOS , XTOTPOS, XORD,;
                 XSUBORD , XTOTORD , XOTS    , XTOTSTY , XSTY , XSUBSTY

      *** To Get Number of Color For Each Style.
      COUNT ALL FOR (SUBSTR(STYLE,1,lnMajLen) = XSTYLE) TO XCLRCNT

      *** Load Array With Color and Stock.
      SEEK XSTYLE                               && To Place Pointer Back to 1st Match
      XCNT = IIF(XCLRCNT > 10 , 10 , XCLRCNT)

      FOR I = 1 TO XCNT
        XCLR(I)   = SUBSTR(STYLE.Style,lnNonMajPo)
        XCOLOR(I) = SUBSTR(STYLE.Style,lnNonMajPo)
        XSTK(I)   = TOTSTK
        SKIP
      ENDFOR

      IF XCLRCNT > 10
        XCLR(10) = 'OTHER'
        FOR I = 11 TO XCLRCNT
          XSTK(10) = XSTK(10) + TOTSTK
          SKIP
        ENDFOR
      ENDIF

      *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
      =SEEK(lcCurntRec)
      *B606147,1 BWA 06/24/2002.[END]
  
      *B606147,1 BWA 06/24/2002 Fix the bug creat the array due to the number of colors.[START]
      IF llCretArry
        IF lnDimOts # 0
          llNewStyCb = IIF( ASCAN(XADDOTS,STYLE.STYLE) > 0 , .F. , .T.)
        ENDIF
        IF lnDimOts = 0 .OR. llNewStyCb
          lnDimOts = lnDimOts + 1
          DIMENSION XADDOTS[lnDimOts,XCNT+1]
          XADDOTS[lnDimOts , 1 ] = STYLE.STYLE
        ENDIF
      ENDIF
      *B606147,1 BWA 06/24/2002.[END]

      IF ROW > 54
        ROW = 0
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
        ROW = 5
      ENDIF

      IF lnLopD # 1
        ROW = ROW + 1
      ENDIF

      *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
      IF lnLopD = 1
      *B606147,1 BWA 06/24/2002.[END]

        *** Display Style, Description, And Range Of Colors
        @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
        COL = 34
        FOR I = 1 TO XCNT
          IF LEN(TRIM(XCLR(I))) < 4
            XCLR(I) = IIF(LEN(TRIM(XCLR(I))) = 1 , '    ' + TRIM(XCLR(I)) , XCLR(I) )
            XCLR(I) = IIF(LEN(TRIM(XCLR(I))) = 2 , '   '  + TRIM(XCLR(I)) , XCLR(I) )
            XCLR(I) = IIF(LEN(TRIM(XCLR(I))) = 3 , '  '   + TRIM(XCLR(I)) , XCLR(I) )
          ENDIF
          @ ROW,COL SAY XCLR(I)
          COL = COL + 9
        ENDFOR

      *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
      ENDIF
      *B606147,1 BWA 06/24/2002.[END]

      IF ROW = 5
        @ ROW,125 SAY 'TOTAL'
      ENDIF
      ROW = ROW + 1
      @ ROW,01 SAY REPLICATE('-',129)

      IF lnHowMany > 0
        ROW = ROW + 1
        @ ROW,01 SAY  IIF( lnDatCount = 0 , SPACE(40) , IIF(lnLopD <= lnDatCount , "Sales Orders: " + DTOC(XSLDAT&lnNum) + " through " + DTOC(XSHDAT&lnNum) , SPACE(40))) +;
                      SPACE(20) +;
                      IIF( lnDatContP = 0 , SPACE(0) , IIF(lnLopD <= lnDatContP , "Purchase Orders: " + DTOC(XPLDAT&lnNum) + " through " + DTOC(XPHDAT&lnNum) , SPACE(0)))
        ROW = ROW + 1
      ENDIF

      IF IIF( lnTowDat = 0 , .T. , lnLopD <= lnTowDat )
        LLPRNTCB = .F.
        IF 'MF' $ gcComp_Mdl
          *** Get WIP From Cutting Ticket Temporary File
          SELECT (CUTTTEMP)
          LOCATE
          SEEK XSTYLE
          IF FOUND()
            XFIRSTTIME = .T.
            DO WHILE !EOF()
              IF (BETWEEN(CUTTKTH.COMPLETE , XPLDAT&lnNum , XPHDAT&lnNum) .OR. lnHowMany = 0) AND STYLE = XSTYLE
                LLPRNTCB = .T.
                IF ROW > 54 .AND. !EOF()
                  ROW = 0
                  PAGENO = PAGENO + 1
                  DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
                  ROW = 5
                  *** Display Style, Description, And Range Of Colors
                  @ ROW,01 SAY XSTYLE
                  COL = 34
                  FOR I = 1 TO XCNT
                    @ ROW,COL SAY XCLR(I)
                    COL = COL + 9
                  ENDFOR
                  @ ROW,125 SAY 'TOTAL'
                  ROW = ROW + 1
                  @ ROW,01 SAY REPLICATE('-',129)
                  XFIRSTTIME = .T.
                ENDIF
                XCUTTKT = CUTTKT
                XDATE   = CUTTKTH.COMPLETE
                XCONTR1 = MFGOPRHD.cContCode
  
                SCAN WHILE &XCUTSCAN
                  DO CASE
                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
                      XCUT(1) = IIF(TRANCD = '1' , XCUT(1) + TOTQTY , XCUT(1)-TOTQTY)
                      XCUT(1) = IIF(XCUT(1) > 0 , XCUT(1) , 0)
  
                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
                      XCUT(2) = IIF(TRANCD = '1' , XCUT(2) + TOTQTY , XCUT(2)-TOTQTY)
                      XCUT(2) = IIF(XCUT(2)>0 , XCUT(2) , 0)
  
                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
                      XCUT(3) = IIF(TRANCD = '1', XCUT(3) + TOTQTY , XCUT(3)-TOTQTY)
                      XCUT(3) = IIF(XCUT(3) > 0 , XCUT(3) , 0)
  
                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
                      XCUT(4) = IIF(TRANCD = '1', XCUT(4) + TOTQTY , XCUT(4)-TOTQTY)
                      XCUT(4) = IIF(XCUT(4) > 0 , XCUT(4) , 0)
  
                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
                      XCUT(5) = IIF(TRANCD = '1', XCUT(5) + TOTQTY , XCUT(5)-TOTQTY)
                      XCUT(5) = IIF(XCUT(5) > 0 , XCUT(5) , 0)

                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
                      XCUT(6) = IIF(TRANCD = '1', XCUT(6) + TOTQTY , XCUT(6)-TOTQTY)
                      XCUT(6) = IIF(XCUT(6) > 0 , XCUT(6) , 0)

                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
                      XCUT(7) = IIF(TRANCD = '1', XCUT(7) + TOTQTY , XCUT(7)-TOTQTY)
                      XCUT(7) = IIF(XCUT(7) > 0 , XCUT(7) , 0)
  
                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
                      XCUT(8) = IIF(TRANCD = '1', XCUT(8) + TOTQTY , XCUT(8)-TOTQTY)
                      XCUT(8) = IIF(XCUT(8) > 0 , XCUT(8) , 0)
  
                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
                      XCUT(9) = IIF(TRANCD = '1', XCUT(9) + TOTQTY , XCUT(9)-TOTQTY)
                      XCUT(9) = IIF(XCUT(9) > 0 , XCUT(9) , 0)

                    CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
                      XCUT(10) = IIF(TRANCD = '1' , XCUT(10) + TOTQTY , XCUT(10)-TOTQTY)
                      XCUT(10) = IIF(XCUT(10) > 0 , XCUT(10) , 0)

                    OTHERWISE
                      XCUT(10) = IIF(TRANCD = '1' , XCUT(10) + TOTQTY , XCUT(10)-TOTQTY)
                      XCUT(10) = IIF(XCUT(10) > 0 , XCUT(10) , 0)
                  ENDCASE
                ENDSCAN

                FOR I = 1 TO XCNT
                  XTOTCUT = XTOTCUT + XCUT(I)
                  XSUBCUT(I) = XSUBCUT(I) + XCUT(I)
                ENDFOR
                IF XWIPRPT .AND. XTOTCUT > 0
                  *** Display WIP Heading
                  IF XFIRSTTIME
                    ROW = ROW + 1
                    @ ROW,01 SAY XCUTHDR
                    ROW = ROW + 1
                    @ ROW,01 SAY REPLICATE('-',22)
                    XFIRSTTIME = .F.
                  ENDIF
                  ROW = ROW + 1
                  DO CASE
                    CASE XWIPSORT = 'D'
                      @ ROW,01 SAY XDATE
                      @ ROW,10 SAY XCUTTKT
                      @ ROW,17 SAY XCONTR1
                    CASE XWIPSORT = 'F'
                      @ ROW,01 SAY XCONTR1
                      @ ROW,08 SAY XCUTTKT
                      @ ROW,15 SAY XDATE
                    OTHERWISE
                      @ ROW,01 SAY XCUTTKT
                      @ ROW,08 SAY XCONTR1
                      @ ROW,15 SAY XDATE
                  ENDCASE

                  COL = 32
                  *** Display WIP Total Line Item
                  FOR I = 1 TO XCNT
                    @ ROW,COL SAY XCUT(I)  PICTURE '9999999'
                    COL = COL + 9
                  ENDFOR
                  @ ROW,122 SAY XTOTCUT  PICTURE '99999999'
                ENDIF
 
                IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen) <> XSTYLE
                  EXIT
                ENDIF
                STORE 0 TO XCUT, XTOTCUT
              ELSE
                SKIP 1
              ENDIF
            ENDDO
            IF XWIPRPT AND LLPRNTCB
              COL = 31
              XSUBTOT = 0
              ROW = ROW + 1
              @ ROW,31 SAY REPLICATE('-',99)
              ROW = ROW + 1
              @ ROW,06 SAY 'Cuttkt Subtotals ......'
              FOR I = 1 TO XCNT
                @ ROW,COL SAY XSUBCUT(I)  PICTURE '99999999'
                XSUBTOT = XSUBTOT + XSUBCUT(I)
                COL = COL + 9
              ENDFOR
              @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
              ROW = ROW + 1
            ENDIF
          ENDIF
        ENDIF    && End of IF 'MF' $ gcComp_Mdl
      ENDIF

       IF IIF( lnTowDat = 0 , .T. , lnLopD <= lnTowDat )
        LLPRNTCB = .F.
 
        *-- If the " Style Purchase Order " or the "Point of Sale" Modules is installed.
        IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
        *** Get WIP From Purchase Order Temporary File
          SELECT (POTEMP)
          GO TOP
          SEEK XSTYLE
          IF FOUND()
            XFIRSTTIME = .T.
            DO WHILE !EOF()
              IF (BETWEEN(POSHDR.COMPLETE , XPLDAT&lnNum , XPHDAT&lnNum) .OR. lnHowMany = 0) AND STYLE = XSTYLE
                LLPRNTCB = .T.
                IF ROW > 54 .AND. !EOF()
                  ROW = 0
                  PAGENO = PAGENO + 1
                  DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
                  ROW = 5
                  *** Display Style, Description, And Range Of Colors
                  @ ROW,01 SAY XSTYLE
                  COL = 34
                  FOR I = 1 TO XCNT
                    @ ROW,COL SAY XCLR(I)
                    COL = COL + 9
                  ENDFOR
                  @ ROW,125 SAY 'TOTAL'
                  ROW = ROW + 1
                  @ ROW,01 SAY REPLICATE('-',129)
                  XFIRSTTIME = .T.
                ENDIF

                XPO = PO
                XVENDOR = VENDOR
                XDATE = POSHDR.COMPLETE

                SCAN WHILE &XPOSCAN
                  IF !INLIST(TranCd , '3' , '6')
                    DO CASE
                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
                        xPOS(1) = IIF(TranCd = '1'                                   ,;
                                      xPOS(1) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(1) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(1) = IIF(XPOS(1) > 0 , XPOS(1) , 0)

                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
                        xPOS(2) = IIF(TranCd = '1'                                   ,;
                                      xPOS(2) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(2) -                                       ;
                                  (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(2) = IIF(XPOS(2) > 0 , XPOS(2) , 0)
  
                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
                        xPOS(3) = IIF(TranCd = '1'                                   ,;
                                      xPOS(3) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(3) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(3) = IIF(XPOS(3) > 0 , XPOS(3) , 0)
  
                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
                        xPOS(4) = IIF(TranCd = '1'                                   ,;
                                      xPOS(4) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(4) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(4) = IIF(XPOS(4) > 0 , XPOS(4) , 0)
  
                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
                        xPOS(5) = IIF(TranCd = '1'                                   ,;
                                      xPOS(5) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(5) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(5) = IIF(XPOS(5) > 0 , XPOS(5) , 0)
 
                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
                        xPOS(6) = IIF(TranCd = '1'                                   ,;
                                      xPOS(6) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(6) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(6) = IIF(XPOS(6) > 0 , XPOS(6) , 0)
  
                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
                        xPOS(7) = IIF(TranCd = '1'                                   ,;
                                      xPOS(7) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(7) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(7) = IIF(XPOS(7) > 0 , XPOS(7) , 0)
 
                      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
                        xPOS(8) = IIF(TranCd = '1'                                   ,;
                                      xPOS(8) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(8) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(8) = IIF(XPOS(8) > 0 , XPOS(8) , 0)
  
                      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
                        xPOS(9) = IIF(TranCd = '1'                                   ,;
                                      xPOS(9) +                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                      xPOS(9) -                                       ;
                                     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(9) = IIF(XPOS(9) > 0 , XPOS(9) , 0)
  
                      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
                        xPOS(10) = IIF(TranCd = '1'                                  ,;
                                       xPOS(10) +                                     ;
                                      (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                       xPOS(10) -                                     ;
                                      (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(10) = IIF(XPOS(10) > 0 , XPOS(10) , 0)
  
                      OTHERWISE
                        xPOS(10) = IIF(TranCd = '1'                                  ,;
                                       xPOS(10) +                                     ;
                                      (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                       xPOS(10) -                                     ;
                                      (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                        XPOS(10) = IIF(XPOS(10) > 0 , XPOS(10) , 0)
                    ENDCASE
                  ENDIF
                ENDSCAN

                FOR I = 1 TO XCNT
                  XTOTPOS = XTOTPOS + XPOS(I)
                  XSUBPOS(I) = XSUBPOS(I) + XPOS(I)
                ENDFOR

                IF XWIPRPT .AND. XTOTPOS > 0
                  *** Display WIP Heading
                  IF XFIRSTTIME
                    ROW = ROW + 1
                    @ ROW,01 SAY XPOHDR
                    ROW = ROW + 1
                    @ ROW,01 SAY REPLICATE('-',22)
                    XFIRSTTIME = .F.
                  ENDIF
                  ROW = ROW + 1
                  DO CASE
                    CASE XWIPSORT = 'D'
                      @ ROW,01 SAY XDATE
                      @ ROW,10 SAY XPO
                      @ ROW,17 SAY XVENDOR
  
                    CASE XWIPSORT = 'F'
                      @ ROW,01 SAY XVENDOR
                      @ ROW,08 SAY XPO
                      @ ROW,15 SAY XDATE
  
                    OTHERWISE
                      @ ROW,01 SAY XPO
                      @ ROW,08 SAY XVENDOR
                      @ ROW,15 SAY XDATE
                  ENDCASE

                  COL = 32
                  *** Display WIP Total Line Item
                  FOR I = 1 TO XCNT
                    @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
                    COL = COL + 9
                  ENDFOR
                  @ ROW,122 SAY XTOTPOS  PICTURE '99999999'
                ENDIF

                IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen) <> XSTYLE
                  EXIT
                ENDIF
                STORE 0 TO XPOS, XTOTPOS
              ELSE
                SKIP 1
              ENDIF
            ENDDO

            IF XWIPRPT AND LLPRNTCB
              COL = 31
              XSUBTOT = 0
              ROW = ROW + 1
              @ ROW,31 SAY REPLICATE('-',99)
              ROW = ROW + 1
              @ ROW,06 SAY 'P.O. Subtotals ........'
              FOR I = 1 TO XCNT
                @ ROW,COL SAY XSUBPOS(I)  PICTURE '99999999'
                XSUBTOT = XSUBTOT + XSUBPOS(I)
                COL = COL + 9
              ENDFOR
              @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
              ROW = ROW + 1
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      *--Get wip from style file in case of the "Style Purchase Order" or the
      *--"Point of Sale" or "Manufacturing" Modules is not installed.
      *--- Get WIP From Syle File
      IF IIF( lnTowDat = 0 , .T. , lnLopD <= lnTowDat )
        LLPRNTCB = .F.
        IF !('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
          SELECT STYLE
          LOCATE
          SEEK XSTYLE
          IF FOUND()
            XFIRSTTIME = .T.
            DO WHILE INKEY() <> 32
              IF ROW > 54 .AND. !EOF()
                ROW = 0
                PAGENO = PAGENO + 1
                DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
                ROW = 5
                *** Display Style, Description, And Range Of Colors
                @ ROW,01 SAY XSTYLE
                COL = 34
                FOR I = 1 TO XCNT
                  @ ROW,COL SAY XCLR(I)
                  COL = COL + 9
                ENDFOR
                @ ROW,125 SAY 'TOTAL'
                ROW = ROW + 1
                @ ROW,01 SAY REPLICATE('-',129)
                XFIRSTTIME = .T.
              ENDIF
              SELECT STYLE
              SCAN WHILE SUBSTR(STYLE,1,lnMajLen) = XSTYLE
                DO CASE
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
                    XSTY(1) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
                    XSTY(2) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
                    XSTY(3) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
                    XSTY(4) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
                    XSTY(5) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
                    XSTY(6) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
                    XSTY(7) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
                    XSTY(8) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
                    XSTY(9) = TOTWIP
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
                    XSTY(10) = TOTWIP
                  OTHERWISE
                    XSTY(10) = TOTWIP
                ENDCASE
              ENDSCAN

              FOR I = 1 TO XCNT
                XTOTSTY = XTOTSTY + XSTY(I)
                XSUBSTY(I) = XSUBSTY(I) + XSTY(I)
              ENDFOR

              IF XWIPRPT .AND. XTOTPOS > 0
                *** Display WIP Heading
                ROW = ROW + 1
                COL = 32
                *** Display WIP Total Line Item
                FOR I = 1 TO XCNT
                  @ ROW,COL SAY XSTY(I)  PICTURE '9999999'
                  COL = COL + 9
                ENDFOR
                @ ROW,122 SAY XTOTSTY  PICTURE '99999999'
              ENDIF
  
              IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen) <> XSTYLE
                EXIT
              ENDIF
              STORE 0 TO XSTY, XTOTSTY
            ENDDO

            IF XWIPRPT
              COL = 31
              XSUBTOT = 0
              ROW = ROW + 1
              @ ROW,31 SAY REPLICATE('-',99)
              ROW = ROW + 1
              @ ROW,06 SAY 'P.O. Subtotals ........'
              FOR I = 1 TO XCNT
                @ ROW,COL SAY XSUBSTY(I)  PICTURE '99999999'
                XSUBTOT = XSUBTOT + XSUBSTY(I)
                COL = COL + 9
              ENDFOR
              @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
              ROW = ROW + 1
            ENDIF
          ENDIF
        ENDIF

        *** Get Sales From Orders Temporary File
        SELECT (ORDTEMP)
        LOCATE
        SEEK XSTYLE
        IF FOUND()
          XFIRSTTIME = .T.
          DO WHILE !EOF()
            IF (BETWEEN(COMPLETE , XSLDAT&lnNum , XSHDAT&lnNum) .OR. lnHowMany = 0) AND STYLE = XSTYLE
              LLPRNTCB = .T.
              IF ROW > 54 .AND. !EOF()
                ROW = 0
                PAGENO = PAGENO + 1
                DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
                ROW = 5
                *** Display Style, Description, And Range Of Colors
                @ ROW,01 SAY XSTYLE
                COL = 34
                FOR I = 1 TO XCNT
    	          @ ROW,COL SAY XCLR(I)
                  COL = COL + 9
	            ENDFOR
                @ ROW,125 SAY 'TOTAL'
	            ROW = ROW + 1
                @ ROW,01 SAY REPLICATE('-',129)
	            XFIRSTTIME = .T.
              ENDIF
  
              XORDER   = ORDHDR.ORDER
              XACCOUNT = ACCOUNT
 	          XDATE    = COMPLETE
  
              SCAN WHILE &XORDSCAN
                DO CASE
	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
	                XORD(1) = XORD(1) + TOTQTY
	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
	                XORD(2) = XORD(2) + TOTQTY
	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
	                XORD(3) = XORD(3) + TOTQTY
                  CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
	                XORD(4) = XORD(4) + TOTQTY
	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
                    XORD(5) = XORD(5) + TOTQTY
	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
                    XORD(6) = XORD(6) + TOTQTY
 	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
  	                XORD(7) = XORD(7) + TOTQTY
  	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
	                XORD(8) = XORD(8) + TOTQTY
	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
  	                XORD(9) = XORD(9) + TOTQTY
	              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
  	                XORD(10) = XORD(10) + TOTQTY
                  OTHERWISE
    	            XORD(10) = XORD(10) + TOTQTY
                ENDCASE
	          ENDSCAN

              FOR I = 1 TO XCNT
	            XTOTORD    = XTOTORD    + XORD(I)
	            XSUBORD(I) = XSUBORD(I) + XORD(I)
	          ENDFOR

              IF XORDRPT .AND. XTOTORD > 0
                *** Display ORD Heading
	            IF XFIRSTTIME
	              ROW = ROW + 1
  	              @ ROW,01 SAY XORDHDR
  	              ROW = ROW + 1
	              @ ROW,01 SAY REPLICATE('-',22)
	              XFIRSTTIME = .F.
	            ENDIF
                ROW = ROW + 1
                DO CASE
 	              CASE XORDSORT = 'D'
	                @ ROW,01 SAY XDATE
 	                @ ROW,10 SAY XORDER
	                @ ROW,18 SAY XACCOUNT

                  CASE XORDSORT = 'A'
                    @ ROW,01 SAY XACCOUNT
  	                @ ROW,07 SAY XORDER
	                @ ROW,15 SAY XDATE

                  OTHERWISE
                    @ ROW,01 SAY XORDER
  	                @ ROW,08 SAY XACCOUNT
 	                @ ROW,15 SAY XDATE
	            ENDCASE

	            COL = 32
	     	    *** Display ORD Total Line Item
                FOR I = 1 TO XCNT
                  @ ROW,COL SAY XORD(I) PICTURE '9999999'
	              COL = COL + 9
	            ENDFOR
	            @ ROW,122 SAY XTOTORD  PICTURE '99999999'
	          ENDIF
 
              IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen) <> XSTYLE
	            EXIT
	          ENDIF
              STORE 0 TO XORD, XTOTORD
 	        ELSE
	          SKIP 1
            ENDIF
          ENDDO
	      IF XORDRPT AND LLPRNTCB
	        COL     = 31
	        XSUBTOT = 0
	        ROW     = ROW + 1
  	        @ ROW,31 SAY REPLICATE('-',99)
  	        ROW = ROW + 1
  	        @ ROW,06 SAY 'Orders Subtotals ......'
	        FOR I = 1 TO XCNT
	          @ ROW,COL SAY XSUBORD(I)  PICTURE '99999999'
  	          XSUBTOT = XSUBTOT + XSUBORD(I)
	          COL = COL + 9
	        ENDFOR
            @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
 	        ROW = ROW + 1
	      ENDIF
        ENDIF

        *** Display Summary Information
        STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS

	    IF ROW > 49
	      ROW = 0
      	  PAGENO = PAGENO + 1
  	      DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
  	      ROW = 5
          @ ROW,01 SAY XSTYLE
   	      COL = 34
   	      FOR I = 1 TO XCNT
	        @ ROW,COL SAY XCLR(I)
	        COL = COL + 9
	      ENDFOR
  	      @ ROW,125 SAY 'TOTAL'
	      ROW = ROW + 1
	      @ ROW,01 SAY REPLICATE('-',129)
        ENDIF

        IF IIF( lnDatCount = 0 , .T. , lnLopD = 1 )
  	      COL = 32
	      ROW = ROW + 1
          @ ROW,13 SAY 'STOCK  .........'
  		  FOR I = 1 TO XCNT
	        @ ROW,COL SAY XSTK(I)  PICTURE '9999999'
            XTOTSTK = XTOTSTK + XSTK(I)
            XOTS(I) = XOTS(I) + XSTK(I)
	        COL = COL + 9
		  ENDFOR
          @ ROW,122 SAY XTOTSTK    PICTURE '99999999'
          XGRDSTK = XGRDSTK + XTOTSTK
        ELSE

          *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
	      *FOR I = 1 TO XCNT
          *  XTOTSTK = XTOTSTK + XSTK(I)
          *ENDFOR
          *XGRDSTK = XGRDSTK + XTOTSTK
          *B606147,1 BWA 06/24/2002.[END]
  	    ENDIF

    	ROW = ROW + 1
  	    COL = 32
  	    @ ROW,13 SAY 'W.I.P  .........'
 
        FOR I = 1 TO XCNT
	      @ ROW,COL SAY XSUBCUT(I) + XSUBPOS(I) + XSUBSTY(I)  PICTURE '9999999'
	      XTOTWIP = XTOTWIP + XSUBCUT(I) + XSUBPOS(I) + XSUBSTY(I)
	      XOTS(I) = XOTS(I) + XSUBCUT(I) + XSUBPOS(I) + XSUBSTY(I)
  	      COL = COL + 9
        ENDFOR
   	    @ ROW,122 SAY XTOTWIP       PICTURE '99999999'
	    XGRDWIP = XGRDWIP + XTOTWIP

        ROW = ROW + 1
	    COL = 32
	    @ ROW,13 SAY 'ORDERS .........'
        FOR I = 1 TO XCNT
	      @ ROW,COL SAY XSUBORD(I)  PICTURE '9999999'
  	      XTOTORD = XTOTORD + XSUBORD(I)
	      XOTS(I) = XOTS(I) - XSUBORD(I)
          COL = COL + 9
  	    ENDFOR
        @ ROW,122 SAY XTOTORD       PICTURE '99999999'
	    XGRDORD = XGRDORD + XTOTORD

        ROW = ROW + 1
        @ ROW,13 SAY REPLICATE('-',117)
        ROW = ROW + 1
	    COL = 31
  	    @ ROW,13 SAY 'O.T.S. .........'

       IF llCretArry
         IF ASUBSCRIPT(XADDOTS,ASCAN(XADDOTS,XSTYLE),1) > 0
           lnStyPos = ASUBSCRIPT(XADDOTS,ASCAN(XADDOTS,XSTYLE),1)
         ENDIF
       ENDIF

        FOR I = 1 TO XCNT
	      IF lnHowMany # 0
	        IF lnLopD = 1
              XADDOTS[lnStyPos , I+1] = XOTS(I)
	        ELSE
	          XOTS(I)                 = XOTS(I) + XADDOTS[lnStyPos , I+1]
	          XADDOTS[lnStyPos , I+1] = XOTS(I)
            ENDIF
  	      ENDIF
	      @ ROW,COL SAY XOTS(I)     PICTURE '99999999'
	      XTOTOTS = XTOTOTS + XOTS(I)
	      COL = COL + 9
        ENDFOR
	    @ ROW,121 SAY XTOTOTS       PICTURE '999999999'
        ROW = ROW + 1

        *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
        *B606147,1 BWA 07/02/2002 Fix the bug of "operator type mismatch".[START]
        *IF IIF( lnDatContP = 0 , .T. , lnLopD = lnDatContP )
        IF IIF( lnTowDat = 0 , .T. , lnLopD = lnTowDat )
        *B606147,1 BWA 07/02/2002.[END]
        *B606147,1 BWA 06/24/2002.[END]

          SELECT STYLE
          SCAN WHILE SUBSTR(STYLE,1,lnMajLen) = XSTYLE
          ENDSCAN

          IF EOF()
	        ROW = ROW + 2
            IF ROW > 52
  	          ROW = 0
    	      PAGENO = PAGENO + 1
              DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
              ROW = 5
            ENDIF
	        @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
            ROW = ROW + 1
            @ ROW,29 SAY 'GRAND TOTALS:  STOCK:'
            @ ROW,51 SAY XGRDSTK  PICTURE '99999999'
	        @ ROW,62 SAY 'W.I.P:'
	        @ ROW,69 SAY XGRDWIP  PICTURE '99999999'
            @ ROW,80 SAY 'ORDERS:'
            @ ROW,88 SAY XGRDORD  PICTURE '99999999'
            @ ROW,99 SAY 'O.T.S:'
	        @ ROW,106 SAY XGRDSTK+XGRDWIP-XGRDORD  PICTURE '99999999'
	        ROW = ROW + 1
            @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
	        EXIT
  	      ENDIF
          ROW = ROW + 3
        ENDIF
	  ENDIF
    ENDFOR
  ENDDO
RETURN

*--End of STY961.
*!*************************************************************
*! Name      : STY962
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : REPORT WITH SIZE BREAKDOWN.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO STY962
*!*************************************************************
PROCEDURE STY962

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL330              123456789012345678901234567890 - SPECIAL CUT & SOLD REPORT          MM/DD/YY
*PAGE: 123                                                                               HH:MM:SS
*
*------------------------------------------------------------------------------------------------
*XXXXXXX XXX XXXXXXXXXXXXXXXXXXXX                 XXX    XXX    XXX    XXX    XXX    XXX    XXX    XXX   TOTAL   PRICE       EXT S P
*COMPLETE PO #  VENDOR NAME
*99/99/99 123456 12345 123456789012345678901234 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*VENDOR NAME                    PO #   COMPLETE
*12345  12345678901234567890123 123456 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*PO #  VENDOR NAME                     COMPLETE
*123456 12345 123456789012345678901234 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*                    P.O. Subtotals .......    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*COMPLETE CUTTKT CONTR NAME
*99/99/99 123456 12345 123456789012345678901234 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*CONTR NAME                     CUTTKT COMPLETE
*12345 123456789012345678901234 123456 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*CUTTKT CONTR NAME                     COMPLETE
*123456 12345 123456789012345678901234 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*                    Cuttkt Subtotals .....    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*START    COMPLETE ORDER  ACCT# NAME
*99/99/99 99/99/99 123456 12345 123456789012345 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ACCT# NAME            ORDER  START    COMPLETE
*12345 123456789012345 123456 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ORDER  ACCT# NAME            START    COMPLETE
*123456 12345 123456789012345 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*                    Orders Subtotals .....    999999 999999 999999 999999 999999 999999 999999 999999 9999999        9999999.99
*
*                          STOCK  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          W.I.P  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ORDERS .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ===================================================================================
*                          O.T.S. .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====
*                          TOTAL PIECES ORDERED: 12345678     TOTAL $$$ ORDERED $ 999,999,999.99
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====

PRIVATE LLPRNTCB
lnHowMany = 1
llCretArry = .F.

IF (lnDatCount > 0 AND lnDatContP > 0 ) OR (lnDatCount # 0 OR lnDatContP # 0)
  lnHowMany = IIF( lnDatCount > lnDatContP ,  lnDatCount , lnDatContP )
ELSE
  IF lnDatCount = 0 AND lnDatContP = 0
    lnHowMany = 0
  ENDIF
ENDIF

*B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
*FOR lnLopD = 1 TO IIF( lnHowMany = 0 , 1 , lnHowMany)
*  lnNum = ALLTRIM(STR(lnLopD))

*  *** Initialize Variables
*  STORE SPACE(30) TO XNAME
*  STORE SPACE(6)  TO XCUTTKT , XPO      , XORDER , XCOLOR
*  STORE SPACE(5)  TO XVENDOR , XACCOUNT , XCONTR1

*  STORE SPACE(1)  TO XSTAT, XPIKTKT
*  STORE CTOD('  /  /    ') TO XDATE, XSTART
*  STORE 0 TO XCNT, XPRICE, XGRDORD, XGRDPRI
  STORE 0 TO XGRDORD , XGRDPRI

*  DIMENSION XSTK(8) , XOTS(8) , XSIZE(8) , XCUT(8) , XSUBCUT(8) , XPOS(8) , XSUBPOS(8) , XORD(8) , XSUBORD(8) , XSTY(8) , XSUBSTY(8)
*B606147,1 BWA 06/24/2002.[END]

  *** Determine Which Index, Seek, and Scan To Use (WIP)
  DO CASE
    CASE XWIPSORT='D'                          && Sort by Completion Date
      XCUTSCAN = '(STYLE+DTOS(CUTTKTH.COMPLETE)+CUTTKT) = (lcStyleCod+DTOS(XDATE)+XCUTTKT)'
      XCUTHDR  = 'COMPLETE CUTTKT CONTR NAME'
      XPOSCAN  = '(STYLE+DTOS(POSHDR.COMPLETE)+PO) = (lcStyleCod+DTOS(XDATE)+XPO)'
      XPOHDR   = 'COMPLETE PO #  VENDOR NAME'

    CASE XWIPSORT='F'                          && Sort by Contractor
      XCUTSCAN = '(STYLE+MFGOPRHD.cContCode+CUTTKT) = (lcStyleCod+XCONTR1+XCUTTKT)'
      XCUTHDR  = 'CONTR NAME'+SPACE(21)+'CUTTKT COMPLETE'
      XPOSCAN  = '(STYLE+VENDOR+PO) = (lcStyleCod+XVENDOR+XPO)'
      XPOHDR   = 'VENDOR NAME'+SPACE(20)+'PO #   COMPLETE'

    OTHERWISE                                  && Sort by Cutting Ticket/PO
      XCUTSCAN = '(STYLE+CUTTKT) = (lcStyleCod+XCUTTKT)'
      XCUTHDR  = 'CUTTKT CONTR NAME'+SPACE(21)+'COMPLETE'
      XPOSCAN  = '(STYLE+PO) = (lcStyleCod+XPO)'
      XPOHDR   = 'PO #  VENDOR NAME'+SPACE(21)+'COMPLETE'
  ENDCASE

  *** Determine Which Index, Seek, and Scan To Use (ORDERS)
  DO CASE
    CASE XORDSORT = 'D'                         && Sort by Completion Date
      XORDHDR = 'START    COMPLETE ORDER  ACCT# NAME'
    CASE XORDSORT = 'A'                         && Sort by Account
      XORDHDR = 'ACCT# NAME'+SPACE(12)+'ORDER  START    COMPLETE'
    OTHERWISE                                  && Sort by Orders
      XORDHDR = 'ORDER  ACCT# NAME'+SPACE(12)+'START    COMPLETE'
  ENDCASE

  *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
  *IF lnLopD = 1 AND lnHowMany # 0
  *  llCretArry = .T.
  *  lnDimOts = 0
  *  llNewStyCb = .F.
  *ENDIF
  *B606147,1 BWA 06/24/2002.[END]

  SELECT STYLE
  SET RELATION TO 'S'+SCALE INTO SCALE
  LOCATE

  DO WHILE !EOF()

    *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
    FOR lnLopD = 1 TO IIF( lnHowMany = 0 , 1 , lnHowMany)
      lnNum = ALLTRIM(STR(lnLopD))

      *** Initialize Variables
      STORE SPACE(30) TO XNAME
      STORE SPACE(6)  TO XCUTTKT , XPO      , XORDER , XCOLOR
      STORE SPACE(5)  TO XVENDOR , XACCOUNT , XCONTR1
      STORE SPACE(1)  TO XSTAT, XPIKTKT
      STORE CTOD('  /  /    ') TO XDATE, XSTART
      STORE 0 TO XCNT, XPRICE

      DIMENSION XSTK(8) , XOTS(8) , XSIZE(8) , XCUT(8) , XSUBCUT(8) , XPOS(8) , XSUBPOS(8) , XORD(8) , XSUBORD(8) , XSTY(8) , XSUBSTY(8)

      IF lnLopD = 1 AND lnHowMany # 0
        llCretArry = .T.
        lnDimOts = 0
        llNewStyCb = .F.
      ENDIF
      *B606147,1 BWA 06/24/2002.[END]

    IF llCretArry
      IF lnDimOts # 0
        llNewStyCb = IIF( ASCAN(XADDOTS,STYLE.STYLE) > 0 , .F. , .T.)
      ENDIF

      IF lnDimOts = 0 .OR. llNewStyCb
        lnDimOts = lnDimOts + 1
        DIMENSION XADDOTS[lnDimOts,9]
        XADDOTS[lnDimOts , 1 ] = STYLE.STYLE
      ENDIF
    ENDIF

    *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
    SELECT STYLE
    *B606147,1 BWA 06/24/2002.[END]

    XSTYLE     = SUBSTR(STYLE,1,lnMajLen)
    lcStyleCod = Style
    XDESC      = DESC
    XSCALE     = SCALE
    XCOLOR     = SUBSTR(STYLE.Style,lnNonMajPo)
    STORE SPACE(3) TO XSIZE
    STORE 0 TO XSTK, XOTS, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XSUBEXT,XSTY,XTOTSTY,XSUBSTY

    *** To Get Number of Size Breakdown For Each Style
    XCNT = SCALE.CNT

    *** Load Array With Scale
    FOR I = 1 TO XCNT
      Z = STR(I,1)
      XSIZE(I) = SCALE.SZ&Z
      XSTK(I) = STK&Z
    ENDFOR

    IF ROW > 54
      ROW = 0
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
      ROW = 5
    ENDIF

    IF lnLopD # 1
      ROW = ROW + 1
    ENDIF

    *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
    IF lnLopD = 1
    *B606147,1 BWA 06/24/2002.[END]

      *** Display Style, Description, And Scale Breakdown
      @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
 
      SELECT STYLE
      COL = 50
      FOR I = 1 TO XCNT
        IF LEN(TRIM(XSIZE(I))) < 3
          XSIZE(I) = IIF(LEN(TRIM(XSIZE(I))) = 1 , '  ' + TRIM(XSIZE(I)) , XSIZE(I))
          XSIZE(I) = IIF(LEN(TRIM(XSIZE(I))) = 2 , ' '  + TRIM(XSIZE(I)) , XSIZE(I))
        ENDIF
        @ ROW,COL SAY XSIZE(I)
        COL = COL + 7
      ENDFOR
    ENDIF

    IF ROW = 5
      @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
    ENDIF

    ROW = ROW + 1
    @ ROW,01 SAY REPLICATE('-',131)

    IF lnHowMany > 0
      ROW = ROW + 1
      @ ROW,01 SAY  IIF( lnDatCount = 0 , SPACE(40) , IIF(lnLopD <= lnDatCount , "Sales Orders: " + DTOC(XSLDAT&lnNum) + " through " + DTOC(XSHDAT&lnNum) , SPACE(40))) +;
                    SPACE(20) +;
                    IIF( lnDatContP = 0 , SPACE(0) , IIF(lnLopD <= lnDatContP , "Purchase Orders: " + DTOC(XPLDAT&lnNum) + " through " + DTOC(XPHDAT&lnNum) , SPACE(0)))
      ROW = ROW + 1
    ENDIF

     IF IIF( lnTowDat = 0 , .T. , lnLopD <= lnTowDat )
      LLPRNTCB = .F.
      *--If the "Manufacturing" module is installed
      IF 'MF' $ gcComp_Mdl
        *** Get WIP From Cutting Ticket Temporary File
        SELECT (CUTTTEMP)
        LOCATE
        SEEK lcStyleCod
        IF FOUND()
          XFIRSTTIME = .T.
          DO WHILE !EOF()
            IF (BETWEEN(CUTTKTH.COMPLETE , XPLDAT&lnNum , XPHDAT&lnNum) .OR. lnHowMany = 0) AND STYLE = lcStyleCod
              LLPRNTCB = .T.
              IF ROW > 54 .AND. !EOF()
                ROW = 0
                PAGENO = PAGENO + 1
                DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
                ROW = 5
                *** Display Style, Description, And Scale Breakdown
                @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)

                SELECT (CUTTTEMP)
                COL = 50
                FOR I = 1 TO XCNT
                  @ ROW,COL SAY XSIZE(I)
                  COL = COL + 7
                ENDFOR
                @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
                ROW = ROW + 1
                @ ROW,01 SAY REPLICATE('-',131)
                XFIRSTTIME = .T.
              ENDIF

              XCUTTKT = CUTTKT
              XDATE   = CUTTKTH.COMPLETE
              XSTAT   = CUTTKTH.STATUS
              XCONTR1 = MFGOPRHD.cContCode

              SELECT APVENDOR
              SEEK XCONTR1
              XNAME = IIF(FOUND() , cVenComp , SPACE(30))

              SELECT (CUTTTEMP)
              SCAN WHILE &XCUTSCAN
                FOR I = 1 TO XCNT
                  Z = STR(I,1)
                  XCUT(I) = IIF(TRANCD = '1', XCUT(I) + QTY&Z , XCUT(I) - QTY&Z)
                  XCUT(I) = IIF(XCUT(I) > 0 , XCUT(I) , 0)
                ENDFOR
              ENDSCAN

              FOR I = 1 TO XCNT
                XTOTCUT = XTOTCUT + XCUT(I)
                XSUBCUT(I) = XSUBCUT(I) + XCUT(I)
              ENDFOR

              IF XWIPRPT .AND. XTOTCUT > 0
                *** Display WIP Heading
                IF XFIRSTTIME
                  ROW = ROW + 1
                  @ ROW,01 SAY XCUTHDR
                  ROW = ROW + 1
                  @ ROW,01 SAY REPLICATE('-',46)
                  XFIRSTTIME = .F.
                ENDIF
                ROW = ROW + 1
                DO CASE
                  CASE XWIPSORT = 'D'
                    @ ROW,01 SAY XDATE
                    @ ROW,10 SAY XCUTTKT
                    @ ROW,17 SAY XCONTR1
                    @ ROW,23 SAY SUBSTR(XNAME,1,24)
                  CASE XWIPSORT = 'F'
                    @ ROW,01 SAY XCONTR1
                    @ ROW,07 SAY SUBSTR(XNAME,1,24)
                    @ ROW,32 SAY XCUTTKT
                    @ ROW,39 SAY XDATE
                  OTHERWISE
                    @ ROW,01 SAY XCUTTKT
                    @ ROW,08 SAY XCONTR1
                    @ ROW,14 SAY SUBSTR(XNAME,1,24)
                    @ ROW,39 SAY XDATE
                ENDCASE

                COL = 48
                *** Display WIP Total Line Item
                FOR I = 1 TO XCNT
                  @ ROW,COL SAY XCUT(I)  PICTURE '99999'
                  COL = COL + 7
                ENDFOR
                @ ROW,104 SAY XTOTCUT  PICTURE '999999'
                @ ROW,129 SAY XSTAT
              ENDIF

              IF EOF() .OR. STYLE <> lcStyleCod
                EXIT
              ENDIF
              STORE 0 TO XCUT, XTOTCUT
            ELSE
              SKIP 1
            ENDIF
          ENDDO

          IF XWIPRPT AND LLPRNTCB
            COL = 47
            XSUBTOT = 0
            ROW = ROW + 1
            @ ROW,47 SAY REPLICATE('-',85)
            ROW = ROW + 1
            @ ROW,21 SAY 'Cuttkt Subtotals .....'
            FOR I = 1 TO XCNT
              @ ROW,COL SAY XSUBCUT(I)  PICTURE '999999'
              XSUBTOT = XSUBTOT + XSUBCUT(I)
              COL = COL + 7
            ENDFOR
            @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
            ROW = ROW + 1
          ENDIF
        ENDIF
      ENDIF    && End of IF 'MF' $ gcComp_Mdl
    ENDIF

    *-- If the "Style Purchase Order" or the "Point of Sale" Modules is installed.
    IF IIF( lnTowDat = 0 , .T. , lnLopD <= lnTowDat )
      LLPRNTCB = .F.
      IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
        *** Get WIP From Purchase Order Temporary File
        SELECT (POTEMP)
        LOCATE
        SEEK lcStyleCod
        IF FOUND()
          XFIRSTTIME = .T.
          DO WHILE !EOF()
            IF (BETWEEN(POSHDR.COMPLETE , XPLDAT&lnNum , XPHDAT&lnNum) .OR. lnHowMany = 0) STYLE = lcStyleCod
              LLPRNTCB = .T.
              IF ROW > 54 .AND. !EOF()
                ROW = 0
                PAGENO = PAGENO + 1
                DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
                ROW = 5
                *** Display Style, Description, And Scale Breakdown
                @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
                SELECT (POTEMP)
                COL = 50
                FOR I = 1 TO XCNT
                  @ ROW,COL SAY XSIZE(I)
                  COL = COL + 7
                ENDFOR
                @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
                ROW = ROW + 1
                @ ROW,01 SAY REPLICATE('-',131)
                XFIRSTTIME = .T.
              ENDIF

              XPO     = PO
              XVENDOR = VENDOR
              XDATE   = POSHDR.COMPLETE
              XSTAT   = POSHDR.STATUS
              SELECT APVENDOR
              SEEK XVENDOR
              XNAME = IIF(FOUND() , cVenComp , SPACE(30))

              SELECT (POTEMP)
              SCAN WHILE &XPOSCAN
                IF !INLIST(TranCd , '3' , '6')
                  FOR I = 1 TO XCNT
                    Z = STR(I,1)
                    xPOS(I) = IIF(TranCd = '1'                                  ,;
                                  xPOS(I) +                                      ;
                                 (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1))  ,;
                                  xPOS(I) -                                      ;
                                 (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                    XPOS(I) = IIF(XPOS(I) > 0 , XPOS(I), 0)
                  ENDFOR
                ENDIF
              ENDSCAN

              FOR I = 1 TO XCNT
                XTOTPOS = XTOTPOS + XPOS(I)
                XSUBPOS(I) = XSUBPOS(I) + XPOS(I)
              ENDFOR

              IF XWIPRPT .AND. XTOTPOS > 0
                *** Display WIP Heading
                IF XFIRSTTIME
                  ROW = ROW + 1
                  @ ROW,01 SAY XPOHDR
                  ROW = ROW + 1
                  @ ROW,01 SAY REPLICATE('-',46)
                  XFIRSTTIME = .F.
                ENDIF
                ROW = ROW + 1
                DO CASE
                  CASE XWIPSORT = 'D'
                    @ ROW,01 SAY XDATE
                    @ ROW,10 SAY XPO
                    @ ROW,17 SAY XVENDOR
                    @ ROW,23 SAY SUBSTR(XNAME,1,24)
                  CASE XWIPSORT = 'F'
                    @ ROW,01 SAY XVENDOR
                    @ ROW,08 SAY SUBSTR(XNAME,1,23)
                    @ ROW,32 SAY XPO
                    @ ROW,39 SAY XDATE
                  OTHERWISE
                    @ ROW,01 SAY XPO
                    @ ROW,08 SAY XVENDOR
                    @ ROW,14 SAY SUBSTR(XNAME,1,24)
                    @ ROW,39 SAY XDATE
                ENDCASE

                COL = 48
                *** Display WIP Total Line Item
                FOR I = 1 TO XCNT
                  @ ROW,COL SAY XPOS(I)  PICTURE '99999'
                  COL = COL + 7
                ENDFOR
                @ ROW,104 SAY XTOTPOS  PICTURE '999999'
                @ ROW,129 SAY XSTAT
              ENDIF

              IF EOF() .OR. STYLE <> lcStyleCod
                EXIT
              ENDIF
              STORE 0 TO XPOS, XTOTPOS
            ELSE
              SKIP 1
            ENDIF
          ENDDO

          IF XWIPRPT AND LLPRNTCB
            COL = 47
            XSUBTOT = 0
            ROW = ROW + 1
            @ ROW,47 SAY REPLICATE('-',85)
            ROW = ROW + 1
            @ ROW,21 SAY 'P.O. Subtotals .......'
            FOR I = 1 TO XCNT
              @ ROW,COL SAY XSUBPOS(I)  PICTURE '999999'
              XSUBTOT = XSUBTOT + XSUBPOS(I)
              COL = COL + 7
            ENDFOR
            @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
            ROW = ROW + 1
          ENDIF
        ENDIF
      ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    ENDIF

    IF IIF( lnTowDat = 0 , .T. , lnLopD <= lnTowDat )
      LLPRNTCB = .F.
      IF !('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
        SELECT STYLE
        LOCATE
        SEEK lcStyleCod
        IF FOUND()
          XFIRSTTIME = .T.
          IF ROW > 54 .AND. !EOF()
            ROW = 0
            PAGENO = PAGENO + 1
            DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
            ROW = 5
            *** Display Style, Description, And Scale Breakdown
            @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)

            SELECT STYLE
            COL = 50
            FOR I = 1 TO XCNT
              @ ROW,COL SAY XSIZE(I)
              COL = COL + 7
            ENDFOR
            @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
            ROW = ROW + 1
            @ ROW,01 SAY REPLICATE('-',131)
            XFIRSTTIME = .T.
          ENDIF

          SELECT STYLE
          FOR I = 1 TO XCNT
            Z = STR(I,1)
            XSTY(I) = WIP&Z
          ENDFOR
          FOR I = 1 TO XCNT
            XTOTSTY    = XTOTSTY + XSTY(I)
            XSUBSTY(I) = XSUBSTY(I) + XSTY(I)
          ENDFOR

          IF XWIPRPT .AND. XTOTPOS > 0
            *** Display WIP Heading

            COL = 48
            *** Display WIP Total Line Item
            FOR I = 1 TO XCNT
              @ ROW,COL SAY XSTY(I)  PICTURE '99999'
              COL = COL + 7
            ENDFOR
            @ ROW,104 SAY XTOTSTY  PICTURE '999999'
            @ ROW,129 SAY XSTAT
          ENDIF
          IF EOF() .OR. STYLE <> lcStyleCod
            EXIT
          ENDIF
          STORE 0 TO XSTY, XTOTSTY
        ENDIF
      ENDIF

      *** Get Sales From Orders Temporary File.
      SELECT (ORDTEMP)
      LOCATE
      SEEK lcStyleCod
      IF FOUND()
        XFIRSTTIME = .T.
        DO WHILE !EOF()
          IF (BETWEEN(COMPLETE , XSLDAT&lnNum , XSHDAT&lnNum) .OR. lnHowMany = 0 ) AND STYLE = lcStyleCod
            LLPRNTCB = .T.
            IF ROW > 54 .AND. !EOF()
              ROW = 0
              PAGENO = PAGENO + 1
              DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
              ROW = 5
              *** Display Style, Description, And Scale Breakdown
              @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)

              SELECT (ORDTEMP)
              COL = 50
              FOR I = 1 TO XCNT
                @ ROW,COL SAY XSIZE(I)
                COL = COL + 7
              ENDFOR
              @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
              ROW = ROW + 1
              @ ROW,01 SAY REPLICATE('-',131)
              XFIRSTTIME = .T.
            ENDIF

            XORDER   = ORDER
            XACCOUNT = ACCOUNT
            XDATE    = COMPLETE
            XSTART   = START
            XSTAT    = IIF(ORDHDR.APPROVAL = 'DECLINE' , 'D' , ORDHDR.STATUS)
            XPIKTKT  = IIF(PICKED = .T. , 'Y' , ' ')
            XPRICE   = PRICE
            SELECT CUSTOMER
            SEEK 'M'+XACCOUNT
            XNAME = IIF(FOUND() , BTNAME , SPACE(30))

            SELECT (ORDTEMP)
            FOR I = 1 TO XCNT
              Z = STR(I,1)
              XORD(I)    = IIF(QTY&Z > 0 , QTY&Z , 0 )
              XTOTORD    = XTOTORD + XORD(I)
              XSUBORD(I) = XSUBORD(I) + XORD(I)
            ENDFOR
            XEXT    = XTOTORD * XPRICE
            XSUBEXT = XSUBEXT + XEXT
            XGRDORD = XGRDORD + XTOTORD
            XGRDPRI = XGRDPRI + XEXT

            IF XORDRPT .AND. XTOTORD > 0
              *** Display ORD Heading
              IF XFIRSTTIME
                ROW = ROW + 1
                @ ROW,01 SAY XORDHDR
                ROW = ROW + 1
                @ ROW,01 SAY REPLICATE('-',46)
                XFIRSTTIME = .F.
              ENDIF

              ROW = ROW + 1
              DO CASE
                CASE XORDSORT = 'D'
                  @ ROW,01 SAY XSTART
                  @ ROW,10 SAY XDATE
                  @ ROW,19 SAY XORDER
                  @ ROW,26 SAY XACCOUNT
                  @ ROW,32 SAY SUBSTR(XNAME,1,15)
                CASE XORDSORT = 'A'
                  @ ROW,01 SAY XACCOUNT
                  @ ROW,07 SAY SUBSTR(XNAME,1,15)
                  @ ROW,23 SAY XORDER
                  @ ROW,30 SAY XSTART
                  @ ROW,39 SAY XDATE
                OTHERWISE
                  @ ROW,01 SAY XORDER
                  @ ROW,08 SAY XACCOUNT
                  @ ROW,14 SAY SUBSTR(XNAME,1,15)
                  @ ROW,30 SAY XSTART
                  @ ROW,39 SAY XDATE
              ENDCASE

              COL = 48
              *** Display ORD Total Line Item
              FOR I = 1 TO XCNT
                @ ROW,COL SAY XORD(I) PICTURE '99999'
                COL = COL+7
              ENDFOR
              @ ROW,104 SAY XTOTORD  PICTURE '999999'
              @ ROW,111 SAY XPRICE   PICTURE '9999.99'
              @ ROW,119 SAY XEXT     PICTURE '999999.99'
              @ ROW,129 SAY XSTAT
              @ ROW,131 SAY XPIKTKT
            ENDIF
            SKIP

            IF EOF() .OR. STYLE <> lcStyleCod
              EXIT
            ENDIF
            STORE 0 TO XORD, XTOTORD, XEXT
          ELSE
            SKIP 1
          ENDIF
        ENDDO
 
        IF XORDRPT AND LLPRNTCB
          COL = 47
          XSUBTOT = 0
          ROW = ROW + 1
          @ ROW,47 SAY REPLICATE('-',85)
          ROW = ROW + 1
          @ ROW,21 SAY 'Orders Subtotals .....'
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
            XSUBTOT = XSUBTOT + XSUBORD(I)
            COL = COL + 7
          ENDFOR
          @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
          @ ROW,118 SAY XSUBEXT  PICTURE '9999999.99'
          ROW = ROW + 1
        ENDIF
      ENDIF

      *** Display Summary Information.
      STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS

      IF ROW > 49
        ROW = 0
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
        ROW = 5
        @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)

        SELECT (ORDTEMP)
        COL = 50
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSIZE(I)
          COL = COL + 7
        ENDFOR
        @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
        ROW = ROW + 1
        @ ROW,01 SAY REPLICATE('-',131)
      ENDIF

      IF IIF( lnDatCount = 0 , .T. , lnLopD = 1 )
        COL = 47
        ROW = ROW + 1
        @ ROW,27 SAY 'STOCK  .........'
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSTK(I)  PICTURE '999999'
          XTOTSTK = XTOTSTK + XSTK(I)
          XOTS(I) = XOTS(I) + XSTK(I)
          COL = COL + 7
        ENDFOR
        @ ROW,103 SAY XTOTSTK    PICTURE '9999999'
      ENDIF

      ROW = ROW + 1
      COL = 46
      @ ROW,27 SAY 'W.I.P  .........'
      FOR I = 1 TO XCNT
        @ ROW,COL SAY XSUBCUT(I) + XSUBPOS(I) + XSUBSTY(I)  PICTURE '9999999'
        XTOTWIP = XTOTWIP + XSUBCUT(I) + XSUBPOS(I) + XSUBSTY(I)
        XOTS(I) = XOTS(I) + XSUBCUT(I) + XSUBPOS(I) + XSUBSTY(I)
        COL = COL + 7
      ENDFOR

      @ ROW,103 SAY XTOTWIP       PICTURE '9999999'
      ROW = ROW + 1
      COL = 47
      @ ROW,27 SAY 'ORDERS .........'
      FOR I = 1 TO XCNT
        @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
        XTOTORD = XTOTORD + XSUBORD(I)
        XOTS(I) = XOTS(I) - XSUBORD(I)
        COL = COL + 7
      ENDFOR
      @ ROW,103 SAY XTOTORD       PICTURE '9999999'

      ROW = ROW + 1
      @ ROW,27 SAY REPLICATE('-',105)
      ROW = ROW + 1
      COL = 47
      @ ROW,27 SAY 'O.T.S. .........'

     IF llCretArry
       IF ASUBSCRIPT(XADDOTS,ASCAN(XADDOTS,STYLE.STYLE),1) > 0
         lnStyPos = ASUBSCRIPT(XADDOTS,ASCAN(XADDOTS,STYLE.STYLE),1)
       ENDIF
     ENDIF

      FOR I = 1 TO XCNT
        IF lnHowMany # 0
          IF lnLopD = 1
            XADDOTS[lnStyPos , I+1] = XOTS(I)
          ELSE
            XOTS(I)                 = XOTS(I) + XADDOTS[lnStyPos , I+1]
            XADDOTS[lnStyPos , I+1] = XOTS(I)
          ENDIF
        ENDIF
        @ ROW,COL SAY XOTS(I)     PICTURE '999999'
        XTOTOTS = XTOTOTS + XOTS(I)
        COL = COL + 7
      ENDFOR
      @ ROW,103 SAY XTOTOTS       PICTURE '9999999'
      ROW = ROW + 1

    *B606147,1 BWA 06/24/2002 Fix the bug of arranging the periods within the styles.[START]
    *B606147,1 BWA 07/02/2002 Fix the bug of "operator type mismatch".[START]
    *IF IIF( lnDatContP = 0 , .T. , lnLopD = lnDatContP )
    IF IIF( lnTowDat = 0 , .T. , lnLopD = lnTowDat )
    *B606147,1 BWA 07/02/2002.[END]
    *B606147,1 BWA 06/24/2002.[END]

      SELECT STYLE
      SKIP
      IF EOF()
        ROW = ROW + 2
        IF ROW > 52
          ROW = 0
          PAGENO = PAGENO + 1
          DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
          ROW = 5
        ENDIF
        @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
        ROW = ROW + 1
        @ ROW,27 SAY 'TOTAL PIECES ORDERED:'
        @ ROW,49 SAY XGRDORD  PICTURE '99999999'
        @ ROW,62 SAY 'TOTAL $$$ ORDERED $'
        @ ROW,82 SAY XGRDPRI  PICTURE '999,999,999.99'
        ROW = ROW + 1
        @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
        EXIT
      ENDIF
      ROW = ROW + 3
    ENDIF
  ENDIF
  ENDFOR
ENDDO

RETURN

*--End of STY962.
*!*************************************************************
*! Name      : lfTermnate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Function to terminate the report if neither
*!           : the "Sales Order" nor the "Point of Sale" modules
*!           : is installed.
*!*************************************************************
*! Called from : Default value of the report variable (llTermnate).
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfTermnate

*-- If neither the "Sales Order" nor the "Point of Sale" module is installed
IF !('SO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  
  *** Message : "Since neither the Sales Order nor the Point of Sale module"
  ***           "is installed, you cannot run this report.                 "
  ***
  *** Buttons : "                         < Ok >                           "
  =gfModalgen("TRM42171B00000","ALERT")
  
  *-- Terminate the Option Grid.
  llOgTrmnat = .T.
ENDIF

*--End of lfTermnate.
*!**************************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Go top in the style IN RANGE.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSty.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 29/05/2002
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

PRIVATE lcAliasCst

lcAliasCst = SELECT(0)
SELECT CUSTOMER
SET ORDER TO CUSTOMER
GO TOP
SELECT(lcAliasCst)

*-- End of lfsrAcc.
*B606533,1 BWA 10/13/2002 function of deleting the temprory files.[START]
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/13/2002
*! Purpose   : deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*B606533,1 BWA 10/13/2002.[END]