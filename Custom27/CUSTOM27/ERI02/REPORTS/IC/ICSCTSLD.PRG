*:***************************************************************************
*: Program file  : ICSCTSLD
*: Program desc. : Custom Special Cut & Sold for Eric Javits. Copied from our standard
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Khalid Mohi El-Din (KHM)
*:***************************************************************************
*: C102809,1 04/15/2003 
*:***************************************************************************
*: B607261,1 ASH 05/14/2003 Collect booked qty based on the status.
*: C102842,1 ABD 10/06/2003 New modification to the report.
*: C102842,3 ASH 11/06/2003 New modification to the report.
*: B120384,1 ABD 12/24/2003 The report layout should skip to the next page if all the information down can not fit on the page.
*: C120872,1 ABD 02/05/2004 new Modification into the custom report.
*: B121315,1 ABD 02/05/2004 Wrong calculation for shipped qty.
*: B121016,1 ABD 02/08/2004 Print the total for all colors in case more than 10 colors.
*: C037661,1 NNA 03/02/2004 new Modification into the custom report By Show the total of only the
*: C037661,1 NNA            positive O.T.S Numbers
*: B122414,1 NNA 04/14/2004 Fix bug of that the report dose't print the style Description if the style
*: B122414,1 NNA            details displayed in two Pages
*: C123019,1 BWA 08/05/2004 New modification to the report.
*: B123770,1 ASH 09/12/2004 Fix problem of accumlative total.
*: B124547,1 BWA 09/21/2004 Fix the problem of subtract the return value from the shipped value.
*: C125377,1 NNA 03/03/2005 Make some modifications as follow :
*: C125377,1 NNA            1 - get and display Stock per warehouse
*: C125377,1 NNA            2 - add the Shipped and returned Qty. with the Grand total
*: C125377,1 NNA            2 - print start date before the complete date
*: B127047,1 NNA 05/06/2005 Fix bug that if you select print by size Yes , the price of the style not showing
*: B127507,1 EIH 05/15/2005 Fix bug that Correct totals if style has more than 10 colors and printed in 2 pages .
*: B127507,2 EIH 07/31/2005 Fix bug that Correct totals of orders and po lines if style has more than 10 colors .
*: B127507,2 EIH 07/31/2005 And Correct the stock totals.
*: B132304,1 HBG 05/31/2006 Reset variable of array position to fix the error while previow
*: B608174,1 NNA 07/24/2007 Fix bug that system save only  DATE SETTINGS FOR START DATES BUT NOT FOR ENTERED DATES OR SHIPPED DATES
*: C200948,1 NNA 02/22/2008 Add price B feild under the colors down every color
*: B609304,1 HES 06/15/2010 BOOKED NUMBER ON CUT AND SOLD REPORT IS WRONG [T20100507.0006]
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
*-- XSLDATE    variable that hold from sales order comp. date
*-- XSHDATE    variable that hold to   sales order comp. date
*-- XPLDATE    variable that hold from Prod. order comp. date
*-- XPHDATE    variable that hold to   Prod. order comp. date
*-- XWIPSORT   variable that showes if it is sorted by WIP or not
*-- XPRTORD    variable that showes print Sales Summary or Detail
*-- XORDSORT   variable that hold the sales order Sort
*-- XSTATHLD   variable that showes if include Orders on hold
*-- XALLOCAT   variable that showes if include Allocated Qty. or All Qty.
*-- XTITLE     variable that hold optional title
*-- XFILTER    variable that hold report filter expression

*C102809,1 04/15/2003 
*-- BOOKTEMP    variable that hold the name of OrdLine temp. file to print the booked Qty
*-- SHIPTEMP    variable that hold the name of InvLine temp. file to print the shipped Qty

*C120872,1 ABD - Define new variable. [Begin]
*-- RETTEMP      Variable that hold the name of retline temp. file to print the shipped qty - return qty.
*C120872,1 ABD - [End]

llDontPrn = .F.
PAGENO=0
ROW=0
XTIME=TIME()
R_WIDTH = ''
R_TITLE = ''

STORE SPACE(0) TO WORKTEMP,CUTTTEMP,POTEMP,ORDTEMP,XPRTWIP,XWIPRPT,;
                  XPRTORD,XORDRPT,XORDRPT,XSTAT

*B127507,1 EIH 05/15/2005 Declare array that hold previos totals [Begin].

*B127507,2 EIH 07/31/2005 Declare array that hold previos totals and warecode [Begin].
*DIMENSION xtotstkpg[90]                  
DIMENSION xtotstkpg[90,2]
*lnNewPos to hold the max used element of the array 
STORE 0 TO lnNewPos
*B127507,2 EIH 07/31/2005 [End].

STORE 0 TO lnTotRetSt
STORE 0 TO xtotstkpg                  
*B127507,1 EIH 05/15/2005 [End].


*C102809,1 04/15/2003 Initialize the variables that will hold temporary file
STORE '' TO BOOKTEMP,SHIPTEMP

*C125377,1 NNA 03/03/2005 (BEGIN) Initialize 2 Variables that will hold the Grand total
*C125377,1 NNA            Shipped and Returned Qty.
STORE 0 TO lnGTotShp , lnGTotRtrn
*C125377,1 NNA (END)

*C120872,1 ABD - Define new variable. [Begin]
*-- Variable that hold the name of retline temp. file to print the shipped qty - return qty.
RETTEMP = ''
*C120872,1 ABD - [End]

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))

XBYSIZE  = IIF(llRPBySize,'Y','N')
XSLDATE  = ldRPSLDate
XSHDATE  = ldRPSHDate
XPLDATE  = ldRPPLDate
XPHDATE  = ldRPPHDate
*C102809,1 04/15/2003 (Begin) Using the new option print open PO's instead of Print WIP
*XPRTWIP  = lcRPWIPSta
XPRTWIP  = lcRpPODtSm
*C102809,1 04/15/2003 (End)

XWIPSORT = lcRPWIPSor
XPRTORD  = lcRPSalSta
XORDSORT = lcRPSALSor
XSTATHLD = IIF(llRPHolOrd,'Y','N')
XALLOCAT = lcRPAllQty
XTITLE   = lcRPTitle

PRIVATE lcCustAcnt
STORE "" TO lcCustAcnt
llMore24 = .F.
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
      lcRPExp = STRTRAN(lcRPExp, lcCustAcnt , " .T. ")
      lcCustAcnt = "AND " + lcCustAcnt
    ELSE
      lcCustAcnt = SUBSTR(lcRpExp ,lnDatPost)
      lcRPExp = STRTRAN(lcRPExp, lcCustAcnt , " .T. )")
      lcCustAcnt = "AND (" + lcCustAcnt
    ENDIF
  ENDIF
ENDIF

XFILTER = lcRPExp

WAIT WINDOW 'Collecting data....' NOWAIT
DO lpCollecData
WAIT CLEAR

IF !llDontPrn
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
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

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- Check the cost access

DIMENSION laRPPrnItm[14]

llCostAccs = gfUserPriv('IC','ICSTYLE')

lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

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

*-- Disable/enable Only This colors, Free Segment. [end]

=lfvWIPSuDt()
=lfvSalSuDt()

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Get major seg. picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfvSODate
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Validate sales order date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSODate()
*!*************************************************************
FUNCTION lfvSODate

PRIVATE ldFrom,ldTo

IF EMPTY(ldRPSLDate) .AND. EMPTY(ldRPSHDate)
  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  ldFrom = ldRPSLDate
  ldTo = ldRPSHDate
ENDIF && end of (IF EMPTY(ldRPSLDate) )

lcTitle = 'Sales Order Start Date'
DO DateRng.Spx
ldRPSLDate = ldFrom
ldRPSHDate = ldTo

*!*************************************************************
*! Name      : lfvPODate
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Validate production order date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvPODate()
*!*************************************************************
FUNCTION lfvPODate

PRIVATE ldFrom,ldTo
IF EMPTY(ldRPPLDate) .AND. EMPTY(ldRPPHDate)
  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  ldFrom = ldRPPLDate
  ldTo = ldRPPHDate
ENDIF && end of (IF EMPTY(ldRPSLDate) )

lcTitle = 'Prod. order comp. date'

DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Validate OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************
FUNCTION lfvpbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvWIPSuDt
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Validate print WIP Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvWIPSuDt()
*!*************************************************************
FUNCTION lfvWIPSuDt

IF ('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  lcWIPSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPWIPSor'),1)
  laOGObjCnt[lcWIPSorPo] = lcRpPODtSm = 'D'
  = lfOGShowGet('lcRPWIPSor')
ENDIF

*!*************************************************************
*! Name      : lfvSalSuDt
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Validate print Sales Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSalSuDt()
*!*************************************************************
FUNCTION lfvSalSuDt

lcSalSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPSalSor'),1)
laOGObjCnt[lcSalSorPo] = lcRPSalSta = 'D'
= lfOGShowGet('lcRPSalSor')

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Validate Style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
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

*!*************************************************************
*! Name      : lpCollecData
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Collecting data in Temp. File
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : DO lpCollecData
*!*************************************************************
PROCEDURE lpCollecData

IF XPRTWIP='D'
  XWIPRPT=.T.
ELSE
  XWIPRPT=.F.
ENDIF

IF XPRTORD='D'
  XORDRPT=.T.
ELSE
  XORDRPT=.F.
ENDIF

XSTAT=IIF(XSTATHLD='N','O','HO')

IF XSLDATE<>CTOD('  /  /    ')
  XSHDATE=IIF(XSHDATE=CTOD('  /  /    '),XSLDATE,XSHDATE)
ENDIF

IF XPLDATE<>CTOD('  /  /    ')
  XPHDATE=IIF(XPHDATE=CTOD('  /  /    '),XPLDATE,XPHDATE)
ENDIF
SELECT STYLE
SET FILTER TO
*C102842,3 ASH 11/06/2003 (Begin) Add an option to print styles with 0 quantities.
*XFILTER=XFILTER+'.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'
XFILTER=XFILTER+'.AND. IIF(llPrintZero,.T.,(TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0))'
*C102842,3 ASH 11/06/2003 (End)
SET FILTER TO &XFILTER
GO TOP

IF EOF()
  Wait wind 'NO RECORDS SELECTED!!!'
  SET DEVICE TO SCREEN
  llDontPrn = .T.
  RETURN
ENDIF

********  EXTRACT RECORDS FROM CUTTING TICKET, PO, AND ORDERS **************

WORKTEMP = gfTEMPNAME()
CUTTTEMP = gfTEMPNAME()
POTEMP   = gfTEMPNAME()
ORDTEMP  = gfTEMPNAME()
*C102809,1 04/15/2003 (begin) Initializing the temporary files
BOOKTEMP = gfTEMPNAME()
SHIPTEMP = gfTEMPNAME()
*C102809,1 04/15/2003 (End)

*C120872,1 ABD - Define new variable. [Begin]
*-- Variable that hold the name of retline temp. file to print the shipped qty - return qty.
RETTEMP =  gfTEMPNAME()
*C120872,1 ABD - [End]

IF 'MF' $ gcComp_Mdl
 
  SELECT CUTTKTL
  SET ORDER TO CUTTKTLS
  SET RELATION TO CUTTKT INTO CUTTKTH
  COPY STRU TO &gcWorkDir.&CUTTTEMP
  USE &gcWorkDir.&CUTTTEMP IN 0 EXCLUSIVE
  SELECT (CUTTTEMP)
  SET RELATION TO CUTTKT INTO CUTTKTH
  
  *-- AAMER(Start)
  SET ORDER TO TktOper IN MFGOPRHD
  SELECT CUTTKTH
  SET RELATION TO 'M' + CutTkt INTO MFGOPRHD
  *-- AAMER(End)
  
  SELECT (CUTTTEMP)
  
  DO CASE
    CASE XBYSIZE='N' .AND. XPRTWIP='S'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE='Y' .AND. XPRTWIP='S'
      
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO(),7) TAG &CUTTTEMP
      
    CASE XBYSIZE='N' .AND. XWIPSORT='D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(CUTTKTH->COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='F'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+MFGOPRHD.cContCode+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='D'
      INDEX ON STYLE + DTOS(CUTTKTH.COMPLETE) + CUTTKT + TRANCD +;
               STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='F'
      INDEX ON STYLE + MFGOPRHD.cContCode + CUTTKT + TRANCD +;
               STR(RECNO() , 7) TAG &CUTTTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='P'
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO() , 7) TAG &CUTTTEMP
  ENDCASE
  
ENDIF    && End of IF 'MF' $ gcComp_Mdl


*-- If the "Style Purchase Order" or the "Point of Sale" Modules
*-- is installed
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  
  SELECT POSHDR
  SELECT POSLN
  SET RELATION TO cStyType + PO INTO POSHDR
  COPY STRU TO &gcWorkDir.&POTEMP
  USE &gcWorkDir.&POTEMP IN 0 EXCLUSIVE
  SELECT (POTEMP)
  
  SET RELATION TO cStyType + PO INTO POSHDR
  DO CASE
    CASE XBYSIZE='N' .AND. XPRTWIP='S'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='Y' .AND. XPRTWIP='S'
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(POSHDR->COMPLETE)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='F'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+VENDOR+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='D'
      INDEX ON STYLE + DTOS(POSHDR.COMPLETE) + PO + TRANCD +;
               STR(RECNO() , 7) TAG &POTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='F'
      INDEX ON STYLE + VENDOR + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='P'
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
  ENDCASE
ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

*C102809,1 04/15/2003 (Begin)
SELECT InvLine
SET RELATION TO Invoice INTO InvHdr ADDITIVE
*C102809,1 04/15/2003 (End)

*C123019,1 BWA 08/05/2004 New modification to the report.[START]
SELECT RETLINE
SET RELATION TO CRMEMO INTO RETHDR ADDITIVE
*C123019,1 BWA 08/05/2004.[END]


SELECT ORDHDR
SELECT ORDLINE

SET RELATION TO cOrdType + Order INTO ORDHDR


COPY STRU TO &gcWorkDir.&ORDTEMP
USE &gcWorkDir.&ORDTEMP IN 0 EXCLUSIVE
SELECT (ORDTEMP)

SET RELATION TO cOrdType + Order INTO ORDHDR

DO CASE
  CASE XBYSIZE='N' .AND. XPRTORD='S'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='Y' .AND. XPRTORD='S'
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
  CASE XBYSIZE='N' .AND. XORDSORT='D'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(COMPLETE)+ORDER+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='N' .AND. XORDSORT='A'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ACCOUNT+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='N' .AND. XORDSORT='O'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='Y' .AND. XORDSORT='D'
    INDEX ON STYLE + DTOS(COMPLETE) + ORDER + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
  CASE XBYSIZE='Y' .AND. XORDSORT='A'
    INDEX ON STYLE + ACCOUNT + ORDER + DTOS(COMPLETE) + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
  CASE XBYSIZE='Y' .AND. XORDSORT='O'
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
ENDCASE

*C102809,1 04/15/2003 (Begin)
COPY STRU TO &gcWorkDir.&BOOKTEMP
USE &gcWorkDir.&BOOKTEMP IN 0 EXCLUSIVE
SELECT (BOOKTEMP)
INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
SELECT InvLine

COPY STRU TO &gcWorkDir.&SHIPTEMP
USE &gcWorkDir.&SHIPTEMP IN 0 EXCLUSIVE
SELECT (SHIPTEMP)
INDEX ON SUBSTR(STYLE,1,lnMajLen)+Invoice+DTOS(InvDate) TAG &SHIPTEMP
*C102809,1 04/15/2003 (End)

*C120872,1 ABD - Define new variable. [Begin]
*-- Variable that hold the name of retline temp. file to print the shipped qty - return qty.
*=gfOpenFile(gcDataDir+'RETHDR','RETHDR','SH')
*=gfOpenFile(gcDataDir+'RETLINE','Retlines','SH')
SELECT RETLINE
COPY STRU TO &gcWorkDir.&RETTEMP
USE &gcWorkDir.&RETTEMP IN 0 EXCLUSIVE
SELECT (RETTEMP)
INDEX ON SUBSTR(STYLE,1,lnMajLen)+crmemo+DTOS(crdate) TAG &RETTEMP
*SELECT RETLINE
*SET RELATION TO crmemo INTO RETHDR
*C120872,1 ABD - [End]

*C123019,1 BWA 08/05/2004 Create temp. retline.[START]
*SELECT RETLINE
*COPY STRU TO &gcWorkDir.&RETRNEMP
*USE &gcWorkDir.&RETRNEMP IN 0 EXCLUSIVE
*SELECT (RETRNEMP)
*INDEX ON SUBSTR(STYLE,1,lnMajLen)+Crmemo+DTOS(CRDATE) TAG &RETRNEMP
*C123019,1 BWA 08/05/2004.[END]

*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to OrdLine according 
  *-- to the CutOrd index.
  =gfOpenFile(gcDataDir+'CutPick','CutOrd','SH')
  SELECT OrdLine 
  SET RELATION TO '2'+Order+STR(LineNo,6) INTO CutPick ADDITIVE

ENDIF    && End of IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

*-- If the "Style Purchase Order" or the "Point of Sale" modules
*-- is installed
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  
  *-- Openning the file CutPick with a relation to PosLn according 
  *-- to the CutPick index in another work area.
  SELECT 0 
  USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick2
  SET ORDER TO CutPick
  SELECT PosLn 
  SET RELATION TO '2'+PO+Style INTO CutPick2 ADDITIVE
  
ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl


*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  
  *-- Openning the file CutPick with a relation to OrdLine according 
  *-- to the CutOrd index in another work area to check if the order 
  *-- was allocated by cuttkt not by PO.
  SELECT 0 
  USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick3
  SET ORDER TO CutOrd
  SELECT OrdLine 
  SET RELATION TO '1'+Order+STR(LineNo,6) INTO CutPick3 ADDITIVE
  
ENDIF    && End of IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

SELE STYLE
GO TOP

XDESC=SPACE(20)
XSTYLE=SPACE(19)         
XCOLOR=SPACE(19)         

*** SAVE OFF RECORDS INTO THEIR RESPECTIVE TEMPORARY FILE
SCAN
  IF INKEY()=32
    RETURN
  ENDIF

  xStyle = Style
  
  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl
    
    *** Store off Records Into First Temp File CUTTTEMP
    SELE CUTTKTL
    SEEK XSTYLE
    IF FOUND()
      IF XPLDATE<>CTOD('  /  /    ')
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) FOR (BETWEEN(CUTTKTH->COMPLETE,XPLDATE,XPHDATE) .AND. CUTTKTH->STATUS<>'X' .AND. CUTTKTH->STATUS<>'C' .AND. TOTQTY<>0)
      ELSE
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) FOR (CUTTKTH->STATUS<>'X' .AND. CUTTKTH->STATUS<>'C' .AND. TOTQTY<>0)
      ENDIF
      SELE &CUTTTEMP
      APPEND FROM &gcWorkDir.&WORKTEMP
    ENDIF
    
  ENDIF    && End of IF 'MF' $ gcComp_Mdl
  
  *-- If the " Style Purchase Order " or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    
    *** Store off Records Into Second Temp File POTEMP
    SELE POSLN
    SEEK XSTYLE
    IF FOUND()

      IF !('POSLN.ACCOUNT INTO CUSTOMER' $ SET('RELATION'))
        SET RELATION TO "M" + Posln.ACCOUNT INTO Customer ADDITIVE
      ENDIF  

      XADD = ''
      DO CASE
        CASE XALLOCAT = 'L'
          XADD = ' .AND.  CutPick2->CtktNo = PosLn->Po'
        CASE XALLOCAT = 'N'
          XADD = ' .AND.  CutPick2->CtktNo <> PosLn->Po'
      ENDCASE

      IF XPLDATE<>CTOD('  /  /    ')

        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
             FOR (BETWEEN(POSHDR->COMPLETE,XPLDATE,XPHDATE) .AND. ;
             POSHDR->STATUS<>'X' .AND. POSHDR->STATUS<>'C' .AND. ;
             TOTQTY<>0 &XADD &lcCustAcnt)
      ELSE

        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
             FOR (POSHDR->STATUS<>'X' .AND. POSHDR->STATUS<>'C' .AND. ;
             TOTQTY<>0 &XADD &lcCustAcnt)
      ENDIF

      SELECT POSLN
      IF 'POSLN.ACCOUNT INTO CUSTOMER' $ SET('RELATION')
        SET RELATION OFF INTO Customer
      ENDIF  

      SELE &POTEMP
      APPEND FROM &gcWorkDir.&WORKTEMP

    ENDIF
    
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *** Store off Records Into Third Temp File ORDTEMP
  SELE ORDLINE
  SEEK XSTYLE
  IF FOUND()

    IF !('ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION'))
      SET RELATION TO "M" + ORDLINE.ACCOUNT INTO Customer ADDITIVE
    ENDIF  

    XADDORD = ''
    
    
    *-- If the "Manufacturing" or the "Style Purchase Order" or the
    *-- "Point of Sale" modules is installed
    IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
      
      DO CASE
        CASE XALLOCAT = 'L'
          XADDORD = ' .AND.  (OrdLine->Order = CutPick->Order .OR. ' + ;
                    ' OrdLine->Order = CutPick3->Order) '
        CASE XALLOCAT = 'N'
          XADDORD = ' .AND.  (OrdLine->Order <>CutPick->Order .AND.' + ;
                    ' OrdLine->Order <> CutPick3->Order) '
      ENDCASE
       
    ENDIF    && End of IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

    IF XSLDATE<>CTOD('  /  /    ')
      *C102809,1 04/15/2003 (Begin) Using the start date instead of complete date
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style = xStyle ;
            FOR BETWEEN(Complete , xSLDate , xSHDate)   .AND. ;
                ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
      
      COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
            FOR BETWEEN(Start , xSLDate , xSHDate)   .AND. ;
                ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
      *C102809,1 04/15/2003 (End)

    ELSE
      
      COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
            FOR ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
    ENDIF

    SELECT ORDLINE
    IF 'ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION')
      SET RELATION OFF INTO Customer
    ENDIF  

    SELE &ORDTEMP
    APPEND FROM &gcWorkDir.&WORKTEMP
  
    *C102809,1 04/15/2003 (Begin) Getting the booked qty
    SELE ORDLINE
    SEEK XSTYLE

    *B608174,1 NNA 07/24/2007 (Begin) Change ldSoSEntDt to ldRPSoStDt and  ldSoHEntDt to ldRPSoEnDt
    *IF ldSoSEntDt <> CTOD('  /  /    ')
      *B607261,1 ASH 05/14/2003 (Begin) Don't compute booked qty for cancelled orders.
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
            FOR BETWEEN(OrdHdr.Entered , ldSoSEntDt , ldSoHEntDt) .AND. ;
                cOrdType = 'O'
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
            FOR BETWEEN(OrdHdr.Entered , ldSoSEntDt , ldSoHEntDt) .AND. ;
                ORDHDR.Status <> 'X' .AND. cOrdType = 'O' 

*: B609304,1 HES 06/15/2010 including the selected Start date range in the filter [Start]
*!*	    IF ldRPSoStDt <> CTOD('  /  /    ')
*!*	      COPY REST TO &gcWorkDir.&WORKTEMP ;
*!*	          WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
*!*	            FOR BETWEEN(OrdHdr.Entered , ldRPSoStDt , ldRPSoEnDt) .AND. ;
*!*	                ORDHDR.Status <> 'X' .AND. cOrdType = 'O' 
*!*	                               
*!*	    *B608174,1 NNA (End)

*!*	      *B607261,1 ASH 05/14/2003 (End)
*!*	    ELSE
*!*	      *B607261,1 ASH 05/14/2003 (Begin) Don't compute booked qty for cancelled orders.
*!*	      *COPY REST TO &gcWorkDir.&WORKTEMP ;
*!*	           WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
*!*	           FOR   cOrdType = 'O' 
*!*	      
*!*	      COPY REST TO &gcWorkDir.&WORKTEMP ;
*!*	           WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
*!*	           FOR   cOrdType = 'O' AND ORDHDR.Status <> 'X'
*!*	       *B607261,1 ASH 05/14/2003 (End)
*!*	    ENDIF
	DO CASE 
	  CASE !EMPTY(ldRPSoStDt) AND EMPTY(ldRPSLDate)
        COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
            FOR BETWEEN(OrdHdr.Entered , ldRPSoStDt , ldRPSoEnDt) .AND. ;
                ORDHDR.Status <> 'X' .AND. cOrdType = 'O' 
                	    
	  CASE !EMPTY(ldRPSoStDt) AND !EMPTY(ldRPSLDate)
        COPY REST TO &gcWorkDir.&WORKTEMP ;
            WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
              FOR BETWEEN(OrdHdr.Entered , ldRPSoStDt , ldRPSoEnDt) .AND. ;
                  BETWEEN(OrdHdr.Start   , ldRPSLDate , ldRPSHDate) .AND. ;
                  ORDHDR.Status <> 'X' .AND. cOrdType = 'O' 
                  	    
	  CASE EMPTY(ldRPSoStDt) AND !EMPTY(ldRPSLDate)
        COPY REST TO &gcWorkDir.&WORKTEMP ;
            WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
              FOR BETWEEN(OrdHdr.Start   , ldRPSLDate , ldRPSHDate) .AND. ;
                  ORDHDR.Status <> 'X' .AND. cOrdType = 'O' 		    
	  OTHERWISE 
        COPY REST TO &gcWorkDir.&WORKTEMP ;
             WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = xStyle ;
             FOR   cOrdType = 'O' AND ORDHDR.Status <> 'X'	      
	ENDCASE 
*: B609304,1 HES 06/15/2010 including the selected Start date range in the filter [End  ]


    SELECT (BOOKTEMP)
    APPEND FROM &gcWorkDir.&WORKTEMP
    *C102809,1 04/15/2003 (End)

  ENDIF
  
  *C102809,1 04/15/2003 (Begin) Getting the shipped qty.
  IF SEEK(xStyle, 'InvLine')
    SELECT InvLine
    
    *B608174,1 NNA 07/24/2007 (Begin) Change ldSoSEntDt to ldRPSoStDt and  ldSoHEntDt to ldRPSoEnDt
    *IF ldStShipDt <> CTOD('  /  /    ')
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style+Invoice+STR(LineNo,6) = xStyle ;
          FOR BETWEEN(InvHdr.ShipDate , ldStShipDt , ldToShipDt) .AND. TotQty <> 0;
              AND InvHdr.Status <> 'V'
    IF ldRPSShpDt <> CTOD('  /  /    ')
      COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style+Invoice+STR(LineNo,6) = xStyle ;
          FOR BETWEEN(InvHdr.ShipDate , ldRPSShpDt , ldRPTShpDt) .AND. TotQty <> 0;
             AND InvHdr.Status <> 'V'
    *B608174,1 NNA (End)

    ELSE
      
      COPY REST TO &gcWorkDir.&WORKTEMP ;
           WHILE Style+Invoice+STR(LineNo,6) = xStyle ;
           FOR InvHdr.Status <> 'V' AND TotQty <> 0 
    ENDIF

    SELECT (SHIPTEMP)
    APPEND FROM &gcWorkDir.&WORKTEMP

  ENDIF
  *C102809,1 04/15/2003 (End)

  *C123019,1 BWA 08/05/2004 Collect the return records.[START]
  *IF SEEK(xStyle, 'RETLINE')
  *  SELECT RETLINE
    
  *  IF ldStShipDt <> CTOD('  /  /    ')
  *    COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style + Crmemo = xStyle ;
          FOR BETWEEN(RETHDR.CRDATE , ldStShipDt , ldToShipDt) .AND. TotQty <> 0;
              AND RETHDR.STATUS <> 'V'
   * ELSE
   *   COPY REST TO &gcWorkDir.&WORKTEMP ;
           WHILE Style + Crmemo = xStyle ;
           FOR RETHDR.STATUS <> 'V' AND TotQty <> 0 
   * ENDIF

    *SELECT (RETRNEMP)
    *APPEND FROM &gcWorkDir.&WORKTEMP

  *ENDIF
  *C123019,1 BWA 08/05/2004.[END]
  
  *C120872,1 ABD - get the retline temp. file to print the shipped qty - return qty. [Begin]
  IF SEEK(xStyle, 'RETLINE')
    SELECT RETLINE
    
    *B608174,1 NNA 07/24/2007 (Begin) Change ldStShipDt to ldRPSShpDt 
    *IF ldStShipDt = CTOD('  /  /    ')
    IF ldRPSShpDt = CTOD('  /  /    ')
    *B608174,1 NNA (End)
    
      COPY REST TO &gcWorkDir.&WORKTEMP ;
           WHILE style+crmemo = xStyle ;
           FOR rethdr.status <> 'V' AND TotQty <> 0 
    ELSE
      
      *B608174,1 NNA 07/24/2007 (Begin) Change ldSoSEntDt to ldRPSoStDt and  ldSoHEntDt to ldRPSoEnDt
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE style+crmemo = xStyle ;
          FOR BETWEEN(retline.crdate , ldStShipDt , ldToShipDt) .AND. TotQty <> 0;
              AND rethdr.status <> 'V'
      COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE style+crmemo = xStyle ;
          FOR BETWEEN(retline.crdate , ldRPSShpDt , ldRPTShpDt) .AND. TotQty <> 0;
              AND rethdr.status <> 'V'
      *B608174,1 NNA (End)

    ENDIF

    SELECT (RETTEMP)
    APPEND FROM &gcWorkDir.&WORKTEMP

  ENDIF
  *C120872,1 ABD - [End]
  
  SELE STYLE
ENDSCAN

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF    && End of IF USED('CUTPICK')

IF USED('CUTPICK2')
  USE IN CUTPICK2
ENDIF    && End of IF USED('CUTPICK2')

IF USED('CUTPICK3')
  USE IN CUTPICK3
ENDIF    && End of IF USED('CUTPICK3')

PAGENO=0
ROW=99
XTIME=TIME()
R_WIDTH='W'
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

*!*************************************************************
*! Name      : lpPrint
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Print report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint
*- Abdou
IF gcDevice='PRINTER'
  *@ PROW(),PCOL() SAY [&l#O1]
*  @ PROW(),PCOL() SAY [&l1O&l14c1E]
ENDIF

*- Abdou
IF XBYSIZE='N'
  DO STY961                                 && No Breakdown In Size
ELSE
  DO STY962                                 && Breakdown In Size
ENDIF

*- Abdou
*--Release Landscape
SET DEVICE TO PRINTER
IF gcDevice='PRINTER'
  *@ PROW(),PCOL() SAY [&l#O0]
 * @ PROW(),PCOL() SAY [&l0O]
ENDIF

*- Abdou

****************************************************************************
* PROG: STY961.PRG
* DATE: 02/20/92
* DESC: REPORT WITHOUT SIZE BREAKDOWN
****************************************************************************
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

*** Declare & Initialize Variables & Array
STORE SPACE(6)  TO XCUTTKT, XPO, XORDER
STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
STORE SPACE(6)  TO XCOLOR         &&TAK06/21/94
STORE CTOD('  /  /    ') TO XDATE

*C037661,1 NNA 03/02/2004 (Begin) Add new variable for the Grand total of only Positive O.T.S numbers
*STORE 0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD
STORE  0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD,XGRDOTS
*C037661,1 NNA (End)

DIMENSION  XCOLOR(10),XCLR(10),XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),;
          XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10),XSTY(10),XSUBSTY(10), laTotShp(10)

*C200948,1 NNA 02/22/2008(Begin) declare an array to hold the price B
DIMENSION XPRICE_B(10)
*C200948,1 (End)

*C120872,1 ABD - define new variable to print the total vendors Po's at the end of grand total. [Begin]
XGRDPos = 0
*C120872,1 ABD - [End]

STORE 0 TO laTotShp

*C120872,1 ABD - Define new variable. [Begin]
STORE "" TO lcStyleTyp , lcPricA_C
*C120872,1 ABD - [Ende]

*C102809,1 04/15/2003 (Begin) Create the temporary file that will print the in-transit
lcTmpShpFl = gfTEMPNAME()
=lfCreatFl()
*C102809,1 04/15/2003 (End)

*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE XWIPSORT='D'                          && Sort by Completion Date
    XCUTSCAN='(CUTTKTH->COMPLETE=XDATE .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN='(POSHDR->COMPLETE=XDATE .AND. PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR='COMPLETE CUTTKT CONTR1'
    
    *C120872,1 ABD - add the entered date to the header of the report. [Begin]
    *XPOHDR='COMPLETE PO #   VENDOR'
    XPOHDR='ENTERED  COMPLETE PO #   VENDOR'
    *C120872,1 ABD - [End]
    
  CASE XWIPSORT='F'                          && Sort by Contractor
    XCUTSCAN='(MFGOPRHD.cContCode=XCONTR1 .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN='(POSHDR->VENDOR=XVENDOR .AND. PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR='CONTR1 CUTTKT COMPLETE'

    *C120872,1 ABD - add the entered date to the header of the report. [Begin]
    *XPOHDR='VENDOR PO #   COMPLETE'
    XPOHDR='VENDOR  PO #   COMPLETE ENTERED'
    *C120872,1 ABD - [End]
    
  OTHERWISE                                 && Sort by Cutting Ticket/PO
    XCUTSCAN='(CUTTKTH->CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN='(PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR='CUTTKT CONTR1 COMPLETE'
    
    *C120872,1 ABD - add the entered date to the header of the report. [Begin]
    *XPOHDR='PO #   VENDOR COMPLETE'
    XPOHDR='PO #   VENDOR  COMPLETE ENTERED'
    *C120872,1 ABD - [End]
    
ENDCASE

*** Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  CASE XORDSORT='D'                         && Sort by Completion Date
    XORDSCAN='(COMPLETE=XDATE .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
    *XORDHDR='COMPLETE ORDER   ACCT#'
    *C125377,1 NNA 03/03/2005 (Begin) print Start date before the Complete date
    *XORDHDR='COMPLETE ORDER  ACCT# START'
    XORDHDR='START    ORDER  ACCT# COMPLETE'
    *C125377,1 NNA (End)
    *C102842,3 ASH 11/06/2003 (End)
  CASE XORDSORT='A'                         && Sort by Account
    XORDSCAN='(ACCOUNT=XACCOUNT .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
    *XORDHDR='ACCT# ORDER   COMPLETE'
    *C125377,1 NNA 03/03/2005 (Begin) print Start date before the Complete date
    *XORDHDR='ACCT# ORDER  COMPLETE START'
    XORDHDR='ACCT# ORDER  START    COMPLETE'
    *C125377,1 NNA (End)
    *C102842,3 ASH 11/06/2003 (End)
  OTHERWISE                                 && Sort by Orders
    XORDSCAN='(ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
    *XORDHDR='ORDER  ACCT#  COMPLETE'
    *C125377,1 NNA 03/03/2005 (Begin) print Start date before the Complete date
    *XORDHDR='ORDER  ACCT# COMPLETE START'
    XORDHDR='ORDER  ACCT# START    COMPLETE'
    *C125377,1 NNA (End)
    *C102842,3 ASH 11/06/2003 (End)
ENDCASE



*B127507,2 EIH 07/31/2005 Create temp file to fix the totals of order's and po's [Begin].
lcStyOrdPg= gfTEMPNAME()
CREATE TABLE (gcWorkDir+lcStyOrdPg)(style C(19) ,order C(6) ,nTColsPg N(7))
INDEX ON (Style+Order) TAG (lcStyOrdPg) OF (lcStyOrdPg)

lcStyPoPg= gfTEMPNAME()
CREATE TABLE (gcWorkDir+lcStyPoPg)(style C(19) ,PO C(6) ,nTPosPg N(7))
INDEX ON (Style+PO) TAG (lcStyPoPg) OF (lcStyPoPg)

*B127507,2 EIH 07/31/2005 [End].
SELE STYLE
GO TOP
DO WHILE INKEY() <> 32
  XSTYLE=SUBSTR(STYLE,1,lnMajLen)
  XDESC=DESC

  *B127507,1 EIH 05/15/2005 Restart array that hold previos totals [Begin].
  STORE 0 TO xtotstkpg 
  STORE 0 TO lnTotRetSt 
  *B132304,1 HBG 05/31/2006 Reset variable of array position [Begin]
  STORE 0 TO lnNewPos
  *B132304,1 [End]
  *B127507,1 EIH 05/15/2005 [End].
  
  STORE SPACE(5) TO XCLR
  STORE SPACE(6) TO XCOLOR

  *C200948,1 NNA 02/22/2008(Begin) initialize the array xprice_b
  STORE 0 TO XPRICE_B 
  *C200948,1 (End)
   
  *C120872,1 ABD - Assign the type of style and the price A and C.  [Begin]
  lcStyleTyp = IIF(Style.Make,'DOM','IMP')
  lcPricA_C  = 'PRICE A/C : '+ALLTRIM(STR(Style.PriceA,13,2))+' / '+ALLTRIM(STR(Style.PriceC,13,2))
  *C120872,1 ABD - [End]
  
  *C102809,1 04/15/2003 (Begin) Commented out.
  *STORE 0 TO XSTK, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD,;
             XSUBORD, XTOTORD, XOTS,XTOTSTY,XSTY,XSUBSTY, laPOShpQty, laOpnPoQty, laBookQty, laShipQty
  *C102809,1 04/15/2003 (End)
  
  *** To Get Number of Color For Each Style
  COUNT ALL FOR (SUBSTR(STYLE,1,lnMajLen)=XSTYLE) TO XCLRCNT

  *** Load Array With Color and Stock
  SEEK XSTYLE                               && To Place Pointer Back to 1st Match
  *C102809,1 04/15/2003 (Begin)
  lnLoop = CEILING(XCLRCNT/10)
  FOR lnStyCnt = 1 to lnLoop
    *C123019,1 BWA 08/05/2004 Add variables for the return data.[START]
    *DIMENSION XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10),;
    *          XSTY(10),XSUBSTY(10),laPOShpQty[10], laOpnPoQty[10], laBookQty[10], laShipQty[10]
    *    STORE 0 TO XSTK,XCUT,XSUBCUT,XPOS,XSUBPOS,XORD,XSUBORD,XOTS,XSTY,XSUBSTY,laPOShpQty,;
    *           laOpnPoQty,laBookQty,laShipQty


    DIMENSION XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10) ,;
              XSTY(10),XSUBSTY(10),laPOShpQty[10], laOpnPoQty[10], laBookQty[10], laShipQty[10],;
              laRtrnQty[10]

    STORE 0 TO XSTK, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD,;
               XSUBORD, XTOTORD, XOTS,XTOTSTY,XSTY,XSUBSTY, laPOShpQty,;
               laOpnPoQty, laBookQty, laShipQty , laRtrnQty
    *C123019,1 BWA 08/05/2004.[END]
  *C102809,1 04/15/2003 (End)

    XCNT=IIF(XCLRCNT>10, 10, XCLRCNT)

    FOR I=1 TO XCNT
      XCLR(I)   = SUBSTR(STYLE.Style,lnNonMajPo)
      XCOLOR(I) = SUBSTR(STYLE.Style,lnNonMajPo)
      XSTK(I)   = TOTSTK
      *C200948,1 NNA 02/22/2008(Begin) fill the array with PriceB for the style/color
      XPRICE_B(I) = STYLE.PRICEB
      *C200948,1 (End)

      *C125377,1 NNA 03/03/2005 (Begin) Get the Total Stock per Style/Color/WareHouse
      = lfStoreStk()
      *C125377,1 NNA (End)
      SKIP
    ENDFOR

   *B121016,1 ABD - Define new variables to hold the subtotal.  [Begin]
   IF lnStyCnt = 1 
     STORE 0 TO ln_SubToPo , ln_SubToCt , ln_SubtOrd , ln_totPos , ln_totcOrd , ln_totCut
   ENDIF 
   *B121016,1 ABD - [End]

   *B120384,1 ABD - The report layout should skip to the next page if all the information down can not fit on the page. [Begin]
   *IF ROW > 54
   *IF ROW > 49
   IF ROW > 46
   *B120384,1 ABD -  [End]
     ROW=0
     PAGENO=PAGENO+1

      
      DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH
      
      ROW=5
    ENDIF
    *- Abdou
    IF gcDevice='PRINTER'
      @ PROW(),PCOL() SAY [(s3B]
    ENDIF
    *- Abdou

    *** Display Style, Description, And Range Of Colors
    @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
    
    COL=34
    FOR I=1 TO XCNT
      IF LEN(TRIM(XCLR(I))) < 4
        XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=1,'    '+TRIM(XCLR(I)),XCLR(I))
        XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=2,'   '+TRIM(XCLR(I)),XCLR(I))
        XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=3,'  '+TRIM(XCLR(I)),XCLR(I))
      ENDIF
      @ ROW,COL SAY XCLR(I)
      COL=COL+9
    ENDFOR
    *- Abdou
    IF gcDevice='PRINTER'
      @ PROW(),PCOL() SAY [(s0B]
    ENDIF
    *- Abdou
      
    
    IF ROW=5
      @ ROW,125 SAY 'TOTAL'
    ENDIF

    *C200948,1 NNA 02/22/2008(Begin) Print the Price B under its colors
    ROW=ROW+1
    COL=LEN(XSTYLE + ' ' + LEFT(XDESC,19))+2
    FOR I=1 TO XCNT
      IF XPRICE_B(I)>0
        @ ROW,COL SAY PADR(XPRICE_B(I),6,' ')
      ENDIF
      COL=COL+9
    ENDFOR
    *C200948,1 (End)

    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('-',129)

    *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
    ROW=ROW+1
    *C120872,1 ABD - Print the style type and price A & C. [Begin]
    *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
    @ ROW,13 SAY lcStyleTyp
    @ ROW,20 SAY lcPricA_C
    *C120872,1 ABD - [End]

    *-- If the "Manufacturing" module is installed
    IF 'MF' $ gcComp_Mdl
    
      *** Get WIP From Cutting Ticket Temporary File
      SELE &CUTTTEMP
      GO TOP
      SEEK XSTYLE
      IF FOUND()
        XFIRSTTIME=.T.
        DO WHILE INKEY()<>32
          IF ROW > 54 .AND. !EOF()
            ROW=0
            PAGENO=PAGENO+1
            
            
            DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH

            *- Abdou
            IF gcDevice='PRINTER'
              @ PROW(),PCOL() SAY [(s3B]
            ENDIF
            *- Abdou
            
            ROW=5
            *** Display Style, Description, And Range Of Colors

            *B122414,1 NNA 04/14/2004 (Begin) Always Show the style with his Discreption
            *@ ROW,01 SAY XSTYLE
            @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
            *B122414,1 NNA (End)

            COL=34
            FOR I=1 TO XCNT
              @ ROW,COL SAY XCLR(I)
              COL=COL+9
            ENDFOR
            *- Abdou
            IF gcDevice='PRINTER'
              @ PROW(),PCOL() SAY [(s0B]
            ENDIF
            
            @ ROW,125 SAY 'TOTAL'
            ROW=ROW+1
            @ ROW,01 SAY REPLICATE('-',129)
 
            *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
            ROW=ROW+1
            
            *C120872,1 ABD - Print the style type and price A & C. [Begin]
            *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
            @ ROW,13 SAY lcStyleTyp
            @ ROW,20 SAY lcPricA_C
            *C120872,1 ABD - [End]
   
            XFIRSTTIME=.T.
          ENDIF
          
          XCUTTKT=CUTTKT
          XDATE=CUTTKTH->COMPLETE
          XCONTR1 = MFGOPRHD.cContCode
          SCAN WHILE &XCUTSCAN
            DO CASE
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
                XCUT(1)=IIF(TRANCD='1',XCUT(1)+TOTQTY, XCUT(1)-TOTQTY)
                XCUT(1)=IIF(XCUT(1)>0, XCUT(1), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
                XCUT(2)=IIF(TRANCD='1',XCUT(2)+TOTQTY, XCUT(2)-TOTQTY)
                XCUT(2)=IIF(XCUT(2)>0, XCUT(2), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
                XCUT(3)=IIF(TRANCD='1',XCUT(3)+TOTQTY, XCUT(3)-TOTQTY)
                XCUT(3)=IIF(XCUT(3)>0, XCUT(3), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
                XCUT(4)=IIF(TRANCD='1',XCUT(4)+TOTQTY, XCUT(4)-TOTQTY)
                XCUT(4)=IIF(XCUT(4)>0, XCUT(4), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
                XCUT(5)=IIF(TRANCD='1',XCUT(5)+TOTQTY, XCUT(5)-TOTQTY)
                XCUT(5)=IIF(XCUT(5)>0, XCUT(5), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
                XCUT(6)=IIF(TRANCD='1',XCUT(6)+TOTQTY, XCUT(6)-TOTQTY)
                XCUT(6)=IIF(XCUT(6)>0, XCUT(6), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
                XCUT(7)=IIF(TRANCD='1',XCUT(7)+TOTQTY, XCUT(7)-TOTQTY)
                XCUT(7)=IIF(XCUT(7)>0, XCUT(7), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
                XCUT(8)=IIF(TRANCD='1',XCUT(8)+TOTQTY, XCUT(8)-TOTQTY)
                XCUT(8)=IIF(XCUT(8)>0, XCUT(8), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
                XCUT(9)=IIF(TRANCD='1',XCUT(9)+TOTQTY, XCUT(9)-TOTQTY)
                XCUT(9)=IIF(XCUT(9)>0, XCUT(9), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
                XCUT(10)=IIF(TRANCD='1',XCUT(10)+TOTQTY, XCUT(10)-TOTQTY)
                XCUT(10)=IIF(XCUT(10)>0, XCUT(10), 0)
              OTHERWISE
                XCUT(10)=IIF(TRANCD='1',XCUT(10)+TOTQTY, XCUT(10)-TOTQTY)
                XCUT(10)=IIF(XCUT(10)>0, XCUT(10), 0)
            ENDCASE
          ENDSCAN
  
          FOR I=1 TO XCNT
            XTOTCUT=XTOTCUT+XCUT(I)
            XSUBCUT(I)=XSUBCUT(I)+XCUT(I)
          ENDFOR
  
          IF XWIPRPT .AND. XTOTCUT>0
            *** Display WIP Heading
            IF XFIRSTTIME
              ROW=ROW+1
              @ ROW,01 SAY XCUTHDR
              ROW=ROW+1
              @ ROW,01 SAY REPLICATE('-',22)
              XFIRSTTIME=.F.
            ENDIF
            ROW=ROW+1
            DO CASE
              CASE XWIPSORT='D'
                @ ROW,01 SAY XDATE
                @ ROW,10 SAY XCUTTKT
                @ ROW,17 SAY XCONTR1
              CASE XWIPSORT='F'
                @ ROW,01 SAY XCONTR1
                @ ROW,08 SAY XCUTTKT
                @ ROW,15 SAY XDATE
              OTHERWISE
                @ ROW,01 SAY XCUTTKT
                @ ROW,08 SAY XCONTR1
                @ ROW,15 SAY XDATE
            ENDCASE
  
            COL=32
            *** Display WIP Total Line Item
            FOR I=1 TO XCNT
              @ ROW,COL SAY XCUT(I)  PICTURE '9999999'
              COL=COL+9
            ENDFOR
            *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total.
            *C120872,1 ABD - add new variable to print the subtotal for the colors if more than 10. [Begin]
            @ ROW,122 SAY XTOTCUT  PICTURE '99999999'
            *@ ROW,122 SAY XTOTCUT+ln_totCut  PICTURE '99999999'
            *B123770,1 ASH 09/12/2004 (End)
            ln_totCut = ln_totCut + XTOTCUT
            *C120872,1 ABD - [End]
            
          ENDIF
  
          IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
            EXIT
          ENDIF
          STORE 0 TO XCUT, XTOTCUT
        ENDDO
  
        IF XWIPRPT
          COL=31
          XSUBTOT=0
          ROW=ROW+1
          @ ROW,31 SAY REPLICATE('-',99)
          ROW=ROW+1
          @ ROW,06 SAY 'Cuttkt Subtotals ......'
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSUBCUT(I)  PICTURE '99999999'
            XSUBTOT=XSUBTOT+XSUBCUT(I)
            COL=COL+9
          ENDFOR
          *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total.
          *B121016,1 ABD - calculate the subtotal for the total cutting. [Begin]
          *@ ROW,121 SAY XSUBTOT  PICTURE '999999999'
          *@ ROW,121 SAY XSUBTOT+ln_SubToCt PICTURE '999999999'
          @ ROW,121 SAY IIF(lnStyCnt>1,XSUBTOT+ln_SubToCt,XSUBTOT) PICTURE '999999999' 
          *B123770,1 ASH 09/12/2004 (End)
          ln_SubToCt = ln_SubToCt + XSUBTOT
          *B121016,1 ABD - [End]
          
          ROW=ROW+1
        ENDIF
      ENDIF
  
    ENDIF    && End of IF 'MF' $ gcComp_Mdl
    
    *-- If the " Style Purchase Order " or the "Point of Sale" Modules
    *-- is installed
    IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
       
      
      *** Get WIP From Purchase Order Temporary File
      SELE &POTEMP
      GO TOP
      SEEK XSTYLE
      IF FOUND()
        XFIRSTTIME=.T.
        DO WHILE INKEY()<>32
          IF ROW > 54 .AND. !EOF()
            ROW=0
            PAGENO=PAGENO+1

            DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH

            ROW=5
            *- Abdou
            IF gcDevice='PRINTER'
              @ PROW(),PCOL() SAY [(s3B]
            ENDIF
            *- Abdou
            *** Display Style, Description, And Range Of Colors

            *B122414,1 NNA 04/14/2004 (Begin) Always Show the style with his Discreption
            *@ ROW,01 SAY XSTYLE
            @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
            *B122414,1 NNA (End)

            COL=34
            FOR I=1 TO XCNT
              @ ROW,COL SAY XCLR(I)
              COL=COL+9
            ENDFOR
            *- Abdou
            IF gcDevice='PRINTER'
              @ PROW(),PCOL() SAY [(s0B]
            ENDIF
            *- Abdou
            @ ROW,125 SAY 'TOTAL'
            ROW=ROW+1
            @ ROW,01 SAY REPLICATE('-',129)

            *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
            ROW=ROW+1

            *C120872,1 ABD - Print the style type and price A & C. [Begin]
            *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
            @ ROW,13 SAY lcStyleTyp
            @ ROW,20 SAY lcPricA_C
            *C120872,1 ABD - [End]
            
            *C120872,1 ABD - [End]

            XFIRSTTIME=.T.
          ENDIF
  
          XPO=PO
          XVENDOR=VENDOR
          XDATE=POSHDR->COMPLETE
          *C120872,1 ABD - Save the po date. [Begin]
          ldEnTrDate = Poshdr.Entered
          *C120872,1 ABD - [End]
            
          laPOShpQty  = 0
          SCAN WHILE &XPOSCAN
          
            *C102809,1 04/15/2003 (Begin)
            *IF !INLIST(TranCd , '3' , '6')
            IF IIF(lcRpPODtSm = "D", TranCd <> '6', !INLIST(TranCd , '3' , '6'))
            *C102809,1 04/15/2003 (End) 
            
              DO CASE
                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
                  xPOS(1) = IIF(TranCd = '1' ,;
                                xPOS(1) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(1) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                  XPOS(1)=IIF(XPOS(1)>0, XPOS(1), 0)
                  
                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(1) = laPOShpQty(1) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(1)   = laTotShp(1) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)
                             
                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
                  xPOS(2) = IIF(TranCd = '1' ,;
                                xPOS(2) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                 xPOS(2) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(2)=IIF(XPOS(2)>0, XPOS(2), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(2) = laPOShpQty(2) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(2)   = laTotShp(2) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
                  xPOS(3) = IIF(TranCd = '1' ,;
                                xPOS(3) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(3) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(3)=IIF(XPOS(3)>0, XPOS(3), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(3) = laPOShpQty(3) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(3)   = laTotShp(3) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
                  xPOS(4) = IIF(TranCd = '1' ,;
                                xPOS(4) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(4) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(4)=IIF(XPOS(4)>0, XPOS(4), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(4) = laPOShpQty(4) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(4)   = laTotShp(4) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
                  xPOS(5) = IIF(TranCd = '1' ,;
                                xPOS(5) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(5) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(5)=IIF(XPOS(5)>0, XPOS(5), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(5) = laPOShpQty(5) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(5)   = laTotShp(5) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
                  xPOS(6) = IIF(TranCd = '1' ,;
                                xPOS(6) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(6) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(6)=IIF(XPOS(6)>0, XPOS(6), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(6) = laPOShpQty(6) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(6)   = laTotShp(6) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
                  xPOS(7) = IIF(TranCd = '1' ,;
                                xPOS(7) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(7) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(7)=IIF(XPOS(7)>0, XPOS(7), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(7) = laPOShpQty(7) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(7)   = laTotShp(7) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
                  xPOS(8) = IIF(TranCd = '1' ,;
                                xPOS(8) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                 xPOS(8) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(8)=IIF(XPOS(8)>0, XPOS(8), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(8) = laPOShpQty(8) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(8)   = laTotShp(8) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
                  xPOS(9) = IIF(TranCd = '1' ,;
                                xPOS(9) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(9) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(9)=IIF(XPOS(9)>0, XPOS(9), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(9) = laPOShpQty(9) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(9)   = laTotShp(9) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)

                CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
                  xPOS(10) = IIF(TranCd = '1' ,;
                                xPOS(10) +;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                xPOS(10) -;
                                (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                  XPOS(10)=IIF(XPOS(10)>0, XPOS(10), 0)

                  *C102809,1 04/15/2003 (Begin)
                  laPOShpQty(10) = laPOShpQty(10) + IIF(TranCd = '3', TotQty,0)
                  laTotShp(10)   = laTotShp(10) + IIF(TranCd = '3', TotQty,0)
                  *C102809,1 04/15/2003 (End)
              ENDCASE
            ENDIF
          ENDSCAN
  
          FOR I=1 TO XCNT
            XTOTPOS=XTOTPOS+XPOS(I)
            XSUBPOS(I)=XSUBPOS(I)+XPOS(I)
            *C102809,1 04/15/2003 (Begin)
            laOpnPoQty(I) = laOpnPoQty(I) + XPOS(I)
            *C102809,1 04/15/2003 (End)
          ENDFOR
  
          *C102809,1 04/15/2003 (Begin) 
          IF XWIPRPT AND lcRpPODtSm = "D" AND USED(lcTmpShpFl) AND;
                                  (laPOShpQty(1)+laPOShpQty(2)+laPOShpQty(3)+laPOShpQty(4)+;
                                  laPOShpQty(5)+laPOShpQty(6)+laPOShpQty(7)+laPOShpQty(8)+;
                                  laPOShpQty(9)+laPOShpQty(10)<>0)
            SELECT (lcTmpShpFl)
            APPEND BLANK
            REPLACE PO        WITH XPO     ,;
                    COMPLETE  WITH XDATE   ,;
                    Vendor    WITH xVendor 
      
            FOR I=1 TO XCNT
              lcCnt = ALLTRIM(STR(I,2))
              REPLACE Qty&lcCnt WITH laPOShpQty(I)
            ENDFOR
          
            SELE &POTEMP
          ENDIF
          *C102809,1 04/15/2003 (End)
          
          IF XWIPRPT .AND. XTOTPOS>0
                       
            *** Display WIP Heading
            IF XFIRSTTIME
              ROW=ROW+1
              @ ROW,01 SAY XPOHDR
              ROW=ROW+1
              
              *C120872,1 ABD - Increse the -- line. [Begin]
              *@ ROW,01 SAY REPLICATE('-',22)
              @ ROW,01 SAY REPLICATE('-',33)
              *C120872,1 ABD - [End]
              
              XFIRSTTIME=.F.
            ENDIF
            ROW=ROW+1
            *C120872,1 ABD - Print the entered date into the layout . [Begin]
            *DO CASE
            *  CASE XWIPSORT='D'
            *    @ ROW,01 SAY XDATE
            *    @ ROW,10 SAY XPO
            *    @ ROW,17 SAY XVENDOR
            *  CASE XWIPSORT='F'
            *    @ ROW,01 SAY XVENDOR
            *    @ ROW,08 SAY XPO
            *    @ ROW,15 SAY XDATE
            *  OTHERWISE
            *    @ ROW,01 SAY XPO
            *    @ ROW,08 SAY XVENDOR
            *    @ ROW,15 SAY XDATE
            *ENDCASE
            DO CASE
              CASE XWIPSORT='D'
                @ ROW,01 SAY ldEnTrDate
                @ ROW,10 SAY XDATE
                @ ROW,19 SAY XPO
                @ ROW,26 SAY XVENDOR
              CASE XWIPSORT='F'
                @ ROW,01 SAY XVENDOR
                @ ROW,08 SAY XPO
                @ ROW,16 SAY XDATE
                @ ROW,25 SAY ldEnTrDate
              OTHERWISE
                @ ROW,01 SAY XPO
                @ ROW,08 SAY XVENDOR
                @ ROW,15 SAY XDATE
                @ ROW,25 SAY ldEnTrDate
            ENDCASE
            *C120872,1 ABD - [End]
  
            
            *C120872,1 ABD - reprint the qty again after print the entered date.  [Begin]
            *COL=32
            *  * Display WIP Total Line Item
            *  FOR I=1 TO XCNT
            *    @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
            *    COL=COL+9
            *  ENDFOR
            *  @ ROW,122 SAY XTOTPOS  PICTURE '99999999'
            IF XWIPSORT='D'
              COL=32
              @ ROW,COL SAY XPOS(1)  PICTURE '99999'
              COL=COL+9
              *** Display WIP Total Line Item
              FOR I=2 TO XCNT
                @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
                COL=COL+9
              ENDFOR
              *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total.
              *C120872,1 ABD - accumulate the total PO's. [Begin]
              *555
              @ ROW,122 SAY XTOTPOS  PICTURE '99999999'
              *@ ROW,122 SAY XTOTPOS+ln_totPos  PICTURE '99999999' 
              *B123770,1 ASH 09/12/2004 (End)
              ln_totPos = ln_totPos + XTOTPOS
              *C120872,1 ABD - [End]
            ELSE
              COL=32
                @ ROW,32 SAY XPOS(1)  PICTURE '999999'
                COL=COL+9
              * Display WIP Total Line Item
              FOR I=2 TO XCNT
                @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
                COL=COL+9
              ENDFOR
              *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total. 
              *C120872,1 ABD - Accumlate the total pos. [Begin]
                          
              *B127507,2 EIH 07/31/2005 Print the right total PO QTY [Begin].
              *@ ROW,122 SAY XTOTPOS  PICTURE '99999999'
              SELECT (lcStyPoPg)
              IF SEEK(PADR(XSTYLE,19,' ')+XPO)
                REPLACE &lcStyPoPg..nTPosPg WITH &lcStyPoPg..nTPosPg+XTOTPOS
              ELSE
                APPEND BLANK 
                REPLACE  &lcStyPoPg..Style    WITH PADR(XSTYLE,19,' ')    ,;
                         &lcStyPoPg..PO       WITH XPO    ,;
                         &lcStyPoPg..nTPosPg  WITH XTOTPOS
              ENDIF      
              @ ROW,122 SAY &lcStyPoPg..nTPosPg  PICTURE '99999999'
              SELE &POTEMP                       
              *B127507,2 EIH 07/31/2005  [End].

        
              
              *@ ROW,122 SAY XTOTPOS+ln_totPos  PICTURE '99999999'
              *B123770,1 ASH 09/12/2004 (End)
              ln_totPos = ln_totPos + XTOTPOS
              *C120872,1 ABD - [End]
              
            ENDIF  
            *C120872,1 ABD - [End]
            
          ENDIF
  
          IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
            EXIT
          ENDIF
          STORE 0 TO XPOS, XTOTPOS
        ENDDO
  
        IF XWIPRPT
          COL=31
          XSUBTOT=0
          ROW=ROW+1
          @ ROW,31 SAY REPLICATE('-',99)
          ROW=ROW+1
          @ ROW,06 SAY 'P.O. Subtotals ........'
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSUBPOS(I)  PICTURE '99999999'
            XSUBTOT=XSUBTOT+XSUBPOS(I)
            COL=COL+9
          ENDFOR
          *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total.
          *B121016,1 ABD - calculate the subtotal for the total Pos's.  [Begin]
          *@ ROW,121 SAY XSUBTOT  PICTURE '999999999'
          *@ ROW,121 SAY XSUBTOT+ln_SubToPo  PICTURE '999999999'
          @ ROW,121 SAY IIF(lnStyCnt>1,XSUBTOT+ln_SubToPo,XSUBTOT)  PICTURE '999999999'
          *B123770,1 ASH 09/12/2004 (End)
          ln_SubToPo = ln_SubToPo + XSUBTOT
          *B121016,1 ABD - [End]
          
          ROW=ROW+1
          *C102809,1 04/15/2003 (Begin) To print in-transit if Print WIP = "Y"
          IF lcRpPODtSm = "D" AND USED(lcTmpShpFl)
            =lfSumShpQt()
          ENDIF  
          IF lcRpPODtSm = "D" AND lcRPWIPSta = "Y" AND USED(lcTmpShpFl)
            =lfPrntShpQ()
          ENDIF
          *C102809,1 04/15/2003 (End)
          
        ENDIF
      ENDIF
    ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  
    *** Get Sales From Orders Temporary File
    SELE &ORDTEMP
    GO TOP
    SEEK XSTYLE
    IF FOUND()
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1
        
          DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH
          
          ROW=5
          *- Abdou
          IF gcDevice='PRINTER'
            @ PROW(),PCOL() SAY [(s3B]
          ENDIF
          *- Abdou
          
          *** Display Style, Description, And Range Of Colors

          *B122414,1 NNA 04/14/2004 (Begin) Always Show the style with his Discreption
          *@ ROW,01 SAY XSTYLE
          @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
          *B122414,1 NNA (End)

          COL=34
          FOR I=1 TO XCNT
            @ ROW,COL SAY XCLR(I)
            COL=COL+9
          ENDFOR
          *- Abdou
          IF gcDevice='PRINTER'
            @ PROW(),PCOL() SAY [(s0B]
          ENDIF
          *- Abdou
          
          @ ROW,125 SAY 'TOTAL'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',129)

          *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
          ROW=ROW+1
          
          *C120872,1 ABD - Print the style type and price A & C. [Begin]
          *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
          @ ROW,13 SAY lcStyleTyp
          @ ROW,20 SAY lcPricA_C
          *C120872,1 ABD - [End]
          
          *C120872,1 ABD - [End]
          
          XFIRSTTIME=.T.
        ENDIF

        XORDER=ORDHDR->ORDER
        XACCOUNT=ACCOUNT
        XDATE=COMPLETE
        *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
        ldStart  = Ordhdr.Start
        *C102842,3 ASH 11/06/2003 (End)
        SCAN WHILE &XORDSCAN
          DO CASE
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
              XORD(1)=XORD(1)+TOTQTY
            
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
              XORD(2)=XORD(2)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
              XORD(3)=XORD(3)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
              XORD(4)=XORD(4)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
              XORD(5)=XORD(5)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
              XORD(6)=XORD(6)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
              XORD(7)=XORD(7)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
              XORD(8)=XORD(8)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
              XORD(9)=XORD(9)+TOTQTY

            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
              XORD(10)=XORD(10)+TOTQTY

          ENDCASE
        ENDSCAN

        FOR I=1 TO XCNT
          XTOTORD=XTOTORD+XORD(I)
          XSUBORD(I)=XSUBORD(I)+XORD(I)
        ENDFOR
        IF XORDRPT .AND. XTOTORD>0
          *** w
          IF XFIRSTTIME
            ROW=ROW+1
            @ ROW,01 SAY XORDHDR
            ROW=ROW+1
            *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
            *@ ROW,01 SAY REPLICATE('-',22)
            @ ROW,01 SAY REPLICATE('-',30)
            *C102842,3 ASH 11/06/2003 (End)
            XFIRSTTIME=.F.
          ENDIF
          ROW=ROW+1
          DO CASE
            CASE XORDSORT='D'
              *C125377,1 NNA 03/03/2005 (Begin) print Start date before the Complete date
              *@ ROW,01 SAY XDATE
              @ ROW,01 SAY ldStart
              *C125377,1 NNA (End)
              @ ROW,10 SAY XORDER
              @ ROW,17 SAY XACCOUNT
              *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
              *C125377,1 NNA 03/03/2005 (Begin) print Start date before the Complete date
              *@ ROW,23 SAY ldStart
              @ ROW,23 SAY XDATE
              *C125377,1 NNA (End)
              *C102842,3 ASH 11/06/2003 (End)
            CASE XORDSORT='A'
              @ ROW,01 SAY XACCOUNT
              @ ROW,07 SAY XORDER
              *C125377,1 NNA 03/03/2005 (Begin) print Start date before the Complete date
              *@ ROW,14 SAY XDATE
              *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
              *@ ROW,23 SAY ldStart
              *C102842,3 ASH 11/06/2003 (End)
              @ ROW,14 SAY ldStart
              @ ROW,23 SAY XDATE
              *C125377,1 NNA (End)
            OTHERWISE
              @ ROW,01 SAY XORDER
              @ ROW,08 SAY XACCOUNT
              *C125377,1 NNA 03/03/2005 (Begin) print Start date before the Complete date
              *@ ROW,14 SAY XDATE
              *C102842,3 ASH 11/06/2003 (Begin) Print the Order start date
              *@ ROW,23 SAY ldStart
              *C102842,3 ASH 11/06/2003 (End)
              @ ROW,14 SAY ldStart
              @ ROW,23 SAY XDATE
              *C125377,1 NNA (End)
          ENDCASE
          COL=32
          *** Display ORD Total Line Item
          FOR I=1 TO XCNT
            @ ROW,COL SAY XORD(I) PICTURE '9999999'
            COL=COL+9
          ENDFOR
          *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total.
          *C120872,1 ABD - Accumlate the total order's  [Begin]
          
          
          *B127507,2 EIH 07/31/2005 Print the right total order QTY [Begin].
          *@ ROW,122 SAY XTOTORD  PICTURE '99999999'
          SELECT (lcStyOrdPg)
          IF SEEK(PADR(XSTYLE,19,' ')+XORDER) 
            REPLACE &lcStyOrdPg..nTColsPg WITH &lcStyOrdPg..nTColsPg+XTOTORD  
          ELSE
            APPEND BLANK
            REPLACE  &lcStyOrdPg..Style    WITH PADR(XSTYLE,19,' ')    ,;
                     &lcStyOrdPg..Order    WITH XORDER    ,;
                     &lcStyOrdPg..nTColsPg WITH XTOTORD

          ENDIF      
          @ ROW,122 SAY &lcStyOrdPg..nTColsPg  PICTURE '99999999'
          SELE &ORDTEMP         
          *B127507,2 EIH 07/31/2005  [End].
          
          
          *@ ROW,122 SAY XTOTORD + ln_totcOrd  PICTURE '99999999'
          *B123770,1 ASH 09/12/2004 (End)
          ln_totcOrd = ln_totcOrd + XTOTORD
          *C120872,1 ABD - [End]
          
        ENDIF

        IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
          EXIT
        ENDIF
        STORE 0 TO XORD, XTOTORD
      ENDDO

      IF XORDRPT
        COL=31
        XSUBTOT=0
        ROW=ROW+1
        @ ROW,31 SAY REPLICATE('-',99)
        ROW=ROW+1
        @ ROW,06 SAY 'Orders Subtotals ......'
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSUBORD(I)  PICTURE '99999999'
          XSUBTOT=XSUBTOT+XSUBORD(I)
          COL=COL+9
        ENDFOR
        *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total.
        *B121016,1 ABD - calculate the subtotal for the total Order's. [Begin]
        *@ ROW,121 SAY XSUBTOT  PICTURE '999999999'
        *@ ROW,121 SAY XSUBTOT+ln_SubtOrd  PICTURE '999999999'
        @ ROW,121 SAY IIF(lnStyCnt>1,XSUBTOT+ln_SubtOrd,XSUBTOT)  PICTURE '999999999'
        *B123770,1 ASH 09/12/2004 (End)
        ln_SubtOrd = ln_SubtOrd + XSUBTOT
        *B121016,1 ABD - [End]
        
        ROW=ROW+1
      ENDIF
    ENDIF

    *C102809,1 04/15/2003 (Begin) sum the booked and shipped qty.
    =lfGetBookQ()
    =lfGetShipQ()
    *C102809,1 04/15/2003 (End)
    
    *C123019,1 BWA 08/05/2004 Sum the returned qty.[START]
    =lfGetRtrnQ()
    *C123019,1 BWA 08/05/2004.[END]
    

    *B121016,1 ABD - Define new variables to hold the total for every line print into the report.  [Begin]
    IF lnStyCnt = 1 
      STORE 0 TO ln_TotStk, ln_TotWip, ln_TotOrd,ln_TotOts,ln_TotOpQt,ln_TotBokq, ln_totShpQ
    ELSE
      ln_TotStk  = ln_TotStk  + XTOTSTK
      ln_TotWip  = ln_TotWip  + XTOTWIP
      ln_TotOts  = ln_TotOts  + XTOTOTS
      ln_TotOpQt = ln_TotOpQt + lnTotOpnQt
      ln_TotBokq = ln_TotBokq + lnTotBookQ
      ln_totShpQ = ln_totShpQ + lnTotShipQ
    ENDIF
    *B121016,1 ABD - [End]

    
    *** Display Summary Information
    STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS
    *khalid
    STORE 0 TO lnTotOpnQt, lnTotBookQ, lnTotShipQ , lnTotRtrn

    *C102809,1 04/15/2003 (Begin)
    *IF ROW > 49
    IF IIF(lcRpPODtSm = "D", ROW > 45, ROW > 49)
    *C102809,1 04/15/2003 (End)
      ROW=0
      PAGENO=PAGENO+1

      
      DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH

      *- Abdou
      IF gcDevice='PRINTER'
        @ PROW(),PCOL() SAY [(s3B]
      ENDIF
      *- Abdou
      
      ROW=5

      *B122414,1 NNA 04/14/2004 (Begin) Always Show the style with his Discreption
      *@ ROW,01 SAY XSTYLE
      @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
      *B122414,1 NNA (End)

      COL=34
      FOR I=1 TO XCNT
        @ ROW,COL SAY XCLR(I)
        COL=COL+9
      ENDFOR
      *- Abdou
      IF gcDevice='PRINTER'
        @ PROW(),PCOL() SAY [(s0B]
      ENDIF
      *- Abdou
      @ ROW,125 SAY 'TOTAL'
      ROW=ROW+1
      @ ROW,01 SAY REPLICATE('-',129)

      *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
      ROW=ROW+1
      
      *C120872,1 ABD - Print the style type and price A & C. [Begin]
      *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
      @ ROW,13 SAY lcStyleTyp
      @ ROW,20 SAY lcPricA_C
      *C120872,1 ABD - [End]
      
      *C120872,1 ABD - [End]
      
    ENDIF

    COL=32
    ROW=ROW+1
    *C125377,1 NNA 03/03/2005 (Begin) Print Stock Per WareHouse
    *@ ROW,13 SAY 'STOCK  .........'
    *FOR I=1 TO XCNT
    *  @ ROW,COL SAY XSTK(I)  PICTURE '9999999'
    *  XTOTSTK=XTOTSTK+XSTK(I)
    *  XOTS(I)=XOTS(I)+XSTK(I)
    *  COL=COL+9
    *ENDFOR
    IF SEEK(XSTYLE,lcStoreTmp)
      SELECT(lCStoreTmp)
      
      *B127507,1 EIH 05/15/2005 Restart the stock counter [Begin].
      STORE 0 TO ln_stkCnt
      *B127507,1 EIH 05/15/2005 [End].
      
      
      SCAN REST WHILE Style+cWarecode = XSTYLE
        *B127507,1 EIH 05/15/2005 Restart ln_TotStk variable and increment stock counter [Begin].
        *STORE 0 TO XTOTSTK
        STORE 0 TO XTOTSTK , ln_TotStk  
        ln_stkCnt = ln_stkCnt + 1
        *B127507,1 EIH 05/15/2005 Restart ln_TotStk variable [End].
        
        
        @ ROW,13 SAY 'STOCK ' + PADR(ALLTRIM(cWarecode),10,'.') 
        FOR I=1 TO XCNT
          LCI=ALLTRIM(STR(I))
          @ ROW,COL SAY &lcStoreTmp..STKCLR&LCI  PICTURE '9999999'
          XTOTSTK=XTOTSTK+&lcStoreTmp..STKCLR&LCI
          XOTS(I)=XOTS(I)+&lcStoreTmp..STKCLR&LCI
          COL=COL+9
        ENDFOR
    *C125377,1 NNA (End)

    *B121016,1 ABD - calculate the current line for the stock. [Begin]
    *@ ROW,122 SAY XTOTSTK    PICTURE '99999999'
    
    *B127507,1 EIH 05/15/2005 Update totals + previous totals  [Begin].
    *@ ROW,122 SAY XTOTSTK+ln_TotStk    PICTURE '99999999'    
    
     	
   	*B127507,2 EIH 07/31/2005 Correct the stock totals.[Begin]
    *xtotstkpg[ln_stkCnt] = xtotstkpg[ln_stkCnt] + XTOTSTK + ln_TotStk
  	*@ ROW,122 SAY  xtotstkpg[ln_stkCnt]   PICTURE '99999999'  
    lnWarePos = ASCAN(xtotstkpg , ALLTRIM(cWarecode)) 
    IF lnWarePos <> 0
      lnWarePos = lnWarePos / 2 
      xtotstkpg[lnWarePos,1] = xtotstkpg[lnWarePos , 1] + XTOTSTK + ln_TotStk     
      @ ROW,122 SAY  xtotstkpg[lnWarePos,1]   PICTURE '99999999'    
    ELSE
      lnNewPos = lnNewPos + 1
      xtotstkpg[lnNewPos,1] = XTOTSTK + ln_TotStk     
      xtotstkpg[lnNewPos,2] = ALLTRIM(cWarecode)
      @ ROW,122 SAY  xtotstkpg[lnNewPos,1]   PICTURE '99999999'    
    ENDIF
    *B127507,2 EIH 07/31/2005 [End] 
   	
    *B127507,1 EIH 05/15/2005 [End].
    
    *B121016,1 ABD - [End]
    
    XGRDSTK=XGRDSTK+XTOTSTK
    
    *C125377,1 NNA 03/03/2005 (Begin) End of Scan and if Statement
        ROW=ROW+1
        COL=32
      ENDSCAN
    ENDIF
    *ROW=ROW+1
    *C125377,1 NNA (End)

    COL=32
    @ ROW,13 SAY 'W.I.P  .........'
    FOR I=1 TO XCNT
      *C102809,1 04/15/2003 (Begin)

      @ ROW,COL SAY XSUBCUT(I)+IIF(lcRpPODtSm = "D",laPOShpQty(I),XSUBPOS(I))+XSUBSTY(I)  PICTURE '9999999'
      XTOTWIP=XTOTWIP+XSUBCUT(I)+IIF(lcRpPODtSm = "D",laPOShpQty(I),XSUBPOS(I))+XSUBSTY(I)
      XOTS(I)=XOTS(I)+XSUBCUT(I)+IIF(lcRpPODtSm = "D",laPOShpQty(I),XSUBPOS(I))+XSUBSTY(I)
      *C102809,1 04/15/2003 (End)
      COL=COL+9
    ENDFOR 
    *B121016,1 ABD - calculate the current line for the WIP. [Begin]
    *@ ROW,122 SAY XTOTWIP       PICTURE '99999999'
    @ ROW,122 SAY XTOTWIP+ln_TotWip PICTURE '99999999'
    *B121016,1 ABD - [End]
    
    **khalid
    *IF lcRpPODtSm = 'D'
    *  XTOTWIP = 0
    *  FOR I=1 TO XCNT
    *    XTOTWIP = XTOTWIP+laTotShp(I)
    *  ENDFOR
      
    *ENDIF 
    **khalid
    XGRDWIP=XGRDWIP+XTOTWIP

    ROW=ROW+1
    COL=32
    *C120872,1 ABD - change the caption. [Begin]
    *@ ROW,13 SAY 'ORDERS .........'
    @ ROW,13 SAY  'CUST ORDERS ....'
    *C120872,1 ABD - [End]
    

    FOR I=1 TO XCNT
      @ ROW,COL SAY XSUBORD(I)  PICTURE '9999999'
      XTOTORD=XTOTORD+XSUBORD(I)
      XOTS(I)=XOTS(I)-XSUBORD(I)
      COL=COL+9
    ENDFOR
    *B123770,1 ASH 09/12/2004 (Begin) Fix problem of accumlative total.
    *B121016,1 ABD - calculate the current line for the customer order's . [Begin]
    *@ ROW,122 SAY XTOTORD       PICTURE '99999999'
    *@ ROW,122 SAY XTOTORD+ln_TotOrd PICTURE '99999999'
    @ ROW,122 SAY IIF(lnStyCnt>1,XTOTORD+ln_TotOrd,XTOTORD) PICTURE '99999999'
    *B123770,1 ASH 09/12/2004 (End)
    ln_TotOrd = ln_TotOrd + XTOTORD
    *B121016,1 ABD - [End]
    
    XGRDORD=XGRDORD+XTOTORD

    ROW=ROW+1
    @ ROW,13 SAY REPLICATE('-',117)
    ROW=ROW+1
    COL=31
    @ ROW,13 SAY 'O.T.S. .........'
    FOR I=1 TO XCNT
      
      *C120872,1 ABD - add the vendor's po to the OTS Qty. [Begin]
      *@ ROW,COL SAY XOTS(I)     PICTURE '99999999'
      *XTOTOTS=XTOTOTS+XOTS(I)
      @ ROW,COL SAY XOTS(I) + laOpnPoQty(I)     PICTURE '99999999'      
       *C037661,1 NNA 03/02/2004 (Begin) Add Only The Positive Numbers to the total O.T.S
       *XTOTOTS=XTOTOTS+XOTS(I)+laOpnPoQty(I)
	   *-- If the OTS Qty<0 and open PO Qty=0 then XTOTOTS=0
	   *-- If the OTS Qty<0 and open PO Qty>0 then XTOTOTS=	[laOpnPoQty(I)+XOTS(I)] if laOpnPoQty(I)+XOTS(I)>0
	   *-- If the OTS Qty>0 and open PO Qty=0 then XTOTOTS=OTS Qty
	   *-- If the OTS Qty>0 and open PO Qty>0 then XTOTOTS=[laOpnPoQty(I)+XOTS(I)]
	   XTOTOTS=XTOTOTS+IIF((XOTS(I)+laOpnPoQty(I))>0,XOTS(I)+laOpnPoQty(I),0)
       *XTOTOTS=XTOTOTS+IIF(XOTS(I)<0,IIF(laOpnPoQty(I)=0,0,;
       								 IIF((laOpnPoQty(I)+XOTS(I))>0,laOpnPoQty(I)+XOTS(I),0)),;
       								 IIF(laOpnPoQty(I)=0,XOTS(I),laOpnPoQty(I)+XOTS(I)))
       *C037661,1 NNA (End)
      *C120872,1 ABD - [End]
      
      COL=COL+9
    ENDFOR
    *C037661,1 NNA 03/02/2004 (Begin) Calculate the grand total Of the O.T.S quantities
    XGrdOTS=XGrdOTS+XTOTOTS    		&& this grand total Appear only if (Print By size=No)
    *C037661,1 NNA (End)
    *B121016,1 ABD - calculate the current line for the ots. [Begin]
    *@ ROW,121 SAY XTOTOTS       PICTURE '999999999'
    @ ROW,121 SAY XTOTOTS+ln_TotOts PICTURE '999999999'
    *B121016,1 ABD - [End]
    *C102809,1 04/15/2003 (Begin)
    IF lcRpPODtSm = "D"
      ROW=ROW+1
      COL=31
      
      *C120872,1 ABD - change the caption. [Begin]
      *@ ROW,13 SAY "Open PO's ......"
      @ ROW,13 SAY  "VENDOR PO's ...."
      *C120872,1 ABD - [End]
      
      FOR I=1 TO XCNT
        @ ROW,COL SAY laOpnPoQty(I)     PICTURE '99999999'
        lnTotOpnQt = lnTotOpnQt + laOpnPoQty(I)
        COL=COL+9
      ENDFOR
      *B121016,1 ABD - calculate the current line for the Open Po's. [Begin]
      *@ ROW,121 SAY lnTotOpnQt     PICTURE '999999999'
      @ ROW,121 SAY lnTotOpnQt+ln_TotOpQt PICTURE '999999999'
      *B121016,1 ABD - [End]
      
      *C120872,1 ABD - calculate the total po's. [Begin]
      XGRDPos = XGRDPos + lnTotOpnQt
      *C120872,1 ABD - [End]

      ROW=ROW+1
      @ ROW,01 SAY REPLICATE('-',129)
      ROW=ROW+1
      COL=31
      @ ROW,13 SAY "Booked ........."
      FOR I=1 TO XCNT
        @ ROW,COL SAY laBookQty(I)     PICTURE '99999999'
        lnTotBookQ = lnTotBookQ + laBookQty(I)
        COL=COL+9
      ENDFOR
      *B121016,1 ABD - calculate the current line for the Booked qty. [Begin]
      *@ ROW,121 SAY lnTotBookQ     PICTURE '999999999'
      @ ROW,121 SAY lnTotBookQ+ln_TotBokq PICTURE '999999999'
      *B121016,1 ABD - [End]
      
      ROW=ROW+1
      COL=31
      @ ROW,13 SAY "Shipped ......."
      FOR I=1 TO XCNT
        @ ROW,COL SAY laShipQty(I)     PICTURE '99999999'
        lnTotShipQ = lnTotShipQ + laShipQty(I)
        COL=COL+9
      ENDFOR
      *@ ROW,121 SAY laShipQty     PICTURE '999999999'  

      *B121016,1 ABD - calculate the current line for the Shipped Qty. [Begin]
      *@ ROW,121 SAY lnTotShipQ     PICTURE '999999999'  
      @ ROW,121 SAY lnTotShipQ+ln_totShpQ PICTURE '999999999'  
      *B121016,1 ABD - [End]

      *C123019,1 BWA 08/05/2004 Add the new linefor the returned values.[START]
      ROW = ROW + 1
      COL = 31
      @ ROW,13 SAY "Returned ......."
      FOR I=1 TO XCNT
        @ ROW,COL SAY IIF(laRtrnQty(I) > 0 , (laRtrnQty(I))*-1 , laRtrnQty(I))    PICTURE '99999999'
        lnTotRtrn = lnTotRtrn + laRtrnQty(I)
        COL = COL + 9
      ENDFOR
    
      *B127507,1 EIH 05/15/2005 Fix bug that Correct totals of Returned if ther is style printed in 2 pages [Begin] . 
      *@ ROW,121 SAY IIF(lnTotRtrn > 0 , (lnTotRtrn)*-1 , lnTotRtrn)    PICTURE '999999999'
      lnTotRetSt = lnTotRetSt + lnTotRtrn
      @ ROW,121 SAY IIF(lnTotRetSt > 0 , (lnTotRetSt)*-1 , lnTotRetSt)    PICTURE '999999999'      
      *B127507,1 EIH 05/15/2005 [End]. 
            
      
      *C123019,1 BWA 08/05/2004.[END]

    *C125377,1 NNA 03/03/2005 (Begin) get the total Shipped and Returned if show POs By Summary
      lnGTotShp  = lnGTotShp  + lnTotShipQ
      lnGTotRtrn = lnGTotRtrn + lnTotRtrn
      

      
    ELSE      
      FOR I=1 TO 10
        lnGTotShp  = lnGTotShp  + laShipQty(I)
        lnGTotRtrn = lnGTotRtrn + laRtrnQty(I)
      ENDFOR
    *C125377,1 NNA (End)

    ENDIF
    XCLRCNT = XCLRCNT - XCNT
    SELE STYLE
    IF lnStyCnt < lnLoop 
      ROW=ROW+3
    ENDIF  
    *C102809,1 04/15/2003 (End)
  ENDFOR
    
    SELE STYLE
    SCAN WHILE SUBSTR(STYLE,1,lnMajLen)=XSTYLE
    ENDSCAN

    IF EOF()
      ROW=ROW+2
      IF ROW > 52
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH
        
      ENDIF
      
      *C120872,1 ABD - Print the total po's. [Begin]
      *@ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
      *ROW=ROW+1
      *@ ROW,29 SAY 'GRAND TOTALS:  STOCK:'
      *@ ROW,51 SAY XGRDSTK  PICTURE '99999999'
      *@ ROW,62 SAY 'W.I.P:'
      *@ ROW,69 SAY XGRDWIP  PICTURE '99999999'
      *@ ROW,80 SAY 'ORDERS:'
      *@ ROW,88 SAY XGRDORD  PICTURE '99999999'
      *@ ROW,99 SAY 'O.T.S:'
      *@ ROW,106 SAY XGRDSTK+XGRDWIP-XGRDORD  PICTURE '99999999'
      *ROW=ROW+1
      *@ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
      
      *C125377,1 NNA 03/03/2005 (Begin) Add the shipped and Returned Qty. to the Grand Total
      *@ ROW,07 SAY '===*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===*===*===*===*===*===*===*=='
      *ROW=ROW+1
      *@ ROW,07 SAY 'GRAND TOTALS:  STOCK:'
      *@ ROW,29 SAY XGRDSTK  PICTURE '99999999'
      *@ ROW,40 SAY 'W.I.P:'
      *@ ROW,47 SAY XGRDWIP  PICTURE '99999999'
      *@ ROW,58 SAY "VENDOR PO's:"
      *@ ROW,71 SAY XGRDPos  PICTURE '99999999'
      *@ ROW,80 SAY 'CUST ORDERS:'
      *@ ROW,93 SAY XGRDORD  PICTURE '99999999'
      *@ ROW,103 SAY 'O.T.S:'
	  *C037661,1 NNA 03/02/2004 (Begin) Print the calculated grand total of O.T.S
      *@ ROW,110 SAY XGRDSTK+XGRDWIP+XGRDPos-XGRDORD  PICTURE '99999999'
      *@ ROW,110 SAY XGrdOTS  PICTURE '99999999'      
	  *C037661,1 NNA (End)
      *ROW=ROW+1
      *@ ROW,07 SAY '===*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===*===*===*===*===*===*===*=='
      @ ROW,01 SAY '===*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===*===*===*===*===*===*===*===*===*===*===*===*'
      ROW=ROW+1
      @ ROW,01 SAY 'GRAND TOTALS :'
      ROW=ROW+1
      @ ROW,001 SAY 'STOCK:'
      @ ROW,007 SAY XGRDSTK  PICTURE '99999999'
      @ ROW,017 SAY 'W.I.P:'
      @ ROW,023 SAY XGRDWIP  PICTURE '99999999'
      @ ROW,033 SAY "VENDOR PO's:"
      @ ROW,045 SAY XGRDPos  PICTURE '99999999'
      @ ROW,055 SAY 'CUST ORDERS:'
      @ ROW,067 SAY XGRDORD  PICTURE '99999999'
      @ ROW,077 SAY 'O.T.S:'
      @ ROW,083 SAY XGrdOTS  PICTURE '99999999'      
      @ ROW,093 SAY 'SHIPPED:'
      @ ROW,101 SAY lnGTotShp  PICTURE '99999999'            
      @ ROW,111 SAY 'RETURNED:'
      @ ROW,120 SAY IIF(lnGTotRtrn > 0 , lnGTotRtrn*-1 , lnGTotRtrn)  PICTURE '99999999'            
      ROW=ROW+1
      @ ROW,01 SAY '===*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===*===*===*===*===*===*===*===*===*===*===*===*'
      *C125377,1 NNA (End)

      *C120872,1 ABD - [End]
      
      EXIT
    ENDIF
    ROW=ROW+3
ENDDO
RETURN

****************************************************************************
* PROG : STY962.PRG
* DATE : 02/20/92
* DESC : REPORT WITH SIZE BREAKDOWN
* MODI : 01/11/94 YMA
****************************************************************************
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
*** Initialize Variables
STORE SPACE(30) TO XNAME
STORE SPACE(6)  TO XCUTTKT, XPO, XORDER
STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
STORE SPACE(6)  TO XCOLOR         &&TAK06/21/94
STORE SPACE(1)  TO XSTAT, XPIKTKT
STORE CTOD('  /  /    ') TO XDATE, XSTART
STORE 0 TO XCNT, XPRICE, XGRDORD, XGRDPRI
DIMENSION XSTK(8),XOTS(8),XSIZE(8),XCUT(8),XSUBCUT(8),XPOS(8),XSUBPOS(8),XORD(8),XSUBORD(8),XSTY(8),XSUBSTY(8)

*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE XWIPSORT='D'                          && Sort by Completion Date
    XCUTSCAN='(STYLE+DTOS(CUTTKTH->COMPLETE)+CUTTKT)=(lcStyleCod+DTOS(XDATE)+XCUTTKT)'
    XCUTHDR='COMPLETE CUTTKT CONTR NAME'
    XPOSCAN='(STYLE+DTOS(POSHDR->COMPLETE)+PO)=(lcStyleCod+DTOS(XDATE)+XPO)'
    XPOHDR='COMPLETE PO #  VENDOR NAME'
  CASE XWIPSORT='F'                          && Sort by Contractor
    XCUTSCAN='(STYLE+MFGOPRHD.cContCode+CUTTKT)=(lcStyleCod+XCONTR1+XCUTTKT)'
    XCUTHDR='CONTR NAME'+SPACE(21)+'CUTTKT COMPLETE'
    XPOSCAN='(STYLE+VENDOR+PO)=(lcStyleCod+XVENDOR+XPO)'
    XPOHDR='VENDOR NAME'+SPACE(20)+'PO #   COMPLETE'
  OTHERWISE                                  && Sort by Cutting Ticket/PO
    XCUTSCAN='(STYLE+CUTTKT)=(lcStyleCod+XCUTTKT)'
    XCUTHDR='CUTTKT CONTR NAME'+SPACE(21)+'COMPLETE'
    XPOSCAN='(STYLE+PO)=(lcStyleCod+XPO)'
    XPOHDR='PO #  VENDOR NAME'+SPACE(21)+'COMPLETE'
ENDCASE

*** Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  CASE XORDSORT='D'                         && Sort by Completion Date
    XORDHDR='START    COMPLETE ORDER  ACCT# NAME'
  CASE XORDSORT='A'                         && Sort by Account
    XORDHDR='ACCT# NAME'+SPACE(12)+'ORDER  START    COMPLETE'
  OTHERWISE                                  && Sort by Orders
    XORDHDR='ORDER  ACCT# NAME'+SPACE(12)+'START    COMPLETE'
ENDCASE

SELE STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
GO TOP

DO WHILE INKEY() <> 32
  
  XSTYLE = SUBSTR(STYLE,1,lnMajLen)
  lcStyleCod = Style
  XDESC  = DESC
  XSCALE = SCALE
  XCOLOR = SUBSTR(STYLE.Style,lnNonMajPo)

  STORE SPACE(3) TO XSIZE
  STORE 0 TO XSTK, XOTS, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XSUBEXT,XSTY,XTOTSTY,XSUBSTY

  *** To Get Number of Size Breakdown For Each Style
  XCNT = SCALE->CNT

  *** Load Array With Scale
  FOR I=1 TO XCNT
    Z=STR(I,1)
    XSIZE(I) = SCALE->SZ&Z
    XSTK(I) = STK&Z
  ENDFOR

  *B120384,1 ABD - The report layout should skip to the next page if all the information down can not fit on the page. [Begin]
  *IF ROW > 54
  IF ROW > 49
  *B120384,1 ABD -  [End]
  
    ROW=0
    PAGENO=PAGENO+1

    DO RPT_HDR WITH 'STY962C',XTITLE,R_WIDTH

    ROW=5
  ENDIF

  *- Abdou
  IF gcDevice='PRINTER'
    *@ PROW(),PCOL() SAY [(s#B3]
    @ PROW(),PCOL() SAY [(s3B]
  ENDIF
  *- Abdou

  *** Display Style, Description, And Scale Breakdown
  @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
  *- Abdou
  IF gcDevice='PRINTER'
    @ PROW(),PCOL() SAY [(s0B]
  ENDIF
  *- Abdou
  
  SELE STYLE

  COL=50
  FOR I=1 TO XCNT
    IF LEN(TRIM(XSIZE(I))) < 3
      XSIZE(I)=IIF(LEN(TRIM(XSIZE(I)))=1,'  '+TRIM(XSIZE(I)),XSIZE(I))
      XSIZE(I)=IIF(LEN(TRIM(XSIZE(I)))=2,' '+TRIM(XSIZE(I)),XSIZE(I))
    ENDIF
    @ ROW,COL SAY XSIZE(I)
    COL=COL+7
  ENDFOR

  *B127047,1 NNA 05/06/2005 (Begin) Assign the type of style and the price A and C
  lcStyleTyp = IIF(Style.Make,'DOM','IMP')
  lcPricA_C  = 'PRICE A/C : '+ALLTRIM(STR(Style.PriceA,13,2))+' / '+ALLTRIM(STR(Style.PriceC,13,2))
  *B127047,1 NNA (End)

  IF ROW=5
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
  ENDIF

  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('-',131)

  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl
    *** Get WIP From Cutting Ticket Temporary File
    SELE &CUTTTEMP
    GO TOP
    
    SEEK lcStyleCod
    
    IF FOUND()
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1

          DO RPT_HDR WITH 'STY962C',XTITLE,R_WIDTH

          ROW=5
          *- Abdou
          IF gcDevice='PRINTER'
            *@ PROW(),PCOL() SAY [(s#B3]
            @ PROW(),PCOL() SAY [(s3B]
          ENDIF
          *- Abdou
          
          *** Display Style, Description, And Scale Breakdown
  
          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)

          *- Abdou
          IF gcDevice='PRINTER'
            @ PROW(),PCOL() SAY [(s0B]
          ENDIF
          *- Abdou
          
          SELE &CUTTTEMP
  
          COL=50
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSIZE(I)
            COL=COL+7
          ENDFOR
          @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',131)
          XFIRSTTIME=.T.
        ENDIF
  
        XCUTTKT=CUTTKT
        XDATE=CUTTKTH->COMPLETE
        XSTAT=CUTTKTH->STATUS
        XCONTR1 = MFGOPRHD.cContCode
  
        SELE APVENDOR
        SEEK XCONTR1
        XNAME=IIF(FOUND(),cVenComp,SPACE(30))
  
        SELE &CUTTTEMP
  
        SCAN WHILE &XCUTSCAN
          FOR I=1 TO XCNT
            Z=STR(I,1)
            XCUT(I) = IIF(TRANCD='1', XCUT(I)+QTY&Z, XCUT(I)-QTY&Z)
            XCUT(I) = IIF(XCUT(I)>0, XCUT(I), 0)
          ENDFOR
        ENDSCAN
  
        FOR I=1 TO XCNT
          XTOTCUT=XTOTCUT+XCUT(I)
          XSUBCUT(I)=XSUBCUT(I)+XCUT(I)
        ENDFOR
  
        IF XWIPRPT .AND. XTOTCUT>0
          *** Display WIP Heading
          IF XFIRSTTIME
            ROW=ROW+1
            @ ROW,01 SAY XCUTHDR
            ROW=ROW+1
            @ ROW,01 SAY REPLICATE('-',46)
            XFIRSTTIME=.F.
          ENDIF
          ROW=ROW+1
          DO CASE
           CASE XWIPSORT='D'
              @ ROW,01 SAY XDATE
              @ ROW,10 SAY XCUTTKT
              @ ROW,17 SAY XCONTR1
              @ ROW,23 SAY SUBSTR(XNAME,1,24)
            CASE XWIPSORT='F'
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
  
          COL=48
          *** Display WIP Total Line Item
          FOR I=1 TO XCNT
            @ ROW,COL SAY XCUT(I)  PICTURE '99999'
            COL=COL+7
          ENDFOR
          @ ROW,104 SAY XTOTCUT  PICTURE '999999'
          @ ROW,129 SAY XSTAT
        ENDIF
  
        IF EOF() .OR. STYLE <> lcStyleCod
          EXIT
        ENDIF
        STORE 0 TO XCUT, XTOTCUT
      ENDDO
  
      IF XWIPRPT
        COL=47
        XSUBTOT=0
        ROW=ROW+1
        @ ROW,47 SAY REPLICATE('-',85)
        ROW=ROW+1
        @ ROW,21 SAY 'Cuttkt Subtotals .....'
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSUBCUT(I)  PICTURE '999999'
          XSUBTOT=XSUBTOT+XSUBCUT(I)
          COL=COL+7
        ENDFOR
        @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
        ROW=ROW+1
      ENDIF
    ENDIF
    
  ENDIF    && End of IF 'MF' $ gcComp_Mdl

  *-- If the "Style Purchase Order" or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    
    *** Get WIP From Purchase Order Temporary File
    SELE &POTEMP
    GO TOP
    
    SEEK lcStyleCod
    
    IF FOUND()
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1

          DO RPT_HDR WITH 'STY962C',XTITLE,R_WIDTH

          ROW=5
          *- Abdou
          IF gcDevice='PRINTER'
            *@ PROW(),PCOL() SAY [(s#B3]
            @ PROW(),PCOL() SAY [(s3B]
          ENDIF
          *- Abdou
          
          *** Display Style, Description, And Scale Breakdown
  
          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)

          *- Abdou
          IF gcDevice='PRINTER'
            @ PROW(),PCOL() SAY [(s0B]
          ENDIF
          *- Abdou
    
          SELE &POTEMP
  
          COL=50
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSIZE(I)
            COL=COL+7
          ENDFOR
          @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',131)
          XFIRSTTIME=.T.
        ENDIF
  
        XPO=PO
        XVENDOR=VENDOR
        XDATE=POSHDR->COMPLETE
        XSTAT=POSHDR->STATUS
        SELE APVENDOR
        SEEK XVENDOR
        XNAME=IIF(FOUND(),cVenComp,SPACE(30))
  
        SELE &POTEMP
        SCAN WHILE &XPOSCAN
          
          IF !INLIST(TranCd , '3' , '6')
            
            FOR I=1 TO XCNT
              Z=STR(I,1)
              xPOS(I) = IIF(TranCd = '1' ,;
                            xPOS(I) +;
                            (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                            xPOS(I) -;
                            (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
              XPOS(I) = IIF(XPOS(I)>0, XPOS(I), 0)
            ENDFOR
          ENDIF
        ENDSCAN
  
        FOR I=1 TO XCNT
          XTOTPOS=XTOTPOS+XPOS(I)
          XSUBPOS(I)=XSUBPOS(I)+XPOS(I)
        ENDFOR
  
        IF XWIPRPT .AND. XTOTPOS>0
          *** Display WIP Heading
          IF XFIRSTTIME
            ROW=ROW+1
            @ ROW,01 SAY XPOHDR
            ROW=ROW+1
            @ ROW,01 SAY REPLICATE('-',46)
            XFIRSTTIME=.F.
          ENDIF
          ROW=ROW+1
          DO CASE
            CASE XWIPSORT='D'
              @ ROW,01 SAY XDATE
              @ ROW,10 SAY XPO
              @ ROW,17 SAY XVENDOR
              @ ROW,23 SAY SUBSTR(XNAME,1,24)
            CASE XWIPSORT='F'
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
  
          COL=48
          *** Display WIP Total Line Item
          FOR I=1 TO XCNT
            @ ROW,COL SAY XPOS(I)  PICTURE '99999'
            COL=COL+7
          ENDFOR
          @ ROW,104 SAY XTOTPOS  PICTURE '999999'
          @ ROW,129 SAY XSTAT
        ENDIF
  
        IF EOF() .OR. STYLE <> lcStyleCod
          EXIT
        ENDIF
        STORE 0 TO XPOS, XTOTPOS
      ENDDO
  
      IF XWIPRPT
        COL=47
        XSUBTOT=0
        ROW=ROW+1
        @ ROW,47 SAY REPLICATE('-',85)
        ROW=ROW+1
        @ ROW,21 SAY 'P.O. Subtotals .......'
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSUBPOS(I)  PICTURE '999999'
          XSUBTOT=XSUBTOT+XSUBPOS(I)
          COL=COL+7
        ENDFOR
        @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
        ROW=ROW+1
      ENDIF
    ENDIF
    
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  IF !('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
    SELE STYLE
    GO TOP
    
    SEEK lcStyleCod
    
    IF FOUND()
      XFIRSTTIME=.T.
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1

          DO RPT_HDR WITH 'STY962C',XTITLE,R_WIDTH

          ROW=5
          *- Abdou
          IF gcDevice='PRINTER'
            *@ PROW(),PCOL() SAY [(s#B3]
            @ PROW(),PCOL() SAY [(s3B]
          ENDIF
          *- Abdou
          
          *** Display Style, Description, And Scale Breakdown
  
          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
          *- Abdou
          IF gcDevice='PRINTER'
            @ PROW(),PCOL() SAY [(s0B]
          ENDIF
          *- Abdou
          
          SELE STYLE
  
          COL=50
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSIZE(I)
            COL=COL+7
          ENDFOR
          @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',131)
          XFIRSTTIME=.T.
        ENDIF
  
        SELE STYLE
          FOR I=1 TO XCNT
            Z=STR(I,1)
            XSTY(I) = WIP&Z
          ENDFOR
        FOR I=1 TO XCNT
        
          XTOTSTY=XTOTSTY+XSTY(I)
          XSUBSTY(I)=XSUBSTY(I)+XSTY(I)
        ENDFOR

        IF XWIPRPT .AND. XTOTPOS>0
          *** Display WIP Heading

          COL=48
          *** Display WIP Total Line Item
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSTY(I)  PICTURE '99999'
            COL=COL+7
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

  *** Get Sales From Orders Temporary File
  SELE &ORDTEMP
  GO TOP
  
  SEEK lcStyleCod
  IF FOUND()
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1

        DO RPT_HDR WITH 'STY962C',XTITLE,R_WIDTH
        *- Abdou
        IF gcDevice='PRINTER'
          @ PROW(),PCOL() SAY [(s3B]
        ENDIF
        *- Abdou
        
        ROW=5
        *** Display Style, Description, And Scale Breakdown

        @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
        
        SELE &ORDTEMP

        COL=50
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSIZE(I)
          COL=COL+7
        ENDFOR
        *- Abdou
        IF gcDevice='PRINTER'
          @ PROW(),PCOL() SAY [(s0B]
        ENDIF
        *- Abdou
        @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('-',131)
        XFIRSTTIME=.T.
      ENDIF

      XORDER=ORDER
      XACCOUNT=ACCOUNT
      XDATE=COMPLETE
      XSTART=START
      XSTAT=IIF(ORDHDR->APPROVAL='DECLINE','D',ORDHDR->STATUS)
      XPIKTKT=IIF(PICKED=.T.,'Y',' ')
      XPRICE=PRICE
      SELE CUSTOMER
      SEEK 'M'+XACCOUNT
      XNAME=IIF(FOUND(),BTNAME,SPACE(30))

      SELE &ORDTEMP
      FOR I=1 TO XCNT
        Z=STR(I,1)
        XORD(I)=IIF(QTY&Z>0,QTY&Z,0)
        XTOTORD=XTOTORD+XORD(I)
        XSUBORD(I)=XSUBORD(I)+XORD(I)
      ENDFOR
      XEXT=XTOTORD*XPRICE
      XSUBEXT=XSUBEXT+XEXT
      XGRDORD=XGRDORD+XTOTORD
      XGRDPRI=XGRDPRI+XEXT
      IF XORDRPT .AND. XTOTORD>0
        *** Display ORD Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XORDHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',46)
          XFIRSTTIME=.F.
        ENDIF

        ROW=ROW+1
        DO CASE
          CASE XORDSORT='D'
            @ ROW,01 SAY XSTART
            @ ROW,10 SAY XDATE
            @ ROW,19 SAY XORDER
            @ ROW,26 SAY XACCOUNT
            @ ROW,32 SAY SUBSTR(XNAME,1,15)
          CASE XORDSORT='A'
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

        COL=48
        *** Display ORD Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XORD(I) PICTURE '99999'
          COL=COL+7
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
    ENDDO

    IF XORDRPT
      COL=47
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,47 SAY REPLICATE('-',85)
      ROW=ROW+1
      @ ROW,21 SAY 'Orders Subtotals .....'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
        XSUBTOT=XSUBTOT+XSUBORD(I)
        COL=COL+7
      ENDFOR
      @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
      @ ROW,118 SAY XSUBEXT  PICTURE '9999999.99'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS

  IF ROW > 49
    ROW=0
    PAGENO=PAGENO+1

    DO RPT_HDR WITH 'STY962C',XTITLE,R_WIDTH

    ROW=5
    *- Abdou
    IF gcDevice='PRINTER'
      *@ PROW(),PCOL() SAY [(s#B3]
      @ PROW(),PCOL() SAY [(s3B]
      
    ENDIF
    *- Abdou
    @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
    *- Abdou
    IF gcDevice='PRINTER'
      @ PROW(),PCOL() SAY [(s0B]
    ENDIF
    *- Abdou
    
    SELE &ORDTEMP

    COL=50
    FOR I=1 TO XCNT
      @ ROW,COL SAY XSIZE(I)
      COL=COL+7
    ENDFOR
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('-',131)
  ENDIF

  COL=47
  ROW=ROW+1

  *B127047,1 NNA 05/06/2005 (Begin) Print the style type and price A & C
  @ ROW,27 SAY lcStyleTyp
  @ ROW,35 SAY lcPricA_C
  ROW=ROW+1
  *B127047,1 NNA (End)
 
  *C125377,1 NNA 03/03/2005 (Begin) Print Stock per wareHouse/size
  *@ ROW,27 SAY 'STOCK  .........'
  *FOR I=1 TO XCNT
  *  @ ROW,COL SAY XSTK(I)  PICTURE '999999'
  *  XTOTSTK=XTOTSTK+XSTK(I)
  *  XOTS(I)=XOTS(I)+XSTK(I)
  *  COL=COL+7
  *ENDFOR
  *@ ROW,103 SAY XTOTSTK    PICTURE '9999999'
  IF SEEK(STYLE.STYLE,'STYDYE')
    SELECT STYDYE
    SCAN REST WHILE Style+cWarecode+Dyelot = STYLE.STYLE
      STORE 0 TO XTOTSTK
      @ ROW,27 SAY 'STOCK ' + PADR(ALLTRIM(cWarecode),10,'.') 
      FOR I=1 TO XCNT
        LCI = ALLTRIM(STR(I))
        @ ROW,COL SAY STYDYE.STK&LCI  PICTURE '999999'
        XTOTSTK=XTOTSTK+STYDYE.STK&LCI
        XOTS(I)=XOTS(I)+STYDYE.STK&LCI
        COL=COL+7
      ENDFOR
      @ ROW,103 SAY XTOTSTK    PICTURE '9999999'
      ROW=ROW+1
      COL=47
    ENDSCAN
  ENDIF
  *ROW=ROW+1
  *C125377,1 NNA (End)

  COL=46
  @ ROW,27 SAY 'W.I.P  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)  PICTURE '9999999'
    XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)
    XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)
    COL=COL+7
  ENDFOR

  @ ROW,103 SAY XTOTWIP       PICTURE '9999999'
  ROW=ROW+1
  COL=47

  *C120872,1 ABD - change the caption. [Begin]
  *@ ROW,27 SAY 'ORDERS .........'
  @ ROW,27 SAY 'CUST ORDERS ....'  
  *C120872,1 ABD - [End]
  
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
    XTOTORD=XTOTORD+XSUBORD(I)
    XOTS(I)=XOTS(I)-XSUBORD(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTORD       PICTURE '9999999'

  ROW=ROW+1
  @ ROW,27 SAY REPLICATE('-',105)
  ROW=ROW+1
  COL=47
  @ ROW,27 SAY 'O.T.S. .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XOTS(I)     PICTURE '999999'
 
    *C037661,1 NNA 03/02/2004 (Begin) Add Only The Positive Numbers to the total O.T.S
    *XTOTOTS=XTOTOTS+XOTS(I)
    XTOTOTS=XTOTOTS+IIf(XOTS(I)>0,XOTS(I),0)	&& if the size O.T.S Number is Positive then Add to the total
    *C037661,1 NNA (End)
     COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTOTS       PICTURE '9999999'

  SELE STYLE
    SKIP

  IF EOF()
    ROW=ROW+2
    IF ROW > 52
      ROW=0
      PAGENO=PAGENO+1
      DO RPT_HDR WITH 'STY962C',XTITLE,R_WIDTH

    ENDIF
    @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
    ROW=ROW+1
    @ ROW,27 SAY 'TOTAL PIECES ORDERED:'
    @ ROW,49 SAY XGRDORD  PICTURE '99999999'
    @ ROW,62 SAY 'TOTAL $$$ ORDERED $'
    @ ROW,82 SAY XGRDPRI  PICTURE '999,999,999.99'
    ROW=ROW+1
    @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
    EXIT
  ENDIF
  ROW=ROW+3
ENDDO
RETURN


*!*************************************************************
*! Name      : lfTermnate
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Function to terminate the report if neither
*!             the "Sales Order" nor the "Point of Sale" modules
*!             is installed.
*!*************************************************************
*! Called from : Default value of the report variable (llTermnate).
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
ENDIF    && End of IF !('SO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)

*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Called from : Option Grid
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

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Change account flag, in range browse screen.
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
SELECT Customer
SET ORDER TO Customer
GO TOP
SELECT(lcAliasCst)

*-- End of lfsrAcc.


*!*************************************************************
*! Name      : lfCreatFl
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : To create the temporary file that will prin the intransit
*!*************************************************************
*! Exaple   : =lfCreatFl()
*!*************************************************************
FUNCTION lfCreatFl
PRIVATE lnAlias, lcIndx

lnAlias = SELECT(0)
SELECT POSLN
DO CASE
  CASE XBYSIZE='N' .AND. XWIPSORT='D'
    lcIndx = 'SUBSTR(STYLE,1,lnMajLen)+DTOS(POSHDR->COMPLETE)+PO+TRANCD'
  CASE XBYSIZE='N' .AND. XWIPSORT='F'
    lcIndx = 'SUBSTR(STYLE,1,lnMajLen)+VENDOR+PO+TRANCD'
  CASE XBYSIZE='N' .AND. XWIPSORT='P'
    lcIndx = 'SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD'
ENDCASE

lnFStru = AFIELDS(laFStru)
DIMENSION laFStru[lnFStru+3 ,4]
laFStru[lnFStru+1,1] = 'Qty9'
laFStru[lnFStru+1,2] = 'N'
laFStru[lnFStru+1,3] = 6
laFStru[lnFStru+1,4] = 0

laFStru[lnFStru+2,1] = 'Qty10'
laFStru[lnFStru+2,2] = 'N'
laFStru[lnFStru+2,3] = 6
laFStru[lnFStru+2,4] = 0

laFStru[lnFStru+3,1] = 'Complete'
laFStru[lnFStru+3,2] = 'D'
laFStru[lnFStru+3,3] = 1
laFStru[lnFStru+3,4] = 0

*--Open Temp files exclusive.
lcOldExcl = SET('EXCL')
SET EXCLUSIVE ON
=gfCrtTmp(lcTmpShpFl,@laFStru,lcIndx,lcTmpShpFl)
SET EXCLUSIVE &lcOldExcl

*C125377,1 NNA 03/03/2005 (Begin) Create Temp table to hold the Warehouses total Stock per style/Color
CREATE TABLE (gcWorkDir+lcStoreTmp)(Style C(lnMajLen),cWarecode C(6),STKClr1 N(7),STKClr2 N(7),STKClr3 N(7),;
       STKClr4 N(7),STKClr5 N(7),STKClr6 N(7),STKClr7 N(7),STKClr8 N(7),STKClr9 N(7),STKClr10 N(7))
INDEX ON (Style+cWarecode) TAG (lcStoreTmp) OF (lcStoreTmp)
*C125377,1 NNA (End)

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfSoEntDat
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : SO entered date (from option grid)
*!*************************************************************
*! Exaple   : =lfSoEntDat()
*!*************************************************************
FUNCTION lfSoEntDat

PRIVATE ldFrom,ldTo

*B608174,1 NNA 07/24/2007 (Begin) Change ldSoSEntDt to ldRPSoStDt and  ldSoHEntDt to ldRPSoEnDt
*IF EMPTY(ldSoSEntDt) .AND. EMPTY(ldSoHEntDt)
IF EMPTY(ldRPSoStDt) .AND. EMPTY(ldRPSoEnDt)
*B608174,1 NNA (End)

  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  
  *B608174,1 NNA 07/24/2007 (Begin) Change ldSoSEntDt to ldRPSoStDt and  ldSoHEntDt to ldRPSoEnDt
  *ldFrom = ldSoSEntDt
  *ldTo   = ldSoHEntDt
  ldFrom = ldRPSoStDt
  ldTo   = ldRPSoEnDt
  *B608174,1 NNA (END)
  
ENDIF

lcTitle = 'Sales Order Entered Date'
DO DateRng.Spx

*B608174,1 NNA 07/24/2007 (Begin) Change ldSoSEntDt to ldRPSoStDt and  ldSoHEntDt to ldRPSoEnDt
*ldSoSEntDt = ldFrom
*ldSoHEntDt = ldTo
ldRPSoStDt = ldFrom
ldRPSoEnDt = ldTo
*B608174,1 NNA (End)

*!*************************************************************
*! Name      : lfShipDat
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : Shipping date (from option grid)
*!*************************************************************
*! Exaple   : =lfShipDat()
*!*************************************************************
FUNCTION lfShipDat

*B608174,1 NNA 07/24/2007 (Begin) Change ldStShipDt to ldRPSShpDt and  ldToShipDt to ldRPTShpDt
*IF EMPTY(ldStShipDt) .AND. EMPTY(ldToShipDt)
IF EMPTY(ldRPSShpDt) .AND. EMPTY(ldRPTShpDt)
*B608174,1 NNA (End)

  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To

  *B608174,1 NNA 07/24/2007 (Begin) Change ldStShipDt to ldRPSShpDt and  ldToShipDt to ldRPTShpDt
  *ldFrom = ldStShipDt
  *ldTo   = ldToShipDt
  ldFrom = ldRPSShpDt
  ldTo   = ldRPTShpDt
  *B608174,1 NNA (END)

ENDIF

lcTitle = 'Shipping Date'
DO DateRng.Spx
*B608174,1 NNA 07/24/2007 (Begin) Change ldStShipDt to ldRPSShpDt and  ldToShipDt to ldRPTShpDt
*ldStShipDt = ldFrom
*ldToShipDt = ldTo
ldRPSShpDt = ldFrom
ldRPTShpDt = ldTo
*B608174,1 NNA (End)
  
*!*************************************************************
*! Name      : lfGetBookQ
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : To calculate the booked qty
*!*************************************************************
*! Exaple   : =lfGetBookQ()
*!*************************************************************
FUNCTION lfGetBookQ
PRIVATE lnAlias
lnAlias = SELECT(0)
SELE &BookTemp

IF SEEK (XSTYLE)

  SCAN REST WHILE SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) = xStyle
    DO CASE
      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
        laBookQty(1) = laBookQty(1) + TotBook

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
        laBookQty(2) = laBookQty(2) + TotBook
 
      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
        laBookQty(3) = laBookQty(3) + TotBook

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
        laBookQty(4) = laBookQty(4) + TotBook
            
      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
        laBookQty(5) = laBookQty(5) + TotBook

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
        laBookQty(6) = laBookQty(6) + TotBook

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
        laBookQty(7) = laBookQty(7) + TotBook

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
        laBookQty(8) = laBookQty(8) + TotBook

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
        laBookQty(9) = laBookQty(9) + TotBook

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
        laBookQty(10) = laBookQty(10) + TotBook
    ENDCASE
  ENDSCAN
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetShipQ
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : To calculate the shipped qty
*!*************************************************************
*! Exaple   : =lfGetShipQ()
*!*************************************************************
FUNCTION lfGetShipQ
PRIVATE lnAlias

lnAlias = SELECT(0)
SELE &ShipTemp

IF SEEK (XSTYLE)

  SCAN REST WHILE SUBSTR(STYLE,1,lnMajLen)+Invoice+DTOS(InvDate) = xStyle
    DO CASE
      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
        laShipQty(1) = laShipQty(1) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
        laShipQty(2) = laShipQty(2) + TotQty
 
      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
        laShipQty(3) = laShipQty(3) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
        laShipQty(4) = laShipQty(4) + TotQty
            
      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
        laShipQty(5) = laShipQty(5) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
        laShipQty(6) = laShipQty(6) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
        laShipQty(7) = laShipQty(7) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
        laShipQty(8) = laShipQty(8) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
        laShipQty(9) = laShipQty(9) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
        laShipQty(10) = laShipQty(10) + TotQty
    ENDCASE
  ENDSCAN
ENDIF

*B124547,1 BWA 09/21/2004 Fix the problem of subtract the return value from the shipped value.
*B124547,1 BWA 09/21/2004 Comment this line.[START]
*
**C120872,1 ABD - Decrese the return Qty. [Begin]
*SELE (RETTEMP)
*IF SEEK (XSTYLE)
*  SCAN REST WHILE SUBSTR(STYLE,1,lnMajLen)+crmemo+DTOS(crdate) = xStyle
*    DO CASE
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
*        laShipQty(1) = laShipQty(1) - TotQty
*
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
*        laShipQty(2) = laShipQty(2) - TotQty
* 
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
*        laShipQty(3) = laShipQty(3) - TotQty
*
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
*        laShipQty(4) = laShipQty(4) - TotQty
*            
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
*        laShipQty(5) = laShipQty(5) - TotQty
*
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
*        laShipQty(6) = laShipQty(6) - TotQty
*
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
*        laShipQty(7) = laShipQty(7) - TotQty
*
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
*        laShipQty(8) = laShipQty(8) - TotQty
*
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
*        laShipQty(9) = laShipQty(9) - TotQty
*
*      CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
*        laShipQty(10) = laShipQty(10) - TotQty
*    ENDCASE
*  ENDSCAN
*ENDIF
**C120872,1 ABD - [End]

*B124547,1 BWA 09/21/2004.[END]

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetRtrnQ
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 08/05/2004
*! Purpose   : To calculate the return qty
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetRtrnQ()
*!*************************************************************
FUNCTION lfGetRtrnQ
PRIVATE lnAlias

lnAlias = SELECT(0)
*SELECT (RETRNEMP)
SELECT (RETTEMP)

IF SEEK(XSTYLE)

  SCAN REST WHILE SUBSTR(STYLE,1,lnMajLen) + CRMEMO = xStyle
    DO CASE
      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
        laRtrnQty(1) = laRtrnQty(1) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
        laRtrnQty(2) = laRtrnQty(2) + TotQty
 
      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
        laRtrnQty(3) = laRtrnQty(3) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
        laRtrnQty(4) = laRtrnQty(4) + TotQty
            
      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
        laRtrnQty(5) = laRtrnQty(5) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
        laRtrnQty(6) = laRtrnQty(6) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
        laRtrnQty(7) = laRtrnQty(7) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
        laRtrnQty(8) = laRtrnQty(8) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
        laRtrnQty(9) = laRtrnQty(9) + TotQty

      CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
        laRtrnQty(10) = laRtrnQty(10) + TotQty
    ENDCASE
  ENDSCAN
ENDIF
SELECT(lnAlias)

*--End of lfGetRtrnQ.

*!*************************************************************
*! Name      : lfPrntShpQ
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/15/2003
*! Purpose   : To print the in-transit
*!*************************************************************
*! Exaple   : =lfPrntShpQ()
*!*************************************************************
FUNCTION lfPrntShpQ
PRIVATE lnAlias, lnTot, laWipTot

DIMENSION laWipTot[10]
laWipTot = 0
lnAlias  = SELECT(0)

SELECT (lcTmpShpFl)
LOCATE
IF !EOF()
  IF ROW > 54
    ROW=0
    PAGENO=PAGENO+1

    
    DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH

    *- Abdou
    IF gcDevice='PRINTER'
      @ PROW(),PCOL() SAY [(s3B]
    ENDIF
    *- Abdou
    
    ROW=5
    *** Display Style, Description, And Range Of Colors

    *B122414,1 NNA 04/14/2004 (Begin) Always Show the style with his Discreption
    *@ ROW,01 SAY XSTYLE
    @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
    *B122414,1 NNA (End)

    COL=34
    FOR I=1 TO XCNT
      @ ROW,COL SAY XCLR(I)
      COL=COL+9
    ENDFOR
    *- Abdou
    IF gcDevice='PRINTER'
      @ PROW(),PCOL() SAY [(s0B]
    ENDIF
    *- Abdou
    
    @ ROW,125 SAY 'TOTAL'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('-',129)
    
    *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
    ROW=ROW+1
    
    *C120872,1 ABD - Print the style type and price A & C. [Begin]
    *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
    @ ROW,13 SAY lcStyleTyp
    @ ROW,20 SAY lcPricA_C
    *C120872,1 ABD - [End]
    
    *C120872,1 ABD - [End]
    
  ENDIF

  ROW=ROW+1
  @ ROW,01 SAY XPOHDR
  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('-',22)
  ROW=ROW+1
  SELECT (lcTmpShpFl)
  SCAN
    IF ROW > 54
      ROW=0
      PAGENO=PAGENO+1

      DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH
      *- Abdou
      IF gcDevice='PRINTER'
        @ PROW(),PCOL() SAY [(s3B]
      ENDIF
      *- Abdou
      
      ROW=5
      *** Display Style, Description, And Range Of Colors

      *B122414,1 NNA 04/14/2004 (Begin) Always Show the style with his Discreption
      *@ ROW,01 SAY XSTYLE
      @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
      *B122414,1 NNA (End)

      COL=34
      FOR I=1 TO XCNT
        @ ROW,COL SAY XCLR(I)
        COL=COL+9
      ENDFOR
      *- Abdou
      IF gcDevice='PRINTER'
        @ PROW(),PCOL() SAY [(s0B]
      ENDIF
      *- Abdou
      
      @ ROW,125 SAY 'TOTAL'
      ROW=ROW+1
      @ ROW,01 SAY REPLICATE('-',129)

      *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
      ROW=ROW+1
      
      *C120872,1 ABD - Print the style type and price A & C. [Begin]
      *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
      @ ROW,13 SAY lcStyleTyp
      @ ROW,20 SAY lcPricA_C
      *C120872,1 ABD - [End]

      *C120872,1 ABD - [End]
      
    ENDIF

    DO CASE
      CASE XWIPSORT='D'
        @ ROW,01 SAY Complete
        @ ROW,10 SAY PO
        @ ROW,17 SAY Vendor
      CASE XWIPSORT='F'
        @ ROW,01 SAY Vendor
        @ ROW,08 SAY PO
        @ ROW,15 SAY Complete
      OTHERWISE
        @ ROW,01 SAY PO
        @ ROW,08 SAY Vendor
        @ ROW,15 SAY Complete
    ENDCASE
  
    COL=32
    *** Display WIP Total Line Item
    lnTot = 0
    FOR I=1 TO XCNT
      lcI = ALLTRIM(STR(I,2))
      @ ROW,COL SAY Qty&lcI  PICTURE '9999999'
      lnTot    = lnTot + Qty&lcI
      laWipTot[I] = laWipTot[I] + Qty&lcI
      COL=COL+9
    ENDFOR
    @ ROW,122 SAY lnTot  PICTURE '99999999'
    ROW = ROW + 1
  ENDSCAN
  
  IF ROW > 54
    ROW=0
    PAGENO=PAGENO+1

    DO RPT_HDR WITH 'STY961C',XTITLE,R_WIDTH
    *- Abdou
    IF gcDevice='PRINTER'
      @ PROW(),PCOL() SAY [(s3B]
    ENDIF
    *- Abdou
    
    ROW=5
    *** Display Style, Description, And Range Of Colors

    *B122414,1 NNA 04/14/2004 (Begin) Always Show the style with his Discreption
    *@ ROW,01 SAY XSTYLE
    @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
    *B122414,1 NNA (End)

    COL=34
    FOR I=1 TO XCNT
      @ ROW,COL SAY XCLR(I)
      COL=COL+9
    ENDFOR
    *- Abdou
    IF gcDevice='PRINTER'
      @ PROW(),PCOL() SAY [(s0B]
    ENDIF
    *- Abdou
    
    @ ROW,125 SAY 'TOTAL'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('-',129)

    *C120872,1 ABD - Print Imp or Dom into the report. [Begin]
    ROW=ROW+1
    
    *C120872,1 ABD - Print the style type and price A & C. [Begin]
    *@ ROW,lnMajLen+2 SAY IIF(Style.Make,'DOM','IMP')
    @ ROW,13 SAY lcStyleTyp
    @ ROW,20 SAY lcPricA_C
    *C120872,1 ABD - [End]
    
  ENDIF
  ROW=ROW+1
  
  COL=31
  lnWipTot = 0
  @ ROW,06 SAY 'WIP Subtotals ........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY laWipTot(I)  PICTURE '99999999'
    lnWipTot = lnWipTot + laWipTot(I)
    COL=COL+9
  ENDFOR
  @ ROW,121 SAY lnWipTot  PICTURE '999999999'
  ROW=ROW+2  
  ZAP
ENDIF
SELECT(lnAlias)

FUNCTION lfSumShpQt
PRIVATE lnAlias

lnAlias = SELECT(0)
DIMENSION laPOShpQty[10]
laPOShpQty = 0

SELECT (lcTmpShpFl)
SCAN
  FOR I=1 TO XCNT
    lcI = ALLTRIM(STR(I,2))
    laPOShpQty[I] = laPOShpQty[I] + Qty&lcI
  ENDFOR
ENDSCAN
IF lcRPWIPSta = "N"
  SELECT (lcTmpShpFl)
  ZAP
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfStoreStk
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Get Stock for the style/color in every Warehouse
*!*************************************************************
*! Exaple   : =lfStoreStk()
*!*************************************************************
*!C125377,1 
FUNCTION lfStoreStk
PRIVATE lnAlias,LCI,LCR,lnStockClr
lnAlias  = SELECT(0)
IF SEEK(STYLE.STYLE,'STYDYE')
  SELECT STYDYE
  LCI=ALLTRIM(STR(I))
  SCAN REST WHILE Style+cWarecode+Dyelot = STYLE.STYLE
    STORE 0 TO lnStockClr
    FOR R = 1 TO 8
      LCR = ALLTRIM(STR(R))
      lnStockClr = lnStockClr + STK&LCR
    ENDFOR
    IF SEEK(XStyle+cWarecode,lcStoreTmp)
      SELECT(lcStoreTmp)
      REPLACE STKCLR&LCI WITH lnStockClr
    ELSE
      SELECT(lcStoreTmp)
      APPEND BLANK
      REPLACE STYLE 	 WITH XSTYLE			,;
              CWARECODE  WITH STYDYE.cWarecode	,;
              STKCLR&LCI WITH lnStockClr
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnAlias)
*--End of function lfStoreStk.