*:***************************************************************************
*: Program file  : ICSCUTSO.PRG
*: Program desc. : Special Cut & Sold
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSCUTSO
*:***************************************************************************
*B802003,1  HS  03/11/99 Fix the following problems:
*B802003,1                 1) WIP data print incorrectly.
*B802003,1                 2) ORDERS data prints zero in all cases.
*B602805,1  HS  04/15/99 Fix the following problems:
*B602805,1                 1) PO Complete date is always empty.
*B602805,1                 2) Take into consideration the differences
*B602805,1                    between Aria26 and Aria27 when calculating
*B602805,1                    the W.I.P.:
*B602805,1                    a) The Inter-location PO records '(TranCd '6')'
*B602805,1                       in the "POSLN" file should not effect the
*B602805,1                       W.I.P.
*B602805,1                    b) Also take into consideration the PO type
*B602805,1                       "PO Return".
*B802167,1  HS  04/21/99 Prevent using any file before checking that at
*B802167,1               least one of the modules that this file is
*B802167,1               assigned to is installed.
*B802399,1  WAB - 07/25/99 - style filter needs to be changed to style browse 
*B802399,1                  to enable select range 		
*B802590,1  RAMY  09/22/99 In the selection grid, click on sales order comp.
*                          date. or prod order comp. date. Type any date, click ok to exit. Go back in
*                          and the date is no longer there.
*B602823,1  MHM  07/27/00 neither the "Manufacturing" nor the "Style Purchase Order or Point of Sale"
*                           modules is installed get the W.I.P. from the style file.
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*B604870,1 TMI 09/05/2001 Fix the bug that WIP is duplicated if MF is installed and No PO,PS
*C102474,1 RAE 02/10/2002 Add a new Royalty Inlist field. (Done as standard)
*B605419,1 BWA 01/28/2002 Fix the bug of the broken fields in case there is no records to display.
*E301857,1 BWA 03/06/2002 Add 2 filters to the option grid which is sort by customer and the allocated for the order.
*B605720,1 BWA 03/26/2002 Fix the bug of report does not show style description in the size break = NO
*B120384,1 ABD 12/24/2003 The report layout should skip to the next page if all the information down can not fit on the page.
*B122318,1 BWA 04/08/2004 Fix the bug of not printing the WIP in case selecting Account for the PO case.
*B123012,1 NNA 05/30/2004 Fix bug that the Sales order date Filter Doesn't Filter Correctly if you 
*B123012,1 NNA            Fill Just To Date To mean that you want all the Completed Date that less
*B123012,1 NNA            than the Selected Date
*B124889,1 BWA 11/02/2004 Fix the bug of printing the displayed color-size mixed with each other.
*B128500,1 BWA 06/26/2005 Fix the bug of diffrenet values for the same style when printing the report with the option print style size ( Yes & No ).
*:***************************************************************************

*-- lcRepNmTtl variable taht hold the header of the non major segment for the frx
*-- llDontPrn  variable that showes that no records match report criteria
*-- PAGENO     variable taht hold Page No.
*-- ROW        variable taht hold Row No.
*-- XTIME      variable taht hold time
*-- R_WIDTH    variable taht hold report width
*-- R_TITLE    variable taht hold report title
*-- WORKTEMP   variable taht hold the name of work temp. file
*-- CUTTTEMP   variable taht hold the name of cuttkt temp. file
*-- POTEMP     variable taht hold the name of PO temp. file
*-- ORDTEMP    variable taht hold the name of ORDHDR temp. file
*-- XPRTWIP    variable taht showes print WIP Summary or Detail
*-- XBYSIZE    variable taht showes print by size
*-- XSLDATE    variable taht hold from sales order comp. date
*-- XSHDATE    variable taht hold to   sales order comp. date
*-- XPLDATE    variable taht hold from Prod. order comp. date
*-- XPHDATE    variable taht hold to   Prod. order comp. date
*-- XWIPSORT   variable taht showes if it is sorted by WIP or not
*-- XPRTORD    variable taht showes print Sales Summary or Detail
*-- XORDSORT   variable taht hold the sales order Sort
*-- XSTATHLD   variable taht showes if include Orders on hold
*-- XALLOCAT   variable taht showes if include Allocated Qty. or All Qty.
*-- XTITLE     variable taht hold optional title
*-- XFILTER    variable taht hold report filter expression

*B124889,1 BWA 11/02/2004 Initialize the variables for the style , color and scale.[START]
*--Initialize the variables.

STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
=lfChkStrct()
llExtSize = gfGetMemVar('M_USEEXSSC')
*B124889,1 BWA 11/02/2004.[END]

llDontPrn = .F.
PAGENO=0
ROW=0
XTIME=TIME()
R_WIDTH = ''
R_TITLE = ''

STORE SPACE(0) TO WORKTEMP,CUTTTEMP,POTEMP,ORDTEMP,XPRTWIP,XWIPRPT,;
                  XPRTORD,XORDRPT,XORDRPT,XSTAT

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
XBYSIZE  = IIF(llRPBySize,'Y','N')
XSLDATE  = ldRPSLDate
XSHDATE  = ldRPSHDate
XPLDATE  = ldRPPLDate
XPHDATE  = ldRPPHDate
XPRTWIP  = lcRPWIPSta
XWIPSORT = lcRPWIPSor
XPRTORD  = lcRPSalSta
XORDSORT = lcRPSALSor
XSTATHLD = IIF(llRPHolOrd,'Y','N')
XALLOCAT = lcRPAllQty
XTITLE   = lcRPTitle

*E301857,1 BWA 03/06/2002 Cut the customer filter from the lcrpexp to use it in 
*E301857,1 BWA 03/06/2002 filtering on the POSLN and ORDLINE files.[START]
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
*E301857,1 BWA 03/06/2002.[END]

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
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
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

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
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
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Get major seg. picture
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

*!*************************************************************
*! Name      : lfvSODate
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate sales order date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSODate()
*!*************************************************************

FUNCTION lfvSODate

PRIVATE ldFrom,ldTo

*B802590,1 RAMY when entering the sales order comp. date [start]

* If empty the vars that hold the dates From and To

*STORE {} TO ldFrom,ldTo

IF EMPTY(ldRPSLDate) .AND. EMPTY(ldRPSHDate)
  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  ldFrom = ldRPSLDate
  ldTo = ldRPSHDate
ENDIF && end of (IF EMPTY(ldRPSLDate) )
*B802590,1 RAMY when entering the sales order comp. date [end]

lcTitle = 'Sales order comp. date'
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

ldRPSLDate = ldFrom
ldRPSHDate = ldTo

*!*************************************************************
*! Name      : lfvPODate
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate production order date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPODate()
*!*************************************************************

FUNCTION lfvPODate

PRIVATE ldFrom,ldTo

*B802590,1 RAMY when entering the sales order comp. date [start]

* If empty the vars that hold the dates From and To

*STORE {} TO ldFrom,ldTo

IF EMPTY(ldRPPLDate) .AND. EMPTY(ldRPPHDate)
  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  ldFrom = ldRPPLDate
  ldTo = ldRPPHDate
ENDIF && end of (IF EMPTY(ldRPSLDate) )
*B802590,1 RAMY when entering the sales order comp. date [end]


lcTitle = 'Prod. order comp. date'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
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
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate print WIP Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWIPSuDt()
*!*************************************************************

FUNCTION lfvWIPSuDt
*-B602823,1  MHM  07/27/00 Disable Tow Option of WIP in Optoin Grid[Begin]
*lcWIPSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPWIPSor'),1)
*laOGObjCnt[lcWIPSorPo] = lcRPWIPSta = 'D'
*= lfOGShowGet('lcRPWIPSor')
IF ('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  lcWIPSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPWIPSor'),1)
  laOGObjCnt[lcWIPSorPo] = lcRPWIPSta = 'D'
  = lfOGShowGet('lcRPWIPSor')
ENDIF
*-B602823,1  MHM  07/27/00 Disable Tow Option of WIP in Optoin Grid[End]

*!*************************************************************
*! Name      : lfvSalSuDt
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate print Sales Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSalSuDt()
*!*************************************************************

FUNCTION lfvSalSuDt

lcSalSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPSalSor'),1)
laOGObjCnt[lcSalSorPo] = lcRPSalSta = 'D'
= lfOGShowGet('lcRPSalSor')

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate Style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
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
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
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
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Collecting data in Temp. File
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
XFILTER=XFILTER+'.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'
SET FILTER TO &XFILTER
GO TOP

IF EOF()
  Wait wind 'NO RECORDS SELECTED!!!'

  *B605419,1 BWA 01/28/2002 The page broken after this message because the device didn't return to its original status which is screen.[START]
  SET DEVICE TO SCREEN
  *B605419,1 BWA 01/28/2002.[END]

  llDontPrn = .T.
  RETURN
ENDIF

********  EXTRACT RECORDS FROM CUTTING TICKET, PO, AND ORDERS **************

WORKTEMP = gfTEMPNAME()
CUTTTEMP = gfTEMPNAME()
POTEMP   = gfTEMPNAME()
ORDTEMP  = gfTEMPNAME()

*B802167,1 Add this line to check if the "Manufacturing" module is installed
*          before using the "Cut Ticket" files [Begin]

*-- If the "Manufacturing" module is installed
IF 'MF' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Manufacturing" module [End]
  
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
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO(),7) TAG &CUTTTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
    CASE XBYSIZE='N' .AND. XWIPSORT='D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(CUTTKTH->COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='F'
      *--AAMER (Start)
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKTH->CONTR1+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+MFGOPRHD.cContCode+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      *--AAMER (End)
    CASE XBYSIZE='N' .AND. XWIPSORT='P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='D'
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(CUTTKTH->COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      INDEX ON STYLE + DTOS(CUTTKTH.COMPLETE) + CUTTKT + TRANCD +;
               STR(RECNO(),7) TAG &CUTTTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
    CASE XBYSIZE='Y' .AND. XWIPSORT='F'
      *--AAMER (Start)
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKTH->CONTR1+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+MFGOPRHD.cContCode+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      INDEX ON STYLE + MFGOPRHD.cContCode + CUTTKT + TRANCD +;
               STR(RECNO() , 7) TAG &CUTTTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
      *--AAMER (End)
    CASE XBYSIZE='Y' .AND. XWIPSORT='P'
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+MFGOPRHD.cContCode+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO() , 7) TAG &CUTTTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
  ENDCASE
  
*B802167,1 Add this line to check if the "Manufacturing" module is installed
*          before using the "Cut Ticket" files [Begin]
ENDIF    && End of IF 'MF' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Manufacturing" module [End]

*B802167,1 Add this line to check if the "Style Purchase Order" or the
*          "Point of Sale" Modules is installed before using the
*          " Purchase Order" files [Begin]

*-- If the "Style Purchase Order" or the "Point of Sale" Modules
*-- is installed
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Style Purchase Order"... [End]
  
  SELECT POSHDR
  SELECT POSLN
  
  *B602805,1 Change this line for we have changed the Index expression [Begin]
  *SET RELATION TO PO INTO POSHDR
  SET RELATION TO cStyType + PO INTO POSHDR
  *B602805,1 Change this line for we have changed the Index expression [End]
  
  COPY STRU TO &gcWorkDir.&POTEMP
  USE &gcWorkDir.&POTEMP IN 0 EXCLUSIVE
  SELECT (POTEMP)
  
  *B602805,1 Change this line for we have changed the Index expression [Begin]
  *SET RELATION TO PO INTO POSHDR
  SET RELATION TO cStyType + PO INTO POSHDR
  *B602805,1 Change this line for we have changed the Index expression [End]
  
  DO CASE
    CASE XBYSIZE='N' .AND. XPRTWIP='S'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='Y' .AND. XPRTWIP='S'
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
    CASE XBYSIZE='N' .AND. XWIPSORT='D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(POSHDR->COMPLETE)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='F'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+VENDOR+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='N' .AND. XWIPSORT='P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE='Y' .AND. XWIPSORT='D'
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(POSHDR->COMPLETE)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
      INDEX ON STYLE + DTOS(POSHDR.COMPLETE) + PO + TRANCD +;
               STR(RECNO() , 7) TAG &POTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
    CASE XBYSIZE='Y' .AND. XWIPSORT='F'
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+VENDOR+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
      INDEX ON STYLE + VENDOR + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
    CASE XBYSIZE='Y' .AND. XWIPSORT='P'
      
      *B802003,1 Change this line to use the whole Style code in the index to
      *          prevent duplicate numbers [Begin]
      *INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
      *B802003,1 Change this line to use the whole Style code [End]
      
  ENDCASE
  
*B802167,1 Add this line to check if the "Style Purchase Order" or the
*          "Point of Sale" Modules is installed before using the
*          "Purchase Order" files [Begin]
ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Style Purchase Order"... [End]

SELECT ORDHDR
SELECT ORDLINE

*B802003,1 Change this line for we have changed the Index expression [Begin]
*SET RELATION TO ORDER INTO ORDHDR
SET RELATION TO cOrdType + Order INTO ORDHDR
*B802003,1 Change this line for we have changed the Index expression [End]

COPY STRU TO &gcWorkDir.&ORDTEMP
USE &gcWorkDir.&ORDTEMP IN 0 EXCLUSIVE
SELECT (ORDTEMP)

*B802003,1 Change this line for we have changed the Index expression [Begin]
*SET RELATION TO ORDER INTO ORDHDR
SET RELATION TO cOrdType + Order INTO ORDHDR
*B802003,1 Change this line for we have changed the Index expression [End]

DO CASE
  CASE XBYSIZE='N' .AND. XPRTORD='S'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='Y' .AND. XPRTORD='S'
    
    *B802003,1 Change this line to use the whole Style code in the index to
    *          prevent duplicate numbers [Begin]
    *INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
    *B802003,1 Change this line to use the whole Style code [End]
    
  CASE XBYSIZE='N' .AND. XORDSORT='D'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(COMPLETE)+ORDER+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='N' .AND. XORDSORT='A'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ACCOUNT+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='N' .AND. XORDSORT='O'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE='Y' .AND. XORDSORT='D'
    
    *B802003,1 Change this line to use the whole Style code in the index to
    *          prevent duplicate numbers [Begin]
    *INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(COMPLETE)+ORDER+STORE+STR(LINENO,6) TAG &ORDTEMP
    INDEX ON STYLE + DTOS(COMPLETE) + ORDER + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
    *B802003,1 Change this line to use the whole Style code [End]
    
  CASE XBYSIZE='Y' .AND. XORDSORT='A'
    
    *B802003,1 Change this line to use the whole Style code in the index to
    *          prevent duplicate numbers [Begin]
    *INDEX ON SUBSTR(STYLE,1,lnMajLen)+ACCOUNT+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
    INDEX ON STYLE + ACCOUNT + ORDER + DTOS(COMPLETE) + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
    *B802003,1 Change this line to use the whole Style code [End]
    
  CASE XBYSIZE='Y' .AND. XORDSORT='O'
    
    *B802003,1 Change this line to use the whole Style code in the index to
    *          prevent duplicate numbers [Begin]
    *INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE +;
             STR(LINENO , 6) TAG &ORDTEMP
    *B802003,1 Change this line to use the whole Style code [End]
    
ENDCASE

*B802167,1 Add this line to check if the "Manufacturing" or the
*          "Style Purchase Order" or the "Point of Sale" Modules
*          is installed before using the CUTPICK file [Begin]

*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Manufacturing"... [End]

  *-- Openning the file CutPick with a relation to OrdLine according 
  *-- to the CutOrd index.
  =gfOpenFile(gcDataDir+'CutPick','CutOrd','SH')
  SELECT OrdLine 
  SET RELATION TO '2'+Order+STR(LineNo,6) INTO CutPick ADDITIVE

*B802167,1 Add this line to check if the "Manufacturing" or the
*          "Style Purchase Order" or the "Point of Sale" Modules
*          is installed before using the CUTPICK file [Begin]
ENDIF    && End of IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Manufacturing"... [End]

*B802167,1 Add this line to check if the "Style Purchase Order" or the
*          "Point of Sale" Modules is installed before using the
*          "Purchase Order" files [Begin]

*-- If the "Style Purchase Order" or the "Point of Sale" modules
*-- is installed
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Style Purchase Order"... [End]
  
  *-- Openning the file CutPick with a relation to PosLn according 
  *-- to the CutPick index in another work area.
  SELECT 0 
  USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick2
  SET ORDER TO CutPick
  SELECT PosLn 
  SET RELATION TO '2'+PO+Style INTO CutPick2 ADDITIVE
  
*B802167,1 Add this line to check if the "Style Purchase Order" or the
*          "Point of Sale" Modules is installed before using the
*          "Purchase Order" files [Begin]
ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Style Purchase Order"... [End]

*B802167,1 Add this line to check if the "Manufacturing" or the
*          "Style Purchase Order" or the "Point of Sale" Modules
*          is installed before using the CUTPICK file [Begin]

*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Manufacturing"... [End]
  
  *-- Openning the file CutPick with a relation to OrdLine according 
  *-- to the CutOrd index in another work area to check if the order 
  *-- was allocated by cuttkt not by PO.
  SELECT 0 
  USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick3
  SET ORDER TO CutOrd
  SELECT OrdLine 
  SET RELATION TO '1'+Order+STR(LineNo,6) INTO CutPick3 ADDITIVE
  
*B802167,1 Add this line to check if the "Manufacturing" or the
*          "Style Purchase Order" or the "Point of Sale" Modules
*          is installed before using the CUTPICK file [Begin]
ENDIF    && End of IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
*B802167,1 Add this line to check if the "Manufacturing"... [End]

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

  *B802003,1 Change this line to use the whole Style code to prevent
  *          duplicate records in the temp. file [Begin]
  *XSTYLE=SUBSTR(STYLE,1,lnMajLen)
  xStyle = Style
  *B802003,1 Change this line to use the whole Style code to prevent [End]

  *B802167,1 Add this line to check if the "Manufacturing" module is installed
  *          before using the "Cut Ticket" files [Begin]
  
  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Manufacturing" module [End]
    
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
    
  *B802167,1 Add this line to check if the "Manufacturing" module is installed
  *          before using the "Cut Ticket" files [Begin]
  ENDIF    && End of IF 'MF' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Manufacturing" module [End]
  
  *B802167,1 Add this line to check if the "Style Purchase Order" or the
  *          "Point of Sale" Modules is installed before using the
  *          "Purchase Order" files [Begin]
  
  *-- If the " Style Purchase Order " or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Style Purchase Order"... [End]
    
    *** Store off Records Into Second Temp File POTEMP
    SELE POSLN
    SEEK XSTYLE
    IF FOUND()
    
	  *B122318,1 BWA 04/08/2004 Comment the relation in case the PO dueto the Account filed 
	  *B122318,1 BWA 04/08/2004  will be filled in one case that the user will create PO and select Customer not location in the PO screen.[START]
      *E301857,1 BWA 03/06/2002 Create relation with customer to be able to use the customer filter.[START]
      *IF !('POSLN.ACCOUNT INTO CUSTOMER' $ SET('RELATION'))
      *  SET RELATION TO "M" + Posln.ACCOUNT INTO Customer ADDITIVE
      *ENDIF  
      *E301857,1 BWA 03/06/2002.[END]
      *B122318,1 BWA 04/08/2004.[END]

      XADD = ''
      DO CASE
        CASE XALLOCAT = 'L'
          XADD = ' .AND.  CutPick2->CtktNo = PosLn->Po'
        CASE XALLOCAT = 'N'
          XADD = ' .AND.  CutPick2->CtktNo <> PosLn->Po'
      ENDCASE

      IF XPLDATE<>CTOD('  /  /    ')
      
		*B122318,1 BWA 04/08/2004 Return to the old code and didn't use the customer filter.[START] 
        *E301857,1 BWA 03/06/2002 Add the customer filter "lcCustAcnt" to the criteria.[START]
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
             FOR (BETWEEN(POSHDR->COMPLETE,XPLDATE,XPHDATE) .AND. ;
             POSHDR->STATUS<>'X' .AND. POSHDR->STATUS<>'C' .AND. ;
             TOTQTY<>0 &XADD)

        *COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
             FOR (BETWEEN(POSHDR->COMPLETE,XPLDATE,XPHDATE) .AND. ;
             POSHDR->STATUS<>'X' .AND. POSHDR->STATUS<>'C' .AND. ;
             TOTQTY<>0 &XADD &lcCustAcnt)
        *E301857,1 BWA 03/06/2002.[END]

      ELSE

        *E301857,1 BWA 03/06/2002 Add the customer filter "lcCustAcnt" to the criteria.[START]
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
             FOR (POSHDR->STATUS<>'X' .AND. POSHDR->STATUS<>'C' .AND. ;
             TOTQTY<>0 &XADD)
        *COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
             FOR (POSHDR->STATUS<>'X' .AND. POSHDR->STATUS<>'C' .AND. ;
             TOTQTY<>0 &XADD &lcCustAcnt)
        *E301857,1 BWA 03/06/2002.[END]
        *B122318,1 BWA 04/08/2004.[END] 

      ENDIF

      *E301857,1 BWA 03/06/2002 Break the relation with the customer file.[START]
      SELECT POSLN
      IF 'POSLN.ACCOUNT INTO CUSTOMER' $ SET('RELATION')
        SET RELATION OFF INTO Customer
      ENDIF  
      *E301857,1 BWA 03/06/2002.[END]

      SELE &POTEMP
      APPEND FROM &gcWorkDir.&WORKTEMP

    ENDIF
    
  *B802167,1 Add this line to check if the "Style Purchase Order" or the
  *          "Point of Sale" Modules is installed before using the
  *          "Purchase Order" files [Begin]
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Style Purchase Order"... [End]

  *** Store off Records Into Third Temp File ORDTEMP
  SELE ORDLINE
  SEEK XSTYLE
  IF FOUND()

    *E301857,1 BWA 03/06/2002 Create relation with customer to be able to use the customer filter.[START]
    IF !('ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION'))
      SET RELATION TO "M" + ORDLINE.ACCOUNT INTO Customer ADDITIVE
    ENDIF  
    *E301857,1 BWA 03/06/2002.[END]

    XADDORD = ''
    
    *B802167,1 Add this line to check if the "Manufacturing" or the
    *          "Style Purchase Order" or the "Point of Sale" Modules
    *          is installed before using the CUTPICK file [Begin]
    
    *-- If the "Manufacturing" or the "Style Purchase Order" or the
    *-- "Point of Sale" modules is installed
    IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    *B802167,1 Add this line to check if the "Manufacturing"... [End]
      
      DO CASE
        CASE XALLOCAT = 'L'
          XADDORD = ' .AND.  (OrdLine->Order = CutPick->Order .OR. ' + ;
                    ' OrdLine->Order = CutPick3->Order) '
        CASE XALLOCAT = 'N'
          XADDORD = ' .AND.  (OrdLine->Order <>CutPick->Order .AND.' + ;
                    ' OrdLine->Order <> CutPick3->Order) '
      ENDCASE
      
    *B802167,1 Add this line to check if the "Manufacturing" or the
    *          "Style Purchase Order" or the "Point of Sale" Modules
    *          is installed before using the CUTPICK file [Begin]
    ENDIF    && End of IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    *B802167,1 Add this line to check if the "Manufacturing"... [End]

    IF XSLDATE<>CTOD('  /  /    ')
      
      *B802003,1 Change this line to skip the contracts [Begin]
      *COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
      *     FOR (BETWEEN(COMPLETE,XSLDATE,XSHDATE) .AND. ;
      *     ORDHDR->STATUS $ XSTAT .AND. TOTQTY<>0 &XADDORD)

      *E301857,1 BWA 03/06/2002 Add the customer filter "lcCustAcnt" and the allocated filter to the criteria.[START]
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style = xStyle ;
            FOR BETWEEN(Complete , xSLDate , xSHDate) .AND. ;
                ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O' &XADDORD	

      COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style = xStyle ;
            FOR BETWEEN(Complete , xSLDate , xSHDate)   .AND. ;
                ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
      *E301857,1 BWA 03/06/2002.[END]

      *B802003,1 Change this line to skip the contracts [End]
      
    ELSE
      
      *B802003,1 Change this line to skip the contracts [Begin]
      *COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
      *     FOR (ORDHDR->STATUS $ XSTAT .AND. TOTQTY<>0 &XADDORD)

      *E301857,1 BWA 03/06/2002 Add the customer filter "lcCustAcnt" and the allocated filter to the criteria.[START]
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style = xStyle ;
            FOR ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O' &XADDORD

      *B123012,1 NNA 05/30/2004 (Begin) If User input only the [To] Date We make a Filter to 
      *B123012,1 NNA            the Completed Dates that less than the Selected Date .and. if 
      *B123012,1 NNA            there are no Dates Selected We Make a Filter For all dates
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
          WHILE Style = xStyle ;
            FOR ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
      COPY REST TO &gcWorkDir.&WORKTEMP WHILE Style = xStyle ;
            FOR IIF(xSHDate <> CTOD('  /  /    '),Complete,'') <= IIF(xSHDate <> CTOD('  /  /    ') , xSHDate ,'') .AND. ;
                ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O' .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
      *B123012,1 NNA (End)

      *E301857,1 BWA 03/06/2002.[END]

      *B802003,1 Change this line to skip the contracts [End]
      
    ENDIF

    *E301857,1 BWA 03/06/2002 Break the relation with the customer file.[START]
    SELECT ORDLINE
    IF 'ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION')
      SET RELATION OFF INTO Customer
    ENDIF  
    *E301857,1 BWA 03/06/2002.[END]

    SELE &ORDTEMP
    APPEND FROM &gcWorkDir.&WORKTEMP
  ENDIF

  SELE STYLE
ENDSCAN

*B802167,1 Change these lines to close the file only if it was opened [Begin]
*SELECT CutPick 
*USE
IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF    && End of IF USED('CUTPICK')
*B802167,1 Change these lines to close the file only if it was opened [End]

*B802167,1 Change these lines to close the file only if it was opened [Begin]
*SELECT CutPick2
*USE
IF USED('CUTPICK2')
  USE IN CUTPICK2
ENDIF    && End of IF USED('CUTPICK2')
*B802167,1 Change these lines to close the file only if it was opened [End]

*B802167,1 Change these lines to close the file only if it was opened [Begin]
*SELECT CutPick3
*USE
IF USED('CUTPICK3')
  USE IN CUTPICK3
ENDIF    && End of IF USED('CUTPICK3')
*B802167,1 Change these lines to close the file only if it was opened [End]

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
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Print report
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

IF XBYSIZE='N'
  DO STY961                                 && No Breakdown In Size
ELSE
  DO STY962                                 && Breakdown In Size
ENDIF

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
STORE 0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD
*-B602823,1  MHM  07/27/00 add 2 array to get wip from style file [Begin]
*DIMENSION XCOLOR(10),XCLR(10),XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),;
          XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10)

*B124889,1 BWA 11/02/2004 Add array hold scale values.[START]
*DIMENSION  XCOLOR(10),XCLR(10),XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),;
          XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10),XSTY(10),XSUBSTY(10)

DIMENSION  XCOLOR(10),XCLR(10),XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),;
          XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10),XSTY(10),XSUBSTY(10),XSCALE(10)
*B124889,1 BWA 11/02/2004.[END]

*-B602823,1  MHM  07/27/00 add 2 array to get wip from style file [End]
*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE XWIPSORT='D'                          && Sort by Completion Date
    XCUTSCAN='(CUTTKTH->COMPLETE=XDATE .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN='(POSHDR->COMPLETE=XDATE .AND. PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR='COMPLETE CUTTKT CONTR1'
    XPOHDR='COMPLETE PO #   VENDOR'
  CASE XWIPSORT='F'                          && Sort by Contractor
    *--AAMER (Start)
    *XCUTSCAN='(CUTTKTH->CONTR1=XCONTR1 .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTSCAN='(MFGOPRHD.cContCode=XCONTR1 .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    *--AAMER (End)
    XPOSCAN='(POSHDR->VENDOR=XVENDOR .AND. PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR='CONTR1 CUTTKT COMPLETE'
    XPOHDR='VENDOR PO #   COMPLETE'
  OTHERWISE                                 && Sort by Cutting Ticket/PO
    XCUTSCAN='(CUTTKTH->CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN='(PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR='CUTTKT CONTR1 COMPLETE'
    XPOHDR='PO #   VENDOR COMPLETE'
ENDCASE

*** Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  CASE XORDSORT='D'                         && Sort by Completion Date
    XORDSCAN='(COMPLETE=XDATE .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XORDHDR='COMPLETE ORDER   ACCT#'
  CASE XORDSORT='A'                         && Sort by Account
    XORDSCAN='(ACCOUNT=XACCOUNT .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XORDHDR='ACCT# ORDER   COMPLETE'
  OTHERWISE                                 && Sort by Orders
    XORDSCAN='(ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XORDHDR='ORDER  ACCT#  COMPLETE'
ENDCASE

SELE STYLE
GO TOP

DO WHILE INKEY() <> 32
  XSTYLE=SUBSTR(STYLE,1,lnMajLen)
  XDESC=DESC

  STORE SPACE(5) TO XCLR

  *B124889,1 BWA 11/02/2004 Fill the scale variable with the wanted length.[START]
  STORE SPACE(lnScaLnGl) TO XSCALE
  *B124889,1 BWA 11/02/2004.[END]

  STORE SPACE(6) TO XCOLOR         &&TAK06/21/94
  *STORE 0 TO XSTK, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD,;
             XSUBORD, XTOTORD, XOTS
  STORE 0 TO XSTK, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD,;
             XSUBORD, XTOTORD, XOTS,XTOTSTY,XSTY,XSUBSTY

  *** To Get Number of Color For Each Style
  COUNT ALL FOR (SUBSTR(STYLE,1,lnMajLen)=XSTYLE) TO XCLRCNT

  *** Load Array With Color and Stock
  SEEK XSTYLE                               && To Place Pointer Back to 1st Match

  XCNT=IIF(XCLRCNT>10, 10, XCLRCNT)

  FOR I=1 TO XCNT

    *B124889,1 BWA 11/02/2004 Fill the scale array with the scale suffix.[START]
    *XCLR(I)   = SUBSTR(STYLE.Style,lnNonMajPo)
    IF llExtSize
      XCLR(I)   = SUBSTR(STYLE.Style, lnClrPosGl , lnClrLnGl)
      XSCALE(I) = SUBSTR(STYLE.Style, lnScaPosGl , lnScaLnGl)
    ELSE
        XCLR(I)   = SUBSTR(STYLE.Style,lnNonMajPo)
    ENDIF
    *B124889,1 BWA 11/02/2004.[END]

    XCOLOR(I) = SUBSTR(STYLE.Style,lnNonMajPo)
    XSTK(I)   = TOTSTK
    SKIP
  ENDFOR

  IF XCLRCNT>10
    XCLR(10) = 'OTHER'
    FOR I=11 TO XCLRCNT
      XSTK(10)=XSTK(10)+TOTSTK
      SKIP
    ENDFOR
  ENDIF
  
  *B120384,1 ABD - The report layout should skip to the next page if all the information down can not fit on the page. [Begin]
  *IF ROW > 54
  *IF ROW > 49
  IF ROW > 46
    *B120384,1 ABD -  [End]
    
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
    ROW=5
  ENDIF

  *** Display Style, Description, And Range Of Colors
  *B605720,1 BWA 03/26/2002 Fix the bug of report does not print style description.[START]
  *@ ROW,01 SAY XSTYLE
  @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,19)
  *B605720,1 BWA 03/26/2002.[END]

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
 
  IF ROW=5
    @ ROW,125 SAY 'TOTAL'
  ENDIF
  
    *B124889,1 BWA 11/02/2004 Print the scale suffix in another line.[START]
    IF llExtSize
      COL=34
      ROW=ROW+1
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSCALE(I)
        COL=COL+9
      ENDFOR
    ENDIF
    *B124889,1 BWA 11/02/2004.[END]

  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('-',129)

  *B802167,1 Add this line to check if the "Manufacturing" module is installed
  *          before using the "Cut Ticket" files [Begin]
  
  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Manufacturing" module [End]
    
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
          DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
          ROW=5
          *** Display Style, Description, And Range Of Colors
          @ ROW,01 SAY XSTYLE
          COL=34
          FOR I=1 TO XCNT
            @ ROW,COL SAY XCLR(I)
            COL=COL+9
          ENDFOR
          @ ROW,125 SAY 'TOTAL'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',129)
          XFIRSTTIME=.T.
        ENDIF
  
        XCUTTKT=CUTTKT
        XDATE=CUTTKTH->COMPLETE
        
        *--AAMER (Start)
        *XCONTR1=CUTTKTH->CONTR1
        XCONTR1 = MFGOPRHD.cContCode
        *--AAMER (End)
        
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
          @ ROW,122 SAY XTOTCUT  PICTURE '99999999'
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
        @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
        ROW=ROW+1
      ENDIF
    ENDIF
  
  *B802167,1 Add this line to check if the "Manufacturing" module is installed
  *          before using the "Cut Ticket" files [Begin]
  ENDIF    && End of IF 'MF' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Manufacturing" module [End]
  
  *B802167,1 Add this line to check if the "Style Purchase Order" or the
  *          "Point of Sale" Modules is installed before using the
  *          "Purchase Order" files [Begin]
  
  *-- If the " Style Purchase Order " or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Style Purchase Order"... [End]
    
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
          DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
          ROW=5
          *** Display Style, Description, And Range Of Colors
          @ ROW,01 SAY XSTYLE
          COL=34
          FOR I=1 TO XCNT
            @ ROW,COL SAY XCLR(I)
            COL=COL+9
          ENDFOR
          @ ROW,125 SAY 'TOTAL'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',129)
          XFIRSTTIME=.T.
        ENDIF
  
        XPO=PO
        XVENDOR=VENDOR
        XDATE=POSHDR->COMPLETE
        
        *B128500,1 BWA 06/26/2005 Initialize the arrays and the variables used in the collecting values.[START]
        DIMENSION laChkQty1(10) , laChkQty2(10) , laChkQty3(10) , laChkQty4(10) , laChkQty5(10)
        DIMENSION laChkQty6(10) , laChkQty7(10) , laChkQty8(10) , laChkQty9(10) , laChkQty10(10)
        STORE 0 TO laChkQty1 , laChkQty2 , laChkQty3 , laChkQty4 , laChkQty5
        STORE 0 TO laChkQty6 , laChkQty7 , laChkQty8 , laChkQty9 , laChkQty10
        STORE 0 TO lnTotQty1 , lnTotQty2 , lnTotQty3 , lnTotQty4 , lnTotQty5
        STORE 0 TO lnTotQty6 , lnTotQty7 , lnTotQty8 , lnTotQty9 , lnTotQty10
        *B128500,1 BWA 06/26/2005.[END]

        SCAN WHILE &XPOSCAN
          
          *B128500,1 BWA 06/26/2005 Search the scale file for the number of the sizes.
          *--The old Code at the end of the Program.
          lnXcnt = IIF(SEEK("S" + SUBSTR(EVAL(POTEMP+'.style'),lnScaPosGl,lnScaLnGl) , 'SCALE') , SCALE.CNT , 0)
          *B128500,1 BWA 06/26/2005.[END]

          *B602805,1 Change this line to skip the Inter-location PO records
          *          (TranCd '6') [Begin]
          *IF TRANCD<>'3'                      && Not In-Transit
          IF !INLIST(TranCd , '3' , '6')
          *B602805,1 Change this line to skip the Inter-location PO [End]
          
            *B128500,1 BWA 06/26/2005 Fix the bug of diffrenet values for the same style when printing the report with the option print style size ( Yes & No ).
            DO CASE
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty1(I) = IIF(TranCd = '1' , laChkQty1(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                   laChkQty1(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty1(I) = IIF( laChkQty1(I) > 0 , laChkQty1(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty1
                FOR I = 1 TO lnXcnt
                  lnTotQty1 =  lnTotQty1 + laChkQty1(I)
                ENDFOR
                XPOS(1) = IIF(lnTotQty1 > 0 , lnTotQty1 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
                
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty2(I) = IIF(TranCd = '1' , laChkQty2(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty2(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty2(I) = IIF( laChkQty2(I) > 0 , laChkQty2(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty2
                FOR I = 1 TO lnXcnt
                  lnTotQty2 =  lnTotQty2 + laChkQty2(I)
                ENDFOR
                XPOS(2) = IIF(lnTotQty2 > 0 , lnTotQty2 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty3(I) = IIF(TranCd = '1' , laChkQty3(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty3(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty3(I) = IIF( laChkQty3(I) > 0 , laChkQty3(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty3
                FOR I = 1 TO lnXcnt
                  lnTotQty3 =  lnTotQty3 + laChkQty3(I)
                ENDFOR
                XPOS(3) = IIF(lnTotQty3 > 0 , lnTotQty3 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty4(I) = IIF(TranCd = '1' , laChkQty4(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty4(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty4(I) = IIF( laChkQty4(I) > 0 , laChkQty4(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty4
                FOR I = 1 TO lnXcnt
                  lnTotQty4 =  lnTotQty4 + laChkQty4(I)
                ENDFOR
                XPOS(4) = IIF(lnTotQty4 > 0 , lnTotQty4 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty5(I) = IIF(TranCd = '1' , laChkQty5(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty5(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty5(I) = IIF( laChkQty5(I) > 0 , laChkQty5(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty5
                FOR I = 1 TO lnXcnt
                  lnTotQty5 =  lnTotQty5 + laChkQty5(I)
                ENDFOR
                XPOS(5) = IIF(lnTotQty5 > 0 , lnTotQty5 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty6(I) = IIF(TranCd = '1' , laChkQty6(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty6(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty6(I) = IIF( laChkQty6(I) > 0 , laChkQty6(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty6
                FOR I = 1 TO lnXcnt
                  lnTotQty6 =  lnTotQty6 + laChkQty6(I)
                ENDFOR
                XPOS(6) = IIF(lnTotQty6 > 0 , lnTotQty6 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty7(I) = IIF(TranCd = '1' , laChkQty7(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty7(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty7(I) = IIF( laChkQty7(I) > 0 , laChkQty7(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty7
                FOR I = 1 TO lnXcnt
                  lnTotQty7 =  lnTotQty7 + laChkQty7(I)
                ENDFOR
                XPOS(7) = IIF(lnTotQty7 > 0 , lnTotQty7 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty8(I) = IIF(TranCd = '1' , laChkQty8(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty8(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty8(I) = IIF( laChkQty8(I) > 0 , laChkQty8(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty8
                FOR I = 1 TO lnXcnt
                  lnTotQty8 =  lnTotQty8 + laChkQty8(I)
                ENDFOR
                XPOS(8) = IIF(lnTotQty8 > 0 , lnTotQty8 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty9(I) = IIF(TranCd = '1' , laChkQty9(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                    laChkQty9(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty9(I) = IIF( laChkQty9(I) > 0 , laChkQty9(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty9
                FOR I = 1 TO lnXcnt
                  lnTotQty9 =  lnTotQty9 + laChkQty9(I)
                ENDFOR
                XPOS(9) = IIF(lnTotQty9 > 0 , lnTotQty9 , 0)

              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty10(I) = IIF(TranCd = '1' , laChkQty10(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                     laChkQty10(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty10(I) = IIF( laChkQty10(I) > 0 , laChkQty10(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty10
                FOR I = 1 TO lnXcnt
                  lnTotQty10 =  lnTotQty10 + laChkQty10(I)
                ENDFOR
                XPOS(10) = IIF(lnTotQty10 > 0 , lnTotQty10 , 0)

              OTHERWISE
                FOR I=1 TO lnXcnt
                  Z=STR(I,1)
                  laChkQty10(I) = IIF(TranCd = '1' , laChkQty10(I) + (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                                                     laChkQty10(I) - (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    			  laChkQty10(I) = IIF( laChkQty10(I) > 0 , laChkQty10(I) , 0)
                ENDFOR
                STORE 0 TO lnTotQty10
                FOR I = 1 TO lnXcnt
                  lnTotQty10 =  lnTotQty10 + laChkQty10(I)
                ENDFOR
                XPOS(10) = IIF(lnTotQty10 > 0 , lnTotQty10 , 0)
                
            ENDCASE
            *B128500,1 BWA 06/26/2005.[END]

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
            @ ROW,01 SAY REPLICATE('-',22)
            XFIRSTTIME=.F.
          ENDIF
          ROW=ROW+1
          DO CASE
            CASE XWIPSORT='D'
              @ ROW,01 SAY XDATE
              @ ROW,10 SAY XPO
              @ ROW,17 SAY XVENDOR
            CASE XWIPSORT='F'
              @ ROW,01 SAY XVENDOR
              @ ROW,08 SAY XPO
              @ ROW,15 SAY XDATE
            OTHERWISE
              @ ROW,01 SAY XPO
              @ ROW,08 SAY XVENDOR
              @ ROW,15 SAY XDATE
          ENDCASE
  
          COL=32
          *** Display WIP Total Line Item
          FOR I=1 TO XCNT
            @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
            COL=COL+9
          ENDFOR
          @ ROW,122 SAY XTOTPOS  PICTURE '99999999'
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
        @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
        ROW=ROW+1
      ENDIF
    ENDIF
    
  *B802167,1 Add this line to check if the "Style Purchase Order" or the
  *          "Point of Sale" Modules is installed before using the
  *          "Purchase Order" files [Begin]
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Style Purchase Order"... [End]
  *-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
  *          "Point of Sale" or "Manufacturing" Modules is not installed [Begin]
  *--- Get WIP From Syle File
  *B604870,1 TMI [Start] Add the condition that if MF is not installed  
  *IF !('PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  IF !('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  *B604870,1 TMI [End  ]  
    SELE STYLE
    GO TOP
    SEEK XSTYLE
    IF FOUND()
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
          ROW=5
          *** Display Style, Description, And Range Of Colors
          @ ROW,01 SAY XSTYLE
          COL=34
          FOR I=1 TO XCNT
            @ ROW,COL SAY XCLR(I)
            COL=COL+9
          ENDFOR
          @ ROW,125 SAY 'TOTAL'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',129)
          XFIRSTTIME=.T.
        ENDIF
        SELECT STYLE  
        SCAN WHILE SUBSTR(STYLE,1,lnMajLen)=XSTYLE
          DO CASE
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
              XSTY(1)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
              XSTY(2)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
              XSTY(3)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
              XSTY(4)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
              XSTY(5)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
              XSTY(6)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
              XSTY(7)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
              XSTY(8)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
              XSTY(9)=TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
              XSTY(10)=TOTWIP
            OTHERWISE
              XSTY(10)=TOTWIP
          ENDCASE
        ENDSCAN
  
        FOR I=1 TO XCNT
          XTOTSTY=XTOTSTY+XSTY(I)
          XSUBSTY(I)=XSUBSTY(I)+XSTY(I)
        ENDFOR
  
        IF XWIPRPT .AND. XTOTPOS>0
          *** Display WIP Heading
          ROW=ROW+1
  
          COL=32
          *** Display WIP Total Line Item
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSTY(I)  PICTURE '9999999'
            COL=COL+9
          ENDFOR
          @ ROW,122 SAY XTOTSTY  PICTURE '99999999'
        ENDIF
  
        IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
          EXIT
        ENDIF
        STORE 0 TO XSTY, XTOTSTY
      ENDDO
  
      IF XWIPRPT
        COL=31
        XSUBTOT=0
        ROW=ROW+1
        @ ROW,31 SAY REPLICATE('-',99)
        ROW=ROW+1
        @ ROW,06 SAY 'P.O. Subtotals ........'
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSUBSTY(I)  PICTURE '99999999'
          XSUBTOT=XSUBTOT+XSUBSTY(I)
          COL=COL+9
        ENDFOR
        @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
        ROW=ROW+1
      ENDIF
    ENDIF
  ENDIF  
  *-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
  *          "Point of Sale" or "Manufacturing" Modules is not installed [End]
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
        DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
        ROW=5
        *** Display Style, Description, And Range Of Colors
        @ ROW,01 SAY XSTYLE
        COL=34
        FOR I=1 TO XCNT
          @ ROW,COL SAY XCLR(I)
          COL=COL+9
        ENDFOR
        @ ROW,125 SAY 'TOTAL'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('-',129)
        XFIRSTTIME=.T.
      ENDIF

      XORDER=ORDHDR->ORDER
      XACCOUNT=ACCOUNT
      XDATE=COMPLETE

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
          OTHERWISE
            XORD(10)=XORD(10)+TOTQTY
        ENDCASE
      ENDSCAN

      FOR I=1 TO XCNT
        XTOTORD=XTOTORD+XORD(I)
        XSUBORD(I)=XSUBORD(I)+XORD(I)
      ENDFOR

      IF XORDRPT .AND. XTOTORD>0
        *** Display ORD Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XORDHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',22)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE XORDSORT='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XORDER
            @ ROW,18 SAY XACCOUNT
          CASE XORDSORT='A'
            @ ROW,01 SAY XACCOUNT
            @ ROW,07 SAY XORDER
            @ ROW,15 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XORDER
            @ ROW,08 SAY XACCOUNT
            @ ROW,15 SAY XDATE
        ENDCASE

        COL=32
        *** Display ORD Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XORD(I) PICTURE '9999999'
          COL=COL+9
        ENDFOR
        @ ROW,122 SAY XTOTORD  PICTURE '99999999'
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
      @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS

  IF ROW > 49
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
    ROW=5
    @ ROW,01 SAY XSTYLE
    COL=34
    FOR I=1 TO XCNT
      @ ROW,COL SAY XCLR(I)
      COL=COL+9
    ENDFOR
    @ ROW,125 SAY 'TOTAL'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('-',129)
  ENDIF

  COL=32
  ROW=ROW+1
  @ ROW,13 SAY 'STOCK  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSTK(I)  PICTURE '9999999'
    XTOTSTK=XTOTSTK+XSTK(I)
    XOTS(I)=XOTS(I)+XSTK(I)
    COL=COL+9
  ENDFOR
  @ ROW,122 SAY XTOTSTK    PICTURE '99999999'
  XGRDSTK=XGRDSTK+XTOTSTK

  ROW=ROW+1
  COL=32
  @ ROW,13 SAY 'W.I.P  .........'
  *-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
  *          "Point of Sale" or "Manufacturing" Modules is not installed [Begin]

  *  FOR I=1 TO XCNT
  *    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)  PICTURE '9999999'
  *    XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)
  *    XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)
  *    COL=COL+9
  *  ENDFOR
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)  PICTURE '9999999'
    XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)
    XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)
    COL=COL+9
  ENDFOR
  *-B602823,1  MHM  07/27/00 get wip from style file [End]
  @ ROW,122 SAY XTOTWIP       PICTURE '99999999'
  XGRDWIP=XGRDWIP+XTOTWIP

  ROW=ROW+1
  COL=32
  @ ROW,13 SAY 'ORDERS .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBORD(I)  PICTURE '9999999'
    XTOTORD=XTOTORD+XSUBORD(I)
    XOTS(I)=XOTS(I)-XSUBORD(I)
    COL=COL+9
  ENDFOR
  @ ROW,122 SAY XTOTORD       PICTURE '99999999'
  XGRDORD=XGRDORD+XTOTORD

  ROW=ROW+1
  @ ROW,13 SAY REPLICATE('-',117)
  ROW=ROW+1
  COL=31
  @ ROW,13 SAY 'O.T.S. .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XOTS(I)     PICTURE '99999999'
    XTOTOTS=XTOTOTS+XOTS(I)
    COL=COL+9
  ENDFOR
  @ ROW,121 SAY XTOTOTS       PICTURE '999999999'

  SELE STYLE
  SCAN WHILE SUBSTR(STYLE,1,lnMajLen)=XSTYLE
  ENDSCAN

  IF EOF()
    ROW=ROW+2
    IF ROW > 52
      ROW=0
      PAGENO=PAGENO+1
      DO RPT_HDR WITH 'STY961',XTITLE,R_WIDTH
    ENDIF
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
    ROW=ROW+1
    @ ROW,29 SAY 'GRAND TOTALS:  STOCK:'
    @ ROW,51 SAY XGRDSTK  PICTURE '99999999'
    @ ROW,62 SAY 'W.I.P:'
    @ ROW,69 SAY XGRDWIP  PICTURE '99999999'
    @ ROW,80 SAY 'ORDERS:'
    @ ROW,88 SAY XGRDORD  PICTURE '99999999'
    @ ROW,99 SAY 'O.T.S:'
    @ ROW,106 SAY XGRDSTK+XGRDWIP-XGRDORD  PICTURE '99999999'
    ROW=ROW+1
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
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
*-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
*          "Point of Sale" or "Manufacturing" Modules is not installed[Begin]
*--DIMENSION XSTK(8),XOTS(8),XSIZE(8),XCUT(8),XSUBCUT(8),XPOS(8),XSUBPOS(8),XORD(8),XSUBORD(8)
DIMENSION XSTK(8),XOTS(8),XSIZE(8),XCUT(8),XSUBCUT(8),XPOS(8),XSUBPOS(8),XORD(8),XSUBORD(8),XSTY(8),XSUBSTY(8)
*-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
*          "Point of Sale" or "Manufacturing" Modules is not installed [End]

*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE XWIPSORT='D'                          && Sort by Completion Date
    
    *B802003,1 Change this line to use the whole Style code in the search
    *          expression to prevent duplicate numbers [Begin]
    *XCUTSCAN='(SUBSTR(STYLE,1,lnMajLen)+DTOS(CUTTKTH->COMPLETE)+CUTTKT)=(XSTYLE+DTOS(XDATE)+XCUTTKT)'
    XCUTSCAN='(STYLE+DTOS(CUTTKTH->COMPLETE)+CUTTKT)=(lcStyleCod+DTOS(XDATE)+XCUTTKT)'
    *B802003,1 Change this line to use the whole Style code [End]
    
    XCUTHDR='COMPLETE CUTTKT CONTR NAME'
    
    *B802003,1 Change this line to use the whole Style code in the search
    *          expression to prevent duplicate numbers [Begin]
    *XPOSCAN='(SUBSTR(STYLE,1,lnMajLen)+DTOS(POSHDR->COMPLETE)+PO)=(XSTYLE+DTOS(XDATE)+XPO)'
    XPOSCAN='(STYLE+DTOS(POSHDR->COMPLETE)+PO)=(lcStyleCod+DTOS(XDATE)+XPO)'
    *B802003,1 Change this line to use the whole Style code [End]
    
    XPOHDR='COMPLETE PO #  VENDOR NAME'
  CASE XWIPSORT='F'                          && Sort by Contractor
    
    *-- AAMER (Start)
    *XCUTSCAN='(SUBSTR(STYLE,1,lnMajLen)+CUTTKTH->CONTR1+CUTTKT)=(XSTYLE+XCONTR1+XCUTTKT)'
    
    *B802003,1 Change this line to use the whole Style code in the search
    *          expression to prevent duplicate numbers [Begin]
    *XCUTSCAN='(SUBSTR(STYLE,1,lnMajLen)+MFGOPRHD.cContCode+CUTTKT)=(XSTYLE+XCONTR1+XCUTTKT)'
    XCUTSCAN='(STYLE+MFGOPRHD.cContCode+CUTTKT)=(lcStyleCod+XCONTR1+XCUTTKT)'
    *B802003,1 Change this line to use the whole Style code [End]
    
    *-- AAMER (End)
    XCUTHDR='CONTR NAME'+SPACE(21)+'CUTTKT COMPLETE'
    
    *B802003,1 Change this line to use the whole Style code in the search
    *          expression to prevent duplicate numbers [Begin]
    *XPOSCAN='(SUBSTR(STYLE,1,lnMajLen)+VENDOR+PO)=(XSTYLE+XVENDOR+XPO)'
    XPOSCAN='(STYLE+VENDOR+PO)=(lcStyleCod+XVENDOR+XPO)'
    *B802003,1 Change this line to use the whole Style code [End]
    
    XPOHDR='VENDOR NAME'+SPACE(20)+'PO #   COMPLETE'
  OTHERWISE                                  && Sort by Cutting Ticket/PO
    
    *B802003,1 Change this line to use the whole Style code in the search
    *          expression to prevent duplicate numbers [Begin]
    *XCUTSCAN='(SUBSTR(STYLE,1,lnMajLen)+CUTTKT)=(XSTYLE+XCUTTKT)'
    XCUTSCAN='(STYLE+CUTTKT)=(lcStyleCod+XCUTTKT)'
    *B802003,1 Change this line to use the whole Style code [End]
    
    XCUTHDR='CUTTKT CONTR NAME'+SPACE(21)+'COMPLETE'
    
    *B802003,1 Change this line to use the whole Style code in the search
    *          expression to prevent duplicate numbers [Begin]
    *XPOSCAN='(SUBSTR(STYLE,1,lnMajLen)+PO)=(XSTYLE+XPO)'
    XPOSCAN='(STYLE+PO)=(lcStyleCod+XPO)'
    *B802003,1 Change this line to use the whole Style code [End]
    
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
  
  *B802003,1 Add this line to add a new variable to hold the whole Style
  *          code [Begin]
  lcStyleCod = Style
  *B802003,1 Add this line to add a new variable [End]
  
  XDESC  = DESC
  XSCALE = SCALE
  XCOLOR = SUBSTR(STYLE.Style,lnNonMajPo)

  STORE SPACE(3) TO XSIZE
*-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
*          "Point of Sale" or "Manufacturing" Modules is not installed [Begin]
*--  STORE 0 TO XSTK, XOTS, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XSUBEXT
  STORE 0 TO XSTK, XOTS, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XSUBEXT,XSTY,XTOTSTY,XSUBSTY
*-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
*          "Point of Sale" or "Manufacturing" Modules is not installed[End]

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
    DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
    ROW=5
  ENDIF

  *** Display Style, Description, And Scale Breakdown
  @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
  
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
  IF ROW=5
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
  ENDIF

  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('-',131)

  *B802167,1 Add this line to check if the "Manufacturing" module is installed
  *          before using the "Cut Ticket" files [Begin]
  
  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Manufacturing" module [End]
    
    *** Get WIP From Cutting Ticket Temporary File
    SELE &CUTTTEMP
    GO TOP
    
    *B802003,1 Change this line to use the whole Style code in the search to
    *          prevent duplicate numbers [Begin]
    *SEEK XSTYLE
    SEEK lcStyleCod
    *B802003,1 Change this line to use the whole Style code in the search [End]
    
    IF FOUND()
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
          ROW=5
          *** Display Style, Description, And Scale Breakdown
  
          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
          
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
  
        *XCONTR1=CUTTKTH->CONTR1
              
        *-- AAMER (Start)
        *DIMENSION laMinSeq[2]
        *STORE SPACE(0) TO laMinSeq
  
        *SELECT MIN(cOperSeq),cContCode INTO ARRAY laMinSeq ;
          FROM MFGOPRHD ;
          WHERE cimtyp+ctktno = 'M'+XCUTTKT
        
        *XCONTR1 = laMinSeq[2]
        XCONTR1 = MFGOPRHD.cContCode
        *-- AAMER (End)
  
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
  
        *B802003,1 Change this line to use the new variable that holds the
        *          whole Style code [BEGIN]
        *IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE .OR. SUBSTR(STYLE.Style,lnNonMajPo)<>XCOLOR
        IF EOF() .OR. STYLE <> lcStyleCod
        *B802003,1 Change this line to use the new variable that holds [End]
        
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
    
  *B802167,1 Add this line to check if the "Manufacturing" module is installed
  *          before using the "Cut Ticket" files [Begin]
  ENDIF    && End of IF 'MF' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Manufacturing" module [End]

  *B802167,1 Add this line to check if the "Style Purchase Order" or the
  *          "Point of Sale" Modules is installed before using the
  *          "Purchase Order" files [Begin]
  
  *-- If the "Style Purchase Order" or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Style Purchase Order"... [End]
    
    *** Get WIP From Purchase Order Temporary File
    SELE &POTEMP
    GO TOP
    
    *B802003,1 Change this line to use the whole Style code in the search to
    *          prevent duplicate numbers [Begin]
    *SEEK XSTYLE
    SEEK lcStyleCod
    *B802003,1 Change this line to use the whole Style code in the search [End]
    
    IF FOUND()
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
          ROW=5
          *** Display Style, Description, And Scale Breakdown
  
          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
    
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
          
          *B602805,1 Change this line to skip the Inter-location PO records
          *          (TranCd '6') [Begin]
          *IF TRANCD<>'3'                    && trancd=3 -> In-transit
          IF !INLIST(TranCd , '3' , '6')
          *B602805,1 Change this line to skip the Inter-location PO [End]
            
            FOR I=1 TO XCNT
              Z=STR(I,1)
              
              *B602805,1 Change this line to take into consideration the
              *          PO Return case [Begin]
              *XPOS(I) = IIF(TRANCD='1', XPOS(I)+QTY&Z, XPOS(I)-QTY&Z)
              xPOS(I) = IIF(TranCd = '1' ,;
                            xPOS(I) +;
                            (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                            xPOS(I) -;
                            (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
              
              *B602805,1 Change this line to take into consideration [End]
              
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
  
        *B802003,1 Change this line to use the new variable that holds the
        *          whole Style code [BEGIN]
        *IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE .OR. SUBSTR(STYLE.Style,lnNonMajPo)<>XCOLOR
        IF EOF() .OR. STYLE <> lcStyleCod
        *B802003,1 Change this line to use the new variable that holds [End]
        
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
    
  *B802167,1 Add this line to check if the "Style Purchase Order" or the
  *          "Point of Sale" Modules is installed before using the
  *          "Purchase Order" files [Begin]
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  *B802167,1 Add this line to check if the "Style Purchase Order"... [End]

  *-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
  *          "Point of Sale" or "Manufacturing" Modules is not installed [Begin]
  *B604870,1 TMI [Start] Add the condition that if MF is not installed  
  *IF !('PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  IF !('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  *B604870,1 TMI [End  ]  
    SELE STYLE
    GO TOP
    
    SEEK lcStyleCod
    
    IF FOUND()
      XFIRSTTIME=.T.
      *DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
          ROW=5
          *** Display Style, Description, And Scale Breakdown
  
          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
          
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
        *B802003,1 Change this line to use the new variable that holds the
        *          whole Style code [BEGIN]
        *IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE .OR. SUBSTR(STYLE.Style,lnNonMajPo)<>XCOLOR
        IF EOF() .OR. STYLE <> lcStyleCod
        *B802003,1 Change this line to use the new variable that holds [End]
        
          EXIT
        ENDIF
        STORE 0 TO XSTY, XTOTSTY
      *ENDDO
  
    ENDIF
  ENDIF  

  *-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
  *          "Point of Sale" or "Manufacturing" Modules is not installed [End]
  
  *** Get Sales From Orders Temporary File
  SELE &ORDTEMP
  GO TOP
  
  *B802003,1 Change this line to use the whole Style code in the search to
  *          prevent duplicate numbers [Begin]
  *SEEK XSTYLE
  SEEK lcStyleCod
  *B802003,1 Change this line to use the whole Style code [End]
  
  IF FOUND()
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
        ROW=5
        *** Display Style, Description, And Scale Breakdown

        @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
        
        SELE &ORDTEMP

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

      *B802003,1 Change this line to use the new variable that holds the
      *          whole Style code [BEGIN]
      *IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE .OR. SUBSTR(STYLE.Style,lnNonMajPo)<>XCOLOR
      IF EOF() .OR. STYLE <> lcStyleCod
      *B802003,1 Change this line to use the new variable that holds [End]
      
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
    DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
    ROW=5

    @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
    
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
  @ ROW,27 SAY 'STOCK  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSTK(I)  PICTURE '999999'
    XTOTSTK=XTOTSTK+XSTK(I)
    XOTS(I)=XOTS(I)+XSTK(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTSTK    PICTURE '9999999'

  ROW=ROW+1
  COL=46
  @ ROW,27 SAY 'W.I.P  .........'
  *-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
  *          "Point of Sale" or "Manufacturing" Modules is not installed [Begin]
  *  FOR I=1 TO XCNT
  *    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)  PICTURE '999999'
  *    XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)
  *    XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)
  *    COL=COL+7
  *  ENDFOR
  
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)  PICTURE '9999999'
    XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)
    XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)+XSUBSTY(I)
    COL=COL+7
  ENDFOR

  *-B602823,1  MHM  07/27/00 get wip from style file in case of the "Style Purchase Order" or the
  *          "Point of Sale" or "Manufacturing" Modules is not installed [End]

  @ ROW,103 SAY XTOTWIP       PICTURE '9999999'

  ROW=ROW+1
  COL=47
  @ ROW,27 SAY 'ORDERS .........'
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
    XTOTOTS=XTOTOTS+XOTS(I)
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
      DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
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
*! Developer : Haytham El_Sheltawi
*! Date      : 04/21/1999
*! Purpose   : Function to terminate the report if neither
*!             the "Sales Order" nor the "Point of Sale" modules
*!             is installed.
*!*************************************************************
*! Called from : Default value of the report variable (llTermnate).
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*B802167,1 This function was add by HS for the Bug# B802167,1.
*!*************************************************************
*
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
*B802399,1 - WAB - Add function for order the table when press button in range 
*B802399,1 		   at option  grad
*B802399,1 - WAB - Start
*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/25/1999
*! Purpose   : Go top in the style IN RANGE
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

*B802399,1 - WAB - END
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/05/2002
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
SELECT Customer
SET ORDER TO Customer
GO TOP
SELECT(lcAliasCst)

*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.






if .f.
        SCAN WHILE &XPOSCAN
          
          *B602805,1 Change this line to skip the Inter-location PO records
          *          (TranCd '6') [Begin]
          *IF TRANCD<>'3'                      && Not In-Transit
          IF !INLIST(TranCd , '3' , '6')
          *B602805,1 Change this line to skip the Inter-location PO [End]
          
            DO CASE
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(1)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(1)=IIF(TRANCD='1',XPOS(1)+TOTQTY, XPOS(1)-TOTQTY)
                xPOS(1) = IIF(TranCd = '1' ,;
                              xPOS(1) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(1) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(1)=IIF(XPOS(1)>0, XPOS(1), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(2)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(2)=IIF(TRANCD='1',XPOS(2)+TOTQTY, XPOS(2)-TOTQTY)
                xPOS(2) = IIF(TranCd = '1' ,;
                              xPOS(2) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(2) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(2)=IIF(XPOS(2)>0, XPOS(2), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(3)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(3)=IIF(TRANCD='1',XPOS(3)+TOTQTY, XPOS(3)-TOTQTY)
                xPOS(3) = IIF(TranCd = '1' ,;
                              xPOS(3) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(3) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(3)=IIF(XPOS(3)>0, XPOS(3), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(4)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(4)=IIF(TRANCD='1',XPOS(4)+TOTQTY, XPOS(4)-TOTQTY)
                xPOS(4) = IIF(TranCd = '1' ,;
                              xPOS(4) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(4) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(4)=IIF(XPOS(4)>0, XPOS(4), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(5)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(5)=IIF(TRANCD='1',XPOS(5)+TOTQTY, XPOS(5)-TOTQTY)
                xPOS(5) = IIF(TranCd = '1' ,;
                              xPOS(5) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(5) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(5)=IIF(XPOS(5)>0, XPOS(5), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(6)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(6)=IIF(TRANCD='1',XPOS(6)+TOTQTY, XPOS(6)-TOTQTY)
                xPOS(6) = IIF(TranCd = '1' ,;
                              xPOS(6) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(6) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(6)=IIF(XPOS(6)>0, XPOS(6), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(7)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(7)=IIF(TRANCD='1',XPOS(7)+TOTQTY, XPOS(7)-TOTQTY)
                xPOS(7) = IIF(TranCd = '1' ,;
                              xPOS(7) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(7) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(7)=IIF(XPOS(7)>0, XPOS(7), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(8)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(8)=IIF(TRANCD='1',XPOS(8)+TOTQTY, XPOS(8)-TOTQTY)
                xPOS(8) = IIF(TranCd = '1' ,;
                              xPOS(8) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(8) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(8)=IIF(XPOS(8)>0, XPOS(8), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(9)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(9)=IIF(TRANCD='1',XPOS(9)+TOTQTY, XPOS(9)-TOTQTY)
                xPOS(9) = IIF(TranCd = '1' ,;
                              xPOS(9) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(9) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(9)=IIF(XPOS(9)>0, XPOS(9), 0)
              CASE SUBSTR(STYLE,lnNonMajPo)=XCOLOR(10)
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(10)=IIF(TRANCD='1',XPOS(10)+TOTQTY, XPOS(10)-TOTQTY)
                xPOS(10) = IIF(TranCd = '1' ,;
                              xPOS(10) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(10) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(10)=IIF(XPOS(10)>0, XPOS(10), 0)
              OTHERWISE
                
                *B602805,1 Change this line to take into consideration the
                *          PO Return case [Begin]
                *XPOS(10)=IIF(TRANCD='1',XPOS(10)+TOTQTY, XPOS(10)-TOTQTY)
                xPOS(10) = IIF(TranCd = '1' ,;
                              xPOS(10) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(10) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                
                *B602805,1 Change this line to take into consideration [End]
                
                XPOS(10)=IIF(XPOS(10)>0, XPOS(10), 0)
            ENDCASE
          ENDIF
        ENDSCAN
endif