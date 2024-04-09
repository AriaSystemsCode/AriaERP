*:***************************************************************************
*: Program file  : SOMAR100   C101774
*: Program desc. : Custom Special Cut & Sold For Maran
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Mohamed_El_Anwar  (AME)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : This program Based on ICSCUTSO
*:***************************************************************************
*: Example : DO SOMAR100
*:***************************************************************************
*: Modifications
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*B804101,1 AME 04/02/2001 1) Enhance the Performance.
*B804101,1 AME            2) Fix the bug alias 'cuttkth' not found when 'MF' module is not installed.
*B804101,1 AME            3) Convert the Sales order status option in OG to be in list.
*B804101,1 AME            4) Make Inlist Option in OG for PO statuas.
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
*-- STYTEMP    variable that hold the name of STYLE temp. file
*-- XSLDATE    variable that hold from sales order comp. date
*-- XSHDATE    variable that hold to   sales order comp. date
*-- XSLSDAT    variable that hold from sales order start date
*-- XSHSDAT    variable that hold to   sales order start date
*-- XSLEDAT    variable that hold from sales order entered date
*-- XSHEDAT    variable that hold to   sales order entered date
*-- XPLDATE    variable that hold from Prod. order comp. date
*-- XPHDATE    variable that hold to   Prod. order comp. date
*-- XALLOCAT   variable that showes if include Allocated Qty. or All Qty.
*-- XTITLE     variable that hold optional title
*-- XFILTER    variable that hold report filter expression
*-- lnRepPos   variable that hold represntaives place in OG Fix. Filter.

llDontPrn = .F.
STORE 0 TO PAGENO,ROW
XTIME=TIME()
STORE SPACE(0) TO WORKTEMP,CUTTTEMP,POTEMP,ORDTEMP,StyTemp,XSTAT,R_WIDTH,R_TITLE

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))

XSLSDAT  = ldRPSLSDat
XSHSDAT  = ldRPSHSDat

lcStDtFlt = lfFormDat(ldRPSLSDat,ldRPSHSDat,'ORDHDR.START')

XSLEDAT  = ldRPSLEDat
XSHEDAT  = ldRPSHEDat

lcEnDtFlt = lfFormDat(ldRPSLEDat,ldRPSHEDat,'ORDHDR.Entered')

XSLDATE  = ldRPSLDate
XSHDATE  = ldRPSHDate

lcCmDtFlt = lfFormDat(ldRPSLDate,ldRPSHDate,'ORDHDR.Complete')

XPLDATE  = ldRPPLDate
XPHDATE  = ldRPPHDate

*B804101,1 AME [Start] the filter var were conveted. TO FIX Bug alias cuttkth not found
*lcCTDtFlt = lfFormDat(ldRPPLDate,ldRPPHDate,'POSHDR.Complete')
*lcPODtFlt = lfFormDat(ldRPPLDate,ldRPPHDate,'CUTTKTH.Complete')
lcCTDtFlt = lfFormDat(ldRPPLDate,ldRPPHDate,'CUTTKTH.Complete')
lcPODtFlt = lfFormDat(ldRPPLDate,ldRPPHDate,'POSHDR.Complete')
*B804101,1 AME [End]

*XSTATHLD = IIF(llRPHolOrd,'Y','N')
XALLOCAT = lcRPAllQty
XTITLE   = lcRPTitle
lnRepPos = lfItmPos('SALESREP.REPCODE')

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
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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

*B804101,1 AME [Start] Declare source & target order status Variables
*B804101,4 AME [Start]  Remove the Canceled status
*DECLARE laRpSource[5],laRpTarget[1],laRpPoSour[6],laRpPoTarg[1]

*STORE 'Open'     TO laRpSource[1]
*STORE 'Hold'     TO laRpSource[2]
*STORE 'Canceled' TO laRpSource[3]
*STORE 'Complete' TO laRpSource[4]
*STORE 'Bid'      TO laRpSource[5]

*lcRpStatus = 'OHXCB'

*STORE 'Open'     TO laRpPoSour[1]
*STORE 'Hold'     TO laRpPoSour[2]
*STORE 'Canceled' TO laRpPoSour[3]
*STORE 'Complete' TO laRpPoSour[4]
*STORE 'Bid'      TO laRpPoSour[5]
*STORE 'Closed'    TO laRpPoSour[6]
*lcRpPoStat = 'OHXBS'

DECLARE laRpSource[4],laRpTarget[1],laRpPoSour[5],laRpPoTarg[1]

STORE 'Open'     TO laRpSource[1]
STORE 'Hold'     TO laRpSource[2]
STORE 'Complete' TO laRpSource[3]
STORE 'Bid'      TO laRpSource[4]

lcRpStatus = 'OHCB'

STORE 'Open'     TO laRpPoSour[1]
STORE 'Hold'     TO laRpPoSour[2]
STORE 'Complete' TO laRpPoSour[3]
STORE 'Bid'      TO laRpPoSour[4]
STORE 'Closed'    TO laRpPoSour[5]
lcRpPoStat = 'OHBS'
*B804101,4 AME [End]
*B804101,1 AME [End]

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


*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
*! Purpose   : Validate sales order Complete date range
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
*! Name      : lfvSOSDat
*! Developer : A. El_anwar (AME)
*! Date      : 19/4/1998
*! Purpose   : Validate sales order Start date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSOSDat()
*!*************************************************************

FUNCTION lfvSOSDat

PRIVATE ldFrom,ldTo


IF EMPTY(ldRPSLSDat) .AND. EMPTY(ldRPSHSDat)
  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  ldFrom = ldRPSLSDat
  ldTo = ldRPSHSDat
ENDIF && end of (IF EMPTY(ldRPSLSDat) )


lcTitle = 'Sales order start date'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]


ldRPSLSDat = ldFrom
ldRPSHSDat = ldTo

*!*************************************************************
*! Name      : lfvSOEDat
*! Developer : A. El_anwar (AME)
*! Date      : 19/4/1998
*! Purpose   : Validate sales order Entered date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSOEDat()
*!*************************************************************

FUNCTION lfvSOEDat

PRIVATE ldFrom,ldTo


IF EMPTY(ldRPSLEDat) .AND. EMPTY(ldRPSHEDat)
  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  ldFrom = ldRPSLEDat
  ldTo = ldRPSHEDat
ENDIF && end of (IF EMPTY(ldRPSLEDat) )


lcTitle = 'Sales order Entered date'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]


ldRPSLEDat = ldFrom
ldRPSHEDat = ldTo

*!*************************************************************
*! Name      : lfvPODate
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
ENDIF && end of (IF EMPTY(ldRPPLDate) )   
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
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
*! Name      : lfvStyle
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
*! Developer : Ahmed El Anwar
*! Date      : 05/11/2000
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

*ahmed
*XSTATHLD = IIF(llRPHolOrd,'Y','N')
*XSTAT=IIF(XSTATHLD='N','O','HO')

*B804101,1 AME [Start] There's no var llrpholord any more
*XSTAT=IIF(!llRPHolOrd,'O','HO')
*B804101,1 AME [End]
*-- adjust date range
*IF XSLDATE<>CTOD('  /  /    ')
*  XSHDATE=IIF(XSHDATE=CTOD('  /  /    '),XSLDATE,XSHDATE)
*ENDIF

*IF XSLSDAT<>CTOD('  /  /    ')
*  XSHSDAT=IIF(XSHSDAT=CTOD('  /  /    '),XSLSDAT,XSHSDAT)
*ENDIF

*IF XSLEDAT<>CTOD('  /  /    ')
*  XSHEDAT=IIF(XSHEDAT=CTOD('  /  /    '),XSLEDAT,XSHEDAT)
*ENDIF

*IF XPLDATE<>CTOD('  /  /    ')
*  XPHDATE=IIF(XPHDATE=CTOD('  /  /    '),XPLDATE,XPHDATE)
*ENDIF

StyTemp  = gfTEMPNAME()
SELECT STYLE
SET FILTER TO
XFILTER=XFILTER+'.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'
*SET FILTER TO &XFILTER
*B804101,1 AME [Start] 
*GO TOP
*COPY REST TO &gcWORKDIR.&StyTemp FOR &XFILTER

COPY TO &gcWORKDIR.&StyTemp FOR &XFILTER
*B804101,1 AME [End] 


USE &gcWorkDir.&StyTemp IN 0 EXCLUSIVE
SELECT (StyTemp)
INDEX ON SEASON+CDIVISION+STYLE TAG &StyTemp

IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
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
  COPY STRUCTURE TO &gcWorkDir.&CUTTTEMP
  USE &gcWorkDir.&CUTTTEMP IN 0 EXCLUSIVE
  

  SELECT (CUTTTEMP)

  *B804101,4 AME [Start]
  *INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(CUTTKTH.COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
  INDEX ON STYLE+DTOS(CUTTKTH.COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
  *B804101,4 AME [End]
  *SET RELATION TO CUTTKT INTO CUTTKTH
  
  *-- AAMER(Start)
  SET ORDER TO TktOper IN MFGOPRHD  && cimtyp+ctktno+coperseq
  SELECT CUTTKTH
  SET RELATION TO 'M' + CutTkt INTO MFGOPRHD
  *-- AAMER(End)
  
ENDIF    && End of IF 'MF' $ gcComp_Mdl

*-- If the "Style Purchase Order" or the "Point of Sale" Modules
*-- is installed
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
  SELECT POSLN
  SET RELATION TO cStyType + PO INTO POSHDR
  *B804101,4 AME [Start]  relation from posln instead of temp po file
  SET RELATION TO  SHIPNO INTO SHPMTHDR ADDITIVE
  *B804101,4 AME [End] 
  =AFIELDS(laFileStru)
  *B804101,4 AME [Start] Add complete to potemp
  *DIMENSION laFileStru[ALEN(laFileStru,1)+1,4]
  *laFileStru[ALEN(laFileStru,1),1] = 'ETA'
  *laFileStru[ALEN(laFileStru,1),2] = 'D'
  *laFileStru[ALEN(laFileStru,1),3] = 8
  *laFileStru[ALEN(laFileStru,1),4] = 0
  DIMENSION laFileStru[ALEN(laFileStru,1)+2,4]
  laFileStru[ALEN(laFileStru,1)-1,1] = 'ETA'
  laFileStru[ALEN(laFileStru,1)-1,2] = 'D'
  laFileStru[ALEN(laFileStru,1)-1,3] = 8
  laFileStru[ALEN(laFileStru,1)-1,4] = 0
  laFileStru[ALEN(laFileStru,1),1] = 'Complete'
  laFileStru[ALEN(laFileStru,1),2] = 'D'
  laFileStru[ALEN(laFileStru,1),3] = 8
  laFileStru[ALEN(laFileStru,1),4] = 0
  *B804101,4 AME [End]
  CREATE TABLE (GcWorkDir+POTemp) FROM ARRAY laFileStru
  *SET RELATION TO cStyType + PO INTO POSHDR

  *B804101,4 AME [Start] Index on complete inside the same table.
  *SET RELATION TO  SHIPNO INTO SHPMTHDR ADDITIVE
  
  *INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(POSHDR.COMPLETE)+PO+DTOS(ETA)+TRANCD+STR(RECNO(),7) TAG &POTEMP

  
  *INDEX ON STYLE+DTOS(POSHDR.COMPLETE)+PO+DTOS(ETA)+TRANCD+STR(RECNO(),7) TAG &POTEMP 
  INDEX ON STYLE+DTOS(COMPLETE)+PO+DTOS(ETA)+TRANCD+STR(RECNO(),7) TAG &POTEMP 
  *B804101,4 AME [End]
    
ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl


SELECT ORDLINE
SET RELATION TO cOrdType + Order INTO ORDHDR

COPY STRUCTURE TO &gcWorkDir.&ORDTEMP
USE &gcWorkDir.&ORDTEMP IN 0 EXCLUSIVE
SELECT (ORDTEMP)
*B804101,4 AME [Start]
*INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(OrdHdr.Cancelled)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
INDEX ON STYLE+DTOS(OrdHdr.Cancelled)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
*B804101,4 AME [End]
*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to OrdLine according 
  *-- to the CutOrd index.
  =gfOpenFile(gcDataDir+'CutPick','CutOrd','SH')
  
  *-- Openning the file CutPick with a relation to OrdLine according 
  *-- to the CutOrd index in another work area to check if the order 
  *-- was allocated by cuttkt not by PO.
  SELECT 0 
  USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick3
  SET ORDER TO CutOrd

  *-- MAKE THE RELATIONS  
  SELECT OrdLine 
  SET RELATION TO '2'+Order+STR(LineNo,6) INTO CutPick ADDITIVE
  SET RELATION TO '1'+Order+STR(LineNo,6) INTO CutPick3 ADDITIVE
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


SELECT &STYTEMP

XDESC=SPACE(20)
STORE SPACE(19) TO XSTYLE,XCOLOR

*** SAVE OFF RECORDS INTO THEIR RESPECTIVE TEMPORARY FILE
lcCTFlt =  lcCTDtFlt + ".AND. CUTTKTH.STATUS<>'X' .AND. CUTTKTH.STATUS<>'C' .AND. TOTQTY<>0 "
XADD = " "
DO CASE
  CASE XALLOCAT = 'L'
    XADD = ' .AND.  CutPick2.CtktNo = PosLn.Po'
  CASE XALLOCAT = 'N'
    XADD = ' .AND.  CutPick2.CtktNo <> PosLn.Po'
ENDCASE
*B804101,1 AME [start] Add status filter to po filter expr.
*lcPoFlt = lcPoDtFlt + ".AND. POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. ;
          TOTQTY<>0" + XADD
lcPoFlt = lcPoDtFlt + ".AND. POSHDR.STATUS $ lcRpPoStat .AND. ;
          TOTQTY<>0" + XADD
*B804101,1 AME [End]          
lcSlctFile = laOGFxFlt[lnRepPos,6]
llLineDeal = !EMPTY(lcSlctFile) AND USED(lcSlctFile) AND RECCOUNT(lcSlctFile) > 0

XADDORD = ' '
DO CASE
  CASE XALLOCAT = 'L'
    XADDORD = ' .AND.  (OrdLine.Order = CutPick.Order .OR. ' + ;
              ' OrdLine.Order = CutPick3.Order) '
  CASE XALLOCAT = 'N'
    XADDORD = ' .AND.  (OrdLine.Order <>CutPick.Order .AND.' + ;
              ' OrdLine.Order <> CutPick3.Order) '
ENDCASE
*B804101,1 AME [Start] Add the order status expression to filter expression
*lcOrFlt = lcStDtFlt + '.AND.' + lcCmDtFlt+ '.AND.' + lcEnDtFlt + '.AND.' + [IIF(lcRPBulk<>"B",IIF(lcRPBulk = 'Y' ,ORDHDR.Bulk = 'Y',ORDHDR.Bulk <> 'Y') ,.T.) .AND.;
                  IIF(lcRpEdiPrn <> "B", IIF(lcRpEdiPrn="O",!OrdHdr.lEdiOrder,OrdHdr.lEdiOrder),.T.) .AND. ;
                  cOrdType = 'O'] + XADDORD
lcOrFlt = lcStDtFlt + '.AND.' + lcCmDtFlt+ '.AND.' + lcEnDtFlt + '.AND.' + [IIF(lcRPBulk<>"B",IIF(lcRPBulk = 'Y' ,ORDHDR.Bulk = 'Y',ORDHDR.Bulk <> 'Y') ,.T.) .AND.;
                  IIF(lcRpEdiPrn <> "B", IIF(lcRpEdiPrn="O",!OrdHdr.lEdiOrder,OrdHdr.lEdiOrder),.T.) .AND. ;
                  cOrdType = 'O'] + XADDORD + ' .AND. '+ "ORDHDR.STATUS $ lcRpStatus"
*B804101,1 AME [End]                 
IF llLineDeal
  lcOrFlt = lcOrFlt + ".AND. IIF(EOF(lcSlctFile),.F.,.T.)"           
  SELECT ORDHDR
  SET RELATION TO ORDHDR.REP1 INTO &lcSlctFile ADDITIVE
  SELECT &STYTEMP
ENDIF
SCAN
  IF INKEY()=32
    RETURN
  ENDIF

  xStyle = Style
  
  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl
    *** Store off Records Into First Temp File CUTTTEMP
    IF SEEK(XSTYLE,"CUTTKTL")
      SELECT CUTTKTL
      *ahmed
      *IF XPLDATE<>CTOD('  /  /    ')
      *  COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) FOR (BETWEEN(CUTTKTH.COMPLETE,XPLDATE,XPHDATE) .AND. CUTTKTH.STATUS<>'X' .AND. CUTTKTH.STATUS<>'C' .AND. TOTQTY<>0)
      *ELSE
      *  COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) FOR (CUTTKTH.STATUS<>'X' .AND. CUTTKTH.STATUS<>'C' .AND. TOTQTY<>0)
      *ENDIF
      *B804101,1 AME [Start] Use full index to rushmor, WITH SCAN INSTEAD OF COPY THEN APPEND
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
                WHILE STYLE = XSTYLE   ;
                FOR &lcCTFlt
      *SELECT &CUTTTEMP
      *APPEND FROM &gcWorkDir.&WORKTEMP
      SCAN REST WHILE STYLE+CUTTKT+TRANCD = XSTYLE FOR &lcCTFlt
        SCATTER MEMVAR MEMO
        INSERT INTO &CUTTTEMP FROM MEMVAR
      ENDSCAN
      *B804101,1 AME [End]
    ENDIF
  ENDIF    && End of IF 'MF' $ gcComp_Mdl
  
  *-- If the " Style Purchase Order " or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    
    *** Store off Records Into Second Temp File POTEMP
        
    IF SEEK (XSTYLE,"POSLN")
      SELECT POSLN
      *ame
      *IF XPLDATE<>CTOD('  /  /    ')
        *ame
        *COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
        *     FOR (BETWEEN(POSHDR.COMPLETE,XPLDATE,XPHDATE) .AND. ;
        *     POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. ;
        *     TOTQTY<>0 &XADD)
       
 
      *ELSE
      * COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
      * FOR (POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. ;
      * TOTQTY<>0 &XADD)
      *ENDIF
      *B804101,1 AME [Start] Use full index to rushmor , and scan instead of copy
      *COPY REST TO &gcWorkDir.&WORKTEMP ;
                 WHILE STYLE=XSTYLE ;
                 FOR  &lcPoFlt
      SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = XSTYLE FOR &lcPoFlt
        SCATTER MEMVAR MEMO

        *B804101,4 AME [Start] Replace inside loop
        M.ETA = SHPMTHDR.ETA
        M.Complete = POSHDR.Complete
        *B804101,4 AME [End]

        INSERT INTO &POTEMP FROM MEMVAR
      ENDSCAN
      *B804101,1 AME [End]            
      SELECT &POTEMP
      *B804101,1 AME [Start]  insert instead of append.
      *APPEND FROM &gcWorkDir.&WORKTEMP
      *B804101,1 AME [End]      
      
      *B804101,4 AME [Start] Replace inside loop
      *REPLACE ALL ETA WITH SHPMTHDR.ETA
      *B804101,4 AME [End]
   
    ENDIF
    
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *** Store off Records Into Third Temp File ORDTEMP
  
  IF SEEK (XSTYLE,"ORDLINE")
    SELECT ORDLINE
    *B804101,1 AME [Start] Use full index to rushmor , WITH SCAN INSTEAD OF COPY THEN APPEND   
    *COPY REST TO &gcWorkDir.&WORKTEMP ;
         WHILE Style = xStyle ;
         FOR &lcOrFlt
    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = XSTYLE FOR &lcOrFlt
      SCATTER MEMVAR MEMO
      INSERT INTO &ORDTEMP FROM MEMVAR
    ENDSCAN
    *SELECT &ORDTEMP
    *APPEND FROM &gcWorkDir.&WORKTEMP
    *B804101,1 AME [End]
  ENDIF
  
  *B804101,1 AME[Start] Meaninigless statment
  *SELECT &STYTEMP
  *B804101,1 AME[End]
ENDSCAN

IF llLineDeal  
  SELECT ORDHDR
  SET RELATION OFF INTO &lcSlctFile
  SELECT &STYTEMP
ENDIF

IF USED('CUTPICK')
  SELECT ORDLINE
  SET RELATION OFF INTO  CUTPICK
ENDIF    && End of IF USED('CUTPICK')

IF USED('CUTPICK2')
  USE IN CUTPICK2
ENDIF    && End of IF USED('CUTPICK2')

IF USED('CUTPICK3')
  SELECT ORDLINE
  SET RELATION OFF INTO  CUTPICK3
ENDIF    && End of IF USED('CUTPICK3')

SELECT ORDLINE
SET RELATION OFF INTO ORDHDR
SELECT (ORDTEMP)
SET RELATION TO cOrdType + Order INTO ORDHDR ADDITIVE

IF USED(POTEMP)
  SELECT POSLN
  SET RELATION OFF INTO POSHDR
  SELECT (POTEMP)
  SET RELATION TO cStyType + PO INTO POSHDR ADDITIVE
ENDIF    && End of IF USED(POTEMP)

IF USED(CUTTTEMP)
  SELECT CUTTKTL
  SET RELATION OFF INTO CUTTKTH
  SELECT (CUTTTEMP)
  SET RELATION TO CUTTKT INTO CUTTKTH   
ENDIF    && End of IF USED(CUTTTEMP)

PAGENO=0
ROW=99
XTIME=TIME()
R_WIDTH='W'
DO CASE
  CASE XALLOCAT = 'L'
    R_TITLE = 'CUSTOM ORDER DETAIL - ALLOCATED  REPORT'
  CASE XALLOCAT = 'N'
    R_TITLE = 'CUSTOM ORDER DETAIL - NOT ALLOCATED REPORT'
  CASE XALLOCAT = 'A'
    R_TITLE = 'CUSTOM ORDER DETAIL - REPORT'
ENDCASE


*!*************************************************************
*! Name      : lpPrint
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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

*** Declare & Initialize Variables & Array
STORE SPACE(6)  TO XCUTTKT, XPO, XORDER , XPKTKT,XCOLOR
STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
STORE SPACE(25) TO XName
STORE SPACE(8) TO XStore
STORE SPACE(15) TO XCusT
STORE SPACE(1) TO XBulk, XSTTUS
STORE SPACE(3) TO XPriO


STORE CTOD('  /  /    ') TO XDATE , XDATE1 , XSTART
STORE 0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD , XGRDEXT , XPRICE , XPKD
DIMENSION XCOLOR(10),XCLR(10),XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),;
          XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10)

*** Determine Index, Seek, and Scan To Use (WIP)

*B804101,1 AME [Start] scan for the complete style instead of cstymaj.
*XCUTSCAN='(CUTTKTH.COMPLETE=XDATE .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
*XPOSCAN='(POSHDR.COMPLETE=XDATE .AND. PO=XPO ) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
XCUTSCAN='(CUTTKTH.COMPLETE=XDATE .AND. CUTTKT=XCUTTKT) FOR STYLE= &StyTemp..Style'
XPOSCAN='(POSHDR.COMPLETE=XDATE .AND. PO=XPO ) FOR STYLE= &StyTemp..Style'
*B804101,1 AME [End]

XCUTHDR='COMPLETE CUTTKT CONTR1'
XPOHDR='PO #   COMPLETE  AVAILABLE   QTY  SHIPMENT#  ETA         QTY'

*** Determine  Index, Seek, and Scan To Use (ORDERS)

*B804101,1 AME [Start] scan for the complete style instead of cstymaj.
*XORDSCAN='(ORDHDR.Cancelled = XDATE1 .AND. Order = XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
XORDSCAN='(ORDHDR.Cancelled = XDATE1 .AND. Order = XORDER) FOR STYLE= &StyTemp..Style'
*B804101,1 AME [End]
XORDHDR='Order# Acct# Acct Name                 Store#   Customer Po#    St. Date Complete S P   B Tot pcs   Price Ext.Price  P/T #     Diff'
*-- if the user choose to print allocated qty
IF XALLOCAT <> 'N'
  *=gfOpenFile(gcDataDir+'CutPick','CutOrd','SH')
  SELECT &ORDTEMP
  SET RELATION TO "2"+Order+STR(LineNo,6) INTO CutPick ADDITIVE
  *SELECT 0 
  *USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick3
  *SET ORDER TO CutOrd
  *SELECT &OrdTemp 
  SET RELATION TO '1'+Order+STR(LineNo,6) INTO CutPick3 ADDITIVE
ENDIF
=gfOpenFile(gcDataDir+'InvLine','InvLineO','SH')
SELECT &OrdTemp 
SET RELATION TO Order+STR(LineNo,6) INTO InvLine ADDITIVE

*B804101,4 AME [Start]  Relation here instead of inside loops
SET RELATION TO IIF(EMPTY(store),"M"+account,"S"+account) INTO CUSTOMER ADDITIVE
*B804101,4 AME [End]

SELECT &StyTemp
*B804101,1 AME [Start]
*GO TOP
LOCATE
*B804101,1 AME [END]
*-- Loop on seasons
DO WHILE INKEY() <> 32
  XSEASON=SEASON
  SELECT &StyTemp
  
  STORE 0 TO  XSEASTK, XSEAWIP, XSEAORD , XSEAEXT
  *-- Loop on division
  DO WHILE INKEY() <>32
    XDIVISION=CDIVISION
    SELECT &StyTemp
    
    STORE 0 TO XDIVSTK, XDIVWIP, XDIVORD , XDIVEXT
    *-- loop on style
    DO WHILE INKEY() <> 32
      *B804101,4 AME [Start]
      *XSTYLE=SUBSTR(STYLE,1,lnMajLen)
  	  XSTYLE = Style
  	  *B804101,4 AME [End]
  	  XDESC=DESC

	  STORE 0 TO XTOTSTK, XTOTWIP,  XTOTOTS, XTOTEXT
	  STORE SPACE(5) TO XCLR
	  STORE SPACE(6) TO XCOLOR         &&TAK06/21/94
	  STORE 0 TO XSTK, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD,;
    	         XSUBORD, XTOTORD, XOTS

  	  *** To Get Number of Color For Each Style
	  
	  *B804101,1 AME [Start]  document these for loops and use scan instead
	  *COUNT ALL FOR (SUBSTR(STYLE,1,lnMajLen)=XSTYLE) TO XCLRCNT

	  
	  *SEEK XSEASON+XDIVISION+XSTYLE                 && To Place Pointer Back to 1st Match

	  *** Load Array With Color and Stock

	  *XCNT = XCLRCNT
	  *IF XCNT > 10
	  *  DIMENSION XCOLOR(XCNT),XCLR(XCNT),XSTK(XCNT),XCUT(XCNT),XSUBCUT(XCNT),XPOS(XCNT),;
                  XSUBPOS(XCNT),XORD(XCNT),XSUBORD(XCNT),XOTS(XCNT)
	  *ENDIF

	  *FOR I=1 TO XCNT
	  *  XCLR(I)   = SUBSTR(&StyTemp..Style,lnNonMajPo)
	  *  XCOLOR(I) = SUBSTR(&StyTemp..Style,lnNonMajPo)
	  *  XSTK(I)   = TOTSTK
	  *  SKIP
	  *ENDFOR
       
	  *FOR I=1 TO XCNT
	  I = 0
	  SCAN WHILE Season+CDivision+Style = XSEASON+XDIVISION+XSTYLE
	    I = I + 1
	    IF I >  10 
	      DIMENSION XCLR(I),XSTK(I),XCUT(I),XSUBCUT(I),XPOS(I),;
                    XSUBPOS(I),XORD(I),XSUBORD(I),XOTS(I)
    
	      STORE 0 TO XCUT(I),XSUBCUT(I),XPOS(I),XSUBPOS(I),XORD(I),XSUBORD(I),XOTS(I)
        ENDIF
	    XCLR(I)   = SUBSTR(&StyTemp..Style,lnNonMajPo)
	    XSTK(I)   = TOTSTK
	    *B804101,1 AME [End] 
	  
	  
	    STORE 0 TO XPICK , XSHPPED , XTRAN , XALC
    	IF ROW > 54
	      ROW=0
	      PAGENO=PAGENO+1
	      DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
	      ROW=5
	    ENDIF

	    *** Display Style, Description, And Range Of Colors
	    @ ROW,01 SAY XSTYLE
	    *ahmed
	    IF LEN(TRIM(XCLR(I))) < 4
	      XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=1,'    '+TRIM(XCLR(I)),XCLR(I))
	      XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=2,'   '+TRIM(XCLR(I)),XCLR(I))
	      XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=3,'  '+TRIM(XCLR(I)),XCLR(I))
	    ENDIF
	    *XCLR[I] = PADL(xClr[I],5)
	    @ ROW,12 SAY "/"
	    @ ROW,13 SAY XCLR(I)  
	    IF ROW=5
	      @ ROW,125 SAY 'TOTAL'
	    ENDIF
	    ROW=ROW+1
	    @ ROW,01 SAY REPLICATE('-',131)
	  
	    
	    *** Get Sales From Orders Temporary File
	    SELECT &ORDTEMP
	    
	    *B804101,4 AME [Start] remove relation outside the loops
	    *SET RELATION TO IIF(EMPTY(store),"M"+account,"S"+account) INTO CUSTOMER ADDITIVE
	    *B804101,4 AME [End]
	    
	    *B804101,1 AME [start]
	    *GO TOP
	    *B804101,1 AME [End]
	    *SEEK XSTYLE
	    *IF FOUND()
	    IF SEEK(XSTYLE,ORDTEMP)
	      XFIRSTTIME=.T.
          XCNCTOT = 0 
	      DO WHILE INKEY()<>32
	        IF ROW > 54 .AND. !EOF()
	          ROW=0
	          PAGENO=PAGENO+1
              DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
	          
	          ROW=5
	          *** Display Style, Description, And Range Of Colors
	          @ ROW,01 SAY XSTYLE
	          @ ROW,12 SAY "/"
	          @ ROW,13 SAY XCLR(I)
	          @ ROW,125 SAY 'TOTAL'
	          ROW=ROW+1
	          @ ROW,01 SAY REPLICATE('-',131)
	          XFIRSTTIME=.T.
	        ENDIF
  
	        
	        XORDER=ORDHDR.ORDER
	        XACCOUNT=ACCOUNT
	        XDATE=COMPLETE
	        XDATE1= OrdHdr.Cancelled
	        XName = CUSTOMER.Btname
	        XStore = IIF(OrdHdr.Multi = 'Y',"Multi " ,Store )
	        XCUST = Custpo
	        XSTART = Start
	        XSTTUS = Ordhdr.Status
	        XPrio = Ordhdr.Priority
	        XBulk = IIF(Ordhdr.Bulk<>"Y",'N','Y')
	        XPrice = Price
            XPKTKT = IIF(OrdHdr.Multi = 'Y',"Multi " ,piktkt)
            *XPKD  = TOTPIK
            XPKD  = 0
            
            *B804101,4 AME [Start] Full index expr for optmiz.            
            *SCAN WHILE &XORDSCAN
            SCAN WHILE STYLE+DTOS(OrdHdr.Cancelled)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) = XStyle+DTOS(XDATE1)+XORDER
	        *B804101,4 AME [End]
            
	          *B804101,1 AME [Start] Now We scan on temp file every time we
	          *B804101,1 AME         reach these lines we are in the proper position. 
	          *IF SUBSTR(STYLE,lnNonMajPo)=XCOLOR(I)
              *B804101,1 AME [End]
              XORD(I)=XORD(I)+TOTQTY
              XPKD = XPKD +TOTPIK
              XCNCTOT = XCNCTOT + TOTQTY             
              XPICK = XPICK + TOTPIK
              XSHPPED = XSHPPED + IIF(ORDER = Invline.Order , Invline.TotQty ,0 ) 
              *B804101,4 AME [Start] don't add allocated for received orders
              *IF XALLOCAT <> 'N' 
              IF XALLOCAT <> 'N' .AND. TOTQTY > 0
              *B804101,4 AME [End]
                XALC = XALC + IIF(CUTPICK.ORDER = ORDER , CUTPICK.TOTQTY , ;
                              IIF(CUTPICK3.ORDER = ORDER , CUTPICK3.TOTQTY, 0)) 
              ENDIF
              *B804101,1 AME [Start] remove the if condition.
              *ENDIF 
              *B804101,1 AME [End]             
            ENDSCAN
            
            XTOTEXT = XTOTEXT+XORD(I)*XPRICE   
	        *XTOTORD=XTOTORD+XORD(I)
            XSUBORD(I)=XSUBORD(I)+XORD(I)
            
            *IF XORDRPT .AND. XTOTORD>0
            *IF  XTOTORD>0
            IF  XORD(I)>0
            *** Display ORD Heading
              IF XFIRSTTIME
                ROW=ROW+1
	            @ ROW,01 SAY XORDHDR
	            ROW=ROW+1
                @ ROW,01 SAY REPLICATE('-',131)
                XFIRSTTIME=.F.
              ENDIF
              ROW=ROW+1
              @ ROW,01 SAY XORDER
              @ ROW,08 SAY XACCOUNT
              @ ROW,14 SAY XName PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXX'
              @ ROW,40 SAY XStore
              @ ROW,49 SAY XCust
              @ ROW,65 SAY XStart
              @ ROW,74 SAY XDATE
              @ ROW,83 SAY XSTTUS
              @ ROW,85 SAY XPrio
              @ ROW,89 SAY XBulk
              @ ROW,91 SAY XORD(I) PICTURE '9999999' 
              @ ROW,99 SAY XPrice  PICTURE '9999.99' 
              @ ROW,107 SAY XORD(I)*XPrice  PICTURE '999999.99' 
              @ ROW,117 SAY XPktkt
              @ ROW,125 SAY XORD(I)-XPKD PICTURE '9999999'
  
              IF   OrdHdr.Cancelled <>XDATE1
                ROW = ROW + 1
              	@ ROW,19 SAY '----------------------------------------------------------------------------------------'
            	ROW=ROW+1
				@ ROW,19 SAY 'SUB TOTAL OF STYLE:'+XSTYLE+'/'+XCLR(I)+'  CANCEL DATE:'
				@ ROW,72 SAY XDATE1  
			    @ ROW,81 SAY 'TOTAL ORDERED QTY:'
			    @ ROW,88 SAY XCNCTOT  PICTURE '99999999'
			    ROW=ROW+1
				@ ROW,19 SAY '----------------------------------------------------------------------------------------'
                XCNCTOT = 0
              ENDIF

              *** Display ORD Total Line Item
                            
            ENDIF
            *B804101,1 AME [Start]
            *IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
            IF EOF() .OR. STYLE<>XSTYLE
            *B804101,1 AME [End]

              *STORE 0 TO XTOTORD
              EXIT
            ENDIF
            STORE 0 TO XORD
            *STORE 0 TO XORD, XTOTORD
          ENDDO
          
          IF XSUBORD(I) >0
            *B804101,1 AME [Start]
            *XSUBTOT=0
            *B804101,1 AME [End]
            ROW=ROW+1
            @ ROW,21 SAY REPLICATE('-',77)
            ROW=ROW+1
            @ ROW,06 SAY 'Orders Subtotals ......'
            @ ROW,90 SAY XSUBORD(I)  PICTURE '99999999'
            *B804101,1 AME [Start]
            *XSUBTOT=XSUBTOT+XSUBORD(I)
            *B804101,1 AME [End]
            ROW=ROW+1
          ENDIF
        ENDIF
        *B804101,4 AME [Start] no need for this line
        *SET RELATION OFF INTO CUSTOMER 
        *B804101,4 AME [End]
    
        *B802167,1 Add this line to check if the "Manufacturing" module is installed
        *          before using the "Cut Ticket" files [Begin]
    
        *-- If the "Manufacturing" module is installed
        IF 'MF' $ gcComp_Mdl
        *B802167,1 Add this line to check if the "Manufacturing" module [End]
      
        *** Get WIP From Cutting Ticket Temporary File

          *B804101,1 AME [Start]
          *SELECT &CUTTTEMP
          *GO TOP
          *SEEK XSTYLE
          *IF FOUND()
          IF SEEK(XSTYLE,CUTTTEMP)
            SELECT &CUTTTEMP          
          *B804101,1 AME [End]
            XFIRSTTIME=.T.
            DO WHILE INKEY()<>32
              IF ROW > 54 .AND. !EOF()
                ROW=0
                PAGENO=PAGENO+1

                DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
                
                ROW=5
                *** Display Style, Description, And Range Of Colors
                @ ROW,01 SAY XSTYLE
                @ ROW,12 SAY "/"
                @ ROW,13 SAY XCLR(I)
                @ ROW,125 SAY 'TOTAL'
                ROW=ROW+1
                @ ROW,01 SAY REPLICATE('-',129)
                XFIRSTTIME=.T.
              ENDIF
   
              XCUTTKT=CUTTKT
              XDATE=CUTTKTH.COMPLETE
        
              *--AAMER (Start)
              *XCONTR1=CUTTKTH.CONTR1
              XCONTR1 = MFGOPRHD.cContCode
              *--AAMER (End)

              *B804101,4 AME [Start] Full index expr for optmiz.
              *SCAN WHILE &XCUTSCAN
              SCAN WHILE STYLE+DTOS(CUTTKTH.COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) = XSTYLE+DTOS(XDATE)+XCUTTKT
              *B804101,4 AME [End] 

                *B804101,1 AME [Start]
                *IF SUBSTR(STYLE,lnNonMajPo)=XCOLOR(I)
                *B804101,1 AME [End]
                XCUT(I)=IIF(TRANCD='1',XCUT(I)+TOTQTY, XCUT(I)-TOTQTY)
                XCUT(I)=IIF(XCUT(I)>0, XCUT(I), 0)
                *B804101,1 AME [Start]
                *ENDIF
                *B804101,1 AME [End]
              ENDSCAN
   
              XTOTCUT=XTOTCUT+XCUT(I)
              XSUBCUT(I)=XSUBCUT(I)+XCUT(I)
                    
              IF XTOTCUT>0 
                *** Display WIP Heading
                IF XFIRSTTIME
                  ROW=ROW+1
                  @ ROW,01 SAY XCUTHDR
                  ROW=ROW+1
                  @ ROW,01 SAY REPLICATE('-',22)
                  XFIRSTTIME=.F.
                ENDIF
                ROW=ROW+1
          
               
                @ ROW,01 SAY XDATE
                @ ROW,10 SAY XCUTTKT
                @ ROW,17 SAY XCONTR1
            
 
            
                *** Display WIP Total Line Item
                @ ROW,32 SAY XCUT(I)  PICTURE '9999999'
              ENDIF
        	  *B804101,1 AME [Start]
              *IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
   	          IF EOF() .OR. STYLE<>XSTYLE
        	  *B804101,1 AME [End]

                EXIT
              ENDIF
              STORE 0 TO XCUT, XTOTCUT
            ENDDO
            IF XSUBCUT(I) >0 
              *B804101,1 AME [Start]
              *XSUBTOT=0
              *B804101,1 AME [End]
              ROW=ROW+1
              @ ROW,31 SAY REPLICATE('-',8)
              ROW=ROW+1
              @ ROW,06 SAY 'Cuttkt Subtotals ......'
              @ ROW,31 SAY XSUBCUT(I)  PICTURE '99999999'
              *B804101,1 AME [Start]
              *XSUBTOT=XSUBTOT+XSUBCUT(I)
              *B804101,1 AME [End]
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

	      *B804101,1 AME [Start]
    	  *SELECT &POTEMP
    	  *GO TOP
	      *SEEK XSTYLE
    	  *IF FOUND()
	      IF SEEK(XSTYLE,POTEMP)
	        SELECT &POTEMP
	      *B804101,1 AME [End]
	        XFIRSTTIME=.T.
	        DO WHILE INKEY()<>32
	          STORE 0 TO XCOUNT
	          IF ROW > 54 .AND. !EOF()
	            ROW=0
    	        PAGENO=PAGENO+1
        	    DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
        	    
            	ROW=5
	            *** Display Style, Description, And Range Of Colors
    	        @ ROW,01 SAY XSTYLE
            	@ ROW,12 SAY "/"
	            @ ROW,13 SAY XCLR(I)
        	    @ ROW,125 SAY 'TOTAL'
            	ROW=ROW+1
	            @ ROW,01 SAY REPLICATE('-',129)
    	        XFIRSTTIME=.T.
        	  ENDIF
  
	          XPO=PO
              
              *B804101,4 AME [Start]
        	  *XDATE=POSHDR.COMPLETE
              XDATE= COMPLETE
              *B804101,4 AME [End] 
              
              XDATE1=POSHDR.AVAILABLE 

              *B804101,4 AME [Start] Full index expr for optmiz.
	          *SCAN WHILE &XPOSCAN
	
	          SCAN WHILE STYLE+DTOS(COMPLETE)+PO+DTOS(ETA)+TRANCD+STR(RECNO(),7) = XSTYLE+DTOS(XDATE)+XPO
              *B804101,4 AME [End]
                
                *BB804101,4 [START] SKIP INTER LOCATION PO
                IF CSTYTYPE = "N"
                  LOOP
                ENDIF
                *BB804101,4 [End]
                                             
	            *B602805,1 Change this line to skip the Inter-location PO records
    	        *          (TranCd '6') [Begin]
        	    *IF TRANCD<>'3'                      && Not In-Transit
            	IF !INLIST(TranCd , '3' , '6')
	            *B602805,1 Change this line to skip the Inter-location PO [End]
        	      *B804101,1 AME [start]
        	      *IF  SUBSTR(STYLE,lnNonMajPo)=XCOLOR(I)           
            	  *B804101,1 AME [End]
            	  xPOS(I) = IIF(TranCd = '1' ,;
                	         xPOS(I) +;
                    	     (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                        	 xPOS(I) -;
                             (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
    	            
    	          
    	          *B804101,1 AME [Start] 
    	          *ENDIF
                  *B804101,1 AME [End]
        	    ELSE
     	          *B804101,1 AME [Start]
     	          *IF  SUBSTR(STYLE,lnNonMajPo)=XCOLOR(I)           
    	          *B804101,1 AME [End]
    	          IF TRANCD = '3'
    	            IF !EMPTY(ETA)
    	              XCOUNT = XCOUNT+1
    	              DIMENSION XETA(XCOUNT),XPOLIN(XCOUNT),XSHIP(XCOUNT)
    	              XETA(XCOUNT) = ETA
    	              XSHIP(XCOUNT) = SHIPNO
    	              XPOLIN(XCOUNT) = TotQty
    	              XTRAN = XTRAN + TotQty
    	            ENDIF
    	          ENDIF  
    	          *B804101,1 AME [Start]
    	          *ENDIF  
	              *B804101,1 AME [End]
	            ENDIF
    	      ENDSCAN
  
	          XTOTPOS=XTOTPOS+XPOS(I)
    	      XSUBPOS(I)=XSUBPOS(I)+XPOS(I)
        	  IF XTOTPOS>0
    	        *** Display WIP Heading
        	    IF XFIRSTTIME
            	  ROW=ROW+1
	              @ ROW,01 SAY XPOHDR
    	          ROW=ROW+1
        	      @ ROW,01 SAY REPLICATE('-',60)
            	  XFIRSTTIME=.F.
	            ENDIF
    	        ROW=ROW+1
          
	            
            	@ ROW,01 SAY XPO
    	        @ ROW,08 SAY XDATE
    	        @ ROW,18 SAY XDATE1
    	        @ ROW,25 SAY XPOS(I)  PICTURE '9999999'
    	        *B804101,4 AME [Start] IF THERE'S SHIPMENT ALWAYS PRINT IT.
    	        *IF XCOUNT = 0

	            *ELSE
                  *FOR J = 1 TO XCOUNT
                    *@ ROW,35 SAY XSHIP(J)
       	            *@ ROW,46 SAY XETA(J)
    	            *@ ROW,52 SAY XPOLIN(J) PICTURE '9999999' 	          
                    *ROW = ROW +1
                  *ENDFOR
	            *ENDIF 
    	        *B804101,4 AME [End]
    	      ENDIF
              
              *B804101,4 AME [Start] IF THERE'S SHIPMENT ALWAYS PRINT IT.
    	     
    	      IF XCOUNT > 0
                IF XFIRSTTIME
                  ROW=ROW+1
                  @ ROW,01 SAY XPOHDR
                  ROW=ROW+1
        	      @ ROW,01 SAY REPLICATE('-',60)
            	  XFIRSTTIME=.F.
	            ENDIF
    	        ROW=ROW+1

                FOR J = 1 TO XCOUNT
                  @ ROW,35 SAY XSHIP(J)
       	          @ ROW,46 SAY XETA(J)
    	          @ ROW,52 SAY XPOLIN(J) PICTURE '9999999' 	          
                  ROW = ROW +1
                ENDFOR
	          ENDIF 
    	      *B804101,4 AME [End]

              
              *B804101,1 AME [Start] 
	          *IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
	          IF EOF() .OR. STYLE<>XSTYLE
        	  *B804101,1 AME [End]
        	    STORE 0 TO XTOTPOS
        	    EXIT
    	      ENDIF
        	  STORE 0 TO XPOS, XTOTPOS
	        ENDDO
    	    IF XSUBPOS(I) >0
      	      *B804101,1 AME [Start]
      	      *XSUBTOT=0
	          *B804101,1 AME [End]
	          ROW=ROW+1
    	      @ ROW,21 SAY REPLICATE('-',40)
        	  ROW=ROW+1
	          *B804101,4 AME [Start] 
	          *@ ROW,02 SAY 'P.O. Subtotals ....'
	          @ ROW,02 SAY 'Net P.O. Subtotals ....'
	          *B804101,4 AME [End]
	          @ ROW,25 SAY XSUBPOS(I)  PICTURE '99999999'
    	      *B804101,1 AME [Start]
    	      *XSUBTOT=XSUBTOT+XSUBPOS(I)
    	      *B804101,1 AME [End]
    	      ROW=ROW+1
    	    ENDIF  
	      ENDIF
    
    	*B802167,1 Add this line to check if the "Style Purchase Order" or the
	    *          "Point of Sale" Modules is installed before using the
	    *          "Purchase Order" files [Begin]
	    ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    	*B802167,1 Add this line to check if the "Style Purchase Order"... [End]

   
        *** Display Summary Information
 
	    IF ROW > 49
	      ROW=0
	      PAGENO=PAGENO+1

	      DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
	      
	      ROW=5
	      @ ROW,01 SAY XSTYLE
	      @ ROW,12 SAY "/"
	      @ ROW,13 SAY XCLR(I)
	      @ ROW,125 SAY 'TOTAL'
	      ROW=ROW+1
	      @ ROW,01 SAY REPLICATE('-',129)
	    ENDIF

	    IF (XSTK(I)<> 0 .OR. XSUBPOS(I) <> 0  .OR. XSUBCUT(I) <>0  .OR. XORD(I) <>0 .OR.;
	        XALC <> 0 .OR. XTRAN <> 0 .OR. XSHPPED <> 0 .OR. XPICK <> 0 )	    
	      IF ROW > 54 
	        ROW=0
	        PAGENO=PAGENO+1
            DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
            
	        ROW=5
	      ENDIF
          
          *** Display Style/COLOR Description
	      
	      ROW=ROW+1
	      
	      @ ROW,01 SAY XSTYLE
	      @ ROW,12 SAY "/"
	      @ ROW,13 SAY XCLR(I)
          
          ROW=ROW+1
          
	      @ ROW,01 SAY REPLICATE('-',131) 
	      
	      ROW=ROW+1
	      
	      @ ROW,13 SAY 'O.T.S :'
          XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)+XSTK(I)-XSUBORD(I)
	      XTOTOTS=XTOTOTS+XOTS(I)
	      @ ROW,23 SAY XOTS(I)     PICTURE '99999999'
	    
	      @ ROW,33 SAY 'WIP    :'
          XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)
	      @ ROW,44 SAY XSUBCUT(I)+XSUBPOS(I)  PICTURE '9999999'
        
          @ ROW,54 SAY 'In-Transit:' 
          @ ROW,67 SAY XTRAN PICTURE '9999999' 
	    
	      @ ROW,79 SAY 'Stock:'
	      XTOTSTK=XTOTSTK+XSTK(I)
	      XDIVSTK=XDIVSTK+XSTK(I)
	      @ ROW,83 SAY XSTK(I)  PICTURE '9999999'
	    
	      ROW=ROW+1
	    
	      @ ROW,13 SAY 'Orders:'
	      @ ROW,23 SAY XSUBORD(I)  PICTURE '99999999'
	    
	      @ ROW,33 SAY 'Shipped:'
          @ ROW,44 SAY XSHPPED PICTURE '9999999'
        
          @ ROW,58 SAY 'Picked:' 
          @ ROW,67 SAY  XPICK PICTURE '9999999' 
        
          @ ROW,75 SAY 'Allocated:'
          IF XALLOCAT <> 'N'
            @ ROW,81 SAY XALC  PICTURE '9999999'
          ENDIF
                
          ROW=ROW+2
	    ENDIF  
      
        *B804101,1 AME [Start] Put This line insid maion scan loop
        XTOTORD=XTOTORD+XSUBORD(I)
        *B804101,1 AME [End]  	 
	  
	  *B804101,1 AME [Start]  document this for loop and use scan instead
	  *ENDFOR   && FOR I=1 TO XCNT
	  ENDSCAN  && Main scan loop (seaso+cdivision+style = xseason+xdivision+xstyle)
	  *B804101,1 AME [End]
	  
      *B804101,1 AME [Start]  No Need For The new for loop
      *B804101,1 AME          We put it in main for loop 
      *FOR I=1 TO XCNT
      *  XTOTORD=XTOTORD+XSUBORD(I)
      *ENDFOR
      *B804101,1 AME [End]         
      
      ROW=ROW+2
	  IF ROW > 52
	    ROW=0
	    PAGENO=PAGENO+1
        DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
        ROW = 5
	  ENDIF
	  @ ROW,3 SAY REPLICATE('=',129)
	  ROW=ROW+1
	  @ ROW,3 SAY 'SUB TOTAL OF STYLE:'+XSTYLE+'  STOCK:'
	  @ ROW,43 SAY XTOTSTK  PICTURE '99999999'
      @ ROW,55 SAY 'W.I.P:'
      @ ROW,61 SAY XTOTWIP  PICTURE '99999999'
      @ ROW,72 SAY 'ORDERS:'
      @ ROW,80 SAY XTOTORD  PICTURE '99999999'
      @ ROW,91 SAY 'O.T.S:'
      @ ROW,98 SAY XTOTOTS  PICTURE '99999999'
      @ ROW,108 SAY 'EXT. Price.:'
      @ ROW,121 SAY XTOTEXT  PICTURE '99999999.99'
      ROW=ROW+1
      @ ROW,3 SAY REPLICATE('=',129)
      
      XDIVEXT = XDIVEXT + XTOTEXT
      XDIVORD = XDIVORD + XTOTORD
      XDIVWIP = XDIVWIP + XTOTWIP
      
      SELECT &StyTemp
      *B804101,1 AME [Start] the Pointer already points to the 
      *B804101,1 AME         fisrt record of the next sty maj.
      *SCAN WHILE season+cdivision+SUBSTR(STYLE,1,lnMajLen)=XSEASON+XDIVISION+XSTYLE
      *ENDSCAN 
      *B804101,1 AME [End]
      
	  IF SEASON+CDIVISION <> XSEASON+XDIVISION
	    ROW=ROW+2
	    IF ROW > 52
	      ROW=0
	      PAGENO=PAGENO+1
	      DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
	      ROW = 5
	    ENDIF
	    @ ROW,3 SAY REPLICATE ('=',129)
	    ROW=ROW+1
	    @ ROW,3 SAY 'SUB TOTAL OF DIVISION:'+XDIVISION+'     STOCK:'
	    @ ROW,43 SAY XDIVSTK  PICTURE '99999999'
        @ ROW,55 SAY 'W.I.P:'
        @ ROW,61 SAY XDIVWIP  PICTURE '99999999'
        @ ROW,72 SAY 'ORDERS:'
        @ ROW,80 SAY XDIVORD  PICTURE '99999999'
        @ ROW,91 SAY 'O.T.S:'
        @ ROW,98 SAY XDIVSTK+XDIVWIP-XDIVORD  PICTURE '99999999'
        @ ROW,108 SAY 'EXT. Price.:'
        @ ROW,121 SAY XDIVEXT  PICTURE '99999999.99'
        ROW=ROW+1
	    @ ROW,3 SAY REPLICATE ('=',129)
	    EXIT
	  ENDIF
	  ROW=ROW+3
	ENDDO
    
    XSEASTK = XSEASTK + XDIVSTK
    XSEAWIP = XSEAWIP + XDIVWIP
    XSEAORD = XSEAORD + XDIVORD
    XSEAEXT = XSEAEXT + XDIVEXT
    *B804101,1 AME [Start] the Pointer already points to the 
    *B804101,1 AME         fisrt record of the next Division
	*SELECT &StyTemp
	*SCAN WHILE SEASON+CDIVISION =XSEASON+XDIVISION
    *ENDSCAN 
    *B804101,1 AME [End]
    
    IF SEASON <> XSEASON
      ROW=ROW+2
      IF ROW > 52
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH
        ROW = 5
      ENDIF
      @ ROW,3 SAY REPLICATE ('=',129)
      ROW=ROW+1
      @ ROW,3 SAY 'SUB TOTAL OF SEASON:'+XSEASON+'       STOCK:'
      @ ROW,43 SAY XSEASTK  PICTURE '99999999'
      @ ROW,55 SAY 'W.I.P:'
      @ ROW,61 SAY XSEAWIP  PICTURE '99999999'
      @ ROW,72 SAY 'ORDERS:'
      @ ROW,80 SAY XSEAORD  PICTURE '99999999'
      @ ROW,91 SAY 'O.T.S:'
      @ ROW,98 SAY XSEASTK+XSEAWIP-XSEAORD  PICTURE '99999999'
      @ ROW,108 SAY 'EXT. Price.:'
      @ ROW,121 SAY XSEAEXT  PICTURE '99999999.99'
      ROW=ROW+1
      @ ROW,3 SAY REPLICATE ('=',129)      
      EXIT
    ENDIF
    ROW=ROW+3
  ENDDO

  XGRDEXT = XGRDEXT + XSEAEXT
  XGRDSTK = XGRDSTK + XSEASTK
  XGRDWIP = XGRDWIP + XSEAWIP
  XGRDORD = XGRDORD + XSEAORD
  
  *B804101,1 AME [Start] the Pointer already points to the 
  *B804101,1 AME         fisrt record of the next season
  *SELECT &StyTemp
  *SCAN WHILE SEASON == XSEASON
  *ENDSCAN 
  *B804101,1 AME [End]
  
  IF EOF()
    ROW=ROW+2
    IF ROW > 52
      ROW=0
      PAGENO=PAGENO+1
      DO RPT_HDR WITH 'SOMAR100',XTITLE,R_WIDTH      
      ROW = 5
    ENDIF
    @ ROW,20 SAY REPLICATE('=',112)
    ROW=ROW+1
    @ ROW,20 SAY 'GRAND TOTALS:   STOCK:'
    @ ROW,43 SAY XGRDSTK  PICTURE '99999999'
    @ ROW,55 SAY 'W.I.P:'
    @ ROW,61 SAY XGRDWIP  PICTURE '99999999'
    @ ROW,72 SAY 'ORDERS:'
    @ ROW,80 SAY XGRDORD  PICTURE '99999999'
    @ ROW,91 SAY 'O.T.S:'
    @ ROW,98 SAY XGRDSTK+XGRDWIP-XGRDORD  PICTURE '99999999'
    @ ROW,108 SAY 'EXT. Price.:'
    @ ROW,121 SAY XGRDEXT  PICTURE '99999999.99'

    ROW=ROW+1
    @ ROW,20 SAY REPLICATE('=',112)    
    EXIT
  ENDIF
  ROW=ROW+3
ENDDO
IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF    && End of IF USED('CUTPICK')
IF USED('INVLINE')
  USE IN INVLINE
ENDIF    && End of IF USED('INVLINE')
IF USED('CUTPICK3')
  USE IN CUTPICK3
ENDIF    && End of IF USED('CUTPICK3')

RETURN

*!*************************************************************
*! Name      : lfTermnate
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
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
   GO TOP
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE
*B802399,1 - WAB - END

*!*************************************************************
*! Name      : lfItmPos
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 05/21/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
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

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.
            *B804101,1 AME [Start] No Mean of this line
            *@ ROW,121 SAY XSUBTOT  PICTURE '999999999'
            *B804101,1 AME [End]
             
*!*************************************************************
*! Name      : lfFormDat
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 15/05/2000
*! Purpose   : Evaluate  filter exprission to the Date Range
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : lcBegDat   Beginning Date
*!                      lcEndDat   End Date
*!                      lcVar      date to be filtered  
*!*************************************************************
*! Returns            : Filter Expression (BETWEEN / .T.)
*!*************************************************************
*! Example   : = lfFormDat()
*!*************************************************************
*
FUNCTION lfFormDat
PARAMETERS lcBegDat, lcEndDat, lcVar
Private lcFlt 
lcsetCen = SET('CENTURY')
SET CENTURY ON
IF lcBegDat<>CTOD('  /  /    ')
  lcEndDat = IIF(lcEndDat=CTOD('  /  /    '),lcBegDat,lcEndDat)
  *lcFlt = IIF(lcEndDat=CTOD('  /  /    '),"&lcVar=&lcBegDat","BETWEEN(&lcVar,&lcBegDat,&lcEndDat)")
  lcFlt = "BETWEEN("+ lcVar + ", {" + DTOC(lcBegDat) + "} , {" + DTOC(lcEndDat) + "})"
ELSE
  lcFlt = ".T."
ENDIF
SET CENTURY &lcSetCen
RETURN lcFlt
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Ahmed Mohamed ELanwar (AME)
*! Date      : 04/02/2001
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG.
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*B804101,1 
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *B804101,4 AME [Start] Remove Cancele status      
    *lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))

    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))

  ENDFOR  && end Loop to make Status expression.
ENDIF

*B804101,4 AME [Start] Remove Cancele status
*lcRpStatus = IIF(EMPTY(lcRpStatus),'OHXCB',ALLTRIM(lcRpStatus))
lcRpStatus = IIF(EMPTY(lcRpStatus),'OHCB',ALLTRIM(lcRpStatus))
*B804101,4 AME [End]

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

IF !(lcOldStat == lcRpStatus) && If difference happened in status expression.
  *-- if length of current selected status differ from previous length 
  IF LEN(lcOldStat) != LEN(lcRpStatus) 
    llOGFltCh = .T.
  
  ELSE  && else if length of current selected status equal previous length
    *-- loop to check if it's the same selected status or not.
    FOR lnJ = 1 TO LEN(lcOldStat)
      lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
      IF !(lcCurrChr $ lcRpStatus)
        llOGFltCh = .T.
        EXIT
      ENDIF
    ENDFOR  && end loop to check if it's the same selected status or not.
  ENDIF
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.




*!*************************************************************
*! Name      : lfvPOStatus
*! Developer : Ahmed Mohamed ELanwar (AME)
*! Date      : 04/02/2001
*! Purpose   : - Evaluate PO Status expression.
*!           : - Rise change PO status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvPOStatus()
*!*************************************************************
*B804101,1 
FUNCTION lfvPOStatus 
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpPoStat  && Save old status value.

= gfMover(@laRpPoSour,@laRpPoTarg,'Select Purchase Order Status',.T.,'')  && call mover function.

lcRpPoStat = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpPoTarg[1])
  FOR lnI = 1 TO ALEN(laRpPoTarg,1)
    
    *B804101,4 AME [Start] Remove Canceled status 
    *lcRpPoStat = lcRpPoStat + IIF(laRpPoTarg[lnI] = 'Open','O',;
                              IIF(laRpPoTarg[lnI] = 'Hold','H',;
                              IIF(laRpPoTarg[lnI] = 'Bid','B',;
                              IIF(laRpPoTarg[lnI] = 'Complete','C',;
                              IIF(laRpPoTarg[lnI] = 'Closed','S',;                              
                              IIF(laRpPoTarg[lnI] = 'Canceled','X',''))))))

    lcRpPoStat = lcRpPoStat + IIF(laRpPoTarg[lnI] = 'Open','O',;
                              IIF(laRpPoTarg[lnI] = 'Hold','H',;
                              IIF(laRpPoTarg[lnI] = 'Bid','B',;
                              IIF(laRpPoTarg[lnI] = 'Complete','C',;
                              IIF(laRpPoTarg[lnI] = 'Closed','S','')))))

  *B804101,4 AME [End]
  ENDFOR  && end Loop to make Status expression.
ENDIF

*B804101,4 AME [Start] Remove  cancel status
*lcRpPoStat = IIF(EMPTY(lcRpPoStat),'OHXBS',ALLTRIM(lcRpPoStat))
lcRpPoStat = IIF(EMPTY(lcRpPoStat),'OHBS',ALLTRIM(lcRpPoStat))
*B804101,4 AME [End]

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

IF !(lcOldStat == lcRpPoStat) && If difference happened in status expression.
  *-- if length of current selected status differ from previous length 
  IF LEN(lcOldStat) != LEN(lcRpPoStat) 
    llOGFltCh = .T.
  
  ELSE  && else if length of current selected status equal previous length
    *-- loop to check if it's the same selected status or not.
    FOR lnJ = 1 TO LEN(lcOldStat)
      lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
      IF !(lcCurrChr $ lcRpPoStat)
        llOGFltCh = .T.
        EXIT
      ENDIF
    ENDFOR  && end loop to check if it's the same selected status or not.
  ENDIF
ENDIF
*-- Compare current selected status with old value  [end]

*-- end of lfvPOStatus