*:***************************************************************************
*: Program file  : SODELVSC.prg
*: Program desc. : Sales Order Delivery Schedule
*: System        : Aria Advantage Series.4xp
*: Module        : Sales Order (SO)
*! Developer   	 : Mariam Mazhar [MMT]
*! Date      	   : 06/22/2006  (037664)
*:***************************************************************************
*: Calls :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO SODELVSC
*:***************************************************************************************
*: Modification:
*!* B609356,1 SMA 07/26/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[T20110627.0037]
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[T20110428.0019]
*:******************************************************************************
*--First check Completion Dates and start dates

lcStTime = TIME()
llNoRecs = .T.
llUseExSzScl = gfGetMemVar('M_USEEXSSC')

loOgScroll.lcOGLastForm = 'SODELVSC'
loOgScroll.cCRorientation = 'L'

DIMENSION loOgScroll.laCRParams[7,2]

loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'Delivery Schedule  '+IIF(lcBuckBy = "S",'By Start Date','By Completion Date')


*-- Make Sure the set Centurey is on
PRIVATE lcCentSt
lcCentSt = SET('CENTURY')
SET CENTURY ON

IF lcBuckBy == "C"
  XDATE1 = ldRpCmp
  XDATE2 = ldRpCmp2
  XDATE3 = ldRpCmp3
  XDATE4 = ldRpCmp4
ELSE
  XDATE1 = ldRpSrt
  XDATE2 = ldRpSrt2
  XDATE3 = ldRpSrt3
  XDATE4 = ldRpSrt4
ENDIF

loOgScroll.laCRParams[2,1] = 'FrstDate'
loOgScroll.laCRParams[2,2] = DTOC(XDATE1)

loOgScroll.laCRParams[3,1] = 'SndDate'
loOgScroll.laCRParams[3,2] = DTOC(XDATE2)

loOgScroll.laCRParams[4,1] = 'ThrdDate'
loOgScroll.laCRParams[4,2] = DTOC(XDATE3)

loOgScroll.laCRParams[5,1] = 'FrthDate'
loOgScroll.laCRParams[5,2] = DTOC(XDATE4)

loOgScroll.laCRParams[6,1] = 'GrpByClr'
loOgScroll.laCRParams[6,2] = IIF(lcRpByClr  = 'Y',1,0)

loOgScroll.laCRParams[7,1] = 'lnMajLen'
loOgScroll.laCRParams[7,2] = lnMajLen



*--To add completion date to the filter look the max and the min date
IF lcBuckBy == "C"
  ldMaxComp = MAX(XDATE1 , XDATE2 , XDATE3 , XDATE4)
  ldMinComp = MIN(XDATE1 , XDATE2 , XDATE3 , XDATE4)
ELSe
  ldMaxStrt = MAX(XDATE1 , XDATE2 , XDATE3 , XDATE4)
  ldMinStrt = MIN(XDATE1 , XDATE2 , XDATE3 , XDATE4)
ENDIF

llCutTkt = ('MF' $ oAriaApplication.CompanyInstalledModules)
llPo = ('PO' $ oAriaApplication.CompanyInstalledModules) OR ('PS' $ oAriaApplication.CompanyInstalledModules)

lnStartPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'ORDLINE.START'),1)
lnCompPos  = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'ORDLINE.COMPLETE'),1)

IF lcBuckBy == "C"
  IF "|  /  / " $ loOGScroll.laOGFxFlt[lnStartPos,6]
    loOGScroll.laOGFxFlt[lnStartPos,6] = ""
  ENDIF
  IF EMPTY(loOGScroll.laOGFxFlt[lnStartPos,6]) .OR. LEN(ALLTRIM(loOGScroll.laOGFxFlt[lnStartPos,6])) < 8
    WAIT WINDOW 'You have to enter at least one Start Date !'
    SET DEVICE TO SCREEN
    llNoRecs = .T.
    RETURN
  ELSE
    IF OCCURS('|',loOGScroll.laOGFxFlt[lnStartPos,6]) > 1
      WAIT WINDOW 'Only 2 Start Dates will be printed ...'
    ENDIF
  ENDIF
ELSE
  IF "|  /  / " $ loOGScroll.laOGFxFlt[lnCompPos,6]
    loOGScroll.laOGFxFlt[lnCompPos,6] = ""
  ENDIF
  IF EMPTY(loOGScroll.laOGFxFlt[lnCompPos,6]) .OR. LEN(ALLTRIM(loOGScroll.laOGFxFlt[lnCompPos,6])) < 8
    WAIT WINDOW 'You have to enter at least one Complete Date !'
    SET DEVICE TO SCREEN
    llNoRecs = .T.
    RETURN
  ELSE
    IF OCCURS('|',loOGScroll.laOGFxFlt[lnCompPos,6]) > 1
      WAIT WINDOW 'Only 2 Complete Dates will be printed ...'
    ENDIF
  ENDIF
ENDIF

IF lcBuckBy == "C"
  IF EMPTY(ldRpCmp) OR EMPTY(ldRpCmp2) OR EMPTY(ldRpCmp3) OR EMPTY(ldRpCmp4)
    WAIT WINDOW 'No Completion date buckets found, Cannot proceed!'
    RETURN
  ENDIF
ELSE
  IF EMPTY(ldRpSrt) OR EMPTY(ldRpSrt2) OR EMPTY(ldRpSrt3) OR EMPTY(ldRpSrt4)
    WAIT WINDOW 'No Starting date buckets found, Cannot proceed!'
    RETURN
  ENDIF
ENDIF

IF lcBuckBy == "C"
  lcRpExp = lcRpExp + '.AND. BETWEEN(DTOS(OrdLine.COMPLETE),DTOS({' + DTOC(ldMinComp) + '}),DTOS({' + DTOC(ldMaxComp) + '}))'
ELSE
  lcRpExp = lcRpExp + '.AND. BETWEEN(DTOS(OrdLine.START),DTOS({' + DTOC(ldMinStrt) + '}),DTOS({' + DTOC(ldMaxStrt) + '}))'
ENDIF  

SET CENTURY &lcCentSt

*!*  IF lcMake <> 'A'
*!*    lcMakeExp = "STYLE.MAKE = IIF(lcMake='N',.F.,.T.)"
*!*    lcRpExp = lcRpExp + ' AND ' + lcMakeExp
*!*  ENDIF

STORE SPACE(0) TO WORKTEMP,CUTTTEMP,POTEMP,ORDTEMP,XPRTWIP,XWIPRPT,XWIPRPT,;
  XPRTORD,XORDRPT,XORDRPT,XSTAT
STORE .F. TO llNoRecs
lcRepNmTtl = gfItemMask("HN")
lcTime     = gfGetTime()
XBYCOLOR   = lcRpByClr

llNewVar = lcRpByClr
*!*  STORE 'Y' TO lcRpByClr


STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

DECLARE laCmpRange[4]

STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
           XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
           XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
           XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
           XWIP1,XWIP2,XWIP3,XWIP4

SELECT ORDLINE


llNoRecs = .F.

IF loOgScroll.llOGFltCh && OG Filters changed
  lfCrtTemp()
  lfCollectData()
ENDIF
SELECT(lcTmpWork)
LOCATE 
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF 

DIMENSION LOogsCROLL.laCRTables[1]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
COPY TO oAriaApplication.WorkDir +  lcTmpFile+ ".DBF"

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTmpFile + ".DBF"


gfDispRe()
RETURN 
  


*!*	DO lpPrepPrnt

*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************

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

*!*************************************************************

FUNCTION lfwRepWhen



IF !llFrstTime
  loDBFStyle = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
  loDBFPOSLN    = CreateObject("RemoteTable","POSLN","POSLN",'POSLN',SET("DATASESSION"))&&,"",.T.)
  loDBFPOSHDR = CreateObject("RemoteTable","POSHDR","POSHDR",'POSHDR',SET("DATASESSION"))&&,"",.T.)
  loDBFOrdline= CreateObject("RemoteTable","Ordline","Ordline",'Ordline',SET("DATASESSION"))&&,"",.T.)
  loDBFordhdr = CreateObject("RemoteTable","ordhdr","ordhdr",'ordhdr',SET("DATASESSION"))&&,"",.T.)
  
  lcSqlStat1 = "SELECT ITEMLOC.Style,ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  lnResult1 = loOGScroll.ORDA.SQLRun(lcSqlStat1, lcTmpFab, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE", SET("Datasession"))
  llFrstTime = .T.
  IF lnResult1 >= 1
    =lfCreateIndecies(lcTmpFab, "Fabric|Style", "lcFabDye|'Style'")
  ENDIF
ENDIF 


*!*  SET ORDER TO SEGVAL IN ICSEGVAL
*!*  SET ORDER TO SCALE IN SCALE
STORE 1 TO lnMajPos

*--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
*ldRpCmp  = LDOM(DATE())
*ldRpCmp2 = LDOM(ldRpCmp + 2)
*ldRpCmp3 = LDOM(ldRpCmp2+ 2)
*ldRpCmp4 = LDOM(ldRpCmp3+ 2)
IF lcBuckBy="C"
  ldRpCmp  = LDOM(DATE())
  ldRpCmp2 = LDOM(ldRpCmp + 2)
  ldRpCmp3 = LDOM(ldRpCmp2+ 2)
  ldRpCmp4 = LDOM(ldRpCmp3+ 2)
ELSE
  ldRpSrt  = LDOM(DATE())
  ldRpSrt2 = LDOM(ldRpSrt + 2)
  ldRpSrt3 = LDOM(ldRpSrt2+ 2)
  ldRpSrt4 = LDOM(ldRpSrt3+ 2)
ENDIF
*--C102263,1 AAN Add a condition of select by complete date or start date [End]

*--C102263,1 AAN Remove the following line [Begin]
*lnStartPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)
*--C102263,1 AAN Remove the following line [End]
*lnCompPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.COMPLETE'),1)

*--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
IF lcBuckBy == "C"
  lnStartPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)
ELSE
  lnCompPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.COMPLETE'),1)
ENDIF
*--C102263,1 AAN Add a condition of select by complete date or start date [End]

*IF EMPTY(laOGFxFlt[lnCompPos,6])
*  laOGFxFlt[lnCompPos,6] = DTOC(XDATE1) + '|' + DTOC(XDATE2)+ '|' + DTOC(XDATE3)+ '|' + DTOC(XDATE4)
*ENDIF

*--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
*IF EMPTY(laOGFxFlt[lnStartPos,6])
*  laOGFxFlt[lnStartPos,6] = DTOC(DATE()) + '|' + DTOC(DATE())
*ENDIF

*B606352,4 BWA 10/21/2002 Comment this lines due to not printing the dates.[START]
*IF lcBuckBy == "S"
*  IF EMPTY(laOGFxFlt[lnCompPos,6])
*    laOGFxFlt[lnCompPos,6] = DTOC(DATE()) + '|' + DTOC(DATE())
*  ENDIF
*ELSE
*  IF EMPTY(laOGFxFlt[lnStartPos,6])
*    laOGFxFlt[lnStartPos,6] = DTOC(DATE()) + '|' + DTOC(DATE())
*  ENDIF
*ENDIF
*B606352,4 BWA 10/21/2002.[END]

*--C102263,1 AAN Add a condition of select by complete date or start date [End]

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

FUNCTION lfAdjPrnArr

DIMENSION laRPPrnDsp[2,1],laRPPrnRet[2,1]

laRPPrnDsp[1,1]=lcMajTtl
laRPPrnDsp[2,1]=lcNonMajT

laRPPrnRet[1,1]='S'
laRPPrnRet[2,1]='C'

*!*********************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************

FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

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

****************************************************************************
PROCEDURE lpPrepPrnt
**lcMake    &&


ROW     = 99
DIMENSION XPREPAK[8]
STORE 0 TO XPREPAK

R_TITLE = 'Delivery Schedule'
R_WIDTH = 'W'
PAGENO  = 00

WAIT WINDOW 'PRINTING ... <Space Bar> to Abort' NOWAIT
SELECT STYLE

*B606352,4 BWA 10/21/2002 Check for the style.[START]
*SET FILTER TO &lcRPExp
PRIVATE lcFltrSty
STORE SPACE(0) TO lcFltrSty , lcStyPos
llSekPart = .T.
lnStyPos = AT('INLIST(PADR(SUBSTR(ORDLINE.STYLE,lnMajPos,lnMajLen', lcRPExp)

IF lnStyPos = 0
  lnStyPos = AT('BETWEEN(PADR(SUBSTR(ORDLINE.STYLE,lnMajPos,lnMajLen', lcRPExp)
  IF lnStyPos > 0
    lnStylePos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'PADR(SUBSTR(ORDLINE.STYLE,lnMajPos,lnMajLen),19)'),1)
    lcHoldNam  = laOGFxFlt[lnStylePos,6]
    lnPosTmp   = AT(lcHoldNam , SUBSTR(lcRPExp,lnStyPos)) + 9
    lcStyPos   = SUBSTR(lcRpExp , lnStyPos , lnPosTmp)
    lcStyPos   = STRTRAN(lcStyPos,"ORDLINE" , "STYLE")
    llSekPart  = .F.
  ENDIF
  lnStyPos   = 0
ENDIF

IF lnStyPos > 0
  lnPos1     = AT('")' , SUBSTR(lcRPExp,lnStyPos))
  IF lnPos1 > 0
    lcStyPos = SUBSTR(lcRpExp , lnStyPos , lnPos1+1)
  ELSE
    lcStyPos = SUBSTR(lcRpExp ,lnStyPos)
  ENDIF
  lcStyPos = STRTRAN(lcStyPos,"ORDLINE" , "STYLE")
ELSE
  IF llSekPart
    lcStyPos = ".T."
  ENDIF
ENDIF

SET FILTER TO &lcStyPos
*B606352,4 BWA 10/21/2002.[END]

*:B603713,11  MHM 10/11/2000  [START]
*GO TOP
LOCATE
*:B603713,11  MHM 10/11/2000  [END]

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
*--B803972,1 AAN Change "GOTO TOP" with "LOCATE"[Begin]
*GOTO TOP
LOCATE
*--B803972,1 AAN Change "GOTO TOP" with "LOCATE"[End]

SELECT 0
*-----------------------------------
WorkTemp = gfTempName()
ORDTEMP  = gfTempName()
*B603713,8  MHM 10/11/2000 icrease amount to 13[start]
*CREATE TABLE (gcWorkDir + ORDTEMP)(STYLE  C(19)      ,;
  COLOR  C(6)       ,;
  O_ORD1 N(10)       ,;
  O_ORD2 N(10)       ,;
  O_ORD3 N(10)       ,;
  O_ORD4 N(10)       ,;
  O_ORDAMT1 N(10,2) ,;
  O_ORDAMT2 N(10,2) ,;
  O_ORDAMT3 N(10,2) ,;
  O_ORDAMT4 N(10,2) ,;
  H_ORD1 N(10)       ,;
  H_ORD2 N(10)       ,;
  H_ORD3 N(10)       ,;
  H_ORD4 N(10)       ,;
  H_ORDAMT1 N(10,2) ,;
  H_ORDAMT2 N(10,2) ,;
  H_ORDAMT3 N(10,2) ,;
  H_ORDAMT4 N(10,2) ,;
  WIP1 N(10)         ,;
  WIP2 N(10)         ,;
  WIP3 N(10)         ,;
  WIP4 N(10)         ,;
  TOTSTK N(10)       ,;
  TOTORD N(12)       ,;
  TOTWIP N(10,2)    ,;
  H_TOTORDA N(10,2) ,;
  O_TOTORDA N(10,2) ,;
  PYSHIP1 N(7)      ,;
  PYSHIP2 N(7)      ,;
  PYSHIP3 N(7)      ,;
  PYSHIP4 N(7)      ,;
  PNSHIP1 N(7)      ,;
  PNSHIP2 N(7)      ,;
  PNSHIP3 N(7)      ,;
  PNSHIP4 N(7))

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

*B606352,4 BWA 10/21/2002 Creat new table to collect the records in it.[START]
IF llNewVar = "N"
  CopyOrd = gfTempName()
  COPY STRUCTURE TO (gcWorkDir + CopyOrd)
  USE (gcWorkDir + CopyOrd) IN 0
  SELECT (CopyOrd)
  INDEX ON STYLE TAG &CopyOrd
  SELECT (ORDTEMP)
ENDIF
*B606352,4 BWA 10/21/2002.[END]

*B603713,8  MHM 10/11/2000 icrease amount to 13[End]
*-----------------------------------
*Table alrady created ..HDM.
*// COPY STRUCTURE TO &QWD.&ORDTEMP
*B800424,1 TAK 01/16/95 Open new temp file for records by style
*B800424,1 the 'OrdTemp' holds the records by color (sum it later).

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

*--Relation 1[start]
*B802747,1 ABD if Company have the MF Module Made The relation else Don't.  [ Begin ]
IF llCutTkt
*B802747,1 ABD [ End ]
  SELECT CUTTKTL
  SET RELATION TO CUTTKT INTO CUTTKTH
*B802747,1 ABD if Company have the MF Module Made The relation else Don't.  [ Begin ]
ENDIF
*B802747,1 ABD [ End ]
*--Relation 1[end]

*--Relation 2[start]
*B603326,1 Ramy [start]
IF llPo
  SELECT POSLN
  SET RELATION TO 'P' + PO INTO POSHDR
ENDIF
*B603326,1 Ramy [end]
*--Relation 2[end]

*--Relation 3[start]
SELECT ORDLINE
SET RELATION TO 'O' + ORDER INTO ORDHDR
*--Relation 3[end]

*--Relation 4 [start]
*--Establish relation between ordline and style file according to report style
SELECT STYLE
SET ORDER TO TAG Style
*IF lcRpByClr = 'Y'

*B606352,4 BWA 10/21/2002 Change the place of the relation.[START]
*SELECT ORDLINE
*SET RELATION TO Ordline.style INTO Style ADDITIVE
*B606352,4 BWA 10/21/2002.[END]

*ELSE
*  *SET ORDER TO TAG cStyle
*  *SET ORDER TO TAG Style
*  SELECT ORDLINE
*  SET RELATION TO PADR(SUBSTR(Ordline.style,1,lnMajLen),19) INTO Style ADDITIVE
*ENDIF
*--Relation 4[end]

XSTYLE = SPACE(19)
XCOLOR = SPACE(6)
HSTYLE = SPACE(6)
SELECT ORDLINE
*:B603713,11  MHM 10/11/2000  [START]
*GO TOP
LOCATE
*:B603713,11  MHM 10/11/2000  [END]
*DO WHILE .T.                   && HDM           INKEY() <> 32
llNoRecs = .T.

*E500271,4 add Field LEDIORDER to the filter expression [Begin.]
IF !EMPTY(lcRpEdiFlt)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcRpEdiFlt
ENDIF
*E500271,4 add Field LEDIORDER to the filter expression [End.]
*-- B802694,1 HDM {Start} Check the report filter before the scan
*:B603988,1 MHM 10/27/2000 Fix the bug of performance to increase speed [start]
*LOCATE FOR &lcRpExp
*--C102263,1 AAN Remove the following lines[Begin]
*lcSavORd = SET('ORDER')
*SET ORDER TO 
*LOCATE FOR &lcRpExp
*SET ORDER TO &lcSavORd
llNoRecs = .F.
*--C102263,1 AAN Remove the following lines[End]

*IF FOUND()
*  llNoRecs = .F.
*ELSE
*  SET DEVICE TO SCREEN
*  llNoRecs = .T.
*  RETURN
*ENDIF
*:B803972,1 AAN Enhance the performance of previewing or printing the report[End]
*-- B802694,1 HDM {End}
lcLopedSty = ''

*:B603988,1 MHM 10/27/2000 Fix the bug of performance to increase speed [start]

*--B803972,1 AAN Remove the following lines [Begin]
*lcSavORd = SET('ORDER')
*SET ORDER TO 
*--B803972,1 AAN Remove the following lines [End]
*:B603988,1 MHM 10/27/2000 Fix the bug of performance to increase speed [end]

*--B803972,1 AAN Scan into the cursor that hold the selected styles [Begin]
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

    *B606352,4 BWA 10/21/2002 Hold the record number.[START]
    lnRcTmpSty = RECNO(laOgFxFLt[1,6])
    *B606352,4 BWA 10/21/2002.[END]

    *-B605425,1 MHM 01/27/2002 get major length from lnMajlen variable   [Start]
    *=SEEK(LEFT(cStyMajor,12),"ORDLINE")
    *IF FOUND()

    =SEEK(LEFT(cStyMajor,lnMajLen),"ORDLINE")
    IF FOUND("ORDLINE")
    *--B605425,1 MHM 01/27/2002 [End]

      *B606352,4 BWA 10/21/2002 The new place of the relation.[START]
      SELECT ORDLINE
      IF !('ORDLINE.STYLE INTO STYLE' $ SET('RELATION'))
        SET RELATION TO ORDLINE.STYLE INTO STYLE ADDITIVE
      ENDIF  
      *B606352,4 BWA 10/21/2002.[END]

      SELECT OrdLine
      *--B803972,1 AAN Scan into the cursor that hold the selected styles [End]
      *--B803972,1 AAN Change scan exp. [Begin]
      *SCAN FOR &lcRpExp AND !(IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) $ lcLopedSty)
  
      *-B605425,1 MHM 01/27/2002 get major length from lnMajlen variable   [Start]
      *SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(lineno,6) = LEFT(&laOgFxFLt[1,6]..cStyMajor,12) FOR &lcRpExp AND !(IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) $ lcLopedSty)
      SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(lineno,6) =;
                      LEFT(&laOgFxFLt[1,6]..cStyMajor,lnMajLen);
                FOR &lcRpExp AND !(IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) $ lcLopedSty)
      *-B605425,1 MHM [End]

        *--B803972,1 AAN Change scan exp. [End]
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
        *--HDM B602388,1 Incorrect Style Printing[start]
        *@ 23,00 SAY XSTYLE + XCOLOR
        *--HDM B602388,1 Incorrect Style Printing[end]
        XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
        *-------------------------
        * LOOP ORDER LINE FILE
        *---------------------- ---
        *-B500811,1 TAK 06/27/95 MOVE IT OUT OF SCAN
        SELECT &ORDTEMP
        IF !SEEK( XKEY )
          APPEND BLANK
          REPLACE STYLE  WITH XSTYLE,;
            COLOR  WITH XCOLOR
        ENDIF
        REPLACE TOTSTK WITH TOTSTK + XTOTSTK
        *--END TAK 06/27/95

        SELECT ORDLINE

        *--HDM B602388,1[START]
        *LOCATE FOR STYLE = XSTYLE
        lnLineRec = RECNO()
        *B802694,1 HDM [Start] Don't seek to void return to the top of the file
        *=SEEK(XSTYLE)
        *B802694,1 HDM [End]
        *--HDM B602388,1[END]
  
        *C200001,1 TMI 04/16/95 (Start) Commented Out and rewrite SCAN command
        SELECT ORDLINE
        *lnLineRec = RECNO()
        *B802694,1 HDM [Start] Don't seek to void return to the top of the file
        *=SEEK(XSTYLE)
        *B802694,1 HDM [End]
        lnRecNo = IIF(EOF(),0,RECNO())
        SCAN REST WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
  
          lnRecNo = IIF(EOF(),0,RECNO())
          IF ORDHDR->STATUS $ 'CX' .OR. TOTQTY <= 0
            LOOP
          ENDIF
          XORDER = ORDER
          *--HDM B602388,1 Blank Page Appears Before the report[start]
          *@ 23,70 SAY XORDER
          *--HDM B602388,1 Blank Page Appears Before the report[end]

          *--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
          *DO CASE
            *CASE ORDHDR->COMPLETE <= XDATE1
              *Z='1'
            *CASE BETWEEN(ORDHDR->COMPLETE,XDATE1 + 1 , XDATE2)
              *Z='2'
            *CASE BETWEEN(ORDHDR->COMPLETE,XDATE2 + 1 , XDATE3)
              *Z='3'
            *CASE BETWEEN(ORDHDR->COMPLETE,XDATE3 + 1 , XDATE4)
              *Z='4'
            *OTHE
              *Z= '*'
          *ENDCASE
          IF lcBuckBy == "C"
            DO CASE
              CASE ORDHDR.COMPLETE <= XDATE1
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
              CASE ORDHDR.START <= XDATE1
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
           *--C102263,1 AAN Add a condition of select by complete date or start date [End]
   
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
        IF STYLE->MAKE
          *B802747,1 ABD if Company have the MF Module Made The relation else Don't.  [ Begin ]
          IF llCutTkt
            *B802747,1 ABD [ End ]
            SELE CUTTKTL
            LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
            SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR CUTTKTH->STATUS $ 'OH'
              *--HDM B602388,1 Blank Page Appears Before the report[start]
              *@ 23,70 SAY CUTTKT
              *--HDM B602388,1 Blank Page Appears Before the report[end]
              *--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
              *DO CASE
                *CASE CUTTKTH->COMPLETE <= XDATE1
                  *XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                *CASE BETWEEN(CUTTKTH->COMPLETE, XDATE1 + 1 , XDATE2)
                  *XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                *CASE BETWEEN(CUTTKTH->COMPLETE , XDATE2 + 1 , XDATE3 )
                  *XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                *CASE BETWEEN(CUTTKTH->COMPLETE , XDATE3 + 1 , XDATE4)
                  *XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
              *ENDCASE
              IF lcBuckBy == "C"
                DO CASE
                  CASE CUTTKTH.COMPLETE <= XDATE1
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
                  *:B803972,4 AAN Change the field name[Begin].
                  *CASE CUTTKTH.START <= XDATE1
                    *XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                  *CASE BETWEEN(CUTTKTH.START, XDATE1 + 1 , XDATE2)
                    *XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                  *CASE BETWEEN(CUTTKTH.START , XDATE2 + 1 , XDATE3 )
                    *XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                  *CASE BETWEEN(CUTTKTH.START , XDATE3 + 1 , XDATE4)
                    *XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )

                  CASE CUTTKTH.ENTERED <= XDATE1
                    XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                  CASE BETWEEN(CUTTKTH.ENTERED, XDATE1 + 1 , XDATE2)
                    XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                  CASE BETWEEN(CUTTKTH.ENTERED , XDATE2 + 1 , XDATE3 )
                    XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                  CASE BETWEEN(CUTTKTH.ENTERED , XDATE3 + 1 , XDATE4)
                     XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
                  *:B803972,4 AAN Change the field name[End].
                ENDCASE
              ENDIF
              *--C102263,1 AAN Add a condition of select by complete date or start date [End]
            ENDSCAN
            *B802747,1 ABD if Company have the MF Module Made The relation else Don't.  [ Begin ]
          ENDIF
          *B802747,1 ABD [ End ]
        ELSE
          *B603326,1 Ramy [start]
          IF llPo        
            SELE POSLN
            *:B803916,1 MHM 12/25/2000 change order to correct locate [start]
            lcSvORd = SET('ORDER')
            SET ORDER TO Poslns
            *:B803916,1 MHM 12/25/2000 [end]
            LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
            *:B803916,1 MHM 12/25/2000 change order to correct locate [start]
            *SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR->STATUS $ 'OH' ;
            *    .AND. TRANCD <> '3'
            *ldPODate = IIF(EMPTY(POSHDR->AVAILABLE),POSHDR->COMPLETE,POSHDR->AVAILABLE)
            *--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
            *SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR->STATUS $ 'OH' ;
                .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR->AVAILABLE),POSHDR->COMPLETE,POSHDR->AVAILABLE)),;
                DTOS(ldMinComp),DTOS((ldMaxComp)))
            *:B803916,1 MHM 12/25/2000 [end]
  
            *XDATE = IIF(EMPTY(POSHDR->AVAILABLE),POSHDR->COMPLETE,;
                  POSHDR->AVAILABLE)
              *--HDM B602388,1 Blank Page Appears Before the report[start]
              *@ 23,70 SAY PO
              *--HDM B602388,1 Blank Page Appears Before the report[start]
              *DO CASE
                *CASE XDATE <=XDATE1
                  *XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                *CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                  *XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                *CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                  *XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                *CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                  *XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
              *ENDCASE
            *ENDSCAN
          
            IF lcBuckBy == "C"
              SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
                   .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),;
                   DTOS(ldMinComp),DTOS((ldMaxComp)))
              *:B803916,1 MHM 12/25/2000 [end]
                XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,;
                  POSHDR.AVAILABLE)
                *--HDM B602388,1 Blank Page Appears Before the report[start]
                *@ 23,70 SAY PO
                *--HDM B602388,1 Blank Page Appears Before the report[start]
                DO CASE
                  CASE XDATE <=XDATE1
                    XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                  CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                    XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                  CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                    *B606422,1 RAE WIP doesn't be printed if we change the buckets comp date.[start]
                    XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                    *B606422,1 RAE [end]
                  CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                    XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
                ENDCASE
              ENDSCAN
            ELSE
              SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
                .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),;
                DTOS(ldMinStrt),DTOS((ldMaxStrt)))
                *:B803916,1 MHM 12/25/2000 [end]
        
                XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,;
                POSHDR.AVAILABLE)
                *--HDM B602388,1 Blank Page Appears Before the report[start]
                *@ 23,70 SAY PO
                *--HDM B602388,1 Blank Page Appears Before the report[start]
                DO CASE
                  CASE XDATE <=XDATE1
                    XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
                  CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                    XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
                  CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                    *B606422,1 RAE WIP doesn't be printed if we change the buckets comp date.[start]
                    XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
                    *B606422,1 RAE [end]
                  CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                    XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
                ENDCASE
              ENDSCAN
            ENDIF
            *--C102263,1 AAN Add a condition of select by complete date or start date [End]
            *:B803916,1 MHM 12/25/2000 retrive order [start]
            SET ORDER TO &lcSvORd
            *:B803916,1 MHM 12/25/2000 [End]
          ENDIF
          *B603326,1 Ramy [end]
        ENDIF
        SELECT &ORDTEMP
        REPLACE WIP1 WITH WIP1 + XWIP1 ,;
                WIP2 WITH WIP2 + XWIP2 ,;
                WIP3 WITH WIP3 + XWIP3 ,;
                WIP4 WITH WIP4 + XWIP4

        SELECT STYLE
        XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
        *SKIP
      ENDSCAN
      IF BETWEEN(lnRcTmpSty,1,RECCOUNT(laOgFxFLt[1,6]))
        GOTO lnRcTmpSty IN laOgFxFLt[1,6]
      ENDIF

      IF lcRpByClr = 'Y'
        SELECT STYLE
        LOCATE
        SCAN REST WHILE STYLE =  "" FOR LEFT(STYLE.STYLE,lnMajLen) = LEFT(EVAL(laOgFxFLt[1,6]+'.CSTYMAJOR'),lnMajLen)
          IF !(STYLE.STYLE $ lcLopedSty)
            =lfOthrColr()
          ENDIF
        ENDSCAN
      ENDIF

    *B606352,4 BWA 10/21/2002 The new code for the PO WIP transaction.[START]
    ELSE
      SELECT STYLE
      =SEEK(LEFT(&laOgFxFLt[1,6]..cStyMajor,lnMajLen),"STYLE")
      lcStyStr = LEFT(&laOgFxFLt[1,6]..cStyMajor,lnMajLen)
      SCAN REST WHILE STYLE = lcStyStr
        IF IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) <> XSTYLE

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
          *-------------------------
          SELECT &ORDTEMP
          IF !SEEK( XKEY )
            APPEND BLANK
            REPLACE STYLE  WITH XSTYLE,;
                    COLOR  WITH XCOLOR
          ENDIF
          REPLACE TOTSTK WITH TOTSTK + XTOTSTK

          SELECT (ORDTEMP)
          REPLACE O_ORD1     WITH O_ORD1+XO_ORD1       ,;
                  O_ORDAMT1  WITH O_ORDAMT1+XO_ORDAMT1 ,;
                  H_ORD1     WITH H_ORD1+XH_ORD1       ,;
                  H_ORDAMT1  WITH H_ORDAMT1+XH_ORDAMT1 ,;
                  O_ORD2     WITH O_ORD2+XO_ORD2       ,;
                  O_ORDAMT2  WITH O_ORDAMT2+XO_ORDAMT2 ,;
                  H_ORD2     WITH H_ORD2+XH_ORD2       ,;
                  H_ORDAMT2  WITH H_ORDAMT2+XH_ORDAMT2 ,;
                  O_ORD3     WITH O_ORD3+XO_ORD3       ,;
                  O_ORDAMT3  WITH O_ORDAMT3+XO_ORDAMT3 ,;
                  H_ORD3     WITH H_ORD3+XH_ORD3       ,;
                  H_ORDAMT3  WITH H_ORDAMT3+XH_ORDAMT3 ,;
                  O_ORD4     WITH O_ORD4+XO_ORD4       ,;
                  O_ORDAMT4  WITH O_ORDAMT4+XO_ORDAMT4 ,;
                  H_ORD4     WITH H_ORD4+XH_ORD4       ,;
                  H_ORDAMT4  WITH H_ORDAMT4+XH_ORDAMT4
          ***
          ****** [PO] PROCESS P/O FILE
          ***

          IF STYLE->MAKE
            IF llCutTkt
              SELE CUTTKTL
              LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
              SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR CUTTKTH->STATUS $ 'OH'
                IF lcBuckBy == "C"
                  DO CASE
                    CASE CUTTKTH.COMPLETE <= XDATE1
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
                    CASE CUTTKTH.ENTERED <= XDATE1
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
                  XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)
                  DO CASE
                    CASE XDATE <=XDATE1
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
                  XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,POSHDR.AVAILABLE)
                  DO CASE
                    CASE XDATE <=XDATE1
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
          ENDIF
          SELECT &ORDTEMP
          REPLACE WIP1 WITH WIP1+XWIP1 ,;
                  WIP2 WITH WIP2+XWIP2 ,;
                  WIP3 WITH WIP3+XWIP3 ,;
                  WIP4 WITH WIP4+XWIP4

          SELECT STYLE
          XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))

        ENDIF
      ENDSCAN
      IF BETWEEN(lnRcTmpSty,1,RECCOUNT(laOgFxFLt[1,6]))
        GOTO lnRcTmpSty IN laOgFxFLt[1,6]
      ENDIF
      *B606352,4 BWA 10/21/2002.[END]

    ENDIF
    *--B803972,1 AAN Close If stat. of cursor scan [End]
  ENDSCAN
ELSE

  SELECT OrdLine

  *B606352,4 BWA 10/21/2002 A new relation to the style.[START]
  IF !('ORDLINE.STYLE INTO STYLE' $ SET('RELATION'))
    SET RELATION TO ORDLINE.STYLE INTO STYLE ADDITIVE
  ENDIF
  *B606352,4 BWA 10/21/2002.[END]

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
      STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1 ,;
                 XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2 ,;
                 XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3 ,;
                 XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4 ,;
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
    *--HDM B602388,1 Incorrect Style Printing[start]
    *@ 23,00 SAY XSTYLE + XCOLOR
    *--HDM B602388,1 Incorrect Style Printing[end]
    XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
    *-------------------------
    * LOOP ORDER LINE FILE
    *---------------------- ---
    *-B500811,1 TAK 06/27/95 MOVE IT OUT OF SCAN
    SELECT &ORDTEMP
    IF !SEEK( XKEY )
      APPEND BLANK
      REPLACE STYLE  WITH XSTYLE,;
        COLOR  WITH XCOLOR
    ENDIF
    REPLACE TOTSTK WITH TOTSTK + XTOTSTK
    *--END TAK 06/27/95

    SELECT ORDLINE

    *--HDM B602388,1[START]
    *LOCATE FOR STYLE = XSTYLE
    lnLineRec = RECNO()
    *B802694,1 HDM [Start] Don't seek to void return to the top of the file
    *=SEEK(XSTYLE)
    *B802694,1 HDM [End]
    *--HDM B602388,1[END]
  
  
    *C200001,1 TMI 04/16/95 (Start) Commented Out and rewrite SCAN command
    SELECT ORDLINE
    *lnLineRec = RECNO()
    *B802694,1 HDM [Start] Don't seek to void return to the top of the file
    *=SEEK(XSTYLE)
    *B802694,1 HDM [End]
    lnRecNo = IIF(EOF(),0,RECNO())
    SCAN REST WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
  
      lnRecNo = IIF(EOF(),0,RECNO())
      IF ORDHDR->STATUS $ 'CX' .OR. TOTQTY <= 0
        LOOP
      ENDIF
      XORDER = ORDER
      *--HDM B602388,1 Blank Page Appears Before the report[start]
      *@ 23,70 SAY XORDER
      *--HDM B602388,1 Blank Page Appears Before the report[end]

      *--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
      *DO CASE
        *CASE ORDHDR->COMPLETE <= XDATE1
          *Z='1'
        *CASE BETWEEN(ORDHDR->COMPLETE,XDATE1 + 1 , XDATE2)
          *Z='2'
        *CASE BETWEEN(ORDHDR->COMPLETE,XDATE2 + 1 , XDATE3)
          *Z='3'
        *CASE BETWEEN(ORDHDR->COMPLETE,XDATE3 + 1 , XDATE4)
          *Z='4'
        *OTHE
          *Z= '*'
      *ENDCASE
      IF lcBuckBy == "C"
        DO CASE
          CASE ORDHDR.COMPLETE <= XDATE1
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
          CASE ORDHDR.START <= XDATE1
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
      *--C102263,1 AAN Add a condition of select by complete date or start date [End]
    
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
    IF STYLE->MAKE
      *B802747,1 ABD if Company have the MF Module Made The relation else Don't.  [ Begin ]
      IF llCutTkt
        *B802747,1 ABD [ End ]
        SELE CUTTKTL
        LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
        SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR CUTTKTH->STATUS $ 'OH'
          *--HDM B602388,1 Blank Page Appears Before the report[start]
          *@ 23,70 SAY CUTTKT
          *--HDM B602388,1 Blank Page Appears Before the report[end]
          *--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
          *DO CASE
            *CASE CUTTKTH->COMPLETE <= XDATE1
              *XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
            *CASE BETWEEN(CUTTKTH->COMPLETE, XDATE1 + 1 , XDATE2)
              *XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
            *CASE BETWEEN(CUTTKTH->COMPLETE , XDATE2 + 1 , XDATE3 )
              *XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
            *CASE BETWEEN(CUTTKTH->COMPLETE , XDATE3 + 1 , XDATE4)
              *XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
          *ENDCASE        
          IF lcBuckBy == "C"
            DO CASE
              CASE CUTTKTH.COMPLETE <= XDATE1
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
              *:B803972,4 AAN Change the field name[Start].
              *CASE CUTTKTH.START <= XDATE1
                *XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
              *CASE BETWEEN(CUTTKTH.START, XDATE1 + 1 , XDATE2)
                *XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
              *CASE BETWEEN(CUTTKTH.START , XDATE2 + 1 , XDATE3 )
                *XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
              *CASE BETWEEN(CUTTKTH.START , XDATE3 + 1 , XDATE4)
                *XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
 
              CASE CUTTKTH.ENTERED <= XDATE1
                XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
              CASE BETWEEN(CUTTKTH.ENTERED, XDATE1 + 1 , XDATE2)
                XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
              CASE BETWEEN(CUTTKTH.ENTERED , XDATE2 + 1 , XDATE3 )
                XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
              CASE BETWEEN(CUTTKTH.ENTERED , XDATE3 + 1 , XDATE4)
                XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
              *:B803972,4 AAN Change the field name[End].
            ENDCASE
          ENDIF
          *--C102263,1 AAN Add a condition of select by complete date or start date [End]
        ENDSCAN
        *B802747,1 ABD if Company have the MF Module Made The relation else Don't.  [ Begin ]
      ENDIF
      *B802747,1 ABD [ End ]
    ELSE
      *B603326,1 Ramy [start]
      IF llPo
        SELE POSLN
        *:B803916,1 MHM 12/25/2000 change order to correct locate [start]
        lcSvORd = SET('ORDER')
        SET ORDER TO Poslns
        *:B803916,1 MHM 12/25/2000 [end]
        LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
        *:B803916,1 MHM 12/25/2000 change order to correct locate [start]
        *SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR->STATUS $ 'OH' ;
        *    .AND. TRANCD <> '3'
        *ldPODate = IIF(EMPTY(POSHDR->AVAILABLE),POSHDR->COMPLETE,POSHDR->AVAILABLE)
        *--C102263,1 AAN Add a condition of select by complete date or start date [Begin]
        *SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR->STATUS $ 'OH' ;
            .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR->AVAILABLE),POSHDR->COMPLETE,POSHDR->AVAILABLE)),;
            DTOS(ldMinComp),DTOS((ldMaxComp)))
        *:B803916,1 MHM 12/25/2000 [end]
    
        *XDATE = IIF(EMPTY(POSHDR->AVAILABLE),POSHDR->COMPLETE,;
            POSHDR->AVAILABLE)
        *--HDM B602388,1 Blank Page Appears Before the report[start]
        *@ 23,70 SAY PO
        *--HDM B602388,1 Blank Page Appears Before the report[start]
        *DO CASE
          *CASE XDATE <=XDATE1
            *XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
          *CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
            *XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
          *CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
            *XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
          *CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
            *XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
        *ENDCASE
      *ENDSCAN
      
      IF lcBuckBy == "C"
        SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
            .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),;
            DTOS(ldMinComp),DTOS((ldMaxComp)))
      *:B803916,1 MHM 12/25/2000 [end]
      
          XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,;
            POSHDR.AVAILABLE)
          *--HDM B602388,1 Blank Page Appears Before the report[start]
          *@ 23,70 SAY PO
          *--HDM B602388,1 Blank Page Appears Before the report[start]
          DO CASE
            CASE XDATE <=XDATE1
              XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
            CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
              XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
            CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
              *B606422,1 RAE WIP doesn't be printed if we change the buckets comp date.[start]
              XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
              *B606422,1 RAE [end]
            CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
              XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
          ENDCASE
        ENDSCAN
      ELSE
        SCAN WHILE IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE FOR POSHDR.STATUS $ 'OH' ;
            .AND. TRANCD <> '3' .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),;
            DTOS(ldMinStrt),DTOS((ldMaxStrt)))
        *:B803916,1 MHM 12/25/2000 [end]
      
        XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,;
          POSHDR.AVAILABLE)
        *--HDM B602388,1 Blank Page Appears Before the report[start]
        *@ 23,70 SAY PO
        *--HDM B602388,1 Blank Page Appears Before the report[start]
        DO CASE
          CASE XDATE <=XDATE1
            XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
          CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
            XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
          CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
            *B606422,1 RAE WIP doesn't be printed if we change the buckets comp date.[start]
            XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
            *B606422,1 RAE [end]
          CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
            XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
        ENDCASE
      ENDSCAN
    ENDIF
    *--C102263,1 AAN Add a condition of select by complete date or start date [End]
    *:B803916,1 MHM 12/25/2000 retrive order [start]
    SET ORDER TO &lcSvORd
    *:B803916,1 MHM 12/25/2000 [End]
  ENDIF
  *B603326,1 Ramy [end]
ENDIF

SELECT &ORDTEMP
REPLACE WIP1 WITH WIP1+XWIP1,;
  WIP2 WITH WIP2+XWIP2,;
  WIP3 WITH WIP3+XWIP3,;
  WIP4 WITH WIP4+XWIP4

SELECT STYLE
XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
*SKIP
ENDSCAN
*************
ENDIF
*:B603988,1 MHM 10/27/2000 Fix the bug of performance to increase speed [start]
*--B803972,1 AAN Remove The following line [Begin]
*SET ORDER TO &lcSavORd
*--B803972,1 AAN Remove The following line [End]
*:B603988,1 MHM 10/27/2000 Fix the bug of performance to increase speed [end]

*-----------------------------
* $$$ PRINT REPORT
*-----------------------------

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ORD940                                            DELIVERY SCHEDULE REPORT                                              PAGE 123
*MM/DD/YY                                          123456789012345678901234567890                                        HH:MM
*
*STYLE: 1234567 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*E300389,1 ASH 06/11/96 Change the word DOLLARS to AMOUNT.
*                     30 DAYS               60 DAYS               90 DAYS                OVER-120
*                     MM/DD/YY              MM/DD/YY              MM/DD/YY               M/DD/YY                 TOTAL
*CLR              PIECES   AMOUNT       PIECES   AMOUNT       PIECES   AMOUNT       PIECES   AMOUNT       PIECES   AMOUNT
*123 (+)STOCK    1234567- 12345678     1234567- 12345678-    1234567- 12345678-     1234567- 12345678-     1234567- 12345678-
*    (+)WIP      1234567-
*     AVAILABLE  1234567-
*    (-)ORDERS:
*       OPEN     1234567- 12345678
*       HOLD     1234567- 12345678
*                -------  --------
*    CAN   SHIP  1234567- 12345678
*    CAN'T SHIP  1234567- 12345678     1234567- 12345678-    1234567- 12345678-     1234567- 12345678-     1234567- 12345678-
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*E300389,1 ASH 06/11/96 Change the word DOLLARS to AMOUNT.
*:B603713,11  MHM 10/11/2000  [START]
*A = PADR(ALLTRIM(laMajSeg[lnCPos, 5]),25) + 'PIECES   AMOUNT      PIECES   AMOUNT      PIECES   AMOUNT       PIECES   AMOUNT       PIECES   AMOUNT'
A = PADR(ALLTRIM(laMajSeg[lnCPos, 5]),18) + 'PIECES     AMOUNT      PIECES     AMOUNT     PIECES       AMOUNT      PIECES      AMOUNT       PIECES      AMOUNT'
*:B603713,11  MHM 10/11/2000  [END]
SELECT &ORDTEMP
*B800424,1 TAK 01/16/95 works always by color.
*SET RELATION TO STYLE+COLOR INTO STYLE
SET RELATION TO STYLE INTO STYLE

*:B603713,11  MHM 10/11/2000  [START]
*GO TOP
LOCATE
*:B605425,1 MHM 01/27/2002 [Start]
IF EOF()
  llNoRecs = .T.
  return
ENDIF
*:B605425,1 MHM 01/27/2002 [End]

*:B603713,11  MHM 10/11/2000  [END]
XBREAK = STYLE
PAGENO = 0
ROW    = 99

* GRAND TOTALS

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

  *B800424,1 TAK 01/16/95 (Start) get the CAN SHIP and CAN'T SHIP values
  *B800424,1 and replace it into their new fields.

  *:B605425,1 MHM 01/27/2002 Fix the bug of EX size scale report not work correctlly
  *STK1 = TOTSTK
  *STK2 = STK1 + WIP1 - (O_ORD1+H_ORD1)
  *STK3 = STK2 + WIP2 - (O_ORD2+H_ORD2)
  *STK4 = STK3 + WIP3 - (O_ORD3+H_ORD3)
  
  REPLACE STK1 WITH  TOTSTK
  REPLACE STK2 WITH  STK1 + WIP1 - (O_ORD1+H_ORD1)
  REPLACE STK3 WITH  STK2 + WIP2 - (O_ORD2+H_ORD2)
  REPLACE STK4 WITH  STK3 + WIP3 - (O_ORD3+H_ORD3)
  *:B605425,1 MHM [End]

  *B606352,4 BWA 10/21/2002 Replace the values in the fields.[START]
  *IF STK2<0
  *  STK2=0
  *ENDIF
  *IF STK3<0
  *  STK3=0
  *ENDIF
  *IF STK4<0
  *  STK4=0
  *ENDIF

  IF STK2<0
    REPLACE STK2 WITH 0
  ENDIF
  IF STK3<0
    REPLACE STK3 WITH 0
  ENDIF
  IF STK4<0
    REPLACE STK4 WITH 0
  ENDIF
  *B606352,4 BWA 10/21/2002.[END]

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

  *B606352,1 RAE Print the total amount to include the subtotals amounts. [start]
  FOR I=1 TO 4
    Z=STR(I,1)
    REPLACE AYSHIP&Z WITH O_ORDAMT&Z
    REPLACE ANSHIP&Z WITH H_ORDAMT&Z
  ENDFOR
  *B606352,1 RAE [end]
  *B800424,1 TAK 01/16/95 (End)
  IF TOTSTK+TOTORD+TOTWIP = 0
    LOOP
  ELSE
    llRecExst = .T.
  ENDIF
ENDSCAN

*B606352,4 BWA 10/21/2002 Calculate the values of the same style in 1 record.[START]
IF llNewVar = "N"
  PRIVATE lcStyle
  STORE SPACE(0) TO lcStyle
  SELECT (ORDTEMP)
  SCAN
    SCATTER MEMVAR MEMO
    IF lcStyle # LEFT(STYLE,lnLenth)
      SELECT (CopyOrd)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE STYLE WITH LEFT(STYLE,lnLenth)
    ELSE
      SELECT (CopyOrd)
      =SEEK(LEFT(STYLE,lnLenth) , CopyOrd )
      REPLACE O_ORD1    WITH O_ORD1    + M.O_ORD1     ,;
              O_ORD2    WITH O_ORD2    + M.O_ORD2     ,;
              O_ORD3    WITH O_ORD3    + M.O_ORD3     ,;
              O_ORD4    WITH O_ORD4    + M.O_ORD4     ,;
              O_ORDAMT1 WITH O_ORDAMT1 + M.O_ORDAMT1  ,;
              O_ORDAMT2 WITH O_ORDAMT2 + M.O_ORDAMT2  ,;
              O_ORDAMT3 WITH O_ORDAMT3 + M.O_ORDAMT3  ,;
              O_ORDAMT4 WITH O_ORDAMT4 + M.O_ORDAMT4  ,;
              H_ORD1    WITH H_ORD1    + M.H_ORD1     ,;
              H_ORD2    WITH H_ORD2    + M.H_ORD2     ,;
              H_ORD3    WITH H_ORD3    + M.H_ORD3     ,;
              H_ORD4    WITH H_ORD4    + M.H_ORD4     ,;
              H_ORDAMT1 WITH H_ORDAMT1 + M.H_ORDAMT1  ,;
              H_ORDAMT2 WITH H_ORDAMT2 + M.H_ORDAMT2  ,;
              H_ORDAMT3 WITH H_ORDAMT3 + M.H_ORDAMT3  ,;
              H_ORDAMT4 WITH H_ORDAMT4 + M.H_ORDAMT4  ,;
              WIP1      WITH WIP1      + M.WIP1       ,;
              WIP2      WITH WIP2      + M.WIP2       ,;
              WIP3      WITH WIP3      + M.WIP3       ,;
              WIP4      WITH WIP4      + M.WIP4       ,;
              STK1      WITH STK1      + M.STK1       ,;
              STK2      WITH STK2      + M.STK2       ,;
              STK3      WITH STK3      + M.STK3       ,;
              STK4      WITH STK4      + M.STK4       ,;
              TOTSTK    WITH TOTSTK    + M.TOTSTK     ,;
              TOTORD    WITH TOTORD    + M.TOTORD     ,;
              TOTWIP    WITH TOTWIP    + M.TOTWIP     ,;
              H_TOTORDA WITH H_TOTORDA + M.H_TOTORDA  ,;
              O_TOTORDA WITH O_TOTORDA + M.O_TOTORDA  ,;
              PYSHIP1   WITH PYSHIP1   + M.PYSHIP1    ,;
              PYSHIP2   WITH PYSHIP2   + M.PYSHIP2    ,;
              PYSHIP3   WITH PYSHIP3   + M.PYSHIP3    ,;
              PYSHIP4   WITH PYSHIP4   + M.PYSHIP4    ,;
              PNSHIP1   WITH PNSHIP1   + M.PNSHIP1    ,;
              PNSHIP2   WITH PNSHIP2   + M.PNSHIP2    ,;
              PNSHIP3   WITH PNSHIP3   + M.PNSHIP3    ,;
              PNSHIP4   WITH PNSHIP4   + M.PNSHIP4    ,;
              AYSHIP1   WITH AYSHIP1   + M.AYSHIP1    ,;
              AYSHIP2   WITH AYSHIP2   + M.AYSHIP2    ,;
              AYSHIP3   WITH AYSHIP3   + M.AYSHIP3    ,;
              AYSHIP4   WITH AYSHIP4   + M.AYSHIP4    ,;
              ANSHIP1   WITH ANSHIP1   + M.ANSHIP1    ,;
              ANSHIP2   WITH ANSHIP2   + M.ANSHIP2    ,;
              ANSHIP3   WITH ANSHIP3   + M.ANSHIP3    ,;
              ANSHIP4   WITH ANSHIP4   + M.ANSHIP4 
    ENDIF
    lcStyle = LEFT(STYLE,lnLenth)
  ENDSCAN

  SELECT (ORDTEMP)
  ZAP
  APPEND FROM (gcWorkDir + CopyOrd)
  lcRpByClr = llNewVar
ENDIF
*B606352,4 BWA 10/21/2002.[END]

SELECT (ORDTEMP)
LOCATE

*B800424,1 TAK 01/16/95 (Start) Convert the colors records into style
*B800424,1 record case of report by style only the WORKTEMP file does not
*B800424,1 use in by color.
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
*-Alias WORK insted of ORDTEMP,now depends on XBYCOLOR var.
SELECT &Work
*:B603713,11  MHM 10/11/2000  [START]
*GO TOP
LOCATE
*:B603713,11  MHM 10/11/2000  [END]

*B800424,1 TAK 01/16/95 (End)

*@ 23,00
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
  *B801573,1 KHM 05/12/98 (End)

  *:B605425,1 MHM 01/27/2002 Fix the bug of EX size scale report not work correctlly
  *STK1 = TOTSTK
  *STK2 = STK1 + WIP1 - (O_ORD1+H_ORD1)
  *STK3 = STK2 + WIP2 - (O_ORD2+H_ORD2)
  *STK4 = STK3 + WIP3 - (O_ORD3+H_ORD3)
  
  REPLACE STK1 WITH  TOTSTK
  REPLACE STK2 WITH  STK1 + WIP1 - (O_ORD1+H_ORD1)
  REPLACE STK3 WITH  STK2 + WIP2 - (O_ORD2+H_ORD2)
  REPLACE STK4 WITH  STK3 + WIP3 - (O_ORD3+H_ORD3)
  *:B605425,1 MHM [End]

  *B606352,4 BWA 10/21/2002 Replace the values in the fields.[START]
  *IF STK2<0
  *  STK2=0
  *ENDIF
  *IF STK3<0
  *  STK3=0
  *ENDIF
  *IF STK4<0
  *  STK4=0
  *ENDIF

  IF STK2<0
    REPLACE STK2 WITH 0
  ENDIF
  IF STK3<0
    REPLACE STK3 WITH 0
  ENDIF
  IF STK4<0
    REPLACE STK4 WITH 0
  ENDIF
  *B606352,4 BWA 10/21/2002.[END]

  ORD1 = O_ORD1 + H_ORD1
  ORD2 = O_ORD2 + H_ORD2
  ORD3 = O_ORD3 + H_ORD3
  ORD4 = O_ORD4 + H_ORD4

  IF STYLE <> XBREAK
    XBREAK = STYLE
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW+1

    *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
    *B801573,1              header each time the row excceeds 53
    =IIF(ROW>=53,lfPrnHdr(),.F.)
    *B801573,1 KHM 05/12/98 (End)

    @ ROW,00  SAY 'STYLE: '
    @ ROW,07  SAY XBREAK
    @ ROW,25  SAY ' ' + STYLE->DESC                        &&TAK02/28/94
    @ ROW,60  SAY ' ' + 'AVG PRICE: '                      &&TAK02/28/94
    @ ROW,72  SAY AVGPRICE       PICTURE '99999.99'  &&TAK02/28/94
    ROW=ROW+2

    *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
    *B801573,1              header each time the row excceeds 53
    =IIF(ROW>=53,lfPrnHdr(),.F.)
    *B801573,1 KHM 05/12/98 (End)
  ENDIF
  *B800424,1 TAK 01/16/95 put color under condition.

  IF XBYCOLOR='Y'
    * XCOLOR  = SUBSTR(STYLE.Style,lnNonMajPo)
    *@ ROW,000 SAY XCOLOR      && HDM   COLOR
  ENDIF

  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,007 SAY '(+)STOCK'
  *@ ROW,019 SAY STK1        PICTURE '99999999'
  *@ ROW,041 SAY STK2        PICTURE '99999999'
  *@ ROW,063 SAY STK3        PICTURE '99999999'
  *@ ROW,085 SAY STK4        PICTURE '99999999'
  *@ ROW,109 SAY TOTSTK      PICTURE '99999999'

  @ ROW,003 SAY '(+)STOCK'
  @ ROW,012 SAY STK1        PICTURE '99999999'
  @ ROW,036 SAY STK2        PICTURE '99999999'
  @ ROW,060 SAY STK3        PICTURE '99999999'
  @ ROW,084 SAY STK4        PICTURE '99999999'
  @ ROW,110 SAY TOTSTK      PICTURE '99999999'
  *:B603713,11  MHM 10/11/2000  [END]
  ROW = ROW + 1

  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,007 SAY '(+)WIP'
  *@ ROW,019 SAY WIP1        PICTURE '99999999'
  *@ ROW,041 SAY WIP2        PICTURE '99999999'
  *@ ROW,063 SAY WIP3        PICTURE '99999999'
  *@ ROW,085 SAY WIP4        PICTURE '99999999'
  *@ ROW,109 SAY TOTWIP      PICTURE '99999999'
  
  @ ROW,003 SAY '(+)WIP'
  @ ROW,012 SAY WIP1        PICTURE '99999999'
  @ ROW,036 SAY WIP2        PICTURE '99999999'
  @ ROW,060 SAY WIP3        PICTURE '99999999'
  @ ROW,084 SAY WIP4        PICTURE '99999999'
  @ ROW,110 SAY TOTWIP      PICTURE '99999999'
  *:B603713,11  MHM 10/11/2000  [END]
  ROW=ROW+1

  AVL1 = STK1 + WIP1
  AVL2 = STK2 + WIP2
  AVL3 = STK3 + WIP3
  AVL4 = STK4 + WIP4
  AVLTOT = STK1+WIP1+WIP2+WIP3+WIP4

  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,007 SAY 'AVAILABLE'
  *@ ROW,019 SAY AVL1        PICTURE '99999999'
  *@ ROW,041 SAY AVL2        PICTURE '99999999'
  *@ ROW,063 SAY AVL3        PICTURE '99999999'
  *@ ROW,085 SAY AVL4        PICTURE '99999999'
  *@ ROW,109 SAY AVLTOT      PICTURE '99999999'
  
  @ ROW,003 SAY 'AVAILABLE'
  @ ROW,012 SAY AVL1        PICTURE '99999999'
  @ ROW,036 SAY AVL2        PICTURE '99999999'
  @ ROW,060 SAY AVL3        PICTURE '99999999'
  @ ROW,084 SAY AVL4        PICTURE '99999999'
  @ ROW,110 SAY AVLTOT      PICTURE '99999999'
  *:B603713,11  MHM 10/11/2000  [END]
  ROW=ROW+1

  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,007 SAY '(-)ORDERS:'
  *ROW=ROW+1
  *@ ROW,009 SAY 'OPEN'
  *@ ROW,019 SAY O_ORD1        PICTURE '99999999'
  *@ ROW,028 SAY O_ORDAMT1     PICTURE '99999999'
  *@ ROW,041 SAY O_ORD2        PICTURE '99999999'
  *@ ROW,050 SAY O_ORDAMT2     PICTURE '99999999'
  *@ ROW,063 SAY O_ORD3        PICTURE '99999999'
  *@ ROW,072 SAY O_ORDAMT3     PICTURE '99999999'
  *@ ROW,085 SAY O_ORD4        PICTURE '99999999'
  *@ ROW,094 SAY O_ORDAMT4     PICTURE '99999999'
  *@ ROW,109 SAY ORDO         PICTURE '99999999'
  *@ ROW,117 SAY O_TOTORDA      PICTURE '99999999'
  *ROW=ROW+1

  @ ROW,003 SAY '(-)ORDERS:'
  ROW=ROW+1
  @ ROW,005 SAY 'OPEN'
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
  *:B603713,11  MHM 10/11/2000  [END]
  *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
  *B801573,1              header each time the row excceeds 53
  =IIF(ROW>=53,lfPrnHdr(),.F.)
  *B801573,1 KHM 05/12/98 (End)

  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,009 SAY 'HOLD'
  *@ ROW,019 SAY H_ORD1        PICTURE '99999999'
  *@ ROW,028 SAY H_ORDAMT1     PICTURE '99999999'
  *@ ROW,041 SAY H_ORD2        PICTURE '99999999'
  *@ ROW,050 SAY H_ORDAMT2     PICTURE '99999999'
  *@ ROW,063 SAY H_ORD3        PICTURE '99999999'
  *@ ROW,072 SAY H_ORDAMT3     PICTURE '99999999'
  *@ ROW,085 SAY H_ORD4        PICTURE '99999999'
  *@ ROW,094 SAY H_ORDAMT4     PICTURE '99999999'
  *@ ROW,109 SAY ORDH         PICTURE '99999999'
  *@ ROW,118 SAY H_TOTORDA      PICTURE '99999999'
  
  @ ROW,005 SAY 'HOLD'
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
  *:B603713,11  MHM 10/11/2000  [END]
  
  *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
  *B801573,1              header each time the row excceeds 53
  =IIF(ROW>=53,lfPrnHdr(),.F.)
  *B801573,1 KHM 05/12/98 (End)

  *                 12345678- 12345678-
  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,019 SAY '-------------------'
  *@ ROW,041 SAY '-------------------'
  *@ ROW,063 SAY '-------------------'
  *@ ROW,086 SAY '-------------------'
  *@ ROW,109 SAY '-------------------'
  
  @ ROW,013 SAY '----------------------'
  @ ROW,037 SAY '----------------------'
  @ ROW,061 SAY '----------------------'
  @ ROW,085 SAY '----------------------'
  @ ROW,110 SAY '----------------------'
  *:B603713,11  MHM 10/11/2000  [END]
  ROW=ROW+1

  *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
  *B801573,1              header each time the row excceeds 53
  =IIF(ROW>=53,lfPrnHdr(),.F.)
  *B801573,1 KHM 05/12/98 (End)

  *-----------------------------------------
  * CAN/CAN'T SHIP TOTALS
  *-----------------------------------------
  *CAN/CAN'T SHIP amounts depends on fields on file.

  *B606352,4 BWA 10/21/2002 Replace the values in the fields.[START]
  *AYSHIP1 = ROUND(PYSHIP1 * AVGPRICE,0)
  *ANSHIP1 = ROUND(PNSHIP1 * AVGPRICE,0)
  *AYSHIP2 = ROUND(PYSHIP2 * AVGPRICE,0)
  *ANSHIP2 = ROUND(PNSHIP2 * AVGPRICE,0)
  *AYSHIP3 = ROUND(PYSHIP3 * AVGPRICE,0)
  *ANSHIP3 = ROUND(PNSHIP3 * AVGPRICE,0)
  *AYSHIP4 = ROUND(PYSHIP4 * AVGPRICE,0)
  *ANSHIP4 = ROUND(PNSHIP4 * AVGPRICE,0)

  REPLACE AYSHIP1 WITH ROUND(PYSHIP1 * AVGPRICE,0) ,;
          ANSHIP1 WITH ROUND(PNSHIP1 * AVGPRICE,0) ,;
          AYSHIP2 WITH ROUND(PYSHIP2 * AVGPRICE,0) ,;
          ANSHIP2 WITH ROUND(PNSHIP2 * AVGPRICE,0) ,;
          AYSHIP3 WITH ROUND(PYSHIP3 * AVGPRICE,0) ,;
          ANSHIP3 WITH ROUND(PNSHIP3 * AVGPRICE,0) ,;
          AYSHIP4 WITH ROUND(PYSHIP4 * AVGPRICE,0) ,;
          ANSHIP4 WITH ROUND(PNSHIP4 * AVGPRICE,0)
  *B606352,4 BWA 10/21/2002.[END]

  *B800424,1 TAK 01/16/95 (End)

  PYTOT = PYSHIP1 + PYSHIP2 + PYSHIP3 + PYSHIP4
  AYTOT = AYSHIP1 + AYSHIP2 + AYSHIP3 + AYSHIP4
  PNTOT = PNSHIP1 + PNSHIP2 + PNSHIP3 + PNSHIP4
  ANTOT = ANSHIP1 + ANSHIP2 + ANSHIP3 + ANSHIP4

  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,007 SAY 'CAN   SHIP'
  *@ ROW,019 SAY PYSHIP1     PICTURE '99999999'
  *@ ROW,028 SAY AYSHIP1     PICTURE '99999999'
  *@ ROW,041 SAY PYSHIP2     PICTURE '99999999'
  *@ ROW,050 SAY AYSHIP2     PICTURE '99999999'
  *@ ROW,063 SAY PYSHIP3     PICTURE '99999999'
  *@ ROW,072 SAY AYSHIP3     PICTURE '99999999'
  *@ ROW,085 SAY PYSHIP4     PICTURE '99999999'
  *@ ROW,094 SAY AYSHIP4     PICTURE '99999999'
  *@ ROW,109 SAY PYTOT       PICTURE '99999999'
  *@ ROW,118 SAY AYTOT       PICTURE '99999999'

  @ ROW,002 SAY 'CAN   SHIP'
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

  *:B603713,11  MHM 10/11/2000  [END]
  ROW=ROW+1

  *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
  *B801573,1              header each time the row excceeds 53
  =IIF(ROW>=53,lfPrnHdr(),.F.)
  *B801573,1 KHM 05/12/98 (End)

  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,007 SAY "CAN'T SHIP"
  *@ ROW,019 SAY PNSHIP1     PICTURE '99999999'
  *@ ROW,028 SAY ANSHIP1     PICTURE '99999999'
  *@ ROW,041 SAY PNSHIP2     PICTURE '99999999'
  *@ ROW,050 SAY ANSHIP2     PICTURE '99999999'
  *@ ROW,063 SAY PNSHIP3     PICTURE '99999999'
  *@ ROW,072 SAY ANSHIP3     PICTURE '99999999'
  *@ ROW,085 SAY PNSHIP4     PICTURE '99999999'
  *@ ROW,094 SAY ANSHIP4     PICTURE '99999999'
  *@ ROW,109 SAY PNTOT       PICTURE '99999999'
  *@ ROW,118 SAY ANTOT       PICTURE '99999999'

  @ ROW,002 SAY "CAN'T SHIP"
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

  *:B603713,11  MHM 10/11/2000  [END]
  ROW=ROW+1

  *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
  *B801573,1              header each time the row excceeds 53
  =IIF(ROW>=53,lfPrnHdr(),.F.)
  *B801573,1 KHM 05/12/98 (End)

  OTS1 = MAX(AVL1 - (O_ORD1+H_ORD1), 0)
  OTS2 = MAX(AVL2 - (O_ORD2+H_ORD2), 0)
  OTS3 = MAX(AVL3 - (O_ORD3+H_ORD3), 0)
  OTS4 = MAX(AVL4 - (O_ORD4+H_ORD4), 0)
  
  *:B603713,11  MHM 10/11/2000  [START]
  *@ ROW,007 SAY "OPN-TO-SEL"
  *@ ROW,019 SAY OTS1        PICTURE '99999999'
  *@ ROW,041 SAY OTS2        PICTURE '99999999'
  *@ ROW,063 SAY OTS3        PICTURE '99999999'
  *@ ROW,085 SAY OTS4        PICTURE '99999999'

  @ ROW,002 SAY "OPN-TO-SEL"
  @ ROW,012 SAY OTS1        PICTURE '99999999'
  @ ROW,036 SAY OTS2        PICTURE '99999999'
  @ ROW,060 SAY OTS3        PICTURE '99999999'
  @ ROW,084 SAY OTS4        PICTURE '99999999'
  *:B603713,11  MHM 10/11/2000  [END]
  ROW = ROW+2

  *B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
  *B801573,1              header each time the row excceeds 53
  =IIF(ROW>=53,lfPrnHdr(),.F.)
  *B801573,1 KHM 05/12/98 (End)

  *-------------------------------
  * SUM GRAND TOTALS
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

  *B800424,1 TAK 01/16/95 alias WORK insted of ORDTEMP,now depends on XBYCOLOR var.
  SELECT &Work
  SKIP
ENDDO

*----------------------
* PRINT GRAND TOTALS
*----------------------

ROW=ROW+2
@ ROW,00 SAY REPLICATE("=", 132)
ROW = ROW+1
*B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
*B801573,1              header each time the row excceeds 53
=IIF(ROW>=53,lfPrnHdr(),.F.)
*B801573,1 KHM 05/12/98 (End)

YPIECE = YPIECE1+YPIECE2+YPIECE3+YPIECE4
NPIECE = NPIECE1+NPIECE2+NPIECE3+NPIECE4
YAMT   = YAMT1+YAMT2+YAMT3+YAMT4
NAMT   = NAMT1+NAMT2+NAMT3+NAMT4

*:B603713,11  MHM 10/11/2000  [START]
*@ ROW,000 SAY '*'
*@ ROW,007 SAY 'CAN   SHIP'
*@ ROW,019 SAY YPIECE1     PICTURE '99999999'
*@ ROW,028 SAY YAMT1       PICTURE '99999999'
*@ ROW,041 SAY YPIECE2     PICTURE '99999999'
*@ ROW,050 SAY YAMT2       PICTURE '99999999'
*@ ROW,063 SAY YPIECE3     PICTURE '99999999'
*@ ROW,072 SAY YAMT3       PICTURE '99999999'
*@ ROW,085 SAY YPIECE4     PICTURE '99999999'
*@ ROW,094 SAY YAMT4       PICTURE '99999999'
*@ ROW,109 SAY YPIECE      PICTURE '99999999'
*@ ROW,118 SAY YAMT        PICTURE '99999999'

*B606352,1 RAE Print the total amount to include the subtotals amounts, and adjust the alignment. [start]
*@ ROW,000 SAY '*'
*@ ROW,002 SAY 'CAN   SHIP'
*@ ROW,012 SAY YPIECE1     PICTURE '99999999'
*@ ROW,021 SAY YAMT1       PICTURE '99999999'
*@ ROW,036 SAY YPIECE2     PICTURE '99999999'
*@ ROW,045 SAY YAMT2       PICTURE '99999999'
*@ ROW,060 SAY YPIECE3     PICTURE '99999999'
*@ ROW,069 SAY YAMT3       PICTURE '99999999'
*@ ROW,084 SAY YPIECE4     PICTURE '99999999'
*@ ROW,093 SAY YAMT4       PICTURE '99999999'
*@ ROW,110 SAY YPIECE      PICTURE '99999999'
*@ ROW,117 SAY YAMT        PICTURE '99999999'

@ ROW,000 SAY '*'
@ ROW,002 SAY 'CAN   SHIP'
@ ROW,012 SAY YPIECE1     PICTURE '99999999'
@ ROW,026 SAY YAMT1       PICTURE '99999999'
@ ROW,036 SAY YPIECE2     PICTURE '99999999'
@ ROW,050 SAY YAMT2       PICTURE '99999999'
@ ROW,060 SAY YPIECE3     PICTURE '99999999'
@ ROW,074 SAY YAMT3       PICTURE '99999999'
@ ROW,084 SAY YPIECE4     PICTURE '99999999'
@ ROW,098 SAY YAMT4       PICTURE '99999999'
@ ROW,110 SAY YPIECE      PICTURE '99999999'
@ ROW,123 SAY YAMT        PICTURE '99999999'
*B606352,1 RAE [end]
*:B603713,11  MHM 10/11/2000  [END]
ROW=ROW+1

*B801573,1 KHM 05/12/98 (Begin) Adding this check to print the
*B801573,1              header each time the row excceeds 53
=IIF(ROW>=53,lfPrnHdr(),.F.)
*B801573,1 KHM 05/12/98 (End)

*:B603713,11  MHM 10/11/2000  [START]
*@ ROW,000 SAY '*'
*@ ROW,007 SAY "CAN'T SHIP"
*@ ROW,019 SAY NPIECE1     PICTURE '99999999'
*@ ROW,028 SAY NAMT1       PICTURE '99999999'
*@ ROW,041 SAY NPIECE2     PICTURE '99999999'
*@ ROW,050 SAY NAMT2       PICTURE '99999999'
*@ ROW,063 SAY NPIECE3     PICTURE '99999999'
*@ ROW,072 SAY NAMT3       PICTURE '99999999'
*@ ROW,085 SAY NPIECE4     PICTURE '99999999'
*@ ROW,094 SAY NAMT4       PICTURE '99999999'
*@ ROW,109 SAY NPIECE      PICTURE '99999999'
*@ ROW,118 SAY NAMT        PICTURE '99999999'

*B606352,1 RAE Print the total amount to include the subtotals amounts, and adjust the alignment. [start]
*@ ROW,000 SAY '*'
*@ ROW,002 SAY "CAN'T SHIP"
*@ ROW,012 SAY NPIECE1     PICTURE '99999999'
*@ ROW,021 SAY NAMT1       PICTURE '99999999'
*@ ROW,036 SAY NPIECE2     PICTURE '99999999'
*@ ROW,045 SAY NAMT2       PICTURE '99999999'
*@ ROW,060 SAY NPIECE3     PICTURE '99999999'
*@ ROW,069 SAY NAMT3       PICTURE '99999999'
*@ ROW,084 SAY NPIECE4     PICTURE '99999999'
*@ ROW,093 SAY NAMT4       PICTURE '99999999'
*@ ROW,110 SAY NPIECE      PICTURE '99999999'
*@ ROW,117 SAY NAMT        PICTURE '99999999'

@ ROW,000 SAY '*'
@ ROW,002 SAY "CAN'T SHIP"
@ ROW,012 SAY NPIECE1     PICTURE '99999999'
@ ROW,026 SAY NAMT1       PICTURE '99999999'
@ ROW,036 SAY NPIECE2     PICTURE '99999999'
@ ROW,050 SAY NAMT2       PICTURE '99999999'
@ ROW,060 SAY NPIECE3     PICTURE '99999999'
@ ROW,074 SAY NAMT3       PICTURE '99999999'
@ ROW,084 SAY NPIECE4     PICTURE '99999999'
@ ROW,098 SAY NAMT4       PICTURE '99999999'
@ ROW,110 SAY NPIECE      PICTURE '99999999'
@ ROW,123 SAY NAMT        PICTURE '99999999'
*B606352,1 RAE [end]
*:B603713,11  MHM 10/11/2000  [END]

ROW=ROW+1


*DO ENDPRT

*B601038,1 MFM 04/24/96 (Begin) Close data at this level to privent
*B601038,1 MFM          causing of bug(s) of undifined filter variables
*B601038,1 MFM          when closing data files at the menu(GFDRV) level.
*=gfclsdata()
*B601038,1 MFM 04/24/96 (End).
*SET DEVICE TO SCREEN
*RETURN

*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/12/1998
*! Purpose   : To print the report header
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
*B801573,1 KHM 05/12/98 Added
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO+1

*:C102263,1 AAN Add subtitle to th ereport [Begin].
*DO RPT_HDR WITH XREPORT,'',R_WIDTH
lcType = IIF(lcBuckBy = "S",'By Start Date','By Completion Date')
DO RPT_HDR WITH XREPORT,lcType,R_WIDTH
*:C102263,1 AAN Add subtitle to th ereport [Begin].

@ 05,00  SAY 'STYLE: '
@ 05,07  SAY STYLE
@ 05,25  SAY ' '+STYLE->DESC
@ 05,60  SAY 'AVG PRICE: '
@ 05,72  SAY AVGPRICE     PICTURE '99999.99'

*:B603713,11  MHM 10/11/2000  [START]
*@ 07,24  SAY XDATE1
*@ 07,47  SAY XDATE2
*@ 07,68  SAY XDATE3
*@ 07,91  SAY XDATE4
*@ 07,116 SAY 'TOTAL'
*@ 08,00  SAY A

@ 07,17  SAY XDATE1
@ 07,40  SAY XDATE2
@ 07,62  SAY XDATE3
@ 07,86  SAY XDATE4
@ 07,114 SAY 'TOTAL'
@ 08,00  SAY A
*:B603713,11  MHM 10/11/2000  [END]
ROW=9

*-----------------------
*   END ORD940.PRG
*-----------------------
*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Hossam El Etreby
*! Date      : 12/29/98
*! Purpose   : control browsing primary fabric and validate
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
*! Due to B602439
*!*************************************************************

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
    *--B803972,1 AAN Change "GO TOP" with "LOCATE"[Begin]
    *GO TOP IN FABRIC
    LOCATE
    *--B803972,1 AAN Change "GO TOP" with "LOCATE"[End]    
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
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Hossab El Etreby
*! Date      : 03/01/1999
*! Purpose   : To set relation on or off when running the in range function
*!             in the option grid.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
*! Due to B602439
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Hossam El Etreby[HDM]
*! Date      : 12/29/98
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
*! Due to B602439
*!*************************************************************



FUNCTION lfFabSum
LPARAMETERS lcFab, lcComp
LOCAL lnTotcomp,  lnAlias
lnTotcomp = 0

  IF SEEK(ALLTRIM(lcFab), lcTmpFab)
    SUM &lcTmpFab..&lcCOMP. TO lnTotcomp WHILE &lcTmpFab..Fabric = lcFab
  ENDIF

RETURN lnTotcomp


FUNCTION _lfFabSum
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
FUNCTION lfvCompDat

*IF OCCURS('|',laOGFXFlt[lnCompPos,6]) > 3
*  WAIT WINDOW 'Only 4 Dates values are available...'
*ENDIF


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

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************

FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ oAriaApplication.CompanyInstalledModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
*-- end of lfvEdiOrd.

*!**************************************************************************
*! Name      : lfvClear
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 04/29/2001
*! Purpose   : To update O.G.
*!**************************************************************************
*! Example   : =lfvClear()
*!**************************************************************************
*-- Refer to C102263,1
FUNCTION lfvClear
CLEARREAD()
*-- end of lfvClear.


*--
FUNCTION lfOthrColr
*IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) 
IF STYLE <> XSTYLE
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
    *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19))
    IF STYLE <> XSTYLE
      STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
                 XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
                 XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
                 XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
                XWIP1,XWIP2,XWIP3,XWIP4
    ENDIF

*!*      iF lcRpByClr = 'Y'
      XSTYLE = STYLE
*!*      ELSE
*!*        XSTYLE = PADR(SUBSTR(STYLE,1,lnMajLen),19)
*!*      ENDIF  
    lcLopedSty = lcLopedSty + '|' + XSTYLE
    WAIT WINDOW 'Calculating Cut&Sold info for ' + lcStyTitle + ' ' + XSTYLE NOWAIT
    XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
    XTOTSTK = 0
    XKEY    = STYLE
    XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
    *-------------------------
    * LOOP ORDER LINE FILE
    *-------------------------
    SELECT(lcTmpWork)
    IF !SEEK( XKEY )
       APPEND BLANK
       REPLACE STYLE WITH XSTYLE,;
              COLOR  WITH XCOLOR,;
              desc   with style.Desc
    ENDIF
    REPLACE TOTSTK WITH TOTSTK + XTOTSTK
    SELECT(lcTmpWork)
    REPLACE O_ORD1     WITH O_ORD1+XO_ORD1       ,;
            O_ORDAMT1  WITH O_ORDAMT1+XO_ORDAMT1 ,;
            H_ORD1     WITH H_ORD1+XH_ORD1       ,;
            H_ORDAMT1  WITH H_ORDAMT1+XH_ORDAMT1 ,;
            O_ORD2     WITH O_ORD2+XO_ORD2       ,;
            O_ORDAMT2  WITH O_ORDAMT2+XO_ORDAMT2 ,;
            H_ORD2     WITH H_ORD2+XH_ORD2       ,;
            H_ORDAMT2  WITH H_ORDAMT2+XH_ORDAMT2 ,;
            O_ORD3     WITH O_ORD3+XO_ORD3       ,;
            O_ORDAMT3  WITH O_ORDAMT3+XO_ORDAMT3 ,;
            H_ORD3     WITH H_ORD3+XH_ORD3       ,;
            H_ORDAMT3  WITH H_ORDAMT3+XH_ORDAMT3 ,;
            O_ORD4     WITH O_ORD4+XO_ORD4       ,;
            O_ORDAMT4  WITH O_ORDAMT4+XO_ORDAMT4 ,;
            H_ORD4     WITH H_ORD4+XH_ORD4       ,;
            H_ORDAMT4  WITH H_ORDAMT4+XH_ORDAMT4
    
            ****** [PO] PROCESS P/O FILE
            IF llCutTkt OR llPo
              SELE POSLN
              lcSvORd = ORDER()
*              SET('ORDER')
              lodbfPOSLN.setorder('Poslns')
              *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19))
              loDBFPOSLN.Seek('0001' + XSTYLE)
*              LOCATE FOR IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE
              IF lcBuckBy == "C"
              *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19)) 
              *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	                SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+XSTYLE ;
*!*	                  FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' ;
*!*	                     .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),;
*!*	                     DTOS(ldMinComp),DTOS((ldMaxComp)))
*!*	XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)
           *:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]
*!*                  SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+XSTYLE ;
*!*                    FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
*!*        			      XDATE = POSHDR.COMPLETE
*!*                *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[END]
*!*                       
*!*                    
*!*                    DO CASE
*!*                      CASE XDATE <=XDATE1
*!*                        XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*                        XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*                        XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*                        XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    ENDCASE
*!*                  ENDSCAN
                DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+XSTYLE ;
                      FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                      
                  lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                  STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                  XDATE = POSLN.COMPLETE
                   
                  SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+XSTYLE+lcPoLine ;
                      FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                    FOR lnCount = 1 TO 8
                      lcCount = STR(lnCount,1)
                      XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount)
                    ENDFOR
                  ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                  DO CASE
                    CASE XDATE <=XDATE1
                      XWIP1 = XWIP1 + XPOWIP
                    CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                      XWIP2 = XWIP2 + XPOWIP
                    CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                      XWIP3 = XWIP3 + XPOWIP
                    CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                      XWIP4 = XWIP4 + XPOWIP
                  ENDCASE
                ENDDO
                *:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]                
              ELSE
              *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19))
              *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	                SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+ XSTYLE FOR  TRANCD <> '3' AND ;
*!*	                          lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' ;
*!*	                         .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),;
*!*	                         DTOS(ldMinStrt),DTOS((ldMaxStrt)))
*!*	                  XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,POSHDR.AVAILABLE)
              *:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]                
*!*                SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+ XSTYLE FOR  TRANCD <> '3' AND ;
*!*                            lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
*!*                    XDATE = POSHDR.Start
*!*                *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[End]                  
*!*                    DO CASE
*!*                      CASE XDATE <=XDATE1
*!*                        XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*                        XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*                        XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*                        XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    ENDCASE
*!*                  ENDSCAN
                DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+ XSTYLE FOR  TRANCD <> '3' AND ;
                            lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                   XDATE = POSHDR.Start
                   lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                   STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                   SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+XSTYLE+lcPoLine ;
                      FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                     FOR lnCount = 1 TO 8
                       lcCount = STR(lnCount,1)
                       XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount )
                     ENDFOR
                   ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                   DO CASE
                     CASE XDATE <=XDATE1
                       XWIP1 = XWIP1 + XPOWIP
                     CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                       XWIP2 = XWIP2 + XPOWIP
                     CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                       XWIP3 = XWIP3 + XPOWIP
                     CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                       XWIP4 = XWIP4 + XPOWIP
                   ENDCASE                   
                ENDDO
              *:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]                  
              ENDIF
              lodbfPOSLN.setorder(lcSvORd)
*              SET ORDER TO &lcSvORd
            ENDIF
          ENDIF
          SELECT(lcTmpWork)
          REPLACE WIP1 WITH WIP1+XWIP1 ,;
                  WIP2 WITH WIP2+XWIP2 ,;
                  WIP3 WITH WIP3+XWIP3 ,;
                  WIP4 WITH WIP4+XWIP4

          SELECT STYLE
          XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))


*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/18/2004
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

lcTages1 = lcTages
lcIndex1 = lcIndex
SELECT (lcCursor)
DO WHILE AT("|", lcIndex1,1) <> 0
  lcIndex  = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
  lcIndex1 = STRTRAN(lcIndex1, lcIndex + "|", "", 1, 1)
  lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
  lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
  *B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
  INDEX ON &lcIndex. TAG (lcTages)
  *B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.
*--End of lfCreateIndecies()
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/22/2006
*! Purpose   : Collect Data
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCollectData

LowDate = ""
HighDate = ""
IF lcBuckBy == "C"
  lnStartPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'ORDLINE.START'),1)
  LowDate  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnStartPos ,6],1,10))
  HighDate= CTOD(SUBSTR(laOGFxFlt[lnStartPos ,6],12,21))
ELSE
  lnCompPos  = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'ORDLINE.COMPLETE'),1)
  LowDate  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnCompPos,6],1,10))
  HighDate= CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnCompPos ,6],12,21))
ENDIF

llUseStyle = .F.
llUseDiv   = .F.
llUseSea   = .F.
llUseGrp   = .F.
llUseFab   = .F.
llUseClr   = .F.

*--Style Filter
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'PADR(SUBSTR(ORDLINE.STYLE,lnMajPos,lnMajLen),19)'),1)
IF lnPosition > 0
  lcStylFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
IF llUseStyle 
  SELECT(lcStylFile)
  LOCATE 
  IF EOF()
    llUseStyle = .F.
  ENDIF 
ENDIF 

*---Division Filter
lnDivPos = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.CDIVISION'),1)
IF lnDivPos  > 0
  lcDivStr = LOOGSCROLL.laOGFxFlt[lnDivPos,6]
  lcDivFile = loOGScroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcDivFile)
ENDIF

*---Season Filter
lnSeaPos = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.SEASON'),1)
IF lnSeaPos > 0
  lcSeaStr = LOOGSCROLL.laOGFxFlt[lnSeaPos,6]
  lcSeaFile = loOGScroll.gfTempName()
  llUseSea = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcSeaFile)
ENDIF

*--Style Group
lnGrpPos = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnGrpPos > 0
  lcGrpStr = LOOGSCROLL.laOGFxFlt[lnGrpPos,6]
  lcGrpFile = loOGScroll.gfTempName()
  llUseGrp = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcGrpFile)
ENDIF

*--Style Fabric
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseFab = IIF(!EMPTY(lcFabFile) .AND. USED(lcFabFile) .AND. RECCOUNT(lcFabFile)>0,.T.,.F.)
ENDIF
IF llUseFab 
  SELECT(lcFabFile)
  LOCATE 
  IF EOF()
    llUseFab = .F.
  ENDIF 
ENDIF 


*--Style Color
lnClrPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))'),1)
IF lnClrPos > 0
  lcClrStr = LOOGSCROLL.laOGVRFlt[lnClrPos,6]
  lcClrFile = loOGScroll.gfTempName()
  llUseClr = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'CSTYCLR',lcClrFile)
ENDIF

*--Scale
IF llUseExSzScl
  llUseScale = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.SCALE'),1)
  IF lnPosition > 0
    lcScalFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
    llUseScale = IIF(!EMPTY(lcScalFile) .AND. USED(lcScalFile) .AND. RECCOUNT(lcScalFile)>0,.T.,.F.)
  ENDIF
  IF llUseScale  
    SELECT(lcScalFile)
    LOCATE 
    IF EOF()
      llUseScale = .F.
    ENDIF 
  ENDIF 
ENDIF 

*--Pattern
lcPattStr = ""
lnPattPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.PATTERN'),1)
IF lnPattPos > 0
  lcPattStr = LOOGSCROLL.laOGVRFlt[lnPattPos,6]
ENDIF

llUseStatus = .F.
lcSatStr  = ""
lnStatPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.STATUS'),1)
IF lnStatPos > 0
  lcSatStr = LOOGSCROLL.laOGVRFlt[lnStatPos,6]
  lcSatStr = IIF(!EMPTY(lcSatStr),"Inlist(STYLE.STATUS,'"+STRTRAN('|',lcSatStr ,"','")+"')","")
  llUseStatus  = !EMPTY(lcSatStr)
ENDIF

XSTYLE = SPACE(19)
XCOLOR = SPACE(6)
HSTYLE = SPACE(6)


*lcRpEdiFlt loDBFStyle.Seek(&lcStylFile..CSTYMAJOR,'CSTYLE')
lcLopedSty = ''
IF llUseStyle
  SELECT(lcStylFile)
  SCAN
    IF loDBFOrdline.SEEK(LEFT(cStyMajor,lnMajLen),"ORDLINES")  
    *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19))
      SELECT OrdLine
      *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	      SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(lineno,6) = LEFT(&lcStylFile..Cstymajor ,lnMajLen);
*!*	                FOR loDBFStyle.Seek(ordline.STYLE,'STYLE') AND  IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
*!*	                AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
*!*	                IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
*!*	                IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),lcClrFile),.T.) AND ;
*!*	                IIF(!EMPTY(lcPattStr), STYLE.PATTERN = lcPattStr,.T.);
*!*	                AND !(STYLE $ lcLopedSty) AND IIF(llUseStatus ,EVALUATE(lcSatStr) ,.T.);
*!*	                AND IIF(lcBuckBy == "C",BETWEEN(ORDLINE.START,LowDate  ,HighDate),BETWEEN(ORDLINE.COMPLETE,LowDate  ,HighDate)) AND ;
*!*	                loDBFordhdr.Seek('O' + ordline.ORDER) AND IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.) AND ;
*!*	                IIF(lcBuckBy == "C",BETWEEN(OrdLine.COMPLETE,ldMinComp,ldMaxComp),BETWEEN(OrdLine.START,ldMinStrt,ldMaxStrt));
*!*	                and IIF(llUseExSzScl AND llUseScale ,SEEK(Style.scale,lcScalFile),.T. )
      SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(lineno,6) = LEFT(&lcStylFile..Cstymajor ,lnMajLen);
                FOR loDBFStyle.Seek(ordline.STYLE,'STYLE') AND  IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
                IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
                IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),lcClrFile),.T.) AND ;
                IIF(!EMPTY(lcPattStr), STYLE.PATTERN = lcPattStr,.T.);
                AND !(STYLE $ lcLopedSty) AND IIF(llUseStatus ,EVALUATE(lcSatStr) ,.T.);
                AND IIF(lcBuckBy == "C",BETWEEN(ORDLINE.START,LowDate  ,HighDate),BETWEEN(ORDLINE.COMPLETE,LowDate  ,HighDate)) AND ;
                loDBFordhdr.Seek('O' + ordline.ORDER) AND IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.) AND ;
                IIF(lcBuckBy == "C",OrdLine.COMPLETE <=ldMaxComp,OrdLine.START <=ldMaxStrt);
                and IIF(llUseExSzScl AND llUseScale ,SEEK(Style.scale,lcScalFile),.T. )
      *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[End]


                
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
        *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) 
        IF STYLE <> XSTYLE
          STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
                     XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
                     XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
                     XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
                     XWIP1,XWIP2,XWIP3,XWIP4
        ENDIF
  
*!*          IF lcRpByClr = 'Y'
          XSTYLE = STYLE
*!*          ELSE
*!*            XSTYLE = PADR(SUBSTR(STYLE,1,lnMajLen),19)
*!*          ENDIF  
        lcLopedSty = lcLopedSty + '|' + XSTYLE
        WAIT WINDOW 'Calculating Cut&Sold info for ' + lcStyTitle + ' ' + XSTYLE NOWAIT
        XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
        XTOTSTK = 0
        XKEY    = STYLE
        XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
        SELECT(lcTmpWork)
        IF !SEEK(XKEY)
          APPEND BLANK
          REPLACE STYLE  WITH XSTYLE,;
                  COLOR  WITH XCOLOR,;
                  desc   with style.Desc
        ENDIF
        REPLACE TOTSTK WITH TOTSTK + XTOTSTK
        SELECT ORDLINE
        lnLineRec = RECNO()
        lnRecNo = IIF(EOF(),0,RECNO())
        *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) = XSTYLE 
        SCAN REST WHILE STYLE= XSTYLE 
          lnRecNo = IIF(EOF(),0,RECNO())
          loDBFordhdr.Seek('O' + ordline.ORDER) 
          IF ORDHDR->STATUS $ 'CX' .OR. TOTQTY <= 0
            LOOP
          ENDIF
          XORDER = ORDER
          IF lcBuckBy == "C"
            DO CASE
              CASE ORDHDR.COMPLETE <= XDATE1
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
              CASE ORDHDR.START <= XDATE1
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
         SELECT(lcTmpWork)
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

        IF llPo OR llCutTkt
          SELECT POSLN
          lcSvORd = ORDER()
          loDBFPOSLN.Setorder('Poslns')
          *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19))
          loDBFPOSLN.Seek('0001' + XSTYLE)
          IF lcBuckBy == "C"
          *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19)) 
          *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	            SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = ;
*!*	                       '0001' + XSTYLE ;
*!*	                 FOR TRANCD <> '3' AND ;
*!*	                     lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' .AND. ;
*!*	                     BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),DTOS(ldMinComp),DTOS((ldMaxComp)))
*!*	                XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)

            
            *:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]     
*!*              SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = ;
*!*                         '0001' + XSTYLE ;
*!*                   FOR TRANCD <> '3' AND ;
*!*                       lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' 
*!*                       
*!*                  XDATE = POSHDR.COMPLETE
*!*                *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[END]
*!*                

*!*                  DO CASE
*!*                    CASE XDATE <=XDATE1
*!*                      XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*                      XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*                      XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*                      XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                  ENDCASE
*!*                ENDSCAN
              DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE ;
                 FOR TRANCD <> '3' AND ;
                     lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' 
                 XDATE = POSLN.COMPLETE      
                 lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                 STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                 SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE +lcPoLine ;
                    FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                   FOR lnCount = 1 TO 8
                    lcCount = STR(lnCount,1)
                     XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount )
                   ENDFOR
                 ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                 DO CASE
                   CASE XDATE <=XDATE1
                     XWIP1 = XWIP1 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                     XWIP2 = XWIP2 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                     XWIP3 = XWIP3 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                     XWIP4 = XWIP4 + XPOWIP
                 ENDCASE                                    
              ENDDO       
              *:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]     
              
            ELSE
*            IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19))
              *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	              SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = ;
*!*	                       '0001' + XSTYLE ;
*!*	                 FOR TRANCD <> '3' AND ;
*!*	                     lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' .AND. ;
*!*	                     BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),DTOS(ldMinComp),DTOS((ldMaxComp)))
*!*	            
*!*	                XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,POSHDR.AVAILABLE)
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]     
*!*                SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = ;
*!*                         '0001' + XSTYLE ;
*!*                   FOR TRANCD <> '3' AND ;
*!*                       lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' 
*!*              
*!*                  XDATE = POSHDR.Start
*!*                *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[End]
*!*                  DO CASE
*!*                    CASE XDATE <=XDATE1
*!*                      XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*                      XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*                      XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*                      XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                  ENDCASE
*!*                ENDSCAN
              DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE ;
                 FOR TRANCD <> '3' AND ;
                     lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' 
                 XDATE = POSHDR.Start    
                 lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                 STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                 SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE +lcPoLine ;
                    FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                   FOR lnCount = 1 TO 8
                     lcCount = STR(lnCount,1)
                     XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount )
                   ENDFOR
                 ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                 DO CASE
                   CASE XDATE <=XDATE1
                     XWIP1 = XWIP1 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                     XWIP2 = XWIP2 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                     XWIP3 = XWIP3 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                     XWIP4 = XWIP4 + XPOWIP
                 ENDCASE                                    
              ENDDO   
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]             
            ENDIF
         ENDIF 

         loDBFPOSLN.setorder(lcSvORd)
         SELECT(lcTmpWork)
         REPLACE WIP1 WITH WIP1 + XWIP1 ,;
                 WIP2 WITH WIP2 + XWIP2 ,;
                 WIP3 WITH WIP3 + XWIP3 ,;
                 WIP4 WITH WIP4 + XWIP4
          SELECT STYLE
          XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
       ENDSCAN           
*!*         IF lcRpByClr = 'Y
  
         SELECT STYLE
         LOCATE
         SCAN REST WHILE STYLE =  "" FOR LEFT(STYLE.STYLE,lnMajLen) = LEFT(EVAL(lcStylFile+'.CSTYMAJOR'),lnMajLen) AND ;
                         IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),lcClrFile),.T.)
           IF !(STYLE.STYLE $ lcLopedSty)
             =lfOthrColr()
           ENDIF
         ENDSCAN
*!*        ENDIF
    ELSE 
      SELECT STYLE
      lodbfStyle.SEEK(LEFT(&lcStylFile..cStyMajor,lnMajLen),"STYLE")
      lcStyStr = LEFT(&lcStylFile..cStyMajor,lnMajLen)
      SCAN REST WHILE STYLE = lcStyStr
      *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19))
        IF STYLE <> XSTYLE
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
          *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19))
          IF STYLE <> XSTYLE
            STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1,;
                       XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2,;
                       XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3,;
                       XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4,;
                       XWIP1,XWIP2,XWIP3,XWIP4
          ENDIF
*!*            IF lcRpByClr = 'Y'
            XSTYLE = STYLE
*!*            ELSE
*!*              XSTYLE = PADR(SUBSTR(STYLE,1,lnMajLen),19)
*!*            ENDIF  
          lcLopedSty = lcLopedSty + '|' + XSTYLE

          WAIT WINDOW 'Calculating Cut&Sold info for ' + lcStyTitle + ' ' + XSTYLE NOWAIT
          XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
          XTOTSTK = 0
          XKEY    = STYLE
          XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
              
          SELECT(lcTmpWork)
          IF !SEEK( XKEY )
            APPEND BLANK
            REPLACE STYLE  WITH XSTYLE,;
                    COLOR  WITH XCOLOR ,;
                    desc   with style.Desc
          ENDIF
          REPLACE TOTSTK WITH TOTSTK + XTOTSTK
          SELECT(lcTmpWork)
          REPLACE O_ORD1     WITH O_ORD1+XO_ORD1       ,;
                  O_ORDAMT1  WITH O_ORDAMT1+XO_ORDAMT1 ,;
                  H_ORD1     WITH H_ORD1+XH_ORD1       ,;
                  H_ORDAMT1  WITH H_ORDAMT1+XH_ORDAMT1 ,;
                  O_ORD2     WITH O_ORD2+XO_ORD2       ,;
                  O_ORDAMT2  WITH O_ORDAMT2+XO_ORDAMT2 ,;
                  H_ORD2     WITH H_ORD2+XH_ORD2       ,;
                  H_ORDAMT2  WITH H_ORDAMT2+XH_ORDAMT2 ,;
                  O_ORD3     WITH O_ORD3+XO_ORD3       ,;
                  O_ORDAMT3  WITH O_ORDAMT3+XO_ORDAMT3 ,;
                  H_ORD3     WITH H_ORD3+XH_ORD3       ,;
                  H_ORDAMT3  WITH H_ORDAMT3+XH_ORDAMT3 ,;
                  O_ORD4     WITH O_ORD4+XO_ORD4       ,;
                  O_ORDAMT4  WITH O_ORDAMT4+XO_ORDAMT4 ,;
                  H_ORD4     WITH H_ORD4+XH_ORD4       ,;
                  H_ORDAMT4  WITH H_ORDAMT4+XH_ORDAMT4


            IF llPo OR llCutTkt
              SELE POSLN
              lcSvORd = ORDER()
              lodbfPOSLN.SETORDER('Poslns')
              *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(STYLE,1,XSTYLE),19))
              lodbfPOSLN.Seek('0001'+XSTYLE)
              IF lcBuckBy == "C"
              *IIF(lcRpByClr = 'Y',XSTYLE ,PADR(SUBSTR(XSTYLE ,1,lnMajLen),19))  
              *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	                SCAN WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+ STR(lineno,6)+ trancd = ;
*!*	                          '0001'+XSTYLE   ;
*!*	                          FOR TRANCD <> '3'  AND lodbfPOSHDR.seek(POsln.cbusdocu+ POsln.cstytype+ POsln.po) AND POSHDR.STATUS $ 'OH' ;
*!*	                         .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),;
*!*	                          DTOS(ldMinComp),DTOS((ldMaxComp)))
*!*	                  XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]             
*!*                  SCAN WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+ STR(lineno,6)+ trancd = ;
*!*                            '0001'+XSTYLE   ;
*!*                            FOR TRANCD <> '3'  AND lodbfPOSHDR.seek(POsln.cbusdocu+ POsln.cstytype+ POsln.po) AND POSHDR.STATUS $ 'OH'
*!*                            
*!*                    XDATE = POSHDR.COMPLETE
*!*                *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[End]
*!*                    DO CASE
*!*                      CASE XDATE <=XDATE1
*!*                        XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*                        XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*                        XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*                        XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    ENDCASE
*!*                  ENDSCAN
                DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+ XSTYLE FOR  TRANCD <> '3' AND ;
                            lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                   XDATE = POSLN.COMPLETE
                   lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                   STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                   SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001'+XSTYLE+lcPoLine ;
                      FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                     FOR lnCount = 1 TO 8
                       lcCount = STR(lnCount,1)
                       XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount )
                     ENDFOR
                   ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                   DO CASE
                     CASE XDATE <=XDATE1
                       XWIP1 = XWIP1 + XPOWIP
                     CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                       XWIP2 = XWIP2 + XPOWIP
                     CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                       XWIP3 = XWIP3 + XPOWIP
                     CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                       XWIP4 = XWIP4 + XPOWIP
                   ENDCASE                   
                ENDDO
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]                             
              ELSE
              *IIF(lcRpByClr = 'Y',XSTYLE ,PADR(SUBSTR(XSTYLE ,1,lnMajLen),19))  
              *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	                SCAN WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+ STR(lineno,6)+ trancd = ;
*!*	                          '0001'+XSTYLE   ;
*!*	                          FOR TRANCD <> '3' AND lodbfPOSHDR.seek(POsln.cbusdocu+ POsln.cstytype+ POsln.po) AND  POSHDR.STATUS $ 'OH' ;
*!*	                        .AND. BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),;
*!*	                     DTOS(ldMinStrt),DTOS((ldMaxStrt)))
*!*	                  XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,POSHDR.AVAILABLE)
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]             
*!*                  SCAN WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+ STR(lineno,6)+ trancd = ;
*!*                            '0001'+XSTYLE   ;
*!*                            FOR TRANCD <> '3' AND lodbfPOSHDR.seek(POsln.cbusdocu+ POsln.cstytype+ POsln.po) AND  POSHDR.STATUS $ 'OH' 
*!*                    XDATE = POSHDR.Start
*!*                *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[ENd]
*!*                    DO CASE
*!*                      CASE XDATE <=XDATE1
*!*                        XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*                        XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*                        XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                      CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*                        XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*                    ENDCASE
*!*                  ENDSCAN

            DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE ;
                 FOR TRANCD <> '3' AND ;
                     lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' 
                 XDATE = POSHDR.Start
                 lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                 STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                 SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE +lcPoLine ;
                    FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                   FOR lnCount = 1 TO 8
                     lcCount = STR(lnCount,1)
                     XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount )
                   ENDFOR
                 ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                 DO CASE
                   CASE XDATE <=XDATE1
                     XWIP1 = XWIP1 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                     XWIP2 = XWIP2 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                     XWIP3 = XWIP3 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                     XWIP4 = XWIP4 + XPOWIP
                 ENDCASE                                    
              ENDDO       
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]                          
              ENDIF 
              lodbfPOSLN.SETORDER(lcSvORd)
*              SET ORDER TO &lcSvORd
            ENDIF
          ENDIF

          SELECT(lcTmpWork)
          REPLACE WIP1 WITH WIP1+XWIP1 ,;
                  WIP2 WITH WIP2+XWIP2 ,;
                  WIP3 WITH WIP3+XWIP3 ,;
                  WIP4 WITH WIP4+XWIP4

          SELECT STYLE
          XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
      ENDSCAN
    ENDIF 
  ENDSCAN 
ELSE
*IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) 
*:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	   SELECT ORDLINE
*!*	   SCAN  FOR lodbfStyle.Seek(ordline.Style,'Style') AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
*!*	              AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
*!*	              IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
*!*	              IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),lcClrFile),.T.) AND ;
*!*	              IIF(!EMPTY(lcPattStr), STYLE.PATTERN = lcPattStr,.T.);
*!*	              AND !(STYLE $ lcLopedSty) AND IIF(llUseStatus ,EVALUATE(lcSatStr) ,.T.);
*!*	              AND IIF(lcBuckBy == "C",BETWEEN(ORDLINE.START,LowDate  ,HighDate),BETWEEN(ORDLINE.COMPLETE,LowDate  ,HighDate))  ;
*!*	              AND loDBFordhdr.Seek('O' + ordline.ORDER) AND IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.) AND ;
*!*	              IIF(lcBuckBy == "C",BETWEEN(OrdLine.COMPLETE,ldMinComp,ldMaxComp),BETWEEN(OrdLine.START,ldMinStrt,ldMaxStrt));
*!*	              AND IIF(llUseExSzScl AND llUseScale ,SEEK(Style.scale,lcScalFile),.T. )
   SELECT ORDLINE
   SCAN  FOR lodbfStyle.Seek(ordline.Style,'Style') AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
              AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
              IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
              IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),lcClrFile),.T.) AND ;
              IIF(!EMPTY(lcPattStr), STYLE.PATTERN = lcPattStr,.T.);
              AND !(STYLE $ lcLopedSty) AND IIF(llUseStatus ,EVALUATE(lcSatStr) ,.T.);
              AND IIF(lcBuckBy == "C",BETWEEN(ORDLINE.START,LowDate  ,HighDate),BETWEEN(ORDLINE.COMPLETE,LowDate  ,HighDate))  ;
              AND loDBFordhdr.Seek('O' + ordline.ORDER) AND IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.) AND ;
              IIF(lcBuckBy == "C",OrdLine.COMPLETE<=ldMaxComp,OrdLine.START<=ldMaxStrt);
              AND IIF(llUseExSzScl AND llUseScale ,SEEK(Style.scale,lcScalFile),.T. )
 *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[END]

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
    *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19))
    IF STYLE <> XSTYLE
      STORE 0 TO XO_ORD1,XO_ORDAMT1,XH_ORD1,XH_ORDAMT1 ,;
                 XO_ORD2,XO_ORDAMT2,XH_ORD2,XH_ORDAMT2 ,;
                 XO_ORD3,XO_ORDAMT3,XH_ORD3,XH_ORDAMT3 ,;
                 XO_ORD4,XO_ORDAMT4,XH_ORD4,XH_ORDAMT4 ,;
                 XWIP1,XWIP2,XWIP3,XWIP4
    ENDIF
  
*!*      IF lcRpByClr = 'Y'
      XSTYLE = STYLE
*!*      ELSE
*!*        XSTYLE = PADR(SUBSTR(STYLE,1,lnMajLen),19)
*!*      ENDIF
  
    lcLopedSty = lcLopedSty + '|' + XSTYLE

    WAIT WINDOW 'Calculating Cut&Sold info for ' + lcStyTitle + ' ' + XSTYLE NOWAIT

    XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
    XTOTSTK = 0
    XKEY    = STYLE

    XTOTSTK = IIF(TOTSTK > 0, TOTSTK, 0)
    
    *-------------------------
    * LOOP ORDER LINE FILE
    *---------------------- ---
    SELECT(lcTmpWork)
    IF !SEEK( XKEY )
      APPEND BLANK
      REPLACE STYLE  WITH XSTYLE,;
              COLOR  WITH XCOLOR,;
              desc   with style.Desc
    ENDIF
    REPLACE TOTSTK WITH TOTSTK + XTOTSTK

    SELECT ORDLINE
    lnLineRec = RECNO()
    SELECT ORDLINE

    lnRecNo = IIF(EOF(),0,RECNO())
    *IIF(lcRpByClr = 'Y',STYLE,PADR(SUBSTR(STYLE,1,lnMajLen),19)) 
    SCAN REST WHILE STYLE = XSTYLE
      loDBFordhdr.Seek('O' + ordline.ORDER) 
      lnRecNo = IIF(EOF(),0,RECNO())
      IF ORDHDR->STATUS $ 'CX' .OR. TOTQTY <= 0
        LOOP
      ENDIF
      XORDER = ORDER

      IF lcBuckBy == "C"
        DO CASE
          CASE ORDHDR.COMPLETE <= XDATE1
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
          CASE ORDHDR.START <= XDATE1
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
  
    SELECT(lcTmpWork)
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
  
      IF llPo OR LLCUTTKT
        SELE POSLN

        lcSvORd = ORDER()
        *SET('ORDER')
        lodbfPOSLN.SETORDER('Poslns')
        *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19))
        lodbfPOSLN.Seek('0001'+XSTYLE) 
    *IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19)) 
        IF lcBuckBy == "C"
        *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	           SCAN WHILE cinvtype+ style+cbusdocu+cstytype+po+ STR(lineno,6)+trancd =;
*!*	            '0001'+XSTYLE FOR TRANCD <> '3' AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' ;
*!*	            .AND.  BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE,POSHDR.AVAILABLE)),;
*!*	            DTOS(ldMinComp),DTOS((ldMaxComp)))
*!*	      
*!*	            XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.COMPLETE, POSHDR.AVAILABLE)
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]             
*!*             SCAN WHILE cinvtype+ style+cbusdocu+cstytype+po+ STR(lineno,6)+trancd =;
*!*              '0001'+XSTYLE FOR TRANCD <> '3' AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
*!*        
*!*              XDATE = POSHDR.COMPLETE
*!*             *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[END]
*!*             DO CASE
*!*              CASE XDATE <=XDATE1
*!*                XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*              CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*                XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*              CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*                XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*              CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*                XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*            ENDCASE
*!*          ENDSCAN
          DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE ;
                 FOR TRANCD <> '3' AND ;
                     lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' 
                 XDATE = POSLN.COMPLETE    
                 lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                 STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                 SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE +lcPoLine ;
                    FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                   FOR lnCount = 1 TO 8
                     lcCount = STR(lnCount,1)
                     XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount )
                   ENDFOR
                 ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                 DO CASE
                   CASE XDATE <=XDATE1
                     XWIP1 = XWIP1 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                     XWIP2 = XWIP2 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                     XWIP3 = XWIP3 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                     XWIP4 = XWIP4 + XPOWIP
                 ENDCASE                                    
              ENDDO       
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]                     
      ELSE
      *--IIF(lcRpByClr = 'Y',XSTYLE,PADR(SUBSTR(XSTYLE,1,lnMajLen),19))
      *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[Start]
*!*	          SCAN WHILE cinvtype+ style+cbusdocu+cstytype+po+ STR(lineno,6)+trancd =;
*!*	            '0001'+XSTYLE FOR TRANCD <> '3' AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' AND ;
*!*	             BETWEEN(DTOS(IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.START,POSHDR.AVAILABLE)),;
*!*	            DTOS(ldMinStrt),DTOS((ldMaxStrt)))
*!*	      
*!*	        XDATE = IIF(EMPTY(POSHDR.AVAILABLE),POSHDR.Start,;
*!*	          POSHDR.AVAILABLE)
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[Start]             
*!*            SCAN WHILE cinvtype+ style+cbusdocu+cstytype+po+ STR(lineno,6)+trancd =;
*!*              '0001'+XSTYLE FOR TRANCD <> '3' AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
*!*        
*!*          XDATE = POSHDR.Start
*!*          *:B609643,1 MMT 07/12/2011 DELIVERY SCHEDULE doesn't get 1st bucket Open & Hold Ordered[END]
*!*          DO CASE
*!*            CASE XDATE <=XDATE1
*!*              XWIP1 = XWIP1 + IIF( TranCd = '1', TotQty, -TotQty )
*!*            CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
*!*              XWIP2 = XWIP2 + IIF( TranCd = '1', TotQty, -TotQty )
*!*            CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
*!*              XWIP3 = XWIP3 + IIF( TranCd = '1', TotQty, -TotQty )
*!*            CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
*!*              XWIP4 = XWIP4 + IIF( TranCd = '1', TotQty, -TotQty )
*!*          ENDCASE
*!*        ENDSCAN
           DO WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE ;
                 FOR TRANCD <> '3' AND ;
                     lodbfPOSHDR.Seek(Posln.cbusdocu+ Posln.cstytype+Posln.po) AND POSHDR.STATUS $ 'OH' 
                 XDATE = POSHDR.START    
                 lcPoLine = cbusdocu+cstytype+po+STR(lineno,6)
                 STORE 0 TO XPOWIP1, XPOWIP2, XPOWIP3, XPOWIP4,XPOWIP5, XPOWIP6, XPOWIP7, XPOWIP8,XPOWIP
                 SCAN WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno,6)+trancd = '0001' + XSTYLE +lcPoLine ;
                    FOR TRANCD <> '3'  AND lodbfPOSHDR.Seek(POSLN.cbusdocu+POSLN.cstytype+POSLN.po) AND POSHDR.STATUS $ 'OH' 
                   FOR lnCount = 1 TO 8
                     lcCount = STR(lnCount,1)
                     XPOWIP&lcCount = XPOWIP&lcCount+ IIF( TranCd = '1', Qty&lcCount, -Qty&lcCount)
                   ENDFOR
                 ENDSCAN   
                  XPOWIP = MAX(XPOWIP1,0)+MAX(XPOWIP2,0)+MAX(XPOWIP3,0)+MAX(XPOWIP4,0)+MAX(XPOWIP5,0)+MAX(XPOWIP6,0)+MAX(XPOWIP7,0)+MAX(XPOWIP8,0)
                 DO CASE
                   CASE XDATE <=XDATE1
                     XWIP1 = XWIP1 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE1+1,XDATE2)
                     XWIP2 = XWIP2 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE2+1,XDATE3)
                     XWIP3 = XWIP3 + XPOWIP
                   CASE BETWEEN(XDATE,XDATE3+1,XDATE4)
                     XWIP4 = XWIP4 + XPOWIP
                 ENDCASE                                    
              ENDDO       
*:B609643,2 MMT 08/18/2011 DELIVERY SCHEDULE Report Print Incorrect WIP in case of Over receiving[END]             
    lodbfPOSLN.SETORDER(lcSvORd)
  ENDIF

ENDIF

SELECT(lcTmpWork)
REPLACE WIP1 WITH WIP1+XWIP1,;
  WIP2 WITH WIP2+XWIP2,;
  WIP3 WITH WIP3+XWIP3,;
  WIP4 WITH WIP4+XWIP4

SELECT STYLE
XCOLOR  = SUBSTR(STYLE.Style,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))
ENDSCAN
ENDIF 

*----------------------
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.
*!*************************************************************
*! Name      : lfCrtTemp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Create temp file
*!*************************************************************
*!
FUNCTION lfCrtTemp
DIMENSION laTableStruct[48,4]

laTableStruct[1,1] =  "STYLE"
laTableStruct[1,2] =  "C"
laTableStruct[1,3] =  19
laTableStruct[1,4] = 0

laTableStruct[2,1] =  "COLOR"
laTableStruct[2,2] =  "C"
laTableStruct[2,3] =  6
laTableStruct[2,4] = 0

laTableStruct[3,1] =  "O_ORD1"
laTableStruct[3,2] =  "N"
laTableStruct[3,3] =  10
laTableStruct[3,4] = 0

laTableStruct[4,1] =  "O_ORD2"
laTableStruct[4,2] =  "N"
laTableStruct[4,3] =  10
laTableStruct[4,4] = 0

laTableStruct[5,1] =  "O_ORD3"
laTableStruct[5,2] =  "N"
laTableStruct[5,3] =  10
laTableStruct[5,4] = 0

laTableStruct[6,1] =  "O_ORD4"
laTableStruct[6,2] =  "N"
laTableStruct[6,3] =  10
laTableStruct[6,4] = 0

laTableStruct[7,1] =  "O_ORDAMT1"
laTableStruct[7,2] =  "N"
laTableStruct[7,3] =  13
laTableStruct[7,4] = 2

laTableStruct[8,1] =  "O_ORDAMT2"
laTableStruct[8,2] =  "N"
laTableStruct[8,3] =  13
laTableStruct[8,4] = 2

laTableStruct[9,1] =  "O_ORDAMT3"
laTableStruct[9,2] =  "N"
laTableStruct[9,3] =  13
laTableStruct[9,4] = 2

laTableStruct[10,1] =  "O_ORDAMT4"
laTableStruct[10,2] =  "N"
laTableStruct[10,3] =  13
laTableStruct[10,4] = 2

laTableStruct[11,1] =  "H_ORD1"
laTableStruct[11,2] =  "N"
laTableStruct[11,3] =  10
laTableStruct[11,4] = 0

laTableStruct[12,1] =  "H_ORD2"
laTableStruct[12,2] =  "N"
laTableStruct[12,3] =  10
laTableStruct[12,4] = 0

laTableStruct[13,1] =  "H_ORD3"
laTableStruct[13,2] =  "N"
laTableStruct[13,3] =  10
laTableStruct[13,4] = 0

laTableStruct[14,1] =  "H_ORD4"
laTableStruct[14,2] =  "N"
laTableStruct[14,3] =  10
laTableStruct[14,4] = 0

laTableStruct[15,1] =  "H_ORDAMT1"
laTableStruct[15,2] =  "N"
laTableStruct[15,3] =  13
laTableStruct[15,4] = 2

laTableStruct[16,1] =  "H_ORDAMT2"
laTableStruct[16,2] =  "N"
laTableStruct[16,3] =  13
laTableStruct[16,4] = 2

laTableStruct[17,1] =  "H_ORDAMT3"
laTableStruct[17,2] =  "N"
laTableStruct[17,3] =  13
laTableStruct[17,4] = 2

laTableStruct[18,1] =  "H_ORDAMT4"
laTableStruct[18,2] =  "N"
laTableStruct[18,3] =  13
laTableStruct[18,4] = 2

laTableStruct[19,1] =  "WIP1"
laTableStruct[19,2] =  "N"
laTableStruct[19,3] =  10
laTableStruct[19,4] = 0

laTableStruct[20,1] =  "WIP2"
laTableStruct[20,2] =  "N"
laTableStruct[20,3] =  10
laTableStruct[20,4] = 0

laTableStruct[21,1] =  "WIP3"
laTableStruct[21,2] =  "N"
laTableStruct[21,3] =  10
laTableStruct[21,4] = 0

laTableStruct[22,1] =  "WIP4"
laTableStruct[22,2] =  "N"
laTableStruct[22,3] =  10
laTableStruct[22,4] = 0

laTableStruct[23,1] =  "STK1"
laTableStruct[23,2] =  "N"
laTableStruct[23,3] =  10
laTableStruct[23,4] = 0

laTableStruct[24,1] =  "STK2"
laTableStruct[24,2] =  "N"
laTableStruct[24,3] =  10
laTableStruct[24,4] = 0

laTableStruct[25,1] =  "STK3"
laTableStruct[25,2] =  "N"
laTableStruct[25,3] =  10
laTableStruct[25,4] = 0

laTableStruct[26,1] =  "STK4"
laTableStruct[26,2] =  "N"
laTableStruct[26,3] =  10
laTableStruct[26,4] = 0

laTableStruct[27,1] =  "TOTSTK"
laTableStruct[27,2] =  "N"
laTableStruct[27,3] =  10
laTableStruct[27,4] = 0

laTableStruct[28,1] =  "TOTORD"
laTableStruct[28,2] =  "N"
laTableStruct[28,3] =  12
laTableStruct[28,4] = 0

laTableStruct[29,1] =  "TOTWIP"
laTableStruct[29,2] =  "N"
laTableStruct[29,3] =  10
laTableStruct[29,4] = 2

laTableStruct[30,1] =  "H_TOTORDA"
laTableStruct[30,2] =  "N"
laTableStruct[30,3] =  13
laTableStruct[30,4] = 2

laTableStruct[31,1] =  "O_TOTORDA"
laTableStruct[31,2] =  "N"
laTableStruct[31,3] =  13
laTableStruct[31,4] = 2

laTableStruct[32,1] =  "PYSHIP1"
laTableStruct[32,2] =  "N"
laTableStruct[32,3] =  7
laTableStruct[32,4] = 0

laTableStruct[33,1] =  "PYSHIP2"
laTableStruct[33,2] =  "N"
laTableStruct[33,3] =  7
laTableStruct[33,4] = 0

laTableStruct[34,1] =  "PYSHIP3"
laTableStruct[34,2] =  "N"
laTableStruct[34,3] =  7
laTableStruct[34,4] = 0

laTableStruct[35,1] =  "PYSHIP4"
laTableStruct[35,2] =  "N"
laTableStruct[35,3] =  7
laTableStruct[35,4] = 0

laTableStruct[36,1] =  "PNSHIP1"
laTableStruct[36,2] =  "N"
laTableStruct[36,3] =  7
laTableStruct[36,4] = 0

laTableStruct[37,1] =  "PNSHIP2"
laTableStruct[37,2] =  "N"
laTableStruct[37,3] =  7
laTableStruct[37,4] = 0

laTableStruct[38,1] =  "PNSHIP3"
laTableStruct[38,2] =  "N"
laTableStruct[38,3] =  7
laTableStruct[38,4] = 0

laTableStruct[39,1] =  "PNSHIP4"
laTableStruct[39,2] =  "N"
laTableStruct[39,3] =  7
laTableStruct[39,4] = 0

laTableStruct[40,1] =  "AYSHIP1"
laTableStruct[40,2] =  "N"
laTableStruct[40,3] =  7
laTableStruct[40,4] = 0

laTableStruct[41,1] =  "AYSHIP2"
laTableStruct[41,2] =  "N"
laTableStruct[41,3] =  7
laTableStruct[41,4] = 0

laTableStruct[42,1] =  "AYSHIP3"
laTableStruct[42,2] =  "N"
laTableStruct[42,3] =  7
laTableStruct[42,4] = 0

laTableStruct[43,1] =  "AYSHIP4"
laTableStruct[43,2] =  "N"
laTableStruct[43,3] =  7
laTableStruct[43,4] = 0

laTableStruct[44,1] =  "ANSHIP1"
laTableStruct[44,2] =  "N"
laTableStruct[44,3] =  7
laTableStruct[44,4] = 0

laTableStruct[45,1] =  "ANSHIP2"
laTableStruct[45,2] =  "N"
laTableStruct[45,3] =  7
laTableStruct[45,4] = 0

laTableStruct[46,1] =  "ANSHIP3"
laTableStruct[46,2] =  "N"
laTableStruct[46,3] =  7
laTableStruct[46,4] = 0

laTableStruct[47,1] =  "ANSHIP4"
laTableStruct[47,2] =  "N"
laTableStruct[47,3] =  7
laTableStruct[47,4] = 0

laTableStruct[48,1] =  "desc"
laTableStruct[48,2] =  "C"
laTableStruct[48,3] =  20
laTableStruct[48,4] = 0


 = gfCrtTmp(lcTmpWork ,@laTableStruct,'Style',lcTmpWork,.F.)
*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.
