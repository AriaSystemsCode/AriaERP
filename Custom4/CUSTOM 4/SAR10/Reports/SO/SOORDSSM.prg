*:***************************************************************************
*: Program file  : SOORDSSM.PRG
*: Program desc. : Custom ORDER STATUS REPORT for SAR10.
*: Date          : 03/06/2019
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : Esraa Ahmed
*: Tracking Job Number: C202258 {T20181120.0005}
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOORDSRP
*:***************************************************************************
*: Modifications :
***************************************************************************
#include R:\Aria4xp\reports\so\soordsrp.h
IF llOgFltCh
llDonprnt=.F.
STORE .F. TO llStyFltr ,llAccFltr ,llOrdFltr
*--The Style length.
lnLnthSty = LEN(gfItemMask('PM'))

*--Section initial the wanted variables.
STORE SPACE(0) TO lcDatPos , lcEntPos , lcComPos , lcStrPos , lcStyPos

*--Case get the value of the Special Instructions.
lnClassPo = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.SPCINST'),1)
lcCodeinst = laOGFxFlt[lnClassPo,6]


*-- Order Filter
lcOrdFltr= lfCheckFilter(1, 'ORDHDR.ORDER')
llOrdFltr   = !EMPTY(lcOrdFltr) AND USED(lcOrdFltr) AND RECCOUNT(lcOrdFltr) > 0
IF llOrdFltr
  SELECT (lcOrdFltr)
  INDEX ON order TAG (lcOrdFltr)
ELSE
  IF TYPE("lcOrdFltr") = "C" AND USED(lcOrdFltr)
    USE IN (lcOrdFltr)
  ENDIF
  lcOrdFltr= ''
ENDIF

*-- Style Filter
lcStyFltr= lfCheckFilter(1, 'STYLE.STYLE')
llStyFltr   = !EMPTY(lcStyFltr) AND USED(lcStyFltr) AND RECCOUNT(lcStyFltr) > 0
IF llStyFltr
  SELECT (lcStyFltr)
  INDEX ON cstymajor TAG (lcStyFltr)
ELSE
  IF TYPE("lcStyFltr") = "C" AND USED(lcStyFltr)
    USE IN (lcStyFltr)
  ENDIF
  lcStyFltr = ''
ENDIF
* Accounts Filter
lcAccFltr= lfCheckFilter(1, 'ORDHDR.ACCOUNT')
llAccFltr   = !EMPTY(lcAccFltr) AND USED(lcAccFltr) AND RECCOUNT(lcAccFltr) > 0
IF llAccFltr
  SELECT (lcAccFltr)
  INDEX ON ACCOUNT TAG (lcAccFltr)
ELSE
  IF TYPE("lcAccFltr") = "C" AND USED(lcAccFltr)
    USE IN (lcAccFltr)
  ENDIF
  lcAccFltr= ''
ENDIF


lcFilter =" From ordhdr inner join ordline on ordhdr.order=ordline.order "
IF llOrdFltr
lcFilter =lcFilter + " inner join " +lcOrdFltr  +" TmpOrd on ordhdr.order=TmpOrd.order "
ENDIF

IF llAccFltr
lcFilter =lcFilter + " inner join " +lcAccFltr+" TmpAcc on ordhdr.account =TmpAcc.account "
ENDIF

IF llStyFltr
lcFilter =lcFilter + " inner join " +lcStyFltr+" Tmpsty on LEFT(ORDLINE.STYLE,lnLnthSty)=Tmpsty.cstymajor "
ENDIF


lcFilter =lcFilter + " Where  Ordhdr.status $'OH' AND ORDHDR.CORDTYPE='O'"


*--Case get the value of the Entered date.
lnEntrPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
IF lnEntrPos > 0
  lnEntPos = AT('BETWEEN(DTOS(ORDHDR.ENTERED',LCRPEXP)
  IF lnEntPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnEntPos))
    IF lnPos1 > 0
      lcEntPos = SUBSTR(lcRpExp ,lnEntPos , lnPos1-1)
    ELSE
      lcEntPos = SUBSTR(lcRpExp ,lnEntPos)
    ENDIF
  ENDIF
ENDIF
*--Case get the value of the Complete date.
lnCompPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.COMPLETE'),1)
IF lnCompPos > 0
  lnComPos = AT('BETWEEN(DTOS(ORDHDR.COMPLETE',LCRPEXP)

  IF lnComPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnComPos))
    IF lnPos1 > 0
      lcComPos = SUBSTR(lcRpExp ,lnComPos , lnPos1-1)
    ELSE
      lcComPos = SUBSTR(lcRpExp ,lnComPos)
    ENDIF
  ENDIF
ENDIF

*--Case get the value of the Start date.
lnStrtPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.START'),1)
IF lnStrtPos > 0
  lnStrPos = AT('BETWEEN(DTOS(ORDHDR.START',LCRPEXP)

  IF lnStrPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnStrPos))
    IF lnPos1 > 0
      lcStrPos = SUBSTR(lcRpExp ,lnStrPos , lnPos1-1)
    ELSE
      lcStrPos = SUBSTR(lcRpExp ,lnStrPos)
    ENDIF
  ENDIF
ENDIF

*--Section get the value of the lcFilter.

IF !EMPTY(lcEntPos)
  lcFilter = lcFilter + ' .AND. ' +  lcEntPos
ENDIF

IF !EMPTY(lcComPos)
  lcFilter = lcFilter + ' .AND. ' +  lcComPos
ENDIF

IF !EMPTY(lcStrPos)
  lcFilter = lcFilter + ' .AND. ' +  lcStrPos
ENDIF

IF !EMPTY(lcCodeinst)
  lcFilter = lcFilter + ' .AND. ' + ' ORDHDR.SPCINST = lcCodeinst'
ENDIF

lcTempFile =loOgScroll.gfTempName()
lcWorkfile =loOgScroll.gfTempName()
LstTemp    =loOgScroll.gfTempName()
lcCursName =loOgScroll.gfTempName()

=lfBuildTmp()
*-- Create the temp file and put data in it.
lnEngin=SET("EngineBehavior" )
SET ENGINEBEHAVIOR 70
lcFields=" Distinct ORDHDR.ORDER, ORDHDR.ACCOUNT, ORDHDR.CUSTPO, ORDHDR.START, ORDHDR.COMPLETE , "
lcFields=lcFields+" ORDHDR.ENTERED, ORDLINE.STYLE AS STYLE ,SUM(ORDLINE.TOTQTY) AS TOTQTY, ORDLINE.PRICE,SUM(ORDLINE.TOTQTY*ORDLINE.PRICE) AS AMOUNT"
lcFields=lcFields+", ORdhdr.Rep1" 

lcFrom=lcFilter+"  AND ORDLINE.TOTQTY>0 "
SET STEP ON 
lcGroupby=" GROUP BY ORDHDR.COMPLETE,ORDHDR.ORDER,7 "
lcOrderby=" ORDER BY ORDHDR.COMPLETE,ORDHDR.ORDER,7 "

SELECT &lcFields.   &lcFrom. &lcGroupby.  &lcOrderby.    INTO DBF &gcWorkDir.&LstTemp
SET ENGINEBEHAVIOR lnEngin

INDEX ON (ORDER+STYLE) TAG ORDSTY
SET ORDER TO ORDSTY
SET RELATION TO order INTO Invline ADDITIVE
SELECT INVLINE
SET ORDER TO TAG INVLINEO
SET RELATION TO INVOICE INTO INVHDR ADDITIVE
SELECT (LSTTEMP)

*-- If there were nothing selected from the SQL then return.
IF RECCOUNT() = 0 .OR. EOF()

  llDonprnt = .T.
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

=lfCollect()

WAIT CLEAR

*-- if no records matches the selection criteria.
SELECT (lcWorkfile)
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF  && end if no records matches the selection criteria.
GOTO TOP
COPY REST TO oAriaApplication.WorkDir + lcTempFile+ ".DBF"

USE oAriaApplication.WorkDir + lcTempFile+ ".DBF" IN 0 EXCLUSIVE


SELECT (lcTempFile) &&global function opens DBF (with-optionally- its index) share or exculsive.
INDEX ON DTOC(COMPLETE)+ORDER TAG (lcTempFile)
=lfAdjustCRSettings()
IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF
=gfDispRe()
ELSE

  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF
ENDIF  &&FILTER CHANGE

                      *-- End of the Program --*
*!*************************************************************
*! Name      : lfCollect
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : CollectING  Data
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfCollect
*!*************************************************************
FUNCTION lfCollect

*--As Ver2.6 Store Complete date.
STORE COMPLETE TO ldWekTot ,ldMonTot

*!* Get cutpick for these orders ************************************* [BEGIN]
lcTemp = loOgScroll.gfTempName()
SELECT DISTINCT ORDER AS ORDERS FROM &lstTemp. INTO CURSOR &lcTemp
lcSQLCur = loOgScroll.gfSQLTempName('','ORDERS C(6)', lcTemp ,'ORDERS') && SQL Temp File
IF EMPTY(lcSQLCur)
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
     RETURN .F.
ENDIF

LCFIELDS=" CutPick.[Order],cTktNo,CutPick.TotQty,CutPick.Style "
lcFrom  ="  FROM CutPick, " +lcSQLCur+ " TmpORDRS "
lcWhere ="  WHERE CutPick.[Order] =TmpORDRS.OrderS AND TRANCD='1' "

lcSQLStmt  = "Select " + LCFIELDS +lcFrom +lcWhere
lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, lcCursName  ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
IF lnResult = 1
  SELECT (lcCursName)
  =CURSORSETPROP("Buffering" ,3)
  INDEX ON ORDER + CTKTNO TAG (lcCursName)
ELSE
  *-- SQL connection error. can't open the report
   =gfModalGen('TRM00416B40011','ALERT')
   RETURN .F.
ENDIF
*!* Get cutpick for these orders  ************************************* [END]

*!* Get Cuttickts for these orders ************************************ [BEGIN]
IF llStyFltr OR llAccFltr  OR llOrdFltr
  lcTemp1 = loOgScroll.gfTempName()
  SELECT DISTINCT cTktNo AS POS FROM &lcCursName.   INTO CURSOR &lcTemp1
  lcSQLCur = loOgScroll.gfSQLTempName('','POS C(6)', lcTemp1 ,'POS') && SQL Temp File
IF EMPTY(lcSQLCur)
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
     RETURN .F.
ENDIF

  lcSQLStmt  =             " Select PO,STATUS,STYLE,DAMAGE,RECEIVE,POTOTAL,[OPEN]  "
  lcSQLStmt  =lcSQLStmt  + " FROM POSHDR(INDEX=POSHDR) INNER JOIN "+lcSQLCur+ " TmpPOS "
  lcSQLStmt  =lcSQLStmt  + " ON POSHDR.CBUSDOCU='P' AND  POSHDR.CSTYTYPE='U' "
  lcSQLStmt  =lcSQLStmt  + " AND POSHDR.PO=TmpPOS.POS "
ELSE
  lcSQLStmt  = " Select PO,STATUS,STYLE,DAMAGE,RECEIVE,POTOTAL,[OPEN] FROM POSHDR(INDEX=POSHDR) WHERE POSHDR.CBUSDOCU='P' AND POSHDR.CSTYTYPE='U' "
ENDIF

lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CUTTKTH'  ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
IF lnResult = 1
  SELECT CUTTKTH
  =CURSORSETPROP("Buffering" ,3)
  INDEX ON  PO+STYLE TAG CUTTKTH
ELSE
  *-- SQL connection error. can't open the report
   =gfModalGen('TRM00416B40011','ALERT')
   RETURN .F.
ENDIF
*!* Get Cuttickts for these orders ************************************ [END]
SELECT STYLE
SET ORDER TO CSTYLE

SELECT (LstTemp)
LOCATE

ldBrkDate = lfGetNxtBrk(Complete)
lnMonth=111 &&months from 111 to 999
lnWeak=1    && weaks from 1 to 5
SCAN

WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Rec,oAriaApplication.GetHeaderText("LANG_Select_Rec",AHEADERFILE)) + ORDER NOWAIT
  STORE 0 TO lnOrdTot ,lnIssTot ,lnCutTot ,ln1stTot ,;
             ln2ndTot ,lnShpTot ,lnUbaTot ,lnBalTot

IF Complete > ldBrkDate
  lnWeak=lnWeak+1
  ldBrkDate  = lfGetNxtBrk(Complete)
  llWekPrntd = .T.
ELSE
  llWekPrntd = .F.
ENDIF

IF MONTH(COMPLETE) <> MONTH(ldMonTot)

  IF !llWekPrntd
     ldBrkDate = lfGetNxtBrk(Complete)
  ENDIF
  lnWeak=1
  lnMonth=lnMonth+1
  ldMonTot = COMPLETE
ENDIF
SET STEP ON 
SELECT (LSTTEMP)
SCATTER MEMVAR memo
DO lpOrder
DO lpIssud
SELECT (LSTTEMP)
m.customer= IIF(SEEK('M'+ACCOUNT,'CUSTOMER'),SUBSTR(CUSTOMER.BTNAME,1,25),SPACE(25))
m.STYDESC = IIF(SEEK(STYLE,'STYLE','STYLE'), STYLE.DESC,'   ')
m.TOTCOST = IIF(SEEK(STYLE,'STYLE','STYLE'), STYLE.TOTCOST,0)
m.CUSTPO  = LEFT(CUSTPO,15)
m.rep1 = rep1
m.CWEK    = STR(lnWeak,1)
m.CMON    = STR(lnMonth,3)
m.UNTORD  = lnOrdTot
m.UNTISSU = lnIssTot
m.UNTCUT  = lnCutTot
m.RCVFRST = ln1stTot
m.RCVSCND = ln2ndTot
m.UNTSHPD = lnShpTot
m.UNTBALC = lnUbaTot
m.OPNBALC = lnBalTot
m.style=STYLE


INSERT INTO (lcworkfile) FROM MEMVAR
ENDSCAN

*!*************************************************************
*! Name      : lpIssud
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : calculate the units (issued,cut,1strecvd,2strecvd)
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpIssud
*!*************************************************************
PROCEDURE lpIssud
*-- Get budget, Actualized, Received and Damaged quantities for all cut tickets
*-- computed from the processed order.
SELECT (lcCursName)
SEEK &LstTemp..Order
PRIVATE lcOrder
lcOrder = SPACE(0)

SCAN WHILE Order = &LstTemp..Order
   IF SEEK(cTktNo,'CutTktH') .AND. &lcCursName..Style = &LstTemp..Style
 
    lnIssTot = lnIssTot + IIF(CutTktH.Status $ 'OH' , TotQty ,0 )
    lnCutTot = lnCutTot + IIF(CutTktH.Status $ 'AC' , TotQty ,0 )
    IF lcOrder # ORDER
      ln1stTot = ln1stTot + IIF(CutTktH.Status $ 'OHACS',CutTktH.RECEIVE,0)
      ln2ndTot = ln2ndTot + IIF(CutTktH.Status $ 'OHACS',CutTktH.DAMAGE,0)
      lcOrder = ORDER
    ENDIF
  ENDIF
ENDSCAN
*--End of lpIssud.
*!*************************************************************
*! Name      : lpOrder
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : calculate the order units And any special notes.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOrder
*!*************************************************************
PROCEDURE lpOrder
SELECT (LstTemp)
lnUbaTot = lnUbaTot + TOTQTY
lnBalTot = lnBalTot + AMOUNT
SELECT INVLINE
IF SEEK(&LstTemp..ORDER)
     SCAN WHILE order+STR(lineno,6)+invoice = &LstTemp..ORDER ;
     	          FOR STYLE= &LstTemp..STYLE .AND. INVHDR.STATUS <> 'V'
    lnShpTot = lnShpTot + TOTQTY
  ENDSCAN
ENDIF
lnOrdTot = lnUbaTot + lnShpTot

*--End of lpOrder.
*!*************************************************************
*! Name      : lfGetNxtBrk
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : To get the next friday date.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfGetNxtBrk
*!*************************************************************
FUNCTION lfGetNxtBrk
PARAMETERS ldDate
PRIVATE ldDate, ldRet
IF EMPTY(ldDate)
  RETURN {}
ENDIF
ldRet = ldDate
DO WHILE UPPER(CDOW(ldRet)) <> "FRIDAY"
  ldRet = ldRet + 1
ENDDO

RETURN ldRet

*--End of lfGetNxtBrk.

*!*************************************************************
*! Name      : lfsrOrder
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm

IF lcParm = "S"
  SELECT ORDHDR
  LOCATE
ENDIF

*--End of lfsrOrder.


*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]

DIMENSION loOgScroll.laCRParams[4,2]

loOgScroll.lcOGLastForm ='SOORDSSM'
loOGScroll.cCROrientation='L'

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTempFile+ ".DBF"

loOgScroll.laCRParams[1,1] = 'OpTitle'
loOgScroll.laCRParams[1,2] = ''

loOgScroll.laCRParams[2,1] = 'ReportName'
loOgScroll.laCRParams[2,2]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORDER_STATUS_REPORT,oAriaApplication.GetHeaderText("LANG_ORDER_STATUS_REPORT",AHEADERFILE))   && report title

loOgScroll.laCRParams[3,1] = 'SortBy'
loOgScroll.laCRParams[3,2] = 'SortBy'

loOgScroll.laCRParams[4,1] = 'Layout'
loOgScroll.laCRParams[4,2] = 'Layout'

*************************************************************
*! Name      : lfBuildTmp
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : Create Tmp Files
*!*************************************************************
FUNCTION lfBuildTmp
DIMENSION laTempStru[23,18] ,laTemplINE[1,18],laTempHDR[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTemplINE,laTempHDR

lcExcStat = SET('EXACT')
SET EXACT ON

SELECT ORDHDR
=OGAFIELDS(@laTempHDR)

laTempStru[1,1] = 'ORDER'
laTempStru[2,1] = 'ACCOUNT'
laTempStru[3,1] = 'CUSTPO'
laTempStru[4,1] = 'START'
laTempStru[5,1] = 'COMPLETE '
laTempStru[6,1] = 'ENTERED'
*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 6
  lnFldRow = ASCAN(laTempHDR,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempHDR,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempHDR[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempHDR[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempHDR[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

laTempStru[7,1] = 'PRICE'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 12
laTempStru[7,4] = 2
laTempStru[8,1] = 'TOTQTY'
laTempStru[8 ,2] = 'N'
laTempStru[8 ,3] = 10
laTempStru[8 ,4] = 0

laTempStru[9,1] = 'STYLE'
laTempStru[9 ,2] = 'C'
laTempStru[9 ,3] = 19
laTempStru[9 ,4] = 0

laTempStru[10 ,1] = 'STYDESC'
laTempStru[10 ,2] = 'C'
laTempStru[10 ,3] = 50
laTempStru[10 ,4] = 0

laTempStru[11 ,1] = 'rep1'
laTempStru[11 ,2] = 'C'
laTempStru[11 ,3] = 3
laTempStru[11 ,4] = 0

laTempStru[12 ,1] = 'CUSTOMER'
laTempStru[12 ,2] = 'C'
laTempStru[12 ,3] = 50
laTempStru[12 ,4] = 0

laTempStru[13 ,1] = 'UNTISSU'
laTempStru[13 ,2] = 'N'
laTempStru[13 ,3] = 10
laTempStru[13 ,4] = 0

laTempStru[14 ,1] = 'UNTORD'
laTempStru[14 ,2] = 'N'
laTempStru[14 ,3] = 10
laTempStru[14 ,4] = 0

laTempStru[15 ,1] = 'OPNBALC'
laTempStru[15 ,2] = 'N'
laTempStru[15 ,3] = 10
laTempStru[15 ,4] = 0

laTempStru[16 ,1] = 'RCVFRST'
laTempStru[16 ,2] = 'N'
laTempStru[16 ,3] = 10
laTempStru[16 ,4] = 0

laTempStru[17 ,1] = 'RCVSCND'
laTempStru[17 ,2] = 'N'
laTempStru[17 ,3] = 10
laTempStru[17 ,4] = 0

laTempStru[18 ,1] = 'UNTCUT'
laTempStru[18 ,2] = 'N'
laTempStru[18 ,3] = 10
laTempStru[18 ,4] = 0

laTempStru[19 ,1] = 'UNTSHPD'
laTempStru[19 ,2] = 'N'
laTempStru[19 ,3] = 10
laTempStru[19 ,4] = 0

laTempStru[20 ,1] = 'UNTBALC'
laTempStru[20 ,2] = 'N'
laTempStru[20 ,3] = 10
laTempStru[20 ,4] = 0

laTempStru[21 ,1] = 'CMON'
laTempStru[21 ,2] = 'C'
laTempStru[21 ,3] = 3
laTempStru[21 ,4] = 0

laTempStru[22 ,1] = 'CWEK'
laTempStru[22 ,2] = 'C'
laTempStru[22 ,3] = 1
laTempStru[22 ,4] = 0


*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 7 TO 22
  lnFldRow = ASCAN(laTemplINE,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTemplINE,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTemplINE[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTemplINE[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTemplINE[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)


AINS(laTempStru,7)
laTempStru[7,1] = 'TOTCOST'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 13
laTempStru[7,4] = 2
FOR lnCntVar = 5 TO 18
  laTempStru[7,lnCntVar] = ''
ENDFOR 


=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat



*************************************************************
*! Name      : lfCheckFilter
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!**************************************************************************
*! Name      : lfSetSTY
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
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

*!**************************************************************************
*! Name      : lfvShipAddress1
*! Developer : Esraa Ahmed
*! Date      : 03/06/2019
*! Purpose   : Fix the Order Browse customer shipping address 1
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : Valid Shipping Address 1
*!**************************************************************************
*! Example   : =lfvShipAddress1("MA100","HOB")
*!**************************************************************************
*!
FUNCTION lfvShipAddress1
LPARAMETERS lcAccount, lcStore
LOCAL lcAddress1, lnSelect
lnSelect = SELECT(0)

IF EMPTY(lcStore)
  =SEEK("M"+lcAccount,"CUSTOMER","CUSTOMER")
ELSE
  =SEEK("S"+lcAccount+lcStore,"CUSTOMER","CUSTOMER")
ENDIF
lcAddress1 = Customer.cAddress1

SELECT(lnSelect)
RETURN lcAddress1
ENDFUNC
