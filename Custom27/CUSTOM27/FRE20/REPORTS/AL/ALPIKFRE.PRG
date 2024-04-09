*:***************************************************************************
*: Program file  : ALPIKFRE.PRG
*: Program desc. : CUSTOMIZED ALLOCATION PIKTICKT LOG FOR FRE20.
*! Date          : 08/05/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION(AL)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102606
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPIKFRE
*:***************************************************************************
*: Modifications :
*: C102635,1 BWA 07/24/2002 Add 2 fields in the layout and 1 filter in the grid.
*:***************************************************************************

*--Section of Variables
lcTime = TIME()         && Variable to hold the Time

*--To get the date in a variables
lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PIKTKT.DATE'),1)
LDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
HDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))

IF llOGFltCh
  DO lpCreaTemp  && Create Temp Cursor
  DO lpColect    && Collect data
ENDIF

SELECT (lcPickTmp)
SET ORDER TO TAG &lcPickTmp

llEndReprt = .F.
GO BOTTOM
REPLACE &lcPickTmp..lEndRep WITH .T.
LOCATE

SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET ORDER TO PIKLINE  IN PIKLINE
SET ORDER TO OrdLinst IN ORDLINE
LOCATE

lcCurrExpr = IIF(llMultCurr AND lcRpCurr="F",[ORDHDR.CCURRCODE],[gcBaseCurr])
lcCurrCode = EVALUATE(lcCurrExpr)
llCurrChg  = .F.

SELECT (lcPickTmp)
LOCATE
DO gfDispRe WITH EVALUATE('lcRpName')

*--Export the temp. file to excel file.
IF llRPExport
  SELECT (lcPickTmp)
  SCAN
    STORE 0 TO lnTotQty , lnTotAmnt
    =lfSumPik()
    SELECT (lcPickTmp)
    REPLACE Totqty  WITH lnTotQty ;
            Amount WITH lnTotAmnt
  ENDSCAN

  *C102635,1 BWA 07/24/2002 Add the 2 new fields to the excel file.[START]
  *EXPORT TO ("C:\"+'PICKLOG') FIELDS PIKTKT,DATE,START,COMPLETE,ORDHDR.ACCOUNT,ACNAME,ORDHDR.ORDER,Prntd,Invcd,Totqty,Amount TYPE XLS
  EXPORT TO ("C:\"+'PICKLOG') FIELDS PIKTKT,DATE,START,COMPLETE,ORDHDR.ACCOUNT,ACNAME,Terms,PO,ORDHDR.ORDER,Prntd,Invcd,Totqty,Amount TYPE XLS
  *C102635,1 BWA 07/24/2002.[END]

ENDIF

*-- Clear relation [Begin]
SELECT (lcPickTmp)
SET RELATION TO
                       *-- End of the Program --*
*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : Option Grid && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   : =fSeTOrdr()
*!**************************************************************************
*! Note      : symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSeTOrdr
PARAMETERS OpGrdParm

SELECT ORDHDR
DO CASE
  CASE OpGrdParm = 'S'
    lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER
    GO TOP
  CASE OpGrdParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER
ENDCASE

*--End of lfSeTOrdr.
*!**************************************************************************
*! Name      : lfSumPik
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : To return the Total Amount and Total Quantity
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        :
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : lfSumPik()
*!**************************************************************************
*! Note               : 
*!**************************************************************************
FUNCTION lfSumPik

PRIVATE lcExpList,lnCurAlias,lnPrice
lnCurAlias = SELECT(0)
lcExpList = "TotPik,TotPik*Price"

IF &lcPickTmp..Status $ 'CX'
  SELECT PIKLINE
  LOCATE
  =SEEK(&lcPickTmp..PikTkt)
  SUM &lcExpList REST WHILE PikTkt+Order+STR(LineNo,6) = &lcPickTmp..PikTkt ;
      TO lnTotQty,lnTotAmnt
ELSE
  SELECT ORDLINE
  LOCATE
  =SEEK('O'+&lcPickTmp..Order+&lcPickTmp..Store)
  SUM &lcExpList REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                            'O'+&lcPickTmp..Order+&lcPickTmp..Store ;
                      FOR PikTkt = &lcPickTmp..PikTkt TO lnTotQty,lnTotAmnt
ENDIF

IF llMultCurr AND (lcRpCurr <> "F") AND;
   (lnTotAmnt <> 0) AND (ORDHDR.CCURRCODE <> gcBaseCurr)
  lnTotAmnt = gfAmntDisp(lnTotAmnt,lcRpCurr,ldRpExDate,;
                         lcRpTmpNam,.F.,"ORDHDR")
ENDIF

IF !llCurrChg
  llCurrChg = !(EVALUATE(lcCurrExpr) == lcCurrCode)
  lcCurrCode = EVALUATE(lcCurrExpr)
ENDIF

llEndReprt =  &lcPickTmp..lEndRep
SELECT (lnCurAlias)
RETURN 0

*--End of lfSumPik.
*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : The when function.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : 
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : lfwRepWhen()
*!**************************************************************************
*! Note               : 
*!**************************************************************************
FUNCTION lfwRepWhen

= lfvInvoice()

*--End of lfwRepWhen.
*!**************************************************************************
*! Name      : lfvInvoice
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Function to disable or enable filter in the option grid.
*!*********************************************************************
*! Calls     : 
*!*********************************************************************
*! Called from        : 
*!*********************************************************************
*! Passed Parameters  : None
*!*********************************************************************
*! Returns            : None
*!*********************************************************************
*! Example            : lfvInvoice()
*!*********************************************************************
*! Note               : 
*!*********************************************************************
FUNCTION lfvInvoice

IF lcRPInv = 'Y'
  llRPRelPT = .F.
ENDIF
lnRelPTPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPRelPT'),1)
laOGObjCnt[lnRelPTPo] = lcRPInv $ 'BN'
= lfOGShowGet('llRPRelPT')

*--End of lfvInvoice.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

SELECT CUSTOMER
SET ORDER TO CUSTOMER
GO TOP

*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfsrPkt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrPkt()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol is Reset
*!*************************************************************
FUNCTION lfsrPkt
PARAMETERS lcParm

SELECT PIKTKT
GO TOP

*-- End of lfsrPkt.
*!**************************************************************************
*! Name      : lfFillVars
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Fill most of report memory variables.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : 
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : lfFillVars()
*!**************************************************************************
*! Note               : 
*!**************************************************************************
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End]
ENDIF

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

llClearFn = .T.  &&You erase temporary file.
*-- Close temp. opended files, if it used.

IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF
ENDIF

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfGtCurDes
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Function Get the Description ofthe Currency
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGtCurDes()
*!*************************************************************
FUNCTION lfGtCurDes

IF lcRpCurr <> "F"
  RETURN gcBaseCurr
ELSE
  IF llMultCurr and SEEK(ORDHDR.cCurrCode,'SYCCURR')
    RETURN SYCCURR.cCurrDesc
  ENDIF
ENDIF

RETURN ""

*--End of lfGtCurDes.
*!*************************************************************
*! Name      : lpCreaTemp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Procedure to create Temp. File.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCreaTemp
*!*************************************************************
PROCEDURE lpCreaTemp

*--Check If the file is created or not.
IF USED(lcPickTmp) AND RECCOUNT(lcPickTmp) > 0
  USE IN (lcPickTmp)
ENDIF

*--Create File.
IF !USED(lcPickTmp)
  IF TYPE("laTempStru[1,1]") $ "UL"
    DIMENSION laTempStru[1,4]
    SELECT PIKTKT
    =AFIELDS(laTempStru)
    =lfAddField("laTempStru","cCurrCode","C",3,0)
    =lfAddField("laTempStru","lEndRep","L",1,0)
  ENDIF

  *--Add the start and complete date fields.
  =lfAddField("laTempStru","start","D",8,0)
  =lfAddField("laTempStru","complete","D",8,0)

  *C102635,1 BWA 07/24/2002 Add 2 fields in the layout.[START]
  =lfAddField("laTempStru","Terms","C",30,0)
  =lfAddField("laTempStru","PO","C",10,0)
  *C102635,1 BWA 07/24/2002.[END]

  *--Add this fields to use it in the exporting
  IF llRPExport
    =lfAddField("laTempStru","Prntd","C",3,0)
    =lfAddField("laTempStru","Invcd","C",3,0)
    =lfAddField("laTempStru","Totqty","N",7,0)
    =lfAddField("laTempStru","Amount","N",14,2)
    =lfAddField("laTempStru","AcName","C",30,0)
  ENDIF

  CREATE CURSOR (lcPickTmp) FROM ARRAY laTempStru
  ZAP

  IF llMultCurr AND (lcRpCurr = "F")
    *-- Index On Temp File

    *C102635,1 BWA 07/24/2002 Add filter SORTBY in the grid.[START]
    *INDEX ON cCurrCode + PIKTKT TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    DO CASE
      CASE lcRpSortBy = 'A'
        INDEX ON cCurrCode + ACCOUNT TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
      CASE lcRpSortBy = 'P'
        INDEX ON cCurrCode + PIKTKT TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
      CASE lcRpSortBy = 'T'
        INDEX ON cCurrCode + Terms TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    ENDCASE
    *C102635,1 BWA 07/24/2002.[END]

  ELSE
    *-- Index On Temp File

    *C102635,1 BWA 07/24/2002 Add filter SORTBY in the grid.[START]
    *INDEX ON  PIKTKT  TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    DO CASE
      CASE lcRpSortBy = 'A'
        INDEX ON ACCOUNT TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
      CASE lcRpSortBy = 'P'
        INDEX ON PIKTKT TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
      CASE lcRpSortBy = 'T'
        INDEX ON Terms TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    ENDCASE
    *C102635,1 BWA 07/24/2002.[END]

  ENDIF
ENDIF

*--End of lpCreaTemp.
*!*************************************************************
*! Name      : lpCollect
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Procedure TO Colecte Data.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollect
*!*************************************************************
PROCEDURE lpColect

SELECT PIKTKT
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SCAN FOR &lcRpExp
  SCATTER MEMVAR MEMO
  m.cCurrCode = ORDHDR.cCurrCode
  m.Start     = ORDHDR.START
  m.Complete  = ORDHDR.COMPLETE
  m.AcName    = CUSTOMER.BTNAME

  *C102635,1 BWA 07/24/2002 Add the value of the terms in the temp. file.[START]
  m.Terms  = ALLTRIM(gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE'))
  m.PO     = LEFT(ORDHDR.CUSTPO,10)
  *C102635,1 BWA 07/24/2002.[END]

  IF llRPExport
    m.Prntd    = IIF(PrtFlag='P','Yes','No')
    m.Invcd   = IIF(Status$'CX','Yes','No')
  ENDIF
  INSERT INTO (lcPickTmp) FROM MEMVAR
ENDSCAN

SELECT PIKTKT
SET RELATION TO

*--End of lpColect.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Add fields to the the array of file structer.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + 1
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfvRepCurr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/05/2002
*! Purpose   : Add an option to allow different currencies.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRepCurr()
*!*************************************************************
FUNCTION lfvRepCurr

llOGFltCh = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*--End of lfvRepCurr.