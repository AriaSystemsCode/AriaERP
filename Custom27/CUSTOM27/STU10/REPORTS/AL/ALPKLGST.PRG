*:***************************************************************************
*: Program file  : ALPKLGST
*: Program desc. : MOMENTRENDS ALLOCATION PIKTICKT LOG FOR STU10
*! Date          : 03/09/2001
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : HEND GHANEM (HBG)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPIKTKT
*:***************************************************************************
*: This Report Program is due to C102421 ...
*:***************************************************************************


*--Section of Variables
lcTime = TIME()         && Variable to hold the Time
*-- To get the date in a variables
lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PIKTKT.DATE'),1)
LDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
HDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))


IF llOGFltCh
  DO lpCreaTemp  && Create Temp Cursor
  DO lpColect    && Collect data
ENDIF

*--Print from Temp file insted of master file [Begin]
SELECT (lcPickTmp)

SET ORDER TO TAG &lcPickTmp

  
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SET ORDER TO PikLine  IN PikLine 
SET ORDER TO OrdLinst IN OrdLine
LOCATE 

lcCurrExpr = IIF(llMultCurr AND lcRpCurr="F",[ORDHDR.CCURRCODE],[gcBaseCurr])
lcCurrCode = EVALUATE(lcCurrExpr)
llCurrChg  = .F.

DO gfDispRe WITH EVALUATE('lcRpName') 

*-- Clear relation [Begin]
SELECT (lcPickTmp)
SET RELATION TO


*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 08/11/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : Option Grid && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : =fSeTOrdr()
*!**************************************************************************
*! Note               : symbol is [S,Set- R,ReSet]
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

*!**************************************************************************
*! Name      : lfSumPik
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 08/08/1999
*! Purpose   : To return the Total Amount and Total Quantity
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : ALPKTMTD.FRX
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
  SELECT PikLine
  LOCATE
  =SEEK(&lcPickTmp..PikTkt)

  SUM &lcExpList REST WHILE PikTkt+Order+STR(LineNo,6) = &lcPickTmp..PikTkt ;
      TO lnTotQty,lnTotAmnt 
ELSE
  SELECT OrdLine
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

SELECT (lnCurAlias)
RETURN 0

**********************************************************************

FUNCTION lfwRepWhen

= lfvInvoice()

**********************************************************************

FUNCTION lfvInvoice

IF lcRPInv = 'Y'
  llRPRelPT = .F.
ENDIF  
lnRelPTPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPRelPT'),1)
laOGObjCnt[lnRelPTPo] = lcRPInv $ 'BN'
= lfOGShowGet('llRPRelPT')


*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/02/2000
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
SELECT Customer
SET ORDER TO Customer
GO TOP
*-- End of lfsrAcc.

*!*************************************************************
*! Name      : lfsrPkt
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/02/2000
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
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


*!*************************************************************
*! Name      : lfFillVars
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/31/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************

FUNCTION lfFillVars

*-- Report support multi currency format open needed files.
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
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/14/1998
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

*-- Report support multi currency format close needed files.
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


*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfGtCurDes
*! Developer : Hend Ghanem (HBG)
*! Date      : 087/17/2000
*! Purpose   : Function Get the Description ofthe Currency
*!*************************************************************
*! Called from : .FRX
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
*-- end of lfGtCurDes.

*!**************************************************************************
*! Name      : lpCreaTemp
*! Developer : Hend Ghanem (HBG)
*! Date      : 16/05/2000 
*! Purpose   : Procedure to create Temp. File 
*!**************************************************************************
*! Example   : DO lpCreaTemp
*!**************************************************************************

PROCEDURE lpCreaTemp

*-- check If File is created or not
IF USED(lcPickTmp) AND RECCOUNT(lcPickTmp) > 0
  USE IN (lcPickTmp)

ENDIF
*-- Create File
IF !USED(lcPickTmp)
  IF TYPE("laTempStru[1,1]") $ "UL" 
    DIMENSION laTempStru[1,4]
    SELECT PIKTKT
    =AFIELDS(laTempStru)

    =lfAddField("laTempStru","cCurrCode","C",3,0)
    =lfAddField("laTempStru","cShipVia","C",6,0)
   
  ENDIF
  CREATE CURSOR (lcPickTmp) FROM ARRAY laTempStru          
  ZAP
  
  IF llMultCurr AND (lcRpCurr = "F") 
    *-- Index On Temp File  
    IF llRPPrtShp
      INDEX ON cCurrCode + cShipVia + PIKTKT  TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    ELSE
      INDEX ON cCurrCode + PIKTKT TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    ENDIF 
  ELSE
    *-- Index On Temp File
    IF llRPPrtShp  
      INDEX ON  cShipVia + PIKTKT  TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    ELSE
      INDEX ON  PIKTKT  TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
    ENDIF    
  ENDIF
  
ENDIF
*-- End of lpCreaTemp.

*!**************************************************************************
*! Name      : lpColect
*! Developer : Hend Ghanem (HBG)
*! Date      : 16/05/2000 
*! Purpose   : Procedure TO Colecte Data
*!**************************************************************************
*! Example   : Do lpColect()
*!**************************************************************************


PROCEDURE lpColect

SELECT PIKTKT
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SCAN FOR &lcRpExp
  SCATTER MEMVAR MEMO
  m.cCurrCode = ORDHDR.cCurrCode
  m.cShipVia  = IIF(ORDHDR.ShipVia = '*',Customer.ShipVia,ORDHDR.ShipVia)
  INSERT INTO (lcPickTmp) FROM MEMVAR
ENDSCAN

SET RELATION TO  
*-- end of lpColect.

*!**************************************************************************
*! Name      : lfAddField
*! Developer : Hend Ghanem (HBG)
*! Date      : 16/05/2000
*! Purpose   : Add fields to the the array of file structer
*!**************************************************************************
*! Example   : =lfAddField()
*!**************************************************************************


FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec
lnFldPos  = ALEN(&lcStruArry,1) + 1
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec
*-- end of lfAddField.

*!*************************************************************
*! Name      : lfvRepCurr
*! Developer : Hend Ghanem (HBG)
*! Date      : 087/17/2000
*! Purpose   : 
*!*************************************************************
*! Called from : .FRX
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

