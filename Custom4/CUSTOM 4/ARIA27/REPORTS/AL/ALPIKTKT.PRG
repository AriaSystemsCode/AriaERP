*:***************************************************************************
*: Program file  : ALPIKTKT
*: Program desc. : MOMENTRENDS ALLOCATION PIKTICKT LOG
*! Date          : 08/11/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT (BWA)
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
*: This Report Program is due to C101593 ...
*:***************************************************************************
*: E500322,1 SAM 01/02/2000 add filter to select Warehouses in multi warehouses (add in syrepuvr)
*: B603470,1 SAM 02/23/2000 Display decimal point in total quantity and grand total quantity ,
*: B603470,1                Modification in AlPikTkt.Frx
*: B603430,1 BWA 07/02/2000 FIX THE BUG OF THE VALIDATION OF THE FILTERS.
*: B603470,1 BWA 04/12/2000 Fix the bug of the amount field display ***** and the 
*: B603470,1                total qty field dispaly decimal. [The fix in the FRX]
*: E301439,1 HBG 08/06/2000 Add an option to allow different currencies
*: B604747,1 ASH 07/30/2001 Fix the bug of 'Alias not found'.
*: B605011,1 HBG 17/10/2001 Add field to check if end of report or not.
*: B605255,1 BWA 01/20/2002 Fix the bug of the order# doesn't print correctly.[Fix in FRX]
*: B607266,1 ABD 07/01/2003 ignore Empty piktkt or equal to  '******' [Begin]
*: B120552,1 MMM 11/17/2003 Fix the bug of displaying  released picking tickets with "Yes" 
*:                          in Invcd column. [The fix in the FRX]   
*:***************************************************************************

*--Section of Variables
lcTime = TIME()         && Variable to hold the Time
*To get the date in a variables
lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PIKTKT.DATE'),1)
LDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
HDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))


IF llOGFltCh
  DO lpCreaTemp  && Create Temp Cursor
  DO lpColect    && Collect data
ENDIF


*E301439,1 HBG 08/08/2000 Print from Temp file insted of master file [Begin]
*SELECT PIKTKT
SELECT (lcPickTmp)
SET ORDER TO TAG &lcPickTmp
*E301439,1 [End]

*B605011,1 HBG 17/10/2001 Replace the field lEndRep with true to say this is end of report [Begin]
llEndReprt = .F.
GO BOTTOM
REPLACE &lcPickTmp..lEndRep WITH .T.
LOCATE
*B605011,1 [End] 
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SET ORDER TO PikLine  IN PikLine 
SET ORDER TO OrdLinst IN OrdLine
LOCATE 

lcCurrExpr = IIF(llMultCurr AND lcRpCurr="F",[ORDHDR.CCURRCODE],[gcBaseCurr])
lcCurrCode = EVALUATE(lcCurrExpr)
llCurrChg  = .F.

*E301439,1 HBG 08/08/2000 Data collected for 'lcRpExp' so there is no need to print for it [Begin]
*DO gfDispRe WITH EVALUATE('lcRpName') , 'FOR ' + lcRpExp
DO gfDispRe WITH EVALUATE('lcRpName') 

*-- Clear relation [Begin]
SELECT (lcPickTmp)
SET RELATION TO
*E301439,1 [End]

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

*E301439,1 HBG 08/08/2000 use temp file insted of master file [Begin]
*IF Piktkt.Status $ 'CX'
IF &lcPickTmp..Status $ 'CX'
*E301439,1 [End]
  SELECT PikLine
  LOCATE
  
  *E301439,1 HBG 08/08/2000 use temp file insted of master file [Begin]
  *=SEEK(PikTkt.PikTkt)
  =SEEK(&lcPickTmp..PikTkt)
  
  
  *SUM &lcExpList REST WHILE PikTkt+Order+STR(LineNo,6) = PikTkt.PikTkt ;
      TO lnTotQty,lnTotAmnt 
  SUM &lcExpList REST WHILE PikTkt+Order+STR(LineNo,6) = &lcPickTmp..PikTkt ;
      TO lnTotQty,lnTotAmnt 
  *E301439,1 [End]      
      
ELSE
  SELECT OrdLine
  LOCATE
  *E301439,1 HBG 08/08/2000 use temp file insted of master file [Begin]
  *=SEEK('O'+PikTkt.Order+PikTkt.Store)
  =SEEK('O'+&lcPickTmp..Order+&lcPickTmp..Store)
    
  *SUM &lcExpList REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                            'O'+PikTkt.Order+PikTkt.Store ;
                      FOR PikTkt = PikTkt.PikTkt TO lnTotQty,lnTotAmnt 
  SUM &lcExpList REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                            'O'+&lcPickTmp..Order+&lcPickTmp..Store ;
                      FOR PikTkt = &lcPickTmp..PikTkt TO lnTotQty,lnTotAmnt 
  *E301439,1 [End]                                           
ENDIF

*E301439,1 HBG Get the Amount if Multi currency and it is not the foregin currency
*E301439,1     And calculate the Grand total if only currency not changed [Begin]
IF llMultCurr AND (lcRpCurr <> "F") AND;
   (lnTotAmnt <> 0) AND (ORDHDR.CCURRCODE <> gcBaseCurr)
  lnTotAmnt = gfAmntDisp(lnTotAmnt,lcRpCurr,ldRpExDate,;
                         lcRpTmpNam,.F.,"ORDHDR")
ENDIF

*lnGrTotQty = lnGrTotQty + lnTotQty
*lnGrTotAmt = lnGrTotAmt + lnTotAmnt
IF !llCurrChg
  llCurrChg = !(EVALUATE(lcCurrExpr) == lcCurrCode)
  lcCurrCode = EVALUATE(lcCurrExpr)
ENDIF  
*lnGrTotAmt = IIF(llCurrChg,0,lnGrTotAmt + lnTotAmnt)
*E301439,1 [End]

*B605011,1 HBG 17/10/2001 Flag to check if end of report or not [Begin]
llEndReprt =  &lcPickTmp..lEndRep 
*B605011,1 [End]
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

*B603430,1 BWA 07/02/2000 FIX THE BUG OF THE VALIDATION OF THE FILTERS[START]
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
*B603430,1 BWA 07/02/2000 [END]

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
*E301439,1
FUNCTION lfFillVars

*E301424,1 ABD- Report support multi currency format open needed files.[begin]
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
*! *E301439,1

FUNCTION lfClearRep
llClearFn = .T.  &&You erase temporary file.
*-- Close temp. opended files, if it used.

*Report support multi currency format close needed files.[begin]
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
*[End]

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
*!*E301439,1
FUNCTION lfGtCurDes

IF lcRpCurr <> "F"
  RETURN gcBaseCurr
ELSE
  *B604747,1 ASH 07/30/2001 (Begin) Fix the bug of 'Alias not found'
  *IF SEEK(ORDHDR.cCurrCode,'SYCCURR')
  IF llMultCurr and SEEK(ORDHDR.cCurrCode,'SYCCURR')
  *B604747,1 ASH 07/30/2001 (End)
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
*!E301439,1 

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
    *B605011,1 HBG 17/10/2001 Add field to check if end of report or not [Begin]
    =lfAddField("laTempStru","lEndRep","L",1,0)
    *B605011,1 [End]
   
  ENDIF
  CREATE CURSOR (lcPickTmp) FROM ARRAY laTempStru          
  ZAP
  
  IF llMultCurr AND (lcRpCurr = "F") 
    *-- Index On Temp File  
    INDEX ON cCurrCode + PIKTKT TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
  ELSE
    *-- Index On Temp File  
    INDEX ON  PIKTKT  TAG (lcPickTmp) OF (gcWorkDir + lcPickTmp + ".CDX")
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
*!E301439,1 

PROCEDURE lpColect

SELECT PIKTKT
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

*B607266,1 ABD - ignore Empty piktkt or equal to  '******' [Begin]
*SCAN FOR &lcRpExp
SCAN FOR &lcRpExp .AND. Piktkt # '******' .AND. !EMPTY(Piktkt)
  *B607266,1 ABD - [End]

  SCATTER MEMVAR MEMO
  m.cCurrCode = ORDHDR.cCurrCode
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
*!E301439,1 

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
*!*E301439,1
FUNCTION lfvRepCurr

llOGFltCh = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

