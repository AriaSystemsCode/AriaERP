*:***************************************************************************
*: Program file  : ARCMLB
*: Program desc. : ACCOUNT RECEIVABLE CUSTOMER MAILING LIST
*! Date          : 02/22/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARCMLB
*:***************************************************************************
*B602734,1 BWA 04/06/99 Fix the bug of Error in label (made in .LBX file)
*B802426,1 Walid (WAM) 08/26/1999 Fix the bug Variable Ccomp_id not found .
*B802925,1 BWA 08/01/2000 Fix the bug of th third column doesn't show [Fix in LBX]
*B802925,1                Change the label layout from A4 to Letter.
*B603996,1 AAN 11/09/2000 Fix the bug of filter on invalid states.
*B606982,1 ABD 03/03/2003 Report Sorted by account priority incorrect.
*B120142,1 ABD 12/14/2003 Fix error "latempexp" Not found.
*:***************************************************************************
*-- laCompAdd Dimension used to get the Account,Name,Buyer,Addressess
*--           and shift any empty record 
DIMENSION laCompAdd[7]
laCompAdd = ''

*B120142,1 ABD - Define the "latempExpr" array to be global at this report not private. [Begin]
laTempExpr = ''
*B120142,1 ABD - [End] 

*-- llCalFad variable that is used to hold the shift function
llCalFad= ''

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPRINT
llPrint = .F.

**---- SELECT THE FILELS

SELECT Customer 
SET ORDER    TO
SET RELATION TO 
SET FILTER   TO
GOTO TOP

*-- lcTempFile  Variable that hold the temporary file name

lcTempFile   = gfTempName()       && NAME OF THE TEMPORARY FILE 
                  
*--- SORT TO TEMP INDEX

DO CASE 
**--- SUMMARY BY ACCOUNT  
  CASE lcRPSort = 'A'
    lcSortFld   = "ACCOUNT+TYPE+STORE"

**--- SUMMARY BY NAME    
  CASE lcRPSort='N'
    lcSortFld  ="STNAME+TYPE+STORE"
    
**--- SUMMARY BY STATE    
  CASE lcRPSort = 'S'
    lcSortFld   = "CADDRESS4+ACCOUNT+TYPE+STORE"
    
**--- SUMMARY BY REGION         
  CASE lcRPSort='R'
    lcSortFld  ='REGION+SALESREP+ACCOUNT+TYPE+STORE'

**--- SUMMARY BY ZIP
  CASE lcRPSort='Z'         
    lcSortFld  ='CADDRESS5+ACCOUNT+TYPE+STORE'
   
ENDCASE

*-- THE MAIN PROGRAM 
DO lpCollData

*-- PRINT THE LABEL  FROM THE lcTempFile
*B606982,1 ABD -  Remark the next lines.  [Begin]
*IF llPrint
*  DO gfDispRe WITH (lcRpForm),'',.F.,'L' 
*ENDIF
DO gfDispRe WITH (lcRpForm),'',.F.,'L' 
*B606982,1 ABD - [End]
  
IF USED  (lcTempFile)  
  SELECT (lcTempFile)
  USE 
ENDIF

*-- ERASE THE lcTempFile
ERASE (gcWorkDir+lcTempFile+'.DBF')
ERASE (gcWorkDir+lcTempFile,'.CDX')  

*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT 
*! Date      : 02/24/1999
*! Purpose   : COLLECT THE DATA 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : ......
*!*************************************************************

PROCEDURE lpCollData

*B606982,1 ABD -  Remark the next lines.  [Begin]
*SELECT Customer
*WAIT 'Selecting records for report ...' WINDOW NOWAIT
*LOCAT ALL FOR &lcRPExp
*IF EOF()
*  WAIT CLEAR 
*  WAIT 'No records selected for report' WINDOW 
*  RETURN
*ELSE
*  llPrint = .T. 
*  COPY REST TO &lcTempFile FOR &lcRPExp
*  =gfOpenFile(lcTempFile,' ','EX')
*  SELECT(lcTempFile)
*  
*  IF !EMPTY(lcSortFld)
*    Z=LTRIM(STR(RECCOUNT(),7))
*    WAIT 'Sorting &Z records for customer labels ' WINDOW NOWAIT
*    INDEX ON &lcSortFld TAG lcTempFile
*  ENDIF
*ENDIF  



*-- this file will release after every preveiw
SELECT Customer
lcOldOrder = ORDER()
SET ORDER TO CUSTOMER
=AFIELDS(laFilestru)
lnfilestru = ALEN(laFilestru,1)
CREATE DBF (gcWorkDir+lcTempFile) FROM ARRAY laFilestru
SELECT (lcTempFile)
IF !EMPTY(lcSortFld)
  INDEX ON &lcSortFld TAG lcTempFile
ENDIF



lcTempCust = gfTempName()       && NAME OF THE TEMPORARY FILE 
STORE '' TO lcAccntExp 
STORE .F. To llStores , llMain


lcTmpRpExp = lcRpExp
STORE '' TO lcMainExp , lcStoreExp
*-- Function to create an expration on the main an store level.
= lfCreatExp ()

IF EMPTY(lcAccntExp)
  *-- SELECT CUSTOMER FILE
  =gfOpenFile(gcDataDir+"CUSTOMER", "CUSTOMER", "SH",@lcTempCust,.T.)
ELSE
  SELECT CUSTOMER
  LOCATE FOR &lcAccntExp
  COPY REST TO &lcTempCust FOR &lcAccntExp .AND. Customer.Type = 'M'
  *-- Open the copy file.
  =gfOpenFile(lcTempCust,' ','EX')
  SELECT(lcTempCust)
  INDEX ON TYPE+ACCOUNT+STORE TAG lcTempCust
ENDIF

SELECT (lcTempCust)
=SEEK('M')
SCAN REST WHILE TYPE+ACCOUNT+STORE = 'M'
  WAIT WINDOW 'Collecting data For Account # ..' + Account NoWait
  *-- Case print main account.
  IF llMain
    IF SEEK('M'+Account,'CUSTOMER')
      SELECT CUSTOMER
      *-- Check For Main customer record.
      IF Eval(lcRpExp)
        SCATTER MEMVAR MEMO
        INSERT INTO (lcTempFile) FROM MEMVAR
      ENDIF
      SELECT (lcTempCust)
    ENDIF 
  ENDIF
  
  *-- Case Store Account.
  IF llStores
    *-- Check For Store customer record.
    IF SEEK('M'+Account,'CUSTOMER')
      SELECT CUSTOMER
      *-- Check for the main expration for main record.
      IF Eval(lcMainExp)
        *-- Check for the Store expration for main record.         
        IF SEEK('S'+Account,'CUSTOMER') 
          SELECT CUSTOMER
          SCAN REST WHILE TYPE+ACCOUNT+STORE = 'S'+ &lcTempCust..Account
            IF Eval(lcStoreExp)
              SCATTER MEMVAR MEMO
              INSERT INTO (lcTempFile) FROM MEMVAR
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
      SELECT (lcTempCust)
    ENDIF
  ENDIF
ENDSCAN

WAIT CLEAR
SELECT CUSTOMER
SET ORDER TO &lcOldOrder

SELECT (lcTempFile)
*-- End OF lpCollData

*B606982,1 ABD -  [End]
*!*************************************************************
*! Name      : lfvAcco
*! Developer : BASSEM RAFAAT 
*! Date      : 02/08/1999
*! Purpose   : Validate Account
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvAcco()
*!*************************************************************

FUNCTION lfvAcco

PRIVATE lcAccFld,lcAccount,lnSelcFile,lcCusTag

lcAccFld   = VARREAD()
lcAccount  = EVAL(lcAccFld)

lnSelcFile = SELECT(0)

SELECT Customer
lcCusTag  = ORDER('Customer')

SET ORDER TO TAG Customer IN Customer

IF !EMPTY(lcAccount).AND.('?' $ lcAccount .OR. !SEEK('M'+lcAccount,'Customer'))
  =CUSBROW(@lcAccount)
ENDIF

&lcAccFld = lcAccount

SET ORDER TO lcCusTag

SELECT (lnSelcFile)

*!*************************************************************
*! Name      : lfvSales
*! Developer : BASSEM RAFAAT 
*! Date      : 02/22/1999
*! Purpose   : Validate SalesRep
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSales()
*!*************************************************************

FUNCTION lfvSales

PRIVATE lcSalFld,lcSales,lnSelcFile,lcSalTag,laTemp,lcBrFields

lcSalFld = VARREAD()
lcSales  = EVAL(lcSalFld)

lnSelcFile = SELECT(0)

SELECT Salesrep
lcSalTag  = ORDER('Salesrep')

SET ORDER TO TAG Salesrep

IF !EMPTY(lcSales).AND.('?' $ lcSales .OR. !SEEK(lcSales , 'Salesrep')) 

  DIMENSION laTemp[1]
  laTemp = ''     

  lcBrFields = "RepCode :H = 'Sales Rerasentitive Code',;
                Name    :H = 'Name'"
               
   = gfBrows('','REPCODE','laTemp')
     
  IF !EMPTY(laTemp[1])
    lcSales = laTemp[1]
  ELSE 
    lcSales = ''
  ENDIF
ENDIF

&lcSalFld = lcSales
SET ORDER TO lcSalTag
SELECT (lnSelcFile)
*!*************************************************************
*! Name      : lfvFactor
*! Developer : BASSEM RAFAAT 
*! Date      : 02/22/1999
*! Purpose   : Validate Factor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvFactor()
*!*************************************************************

FUNCTION lfvFactor

PRIVATE lcFactFld,lcFactor,lnSelcFile,lcOldTagF,lcBrFields

lcFactFld = VARREAD()
lcFactor  = EVAL(lcFactFld)

lnSelcFile = SELECT(0)

SELECT SycFact

lcOldTagF  = order('SycFact')   
SET ORDER TO cFacCode IN SycFact

IF !EMPTY(lcFactor) .AND.('?' $ lcFactor .OR. !SEEK(lcFactor,'SycFact'))
  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcFactor= IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))
ENDIF
&lcFactFld = lcFactor

SET ORDER TO  lcOldTagF

SELECT (lnSelcFile)

*!*************************************************************
*! Name      : lfvPrior
*! Developer : BASSEM RAFAAT 
*! Date      : 02/22/1999
*! Purpose   : Validate Priorty
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvPrior()
*!*************************************************************

FUNCTION lfvPrior

PRIVATE lcPrifld,lcPrior,lcOldTagP


lcPrifld =  VARREAD()
lcPrior  = EVAL(lcPrifld)

IF !(lcPrior  $ "123456789")
  WAIT WINDOW "Range 1 : 9"
ENDIF

&lcPrifld = lcPrior 

*!*************************************************************
*! Name      : lfStitle
*! Developer : Samah Wilson (SWK)
*! Date      : 08/05/1998
*! Purpose   : Get the variable to be used in the sort options
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************

FUNCTION lfStitle
PARAMETER lcX

IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ENDIF

SET ORDER TO Ccomp_id  IN SYCCOMP   && To use it to get state title.
SET ORDER TO CCONTCODE IN SYCINT   && To use it to get state title. IN SYCCOMP   && To use it to get state title.
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')

DO CASE 
  CASE lcX = "S" 
    lcReturn = (SYCINT.CPART4LAB)

  CASE lcX = "Z" 
    lcReturn   = (SYCINT.CPART5LAB)
ENDCASE

RETURN lcReturn

*!*************************************************************
*! Name      : lfvStates 
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/09/98
*! Purpose   : Validate the state code
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [State  Object]- title is changes with country.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStates()
*!*************************************************************
FUNCTION lfvStates
PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal

lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the state code he entered is not in the file.
*B802426,1 Ccomp_id field was removed from codes file .[Start]
*IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK(gcAct_Comp+PADR(lcStateVal,6)+'N'+'STATE','CODES'))
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK(PADR(lcStateVal,6)+'N'+'STATE','CODES'))
*B802426,1 Ccomp_id field was removed from codes file .[End..]
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = 'State Codes'
   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'
   *B802426,1 [Start]
   *IF gfBrows('FOR ccomp_id+cfld_name+ccode_no+cdiscrep = ;
   *  gcAct_Comp+"STATE" AND ;
   *  cRltField="N"','cCode_No,cDiscrep','laCodeFld')
   *  lcStateVal = laCodeFld[1]
   *ENDIF
   IF gfBrows('FOR cfld_name+ccode_no+cdiscrep = ;
     "STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')
     lcStateVal = laCodeFld[1]
   *B603996,1 AAN If a user not select any thing from a browse window[Start].
   ELSE
     lcstateval=""  
   *B603996,1 AAN If a user not select any thing from a browse window[Start].[End].
   ENDIF
   *B802426,1 [End..]
   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF    
*-- If user type inValid code does not have ? Accept it.
*B603996,1 AAN Remove this option[Start].
*lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
*B603996,1 AAN Remove this option[End].
&lcStateObj = lcStateVal
*-- end of lfvStates.

*!*************************************************************
*! Name      : lfShift
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/24/99
*! Purpose   : Use Variabel to hold the function
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfAdrShift
*!*************************************************************
*! Called from : report arcmlbb.frx 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfShift()
*!*************************************************************
FUNCTION lfShift

laCompAdd[1] = ALLTRIM(&lcTempFile..Account)
laCompAdd[2] = IIF(lcRPBur="Y",ALLTRIM(&lcTempFile..Buyer),"")
laCompAdd[3] = ALLTRIM(&lcTempFile..Stname)
laCompAdd[4] = gfGetAdr(lcTempFile , '' , '' , '' , 1)
laCompAdd[5] = gfGetAdr(lcTempFile , '' , '' , '' , 2)
laCompAdd[6] = gfGetAdr(lcTempFile , '' , '' , '' , 3)
laCompAdd[7] = gfGetAdr(lcTempFile , '' , '' , '' , 4)
=lfAdrShift('laCompAdd')

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/24/99
*! Purpose   : used to shift the lacompadd array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : variable from report arcmlbb.frx 
*!*************************************************************
*! Passed Parameters  : lcArrayNam
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
*--HDM =>FOR lnCount = 1 TO 5
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*B606982,1 ABD -  [Begin]
*!*************************************************************
*! Name      : lfVrItmPos
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Postion in Variable filter array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Program.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAdrShift()
*!*************************************************************
*!
FUNCTION lfVrItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGVrFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGVrFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfVrItmPos.
*!*************************************************************
*! Name      : lfFxItmPos
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Postion in fixed filter array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Program.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfFxItmPos()
*!*************************************************************
*!
FUNCTION lfFxItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfFxItmPos.
*!*************************************************************
*! Name      : lfItmVrExp
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Item Filter Expr.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Program.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfItmVrExp()
*!*************************************************************
*
FUNCTION lfItmVrExp

PARAMETERS lnCurrFilt

*B120142,1 ABD - Define the "latempExpr" array to be global at this report not private. [Begin]
*PRIVATE lnCurrFilt , lnNoOfCols , laTempExpr , lcFilter
PRIVATE lnCurrFilt , lnNoOfCols , lcFilter
*B120142,1 ABD - [End]

lnNoOfCols = ALEN(laOGFxFlt,2)
DIMENSION laTempExpr[1,lnNoOfCols]

lnCurrFilt = (lnCurrFilt - 1) * lnNoOfCols + 1
=ACOPY(laOGVrFlt,laTempExpr,lnCurrFilt,lnNoOfCols,1)
lcFilter = gfGenFlt('laTempExpr',.T.,.T.)

RETURN lcFilter

*-- End of lfItmVrExp.
*!*************************************************************
*! Name      : lfItmFxExp
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Item Filter Expr.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Program.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfItmFxExp()
*!*************************************************************
*
FUNCTION lfItmFxExp

PARAMETERS lnCurrFilt
PRIVATE lnCurrFilt , lnNoOfCols , laTempExpr , lcFilter
lnNoOfCols = ALEN(laOGFxFlt,2)
DIMENSION laTempExpr[1,lnNoOfCols]

lnCurrFilt = (lnCurrFilt - 1) * lnNoOfCols + 1
=ACOPY(laOGFxFlt,laTempExpr,lnCurrFilt,lnNoOfCols,1)
lcFilter = gfGenFlt('laTempExpr',.T.,.T.)

RETURN lcFilter

*-- End of lfItmFxExp.
*!*************************************************************
*! Name      : lfItmFxExp
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Item Filter Expr.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Program.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfItmFxExp()
*!*************************************************************
*
FUNCTION lfCreatExp


*-- Account Exp.
lnAccntPos = lfVrItmPos('CUSTOMER.ACCOUNT')
lcAccntExp = lfItmVrExp(lnAccntPos)

*-- Customer Type Position.
lnCustTPos  = lfFxItmPos('CUSTOMER.TYPE')
lcCustTExp  = lfItmFxExp(lnCustTPos)

IF !EMPTY(lcCustTExp)
  IF '"S"' $ lcCustTExp
    llStores = .T.
  ENDIF
  
  IF '"M"' $ lcCustTExp
    llMain = .T.
  ENDIF
ELSE
  STORE .T. To llStores , llMain    
ENDIF

lcTmpRpExp = STRTRAN(lcTmpRpExp,lcCustTExp,'.T.')
*-- Get the type Expr.

IF llStores
  lcStoreExp = lcStoreExp + IIF(EMPTY(lcStoreExp),'',' .AND. ' ) + ' CUSTOMER.TYPE = "S"'
ENDIF


*-- Customer Status Position.
lnStatsPos = lfFxItmPos('CUSTOMER.STATUS')
lcStatsExp  = lfItmFxExp(lnStatsPos)
IF !EMPTY(lcStatsExp)
  lcTmpRpExp = STRTRAN(lcTmpRpExp,lcStatsExp,'.T.')
  lcStoreExp = lcStoreExp + IIF(EMPTY(lcStoreExp),'',' .AND. ' ) + lcStatsExp
ENDIF

*-- Customer Sales rep Position.
lnSalsPos = lfFxItmPos('CUSTOMER.SALESREP')
lcSalsExp = lfItmFxExp(lnSalsPos)
IF !EMPTY(lcSalsExp)
  lcTmpRpExp = STRTRAN(lcTmpRpExp,lcSalsExp,'.T.')
  lcStoreExp = lcStoreExp + IIF(EMPTY(lcStoreExp),'',' .AND. ' ) + lcSalsExp
ENDIF


*-- Customer State Position.
lnstatPos = lfFxItmPos('CUSTOMER.CADDRESS4')
lcStatExp = lfItmFxExp(lnstatPos)
IF !EMPTY(lcStatExp)
  lcTmpRpExp = STRTRAN(lcTmpRpExp,lcStatExp,'.T.')
  lcStoreExp = lcStoreExp + IIF(EMPTY(lcStoreExp),'',' .AND. ' ) + lcStatExp
ENDIF


*-- Customer Zip Position.
lnZipPos = lfFxItmPos('CUSTOMER.CADDRESS5')
lcZipExp = lfItmFxExp(lnZipPos)
IF !EMPTY(lcZipExp)
  lcTmpRpExp = STRTRAN(lcTmpRpExp,lcZipExp,'.T.')
  lcStoreExp = lcStoreExp + IIF(EMPTY(lcStoreExp),'',' .AND. ' ) + lcZipExp
ENDIF

lcMainExp = lcTmpRpExp

*-- End OF lfCreatExp
*B606982,1 ABD -  [End]
*!*************************************************************