*:***************************************************************************
*: Program file       : ARCMLB
*: Program desc.      : ACCOUNT RECEIVABLE CUSTOMER MAILING LABEL
*! Module             : ACCOUNT RECEIVABLE (AR)
*: Developer          : Noha Mohamed Mostafa(NMM)
*: Tracking Job Number: #037442
*: Date               : 01/20/2004
*:***************************************************************************
*: Calls :
*:        Programs : ........
*:        Screens  : ........
*:        Global Functions :  gfTempName ,gfDispRe ,gfOpenFile ,gfBrows ,gfGetAdr,
*:                            gfPhoneTem ,gfGenFlt ,CUSBROW ,ARIABROW
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Called From        : .....
*:**********************************************************************
*: Example : DO ARCMLB
*:***************************************************************************
*: Modifications :E037739,1,NMM,01/27/2004,Add New Record to syrepuvr to change from inlist 
*:                to inrange in 'A4'with cver='A40'and assign cver value 'A27' for the old record.	
*: B609113,1 MMT 12/20/2009 Convert state option to be mover instaed of browse[T20091207.0001]
*: B609134,1 MMT 02/04/2010 Fix bug of error if no State is selected[T20091103.0034]
*: B609175,1 HES 03/17/2010 Fix Bug when choosing active customers, canceled customers show [T20100302.0013]
*: B609757,1 MMT 11/29/2011 Customer Mailing label does not filter by Zip Code[T20111025.0039][T20110617.0003]
*:***************************************************************************
#INCLUDE R:\Aria4xp\reports\arcmlb.h
*-- laCompAdd :Dimension used to get the Account,Name,Buyer,Addressess and shift any empty record

LOCAL lcSelectCommand ,lnResult ,lcSelectCommand1 ,lnResult1
DIMENSION laCompAdd[7]
laCompAdd = ''

*B120142,1 ABD - Define the "latempExpr" array to be global at this report not private. [Begin]
laTempExpr = ''
*B120142,1 ABD - [End]

*-- llCalFad variable that is used to hold the shift function
llCalFad= ''

*-- llPrint variable that is used to prevent printing if there is not
*-- any record matches the report criteria
PRIVATE llPRINT
llPrint = .F.

**---- SELECT THE FILES
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
*-Heba
lcPhonePic = gfPhoneTem()
*-Heba

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
ERASE (oAriaApplication.WorkDir+lcTempFile+'.DBF')
ERASE (oAriaApplication.WorkDir+lcTempFile,'.CDX')

*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT
*! Date      : 02/24/1999
*! Purpose   : COLLECT THE DATA
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
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
CREATE DBF (oAriaApplication.WorkDir+lcTempFile) FROM ARRAY laFilestru
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
  =gfOpenFile(oAriaApplication.DataDir+"CUSTOMER", "CUSTOMER", "SH",@lcTempCust,.T.)
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
  *WAIT WINDOW 'Collecting data For Account # ..' + Account NoWait
  WAIT WINDOW LANG_Arcmlb_waitwindow1 + Account NoWait
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
      *-- Check for the main expression for main record.
     IF Eval(lcMainExp)
        *-- Check for the Store expression for main record.
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
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*due to Enhancement #037739,This Function Is No Longer Used & lfAccSet is used instead
*!*	FUNCTION lfvAcco
*!*	PRIVATE lcAccFld,lcAccount,lnSelcFile,lcCusTag
*!*	lcAccFld   = OGVARREAD()
*!*	lcAccount  = EVAL(lcAccFld)
*!*	lnSelcFile = SELECT(0)
*!*	SELECT Customer
*!*	lcCusTag  = ORDER('Customer')
*!*	SET ORDER TO TAG Customer IN Customer
*!*	IF !EMPTY(lcAccount).AND.('?' $ lcAccount .OR. !SEEK('M'+lcAccount,'Customer'))
*!*	  =CUSBROW(@lcAccount)
*!*	ENDIF
*!*	&lcAccFld = lcAccount
*!*	SET ORDER TO lcCusTag
*!*	SELECT (lnSelcFile)
FUNCTION lfAccSet

PARAMETERS opGrdparam

DO CASE
  CASE opGrdparam = 'S' 
    SELECT Customer
    SET ORDER TO TAG Customer IN Customer
    GO TOP
  CASE opGrdparam = 'R'
    SELECT Customer
    SET ORDER  TO 
ENDCASE

*!*************************************************************
*! Name      : lfvSales
*! Developer : BASSEM RAFAAT
*! Date      : 02/22/1999
*! Purpose   : Validate SalesRep
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvSales

PRIVATE lcSalFld,lcSales,lnSelcFile,lcSalTag,laTemp,lcBrFields
 
lcSalFld = OGVARREAD()
lcSales  = EVAL(lcSalFld)

lnSelcFile = SELECT(0)

SELECT Salesrep
lcSalTag  = ORDER('Salesrep')

SET ORDER TO TAG Salesrep

IF !EMPTY(lcSales).AND.('?' $ lcSales .OR. !SEEK(lcSales , 'Salesrep'))

  DIMENSION laTemp[1]
  laTemp = ''

  *lcBrFields = "RepCode :H = 'Sales Representitive Code',Name :H = 'Name'"
  lcBrFields = [REPCODE :R:H =LANG_Arcmlb_SRepCode ,Name :R:H =LANG_Arcmlb_Name ] 
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
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvFactor

PRIVATE lcFactFld,lcFactor,lnSelcFile,lcOldTagF,lcBrFields

lcFactFld = OGVARREAD()
lcFactor  = EVAL(lcFactFld)

lnSelcFile = SELECT(0)

SELECT SycFact

lcOldTagF  = order('SycFact')
SET ORDER TO cFacCode IN SycFact

IF !EMPTY(lcFactor) .AND.('?' $ lcFactor .OR. !SEEK(lcFactor,'SycFact'))
  *lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  lcBrFields  = [cFacCode:R:H= LANG_Arcmlb_Fac ,cFacComp:R:H= LANG_Arcmlb_Name ,cFacCont:R:H= LANG_Arcmlb_Contact ,cPhoneNo :P= gfPhoneTem() :H= LANG_Arcmlb_Phone]
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
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvPrior

PRIVATE lcPrifld,lcPrior,lcOldTagP

lcPrifld =  OGVARREAD()
lcPrior  = EVAL(lcPrifld)

IF !(lcPrior  $ "123456789")
  *WAIT WINDOW "Range 1 : 9"
  WAIT WINDOW LANG_Arcmlb_Range
ENDIF

&lcPrifld = lcPrior

*!*************************************************************
*! Name      : lfStitle
*! Developer : Noha Mohamed(NMM)
*! Date      : 08/05/1998
*! Purpose   : Get the variable to be used in the sort options
*!*************************************************************
*! Passed Parameters : lcX
*!*************************************************************

FUNCTION lfStitle

PARAMETER lcX
PRIVATE lnSelectAlias

lnSelectAlias=SELECT()

IF !USED('SYCINT')
  = gfOpenFile(oAriaApplication.SysPath +'SYCINT',oAriaApplication.SysPath +'Ccontcode','SH')
ENDIF

IF !USED('SYCCOMP')
  = gfOpenFile(oAriaApplication.SysPath +'SYCCOMP',oAriaApplication.SysPath +'Ccomp_id','SH')
ENDIF
*SET ORDER TO Ccomp_id  IN SYCCOMP   && To use it to get state title.
*SET ORDER TO CCONTCODE IN SYCINT   && To use it to get state title. IN SYCCOMP   && To use it to get state title.
*= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
*DO CASE
*  CASE lcX = "S"
*    lcReturn = (SYCINT.CPART4LAB)

*  CASE lcX = "Z"
*    lcReturn   = (SYCINT.CPART5LAB)
*ENDCASE
*RETURN lcReturn
lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
IF lnResult>=1
   lcSelectCommand1=[SELECT * FROM SYCINT WHERE CCONT_CODE=SYCCOMP.CCONT_CODE] 
   lnResult1= oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
   IF lnResult1 >=1
     DO CASE
       CASE lcX = "S"
          lcReturn = (SYCINT.CPART4LAB)
       CASE lcX = "Z"
         lcReturn   = (SYCINT.CPART5LAB)
     ENDCASE
     RETURN lcReturn
  ENDIF 
ENDIF 

SELECT(lnSelectAlias)

*!*************************************************************
*! Name      : lfvStates
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/09/98
*! Purpose   : Validate the state code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*due to Enhancement #037739,This Function Is No Longer Used and Function lfStatSet is used instead
*!*	FUNCTION lfvStates

*!*	PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal

*!*	lcStateObj = OGSYS18()                    && Varible to hold  the name of the memory variable used to create the current GET field
*!*	lcStateVal = EVALUATE(OGSYS18())  && Varible to hold  the value of the current GET field

*!*	lnCurAlias = SELECT(0)

*!*	*--IF The user want to Browse or if the state code he entered is not in the file.
*!*	IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK(PADR(lcStateVal,6)+'N'+'STATE','CODES')) 
*!*	   SELECT CODES
*!*	   DECLARE laCodeFld[2]
*!*	   lcFile_Ttl = 'State Codes'
*!*	   *lcFile_Ttl = LANG_Arcmlb_FileTtl   
*!*	   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'
*!*	   *lcBrfields = 'cCode_No :H= LANG_Arcmlb_StateCode  ,cDiscrep :H= LANG_Arcmlb_Desc :30'
*!*	   IF gfBrows('FOR cfld_name+ccode_no+cdiscrep ="STATE" AND cRltField="N"','cCode_No,cDiscrep','laCodeFld')
*!*	     lcStateVal = laCodeFld[1]
*!*	   ELSE
*!*	     lcstateval=""
*!*	   ENDIF
*!*	   SELECT (lnCurAlias)
*!*	ENDIF

*!*	&lcStateObj = lcStateVal
*!*************************************************************
FUNCTION lfstatSet

PARAMETERS opGrdparam

DO CASE
  CASE opGrdparam = 'S' 
    SELECT Codes
    SET FILTER TO cfld_name+ccode_no+cdiscrep ="STATE" AND cRltField="N"
    GO TOP
  CASE opGrdparam = 'R'
    SELECT Codes
    SET FILTER TO 
ENDCASE
*-- End of lfstatSet

*!*************************************************************
*! Name      : lfShift
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/24/99
*! Purpose   : Use Variabel to hold the function
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
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
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************

FUNCTION lfVrItmPos

PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loOGScroll.laOGVrFlt,lcItmInFlt)

IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnItmPos,1)
ENDIF

RETURN lnItmPos

*!*************************************************************
*! Name      : lfFxItmPos
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Postion in fixed filter array.
*!*************************************************************
*! Called from : Program.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************

FUNCTION lfFxItmPos

PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loOGScroll.laOGFxFlt,lcItmInFlt)

IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF

RETURN lnItmPos

*!*************************************************************
*! Name      : lfItmVrExp
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Item Filter Expr.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************

FUNCTION lfItmVrExp

PARAMETERS lnCurrFilt

*B120142,1 ABD - Define the "latempExpr" array to be global at this report not private. [Begin]
*PRIVATE lnCurrFilt , lnNoOfCols , laTempExpr , lcFilter
PRIVATE lnCurrFilt , lnNoOfCols , lcFilter
*B120142,1 ABD - [End]

lnNoOfCols = ALEN(loOGScroll.laOGFxFlt,2)
DIMENSION laTempExpr[1,lnNoOfCols]

lnCurrFilt = (lnCurrFilt - 1) * lnNoOfCols + 1
=ACOPY(loOGScroll.laOGVrFlt,laTempExpr,lnCurrFilt,lnNoOfCols,1)

lcFilter = gfGenFlt('laTempExpr',.T.,.T.)

RETURN lcFilter

*!*************************************************************
*! Name      : lfItmFxExp
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Item Filter Expr.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************

FUNCTION lfItmFxExp

PARAMETERS lnCurrFilt
PRIVATE lnCurrFilt , lnNoOfCols , laTempExpr , lcFilter

lnNoOfCols = ALEN(loOGScroll.laOGFxFlt,2)
DIMENSION laTempExpr[1,lnNoOfCols]

lnCurrFilt = (lnCurrFilt - 1) * lnNoOfCols + 1
=ACOPY(loOGScroll.laOGFxFlt,laTempExpr,lnCurrFilt,lnNoOfCols,1)

lcFilter = gfGenFlt('laTempExpr',.T.,.T.)

RETURN lcFilter

*!*************************************************************
*! Name      : lfItmFxExp
*! Developer : Abdou Elgendy (ABD)
*! Date      : 03/03/2003
*! Purpose   : Item Filter Expr.
*!*************************************************************
*! Passed Parameters  : Item Position
*!*************************************************************
*! Returns            : None
*!*************************************************************

FUNCTION lfCreatExp

*-- Account Expression
lnAccntPos = lfVrItmPos('CUSTOMER.ACCOUNT')
lcAccntExp = lfItmVrExp(lnAccntPos)

*-- Customer Type Position
lnCustTPos  = lfFxItmPos('CUSTOMER.TYPE')
lcCustTExp  = lfItmFxExp(lnCustTPos)

IF !EMPTY(lcCustTExp)
  IF '"S"' $ lcCustTExp OR "'S'" $ lcCustTExp
    llStores = .T.
  ENDIF

  IF '"M"' $ lcCustTExp OR "'M'" $ lcCustTExp
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
  *: B609175,1 HES 03/17/2010 Fix Bug when choosing active customers, canceled customers show [Start]
  *lcTmpRpExp = STRTRAN(lcTmpRpExp,lcStatsExp,'.T.')
  *: B609175,1 HES 03/17/2010 Fix Bug when choosing active customers, canceled customers show [End  ]
  lcStoreExp = lcStoreExp + IIF(EMPTY(lcStoreExp),'',' .AND. ' ) + lcStatsExp
ENDIF

*-- Customer Sales rep Position.
lnSalsPos = lfFxItmPos('CUSTOMER.SALESREP')
lcSalsExp = lfItmFxExp(lnSalsPos)

IF !EMPTY(lcSalsExp)
  lcTmpRpExp = STRTRAN(lcTmpRpExp,lcSalsExp,'.T.')
  lcStoreExp = lcStoreExp + IIF(EMPTY(lcStoreExp),'',' .AND. ' ) + lcSalsExp
ENDIF

*: B609134,1 MMT 02/04/2010 Fix bug of error if no State is selected[Start]
lcStatExp = ''
*: B609134,1 MMT 02/04/2010 Fix bug of error if no State is selected[End]

*-- Customer State Position.
lnstatPos = lfFxItmPos('CUSTOMER.CADDRESS4')
*: B609113,1 MMT 12/20/2009 Convert state option to be mover instaed of browse[Start]
*lcStatExp = lfItmFxExp(lnstatPos)
llUseState  = .F.
IF lnstatPos > 0 
  lcStatSel =IIF(!EMPTY(laOgFxFlt[lnstatPos ,6]),laOgFxFlt[lnstatPos ,6],'')
  IF !EMPTY(lcStatSel) 
    lcStatFile = loOGScroll.gfTempName()
    llUseState  = IIF(LEN(lcStatSel)>0,.T.,.F.) AND lfConvertToCursor(lcStatSel ,'STATE',lcStatFile)
    IF llUseState 
      lnStatStart = AT('INLIST(CUSTOMER.CADDRESS4',lcTmpRpExp)
      IF lnStatStart > 0
         lnEndPos = AT(")",SUBSTR(lcTmpRpExp,lnStatStart ))+lnStatStart -1
         lnNumChar = lnEndPos- lnStatStart +1
         lcTmpRpExp= STUFF(lcTmpRpExp,lnStatStart ,lnNumChar,"Seek(CUSTOMER.CADDRESS4,'&lcStatFile')")
	  ENDIF
	  lnStatStart =  0
      lnStatStart = AT('INLIST(CUSTOMER.CADDRESS4',lcRpExp)
      IF lnStatStart > 0
         lnEndPos = AT(")",SUBSTR(lcRpExp,lnStatStart ))+lnStatStart -1
         lnNumChar = lnEndPos- lnStatStart +1
         lcRpExp= STUFF(lcRpExp,lnStatStart ,lnNumChar,"Seek(CUSTOMER.CADDRESS4,'&lcStatFile')")
	  ENDIF
      lcStatExp = "Seek(CUSTOMER.CADDRESS4,'&lcStatFile')"
      
    ENDIF     
  ENDIF   
ENDIF   
*: B609113,1 MMT 12/20/2009 Convert state option to be mover instaed of browse[End]

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

*B606982,1 ABD -  [End]
*: B609113,1 MMT 12/20/2009 Convert state option to be mover instaed of browse[Start]
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 12/20/2009
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'STATE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 30
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
*: B609113,1 MMT 12/20/2009 Convert state option to be mover instaed of browse[End]

*: B609757,1 MMT 11/29/2011 Customer Mailing label does not filter by Zip Code[Start]
*!*************************************************************
*! Name      : lfZIPVLD  
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/29/2011
*! Purpose   : Validate Zip Code
*!*************************************************************
FUNCTION lfZIPVLD  
lPARAMETERS lpGrdparam
IF lpGrdparam = 'S' 
  IF !USED(lcZIPTmp)
    DIMENSION laZIPCODEAr[1,4]
    laZIPCODEAr[1,1] = 'caddress5'
    laZIPCODEAr[1,2] = 'C'
    laZIPCODEAr[1,3] = 30
    laZIPCODEAr[1,4] = 0
     = gfCrtTmp(lcZIPTmp,@laZIPCODEAr, 'caddress5',lcZIPTmp,.T.)
    IF !USED('Customer_ZIP')
      =gfOpenTable('Customer','Customer','SH','Customer_ZIP')
    ENDIF
    SELECT Customer_ZIP
    =gfSeek('')
    SCAN FOR !EMPTY(caddress5)
      IF !SEEK(Customer_ZIP.caddress5,lcZIPTmp)
        INSERT INTO (lcZIPTmp) VALUES (Customer_ZIP.caddress5)
      ENDIF
    ENDSCAN 
    SELECT(lcZIPTmp)
    LOCATE 
  ENDIF
ENDIF
*: B609757,1 MMT 11/29/2011 Customer Mailing label does not filter by Zip Code[End]