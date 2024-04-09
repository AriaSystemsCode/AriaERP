*B500790,1 MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1              to print the commas   
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APAPROVE.FRX
*B601526,1  HS  01/23/97 1)Use the new field APINVHDR.nInvFAAp in the report
*B601526,1                 APAPROVE.FRX to get the approved amount in base 
*B601526,1                 currency  
*B601526,1               2)Added a new report APAPROVF.FRX to be used if the
*B601526,1                 currency display is foreign
*B601526,1               3)Added a new field in the Option Grid [Invoice currency]
*B601526,1               4)Added the Validition function [lfvInvCurr] and When 
*B601526,1                 function [lfvInvCurr] of the Invoice currency  
*B601526,1                 field and function to set the field defult value 
*B601526,1                 and picture [lfwDefCurr]
*B601526,1               5)Added an Option Grid WHEN function [lfwOpGrid]
*B601526,1               6)Added Function to set the currency symbol [lfSetCurSm]
*B802458,1 AKA 07/21/99  Two new records added in option grid (Invoice date & invoice due date)
*B603139,1 Walid Abou El-Magd (WAM) 09/05/99 
*B603139,1               1)Print the grand total when all transactions are converted to base currency
*B603139,1               2)Fix the bug that valid currency code triger the currency browse screen.
*B603139,1               3)Suppress the currency fields in case of single currency company. 
*B603522,1 SSE 03/19/2000 Convert all objects in FRX to TRANSPARENT
*B604005,1 MHM 11/02/00 Fix bug of Numeric overflow in AP reports (modified in reports only)
*B604005,1 MHM          we add one record in SYREPUVR for DICEMAL yes or no and i modify this fileds
*B604005,1 MHM          inv amount, appr. to pay, appr. in for, appr. disc., appr. adj, open balance , appr. 1099
****************************************************************************
*
*B601526,1 Add this lines [Begin]
lcSavCurSm = SET('CURRENCY',1)    &&Varible to save the seting of the currency symbol
*MAN Added Flag To check if the currency file was already open
PRIVATE llCurISopen

*B802458,1  Remove file opening variable
*llCurISopen = USED("SYCCURR")
*B802458,1 end

*B601526,1 Select SQL Statment to get all the currency codes and symbols
*B601526,1 in an Array called laCurrSmbl
SELECT       cCurrCode , cCurrSmbl ;
  FROM       (gcSysHome + 'SYCCURR') ;
  INTO ARRAY laCurrSmbl     

*B802458,1 Remove file closing
*IF !llCurISopen 
*  USE IN SYCCURR
*ENDIF  
*B802458,1 end

*B601526,1 Add this lines [END]

SELECT APINVHDR
*
*B603139,1 Use a new tmp index on the master file to support the currency groupping [Start]
*SET ORDER TO TAG VENDINV
=lfCreatNdx()
*B603139,1 Use this index to support the currency groupping [End..]

SET ORDER TO TAG VENCODE IN APVENDOR
SET RELATION TO cvendcode INTO APVENDOR ADDITIVE
DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp+ ' AND' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')
SET RELATION TO

*B601526,1 Add this lines [Begin]
SET CURRENCY TO lcSavCurSm   && Reset the currency symbol
*B601526,1 Add this lines [END]

*!**************************************************************************
*!
*!      Function: lfShowRep
*!
*!**************************************************************************
*
FUNCTION lfShowRep

*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
laOGObjCnt[3] = gfGetMemVar('LLMULCURR')

*B603139,1 Initailization of this filter in all cases is very important 
*          to the #OBJDISP condition in all grand totals fields [Start]
laOGFxFlt[5,6]= gcBaseCurr
*B603139,1 [End..]

=lfOGShowGet("lnRepCurr")
*E300296,1 End

DO CASE
  CASE laOGFxFlt[1,6]='H'
    laOGFxFlt[2,6]=''
    laOGFxFlt[3,6]=''
    laOGObjCnt[lnOGVariable+2] = .F.
    laOGObjCnt[lnOGVariable+3] = .F.
    laOGObjCnt[lnOGVariable+4] = .T.
  CASE laOGFxFlt[1,6]='C' OR EMPTY(laOGFxFlt[1,6])    
    laOGFxFlt[2,6]=''
    laOGFxFlt[3,6]=''
    laOGFxFlt[4,6]=''    
    laOGObjCnt[lnOGVariable+2] = .F.
    laOGObjCnt[lnOGVariable+3] = .F.
    laOGObjCnt[lnOGVariable+4] = .F.  
  OTHERWISE
    laOGFxFlt[4,6]=''
    laOGObjCnt[lnOGVariable+4] = .F.
    laOGObjCnt[lnOGVariable+2] = .T.
    laOGObjCnt[lnOGVariable+3] = .T.  
ENDCASE

=lfActvateWin()

*!**************************************************************************
*!
*!      Function: lfvInvoice
*!
*!**************************************************************************
FUNCTION lfvInvoice

DO CASE
  CASE lcRpStatus = 'O'
    SHOW GET llRpIncInv ENABLE
    DIMENSION laOGHDFlt[IIF(!llRpIncInv,3,2),7]
    laOGHDFlt[2,1] = [ABS(APINVHDR.NINVAMNT)]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'N'    
    laOGHDFlt[2,4] = .T.    
    laOGHDFlt[2,5] = 'Greater Than'    
    laOGHDFlt[2,6] = [ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK)]
    laOGHDFlt[2,7] = 'E'
    IF !llRpIncInv
      laOGHDFlt[3,1] = [APINVHDR.CINVSTAT]
      laOGHDFlt[3,2] = 'F'    
      laOGHDFlt[3,3] = 'C'    
      laOGHDFlt[3,4] = .F.    
      laOGHDFlt[3,5] = 'Like'    
      laOGHDFlt[3,6] = 'V'    
      laOGHDFlt[3,7] = 'V'        
    ENDIF

  CASE lcRpStatus = 'P'
    llRpIncInv=.F.
    SHOW GET llRpIncInv DISABLE
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [APINVHDR.NINVAMNT]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'N'    
    laOGHDFlt[2,4] = .T.    
    laOGHDFlt[2,5] = 'Like'    
    laOGHDFlt[2,6] = [APINVHDR.NINVPAID+APINVHDR.NINVADJ+APINVHDR.NINVDISTK]        
    laOGHDFlt[2,7] = 'E'

  CASE lcRpStatus = 'A'  
    SHOW GET llRpIncInv ENABLE  
    DIMENSION laOGHDFlt[IIF(!llRpIncInv,2,1),7]
    IF !llRpIncInv
      laOGHDFlt[2,1] = [APINVHDR.CINVSTAT]
      laOGHDFlt[2,2] = 'F'    
      laOGHDFlt[2,3] = 'C'    
      laOGHDFlt[2,4] = .F.    
      laOGHDFlt[2,5] = 'Like'    
      laOGHDFlt[2,6] = 'V'    
      laOGHDFlt[2,7] = 'V'        
    ENDIF    
ENDCASE


*!**************************************************************************
*!
*!      Function: lfBnkCode
*!
*!**************************************************************************
FUNCTION lfBnkCode

PRIVATE lcRetVal 

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias
SELECT APSETUP
lcRetVal   = APSETUP.CBNKCODE

llRpGlLink = IIF(APSETUP.CAPSGLLINK='Y',.T.,.F.)
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
                 ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink 
  SELECT SYCCOMP
  =SEEK(gcAct_Comp)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = gcDataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(gcAct_Comp)
  ENDIF
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName) 
  &lcOGVarName=lcRetVal
ENDIF

RETURN REPLI('X',8)


*!**************************************************************************
*!
*!      Function: lfChkAct
*!
*!**************************************************************************
FUNCTION lfChkAct
PARAMETERS llFirsTime

PRIVATE lcRetVal 

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias

IF llFirsTime
  SELECT APSETUP
  lcRetVal = APSETUP.CCHKACCT
ELSE
  SELECT APCHECKS
  SET ORDER TO TAG BANKCHECK
  =SEEK(laOGFxFlt[2,6])
  lcRetVal = APCHECKS.CCHKACCT
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName) AND llFirsTime
  &lcOGVarName=lcRetVal
ENDIF
RETURN IIF(llFirsTime,REPL('X',12),lcRetVAl)


*!**************************************************************************
*!
*!      Function: lfGlCkActD
*!
*!**************************************************************************
FUNCTION lfGlCkActD
PARAMETERS llFirsTime
PRIVATE lcRetVal 

lcRetVal = ' '
lcRetVal = APSETUP.CCASHACCT

IF EMPTY(&lcOGVarName) AND llFirsTime
  &lcOGVarName=lcRetVal
ENDIF
RETURN IIF(llFirsTime,lcRpActPic,lcRetVAl)


*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
FUNCTION lfvBank

lcSendData=SYS(18)
IF lcOldVal=&lcSendData
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time
IF NOT USED("APBANKS") 
  SELECT 0
  *MAN
  *USE &gcDataDir.APBANKS ORDER TAG BANKCOD
  USE &gcDataDir.APBANKS ORDER TAG BANKCODE
  llUesdBefo = .T.
ENDIF
SELECT APBANKS
SET ORDER TO TAG BANKCODE

  lcRpCurFld      = SYS(18)
  && Check If year field is empty
  IF lcOldVal <> &lcRpCurFld.
    *** Search for the current Group code
    IF ('?' $ &lcRpCurFld. .OR.!SEEK(ALLTRIM(&lcRpCurFld.)))
      =gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
      IF !EMPTY(laRpRetFld[1])  
        &lcRpCurFld = laRpRetFld[1]
      ELSE
        &lcRpCurFld = lcOldVal
      ENDIF
      SHOW GET (lcRpCurFld)
    ENDIF
    laOGFxFlt[3,6] = lfchkact()
    SHOW GET laOGFxFlt[3,6]

  ENDIF

IF llUesdBefo       && .F.- this file used by the system
  USE IN APBANKS
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF


*!**************************************************************************
*!
*!      Function: lfvChkAct
*!
*!**************************************************************************
FUNCTION lfvChkAct

lcSendData=SYS(18)
IF lcOldVal=&lcSendData
  RETURN
ENDIF
DECLARE laRpRetFld(1)
lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time
IF NOT USED("APCHECKS") 
  SELECT 0
  USE &gcDataDir.APCHECKS ORDER TAG BANKCHECK
  llUesdBefo = .T.
ENDIF
SELECT APCHECKS
SET ORDER TO TAG BANKCHECK

  lcRpCurFld      = SYS(18)
  && Check If year field is empty
  IF lcOldVal <> &lcRpCurFld.
    *** Search for the current Group code
    IF ('?' $ &lcRpCurFld. .OR.!SEEK(ALLTRIM(laOGFxFlt[2,6]+&lcRpCurFld.)))
      =gfBrows('"'+laOGFxFlt[2,6]+'"','CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)
      IF !EMPTY(laRpRetFld[1])  
        &lcRpCurFld = laRpRetFld[1]
      ELSE
        &lcRpCurFld = lcOldVal
      ENDIF
      SHOW GET (lcRpCurFld)
    ENDIF
  ENDIF


IF llUesdBefo       && .F.- this file used by the system
  USE IN APCHECKS
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    



*!**************************************************************************
*!
*!      Function: lfGlLink
*!
*!**************************************************************************
FUNCTION lfGlLink

lcApTmpAlias=ALIAS()
SELECT APSETUP
GO TOP
llRpGlLink = capsgllink = 'Y'
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
                 ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink 
  SELECT SYCCOMP
  =SEEK(gcAct_Comp)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = gcDataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(gcAct_Comp)
  ENDIF
ENDIF

SELECT (lcApTmpAlias)

*!**************************************************************************
*!
*!      Function: lfvGlAct
*!
*!**************************************************************************
FUNCTION lfvGlAct

lcSendData=SYS(18)
IF lcOldVal=&lcSendData
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CACCTCODE:H="Code",CACCNLDES:H="Description"'
laRpRetFld[1] = ''
IF llRpGlLink
  lcOldAlias = ALIAS()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  lcTempFile = gfTempName() 
  SELECT 0
  USE &lcRpParDir.GLACCHAR  AGAIN ALIAS &lcTempFile
  SELECT (lcTempFile)
  SET ORDER TO TAG ACCTCODE

    lcRpCurFld      = SYS(18)
    && Check If year field is empty
    IF .NOT. EMPTY(ALLTRIM(&lcRpCurFld.))  AND lcOldVal <> &lcRpCurFld. 
      *** Search for the current Group code
      IF ('?' $ &lcRpCurFld. .OR.!SEEK(ALLTRIM(&lcRpCurFld.)))
        =gfBrows([],'CACCTCODE',"laRpRetFld",'Accoonts ',.F.)
        IF !EMPTY(laRpRetFld[1])  
          &lcRpCurFld = laRpRetFld[1]
        ELSE
          &lcRpCurFld = lcOldVal
        ENDIF
        SHOW GET (lcRpCurFld)
      ENDIF
    ENDIF

  USE IN &lcTempFile
  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF
ENDIF


*!**************************************************************************
*!
*!      Function: lfvpmethod
*!
*!**************************************************************************
*
FUNCTION lfvpmethod

DO CASE
  CASE laOGFxFlt[1,6]='H'
    laOGFxFlt[2,6]=''
    laOGFxFlt[3,6]=''
    laOGFxFlt[4,6] = lfGlCkActD()
    laOGObjCnt[lnOGVariable+4] = .T.  
    laOGObjCnt[lnOGVariable+2] = .F.
    laOGObjCnt[lnOGVariable+3] = .F.
  CASE (laOGFxFlt[1,6]='C' OR EMPTY(TRIM(laOGFxFlt[1,6])))
    laOGFxFlt[2,6]=''
    laOGFxFlt[3,6]=''
    laOGFxFlt[4,6]=''    
    laOGObjCnt[lnOGVariable+2] = .F.
    laOGObjCnt[lnOGVariable+3] = .F.
    laOGObjCnt[lnOGVariable+4] = .F.    
  OTHERWISE
    laOGFxFlt[4,6]=''
    IF EMPTY(laOGFxFlt[2,6])
      lcOGVarName='laOGFxFlt[2,6]'
      =lfBnkCode()
      laOGFxFlt[3,6] = lfchkact()
    ENDIF  
    laOGObjCnt[lnOGVariable+4] = .F.
    laOGObjCnt[lnOGVariable+2] = .T.
    laOGObjCnt[lnOGVariable+3] = .T.  
ENDCASE

=lfActvateWin()

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp

*B601526,1 Add this line [Begin]
lcOldCurDs = lcRpCurr 
*B601526,1 Add this line [End]

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*B601526,1 Add this lines [Begin]
lcRpForm = IIF(lcRpCurr = 'F' , IIF(gfGetMemVar('LLMULCURR') , 'APAPROVF' , 'APAPROVE') , 'APAPROVE')
*B603139,1 Don't force the user to select currency in this case [Start]
*IF lcRpCurr = 'F' .AND. lcRpCurr <> lcOldCurDs .AND. EMPTY(laOGFxFlt[5,6])
*  laOGFxFlt[5,6] = gcBaseCurr
*  =lfActvateWin()
*ENDIF
*B603139,1 Don't force the user to select currency in this case [End..]
*B601526,1 Add this lines [End] 

*!*************************************************************
*! Name      : lfvInvCurr
*! Developer : Haytham El-Sheltawi
*! Date      : 01/23/1997
*! Purpose   : VALID function of Invoice currency field in the Option Grid
*!*************************************************************
*! Called    : Option Grid
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvInvCurr()
*!*************************************************************
*B601526,1 This function was added by HS for this bug
*!*************************************************************
*
FUNCTION lfvInvCurr

lcInvCurr = VARREAD()

lcOldAlias = ALIAS()
*B603139,1 Don't force the user to select currency in this case [Start]
*IF EMPTY(&lcInvCurr) .AND. lcRpCurr <> 'F' 
IF EMPTY(&lcInvCurr)  
*B603139,1 Don't force the user to select currency in this case [End..]
  RETURN
ENDIF

SELECT SYCCURR

IF ATC("?" , &lcInvCurr) > 0 .OR. !SEEK(&lcInvCurr)  
  DIMENSION laTemp[1]
  laTemp     = ''
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  IF EMPTY(laTemp[1])
    &lcInvCurr = lcRpOldCur
  ELSE
    &lcInvCurr = laTemp[1]
  ENDIF
ENDIF

SELECT &lcOldAlias

*!*************************************************************
*! Name      : lfwInvCurr
*! Developer : Haytham El-Sheltawi
*! Date      : 01/23/1997
*! Purpose   : WHEN function of Invoice currency field in the Option Grid
*!*************************************************************
*! Called    : Option Grid
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwInvCurr()
*!*************************************************************
*B601526,1 This function was added by HS for this bug
*!*************************************************************
*
FUNCTION lfwInvCurr

lcRpOldCur = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfwDefCurr
*! Developer : Haytham El-Sheltawi
*! Date      : 01/23/1997
*! Purpose   : Function to defult Invoice currency field in the Option Grid
*!*************************************************************
*! Called    : Option Grid
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwDefCurr()
*!*************************************************************
*B601526,1 This function was added by HS for this bug
*!*************************************************************
*
FUNCTION lfwDefCurr

IF EMPTY(&lcOGVarName) 
  &lcOGVarName = gcBaseCurr
ENDIF
*B603139,1 Fix the bug that valid currency code triger the currency browse screen. [Start]
*RETURN "@! XXXXX"
RETURN "@! XXX"
*B603139,1 Fix the bug that valid currency code triger the currency browse screen. [End..]
*!*************************************************************
*! Name      : lfwOpGrid
*! Developer : Haytham El-Sheltawi
*! Date      : 01/23/1997
*! Purpose   : WHEN function of the Option Grid
*!*************************************************************
*! Called    : Option Grid
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwOpGrid()
*!*************************************************************
*B601526,1 This function was added by HS for this bug
*!*************************************************************
*
FUNCTION lfwOpGrid
IF !gfGetMemVar('LLMULCURR')
  *--
  lcRpForm = 'APAPROVE'  
  laOGObjCnt[8] = .F.
  =lfActvateWin()
ENDIF

*!*************************************************************
*! Name      : lfSetCurSm
*! Developer : Haytham El-Sheltawi
*! Date      : 01/23/1997
*! Purpose   : Function to set the currency symbol
*!*************************************************************
*! Called    : APAPROVF.FRX
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : lcCrCod 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfSetCurSm()
*!*************************************************************
*B601526,1 This function was added by HS for this bug
*!*************************************************************
*
FUNCTION lfSetCurSm

PARAMETER lcCrCod

PRIVATE lcExactSt , lnCurSEl

lcExactSt = SET('EXACT')
SET EXACT ON
lnCurSEl = ASCAN(laCurrSmbl , lcCrCod)

SET CURRENCY TO IIF(lnCurSEl <> 0 , ALLTRIM(laCurrSmbl(lnCurSEl + 1)) , '')

SET EXACT &lcExactSt

RETURN ''






*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
*B603139,1 Walid Abou El-Magd
FUNCTION lfClearRep
IF FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
  SELECT APINVHDR
  CLOSE INDEX
  ERASE (gcWorkDir +lcApInvHdr+ '.CDX')
ENDIF

*!**************************************************************************
*!
*!      Function: lfCreatNdx
*!
*!**************************************************************************
*B603139,1 Walid Abou El-Magd
FUNCTION lfCreatNdx
SELECT APINVHDR
* If index name is already defined 
IF TYPE("lcApInvHdr") <> 'U'
  * If index file is already creadted 
  IF !FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
    lcApInvHdr = gfTempName()
    INDEX ON cvendcode+ccurrcode+cinvno TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')
  ELSE
    SET ORDER TO TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
  ENDIF
ELSE    && ELASE OF  "If index name is already defined "
    lcApInvHdr = gfTempName()          
    INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
ENDIF
  
