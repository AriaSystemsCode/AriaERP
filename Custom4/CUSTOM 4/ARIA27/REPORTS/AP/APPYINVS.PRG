**************************************************************************
*
*B500790,1  MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1               to print the commas   
*
*B800102,1 Reham On 08/23/95
*B800102,1 Change in "APPYINVS.FRX" :_
*B800102,1   1- Decrease the width of the following fields from 15 to 12
*B800102,1      _ Adj. Amount , Open Bal. , Paid Amount , Inv. Amount.
*B800102,1   2- Add two fields to the frx { Aging - Factor }
*E300301,1 MAN 10/25/95 Adding the session # in the variable filter
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
*B600904,1 M.H 01/02/96 When you enter the option grid you have the invoicce 
*B600904,1              status option = 'OPEN' but if you click on preview 
*B600904,1              or print the report will display all the invoices
*B600904,1              so I fixed this bug by calling the lfvInvoice in
*B600904,1              the report show function.
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APPYINVS.FRX
*E300663,1 HS  04/24/97 Make some changes for we have change the file
*E300663,1              SYCSEQUN name to SEQUENCE and make it a data file
*E300663,1              [Change its dir. from SYSFILES to DBFS]
*B802483,1 AKA 07/28/99 Fix bug in AP invoice summary didn't show paid invoice if you choose
*B802483,1              "Paid" or "All".
*B603131,1 Walid Abou El-Magd 08/30/99 Add currency code field as a fixed filter
*B603131,1              and print the grand total.Also fix the bug of no
*B603131,1              Validation on vendor code.     
*B603432,1 AKA 02/10/00 When you enter the option grid you have the invoicce 
*B603432,1              status option = 'OPEN' but if you click on preview 
*B603432,1              or print the report will display all the invoices
*B603432,1              so I fixed this bug by calling the lfvInvoice in
*B603432,1              the report show function. Beacuse the invoice status is 
*B603432,1              defined as varibale not as a filter expression, so I will
*B603432,1              call a function lfvInvoice() before calling the report just to 
*B603432,1              build the report expression. 
*B604005,1 MHM 11/02/00 Fix bug of Numeric overflow in AP reports (modified in reports only)
*B604005,1 MHM          we add one record in SYREPUVR for DICEMAL yes or no and i modify these fileds
*B604005,1 MHM          inv amount, paid amount, dis taken , adj. amount , open balance
************************************************************************
SELECT APINVHDR

*E300296,1 M.H 12/18/95 Use the new index in the apinvhdr.
*SET ORDER TO TAG VENDINV
SET ORDER TO CURVENINV
*E300296,1 M.H End.

SET ORDER TO TAG VENCODE IN APVENDOR
SET RELATION TO cvendcode INTO APVENDOR ADDITIVE
*E300301,1 MAN Adding the session # in the variable filter if the session # 
*E300301,1     is entered
*DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp+ ' AND' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')
IF !EMPTY(lcRpSess)
  SET RELATION TO APINVHDR.CINVNO + APINVHDR.CVENDCODE INTO APDIST ADDITIVE
ENDIF

*B603432,1 AKA 02/10/00  [Start]
=lfvInvoice()
*B603432,1 AKA 02/10/00  [End] 

*B802483,1 AKA (Start)
IF !EMPTY(lcStatExp)
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp) , '' , ' AND ') + ' ( ' + lcStatExp  + ')'
ENDIF
*B802483,1 AKA (End)

*B603131,1 Use a new tmp index on the master file to support the currency groupping [Start]
=lfCreatNdx()
*B603131,1 Use this index to support the currency groupping [End..]


DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp+ ' AND' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')+;
                 IIF(EMPTY(lcRpSess),'',' .AND. lfFltSess()')

SET RELATION TO

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
* 
FUNCTION lfRepShow
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
laOGObjCnt[6] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")
*E300296,1 M.H 12/18/95 End.
*B600904,1 M.H 01/02/96 Begin.
*B600904,1 M.H 01/02/96 When you enter the option grid you have the invoicce 
*B600904,1              status option = 'OPEN' but if you click on preview 
*B600904,1              or print the report will display all the invoices
*B600904,1              so I fixed this bug by calling the lfvInvoice in
*B600904,1              the report show function.
=lfvInvoice()
*B600904,1              End.
*!**************************************************************************
*!
*!      Function: lfvInvoice
*!
*!**************************************************************************
* 
FUNCTION lfvInvoice

*B802483,1 AKA (Start)
lcStatExp  = ""
*B802483,1 AKA (End)

DO CASE
  CASE lcRpStatus = 'O'
    SHOW GET llRpIncInv ENABLE

    *B802483,1 AKA (Start)
    lcStatExp = " ABS(APINVHDR.NINVAMNT) > ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK) "
    IF !llRpIncInv
      lcStatExp = lcStatExp  + " .AND. "  + " APINVHDR.CINVSTAT  <> 'V' "
    ENDIF
    *DIMENSION laOGHDFlt[IIF(!llRpIncInv,3,2),7]
    *laOGHDFlt[2,1] = [ABS(APINVHDR.NINVAMNT)]
    *laOGHDFlt[2,2] = 'F'    
    *laOGHDFlt[2,3] = 'N'    
    *laOGHDFlt[2,4] = .T.    
    *laOGHDFlt[2,5] = 'Greater Than'    
    *laOGHDFlt[2,6] = [ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK)]    
    *laOGHDFlt[2,7] = 'E'
    *IF !llRpIncInv
    *  laOGHDFlt[3,1] = [APINVHDR.CINVSTAT]
    *  laOGHDFlt[3,2] = 'F'    
    *  laOGHDFlt[3,3] = 'C'    
    *  laOGHDFlt[3,4] = .F.    
    *  laOGHDFlt[3,5] = 'Like'    
    *  laOGHDFlt[3,6] = 'V'    
    *  laOGHDFlt[3,7] = 'V'        
    *ENDIF
    *B802483,1 AKA (End)        
    


  CASE lcRpStatus = 'P'
    llRpIncInv=.F.
    SHOW GET llRpIncInv DISABLE

    *B802483,1 AKA (Start)
    lcStatExp = " ABS(APINVHDR.NINVAMNT)  =  ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK) "

    *DIMENSION laOGHDFlt[2,7]
    *laOGHDFlt[2,1] = [APINVHDR.NINVAMNT]
    *laOGHDFlt[2,2] = 'F'    
    *laOGHDFlt[2,3] = 'N'    
    *laOGHDFlt[2,4] = .T.    
    *laOGHDFlt[2,5] = 'Like'    
    *laOGHDFlt[2,6] = [APINVHDR.NINVPAID+APINVHDR.NINVADJ+APINVHDR.NINVDISTK]        
    *laOGHDFlt[2,7] = 'E'
    *B802483,1 AKA (End)    
    
  CASE lcRpStatus = 'A'  
    SHOW GET llRpIncInv ENABLE  
    
    *B802483,1 AKA (Start)
    IF !llRpIncInv
    lcStatExp = " APINVHDR.CINVSTAT  <>  'V' "
    ENDIF
    *DIMENSION laOGHDFlt[IIF(!llRpIncInv,2,1),7]
    *IF !llRpIncInv
    *  laOGHDFlt[2,1] = [APINVHDR.CINVSTAT]
    *  laOGHDFlt[2,2] = 'F'    
    *  laOGHDFlt[2,3] = 'C'    
    *  laOGHDFlt[2,4] = .F.    
    *  laOGHDFlt[2,5] = 'Like'    
    *  laOGHDFlt[2,6] = 'V'    
    *  laOGHDFlt[2,7] = 'V'        
    *ENDIF    
    *B802483,1 AKA (End)        
ENDCASE



*!**************************************************************************
*!
*!      Function: lfvSess
*!
*!**************************************************************************
*E300301,1 MAN to Validate the entered session #

FUNCTION lfvSess
PRIVATE ALL LIKE lcOldAl*

IF EMPTY(lcRpSess)
  RETURN
ENDIF
*MAN
PRIVATE llSeqisOpen
llSeqisOpen = USED("SEQUENCE")
*E300663,1 Change this line for the changes we have made to SYCSEQUN [Begin]
*IF !USED('SYCSEQUN')
IF !USED('SEQUENCE')
*E300663,1 Change this line for the changes we have made to SYCSEQUN [End]

  lcOldAl = ALIAS()
  SELECT 0

  *E300663,1 Change this line for the changes we have made to SYCSEQUN [Begin]
  *USE &gcSysHome.SYCSEQUN ORDER CSEQ_TYPE 
  USE &gcDataDir.SEQUENCE ORDER CSEQ_TYPE 
  *E300663,1 Change this line for the changes we have made to SYCSEQUN [End]

  IF EMPTY(lcOldAl)
    SELECT 0
  ELSE
    SELECT (lcOldAl)
  ENDIF  
ELSE

  *E300663,1 Change this line for the changes we have made to SYCSEQUN [Begin]
  *SET ORDER TO TAG CSEQ_TYPE IN SYCSEQUN
  SET ORDER TO TAG CSEQ_TYPE IN SEQUENCE
  *E300663,1 Change this line for the changes we have made to SYCSEQUN [End]

ENDIF  

lcRpSess= PADL(ALLTRIM(lcRpSess),8,'0')
  
*E300663,1 Change this line for the changes we have made to SYCSEQUN [Begin]
*IF SEEK('APSESS'+gcAct_Comp,'SYCSEQUN')
*  IF !BETWEEN(VAL(lcRpSess),1,SYCSEQUN.nSeq_No)
*    =gfModalGen("TRM04158B00000","DIALOG",ALLTRIM(STR(SYCSEQUN.nSeq_No)))

*HAYTHAR Change this line for we have changed the index tag (CSEQ_TYPE)
*expression [Begin]
*IF SEEK('CAPSESSNO' , 'SEQUENCE')
IF SEEK('CAPSESSNO ' , 'SEQUENCE')
*HAYTHAR Change this line for we have changed the index tag [End]
  
  IF !BETWEEN(VAL(lcRpSess),1,SEQUENCE.nSeq_No)
    =gfModalGen("TRM04158B00000","DIALOG",ALLTRIM(STR(SEQUENCE.nSeq_No)))
*E300663,1 Change this line for the changes we have made to SYCSEQUN [End]
    *MAN
    IF !llSeqisOpen
      USE IN SEQUENCE
    ENDIF 
    lcRpSess = ''
    SHOW GET lcRpSess 
    RETURN 
  ENDIF
ENDIF  
*MAN
IF !llSeqisOpen
  USE IN SEQUENCE
ENDIF  

*!**************************************************************************
*!
*!      Function: lfFltSess
*!
*!**************************************************************************
*E300301,1 MAN To check if the one of distribution lines for the current
*E300301,1     invoice has the entered session number
FUNCTION lfFltSess

lcOldFl = ALIAS()
SELECT APDIST

LOCATE REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = APINVHDR.CINVNO + APINVHDR.CVENDCODE ;
            FOR   cApSessNo = lcRpSess

IF EMPTY(lcOldFl)
  SELECT 0
ELSE  
  SELECT (lcOldFl)      
ENDIF
RETURN FOUND("APDIST")

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603131,1 New when function to save the old currency code.

FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,6]

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603131,1 New valid function to validate the currency code.

FUNCTION lfvCurCode
lnAlias=SELECT(0)

IF EMPTY(laOGFxFlt[1,6]) 
  RETURN
ENDIF

IF !SEEK(laOGFxFlt[1,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[1,6]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laOGFxFlt[1,6] = lcOldCurr
  ELSE
    laOGFxFlt[1,6] = laTemp[1]
  ENDIF
ENDIF

SHOW GET laOGFxFlt[1,6]

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfRepShow
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603131,1
*
FUNCTION lfRepShow

laOGFxFlt[1,6]= gcBaseCurr
=lfOGShowGet("lnRepCurr")

*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
*B603131,1 Walid Abou El-Magd
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
*B603131,1 Walid Abou El-Magd
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
