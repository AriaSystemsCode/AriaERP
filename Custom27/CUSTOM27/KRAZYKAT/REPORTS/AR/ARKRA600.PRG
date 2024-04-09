************************************************************************
*: Program file  : ARARK600.PRG (C# 101635)
*: Program desc. : Accounts Receivables Open Invoice aging report.
*:                 Convert KRA600 (for Krazy Kat) from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:      Module   : AR
*:************************************************************************
*: Calls : 
*:         Functions  : gfOpenFile()
*:                    : gfTempName()
*:                    : FacChk()
*:                    : lfvAccount()
*:                    : lfvRang()
*:                      lfUpdate()
*:         Procedures : Header
*:************************************************************************
*: Modifications     : ........
*:B603997,1 BWA 10/31/2000 1) Fix the bug of variable factor not found.
*:B603997,1                2) Increase the picture of some fields in the FRX.
*:C200296,1 SSE 03/10/2002 Custom make the report work in Text mode.
*:B606298,1 SSE 08/07/2002 Fix bug of not calculating Due date correctly.
*:B606588,1 RAE 11/13/2002 Stop printing in line number 55 instead of 57 in the FRX.
*:***********************************************************************************
*-- Variables Declarations
STORE SPACE(5) TO lcFAccount,lcTAccount,lcFactor
ldDate     = ldRpEDate
lcFhb      = lcRpFhb
lcPastDue  = lcRpPastDue
lcPTotal   = lcRpPTotal
lcFactor   = lcRpSpeFac
lcTempName = gfTempName()

*B606298,1 Increase the Array. [Begin]
*DECLARE laTRltFld[2,2]
DECLARE laTRltFld[3,2]
*B606298,1 Increase the Array. [End]

laTRltFld[1,1] = 'EOM'
laTRltFld[1,2] = 'lcTEOM'
laTRltFld[2,1] = 'EOMDAY'
laTRltFld[2,2] = 'lnEomDay'

*B606298,1 Add the Net terms days. [Begin]
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnTDaysDue'  && Net Due Days
lnTDaysDue = 0
*B606298,1 Add the Net terms days. [End]

lcTEOM = ''
lnEomDay = 0
lcAcc= SPACE(5)
lcKeyOff = lcRpKeyOff         &&& To hold the answer of Question's grid.
llPrnKey = .T.         &&& To determain if print the 'key off' or not.
IF ldDate >= gdsysdate
  lcKeyOff = 'N'
ENDIF
=lfUpdFltVar()
CREATE TABLE (gcWorkDir+lcTempName) (ACCOUNT C(5),TRAN C(9),CHECK C(9),TYP C(1),;
                        TRANDATE D ,DUEDATE D ,CURRENT N(10,2),;
                        AGE1_30 N(10,2),AGE31_60 N(10,2),AGE61_OVER N(10,2),;
                        AGE N(3),AMOUNT N(10,2))
INDEX ON ACCOUNT+TYP TAG (lcTempName)
ldAgedate = ldDate
lcFilter = "TRANDATE <= ldDate"
lcArFilter = ''
IF lcKeyOff = 'Y'
  lcArFilter = "HistDate >ldDate AND TranDate <=ldDate"
ENDIF
DO CASE
  CASE EMPTY(lcfAccount) AND EMPTY(lctAccount) 
    lcFilter = lcFilter + " AND !EMPTY(Account)"
    IF lcKeyOff = 'Y'
      lcArFilter = lcArFilter + " AND !EMPTY(Account)"
    ENDIF
  CASE EMPTY(lcfAccount) 
    lcFilter = lcFilter + " AND ACCOUNT <= lctAccount "
    IF lcKeyOff = 'Y'
      lcArFilter = lcArFilter + " AND ACCOUNT <= lctAccount "
    ENDIF
  CASE EMPTY(lctAccount)
    lcFilter = lcFilter + " AND ACCOUNT >= lcfAccount "
    IF lcKeyOff = 'Y'
      lcArFilter = lcArFilter + " AND ACCOUNT >= lcfAccount "
    ENDIF
  OTHERWISE
    lcFilter = lcFilter + " AND BETWEEN(ACCOUNT,lcfAccount,lctAccount)"
    IF lcKeyOff = 'Y'
      lcArFilter = lcArFilter + " AND BETWEEN(ACCOUNT,lcfAccount,lctAccount)"
    ENDIF
ENDCASE
lcFilter1 = lcFilter

*B603997,1 BWA 10/31/2000 Fix the bug of variable factor not found.[START]
*lcFilter = lcFilter + IIF(lcFhb = 'F',IIF(EMPTY(lcFactor),' AND !EMPTY(FACTOR)',;
            ' AND FACTOR = lcFactor'),IIF(lcFhb = 'H',' AND EMPTY(FACTOR)',''))

*lcFilter1 = lcFilter1 + IIF(lcFhb = 'F',IIF(EMPTY(lcFactor),' AND !EMPTY(CUSTOMER.FACTOR)',;
            ' AND CUSTOMER.FACTOR = lcFactor'),IIF(lcFhb = 'H',' AND EMPTY(CUSTOMER.FACTOR)',''))

lcFilter = lcFilter + IIF(lcFhb = 'F',IIF(EMPTY(lcFactor),' AND !EMPTY(CFACCODE)',;
            ' AND CFACCODE = lcFactor'),IIF(lcFhb = 'H',' AND EMPTY(CFACCODE)',''))

lcFilter1 = lcFilter1 + IIF(lcFhb = 'F',IIF(EMPTY(lcFactor),' AND !EMPTY(CUSTOMER.CFACCODE)',;
            ' AND CUSTOMER.CFACCODE = lcFactor'),IIF(lcFhb = 'H',' AND EMPTY(CUSTOMER.CFACCODE)',''))
*B603997,1 [END]

WAIT 'Collecting data. Please wait ...' WINDOW NOWAIT

=lfUpdate('DEBIT','DUEDATE','I',lcFilter)
IF lcPastDue = 'N'
  =lfUpdate('CREDIT','TRANDATE','P',lcFilter1)
  SET RELATION TO
ENDIF

IF lcKeyOff = 'Y'
  =lfUpdate('ARHIST','DUEDATE','Z',lcFilter)
ENDIF  
WAIT CLEAR
SELECT (lcTempName)
SET ORDER TO TAG (lcTempName)
GO TOP
IF EOF()
  =gfDialog('I','No records selected for report.')
ELSE
  SET RELATION TO 'M'+Account INTO CUSTOMER
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF
USE IN (lcTempName)
ERASE (gcWorkDir+lcTempName)+'.DBF'
ERASE (gcWorkDir+lcTempName)+'.CDX'

*!**************************************************************************
*! Name      : lfUpdate
*! Developer : 
*! Date      : 
*! Purpose   : Function that update temp.file from which the report will be
*!             printed . 
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : lcFile,ldDate,lcTyp
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfUpdate('CREDIT','TRANDATE','P',lcFilt)
*!**************************************************************************
FUNCTION lfUpdate
PARAMETERS lcFile,ldDat,lcTyp,lcFilt

SELECT &lcFile
IF lcFile = 'CREDIT'
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
ENDIF
lcCond = IIF(lcFile='ARHIST' AND lcKeyOff = 'Y',lcArFilter,lcFilt)
SCAN FOR &lcCond  AND (TRANTYPE $ IIF(lcFile= 'DEBIT', '1',IIF(lcFile = "CREDIT",'4','14'));
     .OR. BETWEEN(IIF(lcFile = "CREDIT" ,cCreditCod,TranCode),'42','50'))
  IF lcPastDue = 'Y' AND DUEDATE >= ldAgedate
    LOOP
  ENDIF
  DO CASE
    CASE lcFile = 'CREDIT'
      lcTyp = 'P'
      IF BETWEEN(IIF(lcFile = "CREDIT" ,cCreditCod,TranCode),'42','50')
        lcTyp = 'C'
      ENDIF
    CASE lcFile = 'DEBIT'
      lcTyp = 'I'
      IF BETWEEN(IIF(lcFile = "CREDIT" ,cCreditCod,TranCode),'42','50')
        lcTyp = 'D'
      ENDIF
    OTHERWISE
      DO CASE
        CASE TranType ="1" 
          lctyp =  "I"
        CASE TranType = "4" 
          lcTyp = "P"
        CASE BETWEEN(IIF(lcFile = "CREDIT" ,cCreditCod,TranCode),'42','50') AND TranType $ "059"
          lctyp = "C"
        OTHERWISE
          lcTyp = "D"               
     ENDCASE
  ENDCASE
  lcCheck = ' '
  lcDoc   = ' '
  IF lcFile = 'CREDIT'
    DO CASE
      CASE Credit.TranType $ '056'
        lcCheck = IIF(TranType <> "6",Credit.Tran,lcCheck)
        lcDoc   = SUBSTR(Credit.Reference,1,9)
      CASE Credit.TranType = '4'
        lcCheck = Credit.Store
      OTHERWISE
        lcDoc   = Credit.Tran
    ENDCASE
  ELSE
    IF lcFile = 'DEBIT' AND Debit.TranType = '2'
      lcCheck = DEBIT.Tran
    ENDIF
    IF TranType = '1'
      lcDoc = Tran
    ELSE
      lcDoc =IIF(lcFile = "ARHIST" AND TranType ="4","", SUBSTR(Reference,1,9))  &&lcDoc = SUBSTR(Reference,1,9)
      lcCheck = IIF(lcFile = "ARHIST" AND TranType $ "04",;
                IIF(TranType = "4",Store,Tran),lcCheck)
      ENDIF
    ENDIF
    ldNewD = IIF(BETWEEN(IIF(lcFile = "CREDIT" ,&lcFile..cCreditCod,&lcFile..TranCode),'42','50'),&lcFile..TranDate,&lcFile..&ldDat)
    ldNewD = IIF(EMPTY(ldnewD),TranDate,ldNewD)
    ldDueDate = IIF(lcFile = 'CREDIT',&lcFile..TRANDATE,&lcFile..DUEDATE)
    *---This if statement concest of two part
    *---First Part (OR) : To find any invoices transaction 
    *---                  come from Debit,Credit or ArHist files,
    *---Part Two (AND)  : The transaction from the first part what ever it is
    *---                  must have EOM customer.
    IF ( (lcFile = 'DEBIT'  AND Trantype='1' );
     OR  (lcFile = 'CREDIT' AND Trantype='0' AND !EMPTY(SUBSTR(Desc,9,6)) );
     OR  (lcFile = 'ARHIST' AND Trantype='1' ));
     AND (SEEK(&lcFile..Tran,'InvHdr') AND gfRltFld(InvHdr.CTERMCODE,@laTRltFld,'CTERMCODE');
           AND ALLTRIM(lcTEOM) = 'Y')

      *B606298,1 Calculate the right due date. [Begin]
      *ldDueDate = IIF(DAY(TRANDATE)<=25,(GOMONTH(TRANDATE,1))+(10-(DAY(TRANDATE))),;
                                        (GOMONTH(TRANDATE,2))+(10-(DAY(TRANDATE)))) + IIF(TYPE('lnEomDay')='N',lnEomDay,0)
      ldDueDate = IIF(DAY(TRANDATE)<=25,(GOMONTH(TRANDATE,1))+(10-(DAY(TRANDATE))),;
                                        (GOMONTH(TRANDATE,2))+(10-(DAY(TRANDATE)))) + lnTDaysDue
      *B606298,1 Calculate the right due date. [End]
                                        
      ldNewD = ldDueDate
    ENDIF
    INSERT INTO (lcTempName) ;
       (ACCOUNT ,TRAN ,check ,TYP ,TRANDATE,DUEDATE,CURRENT,AGE1_30,AGE31_60,;
       AGE61_OVER,AGE,AMOUNT);
       VALUES (&lcFile..ACCOUNT,lcDoc,lcCheck  ,;
              lcTyp,&lcFile..TRANDATE,;
              IIF(lcFile <> 'CREDIT',IIF(!BETWEEN(&lcFile..TranCode,'42','50'),ldDueDate,{}),{}),;
              IIF(ldAgedate - ldNewD <= 0 ,&lcFile..AMOUNT,0),;
              IIF(BETWEEN(ldAgedate - ldNewD,1,30),&lcFile..AMOUNT,0),;
              IIF(BETWEEN(ldAgedate - ldNewD,31,60),&lcFile..AMOUNT,0),;
              IIF(ldAgedate - ldNewD >=61 ,&lcFile..AMOUNT,0),;
              IIF(EMPTY(CURRENT),ldAgedate - ldNewD,0),;
              &lcFile..AMOUNT)
ENDSCAN

*!**************************************************************************
*! Name      : lfPrtAcc
*! Developer : Sherif Attala Ishak
*! Date      : 03/26/97
*! Purpose   : Function to determine if we 'll print the account name and 
*!           : code or not.
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfPrtAcc()
*!**************************************************************************
*B801032,1 SHA 03/26/97
*!**************************************************************************
FUNCTION lfPrtAcc

IF lcAcc = Account
  RETURN
ENDIF
RETURN .F.

*!**************************************************************************
*! Name      : lfAcc
*! Developer : Sherif Attala Ishak
*! Date      : 03/26/97
*! Purpose   : 
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfAcc()
*!**************************************************************************
*B801032,1 SHA 03/26/97
*!**************************************************************************
FUNCTION lfAcc
lcAcc = Account
RETURN " "

*!**************************************************************************
*! Name      : lfArHist
*! Developer : Tarek Mohamed Ismael (TMI)
*! Date      : 04/09/97
*! Purpose   : 
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfAcc()
*!**************************************************************************
FUNCTION lfPrnKey
llPrnKey = .F.
RETURN ('<---------- KEYED OFF TRANSACTIONS ------------>')

FUNCTION lfOgWhen
ldRpEDate = gdSysDate

*C200296,1 SSE Commented out. [Begin]
*SHOW GET lcRepMode DISABLE
*C200296,1 SSE Commented out. [End]

R_WIDTH = 'N'

*!*************************************************************
*! Name      : lfvcustom
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfVAcc

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

FUNCTION lfClr

CLEAR READ

FUNCTION lfvForm
IF lcRpRepFrm = 'D' 
  lcRpForm  = 'ARKRA600'
ELSE
  lcRpForm  = 'ARKRA610'
ENDIF

*!*************************************************************
*! Name      : lfUpdFltVar  (C# 101591)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 08/12/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcFAccount = SUBSTR(laOgFxFlt[lnInd,6],1,5)
      lcTAccount = SUBSTR(laOgFxFlt[lnInd,6],7,5)
  ENDCASE
ENDFOR

