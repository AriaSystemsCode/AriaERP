*:***************************************************************************
*: Program file  : AROVRDU.PRG
*: Program desc. : CUSTOMIZED INTEREST ON OVERDUE ACCOUNT FOR ROB20
*: Date          : 03/10/2005
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Ashraf Sherif Mohammed
*: Tracking Job Number: C039115 copied from C200188
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO AROVRDU
*:***************************************************************************
*: Modifications :

*--Section to creat the Temp. files names.
lcOvrDue = gfTempName()
lcGLTemp = gfTempName()

ldChekDat = {}
lcRpFile = "AROVRDU"
IF FILE(gcDataDir+lcRpFile+'.MEM')
  RESTORE FROM gcDataDir+lcRpFile+'.MEM' ADDITIVE
ENDIF

*--Section to Initial the used varaibles in the program.
PRIVATE llCustomer , llDebit , llCredit , llGldist , llOpenFac
STORE .F. TO llCustomer , llDebit , llCredit , llGldist , llOpenFac , llLink_GlD

lcInterest = gfGetMemVar('M_INTEREST',gcAct_Comp)
llLink_GlD = ALLTRIM(gfGetMemVar("M_LINK_GL")) = "Y"

*--Section to open , creat , collect data and close files.
IF (ldChekDat # {} AND gfModalGen('QRM00000B40000','DIALOG','','', 'Last time this program was run on ' + DTOC(ldChekDat) + '. Do you want to update the customers accounts with interest?') = 1) OR ldChekDat = {}
  =lfOpenFils()
  =lfCreatTmp()
  =lfColctDat()
  =lfClosFile()
ENDIF
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfOpenFils
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
*! Purpose   : Function to open the files used in the program.
*!*************************************************************
*! Called from : AROVRDU.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOpenFils

IF !USED('CUSTOMER')
  llCustomer = gfOpenFile(gcDataDir + "CUSTOMER" , "CUSTOMER" , 'SH')
ENDIF

IF !USED('CREDIT')
  llCredit = gfOpenFile(gcDataDir + "CREDIT" , "CREDIT" , 'SH')
ENDIF

IF !USED('DEBIT')
  llDebit = gfOpenFile(gcDataDir + "DEBIT" , "DEBIT" , 'SH')
ENDIF

IF !USED('GLDIST')
  llGldist = gfOpenFile(gcDataDir + "GLDIST" , "GLDISTAC" , 'SH')
ENDIF

*--End of lfOpenFils
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : AROVRDU.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

DECLARE laFileStru[1,1] , laFilGlDst[1,1]

SELECT DEBIT
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcOvrDue) FROM ARRAY laFileStru
INDEX ON ACCOUNT+STORE TAG Account OF (gcWorkDir+lcOvrDue)

SELECT GLDIST
=AFIELDS(laFilGlDst)
CREATE TABLE (gcWorkDir+lcGLTemp) FROM ARRAY laFilGlDst

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfColctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Called from : AROVRDU.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias,lcAccount

lcAlias = ALIAS()
STORE SPACE(0) TO lcStore

SELECT CUSTOMER
LOCATE

SCAN WHILE Type+Account+Store = 'M' + '' 

  WAIT WINDOW IIF(EMPTY(CUSTOMER.STORE) , "Collecting data for Account : " , "Collecting data for Account - Store : " ) +CUSTOMER.ACCOUNT + IIF(EMPTY(CUSTOMER.STORE) , " " , "-" + CUSTOMER.STORE) NOWAIT

  lcStore = CUSTOMER.STORE
  STORE 0 TO lnDebTotal , lnCrdTotal
  SELECT DEBIT
  =SEEK(CUSTOMER.ACCOUNT)

  SCAN REST WHILE Account+TRAN+CINSTALNO+DTOS(TRANDATE) = CUSTOMER.ACCOUNT ;
              FOR TRANTYPE = '1' AND GDSYSDATE > DEBIT.TRANDATE + 30

    lnDebTotal = lnDebTotal + DEBIT.AMOUNT
  ENDSCAN

  IF lnDebTotal # 0             &&Case there is a debit value.
    SELECT CREDIT
    =SEEK(CUSTOMER.ACCOUNT)

    SCAN REST WHILE Account+TRAN+DTOS(TRANDATE) = CUSTOMER.ACCOUNT 
      lnCrdTotal = lnCrdTotal + CREDIT.AMOUNT
    ENDSCAN
  ENDIF

  *--Check if the net balance is +ve to creat a record in the temp. file or not.
  lnNetAmnt = lnDebTotal + lnCrdTotal

  SELECT (lcOvrDue)
  IF lnNetAmnt > 0
    STORE SPACE(0) TO lcTEOM , lcAdjAcct
    STORE 0 TO lnTDaysDue , lnEOMDay
    STORE 1 TO lnExRate
    STORE {} TO ldDueDate
    ldBatchDt = gdSysDate

    DECLARE laTrmRltd[2,2]
    laTrmRltd[1,1]='EOM'
    laTrmRltd[1,2]='lcTEOM'
    laTrmRltd[2,1]='NTERDUED'
    laTrmRltd[2,2]='lnTDaysDue'
    =gfRltFld(Customer.cTermCode,@laTrmRltd,'CTERMCODE')
    lcTEOM = ALLTRIM(lcTEOM)

    ldDueDate = IIF(lcTEOM <> 'Y', ldBatchDt + lnTDaysDue ,;
                CTOD('01'+SUBSTR(DTOC(GOMONTH(ldBatchDt,1)),3))-1+lnTDaysDue)

    *lnCurrUnit = 1
    *lcCurrCode = lcCurrncy
    *lnExRate = gfChkRate('lnCurrUnit' , lcCurrCode , ldBatchDt , .T. , ;
                            gcAct_Comp , .F. , .T.)

    DECLARE laAdjRltFd[1,2]
    laAdjRltFd[1,1] = 'CADJACCT'
    laAdjRltFd[1,2] = 'lcAdjAcct'
    lcCode = "000509"
    =gfRltFld(lcCode , @laAdjRltFd , "TRANCODE")

    SELECT (lcOvrDue)
    APPEND BLANK
    REPLACE &lcOvrDue..Account   WITH CUSTOMER.ACCOUNT      ,;
            &lcOvrDue..STORE     WITH lcStore               ,;
            &lcOvrDue..TRANTYPE  WITH "2"                   ,;
            &lcOvrDue..TRANCODE  WITH lcCode                ,;
            &lcOvrDue..TRANDATE  WITH GDSYSDATE             ,;
            &lcOvrDue..DPOSTDATE WITH GDSYSDATE             ,;
            &lcOvrDue..DESC      WITH "Interest on Overdue" ,;
            &lcOvrDue..DUEDATE   WITH ldDueDate             ,;
            &lcOvrDue..CCURRCODE WITH 'USD'                 ,;
            &lcOvrDue..NCURRUNIT WITH 1                     ,;
            &lcOvrDue..NEXRATE   WITH 1                     ,;
            &lcOvrDue..CADJACCT  WITH lcAdjAcct             ,;
            &lcOvrDue..CARGLACC  WITH &lcGLTemp..GLAccount  ,;
            &lcOvrDue..CFACCODE  WITH SPACE(6)              ,;
            &lcOvrDue..DSC_AMT   WITH 0                     ,;
            &lcOvrDue..Amount    WITH (lnNetAmnt * lcInterest)/100
    =gfAdd_Info()
  ENDIF

ENDSCAN
WAIT CLEAR

SELECT (lcOvrDue)
IF RECCOUNT(lcOvrDue) > 0
  *-- Due to this message we will update the files or not.
  IF gfModalGen('QRM00000B40000','DIALOG','','','Update customers accounts with interest.') = 1

    SELECT (lcOvrDue)
    LOCATE
    SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
    SCAN

      WAIT WINDOW "Processing Account:" + &lcOvrDue..Account NOWAIT
      *-- Update the customer file with : open credit, current, total age and net balance.

      *lcExRSin = "/"
      *lcUntSin = ' '
      STORE 1 TO lnCurrUnit , lnExRate
      *lnEqvAmnt = 0

      IF SEEK('M'+&lcOvrDue..ACCOUNT , 'CUSTOMER')
        SELECT CUSTOMER
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        *-- Add the equivelant amount to customer file.
        *lcExRSin = gfGetExSin(@lcUntSin, &lcOvrDue..Ccurrcode)
        *lnExRate = gfChkRate('lnCurrUnit' , &lcOvrDue..Ccurrcode , gdSysDate , .T. , ;
        gcAct_Comp , .F. , .T.)

        *lnEqvAmnt = ROUND(&lcOvrDue..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        lnEqvAmnt = ROUND(&lcOvrDue..AMOUNT,2)
        REPLACE OPENCR  WITH OPENCR  + lnEqvAmnt ,;
                CURRENT WITH CURRENT + lnEqvAmnt ,;
                TOTAGE  WITH TOTAGE  + lnEqvAmnt ,;
                NETBAL  WITH NETBAL  + lnEqvAmnt

      ENDIF
      SELECT (lcOvrDue)

      STORE SPACE(0) TO lcBatchNo , lcTranSec
      lcBatchNo = gfsequence('BATCH')
      lcTranSec = gfsequence('DEBIT', gcAct_Comp, "", "", "TRAN")

      REPLACE &lcOvrDue..TRAN  WITH lcTranSec ,;
              &lcOvrDue..BATCH WITH lcBatchNo

      *--Case there is link to the Gl module.
      IF llLink_GlD

        lcGLFYear  = SPACE(4)
        lcGLPeriod = SPACE(2)
        *-- Check the period & the year for the entered date.
        =CHECKPRD(ldBatchDt,'lcGLFYear','lcGLPeriod','AJ',.T.)

        lcGLAcc = &lcOvrDue..cAdjAcct
        lcLinkCode = IIF(!EMPTY(CUSTOMER.Link_Code) , CUSTOMER.Link_Code , "DEFDEF")
        IF !EMPTY(&lcOvrDue..cFacCode)
          lcLinkCode = lfGetFacLk(lcLinkCode)
        ENDIF

        *-- If Debit Adjustments '010'.
        DO GLDIST WITH '','010',-(&lcOvrDue..AMOUNT), ;
           'DA',&lcOvrDue..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
           lcGLAcc,&lcOvrDue..CCURRCODE,&lcOvrDue..NCURRUNIT,&lcOvrDue..NEXRATE

        DO GLDIST WITH lcLinkCode,'001',&lcOvrDue..AMOUNT, ;
           'DA',&lcOvrDue..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
           '',&lcOvrDue..CCURRCODE,&lcOvrDue..NCURRUNIT,&lcOvrDue..NEXRATE
      ENDIF
    ENDSCAN
    WAIT CLEAR

    *--Case there is link to the Gl module.
    IF llLink_GlD
      *-- Generate a unique session no.
      lcGLSession = gfsequence('GLSESSION')
      SELECT (lcGLTemp)
      REPLACE ALL &lcGLTemp..GLSESSION WITH lcGLSession
      =gfAdd_Info()

      *--Case update the gldist file with the records.
      SELECT GLDIST
      APPEND FROM (gcWorkDir+lcGLTemp)
    ENDIF

    *--Case update the debit file with the records.
    SELECT DEBIT
    APPEND FROM (gcWorkDir+lcOvrDue)
    =gfModalGen('INM00000B40011','DIALOG','','','Accounts have been succsefully updated.')

    *--Saving the variables to the mem file.
    ldChekDat = GDSYSDATE
    IF !EMPTY(ldChekDat)
      SAVE TO gcDataDir+lcRpFile+'.MEM' ALL LIKE ldChekD*
    ENDIF

  ENDIF
ELSE
  =gfModalGen('INM00000B40011','DIALOG','','','No overdue transaction found.')
ENDIF

*--End of lfColctDat.
*!*************************************************************
*! Name      : lfGetFacLk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 05/16/2001
*! Purpose   : Get link Code of The factor.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION lfGetFacLk
PARAMETER lclink
PRIVATE lcLnkCod

IF !USED('FACTOR')
  llOpenFac = gfOpenFile(gcDataDir+'FACTOR',gcDataDir+'FACTOR','SH')
ENDIF

IF SEEK(&lcOvrDue..cfaccode,'FACTOR')
  lcLnkCod = IIF(!EMPTY(FACTOR.Link_code) , FACTOR.Link_code , lclink)
ELSE
  lcLnkCod = lclink
ENDIF

RETURN lcLnkCod

*--End of lfGetFacLk.
*!*************************************************************
*! Name      : lfClosFile
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 05/16/2001
*! Purpose   : Function to close the files used in the program.
*!*************************************************************
*! Called from : AROVRDU.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfClosFile

IF llCustomer
  USE IN CUSTOMER
ENDIF

IF llCredit
  USE IN CREDIT
ENDIF

IF llDebit
  USE IN DEBIT
ENDIF

IF llGldist
  USE IN GLDIST
ENDIF

IF llOpenFac
  USE IN FACTOR
ENDIF

IF USED(lcOvrDue)
  CLOSE INDEX
  USE IN (lcOvrDue)
  ERASE (gcWorkDir + lcOvrDue + '.CDX')
ENDIF

IF USED(lcGLTemp)
  USE IN (lcGLTemp)
ENDIF

*--End of lfClosFile.

