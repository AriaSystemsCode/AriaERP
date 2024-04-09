*:***************************************************************************
*: Program file  : ARMBIEEC.PRG
*: Program desc. : CUSTOMIZED EXPORTS TO NON EUROPEAN COMMUNITY COUNTRIES.  
*! Date          : 06/25/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C200196
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARMBIEEC
*:****************************************************************************

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

STORE SPACE(0) TO lcContCode
IF ldCompFrom # ldFrom OR ldCompTo # ldTo OR (ldFrom = {} AND ldTo = {} )
  ldCompFrom = ldFrom
  ldCompTo = ldTo
  =lfGetData()
  =lfCreatTmp()
  =lfcolctDat()
ENDIF

SELECT (lcDataMbi)
LOCATE
DO gfDispRe WITH EVALUATE('lcRpForm')

*-- Get the original setting.
SET CENTURY &lcCentury
SET DEVICE TO SCREEN

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfGetData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/25/2001
*! Purpose   : Function to get the data from the SYCINT.DBF
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetData

SELECT SYCINT
SCAN
  IF SYCINT.LEUROPCOM
    lcContCode = lcContCode + SYCINT.CCONT_CODE
  ENDIF
ENDSCAN

*-- End of lfGetData.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/25/2001
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

CREATE CURSOR (lcDataMbi) (StylComCod C(20)  , TransNo C(6)     , TransDate D(8) , STYLE C(19)     ,;
                           Description C(60) , Quantity N(14)   , AccountNo C(5) , AcountNam C(30) ,;
                           Country C(20))
ZAP
INDEX ON STYLCOMCOD + DTOS(TRANSDATE) TAG STYLCOMCOD OF (gcWorkDir+lcDataMbi)

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/25/2001
*! Purpose   : Function to Collect the date from the Invhdr and
*!           : the Rethdr file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat

*--From the INVHDR file
SELECT INVHDR
SET RELATION TO Invhdr.INVOICE INTO INVLINE ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SELECT INVLINE
SET RELATION TO INVLINE.STYLE INTO STYLE ADDITIVE

SELECT INVHDR
SCAN FOR INVOICE = ""
  IF (ldTo # {} AND BETWEEN(INVHDR.INVDATE,ldFrom,ldTo)) ;
     OR ldTo = {}

    IF INVHDR.STATUS # "V" AND !(CUSTOMER.CCONT_CODE $ lcContCode)
      SELECT INVLINE
      SCAN FOR INVOICE + STR(LINENO,6) = INVHDR.INVOICE
        WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
        SELECT (lcDataMbi)
        APPEND BLANK
        REPLACE StylComCod  WITH STYLE.CCOMCODE  ,;
                TransNo     WITH INVLINE.INVOICE ,;
                TransDate   WITH INVLINE.INVDATE ,;
                STYLE       WITH INVLINE.STYLE   ,;
                Description WITH INVLINE.DESC1   ,;
                Quantity    WITH INVLINE.TOTQTY  ,;
                AccountNo   WITH INVLINE.ACCOUNT ,;
                AcountNam   WITH CUSTOMER.BTNAME ,;
                Country     WITH lfContry()
      ENDSCAN
    ENDIF
  ENDIF
ENDSCAN
SET RELATION OFF INTO INVLINE
SET RELATION OFF INTO CUSTOMER

SELECT INVLINE
SET RELATION OFF INTO STYLE

*--From the RETHDR file
SELECT RETHDR
SET RELATION TO RETHDR.CRMEMO INTO RETLINE ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SELECT RETLINE
SET RELATION TO RETLINE.STYLE INTO STYLE ADDITIVE

SELECT RETHDR
SCAN FOR CRMEMO = ""
  IF (ldTo # {} AND BETWEEN(RETHDR.CRDATE,ldFrom,ldTo)) ;
     OR ldTo = {}

    IF RETHDR.STATUS # "V" AND !(CUSTOMER.CCONT_CODE $ lcContCode)
      SELECT RETLINE
      SCAN FOR CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO
        WAIT WINDOW 'Selecting Records For The Report ...' + CRMEMO NOWAIT
        SELECT (lcDataMbi)
        APPEND BLANK
        REPLACE StylComCod  WITH STYLE.CCOMCODE  ,;
                TransNo     WITH RETLINE.CRMEMO  ,;
                TransDate   WITH RETLINE.CRDATE  ,;
                STYLE       WITH RETLINE.STYLE   ,;
                Description WITH STYLE.DESC1     ,;
                Quantity    WITH -RETLINE.TOTQTY ,;
                AccountNo   WITH RETLINE.ACCOUNT ,;
                AcountNam   WITH CUSTOMER.BTNAME ,;
                Country     WITH lfContry()
      ENDSCAN
    ENDIF
  ENDIF
ENDSCAN
SET RELATION OFF INTO RETLINE
SET RELATION OFF INTO CUSTOMER

SELECT RETLINE
SET RELATION OFF INTO STYLE
WAIT CLEAR

*--End of lfcolctDat
*!*************************************************************
*! Name      : lfContry
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/25/2001
*! Purpose   : Get the country description from SYCINT.
*!*************************************************************
*! Called from : lfcolctDat
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfContry()
*!*************************************************************
FUNCTION lfContry
PRIVATE lcConDesc , lcAlias , lcKey

lcAlias = SELECT(0)
STORE SPACE(0) TO lcConDesc
SELECT SYCINT
lcKey = EVAL(KEY())

IF SEEK(CUSTOMER.CCONT_CODE)
  lcConDesc = SYCINT.CCONT_DESC
ENDIF

=SEEK(lcKey)
SELECT(lcAlias)

RETURN lcConDesc

*--End of lfContry.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/25/2001
*! Purpose   : Showes date range screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

lcTitle = 'Transaction Period'

DO (gcRepHome + gcAct_Appl + '\Daterng.SPR')

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/25/2001
*! Purpose   : Validate Transaction PERIOD screen's OK button.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************
FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*-- End of lfvPbOk.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2001
*! Purpose   : When function to disable the save button.
*!*************************************************************
*! Called from :SYDREPRT.DBF
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

SHOW GET pbOGSave DISABLE

*--End of lfwRepWhen.