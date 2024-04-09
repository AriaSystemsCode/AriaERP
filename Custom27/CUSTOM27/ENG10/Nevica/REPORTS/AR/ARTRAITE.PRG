*:***************************************************************************
*: Program file  : ARTRAITE.PRG
*: Program desc. : CUSTOMIZED Traite Report FOR NEVICA.
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tester        : Gehan
*: Tracking Job Number: C200369
*:***************************************************************************
*: Calls :
*:    Functions : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARTRAITE.PRG
*:***************************************************************************
*: Modifications :
*: C200415,1 ADEL 10/01/02 Adjust address,Due date, and ad another option to OG.
*****************************************************************************

llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*C200415,1 (Begin) Initialize a new array fro address
DIMENSION laAddress[6]
laAddress = " "
*C200415,1 (End)

*--The main functions of the report.
=lfCreatTmp()
=lfRelation()
=lfColctDat()

*-- Browsing the report.
SELECT (lcLinesTmp)
LOCATE
IF RECCOUNT() = 0 .OR. EOF(lcLinesTmp)
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SELECT (lcLinesTmp)
DO gfDispRe WITH EVALUATE('lcRpForm')
SET CENTURY &lcCentury
WAIT CLEAR

*--Section break the relations.
SELECT (lcLinesTmp)
SET RELATION TO

SELECT CUSTOMER
SET RELATION TO
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/01/2002
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfcolctDat()
*!*************************************************************
FUNCTION lfcolctDat

STORE SPACE(0) TO lcAccount
STORE 0 TO lnDptCrdt
lcRpExp = IIF(ALLTRIM(lcRpExp) = '.T.', 'CUSTOMER.TYPE = "M" .AND. ALLTRIM(CUSTOMER.CCONT_CODE) = "FR"' , lcRpExp)
*C200415,1 (Begin) Don't print for traite = 0 (Tony's mail from NAD)
lcRpExp = lcRpExp + ' AND CUSTOMER.TRAITE <> 0'
*C200415,1 (End)

*--Section for collecting the data.
SELECT CUSTOMER
SCAN FOR &lcRpexp

  WAIT WINDOW 'Collecting data For Customer ...' + CUSTOMER.ACCOUNT NOWAIT 
  IF lcAccount # CUSTOMER.ACCOUNT
    SELECT (lcLinesTmp)
    *C200415,1 (Begin) Don't print Zero balance.
    *IF lnDptCrdt < 0
    IF lnDptCrdt <= 0
    *C200415,1 (End)
      DELETE ALL FOR MainCust = lcAccount
    ELSE
      REPLACE llPrntTot WITH .T.       ,;
              Duedate   WITH ldDueDate
    ENDIF
    lcAccount = CUSTOMER.ACCOUNT
    STORE 0 TO lnDptCrdt
    *--Section to get the due date from calculating the terms in the customer file and the code file.
    STORE SPACE(0) TO lcTEOM
    STORE 0 TO lnTDaysDue
    STORE {} TO ldDueDate
    ldBatchDt = gdSysDate
    STORE 20 TO lnEomDay

    DECLARE laTrmRltd[3,2]
    laTrmRltd[1,1] ='EOM'
    laTrmRltd[1,2] ='lcTEOM'
    laTrmRltd[2,1] ='NTERDUED'
    laTrmRltd[2,2] ='lnTDaysDue'
    laTrmRltd[3,1] ='EOMDAY'
    laTrmRltd[3,2] ='lnEomDay'
    =gfRltFld(Customer.cTermCode,@laTrmRltd,'CTERMCODE')
    lcTEOM = ALLTRIM(lcTEOM)
    lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
    IF UPPER(ALLTRIM(gcContCode))='ENG'   && in case of England
      ldDueDate = IIF(lcTEOM <> 'Y', ldBatchDt + lnTDaysDue ,;
                    CTOD('01'+SUBSTR(DTOC(GOMONTH(ldBatchDt,1)),3))-1+lnTDaysDue)
    ELSE
      ldDueDate = IIF(lcTEOM <> 'Y', ldBatchDt + lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(ldBatchDt),1,3) +'10'+;
                  SUBSTR(DTOC(ldBatchDt),6,5)),IIF(DAY(ldBatchDt)>lnEOMDay,2,1))+lnTDaysDue)
    ENDIF
    *--End of calculating the due date.

  ENDIF

  SELECT INVHDR
  LOCATE
  SCAN REST WHILE ACCOUNT + INVOICE = "" FOR INVHDR.ACCOUNT = CUSTOMER.ACCOUNT
    IF IIF(ldRpTrLDat={} .AND. ldRpTrHDat= {} , INVDATE = INVDATE ,;
       IIF(ldRpTrLDat={} .AND. ldRpTrHDat<>{} , INVDATE <= ldRpTrHDat , BETWEEN(INVDATE,ldRpTrLDat,ldRpTrHDat)))
      SELECT (lcLinesTmp)
      APPEND BLANK
      REPLACE MainCust WITH CUSTOMER.ACCOUNT ,;
              TransNo  WITH INVHDR.INVOICE   ,;
              TransDat WITH INVHDR.INVDATE   ,;
              TransRep WITH INVHDR.REP1      ,;
              DptTot   WITH INVHDR.TOTALCHG  ,;
              Duedate  WITH ldDueDate

      lnDptCrdt = lnDptCrdt + INVHDR.TOTALCHG
    ENDIF
  ENDSCAN

  SELECT RETHDR
  LOCATE
  SCAN REST WHILE ACCOUNT + CRMEMO = "" FOR RETHDR.ACCOUNT = CUSTOMER.ACCOUNT
    IF IIF(ldRpTrLDat={} .AND. ldRpTrHDat= {} , CRDATE = CRDATE ,;
       IIF(ldRpTrLDat={} .AND. ldRpTrHDat<>{} , CRDATE <= ldRpTrHDat , BETWEEN(CRDATE,ldRpTrLDat,ldRpTrHDat)))
      SELECT (lcLinesTmp)
      APPEND BLANK
      REPLACE MainCust WITH CUSTOMER.ACCOUNT ,;
              TransNo  WITH RETHDR.CRMEMO    ,;
              TransDat WITH RETHDR.CRDATE    ,;
              TransRep WITH RETHDR.SALESREP1 ,;
              CrTot    WITH RETHDR.TOTCREDIT ,;
              Duedate  WITH ldDueDate

      lnDptCrdt = lnDptCrdt - RETHDR.TOTCREDIT
    ENDIF
  ENDSCAN
ENDSCAN

IF EOF('CUSTOMER')
  SELECT (lcLinesTmp)
  *C200415,1 (Begin) Don't print Zero balance.
  *IF lnDptCrdt < 0
  IF lnDptCrdt <= 0
  *C200415,1 (End)
    DELETE ALL FOR MainCust = lcAccount
    PACK
  ELSE
    REPLACE llPrntTot WITH .T. ,;
            Duedate   WITH ldDueDate
  ENDIF
ENDIF
SELECT (lcLinesTmp)
SET RELATION TO "M" + &lcLinesTmp..maincust INTO CUSTOMER ADDITIVE
SCAN
  *C200415,1 (Begin) Don't split the amount if it's less than the Minimum
  *IF CUSTOMER.TRAITE > 1  
  lcKey = MAINCUST
  lnRecNo = RECNO()
  =SEEK(lcKey)
  SUM (DptTot-CrTot) WHILE MainCust = lcKey TO lnDptTot
  GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
  IF CUSTOMER.TRAITE > 1  AND lnDptTot >= lnRpMin
  *C200415,1 (End)
    FOR lnLop = 1 TO (CUSTOMER.TRAITE - 1)
      IF lnLop = 1
        SELECT (lcLinesTmp)
        REPLACE Type WITH 'A'
        SCATTER MEMVAR MEMO
      ENDIF

      SELECT (lcTratTmp)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE Type WITH IIF(lnlop = 1 , 'B' , 'C')
      *C200415,1 (Begin) Adjust due date for B,C
      REPLACE DueDate WITH lfCalcDue(Type)
      *C200415,1 (End)
    ENDFOR
  ENDIF
ENDSCAN

SELECT (lcLinesTmp)
APPEND FROM (lcTratTmp)
SET ORDER TO TAG Arrange

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/01/2002
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

*--Creat temp. file to collect all the transations.
CREATE Table (lcLinesTmp) (MainCust C(5)  , TransNo C(6)  , TransDat D     , TransRep C(3) ,;
                           DptTot N(12,2) , CrTot N(12,2) , llPrntTot L(1) , Type C(1)     ,;
                           Duedate D)
ZAP
INDEX ON MainCust + Type TAG Arrange OF (gcWorkDir + lcLinesTmp + ".CDX")
INDEX ON MainCust TAG Customer OF (gcWorkDir + lcLinesTmp + ".CDX")

*--Creat temp. file add the new fields in case the customer.traite more than 1.
CREATE Table (lcTratTmp) (MainCust C(5)  , TransNo C(6)  , TransDat D     , TransRep C(3) ,;
                          DptTot N(12,2) , CrTot N(12,2) , llPrntTot L(1) , Type C(1)     ,;
                          Duedate D)
ZAP
INDEX ON MainCust + Type TAG Customer OF (gcWorkDir + lcTratTmp + ".CDX")

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/01/2002
*! Purpose   : Function to create the needed relations.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT CUSTOMER
IF !('CUSTOMER.ACCOUNT' $ SET('RELATION'))
  SET RELATION TO CUSTOMER.ACCOUNT INTO INVHDR ADDITIVE
  SET RELATION TO CUSTOMER.ACCOUNT INTO RETHDR ADDITIVE
ENDIF

*--End of lfRelation.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/01/2002
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
PRIVATE lcAliasCst

lcAliasCst = SELECT(0)
SELECT Customer
SET ORDER TO Customer
GO TOP
SELECT(lcAliasCst)

*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/01/2002
*! Purpose   : Showes date range screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo

ldFrom = ldRpTrLDat
ldTo   = ldRpTrHDat
lcTitle = 'Transaction Date'

DO DateRng.Spx

ldRpTrLDat = ldFrom
ldRpTrHDat = ldTo

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/01/2002
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvpbOk()
*!*************************************************************
FUNCTION lfvpbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*--End of lfvpbOk.

*!**************************************************************************
*! Name      : lfCalcDue
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/01/02
*! Purpose   : Adjust due date for B,C
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*--C200415,1
FUNCTION lfCalcDue
PARAMETERS lcType

*--Section to get the due date from calculating the terms in the customer file and the code file.
STORE SPACE(0) TO lcTEOM
STORE 0 TO lnTDaysDue
STORE {} TO ldDueDate
ldBatchDt = gdSysDate
STORE 20 TO lnEomDay

DECLARE laTrmRltd[3,2]
laTrmRltd[1,1] ='EOM'
laTrmRltd[1,2] ='lcTEOM'
laTrmRltd[2,1] ='NTERDUED'
laTrmRltd[2,2] ='lnTDaysDue'
laTrmRltd[3,1] ='EOMDAY'
laTrmRltd[3,2] ='lnEomDay'
=gfRltFld(Customer.cTermCode,@laTrmRltd,'CTERMCODE')
lcTEOM = ALLTRIM(lcTEOM)
lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)

IF UPPER(ALLTRIM(gcContCode))='ENG'   && in case of England
  ldDueDate = IIF(lcTEOM <> 'Y', ldBatchDt + lnTDaysDue ,;
              CTOD('01'+SUBSTR(DTOC(GOMONTH(ldBatchDt,1)),3))-1+lnTDaysDue)
ELSE
  ldDueDate = IIF(lcTEOM <> 'Y', ldBatchDt + lnTDaysDue,;
              GOMONTH(CTOD(SUBSTR(DTOC(ldBatchDt),1,3) +'10'+;
              SUBSTR(DTOC(ldBatchDt),6,5)),IIF(DAY(ldBatchDt)>lnEOMDay,2,1))+lnTDaysDue)
ENDIF
RETURN ldDueDate + IIF(lcType = 'B',lnTDaysDue,lnTDaysDue*2)
*--End of calculating the due date.
*!**************************************************************************
*! Name      : lfGetAdd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/01/02
*! Purpose   : Get address
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*--C200415,1
FUNCTION lfGetAdd
laAddress = " "
lnFillNo = 0
FOR lnFrom = 1 TO 6
  lcFrom  = STR(lnFrom,1)
  IF !EMPTY(CUSTOMER.cAddress&lcFrom)
    lnFillNo = lnFillNo + 1
    laAddress[lnFillNo] = CUSTOMER.cAddress&lcFrom
  ENDIF
ENDFOR

RETURN " "

