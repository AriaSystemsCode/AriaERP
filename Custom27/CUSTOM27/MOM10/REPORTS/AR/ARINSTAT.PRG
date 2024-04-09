*:***************************************************************************
*: Program file  : ARINSTAT.PRG
*: Program desc. : INTRASTAT REPORT.
*: Date          : 08/21/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102415
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARINSTAT
*:Modifications:
*:***************************************************************************
*B607166,1 KHM 05/15/2003 Adding the currency option to filter the data
*:***************************************************************************
*-- ldRPPLDate Variable that hold From date value
*-- ldRPPHDate Variable that hold to   date value

*-- Variables Declerations.
STORE SPACE(0) TO lcCurrMO

=lfCreatTmp()
=lfvCrATVar()
=lfRelation()
=lfColctDat()

SELECT (lcLinesMm)
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
LOCATE

DO gfDispRe WITH EVAL('lcRpForm')
=lfClearAll()
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
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

CREATE TABLE (lcLinesMm) (Style C(19)   , Currency C(3) , Totqty N(7)   , Price N(8,2)    , ExtTotal N(12,2) , InvCrd C(6) ,;
                          TranDate D(8) , TranText C(5) , Weight N(6,2) , StyComCod C(20) , Ccomm C(20)      , Type C(1)   ,;
                          Dpartur C(20) , Arrival  C(20))
ZAP
INDEX ON StyComCod + Dpartur + Arrival + Type TAG lcLinesMm OF (gcWorkDir + lcLinesMm)

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfvCratVar
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCratVar()
*!*************************************************************
FUNCTION lfvCratVar

*--The filter
lcRpexp  = IIF(!EMPTY(lcRpexp),lcRpexp,'.T.')
lnCurPos = AT('SYCCURR.CCURRCODE',lcRpexp)
lcCurrMO = SUBSTR(lcRpexp,lnCurPos,25)
lcRpexp  = STRTRAN(lcRpexp, SUBSTR(lcRpexp,lnCurPos,25) , " .T. ")
lcCurrMO = SUBSTR(lcCurrMO,22,3)

lcRpexp = lcRpexp + IIF(EMPTY(lcRPSeason) , "" , " AND STYLE.SEASON = lcRPSeason ")
lcRpexp = lcRpexp + IIF(EMPTY(lcRPDiv) , "" , " AND STYLE.CDIVISION = lcRPDiv ")

*--The Country departure.
STORE SPACE(0) TO lcConDp
FOR lnInd = 1 TO ALEN(laRpTConDp)
  lcConDp = lcConDp + PADR(laRpTConDp[lnInd],6) + ' | '
ENDFOR
lcConDp = IIF(ALLTRIM(lcConDp) = '|','',lcConDp)

*--The Country Arrival.
STORE SPACE(0) TO lcArval
FOR lnInd = 1 TO ALEN(laRpTConAr)
  lcArval = lcArval + PADR(laRpTConAr[lnInd],6) + ' | '
ENDFOR
lcArval = IIF(ALLTRIM(lcArval) = '|','',lcArval)

*-- End of lfvCrATVar.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Function to prepare the relations of the program.
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

SELECT STYLE
SET RELATION TO STYLE.STYLE INTO INVLINE ADDITIVE
SET RELATION TO STYLE.STYLE INTO RETLINE ADDITIVE

SELECT INVLINE
SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE

SELECT POSLN
SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE

SELECT RETLINE
SET RELATION TO RETLINE.CRMEMO INTO RETHDR ADDITIVE

*-- End of lfRelation.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Function to Collect the date from the INVHDR and 
*!           : the RETHDR file.
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
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT STYLE
LOCATE

SCAN FOR &lcRpexp
  SELECT INVLINE
  SET RELATION TO INVLINE.STYLE   INTO POSLN  ADDITIVE

  SELECT INVHDR
  SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
  SELECT INVLINE
  SCAN REST WHILE STYLE + INVOICE + STR(LINENO,6) = STYLE.STYLE ;
            FOR IIF(ldRPPLDate = {} , .T. ,;
                IIF(ldRPPLDate = {} .AND. ldRPPHDate <> {} , INVDATE <= ldRPPHDate , BETWEEN(INVDATE,ldRPPLDate,ldRPPHDate)));
            AND IIF(EMPTY(lcConDp), POSHDR.ORIGIN = "" , ALLTRIM(POSHDR.ORIGIN) $ lcConDp );
            AND IIF(EMPTY(lcArval), POSHDR.ORIGIN = "" , ALLTRIM(CUSTOMER.CADDRESS6) $ lcArval )

    *B607166,1 KHM 05/15/2003 (Begin) Check if the invoice currency is not equal to the selected
    *B607166,1                on in the option grid then ignore this record.
    IF INVHDR.CCURRCODE  <> lcCurrMo
      LOOP
    ENDIF
    *B607166,1 KHM 05/15/2003 (End)
    
    STORE "I" TO lcCall
    lnAmount = ROUND(lfAmntDisp(INVLINE.PRICE,"O",{},lcRpTmpNam),2)

    SELECT (lcLinesMm)
    APPEND BLANK
    REPLACE &lcLinesMm..Style     WITH STYLE.STYLE        ,;
            &lcLinesMm..StyComCod WITH STYLE.CCOMCODE     ,;
            &lcLinesMm..Totqty    WITH INVLINE.TOTQTY     ,;
            &lcLinesMm..Price     WITH lnAmount           ,;
            &lcLinesMm..InvCrd    WITH INVLINE.INVOICE    ,;
            &lcLinesMm..TranDate  WITH INVHDR.INVDATE     ,;
            &lcLinesMm..TranText  WITH "Sale"             ,;
            &lcLinesMm..Type      WITH "A"                ,;
            &lcLinesMm..Ccomm     WITH STYLE.CCOMM        ,;
            &lcLinesMm..Dpartur   WITH POSHDR.ORIGIN      ,;
            &lcLinesMm..Arrival   WITH CUSTOMER.caddress6 ,;
            &lcLinesMm..ExtTotal  WITH INVLINE.TOTQTY * lnAmount ,;
            &lcLinesMm..Weight    WITH STYLE.NSTYWEIGHT * INVLINE.TOTQTY
  ENDSCAN

  SELECT INVLINE
  SET RELATION OFF INTO POSLN

  SELECT INVHDR
  SET RELATION OFF INTO CUSTOMER

  SELECT RETHDR
  SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
  SELECT RETLINE
  SET RELATION TO RETLINE.STYLE INTO POSLN ADDITIVE

  SELECT RETLINE
  SCAN REST WHILE STYLE + CRMEMO = STYLE.STYLE ;
            FOR IIF(ldRPPLDate = {} , .T. ,;
                IIF(ldRPPLDate = {} .AND. ldRPPHDate <> {} , CRDATE <= ldRPPHDate , BETWEEN(CRDATE,ldRPPLDate,ldRPPHDate)));
            AND IIF(EMPTY(lcConDp), POSHDR.ORIGIN = "" , ALLTRIM(POSHDR.ORIGIN) $ lcConDp );
            AND IIF(EMPTY(lcArval), POSHDR.ORIGIN = "" , ALLTRIM(CUSTOMER.CADDRESS6) $ lcArval )

    *B607166,1 KHM 05/15/2003 (Begin) Check if the return currency is not equal to the selected
    *B607166,1                on in the option grid then ignore this record.
    IF RETHDR.CCURRCODE  <> lcCurrMo
      LOOP
    ENDIF
    *B607166,1 KHM 05/15/2003 (End)

    STORE "C" TO lcCall
    lnAmount = ROUND(lfAmntDisp(RETLINE.PRICE,"O",{},lcRpTmpNam),2)

    SELECT (lcLinesMm)
    APPEND BLANK
    REPLACE &lcLinesMm..Style     WITH STYLE.STYLE        ,;
            &lcLinesMm..StyComCod WITH STYLE.CCOMCODE     ,;
            &lcLinesMm..Totqty    WITH RETLINE.TOTQTY     ,;
            &lcLinesMm..Price     WITH lnAmount           ,;
            &lcLinesMm..InvCrd    WITH RETLINE.CRMEMO     ,;
            &lcLinesMm..TranDate  WITH RETHDR.CRDATE      ,;
            &lcLinesMm..TranText  WITH "Return"           ,;
            &lcLinesMm..Type      WITH "B"                ,;
            &lcLinesMm..Ccomm     WITH STYLE.CCOMM        ,;
            &lcLinesMm..Dpartur   WITH POSHDR.ORIGIN      ,;
            &lcLinesMm..Arrival   WITH CUSTOMER.caddress6 ,;
            &lcLinesMm..ExtTotal  WITH RETLINE.TOTQTY * lnAmount ,;
            &lcLinesMm..Weight    WITH STYLE.NSTYWEIGHT * RETLINE.TOTQTY
  ENDSCAN

  SELECT RETLINE
  SET RELATION OFF INTO POSLN

  SELECT RETHDR
  SET RELATION OFF INTO CUSTOMER

ENDSCAN
SELECT (lcAlias)

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfClearAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Function to delete all the temp. files.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearAll

IF USED (lcLinesMm)
  SELECT (lcLinesMm)
  CLOSE INDEX
  USE
  ERASE (gcWorkDir + lcLinesMm + '.DBF')
  ERASE (gcWorkDir + lcLinesMm + '.CDX')
ENDIF

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Showes date range screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo
ldFrom = ldRPPLDate
ldTo   = ldRPPHDate
lcTitle = 'Date'
DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Validate date range screen's OK button.
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
  ldRPPLDate = ldFrom
  ldRPPHDate = ldTo
  CLEAR READ
ENDIF

*--End of lfvPbOk.
*!*************************************************************
*! Name      : lfFillARAY
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Function to fill the arrays with the country.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillARAY()
*!*************************************************************
FUNCTION lfFillARAY
PRIVATE lcAlias

DIMENSION laRpSConDp[1,1],laRpTConDp[1,1],laRpSConAr[1,1],laRpTConAr[1,1],laCurrDesc[1,1]
STORE '' TO laRpSConDp,laRpTConDp,laRpSConAr,laRpTConAr,laCurrDesc

lcAlias = SELECT(0)

SELECT SYCINT
GOTO TOP
SELECT DISTINCT ccont_code FROM SYCINT WHERE ccont_code = "" INTO ARRAY laRpSConDp

SELECT SYCINT
GOTO TOP
SELECT DISTINCT ccont_code FROM SYCINT WHERE ccont_code = "" INTO ARRAY laRpSConAr

DIMENSION laCurrVal[1,1]
SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

SELECT SYCCURR
SET ORDER TO CCURRCODE  && To VALIDATE currency code.
FOR lnI = 1 TO ALEN(laCurrVal,1)
  = SEEK(ALLTRIM(laCurrVal[lnI,1]))
  laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
ENDFOR

SELECT(lcAlias)

*--End of lfFillARAY.
*!*************************************************************
*! Name      : lfvConDep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Function to fill the Country mover with the countries.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvConDep()
*!*************************************************************
FUNCTION lfvConDep
= gfMover(@laRpSConDp,@laRpTConDp,'Country of departure',.T.,'')

*--End of lfvConDep.
*!*************************************************************
*! Name      : lfvConArr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Function to fill the Country mover with the countries.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvConArr()
*!*************************************************************
FUNCTION lfvConArr
= gfMover(@laRpSConAr,@laRpTConAr,'Country of Arrival',.T.,'')

*--End of lfvConArr.
*!*************************************************************
*! Name      : lfsrSty
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF

*--End of lfsrvSty.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Option Grid When function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*--Get the filter the base currency.
lnCurrPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SYCCURR.CCURRCODE'),1)
laOGFxFlt[lnCurrPos,6] = gcBaseCurr
=lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfAmntDisp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/21/2001
*! Purpose   : Convert the amounts to the selected currency.
*!*************************************************************
*! Called from : ARINSTAT.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAmntDisp()
*!*************************************************************
FUNCTION lfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile') $ "UL" , '' ,lcGetFile)

DO CASE
  CASE lcRpDispCur = 'O'
    lcCurrCode = IIF(lcCall = "I" , INVHDR.CCURRCODE , RETHDR.CCURRCODE)
    lcExSin2   = ' '
    lcBaseFrst = gcBaseCurr
    gcBaseCurr = lcCurrMO
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    gcBaseCurr = lcBaseFrst
    lnExRate   = 0
    lnUnit     = IIF(lcCall = "I" , INVHDR.NCURRUNIT , RETHDR.NCURRUNIT)
    lnExRate   = IIF(lcCall = "I" , INVHDR.NEXRATE , RETHDR.NEXRATE)
    lnExRate   = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit     = IIF(lnExRate <> 0 , lnUnit , 1)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE

SELECT (lnSavAlias)

RETURN lnAmount
*-- End of lfAmntDisp.