*:***************************************************************************
*: Program file  : ICROYLTT.PRG
*: Program desc. : CUSTOMIZED ROYALTY REPORT FOR TIP TOPCOMPUTERS LTD.
*: Date          : 11/05/2002
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200426
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICROYLTY
*:***************************************************************************
*: Modifications :
*: B606698,1 BWA 11/28/2002 Fix the bug of Amount field in sort by account = 0.[FIX IN FRX]
*: B606864,1 BWA 01/27/2003 Add the currency filter to the report.
***************************************************************************
*-- ldRPPLDate Variable that hold From date value
*-- ldRPPHDate Variable that hold to   date value
*-- lcRPReprt Variable that hold the type of the report.

*-- Variables Declerations.
LdfDate = ldRPPLDate
LdtDate = ldRPPHDate

*--THE STYLE LENGTH
STORE 0 TO lnLenthRo
lnLenthRo = LEN(gfItemMask('PM'))

*--Functions to creat the Temp. file and the varible filter.
=lfCreatTmp()
=lfvCrATVar()

DO CASE
  CASE EMPTY(LdfDate) AND EMPTY(LdtDate)
    LcFilter  = ".T."
    LcFilter2 = ".T."
  CASE EMPTY(LdfDate)
    LcFilter  = "InvDate <= LdtDate"
    LcFilter2 = "Crdate <= LdtDate"
  OTHERWISE
    LcFilter  = "BETWEEN(InvDate,LdfDate,LdtDate)"
    LcFilter2 = "BETWEEN(Crdate,LdfDate,LdtDate)"
ENDCASE

*--Filter the royalty code.
LcFilter1 = lcRoyalty

*-- Variables Declerations.

*B606864,1 BWA 01/27/2003 Add the "lnCpyNtRt" to hold the net return before change it's value.[START]
*STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet
STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet , lnCpyNtRt
*B606864,1 BWA 01/27/2003.[END]

LcSort = 'LEFt(STYLE,lnLenthRo)'

*B606864,1 BWA 01/27/2003 Get the value of this variable "llCallGfam".[START]
llCallGfam = EMPTY(laOGFxFlt[lnCurrPos,6]) OR (!EMPTY(laOGFxFlt[lnCurrPos,6]) AND lcRpCurr <> 'F')
*B606864,1 BWA 01/27/2003.[END]

IF lcRPReprt = 'S'
  =lFStyRep()
  SELECT (lcRoyTemp)
ELSE
  =lFAccRep()
  SELECT (lcRoyTemp1)
ENDIF

IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
IF lcRPReprt = 'S'
  INDEX ON Royalty + &LcSort TAG lcRoyTemp OF (gcWorkDir + lcRoyTemp)
ELSE
  INDEX ON Royalty + &LcSort TAG lcRoyTemp1 OF (gcWorkDir + lcRoyTemp1)
ENDIF
LOCATE

DO gfDispRe WITH EVAL('lcRpForm')
=lfClearAll()
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lFStyRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Royalty report by style.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lFStyRep

SELECT INVLINE
SET RELATION TO Invoice INTO INVHDR ADDITIVE
SELECT STYLE
SET RELATION TO LEFT(Style,lnLenthRo) INTO INVLINE ,;
                LEFT(Style,lnLenthRo) INTO RETLINE ADDITIVE
LOCATE
WAIT 'Collecting data. Please wait ...' WINDOW NOWAIT
STORE "" TO lcStyle

SCAN FOR IIF(EMPTY(lcRoyalty) , STYLE.Royalty   = "" , STYLE.Royalty $ lcRoyalty )
  IF lcStyle # LEFT(STYLE.Style,lnLenthRo)
    lcStyle = LEFT(STYLE.Style,lnLenthRo)

    *B606864,1 BWA 01/27/2003 Add the "lnCpyNtRt" to hold the net return before change it's value.[START]
    *STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet
    STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet , lnCpyNtRt
    *B606864,1 BWA 01/27/2003.[END]

    SELECT INVLINE
    LnRec = RECNO()

    SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR EVAL(LcFilter) AND INVHDR.STATUS <> "V"

      *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.
      IF INVHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
      *B606864,1 BWA 01/27/2003.[END]

        LnTotAmt = LnTotAmt + Price * TotQty
        LnDisc = LnDisc + (Price * TotQty) * INVHDR.Trde_Disc/100
        lnDisc = ROUND(lnDisc,2)
        IF EVAL(LcFilter)
          LnShip = LnShip + TOTQTY
        ENDIF
     *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
     ENDIF
     *B606864,1 BWA 01/27/2003.[END]

    ENDSCAN

    IF BETWEEN(LnRec,1,RECCOUNT('INVLINE'))
      GOTO LnRec IN ('INVLINE')
    ENDIF

    =lfGetRet()
    LcDesc = STYLE.Desc

  *B606864,1 BWA 01/27/2003 Section of converting the values in case use the currency.[START]
  PRIVATE lcCurAls
  lcCurAls = SELECT(0)
  SELECT INVHDR

  *--The old line Gross_Sale - LnDisc - Ret_Amt
  lnNet_amt = LnTotAmt - LnDisc - lnCpyNtRt

  *--The old Roy_amt >> (Ship_Qty - Ret_Qty) * STYLE.CHARGEROL
  lnRoy_amt = (LnShip - LnTotRet) * STYLE.CHARGEROL

  IF LnTotAmt > 0
    LnTotAmt = IIF(llCallGfam,gfAmntDisp(LnTotAmt , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnTotAmt)
  ENDIF
  
  IF LnDisc > 0
    LnDisc = IIF(llCallGfam,gfAmntDisp(LnDisc , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnDisc)
  ENDIF

  IF lnNet_amt > 0
    lnNet_amt = IIF(llCallGfam,gfAmntDisp(lnNet_amt , lcRpCurr , ldRpExDate , lcRpTmpNam) , lnNet_amt)
  ENDIF

  IF lnRoy_amt > 0
    lnRoy_amt = IIF(llCallGfam,gfAmntDisp(lnRoy_amt , lcRpCurr , ldRpExDate , lcRpTmpNam) , lnRoy_amt)
  ENDIF

  SELECT(lcCurAls)
  *B606864,1 BWA 01/27/2003.[END]

    IF lnShip > 0 OR LnTotRet > 0
      SELECT (lcRoyTemp)
      APPEND BLANK
      *REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty                       ,;
              &lcRoyTemp..Style      WITH LEFT(STYLE.Style,lnLenthRo)         ,;
              &lcRoyTemp..Desc       WITH LcDesc                              ,;
              &lcRoyTemp..Ship_Qty   WITH LnShip                              ,;
              &lcRoyTemp..Gross_Sale WITH LnTotAmt                            ,;
              &lcRoyTemp..Discount   WITH LnDisc                              ,;
              &lcRoyTemp..Ret_Qty    WITH LnTotRet                            ,;
              &lcRoyTemp..Ret_Amt    WITH LnNetRet                            ,;
              &lcRoyTemp..Adj_Qty    WITH Ship_Qty - Ret_Qty                  ,;
              &lcRoyTemp..NET_AMT    WITH Gross_Sale - LnDisc - Ret_Amt       ,;
              &lcRoyTemp..Roy_Amt    WITH (Ship_Qty - Ret_Qty) * STYLE.CHARGEROL ,;
              &lcRoyTemp..Avg_Price  WITH IIF(LnTotAmt = 0,0, IIF(LnShip > 0 , LnTotAmt/LnShip , 0 ))

      REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty                       ,;
              &lcRoyTemp..Style      WITH LEFT(STYLE.Style,lnLenthRo)         ,;
              &lcRoyTemp..Desc       WITH LcDesc                              ,;
              &lcRoyTemp..Ship_Qty   WITH LnShip                              ,;
              &lcRoyTemp..Gross_Sale WITH LnTotAmt                            ,;
              &lcRoyTemp..Discount   WITH LnDisc                              ,;
              &lcRoyTemp..Ret_Qty    WITH LnTotRet                            ,;
              &lcRoyTemp..Ret_Amt    WITH LnNetRet                            ,;
              &lcRoyTemp..Adj_Qty    WITH Ship_Qty - Ret_Qty                  ,;
              &lcRoyTemp..NET_AMT    WITH lnNet_amt                           ,;
              &lcRoyTemp..Roy_Amt    WITH lnRoy_amt                           ,;
              &lcRoyTemp..Avg_Price  WITH IIF(LnTotAmt = 0,0, IIF(LnShip > 0 , LnTotAmt/LnShip , 0 ))
    ENDIF
  ENDIF
ENDSCAN
WAIT CLEAR

*--End of lFStyRep.
*!*************************************************************
*! Name      : lFAccRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Royalty report by ACCOUNT.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lFAccRep

LcSort = 'ACCOUNT + TYPE + LEFT(STYLE,lnLenthRo) + INVOICE'
SET ORDER TO TAG Retlinei IN "RETLINE"
SELECT INVLINE
SET RELATION TO Invoice INTO INVHDR ADDITIVE
SET RELATION TO INVLINE.STYLE INTO RETLINE ADDITIVE
SELECT RETLINE
SET ORDER TO TAG Retlines
SET RELATION TO RETLINE.CRMEMO INTO RETHDR ADDITIVE

SELECT STYLE
SET RELATION TO LEFT(STYLE.Style,lnLenthRo) INTO INVLINE ADDITIVE

LOCATE
WAIT 'Collecting data. Please wait ...' WINDOW NOWAIT
STORE "" TO lcStylAcc , lcStylRet

SCAN FOR IIF(EMPTY(lcRoyalty) , STYLE.Royalty   = "" , STYLE.Royalty   $ lcRoyalty )
  IF lcStylAcc # LEFT(STYLE.Style,lnLenthRo)
    lcStylAcc = LEFT(STYLE.Style,lnLenthRo)
    STORE 0 TO LnShip , LnTotAmt , LnDisc
    LcDesc   = STYLE.Desc
    SELECT INVLINE
    SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo)

      *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
      *IF EVAL(LcFilter) AND INVHDR.STATUS <> "V"
      IF EVAL(LcFilter) AND INVHDR.STATUS <> "V" AND INVHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
      *B606864,1 BWA 01/27/2003.[END]


        LnTotAmt = Price * TotQty
        LnShip   = TOTQTY
        LnDisc   = (Price * TotQty) * INVHDR.Trde_Disc/100
        lnDisc   = ROUND(lnDisc,2)
        LnTotRet = 0
        LnNetRet = 0

        *B606864,1 BWA 01/27/2003 Add the "lnCpyNtRt" to hold the net return before change it's value.[START]
        lnCpyNtRt = 0
        *B606864,1 BWA 01/27/2003.[END]

        lcCrMemo = SPACE(6)
        lcStyle  = SPACE(lnLenthRo)

        SELECT RETLINE
        *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
        *SCAN WHILE STYLE + CRMEMO = INVLINE.STYLE FOR EVAL(LcFilter2) AND INVHDR.STATUS <> "V" AND INVLINE.INVOICE = RETLINE.INVOICE AND RETHDR.STATUS <> "V"
        SCAN WHILE STYLE + CRMEMO = INVLINE.STYLE FOR EVAL(LcFilter2) AND ;
           INVHDR.STATUS <> "V" AND INVLINE.INVOICE = RETLINE.INVOICE AND ;
           RETHDR.STATUS <> "V" AND RETHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
           *B606864,1 BWA 01/27/2003.[END]

          LnTotRet = LnTotRet+RETLINE.TotQty
          LnNetRet = LnNetRet+RETLINE.Amount
          lcCrMemo = CrMemo
          lcStyle  = LEFT(RetLine.Style,lnLenthRo)

          *B606864,1 BWA 01/27/2003 Convert the variable in case using the currency filter.[START]
          lnCpyNtRt = lnCpyNtRt + RETLINE.Amount
          PRIVATE lcCurAlsR
          lcCurAlsR = SELECT(0)
          SELECT RETHDR
          IF LnNetRet > 0
            LnNetRet = IIF(llCallGfam,gfAmntDisp(LnNetRet , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnNetRet)
          ENDIF
          SELECT(lcCurAlsR)
          *B606864,1 BWA 01/27/2003.[END]

        ENDSCAN
        SELECT INVLINE

        *B606864,1 BWA 01/27/2003 Section of converting the values in case use the currency.[START]
        PRIVATE lcCurAls
        lcCurAls = SELECT(0)
        SELECT INVHDR

        *--The old line Gross_Sale - LnDisc - Ret_Amt
        lnNet_amt = LnTotAmt - LnDisc - lnCpyNtRt

        *--The old Roy_amt >> (Ship_Qty - Ret_Qty) * STYLE.CHARGEROL
        lnRoy_amt = (LnShip - LnTotRet) * STYLE.CHARGEROL

        IF LnTotAmt > 0
          LnTotAmt = IIF(llCallGfam,gfAmntDisp(LnTotAmt , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnTotAmt)
        ENDIF
  
        IF LnDisc > 0
          LnDisc = IIF(llCallGfam,gfAmntDisp(LnDisc , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnDisc)
        ENDIF

        IF lnNet_amt > 0
          lnNet_amt = IIF(llCallGfam,gfAmntDisp(lnNet_amt , lcRpCurr , ldRpExDate , lcRpTmpNam) , lnNet_amt)
        ENDIF

        IF lnRoy_amt > 0
          lnRoy_amt = IIF(llCallGfam,gfAmntDisp(lnRoy_amt , lcRpCurr , ldRpExDate , lcRpTmpNam) , lnRoy_amt)
        ENDIF

        SELECT(lcCurAls)
        *B606864,1 BWA 01/27/2003.[END]

        SELECT (lcRoyTemp)
        IF !SEEK(lcCrMemo+lcStyle)
          APPEND BLANK
        ENDIF
        REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty                               ,;
                &lcRoyTemp..Style      WITH LEFt(STYLE.Style,lnLenthRo)                 ,;
                &lcRoyTemp..Desc       WITH LcDesc                                      ,;
                &lcRoyTemp..Ship_Qty   WITH &lcRoyTemp..Ship_Qty   + LnShip             ,;
                &lcRoyTemp..Gross_Sale WITH &lcRoyTemp..Gross_Sale + LnTotAmt           ,;
                &lcRoyTemp..TotAmt     WITH &lcRoyTemp..TotAmt     + LnTotAmt           ,;
                &lcRoyTemp..Discount   WITH &lcRoyTemp..Discount   + LnDisc             ,;
                &lcRoyTemp..Ret_Qty    WITH &lcRoyTemp..Ret_Qty    + LnTotRet           ,;
                &lcRoyTemp..Ret_Amt    WITH &lcRoyTemp..Ret_Amt    + LnNetRet           ,;
                &lcRoyTemp..Adj_Qty    WITH &lcRoyTemp..Adj_Qty    + Ship_Qty - Ret_Qty ,;
                &lcRoyTemp..Account    WITH INVLINE.Account                             ,;
                &lcRoyTemp..Type       WITH "1"                                         ,;
                &lcRoyTemp..Invoice    WITH IIF(lnTotRet = 0 , InvLine.Invoice , lcCrMemo )

        SELECT INVLINE
      ELSE
        SELECT RETLINE
        SET ORDER TO TAG Retlines
        STORE 0 TO LnShip , LnTotAmt , LnDisc
        =SEEK(LEFT(STYLE.Style,lnLenthRo))
        IF lcStylRet # LEFT(STYLE.Style,lnLenthRo)
          lcStylRet = LEFT(STYLE.Style,lnLenthRo)

          *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
          *SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR !EMPTY(Invoice) AND EVAL(LcFilter2) AND INVHDR.STATUS <> "V"
          SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR !EMPTY(Invoice) AND EVAL(LcFilter2) AND INVHDR.STATUS <> "V" ;
            AND RETHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
            *B606864,1 BWA 01/27/2003.[END]

            SELECT (lcRoyTemp)
            IF !SEEK(RETLINE.CRMEMO + LEFT(STYLE.STYLE,lnLenthRo))
              APPEND BLANK
              REPLACE Royalty    WITH STYLE.Royalty               ,;
                      Style      WITH LEFT(STYLE.Style,lnLenthRo) ,;
                      Desc       WITH LcDesc                      ,;
                      Discount   WITH LnDisc                      ,;
                      Ret_Qty    WITH RETLINE.TotQty              ,;
                      Ret_Amt    WITH RETLINE.Amount              ,;
                      Adj_Qty    WITH Ship_Qty - Ret_Qty          ,;
                      Account    WITH RETLINE.Account             ,;
                      Invoice    WITH RETLINE.Crmemo              ,;
                      Type WITH "1"
            ELSE
              REPLACE Ret_Qty WITH Ret_Qty + RETLINE.TotQty       ,;
                      Ret_Amt WITH Ret_Amt + RETLINE.Amount       ,;
                      Adj_Qty WITH (Ship_Qty - Ret_Qty)
            ENDIF
            SELECT RETLINE
          ENDSCAN
        ENDIF
      ENDIF
      SELECT INVLINE
    ENDSCAN

    SELECT RETLINE
    *-- Get returns without invoice#
    STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet
    =SEEK(LEFT(STYLE.STYLE,lnLenthRo))

    *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
    *SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR EMPTY(Invoice) AND EVAL(LcFilter2)
     SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR EMPTY(Invoice) AND EVAL(LcFilter2);
       AND RETHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
       *B606864,1 BWA 01/27/2003.[END]

      LnTotRet = RETLINE.TotQty
      LnNetRet = RETLINE.Amount

      *B606864,1 BWA 01/27/2003 Section of converting the values in case use the currency.[START]
      PRIVATE lcCurAlsR
      lcCurAlsR = SELECT(0)
      SELECT RETHDR

      IF LnTotRet > 0
        LnTotRet = IIF(llCallGfam,gfAmntDisp(LnTotRet , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnTotRet)
      ENDIF

      IF LnNetRet > 0
        LnNetRet = IIF(llCallGfam,gfAmntDisp(LnNetRet , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnNetRet)
      ENDIF

      SELECT(lcCurAlsR)
      *B606864,1 BWA 01/27/2003.[END]

      SELECT (lcRoyTemp)
      APPEND BLANK
      REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty               ,;
              &lcRoyTemp..Style      WITH LEFT(STYLE.Style,lnLenthRo) ,;
              &lcRoyTemp..Desc       WITH LcDesc                      ,;
              &lcRoyTemp..Ship_Qty   WITH LnShip                      ,;
              &lcRoyTemp..Gross_Sale WITH LnTotAmt                    ,;
              &lcRoyTemp..TotAmt     WITH LnTotAmt                    ,;
              &lcRoyTemp..Discount   WITH LnDisc                      ,;
              &lcRoyTemp..Ret_Qty    WITH LnTotRet                    ,;
              &lcRoyTemp..Ret_Amt    WITH LnNetRet                    ,;
              &lcRoyTemp..Adj_Qty    WITH Ship_Qty - Ret_Qty          ,;
              &lcRoyTemp..Account    WITH RETLINE.Account             ,;
              &lcRoyTemp..Invoice    WITH RETLINE.Crmemo              ,;
              &lcRoyTemp..Type       WITH "2"
      SELECT RETLINE
    ENDSCAN
  ENDIF
ENDSCAN

SELECT Royalty                       ,;
       Style                         ,;
       Desc                          ,;
       SUM(Totamt)     AS Totamt     ,;
       Avg_price                     ,;
       SUM(Ship_Qty)   AS Ship_Qty   ,;
       SUM(Gross_Sale) AS Gross_Sale ,;
       SUM(Discount)   AS Discount   ,;
       SUM(Ret_Qty)    AS Ret_Qty    ,;
       SUM(Ret_Amt)    AS Ret_Amt    ,;
       SUM(Adj_Qty)    AS Adj_Qty    ,;
       NET_AMT                       ,;
       Roy_Amt                       ,;
       Invoice                       ,;
       Account                       ,;
       TaxRoylty                     ,;
       Type                           ;
FROM (lcRoyTemp)                      ;
INTO TABLE (GCWORKDIR + lcRoyTemp1)   ;
GROUP BY STYLE,INVOICE,TYPE

SELECT (lcRoyTemp1)
SET RELATION TO STYLE INTO STYLE ADDITIVE
LOCATE
SCAN WHILE !EOF()
  REPLACE Avg_Price WITH IIF(TotAmt = 0,0,TotAmt/Ship_Qty) ,;
          NET_AMT   WITH Gross_Sale - Discount - Ret_Amt   ,;
          Roy_Amt   WITH Adj_Qty * STYLE.CHARGEROL
ENDSCAN
SET RELATION OFF INTO STYLE
WAIT CLEAR

*--End of lFAccRep.
*!*************************************************************
*! Name      : lfGetRet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Calculate total returns for the current style.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfGetRet()
*!*************************************************************
FUNCTION lfGetRet
PRIVATE lcAlias

lcAlias = SELECT(0)
SELECT RETLINE
SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR EVAL(LcFilter2)

  *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
  *IF SEEK(RETLINE.CRMEMO,'RETHDR') AND RETHDR.STATUS <> "V"
  IF SEEK(RETLINE.CRMEMO,'RETHDR') AND RETHDR.STATUS <> "V" AND RETHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
  *B606864,1 BWA 01/27/2003.[END]

    LnTotRet = LnTotRet + TotQty
    LnNetRet = LnNetRet + Amount

    *B606864,1 BWA 01/27/2003 Convert the variable in case using the currency filter.[START]
    lnCpyNtRt = lnCpyNtRt + Amount
    PRIVATE lcCurAlsR
    lcCurAlsR = SELECT(0)
    SELECT RETHDR
    IF LnNetRet > 0
      LnNetRet = IIF(llCallGfam,gfAmntDisp(LnNetRet , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnNetRet)
    ENDIF

    SELECT(lcCurAlsR)
    *B606864,1 BWA 01/27/2003.[END]

  ENDIF
ENDSCAN

SELECT(lcAlias)

*--End of lfGetRet.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
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

CREATE TABLE (lcRoyTemp)  (Royalty C(6)       , Style C(lnLenthRo), Desc C(20)   , Totamt N(10,2)  , Avg_price N(8,2) , Ship_qty N(6) ,;
                           Gross_sale N(10,2) , Discount N(9,3) , Ret_qty N(6) , Ret_amt N(8,2)    , Adj_qty N(6,2)   , Net_amt N(8,2),;
                           Roy_amt N(8,2)     , Invoice C(6)    , Account C(5) , Type C(1)         , TaxRoylty N(5,2))
ZAP
INDEX ON INVOICE + LEFT(Style,lnLenthRo) TAG lcRoyTemp OF (gcWorkDir + lcRoyTemp)

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
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
lcTitle = 'Invoice Date'
DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
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

*-- End of lfvPbOk.
*!*************************************************************
*! Name      : lfFillARAY
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Function to fill Royalty code.
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

DIMENSION laRpSrylty[1,1],laRpTrylty[1,1]
STORE '' TO laRpSrylty,laRpTrylty

lcAlias = SELECT(0)

SELECT CODES
LOCATE
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'ROYALTY   ' AND crltField = 'N';
                                        INTO ARRAY laRpSrylty
SELECT(lcAlias)

*-- End of lfFillARAY.
*!*************************************************************
*! Name      : lfvRoyalty
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Function to fill the Royalty mover with the code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRoyalty()
*!*************************************************************
FUNCTION lfvRoyalty
= gfMover(@laRpSrylty,@laRpTrylty,'Royalty ',.T.,'')

*--End of lfvRoyalty.
*!*************************************************************
*! Name      : lfvCratVar
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
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

*--The Royalty.
STORE SPACE(0) TO lcRoyalty
FOR lnInd = 1 TO ALEN(laRpTrylty)
  lcRoyalty = lcRoyalty + PADR(laRpTrylty[lnInd],6) + ' | '
ENDFOR
lcRoyalty = IIF(ALLTRIM(lcRoyalty) = '|','',lcRoyalty)

*-- End of lfvCrATVar.
*!*************************************************************
*! Name      : lfNamForm
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Function to get the name of the report to print.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNamForm()
*!*************************************************************
FUNCTION lfNamForm

lcRpForm = IIF(lcRPReprt = 'S','ICROYLTY','ICROYLT2')
= lfRepPltFr(lcRpForm)

*--End of LFNAMFORM.
*!*************************************************************
*! Name      : lfClearAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
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

IF USED (lcRoyTemp1)
  SELECT (lcRoyTemp1)
  CLOSE INDEX
  USE
  ERASE (gcWorkDir + lcRoyTemp1 + '.DBF')
  ERASE (gcWorkDir + lcRoyTemp1 + '.CDX')
ENDIF

IF USED (lcRoyTemp)
  SELECT (lcRoyTemp)
  CLOSE INDEX
  USE
  ERASE (gcWorkDir + lcRoyTemp + '.DBF')
  ERASE (gcWorkDir + lcRoyTemp + '.CDX')
ENDIF

*--End of lfClearAll.
*B606864,1 BWA 01/27/2003 Add this functions to support the currency filter.[START]
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/27/2003
*! Purpose   : This function called from the currency Display
*!             field to display currency options screen .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*-- If forgin currency you must be sure invoice currency has value.
IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[lnCurrPos,6])
  laOGFxFlt[lnCurrPos,6] = gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[lnCurrPos,6]

*--End of lfvCurDisp.
*!*************************************************************
*! Name      : lfvCurCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/27/2003
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvCurCode()
*!*************************************************************
FUNCTION lfvCurCode
lnAlias = SELECT(0)

*-- Allow blank curreny in case user have no selected forign currency.
IF EMPTY(laOGFxFlt[lnCurrPos,6]) .AND. lcRpCurr <> 'F' 
  RETURN
ENDIF

IF !SEEK(laOGFxFlt[lnCurrPos,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[lnCurrPos,6]) > 0
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
    laOGFxFlt[lnCurrPos,6] = lcOldCurr
  ELSE
    laOGFxFlt[lnCurrPos,6] = laTemp[1]
  ENDIF
ENDIF

SHOW GET laOGFxFlt[lnCurrPos,6]

SELECT(lnAlias)

*--End of lfvCurCode.
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/27/2003
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwCurCode()
*!*************************************************************
FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[lnCurrPos,6]

*--End of lfwCurCode.
*!*************************************************************
*! Name      : lfRepShow
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/27/2003
*! Purpose   : When function to initial the wanted variables.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfRepShow

lnCurrPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.CCURRCODE'),1)
laOGFxFlt[lnCurrPos,6] = gcBaseCurr
=lfOGShowGet("lnRepCurr")

*--End of lfRepShow.
*B606864,1 BWA 01/27/2003.[END]