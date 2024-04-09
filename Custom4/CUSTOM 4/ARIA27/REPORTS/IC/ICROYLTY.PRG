*:***************************************************************************
*: Program file  : ICROYLTY.PRG
*: Program desc. : ROYALTY REPORT BY STYLE.
*: Date          : 07/22/2001
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: E301622  (REFERE TO C100681)
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
*: B804417,1 BWA 26/09/2001 Fix the bug of printing the royalty code not description.[FIX IN FRX]
*: B804479,1 BWA 21/10/2001 Fix the bug of the 2nd color doesn't print.
*: B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.
*: B606864,1 BWA 01/27/2003 Add the currency filter to the report.
*: B607424,1 ASH 07/16/2003 Fix the bug of not adding the merchandise discount to the discount field.
*: B126586,1 BWA 04/07/2005 Fix the bug of wrong print average price.
*: B131421,1 HBG 03/04/2006 FIx bug of duplicated value for retern Values 
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

lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NRYLRATE'
laTaxRat[1,2] = 'lnTaxRat'

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
*! Date      : 07/24/2001
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

*B804479,1 BWA 21/10/2001 Fix the bug of the 2nd color doesn't print.[START]
*SET RELATION TO Style INTO INVLINE ,;
                Style INTO RETLINE ADDITIVE
SET RELATION TO LEFT(Style,lnLenthRo) INTO INVLINE ,;
                LEFT(Style,lnLenthRo) INTO RETLINE ADDITIVE
*B804479,1 BWA 21/10/2001.[END]

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
        *B131421,1 HBG 03/04/2006 FIx bug of wrong value for retern Values in case of equevelent [Begin]
        *LnTotAmt = LnTotAmt + Price * TotQty
        LnTotBAmt = Price * TotQty
        *B131421,1 HBG 03/04/2006 [End]
        
        *B607424,1 ASH 07/16/2003 (Begin) Add the merchandise discount to the discount field.
        *LnDisc = LnDisc + (Price * TotQty) * INVHDR.Trde_Disc/100
        LnDisc = LnDisc + (Price * TotQty * INVHDR.Trde_Disc/100) + (Price * TotQty * INVHDR.DiscPcnt/100)
        *B607424,1 ASH 07/16/2003 (End)
        lnDisc = ROUND(lnDisc,2)

        *B126586,1 BWA 04/07/2005 Fix the bug of wrong print average price.[START]
        PRIVATE lcAlasInvh
        lcAlasInvh = SELECT(0)
		SELECT INVHDR
		
		*B131421,1 HBG 03/04/2006 FIx bug of wrong value for retern Values in case of equevelent [Begin]
		*IF LnTotAmt > 0
        *  LnTotAmt = IIF(llCallGfam,gfAmntDisp(LnTotAmt , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnTotAmt)
		*ENDIF
		IF LnTotBAmt > 0
          LnTotBAmt = IIF(llCallGfam,gfAmntDisp(LnTotBAmt , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnTotBAmt)
          LnTotAmt = LnTotAmt + LnTotBAmt
		ENDIF
        *B131421,1 HBG 03/04/2006 [End]
        
        IF LnDisc > 0
          LnDisc = IIF(llCallGfam,gfAmntDisp(LnDisc , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnDisc)
        ENDIF

	    SELECT(lcAlasInvh)
        *B126586,1 BWA 04/07/2005.[END]

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
    lnTaxRat = 0
    =gfRltFld(STYLE.Royalty , @laTaxRat , 'ROYALTY')

    *B606864,1 BWA 01/27/2003 Section of converting the values in case use the currency.[START]
    PRIVATE lcCurAls
    lcCurAls = SELECT(0)
    SELECT INVHDR

    *--The old line Gross_Sale - LnDisc - Ret_Amt
    lnNet_amt = LnTotAmt - LnDisc - lnCpyNtRt

    *--The old Roy_amt >> NET_AMT * (lnTaxRat/100)
    lnRoy_amt = lnNet_amt * (lnTaxRat/100)

    *B126586,1 BWA 04/07/2005 Fix the bug of wrong print average price.[START]
    *IF LnTotAmt > 0
    *  LnTotAmt = IIF(llCallGfam,gfAmntDisp(LnTotAmt , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnTotAmt)
    *ENDIF
  
    *IF LnDisc > 0
    *  LnDisc = IIF(llCallGfam,gfAmntDisp(LnDisc , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnDisc)
    *ENDIF

	*I comment this 2 section without add them in any place dueto they get there amounts up.
    *IF lnNet_amt > 0
    *  lnNet_amt = IIF(llCallGfam,gfAmntDisp(lnNet_amt , lcRpCurr , ldRpExDate , lcRpTmpNam) , lnNet_amt)
    *ENDIF

    *IF lnRoy_amt > 0
    *  lnRoy_amt = IIF(llCallGfam,gfAmntDisp(lnRoy_amt , lcRpCurr , ldRpExDate , lcRpTmpNam) , lnRoy_amt)
    *ENDIF
    *B126586,1 BWA 04/07/2005.[END]

    
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
               &lcRoyTemp..Roy_Amt    WITH NET_AMT * (lnTaxRat/100)            ,;
               &lcRoyTemp..TaxRoylty  WITH lnTaxRat                            ,;
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
              &lcRoyTemp..TaxRoylty  WITH lnTaxRat                            ,;
              &lcRoyTemp..Avg_Price  WITH IIF(LnTotAmt = 0,0, IIF(LnShip > 0 , LnTotAmt/LnShip , 0 ))
    ENDIF
  ENDIF
ENDSCAN
WAIT CLEAR

*--End of lFStyRep.
*!*************************************************************
*! Name      : lFAccRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/24/2001
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

*B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
SET RELATION TO INVLINE.STYLE INTO RETLINE ADDITIVE
SELECT RETLINE
SET ORDER TO TAG Retlines
SET RELATION TO RETLINE.CRMEMO INTO RETHDR ADDITIVE
*B804506,1 BWA 29/10/2001.[END]

SELECT STYLE

*B804479,1 BWA 21/10/2001 Fix the bug of the 2nd color doesn't print.[START]
*SET RELATION TO STYLE.Style INTO INVLINE ADDITIVE
SET RELATION TO LEFT(STYLE.Style,lnLenthRo) INTO INVLINE ADDITIVE
*B804479,1 BWA 21/10/2001.[END]

LOCATE
WAIT 'Collecting data. Please wait ...' WINDOW NOWAIT
STORE "" TO lcStylAcc

*B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
STORE "" TO lcStylRet
*B804506,1 BWA 29/10/2001.[END]

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
        *B607424,1 ASH 07/16/2003 (Begin) Add the merchandise discount to the discount field.
        *LnDisc   = (Price * TotQty) * INVHDR.Trde_Disc/100
        LnDisc = (Price * TotQty * INVHDR.Trde_Disc/100) + (Price * TotQty * INVHDR.DiscPcnt/100)
        *B607424,1 ASH 07/16/2003 (End)
        lnDisc   = ROUND(lnDisc,2)
        LnTotRet = 0
        LnNetRet = 0

        *B606864,1 BWA 01/27/2003 Add the "lnCpyNtRt" to hold the net return before change it's value.[START]
        lnCpyNtRt = 0
        *B606864,1 BWA 01/27/2003.[END]

        lcCrMemo = SPACE(6)
        lcStyle  = SPACE(lnLenthRo)

        *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
        *SET ORDER TO TAG Retlinei IN "RETLINE"
        *IF SEEK(Account+Invoice+Style,"RETLINE")
          SELECT RETLINE
          *SCAN WHILE Account+Invoice+Style+Crmemo = INVLINE.Account+INVLINE.Invoice+INVLINE.Style
          *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
          *SCAN WHILE STYLE + CRMEMO = INVLINE.STYLE FOR EVAL(LcFilter2) AND INVHDR.STATUS <> "V" AND INVLINE.INVOICE = RETLINE.INVOICE AND RETHDR.STATUS <> "V"
          SCAN WHILE STYLE + CRMEMO = INVLINE.STYLE FOR EVAL(LcFilter2) AND ;
             INVHDR.STATUS <> "V" AND INVLINE.INVOICE = RETLINE.INVOICE AND ;
             RETHDR.STATUS <> "V" AND RETHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
             *B606864,1 BWA 01/27/2003.[END]

             *IF SEEK(RETLINE.CRMEMO,'RETHDR') AND RETHDR.STATUS <> "V"
             *IF EVAL(LcFilter2)
             *B804506,1 BWA 29/10/2001.[END]
             *B131421,1 HBG 03/04/2006 FIx bug of duplicated value for retern Values [Begin]
             IF !SEEK(RETLINE.crmemo+LEFT(RetLine.Style,lnLenthRo)+RetLine.cret_linno,lcRetTemp)
                INSERT INTO (lcRetTemp) (crmemo,Style,cret_linno);
                				 VALUES (RETLINE.crmemo,LEFT(RetLine.Style,lnLenthRo),RetLine.cret_linno)
             *B131421,1 HBG 03/04/2006 [End]
    	        LnTotRet = LnTotRet + RETLINE.TotQty
                LnNetRet = LnNetRet + RETLINE.Amount
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
              *B131421,1 HBG 03/04/2006 FIx bug of duplicated value for retern Values [Begin]
              ENDIF
              *B131421,1 HBG 03/04/2006 [End]

              *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
              *ENDIF
            *ENDIF
            *B804506,1 BWA 29/10/2001.[END]
          ENDSCAN
          
          
          SELECT INVLINE
        *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
        *ENDIF
        *B804506,1 BWA 29/10/2001.[END]

        *B606864,1 BWA 01/27/2003 Section of converting the values in case use the currency.[START]
        PRIVATE lcCurAls
        lcCurAls = SELECT(0)
        SELECT INVHDR

        *--The old line Gross_Sale - LnDisc - Ret_Amt
        lnNet_amt = LnTotAmt - LnDisc - lnCpyNtRt

        *--The old Roy_amt >> NET_AMT * (lnTaxRat/100)
        lnRoy_amt = lnNet_amt * (lnTaxRat/100)

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

        *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
        *REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty              ,;
                 &lcRoyTemp..Style      WITH LEFt(STYLE.Style,lnLenthRo),;
                 &lcRoyTemp..Desc       WITH LcDesc                     ,;
                 &lcRoyTemp..Ship_Qty   WITH LnShip                     ,;
                 &lcRoyTemp..Gross_Sale WITH LnTotAmt                   ,;
                 &lcRoyTemp..TotAmt     WITH LnTotAmt                   ,;
                 &lcRoyTemp..Discount   WITH LnDisc                     ,;
                 &lcRoyTemp..Ret_Qty    WITH LnTotRet                   ,;
                 &lcRoyTemp..Ret_Amt    WITH LnNetRet                   ,;
                 &lcRoyTemp..Adj_Qty    WITH Ship_Qty - Ret_Qty         ,;
                 &lcRoyTemp..Account    WITH INVLINE.Account            ,;
                 &lcRoyTemp..Type       WITH "1"                        ,;
                 &lcRoyTemp..Invoice    WITH IIF(lnTotRet=0,InvLine.Invoice,lcCrMemo)

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

        *B804506,1 BWA 29/10/2001.[END]

        SELECT INVLINE
      ELSE
        SELECT RETLINE
        SET ORDER TO TAG Retlines
        STORE 0 TO LnShip , LnTotAmt , LnDisc
        =SEEK(LEFT(STYLE.Style,lnLenthRo))

        *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
        IF lcStylRet # LEFT(STYLE.Style,lnLenthRo)
          lcStylRet = LEFT(STYLE.Style,lnLenthRo)
        *B804506,1 BWA 29/10/2001.[END]

        *B606864,1 BWA 01/27/2003 Add the currency filter to the collecting.[START]
        *SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR !EMPTY(Invoice) AND EVAL(LcFilter2) AND INVHDR.STATUS <> "V"
        SCAN WHILE LEFT(Style,lnLenthRo) = LEFT(STYLE.Style,lnLenthRo) FOR !EMPTY(Invoice) AND EVAL(LcFilter2) AND INVHDR.STATUS <> "V" ;
          AND RETHDR.CCURRCODE = laOGFxFlt[lnCurrPos,6]
          *B606864,1 BWA 01/27/2003.[END]
          *B131421,1 HBG 03/04/2006 FIx bug of duplicated value for retern Values [Begin]
          IF !SEEK(RETLINE.crmemo+LEFT(RetLine.Style,lnLenthRo)+RetLine.cret_linno,lcRetTemp)
            INSERT INTO (lcRetTemp) (crmemo,Style,cret_linno);
                				 VALUES (RETLINE.crmemo,LEFT(RetLine.Style,lnLenthRo),RetLine.cret_linno)
          *B131421,1 HBG 03/04/2006 [End]

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
            *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
            ELSE
              REPLACE Ret_Qty WITH Ret_Qty + RETLINE.TotQty       ,;
                      Ret_Amt WITH Ret_Amt + RETLINE.Amount       ,;
                      Adj_Qty WITH (Ship_Qty - Ret_Qty)
            *B804506,1 BWA 29/10/2001.[END]

            ENDIF
            SELECT RETLINE
          *B131421,1 HBG 03/04/2006 FIx bug of duplicated value for retern Values [Begin]
          ENDIF
          *B131421,1 HBG 03/04/2006 [End]
        ENDSCAN
        *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
        ENDIF
        *B804506,1 BWA 29/10/2001.[END]
  

      ENDIF
      SELECT INVLINE
    ENDSCAN

    SELECT RETLINE
    *SET ORDER TO TAG Retlines
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

    *B804506,1 BWA 29/10/2001 Fix the bug of wrong Return quantity.[START]
    *SET ORDER TO TAG Retlinei IN "RETLINE"
    *B804506,1 BWA 29/10/2001.[END]

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
LOCATE
SCAN WHILE !EOF()
  lnTaxRat = 0
  = gfRltFld(&lcRoyTemp1..Royalty , @laTaxRat , 'ROYALTY')
  REPLACE Avg_Price WITH IIF(TotAmt = 0,0,TotAmt/Ship_Qty) ,;
          NET_AMT   WITH Gross_Sale - Discount - Ret_Amt   ,;
          TaxRoylty WITH lnTaxRat                          ,;
          Roy_Amt   WITH NET_AMT * (lnTaxRat/100)
ENDSCAN
WAIT CLEAR

*--End of lFAccRep.
*!*************************************************************
*! Name      : lfGetRet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/24/2001
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
*! Date      : 07/24/2001
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
                           Gross_sale N(10,2) , Discount N(9,3) , Ret_qty N(6) , Ret_amt N(8,2)  , Adj_qty N(6,2)   , Net_amt N(8,2),;
                           Roy_amt N(8,2)     , Invoice C(6)    , Account C(5) , Type C(1)       , TaxRoylty N(5,2))
ZAP
INDEX ON INVOICE + LEFT(Style,lnLenthRo) TAG lcRoyTemp OF (gcWorkDir + lcRoyTemp)

*B131421,1 HBG 03/04/2006 FIx bug of duplicated value for retern Values [Begin]
CREATE TABLE (lcRetTemp)  (crmemo C(6)       , Style C(lnLenthRo),cret_linno C(4))
ZAP
INDEX ON crmemo + LEFT(Style,lnLenthRo)+cret_linno TAG lcRetTemp OF (gcWorkDir + lcRetTemp)
*B131421,1 HBG 03/04/2006 [End]

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/24/2001
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
*! Date      : 07/24/2001
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
*! Date      : 07/24/2001
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
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'ROYALTY   ' AND crltField = 'N';
                                        INTO ARRAY laRpSrylty
SELECT(lcAlias)

*-- End of lfFillARAY.
*!*************************************************************
*! Name      : lfvRoyalty
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/24/2001
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
*! Date      : 07/24/2001
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
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
*! Date      : 07/24/2001
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
*! Date      : 07/24/2001
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

*--End of lfClearRep.

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


*!********************************************************************
*! Name      : gfAmntDisp
*! Developer : Mohamed Hassan
*! Date      : 12/26/95
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp. 
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
FUNCTION gfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile,lcGetField
PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias,lcGetField

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)
lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
lcGetField  = IIF(TYPE('lcGetField') ='C',lcGetField,'')

DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
       lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnExRate = 0

    IF EMPTY(lcGetFile)
      lnUnit = NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF  
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit , 2)

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))

    ENDIF
      
    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

  CASE lcRpDispCur = 'U'

    lnExRate   = 0
    lnUnit     = 0
    
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE

SELECT (lnSavAlias)

RETURN lnAmount
*-- end of gfAmntDisp.

