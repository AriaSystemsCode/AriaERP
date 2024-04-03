*:***************************************************************************
*: Program file  : ICROYLTY.PRG
*: Program desc. : ROYALTY REPORT BY STYLE.
*: Date          : 01/15/2007
*: System        : Aria 4XP.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Tracking Num  : N000578
*: Ticket Number :T20061221.0001
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
*: Modifications:
*! B609356,1 SMA 09/20/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
***************************************************************************
*!*	_screen.Visible =.t.
*!*	ACTIVATE WINDOW trace
*!*	SUSPEND
#Include R:\aria4xp\Reports\IC\ICROYLTY.h

IF llOGFltCh

	*-- Variables Declerations.
	loogScroll.cCROrientation = 'P'


	*--THE STYLE LENGTH
*!*		STORE 0 TO lnLenRo
*!*		lnLenRo = LEN(gfItemMask('PM'))

	*--Functions to creat the Temp. file and the varible filter.
	=lfCreatTmp()

	lnTaxRat = 0
	DECLARE laTaxRat[1,2]
	laTaxRat[1,1] = 'NRYLRATE'
	laTaxRat[1,2] = 'lnTaxRat'

	* CURRENCY Filter
	lcCurFltr= ''

	IF llMultCurr
	  lcCurFltr= lfCheckFilter(1, 'INVHDR.CCURRCODE')
	  llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
	  IF llCurFltr
	    SELECT (lcCurFltr)
	    INDEX ON CCURRCODE TAG (lcCurFltr)
	*!*	    lcseek=lcseek+" AND SEEK(ORDHDR.CCURRCODE ,'"+lcCurFltr+"')"
	  ELSE
	    IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
	      USE IN (lcCurFltr)
	    ENDIF
	    lcCurFltr= ''
	  ENDIF
	ENDIF
	llAllCurr=EMPTY(ALLTRIM(lcCurFltr)) OR RECCOUNT(lcCurFltr)=0
	*Date fiter
	lnEntPos   = lfItmPos('INVHDR.INVDATE')
	LdfDate= CTOD(SUBSTR(laOGFxFlt[lnEntPos   ,6],1,;
	                  ATC('|',laOGFxFlt[lnEntPos   ,6])-1))
	LdtDate=CTOD(SUBSTR(laOGFxFlt[lnEntPos   ,6],;
	                   ATC('|',laOGFxFlt[lnEntPos   ,6])+1))

	DO CASE
	  CASE EMPTY(LdfDate) AND EMPTY(LdtDate)
	    LcFilter  = " .T. "
	    LcFilter2 = " .T. "
	  CASE EMPTY(LdfDate)
	    LcFilter  = "InvDate <= LdtDate"
	    LcFilter2 = "Crdate <= LdtDate"
	  OTHERWISE
	    LcFilter  = "BETWEEN(InvDate,LdfDate,LdtDate)"
	    LcFilter2 = "BETWEEN(Crdate ,LdfDate,LdtDate)"
	ENDCASE

	*--Filter the royalty code.
	LcFilter1 = " .T. "
	* Check if there is a filter on Style CDIVISION
	lcCurName = lfCheckFilter(1, 'STYLE.ROYALTY')
	lcRoyal   = loOgScroll.gfTempName()
	llRoyal   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcRoyal   ,"CROYAL")
	IF llRoyal
	  SELECT (lcRoyal   )
	  INDEX on CROYAL TAG (lcRoyal   )
	  LcFilter1 =" SEEK(STYLE.ROYALTY,'"+lcRoyal   +"')"
	ENDIF


	*-- Variables Declerations.

	STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet , lnCpyNtRt

	LcSort = 'LEFt(STYLE,lnLenRo)'

	llAllCurr=EMPTY(ALLTRIM(lcCurFltr)) OR RECCOUNT(lcCurFltr)=0

	llCallGfam = llAllCurr OR (!llAllCurr AND lcRpCurr <> 'F')






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
	*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
	  *INDEX ON Royalty + &LcSort TAG lcRoyTemp OF (oAriaApplication.WorkDir + lcRoyTemp)
	  INDEX ON Royalty + &LcSort TAG lcRoyTemp 	
	ELSE
	  *INDEX ON Royalty + &LcSort TAG lcRoyTemp1 OF (oAriaApplication.WorkDir + lcRoyTemp1)
	  INDEX ON Royalty + &LcSort TAG lcRoyTemp1
	*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
	ENDIF
	LOCATE
ELSE
	IF lcRPReprt = 'S'
	  SELECT (lcRoyTemp)
	ELSE
	  SELECT (lcRoyTemp1)
	ENDIF


	IF RECCOUNT() = 0
	  *---Text : 'No Record Selected for the report..!'
	  =gfModalGen('TRM00052B00000','DIALOG')
	  SET DEVICE TO SCREEN
	  RETURN
	ENDIF
ENDIF
DO gfDispRe WITH EVAL('lcRpForm')
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

=gfOpenTable('RETLINE','RETLINES','SH','RETLINE')
=gfOpenTable('RETHDR','RETHDR','SH','RETHDR')
SELECT RETHDR
=GFSEEK('')

SELECT RETLINE
=GFSEEK('')

SELECT INVLINE
SET RELATION TO Invoice INTO INVHDR ADDITIVE

SELECT STYLE
SET RELATION TO LEFT(Style,lnLenRo) INTO INVLINE ADDITIVE
LOCATE
* N000862 ,1 Thabet Handle globalization issues [Start]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT Lang_Collecting_data WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Collecting_data,oAriaApplication.GetHeaderText("Lang_Collecting_data",AHEADERFILE)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000862 ,1 Thabet Handle globalization issues [END]

STORE "" TO lcStyle

SCAN FOR &LcFilter1

  IF lcStyle # LEFT(STYLE.Style,lnLenRo)
    lcStyle = LEFT(STYLE.Style,lnLenRo)
    =SEEK(lcStyle ,'RETLINE')
    STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet , lnCpyNtRt
    SELECT INVLINE
    LnRec = RECNO()

    SCAN WHILE LEFT(Style,lnLenRo) = LEFT(STYLE.Style,lnLenRo) FOR EVAL(LcFilter) AND INVHDR.STATUS <> "V"
      IF llAllCurR OR SEEK(INVHDR.CCURRCODE,lcCurFltr)
        LnTotBAmt = Price * TotQty
        LnDisc = LnDisc + (Price * TotQty * INVHDR.Trde_Disc/100) + (Price * TotQty * INVHDR.DiscPcnt/100)
        lnDisc = ROUND(lnDisc,2)

        PRIVATE lcAlasInvh
        lcAlasInvh = SELECT(0)
    		SELECT INVHDR
        IF LnTotBAmt > 0
          LnTotBAmt = IIF(llCallGfam,gfAmntDisp(LnTotBAmt , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnTotBAmt)
          LnTotAmt = LnTotAmt + LnTotBAmt
		    ENDIF
        IF LnDisc > 0
          LnDisc = IIF(llCallGfam,gfAmntDisp(LnDisc , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnDisc)
        ENDIF
  	    SELECT(lcAlasInvh)

        IF EVAL(LcFilter)
         LnShip = LnShip + TOTQTY
        ENDIF
      ENDIF
    ENDSCAN
    IF BETWEEN(LnRec,1,RECCOUNT('INVLINE'))
      GOTO LnRec IN ('INVLINE')
    ENDIF

    =lfGetRet()

    LcDesc = STYLE.Desc
    lnTaxRat = 0
    =gfRltFld(STYLE.Royalty , @laTaxRat , 'ROYALTY')

    PRIVATE lcCurAls
    lcCurAls = SELECT(0)
    SELECT INVHDR


    lnNet_amt = LnTotAmt - LnDisc - lnCpyNtRt

    *--The old Roy_amt >> NET_AMT * (lnTaxRat/100)
    lnRoy_amt = lnNet_amt * (lnTaxRat/100)

    SELECT(lcCurAls)
    IF lnShip > 0 OR LnTotRet > 0
      SELECT (lcRoyTemp)
      APPEND BLANK
      REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty                       ,;
              &lcRoyTemp..Style      WITH LEFT(STYLE.Style,lnLenRo)         ,;
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
LcSort = 'ACCOUNT + TYPE + LEFT(STYLE,lnLenRo) + INVOICE'
=gfOpenTable('RETLINE','Retlines','SH','RETLINE')
=gfOpenTable('RETHDR','RETHDR','SH','RETHDR')
SELECT RETHDR
=GFSEEK('')
SELECT RETLINE
=GFSEEK('')

SELECT INVLINE
SET RELATION TO Invoice INTO INVHDR ADDITIVE


SELECT STYLE
SET RELATION TO LEFT(STYLE.Style,lnLenRo) INTO INVLINE ADDITIVE

LOCATE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT Lang_Collecting_data WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Collecting_data,oAriaApplication.GetHeaderText("Lang_Collecting_data",AHEADERFILE)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

STORE "" TO lcStylAcc
STORE "" TO lcStylRet


SCAN FOR &LcFilter1
  IF lcStylAcc # LEFT(STYLE.Style,lnLenRo)
    lcStylAcc = LEFT(STYLE.Style,lnLenRo)
    =SEEK(lcStylAcc ,'RETLINE')

    STORE 0 TO LnShip , LnTotAmt , LnDisc
    LcDesc   = STYLE.Desc
    SELECT INVLINE
    SCAN WHILE LEFT(Style,lnLenRo) = LEFT(STYLE.Style,lnLenRo)
      IF EVAL(LcFilter) AND INVHDR.STATUS <> "V" AND (llAllCurR OR SEEK(INVHDR.CCURRCODE,lcCurFltr))
        LnTotAmt = Price * TotQty
        LnShip   = TOTQTY
        LnDisc = (Price * TotQty * INVHDR.Trde_Disc/100) + (Price * TotQty * INVHDR.DiscPcnt/100)
        lnDisc   = ROUND(lnDisc,2)
        LnTotRet = 0
        LnNetRet = 0

        lnCpyNtRt = 0

        lcCrMemo = SPACE(6)
        lcStyle  = SPACE(lnLenRo)
        SELECT RETLINE
         =SEEK(INVLINE.STYLE)

        SCAN REST WHILE STYLE+CRMEMO  = INVLINE.STYLE FOR EVAL(LcFilter2) AND SEEK(RETLINE.CRMEMO,'RETHDR') AND ;
          INVHDR.STATUS <> "V" AND INVLINE.INVOICE = RETLINE.INVOICE AND ;
          RETHDR.STATUS <> "V" AND (llAllCurR OR SEEK(RETHDR.CCURRCODE,lcCurFltr))
          IF !SEEK(RETLINE.crmemo+LEFT(RetLine.Style,lnLenRo)+RetLine.cret_linno,lcRetTemp)
              INSERT INTO (lcRetTemp) (crmemo,Style,cret_linno);
              				 VALUES (RETLINE.crmemo,LEFT(RetLine.Style,lnLenRo),RetLine.cret_linno)
  	          LnTotRet = LnTotRet + RETLINE.TotQty
              LnNetRet = LnNetRet + RETLINE.Amount
              lcCrMemo = CrMemo
              lcStyle  = LEFT(RetLine.Style,lnLenRo)

              lnCpyNtRt = lnCpyNtRt + RETLINE.Amount
              PRIVATE lcCurAlsR
              lcCurAlsR = SELECT(0)
              SELECT RETHDR
              IF LnNetRet > 0
                LnNetRet = IIF(llCallGfam,gfAmntDisp(LnNetRet , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnNetRet)
              ENDIF
              SELECT(lcCurAlsR)
           ENDIF
        ENDSCAN
        SELECT INVLINE
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
        SELECT (lcRoyTemp)
        IF !SEEK(lcCrMemo+lcStyle)
          APPEND BLANK
        ENDIF
        REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty                               ,;
                &lcRoyTemp..Style      WITH LEFt(STYLE.Style,lnLenRo)                 ,;
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
      ELSE  &&IF lcStylAcc # LEFT(STYLE.Style,lnLenRo)
        SELECT RETLINE
        *SET ORDER TO TAG Retlines
        STORE 0 TO LnShip , LnTotAmt , LnDisc
         =SEEK('')
        IF lcStylRet # LEFT(STYLE.Style,lnLenRo)
          lcStylRet = LEFT(STYLE.Style,lnLenRo)

*!*	          	        SCAN WHILE LEFT(Style,lnLenRo) = LEFT(STYLE.Style,lnLenRo) FOR !EMPTY(Invoice) AND EVAL(LcFilter2) AND INVHDR.STATUS <> "V" ;

	        SCAN REST WHILE STYLE + CRMEMO = LEFT(STYLE.Style,lnLenRo) FOR !EMPTY(Invoice) AND EVAL(LcFilter2) AND INVHDR.STATUS <> "V" ;
	          AND (llAllCurR OR SEEK(RETHDR.CCURRCODE,lcCurFltr))
	          IF !SEEK(RETLINE.crmemo+LEFT(RetLine.Style,lnLenRo)+RetLine.cret_linno,lcRetTemp)
	            INSERT INTO (lcRetTemp) (crmemo,Style,cret_linno);
	                				 VALUES (RETLINE.crmemo,LEFT(RetLine.Style,lnLenRo),RetLine.cret_linno)

	            SELECT (lcRoyTemp)
	            IF !SEEK(RETLINE.CRMEMO + LEFT(STYLE.STYLE,lnLenRo))
	              APPEND BLANK
	              REPLACE Royalty    WITH STYLE.Royalty               ,;
	                      Style      WITH LEFT(STYLE.Style,lnLenRo) ,;
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
	          ENDIF
	        ENDSCAN
        ENDIF
      ENDIF
      SELECT INVLINE
    ENDSCAN

    SELECT RETLINE
    *SET ORDER TO TAG Retlines
    *-- Get returns without invoice#
    STORE 0 TO LnShip , LnTotAmt , LnDisc , LnTotRet , LnNetRet
   =SEEK(LEFT(STYLE.Style,lnLenRo))

    SCAN rest  WHILE STYLE+CRMEMO = LEFT(STYLE.Style,lnLenRo) FOR EMPTY(Invoice) AND EVAL(LcFilter2);
       AND SEEK(RETLINE.CRMEMO,'RETHDR') AND (llAllCurR OR SEEK(RETHDR.CCURRCODE,lcCurFltr))

      LnTotRet = RETLINE.TotQty
      LnNetRet = RETLINE.Amount

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

      SELECT (lcRoyTemp)
      APPEND BLANK
      REPLACE &lcRoyTemp..Royalty    WITH STYLE.Royalty               ,;
              &lcRoyTemp..Style      WITH LEFT(STYLE.Style,lnLenRo) ,;
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

SELECT MIN(Royalty)    AS Royalty    ,;
       Style                         ,;
       MIN(Desc)       AS DESC       ,;
       SUM(Totamt)     AS Totamt     ,;
       MIN(Avg_price)  AS Avg_price  ,;
       SUM(Ship_Qty)   AS Ship_Qty   ,;
       SUM(Gross_Sale) AS Gross_Sale ,;
       SUM(Discount)   AS Discount   ,;
       SUM(Ret_Qty)    AS Ret_Qty    ,;
       SUM(Ret_Amt)    AS Ret_Amt    ,;
       SUM(Adj_Qty)    AS Adj_Qty    ,;
       MIN(NET_AMT)    AS NET_AMT    ,;
       MIN(Roy_Amt)    AS Roy_Amt    ,;
       MIN(Account)    AS Account    ,;
       MIN(TaxRoylty)  AS TaxRoylty  ,;
       Invoice                       ,;
       Type                           ;
FROM (lcRoyTemp)                      ;
INTO TABLE (oAriaApplication.WorkDir + lcRoyTemp1)   ;
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
SCAN REST WHILE STYLE+CRMEMO = LEFT(STYLE.Style,lnLenRo) FOR EVAL(LcFilter2)
  IF SEEK(RETLINE.CRMEMO,'RETHDR') AND RETHDR.STATUS <> "V" AND (llAllCurR OR SEEK(RETHDR.CCURRCODE,lcCurFltr))
    LnTotRet = LnTotRet + TotQty
    LnNetRet = LnNetRet + Amount

    lnCpyNtRt = lnCpyNtRt + Amount
    PRIVATE lcCurAlsR
    lcCurAlsR = SELECT(0)
    SELECT RETHDR
    IF LnNetRet > 0
      LnNetRet = IIF(llCallGfam,gfAmntDisp(LnNetRet , lcRpCurr , ldRpExDate , lcRpTmpNam) , LnNetRet)
    ENDIF

    SELECT(lcCurAlsR)

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

*B609356,1 SMA 09/20/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*CREATE TABLE (lcRoyTemp)  (Royalty C(6)       , Style C(lnLenRo), Desc C(20)   , Totamt N(10,2)  , Avg_price N(8,2) , Ship_qty N(6) ,;
                           Gross_sale N(10,2) , Discount N(9,3) , Ret_qty N(6) , Ret_amt N(8,2)  , Adj_qty N(6,2)   , Net_amt N(8,2),;
                           Roy_amt N(8,2)     , Invoice C(6)    , Account C(5) , Type C(1)       , TaxRoylty N(5,2))
CREATE TABLE (oAriaApplication.WorkDir +lcRoyTemp)  (Royalty C(6)       , Style C(lnLenRo), Desc C(20)   , Totamt N(10,2)  , Avg_price N(8,2) , Ship_qty N(6) ,;
                           Gross_sale N(10,2) , Discount N(9,3) , Ret_qty N(6) , Ret_amt N(8,2)  , Adj_qty N(6,2)   , Net_amt N(8,2),;
                           Roy_amt N(8,2)     , Invoice C(6)    , Account C(5) , Type C(1)       , TaxRoylty N(5,2))
*B609356,1 SMA 09/20/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ZAP
INDEX ON INVOICE + LEFT(Style,lnLenRo) TAG lcRoyTemp OF (oAriaApplication.WorkDir + lcRoyTemp)
*B609356,1 SMA 09/20/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*CREATE TABLE (lcRetTemp)  (crmemo C(6)       , Style C(lnLenRo),cret_linno C(4))
CREATE TABLE (oAriaApplication.WorkDir +lcRetTemp)  (crmemo C(6)       , Style C(lnLenRo),cret_linno C(4))
*B609356,1 SMA 09/20/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ZAP
INDEX ON crmemo + LEFT(Style,lnLenRo)+cret_linno TAG lcRetTemp OF (oAriaApplication.WorkDir+ lcRetTemp)






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

*!******************************************************************************************************
*! Name      : lfItmPos
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate fixed filter position within array.
*!******************************************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : ...
*!******************************************************************************************************
*! Returns            : Position
*!******************************************************************************************************
*! Example   : = lfItmPos()
*!******************************************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt

PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter



LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString



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
*--End of lfvCurDisp.
