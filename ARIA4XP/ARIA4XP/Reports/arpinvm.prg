*:***************************************************************************
*: Program file  : ARPINVM.PRG
*: Program desc. : STANDERD INVOICE FOR ENGLAND FORM "M".
*: Date          : 03/11/2004
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: E124928
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVM
*:***************************************************************************
*: Modifications :
*: C123950,1 NNA 01/05/2005 Fix bug that Goods field(Taxable Amount) prints the Goods
*: C123950,1 NNA            value only for the first style even though all styles on 
*: C123950,1 NNA            the invoice are taxable only if this invoice is consolidated.
*: C123950,1 NNA            (I fixed this bug and documented it with my custom because it appeared
*: C123950,1 NNA            in testing my Custom) 
*: B129076,1 WAB 08/03/2005 Fix the bug of VAt amount is not calculating properly in the Vat summary     
*! N130335,1 TNA 03/29/2006  Add the CompPhone Variale.
*! N130335,1 TNA 03/29/2006  Fix Problem of selecting Print Status "Not Printed"
*! B607820,1 MMT 11/05/2006 Fix problem of not printing style Desc. in case of Cosnslated Inovices
*! C200914,1 MMT 01/22/2008 Add Amend standard invoice form M as per issue 131675 [T20061201.0002]
*! B608727,1 WAM 10/23/2008 Fix Discount calculation [T20081020.0001]
*! B609622,1 MMT 06/19/2011 Wrong vat summary for non merchandise charges  Invoice form M[T20110610.0001]
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004]
*!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024]
*! B610550,1 SAB 10/21/2013 Modify Invoice Form M to Run from Request Builder [T20130717.0010]
*! B611126,1 MMT 03/16/2016 Custom Invoice form M takes long time to collect data[T20160226.0010]
***************************************************************************
STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl

*N130335,1 TNA 03/29/2006 (Begin) Add the CompPhone Variale
lcCompPhone = TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)
*N130335,1 TNA 03/29/2006 (End)

*C123950,1 NNA 01/05/2005 (Begin) New variable that hold the scale count
STORE 0 TO lnScale
*C123950,1 NNA (End)

*--THE COLOR LENGTH

DECLARE laItemSeg[1]

* MAH Add to Support Request Builder
*=gfItemMask(@laItemSeg)
IF TYPE('oAriaEnvironment') == 'O'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laItemSeg)
ELSE
  =gfItemMask(@laItemSeg)
ENDIF
* MAH Add to Support Request Builder

FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
* MAH Add to Support Request Builder
IF TYPE('oAriaEnvironment') <> 'O'
  * MAH Add to Support Request Builder
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  * MAH Add to Support Request Builder
ENDIF
* MAH Add to Support Request Builder
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR


lcTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'

*- N130335,1 TNA, 02/01/2005 [Start]
lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
*- N130335,1 TNA, 02/01/2005 [End]

*-- Case the user select sort by style.
SELECT InvHdr
*! B611126,1 MMT 03/16/2016 Custom Invoice form M takes long time to collect data[T20160226.0010][Start]
*!*	LOCATE FOR &lcRpExp
*!*	IF !FOUND()
SELECT INVOICE FROM INVHDR WHERE INVOICE = '' AND &lcRpExp. INTO Cursor "TempINV"
if Reccount("TempINV") = 0
*! B611126,1 MMT 03/16/2016 Custom Invoice form M takes long time to collect data[T20160226.0010][End]
  *---Text : 'No Record Selected for the report..!'
  * MAH Add to Support Request Builder
  IF TYPE('oAriaEnvironment') <> 'O'
  * MAH Add to Support Request Builder
    =gfModalGen('TRM00052B00000','DIALOG') 
  * MAH Add to Support Request Builder
  ENDIF
  * MAH Add to Support Request Builder
  llNoRec = .T.
  SET DEVICE TO SCREEN
  llarpinv = .F.
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

PRIVATE lnConsLine
lnConsLine = 0
lcTmpOrdLn = GFTEMPNAME()
lcTaxable  = GFTEMPNAME()
lcStylIndx = GFTEMPNAME()
INVLINE    = GFTEMPNAME()

IF !USED(lcTmpOrdLn)
  =gfOpenFile(gcDataDir+"OrdLine","OrdLine",'SH', @lcTmpOrdLn)
ENDIF

*B129076,1 -- WAB (Start)  
lnChrgTax = 0					&&--variable to hold the taxable Charge amount
lnChgNonTx = 0					&&--Variable to hold the non Taxable charge amount
lcTmpInvCh = GFTEMPNAME()		&&--Variable to hold tempfile name to open the invoice charge table 
*--open the inv Charges 
IF !USED(lcTmpInvCh)
  =gfOpenFile(gcDataDir+"invchrg","invchrg",'SH', @lcTmpInvCh)
ENDIF
lcInvChgNo = '' 
*B129076,1 -- WAB (End)  

SELECT INVLINE
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][Start]
IF USED(lcStylIndx)
  USE IN (lcStylIndx)
ENDIF
USE (gcWorkDir+lcStylIndx) IN 0 EXCLUSIVE
SELECT (lcStylIndx)
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][End]
*-- The invline is right due to use this name when replace the invline file place the temp. file(gcWorkDir + INVLINE + '.CDX')
INDEX ON INVOICE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + INVLINE + '.CDX')
CREATE TABLE (gcWorkDir+lcTaxable) (INVOICE C(6) , Taxable N(12,2) , NonTax N(12,2) , Price N(9,2))
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][Start]
IF USED(lcTaxable)
  USE IN (lcTaxable)
ENDIF
USE (gcWorkDir+lcTaxable) IN 0 EXCLUSIVE
SELECT(lcTaxable)
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][End]
INDEX ON INVOICE TAG TaxPlat OF (gcWorkDir + lcTaxable + '.CDX') 

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO CONSINVL ADDITIVE
*! B611126,1 MMT 03/16/2016 Custom Invoice form M takes long time to collect data[T20160226.0010][Start]
*SCAN FOR &lcASExp
SELECT "TempINV"
LOCATE  
SCAN
  =SEEK(TempINV.INVOICE ,"INVHDR",'INVHDR')
*! B611126,1 MMT 03/16/2016 Custom Invoice form M takes long time to collect data[T20160226.0010][End]
  *-lcInvcee    >> Variable hold the invoice No.
  lcInvcee = INVHDR.INVOICE

  IF INVHDR.CONSOL = 'Y'
    lnConsLine = 0

    *--Case consolidated invoice.
    SELECT CONSINVL
    SCAN REST WHILE Invoice+Store+Order+Style+STR(lineno,6) = lcInvcee
      *! B610550,1 SAB 10/21/2013 Modify Invoice Form M to Run from Request Builder [T20130717.0010][Start]
      *WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      IF TYPE('oAriaEnvironment') <> 'O'
        WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      ENDIF
      *! B610550,1 SAB 10/21/2013 Modify Invoice Form M to Run from Request Builder [T20130717.0010][End]

      SCATTER MEMVAR MEMO
      lnConsLine = lnConsLine + 1
      m.LineNo = lnConsLine      

      lnRecNoSt = 0
      lnRecNoSt = RECNO('STYLE')
      =SEEK(CONSINVL.STYLE,'STYLE')

      *C123950,1 NNA 01/05/2005 (Begin) Seek for the Style's Scale and hold its count in a variable
      =SEEK('S'+STYLE.SCALE,'SCALE')
      lnScale = SCALE.CNT
      *C123950,1 NNA (End)
    
          
      *B607820,1 MMT 11/05/2006 Fix problem of not printing style Desc. in case of Cosnslated Inovices[Start]
      =SEEK(CONSINVL.STYLE+lcInvcee,'INVLINE','INVLINES')
       m.Desc1 = INVLINE.Desc1
      *B607820,1 MMT 11/05/2006 Fix problem of not printing style Desc. in case of Cosnslated Inovices[End]
  
      lnTaxRat = 0
      = gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
      lnTAxBreak = STYLE.NTAXBREAK
      IF BETWEEN(lnRecNoSt,1,RECCOUNT('STYLE'))
        GOTO lnRecNoSt IN STYLE
      ENDIF  
      
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT (lcTaxable)
      IF SEEK(lcInvcee)

        *C123950,1 NNA 01/05/2005 (Begin) Loop for scale count that hold in the variable
        *FOR lnLop = 1 TO SCALE.CNT
        FOR lnLop = 1 TO lnScale
        *C123950,1 NNA (End)
        
          lcTax = "CONSINVL.QTY" + ALLTRIM(STR(lnLop))

          llExemted = lfCustExmt()
          IF BETWEEN(lnLop,lnTAxBreak,8) AND llExemted AND lnTaxRat > 0
            IF EVAL(lcTax) > 0
              REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * CONSINVL.PRICE) ,;
                      &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * CONSINVL.PRICE)
            ENDIF
          ELSE
            REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + ( EVAL(lcTax) * CONSINVL.PRICE)
          ENDIF
        ENDFOR
      ELSE
        APPEND BLANK
        REPLACE &lcTaxable..INVOICE WITH M.INVOICE

        *C123950,1 NNA 01/05/2005 (Begin) Loop for scale count that hold in the variable
        *FOR lnLop = 1 TO SCALE.CNT
        FOR lnLop = 1 TO lnScale
        *C123950,1 NNA (End)
        
          lcTax = "CONSINVL.QTY" + ALLTRIM(STR(lnLop))

          llExemted = lfCustExmt()
          IF BETWEEN(lnLop,lnTAxBreak,8) AND llExemted AND lnTaxRat > 0
            IF EVAL(lcTax) > 0
              REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * CONSINVL.PRICE) ,;
                      &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * CONSINVL.PRICE)
            ENDIF
          ELSE
            REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + (EVAL(lcTax) * CONSINVL.PRICE)
          ENDIF
        ENDFOR
      ENDIF
    ENDSCAN

  ELSE

    SELECT INVLINE
    SCAN REST WHILE Invoice+STR(lineno,6) = lcInvcee
      *! B610550,1 SAB 10/21/2013 Modify Invoice Form M to Run from Request Builder [T20130717.0010][Start]
      *WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      IF TYPE('oAriaEnvironment') <> 'O'
        WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      ENDIF
      *! B610550,1 SAB 10/21/2013 Modify Invoice Form M to Run from Request Builder [T20130717.0010][End]
      SCATTER MEMVAR MEMO

      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')

      SELECT (lcTaxable)
      lnTaxable = 0
      IF SEEK(lcInvcee)
        FOR lnLop = 1 TO SCALE.CNT
          lcTax = "INVLINE.QTY" + ALLTRIM(STR(lnLop))

          IF BETWEEN(lnLop,STYLE.NTAXBREAK,8) AND IIF(CUSTOMER.TYPE = "M" , !CUSTOMER.lvatexem , lfCustExmt()) AND lnTaxRat > 0
            IF EVAL(lcTax) > 0
              REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * INVLINE.PRICE) ,;
                      &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * INVLINE.GROS_PRICE)
            ENDIF
          ELSE
            REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + (EVAL(lcTax) * INVLINE.PRICE)
          ENDIF
        ENDFOR
      ELSE 
        APPEND BLANK
        REPLACE &lcTaxable..INVOICE WITH M.INVOICE
        FOR lnLop = 1 TO SCALE.CNT
          lcTax = "INVLINE.QTY" + ALLTRIM(STR(lnLop))

          IF BETWEEN(lnLop,STYLE.NTAXBREAK,8) AND IIF(CUSTOMER.TYPE = "M" , !CUSTOMER.lvatexem , lfCustExmt()) AND lnTaxRat > 0
            IF EVAL(lcTax) > 0
              REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * INVLINE.PRICE) ,;
                      &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * INVLINE.GROS_PRICE)
            ENDIF
          ELSE
            REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + (EVAL(lcTax) * INVLINE.PRICE)
          ENDIF
        ENDFOR
      ENDIF
    ENDSCAN
  ENDIF
  *B129076,1 -- WAB (Start)  
  *--Add the charges amount .. Taxable and Non Taxable
  lnChrgTax = 0
  lnChgNonTx = 0
  *--Check if we still on the same invoice to prevent duplication of amount
  IF lcInvChgNo <> lcInvcee 
    lcInvChgNo = lcInvcee
    SELECT (lcTmpInvCh)
    IF SEEK(lcInvcee)
      *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
      *SCAN REST WHILE invoice+cstore+cchrgcode = lcInvcee
      SCAN REST WHILE invoice+order+cstore+piktkt+cchrgcode = lcInvcee
      *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]
        *--if this charge is taxable, add the amount to the taxable charge amount 
        IF  nTaxRate > 0 
          lnChrgTax   = lnChrgTax  + nChrgAmnt
        ELSE
          lnChgNonTx  = lnChgNonTx + nChrgAmnt
        ENDIF
      ENDSCAN
      SELECT (lcTaxable)
      IF SEEK(lcInvcee)
         *! B609622,1 MMT 06/19/2011 Wrong vat summary for non merchandise charges  Invoice form M[Start]
         *REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + lnChrgTax ,;
                 &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax   ,;
                 &lcTaxable..NonTax  WITH &lcTaxable..NonTax + lnChgNonTx
         REPLACE &lcTaxable..Taxable WITH ROUND(&lcTaxable..Taxable-(&lcTaxable..Taxable *INVHDR.Discpcnt)/100,2)+ lnChrgTax ,;
                 &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax   ,;
                 &lcTaxable..NonTax  WITH ROUND(&lcTaxable..NonTax  -(&lcTaxable..NonTax  *INVHDR.Discpcnt)/100,2) + lnChgNonTx
         *! B609622,1 MMT 06/19/2011 Wrong vat summary for non merchandise charges  Invoice form M[END]
      ELSE
        APPEND BLANK
        *! B609622,1 MMT 06/19/2011 Wrong vat summary for non merchandise charges  Invoice form M[Start]        
*!*	        REPLACE &lcTaxable..INVOICE WITH lcInvcee ,;
*!*	                &lcTaxable..Taxable WITH &lcTaxable..Taxable + lnChrgTax ,;
*!*	                &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax   ,;
*!*	                &lcTaxable..NonTax  WITH &lcTaxable..NonTax + lnChgNonTx
        REPLACE &lcTaxable..INVOICE WITH lcInvcee ,;
                &lcTaxable..Taxable WITH ROUND(&lcTaxable..Taxable-(&lcTaxable..Taxable *INVHDR.Discpcnt)/100,2)+ lnChrgTax ,;
                &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax   ,;
                &lcTaxable..NonTax  WITH ROUND(&lcTaxable..NonTax  -(&lcTaxable..NonTax  *INVHDR.Discpcnt)/100,2)+ lnChgNonTx
        *! B609622,1 MMT 06/19/2011 Wrong vat summary for non merchandise charges  Invoice form M[End]
      ENDIF
    ENDIF
  ENDIF
  *B129076,1 -- WAB (END)  
ENDSCAN

             *-- Section break the relatoins --*             
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO ORDHDR
SET RELATION OFF INTO CONSINVL
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
             *-- End Section break the relatoins --*

*--Close the invline and open the temp. file with the invline name.
SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
USE IN (lcStylIndx)
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][Start]
*USE (gcWorkDir+lcStylIndx) IN 0 ALIAS INVLINE
USE (gcWorkDir+lcStylIndx) IN 0 ALIAS INVLINE EXCLUSIVE
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][End]
SELECT INVLINE
INDEX ON INVOICE + STYLE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + lcStylIndx + '.CDX')
             *-- Section Create the new relatoins --*
SELECT INVHDR
IF llPrntInst .OR. llRpInvNot 
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE
ELSE
  SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
ENDIF

SELECT INVLINE
LOCATE
SET RELATION TO IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE , INVLINE.Style) INTO STYLE ADDITIVE
SET RELATION TO "S" + INVLINE.Account + INVLINE.Style INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                  'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO OrdHdr ADDITIVE
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE
SET RELATION TO INVHDR.INVOICE INTO (lcTaxable) ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , INVLINE
ELSE
  SET SKIP TO INVLINE
ENDIF
*B608727,1 WAM 10/23/2008 Store the term code in the report layout
lcTermCode = ''
lnTotalChg = 0
lnTax_Amt = 0
ldInvDate = {}
*B608727,1 WAM 10/23/2008 (End)

SELECT INVHDR
*! B610550,1 SAB 10/21/2013 Modify Invoice Form M to Run from Request Builder [T20130717.0010][Start]
*DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
IF TYPE('oAriaEnvironment') <> 'O'
  DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
ELSE
  SELECT INVHDR
  Set FILTER TO &lcRpExp
  LOCATE 
  IF EOF()
      SET FILTER TO 
    RETURN 
  ENDIF  
  loProgress.Percent = 0.9
  loProgress.Description = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  
  IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
    loOGScroll   = oAriaEnvironment.report
    oAriaEnvironment.report.OGLastForm = lcFormName
    oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
    loProgress.Percent = 1.0
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
  ENDIF
  SELECT INVHDR
  SET FILTER TO 
ENDIF 
*! B610550,1 SAB 10/21/2013 Modify Invoice Form M to Run from Request Builder [T20130717.0010][End]
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

* MAH Add to Support Request Builder
IF TYPE('oAriaEnvironment') <> 'O'
* MAH Add to Support Request Builder
 SELECT INVLINE
  CLOSE INDEX
  USE IN INVLINE
  =gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')
  
  =lfBasToClr(lcTmpOrdLn , 'F')
  =lfBasToClr(lcStylIndx , 'F')
  =lfBasToClr(lcTaxable , 'F')
  =lfBasToClr(INVLINE , 'F')
 
  *WAB
  =lfBasToClr(lcTmpInvCh , 'F')
  *WAB
* MAH Add to Support Request Builder  
ENDIF
* MAH Add to Support Request Builder

                     *-- End of the Program --*
*!*************************************************************
*! Name      : lfCustExmt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2004
*! Purpose   : Function to check if the customer is exempted oe not.
*!*************************************************************
*! Called from : ARPINVM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCustExmt()
*!*************************************************************
FUNCTION lfCustExmt
PRIVATE llReturn , lcAliasX1

llReturn = .F.
lcAliasX1 = SELECT(0)
SELECT CUSTOMER
lcKeyx1 = EVAL(KEY())

IF SEEK(IIF(M.STORE = SPACE(8),'M'+M.ACCOUNT,'S'+M.ACCOUNT+M.STORE))
  llReturn = !CUSTOMER.lvatexem
ENDIF

SELECT CUSTOMER
=SEEK(lcKeyx1)

SELECT(lcAliasX1)

RETURN llReturn

*--End of lfCustExmt.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/11/2004
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.

*! C200914,1 MMT 01/22/2008 Add Develop - UK - Amend standard invoice form M as per issue 131675 [Start]
*:**************************************************************************
*:* Name        : lfGetDisc
*:* Developer   : MMT- Mariam Mazhar
*:* Date        : 01/22/2008
*:* Purpose     : Get discount day and disc. amount based on net discount days and pecenct
*:***************************************************************************
*:* Called from : ARPINVGS.FRX
*:***************************************************************************
*C200914

FUNCTION lfGetDisc
PRIVATE laTRltFld,lcPyTerm,lcRet,;
        lnTerDiscD,lnTerDiscR,lnTerDscD1,lnTerDscR1,;
        lnSlct,lnI,lnK
        
lnSlct = SELECT()

STORE 0 TO lnTerDiscD,lnTerDiscR,lnTerDscD1,lnTerDscR1
STORE '' TO lcDiscnt1,lcDiscnt2,;
            lcNoteT1,lcNoteT2,lcNoteT3,lcNoteT4

IF !llEndGroup
  RETURN ''
ENDIF

PRIVATE laString,laVars
DIME laString[2],laVars[2]
STORE '' TO laString,laVars
laVars[1] = "lcDiscnt1"
laVars[2] = "lcDiscnt2"

IF llRpPrSDC
  DIMENSION laTRltFld[4,2]
  *-- Payment Terms Related Fields
  laTRltFld[1,1] = 'NTERDISCD'
  laTRltFld[1,2] = 'lnTerDiscD'
  laTRltFld[2,1] = 'NTERDISCR'
  laTRltFld[2,2] = 'lnTerDiscR'
  laTRltFld[3,1] = 'NTERDISCD1'
  laTRltFld[3,2] = 'lnTerDscD1'
  laTRltFld[4,1] = 'NTERDISCR1'
  laTRltFld[4,2] = 'lnTerDscR1'
  
  *=gfRltFld(INVHDR.cTermCode,@laTRltFld,'CTERMCODE')
  lcAlias = ALIAS()
  SELECT Codes
  SET ORDER TO TAG Codes

  *B608727,1 WAM 10/23/2008 Store the term code in the report layout
  *IF SEEK('N'+INVHDR.cTermCode+'Y'+'CTERMCODE')
    *SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+INVHDR.cTermCode+'Y'+'CTERMCODE'
  IF SEEK('N'+lcTermCode+'Y'+'CTERMCODE')
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+lcTermCode+'Y'+'CTERMCODE'
  *B608727,1 WAM 10/23/2008 (End)

      *B608727,1 WAM 10/23/2008 Get the proper discount1
      *IF crltd_nam = 'NTERDISCD'
      IF crltd_nam = PADR('NTERDISCD',10)
      *B608727,1 WAM 10/23/2008 (End)

        lnTerDiscD = VAL(crltd_vlu)
      ENDIF
      
      *B608727,1 WAM 10/23/2008 Get the proper discount1
      *IF crltd_nam = 'NTERDISCR'
      IF crltd_nam = PADR('NTERDISCR',10)
      *B608727,1 WAM 10/23/2008 (End)

        lnTerDiscR = VAL(crltd_vlu)
      ENDIF

      IF crltd_nam = 'NTERDISCD1'
        lnTerDscD1 = VAL(crltd_vlu)
      ENDIF

      IF crltd_nam = 'NTERDISCR1'
        lnTerDscR1 = VAL(crltd_vlu)
      ENDIF
      
    ENDSCAN
  ENDIF
  SELECT (lcAlias)
  
  IF TYPE('lnTerDiscD') = 'C'
    lnTerDiscD = 0
  ENDIF 

  
  IF lnTerDiscD <> 0 .AND. lnTerDiscR <> 0
    *B608727,1 WAM 10/23/2008 Store the Total charge, tax amout and invoice date in the report layout
    *laString[1] = "If paid by " + DTOC(INVHDR.INVDATE+lnTerDiscD) + " you may deduct " +  ALLT(STR( (INVHDR.TOTALCHG-INVHDR.TAX_AMT)*lnTerDiscR/100 , 10,2))
    laString[1] = "If paid by " + DTOC(ldInvDate+lnTerDiscD) + " you may deduct " +  ALLT(STR( (lnTotalChg-lnTax_Amt)*lnTerDiscR/100 , 10,2))
    *B608727,1 WAM 10/23/2008 (End)
  ENDIF

  IF TYPE('lnTerDscD1') = 'C'
    lnTerDscD1 = 0
  ENDIF 

  
  IF lnTerDscD1 <> 0 .AND. lnTerDscR1 <> 0
    *B608727,1 WAM 10/23/2008 Store the Total charge, tax amout and invoice date in the report layout
    *laString[2] = "If paid by " + DTOC(INVHDR.INVDATE+lnTerDscD1) + " you may deduct " +  ALLT(STR( (INVHDR.TOTALCHG-INVHDR.TAX_AMT)*lnTerDscR1/100 , 10,2))
    laString[2] = "If paid by " + DTOC(ldInvDate+lnTerDscD1) + " you may deduct " +  ALLT(STR( (lnTotalChg-lnTax_Amt)*lnTerDscR1/100 , 10,2))
    *B608727,1 WAM 10/23/2008 (End)
  ENDIF
 
  =lfRmvGap()
  
ENDIF

SELECT NOTEPAD
IF SEEK("TBANK","NOTEPAD")

  DIME laString[4],laVars[4]
  STORE '' TO laString,laVars
  laVars[1] = "lcNoteT1"
  laVars[2] = "lcNoteT2"
  laVars[3] = "lcNoteT3"
  laVars[4] = "lcNoteT4"

  laString[1] = ALLTRIM(MLINE(MNOTES,1))
  laString[2] = ALLTRIM(MLINE(MNOTES,2))
  laString[3] = ALLTRIM(MLINE(MNOTES,3))
  laString[4] = ALLTRIM(MLINE(MNOTES,4))

  =lfRmvGap()
ENDIF

SELECT (lnSlct)
RETURN ''
*-- end of lfGetDisc.

*:**************************************************************************
*:* Name        : lfRmvGap
*:* Developer   : MMT- Mariam Mazhar
*:* Date        : 01/22/2008
*:* Purpose     : removing gaps in variable for the purpose of display
*:***************************************************************************
*C200914
FUNCTION lfRmvGap
*- the following loop is to remove any gap between variable to be printed on the report from bottom to top

lnK = ALEN(laVars,1)
FOR lnI = ALEN(laVars,1) TO 1 STEP - 1
  IF !EMPTY(laString[lnI])
    &laVars[lnK] = laString[lnI]
    lnK = lnK - 1
  ENDIF
ENDFOR
*-- end of lfRmvGap.
*! C200914,1 MMT 01/22/2008 Add Develop - UK - Amend standard invoice form M as per issue 131675 [End]