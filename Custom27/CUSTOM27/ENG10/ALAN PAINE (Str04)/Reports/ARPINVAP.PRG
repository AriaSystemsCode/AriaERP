*:***************************************************************************
*: Program file  : ARPINVAP.PRG
*: Program desc. : CUSTOM INVOICE FORM FOR ALAN PAINE (ENGLAND) BASED ON FORM "M".
*: Date          : 08/25/2007
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : NADER NABIL (NNA)
*: Tracking Job Number: CP#200839 - T20070627.0001 
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVAP
*:***************************************************************************
*: Modifications :
***************************************************************************

STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl

STORE 0 TO lnScale

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

lcTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lcEmail    = gfGetMemVar('E_MAIL',gcAct_Comp)
lcwebsite  = gfGetMemVar('WEBSITE',gcAct_Comp)
lcCoRegNo  = gfGetMemVar('COREGNO',gcAct_Comp)
lcSpcInst  = gfCodDes(INVHDR.SPCINST ,'SPCINST')

lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'

*-- Case the user select sort by style.
SELECT InvHdr
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
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
  *force open the ordline file in a temp alias
  =gfOpenFile(gcDataDir+"OrdLine","OrdLine",'SH', @lcTmpOrdLn,.T.)
ENDIF

lnChrgTax = 0					&&--variable to hold the taxable Charge amount
lnChgNonTx = 0					&&--Variable to hold the non Taxable charge amount
lcTmpInvCh = GFTEMPNAME()		&&--Variable to hold tempfile name to open the invoice charge table 
*--open the inv Charges 
IF !USED(lcTmpInvCh)
  =gfOpenFile(gcDataDir+"invchrg","invchrg",'SH', @lcTmpInvCh)
ENDIF
lcInvChgNo = '' 

SELECT INVLINE
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru

*-- The invline is right due to use this name when replace the invline file place the temp. file(gcWorkDir + INVLINE + '.CDX')
INDEX ON INVOICE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + INVLINE + '.CDX')
CREATE TABLE (gcWorkDir+lcTaxable) (INVOICE C(6) , Taxable N(12,2) , NonTax N(12,2) , Price N(9,2))
INDEX ON INVOICE TAG TaxPlat OF (gcWorkDir + lcTaxable + '.CDX') 

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO CONSINVL ADDITIVE

SCAN FOR &lcASExp
  *-lcInvcee    >> Variable hold the invoice No.
  lcInvcee = INVHDR.INVOICE
  IF INVHDR.CONSOL = 'Y'
    lnConsLine = 0

    *--Case consolidated invoice.
    SELECT CONSINVL
    SCAN REST WHILE Invoice+Store+Order+Style+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT

      SCATTER MEMVAR MEMO
      lnConsLine = lnConsLine + 1
      m.LineNo = lnConsLine      

      lnRecNoSt = 0
      lnRecNoSt = RECNO('STYLE')
      =SEEK(CONSINVL.STYLE,'STYLE')

      *--Seek for the Style's Scale and hold its count in a variable
      =SEEK('S'+STYLE.SCALE,'SCALE')
      lnScale = SCALE.CNT
      
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

        *-- Loop for scale count that hold in the variable
        FOR lnLop = 1 TO lnScale
        
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

        *-- Loop for scale count that hold in the variable
        FOR lnLop = 1 TO lnScale
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
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
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
  *--Add the charges amount .. Taxable and Non Taxable
  lnChrgTax = 0
  lnChgNonTx = 0
  *--Check if we still on the same invoice to prevent duplication of amount
  IF lcInvChgNo <> lcInvcee 
    lcInvChgNo = lcInvcee
    SELECT (lcTmpInvCh)
    IF SEEK(lcInvcee)
      SCAN REST WHILE invoice+cstore+cchrgcode = lcInvcee
        *--if this charge is taxable, add the amount to the taxable charge amount 
        IF  nTaxRate > 0 
          lnChrgTax   = lnChrgTax  + nChrgAmnt
        ELSE
          lnChgNonTx  = lnChgNonTx + nChrgAmnt
        ENDIF
      ENDSCAN
      SELECT (lcTaxable)
      IF SEEK(lcInvcee)
         REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + lnChrgTax ,;
                 &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax   ,;
                 &lcTaxable..NonTax  WITH &lcTaxable..NonTax + lnChgNonTx
      ELSE
        APPEND BLANK
        REPLACE &lcTaxable..INVOICE WITH lcInvcee ,;
                &lcTaxable..Taxable WITH &lcTaxable..Taxable + lnChrgTax ,;
                &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax   ,;
                &lcTaxable..NonTax  WITH &lcTaxable..NonTax + lnChgNonTx
      ENDIF
    ENDIF
  ENDIF
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
USE (gcWorkDir+lcStylIndx) IN 0 ALIAS INVLINE
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

SELECT INVHDR
DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')

=lfBasToClr(lcTmpOrdLn , 'F')
=lfBasToClr(lcStylIndx , 'F')
=lfBasToClr(lcTaxable , 'F')
=lfBasToClr(INVLINE , 'F')
=lfBasToClr(lcTmpInvCh , 'F')
                     *-- End of the Program --*
*!***************************************************************************
*! Name        : lfCustExmt
*! Developer   : Nader Nabil (NNA)
*! Date        : 08/25/2007
*! Purpose     : Function to check if the customer is exempted oe not.
*!***************************************************************************
*! Called from : ARPINVM.PRG
*!***************************************************************************
*! Calls       : 
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : =lfCustExmt()
*!***************************************************************************
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
*!***************************************************************************
*! Name        : lfBasToClr
*! Developer   : Nader Nabil (NNA)
*! Date        : 08/25/2007
*! Purpose     : Deleting temp. files.
*!***************************************************************************
*! Called from : 
*!***************************************************************************
*! Calls       : ....
*!***************************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!***************************************************************************
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

*!***************************************************************************
*! Name        : lfGetDisc
*! Developer   : Nader Nabil (NNA)
*! Date        : 08/25/2007
*! Purpose     : Get discount day and disc. amount based on net discount days and pecenct
*!***************************************************************************
*! Called from : ARPINVAP.FRX
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : = lfGetDisc()
*!***************************************************************************
FUNCTION lfGetDisc
PRIVATE laTRltFld,lcPyTerm,lcRet,;
        lnTerDiscD,lnTerDiscR,lnTerDscD1,lnTerDscR1,;
        lnSlct,lnI,lnK
        
lnSlct = SELECT()
        
STORE 0 TO lnTerDiscD,lnTerDiscR,lnTerDscD1,lnTerDscR1
STORE '' TO lcDiscnt1,lcDiscnt2,;
            lcNoteT1,lcNoteT2,lcNoteT3,lcNoteT4,lcNoteT5,lcNoteT6,lcNoteT7
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
  =gfRltFld(INVHDR.cTermCode,@laTRltFld,'CTERMCODE')
  IF lnTerDiscD <> 0 .AND. lnTerDiscR <> 0
    laString[1] = "If paid by " + DTOC(INVHDR.INVDATE+lnTerDiscD) + " you may deduct " +  ALLT(STR( (INVHDR.TOTALCHG-INVHDR.TAX_AMT)*lnTerDiscR/100 , 10,2))
  ENDIF
  
  IF lnTerDscD1 <> 0 .AND. lnTerDscR1 <> 0
    laString[2] = "If paid by " + DTOC(INVHDR.INVDATE+lnTerDscD1) + " you may deduct " +  ALLT(STR( (INVHDR.TOTALCHG-INVHDR.TAX_AMT)*lnTerDscR1/100 , 10,2))
  ENDIF
  
  =lfRmvGap()
ENDIF
SELECT NOTEPAD
IF SEEK('T'+CUSTOMER.USR_DFND10,"NOTEPAD")
  DIME laString[7],laVars[7]
  STORE '' TO laString,laVars
  laVars[1] = "lcNoteT1"
  laVars[2] = "lcNoteT2"
  laVars[3] = "lcNoteT3"
  laVars[4] = "lcNoteT4"
  laVars[5] = "lcNoteT5"
  laVars[6] = "lcNoteT6"
  laVars[7] = "lcNoteT7"

  laString[1] = ALLTRIM(MLINE(MNOTES,1))
  laString[2] = ALLTRIM(MLINE(MNOTES,2))
  laString[3] = ALLTRIM(MLINE(MNOTES,3))
  laString[4] = ALLTRIM(MLINE(MNOTES,4))
  laString[5] = ALLTRIM(MLINE(MNOTES,5))
  laString[6] = ALLTRIM(MLINE(MNOTES,6))
  laString[7] = ALLTRIM(MLINE(MNOTES,7))

  =lfRmvGap()
ENDIF

SELECT (lnSlct)
RETURN ''
*-- end of lfGetDisc.
*!***************************************************************************
*! Name        : lfRmvGap
*! Developer   : Nader Nabil (NNA)
*! Date        : 08/25/2007
*! Purpose     : removing gaps in variable for the purpose of display
*!***************************************************************************
*! Called from : ARPINVAP.PRG
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : = lfRmvGap()
*!***************************************************************************
FUNCTION lfRmvGap
*- the following loop is to remove any gap between variable to be printed on the report from bottom to top

lnK = ALEN(laVars,1)
FOR lnI = ALEN(laVars,1) TO 1 STEP - 1
  IF !EMPTY(laString[lnI])
    &laVars[lnK] = laString[lnI]
    lnK = lnK - 1
  ENDIF
ENDFOR
*-- End of lfRmvGap.