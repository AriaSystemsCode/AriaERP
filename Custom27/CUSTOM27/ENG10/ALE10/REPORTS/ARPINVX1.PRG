*:***************************************************************************
*: Program file  : ARPINVEN.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR ENGLAND.
*: Date          : 05/14/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200201
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVEN
*:***************************************************************************
*: Modifications :
*: B604593,1 BWA 06/25/2001 Fix the bug in the vat table.
*: C200310,1 SSE 04/02/2002 Custom modifications on Invoice form X1.
*: B605839,1 BWA 01/12/2003 Fix the bug of printing the message "There is no records" twice.
***************************************************************************

*B604593,1 BWA 06/25/2001 Fix the bug in the vat table.[START]
lcTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'
*B604593,1 BWA 06/25/2001 [END]

*-- Case the user select sort by style.
SELECT InvHdr
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN

  *B605839,1 BWA 01/12/2003 Fix the bug of printing the message "There is no records" twice.[START]
  llarpinv = .F.
  *B605839,1 BWA 01/12/2003.[END]

  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

*C200310,1 Open OrdLine file. [Begin]
PRIVATE lnConsLine
lnConsLine = 0
IF !USED(lcTmpOrdLn)
  =gfOpenFile(gcDataDir+"OrdLine","OrdLine",'SH', @lcTmpOrdLn)
ENDIF
*C200310,1 Open OrdLine file. [End]

SELECT INVLINE
=AFIELDS(laFileStru)

*C200310,1 Add Item_No to Temp InvLine file. [Begin]
DIMENSION laFileStru[ALEN(laFileStru,1)+1,4]
laFileStru[ALEN(laFileStru,1),1] = 'Item_No'
laFileStru[ALEN(laFileStru,1),2] = 'C'
laFileStru[ALEN(laFileStru,1),3] = 19
laFileStru[ALEN(laFileStru,1),4] = 0
*C200310,1 Add Item_No to Temp InvLine file. [End]

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

  *C200310,1 Remove !EMPTY(INVHDR.ORDER). [Begin]
  *IF INVHDR.CONSOL = 'Y' AND !EMPTY(INVHDR.ORDER)
  IF INVHDR.CONSOL = 'Y'
    lnConsLine = 0
  *C200310,1 Remove !EMPTY(INVHDR.ORDER). [End]

    *--Case consolidated invoice.
    SELECT CONSINVL
    SCAN REST WHILE Invoice+Store+Order+Style+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT

      SCATTER MEMVAR MEMO
      
      *C200310,1 Update Item_No field. [Begin]
      m.Item_No = IIF(SEEK("O"+m.Order+STR(m.LineNo,6),(lcTmpOrdLn)),EVAL(lcTmpOrdLn+'.Item_No'),'')
      lnConsLine = lnConsLine + 1
      m.LineNo = lnConsLine      
      *C200310,1 Update Item_No field. [End]
      
      lnRecNoSt = 0
      lnRecNoSt = RECNO('STYLE')
      =SEEK(CONSINVL.STYLE,'STYLE')
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
        FOR lnLop = 1 TO SCALE.CNT
          lcTax = "CONSINVL.QTY" + ALLTRIM(STR(lnLop))

          *B604593,1 BWA 06/25/2001 Fix the bug in the vat table.[START]
          *IF BETWEEN(lnLop,lnTAxBreak,8)
          llExemted = lfCustExmt()
          IF BETWEEN(lnLop,lnTAxBreak,8) AND llExemted AND lnTaxRat > 0
          *B604593,1 BWA 06/25/2001 [END]

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
        FOR lnLop = 1 TO SCALE.CNT
          lcTax = "CONSINVL.QTY" + ALLTRIM(STR(lnLop))

          *B604593,1 BWA 06/25/2001 Fix the bug in the vat table.[START]
          *IF BETWEEN(lnLop,lnTAxBreak,8)
          llExemted = lfCustExmt()
          IF BETWEEN(lnLop,lnTAxBreak,8) AND llExemted AND lnTaxRat > 0
          *B604593,1 BWA 06/25/2001 [END]

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

      *C200310,1 Update Item_No field. [Begin]
      m.Item_No = IIF(SEEK("O"+m.Order+STR(m.LineNo,6),(lcTmpOrdLn)),EVAL(lcTmpOrdLn+'.Item_No'),'')
      *C200310,1 Update Item_No field. [End]

      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')

      SELECT (lcTaxable)
      lnTaxable = 0
      IF SEEK(lcInvcee)
        FOR lnLop = 1 TO SCALE.CNT
          lcTax = "INVLINE.QTY" + ALLTRIM(STR(lnLop))

          *B604593,1 BWA 06/25/2001 Fix the bug in the vat table.[START]
          *IF BETWEEN(lnLop,STYLE.NTAXBREAK,8)
          IF BETWEEN(lnLop,STYLE.NTAXBREAK,8) AND IIF(CUSTOMER.TYPE = "M" , !CUSTOMER.lvatexem , lfCustExmt()) AND lnTaxRat > 0
          *B604593,1 BWA 06/25/2001 [END]

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

          *B604593,1 BWA 06/25/2001 Fix the bug in the vat table.[START]
          *IF BETWEEN(lnLop,STYLE.NTAXBREAK,8)
          IF BETWEEN(lnLop,STYLE.NTAXBREAK,8) AND IIF(CUSTOMER.TYPE = "M" , !CUSTOMER.lvatexem , lfCustExmt()) AND lnTaxRat > 0
          *B604593,1 BWA 06/25/2001 [END]

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

*--Clos the invline and open the temp. file with the invline name.
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

                     *-- End of the Program --*
*!*************************************************************
*! Name      : lfCustExmt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/25/2001
*! Purpose   : Function to check if the customer is exempted oe not.
*!*************************************************************
*! Called from : ARPINVX1.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCustExmt()
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