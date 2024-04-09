*:***************************************************************************
*: Program file  : ARPINVCK.PRG
*: Program desc. : Customized Invoice Form CK for Charles Kirk (ENGLAND)
*: Date          : 02/04/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Tracking Job Number: C200275
*:***************************************************************************
*: Example : DO ARPINVCK
*:***************************************************************************
*B605588,1 SSE 02/24/2002 Fix some bugs regarding the alignment.
*B606539,1 SSE 10/28/2002 Fix bug of displaying the Taxable and Non Taxable amount with taking
*B606539,1                Discount and charges into consideration (Merchandise-Discount+Charges)
*B606539,1                All modifications is in FRX (Prg for documentation only)

PRIVATE lcTEOM , lnEomDay , lnTDaysDue , ldDueDate
lcTEOM = ''
lnTDaysDue = 0
lnEomDay = 20

DECLARE laTRltFld[3,2]

laTRltFld[1,1] = 'EOM'
laTRltFld[1,2] = 'lcTEOM'
laTRltFld[2,1] = 'NTERDUED'
laTRltFld[2,2] = 'lnTDaysDue'
laTRltFld[3,1] = 'EOMDAY'
laTRltFld[3,2] = 'lnEomDay'

*-- Search for company Phone number in laCompAdd
PRIVATE lnPhonePos , lcTaxRefr
lnPhonePos = ASCAN(laCompAdd,TRANSFORM(lcCompPhon , lcPhonPict))
IF lnPhonePos > 0
  
  *B605588,1 Remove the "\" from phone no. [Begin]
  *laCompAdd[lnPhonePos] = 'Phone No \: ' + laCompAdd[lnPhonePos]
  laCompAdd[lnPhonePos] = 'Phone No : ' + laCompAdd[lnPhonePos]
  *B605588,1 Remove the "\" from phone no. [End]

ENDIF

*-- Open BomVar & IcDesign & Debit files with Temp names.
IF !USED(lcTmpBomVr)
  =gfOpenFile(gcDataDir+"BomVar","BomVar",'SH', @lcTmpBomVr)
ENDIF
IF !USED(lcTmpIcDsg)
  =gfOpenFile(gcDataDir+"IcDesign","IcDesign",'SH', @lcTmpIcDsg)
ENDIF
IF !USED(lcTmpDebit)
  =gfOpenFile(gcDataDir+"Debit","Debit",'SH', @lcTmpDebit)
ENDIF

STORE 0 TO lnClrLen , lnClrPos
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

lcTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'

lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

*-- Case the user select sort by style.
SELECT InvHdr
LOCATE FOR &lcRpExp

IF !FOUND()
  *-- Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SELECT INVLINE
=AFIELDS(laFileStru)
DIMENSION laFileStru[ALEN(laFileStru,1)+2,4]
laFileStru[ALEN(laFileStru,1),1] = 'cDsgnName'
laFileStru[ALEN(laFileStru,1),2] = 'C'
laFileStru[ALEN(laFileStru,1),3] = 30
laFileStru[ALEN(laFileStru,1),4] = 0

laFileStru[ALEN(laFileStru,1)-1,1] = 'cDsgnCode'
laFileStru[ALEN(laFileStru,1)-1,2] = 'C'
laFileStru[ALEN(laFileStru,1)-1,3] = 6
laFileStru[ALEN(laFileStru,1)-1,4] = 0

CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru
*-- The invline is right due to use this name when replace the invline file place the temp. file(gcWorkDir + INVLINE + '.CDX')
INDEX ON INVOICE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + INVLINE + '.CDX')

CREATE TABLE (gcWorkDir+lcTaxable) (INVOICE C(6) , Taxable N(12,2) , NonTax N(12,2) , Price N(9,2))
INDEX ON INVOICE TAG TaxPlat OF (gcWorkDir + lcTaxable + '.CDX') 

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO CONSINVL ADDITIVE

PRIVATE lnConsLine , lcDateCent , lcInvLnTag
lcDateCent = SET('CENTURY')
SET CENTURY ON

lnConsLine = 0
SCAN FOR &lcASExp

  =gfRltFld(cTermCode,@laTRltFld,'CTERMCODE')
  *lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' OR lnEOMDay = 0,20,lnEOMDay-1)   && standard
  lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' OR lnEOMDay = 0,20,lnEOMDay)
  ldDueDate = IIF(lcTEOM <> 'Y',InvDate + lnTDaysDue,;
              CTOD('01'+SUBSTR(DTOC(GOMONTH(InvDate,IIF(DAY(InvDate) > lnEOMDay,2,1))),3))-1+lnTDaysDue)
  
  IF DueDate <> ldDueDate
    =RLOCK()
    REPLACE DueDate WITH ldDueDate
    UNLOCK
  ENDIF  
  
  IF SEEK(Account + Invoice,(lcTmpDebit))
    SELECT (lcTmpDebit)
    SCAN REST WHILE Account + Tran + cInstalNo + DTOS(TranDate) = InvHdr.Account + InvHdr.Invoice
      IF TranType = '1' AND DueDate <> ldDueDate
        =RLOCK()
        REPLACE DueDate WITH ldDueDate
        UNLOCK
      ENDIF  
    ENDSCAN
    SELECT InvHdr
  ENDIF
                 
  *-lcInvcee    >> Variable hold the invoice No.
  lcInvcee = INVHDR.INVOICE
  lnConsLine = 0
  
  IF INVHDR.CONSOL = 'Y'
    *--Case consolidated invoice.
    SELECT CONSINVL
    SCAN REST WHILE Invoice+Store+Order+Style+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT

      SCATTER MEMVAR MEMO
      lnConsLine = lnConsLine + 1
      m.LineNo = lnConsLine
      
      m.cDsgnCode = IIF(SEEK("SO"+Order+STR(LineNo,6),(lcTmpBomVr)),EVAL(lcTmpBomVr+'.cDsgnCode'),'')
      IF !EMPTY(m.cDsgnCode)
        m.cDsgnName = IIF(SEEK(m.cDsgnCode,(lcTmpIcDsg)),EVAL(lcTmpIcDsg+'.cDsgnName'),'')
      ENDIF        
      
      lnRecNoSt = 0
      lnRecNoSt = RECNO('STYLE')
      =SEEK(CONSINVL.STYLE,'STYLE')
      lnTaxRat = 0
      = gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
      lnTaxBreak = STYLE.NTAXBREAK
      IF BETWEEN(lnRecNoSt,1,RECCOUNT('STYLE'))
        GOTO lnRecNoSt IN STYLE
      ENDIF  
      
      *B605588,1 Get description from InvLine file. [Begin]
      SELECT InvLine
      lcInvLnTag = TAG()
      SET ORDER TO TAG InvLines
      m.Desc1 = IIF(SEEK(m.Style+m.Invoice),Desc1,'')
      SET ORDER TO TAG (lcInvLnTag)
      *B605588,1 Get description from InvLine file. [End]
      
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT (lcTaxable)
      IF SEEK(lcInvcee)
        FOR lnLop = 1 TO SCALE.CNT
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
        FOR lnLop = 1 TO SCALE.CNT
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

      m.cDsgnCode = IIF(SEEK("SO"+Order+STR(LineNo,6),(lcTmpBomVr)),EVAL(lcTmpBomVr+'.cDsgnCode'),'')
      IF !EMPTY(m.cDsgnCode)
        m.cDsgnName = IIF(SEEK(m.cDsgnCode,(lcTmpIcDsg)),EVAL(lcTmpIcDsg+'.cDsgnName'),'')
      ENDIF  
      
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
ENDSCAN

SET CENTURY &lcDateCent

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