*!***************************************************************************
*! Program file  : ARPINVDR.PRG
*! Program desc. : Customized Invoice Form for PANACHE LINGERIE Ltd  [PAN21]
*! Date          : 09/06/2005
*! System        : Aria Advantage Series.
*! Module        : ACCOUNT RECEIVABLE (AR)
*! Developer     : Nader Nabil Abd Elmonam (NNA)
*! Tracking Job Number : C128476
*!**************************************************************************
*! Calls : FUNCTIONS  : 
*!**************************************************************************
*! Example : DO ARPINVDR
*!**************************************************************************
*!Modification:
*! B130741,1 MMR 02/22/2006 Fix bug of Showing the goods amount with wrong values.
*!C131057,1 EIH 03/02/2006 Amendments to invoice form for panache.
*!B130741,2 MMR 03/06/2006 Fix bug of Showing the goods without deducting discount in FRX.
*!**************************************************************************
STORE .F. TO llUpdated
STORE '' to lcInvChgNo,lcChrgDesc
STORE 0 TO lnScale,lnTaxRat
SET CENT ON
lcTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lcCostMeth = gfGetMemVar('M_Cost_Meth',gcAct_Comp)
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'
lcTaxable  = GFTEMPNAME()


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

*-- Open needed files
= lfOpClFles('OPEN')

*-- Create Temp File
= lfCrtTemp()

*-- Make RelationShip Between files Before collecting Data
= lfSetRela1()

*-- Collecting Data
= lfColData()

*-- Make RelationShip Between files After collecting Data
= lfSetRela2()
SELECT INVHDR
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.

SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')

*-- Close the opened files
= lfOpClFles('CLOSE')

*!**************************************************************************
*!* Name        : lfOpClFles
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Open or Close Files
*!**************************************************************************
*!* Called from : ARPINVPN.PRG
*!**************************************************************************
*!* Parameters  : None
*!**************************************************************************
*!* Return      : None
*!**************************************************************************
*!* Example     : = lfOpClFles()
*!**************************************************************************
*!*!C128476,1
FUNCTION lfOpClFles
PARAMETER lcAction
IF lcAction = 'OPEN' AND !USED('APVENDOR')
  = gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')
ELSE
  IF lcAction = 'CLOSE' AND USED('APVENDOR')
    USE IN APVENDOR
  ENDIF
ENDIF
IF lcAction = 'OPEN' AND !USED('PIKLINE')
  = gfOpenFile(gcDataDir+'PIKLINE','PIKLINE','SH')
ELSE
  IF lcAction = 'CLOSE' AND USED('PIKLINE')
    USE IN PIKLINE
  ENDIF
ENDIF
IF lcAction = 'OPEN' AND !USED('INVCHRG')
  = gfOpenFile(gcDataDir+'INVCHRG','INVCHRG','SH')
ELSE
  IF lcAction = 'CLOSE' AND USED('INVCHRG')
    USE IN INVCHRG
  ENDIF
ENDIF
IF lcAction = 'OPEN' AND !USED('POSHDR')
  = gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
ELSE
  IF lcAction = 'CLOSE' AND USED('POSHDR')
    USE IN POSHDR
  ENDIF
ENDIF

IF lcAction = 'OPEN' AND !USED('STYINVJL')
  = gfOpenFile(gcDataDir+'STYINVJL','MFGOPR','SH')
ELSE
  IF lcAction = 'CLOSE' AND USED('STYINVJL')
    USE IN STYINVJL
  ENDIF
ENDIF
*-- End of Function lfOpClFles.
*!***************************************************************************
*!*! Name      : lfCrtTemp
*! Developer   : Nader Nabil (NNA)
*! Date        : 09/06/2005
*! Purpose     : Function to Creat the temp. File to hold the 
*!             : needed invoice's information
*!***************************************************************************
*! Called from : ARPINVPN.PRG
*!***************************************************************************
*!*! Return      : None
*!***************************************************************************
*!*! Example     : = lfCrtTemp()
*!***************************************************************************
*!*!C128476,1
FUNCTION lfCrtTemp
PRIVATE laFileStru,lnFileStru,laIndex
SELECT INVLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+4,4]
laFileStru[lnFileStru+1,1] = 'Piktkt'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'CountryOri'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 20
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'TariffCode'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 20
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'LPRINT'
laFileStru[lnFileStru+4,2] = 'L'
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0

DECLARE laIndex[2,2]
laIndex[1,1] = 'INVOICE+STORE+ORDER+PIKTKT+STYLE+STR(LINENO,6)'
laIndex[1,2] = 'InvPik'
laIndex[2,1] = 'INVOICE+STORE+ORDER+STYLE+STR(LINENO,6)'
laIndex[2,2] = (lcDetTemp)
=gfCrtTmp(lcDetTemp,@laFileStru,@laIndex)

CREATE TABLE (gcWorkDir + lcInvTemp)(Invoice C(6),SHIPAMT N(10,2),Discount N(13,2),DUEDATE1 D(8),DISC1 N(5,2),DUEDATE2 D(8),;
             DISC2 N(5,2),VAT N(10,2),MNOTES M(10),MChrgDesc M(10),MChrgVal M(10),NONMERCH L(1),LPRINT L(1))
INDEX ON (INVOICE) TAG (lcInvTemp) OF (lcInvTemp)

CREATE TABLE (gcWorkDir+lcTaxable) (INVOICE C(6) , Taxable N(12,2) , NonTax N(12,2) , Price N(9,2))
INDEX ON INVOICE TAG TaxPlat OF (gcWorkDir + lcTaxable + '.CDX') 

*--End function lfCrtTmp.
*!***************************************************************************
*!* Name       : lfColData
*!* Developer  : Nader Nabil (NNA)
*!* Date       : 09/06/2005
*!* Purpose    : Collecting the needed data for PAN21's form e.g :
*!*            : 1 - Terms Discount and Date (the first and the second)
*!*            : 2 - Country Origin (Vendor Country) and Tariff Code (Style.CcomCode)
*!*            : 3 - Note Template.
*!***************************************************************************
*!*! Called from : ARPINVDR.PRG
*!***************************************************************************
*!*! Return      : None
*!***************************************************************************
*!*! Example     : = lfColData()
*!***************************************************************************
*!*!C128476,1
FUNCTION lfColData
PRIVATE lcNoteTemp,lcStyleCom,lcCountry
STORE '' TO lcNoteTemp,lcStyleCom
SET ORDER TO TAG CINVLINE IN CONSINVL
SELECT INVHDR
SCAN FOR IIF(EMPTY(lcRpExp) , .T. , &lcRpExp)
  lcNoteTemp = ALLTRIM(INVHDR.CCURRCODE)+'/'+ALLTRIM(CUSTOMER.CCONT_CODE)
  *--lcInvcee >> hold the invoice No.
  lcInvcee = INVHDR.INVOICE
  llUpdated = .F.
  *--Case consolidated invoice.
  IF INVHDR.CONSOL = 'Y'
    lnConsLine = 0
    SET SKIP TO
    SET ORDER TO TAG INVPIK IN (lcDetTemp)
    IF SEEK(INVHDR.INVOICE,'Consinvh')
      SELECT Consinvh
      SCAN REST WHILE Invoice+Store+Order = lcInvcee
        IF SEEK(Consinvh.PikTkt+Consinvh.order,'PikLine')
          SELECT PikLine
          SCAN REST WHILE PikTkt+Order+STR(LineNo,6) = Consinvh.PikTkt+Consinvh.order
            *B130741,1 MMR 02/22/2006 Fix bug of Showing the goods amount with wrong values.
            *IF SEEK(PikLine.Order + STR(Pikline.LineNo,6),'Consinvl')
            IF SEEK(PikLine.Order + STR(Pikline.LineNo,6) + Consinvh.Invoice,'Consinvl')
            *B130741,1 MMR.[End]
              SELECT Consinvl
              *B130741,1 MMR 02/22/2006 Fix bug of Showing the goods amount with wrong values.
              *SCAN REST WHILE Order+STR(LineNo,6)+Invoice = PikLine.Order + STR(Pikline.LineNo,6)
              SCAN REST WHILE Order+STR(LineNo,6)+Invoice = PikLine.Order + STR(Pikline.LineNo,6) + Consinvh.Invoice
              *B130741,1 MMR.[End]
                WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
                SCATTER MEMVAR MEMO
                lnConsLine = lnConsLine + 1
                m.LineNo = lnConsLine      
                lnRecNoSt = 0
                lnRecNoSt = RECNO('STYLE')
                =SEEK(CONSINVL.STYLE,'STYLE')
                m.Desc1 = STYLE.DESC1
                =SEEK('S'+STYLE.SCALE,'SCALE')
                lnScale = SCALE.CNT
                lnTaxRat = 0
                = gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
                lnTAxBreak = STYLE.NTAXBREAK
                IF BETWEEN(lnRecNoSt,1,RECCOUNT('STYLE'))
                  GOTO lnRecNoSt IN STYLE
                ENDIF  
                lcCountry    = IIF(ALLTRIM(CUSTOMER.CCONT_CODE)<>'GB',lfGetCntry(lcInvcee),'')
                lcStyleCom   = IIF(SEEK(Consinvl.STYLE,'STYLE') AND ALLTRIM(CUSTOMER.CCONT_CODE)<>'GB',STYLE.CCOMCODE,'')
                m.CountryOri = lcCountry
                m.TariffCode = lcStyleCom
                m.PikTkt     = Consinvh.PikTkt
                m.lPrint     = IIF(ALLTRIM(SUBSTR(Consinvl.STYLE , lnStyPosGl , lnStyLnGl ))='ZZ',.F.,.T.)
                SELECT(lcDetTemp)
                APPEND BLANK
                GATHER MEMVAR MEMO
                SELECT (lcTaxable)
                IF SEEK(lcInvcee)
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
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
  ELSE
    SET SKIP TO
    SET ORDER TO TAG (lcDetTemp) IN (lcDetTemp)
    IF SEEK(INVHDR.INVOICE,'INVLINE')
      SELECT INVLINE
      SCAN REST WHILE Invoice+STR(Lineno,6)=lcInvcee
        WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
        SCATTER MEMVAR MEMO
        lcCountry  = IIF(ALLTRIM(CUSTOMER.CCONT_CODE)<>'GB',lfGetCntry(lcInvcee),'')
        lcStyleCom = IIF(SEEK(INVLINE.STYLE,'STYLE') AND ALLTRIM(CUSTOMER.CCONT_CODE)<>'GB',STYLE.CCOMCODE,'')
        SELECT(lcDetTemp)
        m.CountryOri = lcCountry
        m.TariffCode = lcStyleCom
        m.lPrint      = IIF(ALLTRIM(SUBSTR(InvLine.STYLE , lnStyPosGl , lnStyLnGl ))='ZZ',.F.,.T.)
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
  ENDIF
  *--Add the charges amount .. Taxable and Non Taxable
  lnChrgTax = 0
  lnChgNonTx = 0
  *--Check if we still on the same invoice to prevent duplication of amount
  IF lcInvChgNo <> lcInvcee 
    lcInvChgNo = lcInvcee
    SELECT InvChrg
    IF SEEK(lcInvcee)
      SCAN REST WHILE invoice+cstore+cchrgcode = lcInvcee
        lcChrgDesc = gfCodDes(INVCHRG.CCHRGCODE, "CCHRGCODE") 
        IF SEEK(lcInvcee,lcInvTemp)
          SELECT(lcInvTemp)
          REPLACE MChrgDesc WITH IIF(MEMLINES(MChrgDesc)>0,MChrgDesc+CHR(10)+ALLTRIM(lcChrgDesc),ALLTRIM(lcChrgDesc)) ,;
                  MChrgVal  WITH IIF(MEMLINES(MChrgVal)>0,MChrgVal+CHR(10)+ALLTRIM(STR(INVCHRG.NCHRGAMNT,12,2)),ALLTRIM(STR(INVCHRG.NCHRGAMNT,12,2)))
        ELSE
          INSERT INTO (lcInvTemp)(INVOICE,SHIPAMT,MChrgDesc,MChrgVal) VALUES(INVHDR.INVOICE,INVHDR.SHIPAMT,lcChrgDesc,ALLTRIM(STR(INVCHRG.NCHRGAMNT,12,2)))       
        ENDIF
        *--if this charge is taxable, add the amount to the taxable charge amount 
        SELECT InvChrg
        IF  nTaxRate > 0 
          lnChrgTax   = lnChrgTax  + nChrgAmnt
        ELSE
          lnChgNonTx  = lnChgNonTx + nChrgAmnt
        ENDIF
      ENDSCAN
      SELECT (lcTaxable)
      IF SEEK(lcInvcee)
         REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + lnChrgTax  ,;
                 &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax    ,;
                 &lcTaxable..NonTax  WITH &lcTaxable..NonTax + lnChgNonTx
      ELSE
        APPEND BLANK
        REPLACE &lcTaxable..INVOICE WITH lcInvcee                          ,;
                &lcTaxable..Taxable WITH &lcTaxable..Taxable + lnChrgTax   ,;
                &lcTaxable..PRICE   WITH &lcTaxable..PRICE + lnChrgTax     ,;
                &lcTaxable..NonTax  WITH &lcTaxable..NonTax + lnChgNonTx
      ENDIF
    ENDIF
  ENDIF
  IF !llUpdated
    = lfCalcDisc()
  ENDIF
  IF SEEK('T'+lcNoteTemp,'NOTEPAD')
    IF SEEK(INVHDR.INVOICE,lcInvTemp)
      SELECT (lcInvTemp)
      REPLACE &lcInvTemp..Mnotes WITH NOTEPAD.Mnotes
    ELSE
      INSERT INTO (lcInvTemp)(INVOICE,SHIPAMT,MNOTES) VALUES(INVHDR.INVOICE,INVHDR.SHIPAMT,NOTEPAD.MNOTES)
    ENDIF
  ENDIF
  SELECT (lcInvTemp)
  REPLACE LPRINT WITH m.lPrint
ENDSCAN
SET ORDER TO TAG CONSINVL IN CONSINVL
*--End function lfColDataN.
*!***************************************************************************
*! Name        : lfSetRela1
*! Developer   : Nader Nabil (NNA)
*! Date        : 09/06/2005
*! Purpose     : Function to Creat Relationship between files
*!             : before collecting data.
*!***************************************************************************
*!*! Called from : ARPINVDR.PRG
*!***************************************************************************
*!*! Return      : None
*!***************************************************************************
*!*! Example     : = lfSetRela1()
*!***************************************************************************
*!*!C128476,1
FUNCTION lfSetRela1
SELECT INVHDR
SET RELATION OFF INTO Consinvh
SET RELATION OFF INTO INVCHRG
SET RELATION TO INVOICE INTO Consinvh ADDITIVE
SET RELATION TO INVOICE INTO InvChrg ADDITIVE

SELECT Consinvh
SET RELATION OFF INTO PikLine
SET RELATION TO Consinvh.PikTkt+ Consinvh.Order INTO PikLine ADDITIVE
SELECT PikLine
SET RELATION OFF INTO Consinvl
SET RELATION TO PikLine.order+STR(PikLine.LineNo,6) INTO Consinvl ADDITIVE

*--End function lfSetRela1.
*!***************************************************************************
*! Name        : lfSetRela2
*! Developer   : Nader Nabil (NNA)
*! Date        : 09/06/2005
*! Purpose     : Function to Creat Relationship between files
*!             : After collecting data and before printing.
*!***************************************************************************
*!*! Called from : ARPINVDR.PRG
*!***************************************************************************
*!*! Return      : None
*!***************************************************************************
*!*! Example     : = lfSetRela2()
*!***************************************************************************
*!*!C128476,1
FUNCTION lfSetRela2
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO ORDHDR
SET RELATION OFF INTO CONSINVL
SET RELATION OFF INTO (lcInvTemp)
SET RELATION OFF INTO InvChrg

SELECT (lcTmpDbt)
SET RELATION TO

SELECT STYLE
SET RELATION OFF INTO SCALE
             *-- End Section break the relatoins --*

*--Close the invline and open the temp. file with the invline name.
SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
USE IN (lcDetTemp)
USE (gcWorkDir+lcDetTemp) IN 0 ALIAS INVLINE
SELECT INVLINE
SET ORDER TO TAG INVPIK 

*--Create the new relatoins
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
SET RELATION TO INVHDR.INVOICE INTO (lcInvTemp) ADDITIVE
SET RELATION TO INVHDR.INVOICE INTO InvChrg ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , INVLINE
ELSE
  SET SKIP TO INVLINE
ENDIF
*--End function lfSetRela2.
*!***************************************************************************
*!* Name        : lfGetCntry
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Get the Country of Origin (Vendor Country)
*!***************************************************************************
*!* Called from : ARPINVPN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetCntry()
*!***************************************************************************
*!*!C128476,1
FUNCTION lfGetCntry
PARAMETER lcInvoice
PRIVATE lnOldAlias,lcRecvSess,lcOldOrder,lcPoNo,lcVendCont
STORE '' TO lcRecvSess,lcOldOrder,lcPoNo,lcVendCont
lnOldAlias = SELECT(0)
lcOldOrder = ORDER('STYINVJL')
SET ORDER TO TAG MFGOPR IN STYINVJL
IF SEEK(lcInvoice,'STYINVJL')
  SELECT STYINVJL
  lcRecvSess = STYINVJL.CRSESSION
  SET ORDER TO TAG STYINVJL IN STYINVJL
  LOCATE
  = SEEK(IIF(INVHDR.CONSOL='Y',CONSINVL.STYLE+CONSINVH.CWARECODE,INVLINE.STYLE+INVLINE.CWARECODE))
  SCAN REST WHILE Style+cWarecode+cSession+DTOS(dTrdate)+cTrcode+STR(Lineno,6) = ;
       IIF(INVHDR.CONSOL='Y',CONSINVL.STYLE+CONSINVH.CWARECODE,INVLINE.STYLE+INVLINE.CWARECODE) FOR CTRTYPE = '6' AND CRSESSION = lcRecvSess
    lcPoNo = STYINVJL.CTRCODE
    EXIT
  ENDSCAN
  IF !EMPTY(ALLTRIM(lcPoNo)) AND SEEK('P'+lcPoNo,'POSHDR')
    IF SEEK(POSHDR.VENDOR,'APVENDOR')
      lcVendCont = APVENDOR.CADDRESS6
    ENDIF
  ELSE
    IF SEEK(IIF(INVHDR.CONSOL='Y',CONSINVL.STYLE,INVLINE.STYLE),'STYLE') AND !EMPTY(STYLE.VENDOR)
      IF SEEK(STYLE.VENDOR,'APVENDOR')
        lcVendCont = APVENDOR.CADDRESS6
      ENDIF
    ELSE
      lcVendCont = ''
    ENDIF
  ENDIF
ELSE
  IF SEEK(IIF(INVHDR.CONSOL='Y',CONSINVL.STYLE,INVLINE.STYLE),'STYLE') AND !EMPTY(STYLE.VENDOR)
    IF SEEK(STYLE.VENDOR,'APVENDOR')
      lcVendCont = APVENDOR.CADDRESS6
    ENDIF
  ELSE
    lcVendCont = ''
  ENDIF
ENDIF
SET ORDER TO TAG &lcOldOrder IN STYINVJL
SELECT(lnOldAlias)
RETURN lcVendCont

*-- End of Function lfGetCntry.
*!***************************************************************************
*!* Name        : lfCalcDisc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Calculate the payment Terms Discount
*!***************************************************************************
*!* Called from : ARPINVPN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCalcDisc()
*!***************************************************************************
FUNCTION lfCalcDisc
DECLARE laDisc1[1,2],laDDate1[1,2]
lnOldAlias = SELECT(0)
lnDisc1  = 0
lnDDate1 = 0
lnDisc2  = 0
lnDDate2 = 0
laDisc1[1,1]  = 'NTERDISCR'
laDisc1[1,2]  = 'lnDisc1'
laDDate1[1,1] = 'NTERDISCD'
laDDate1[1,2] = 'lnDDate1'
=gfRltFld(INVHDR.CTERMCODE,@laDisc1,'CTERMCODE')
=gfRltFld(INVHDR.CTERMCODE,@laDDate1,'CTERMCODE')
IF SEEK('T'+INVHDR.CTERMCODE,'NOTEPAD')
  lnDisc2  = VAL(MLINE(NOTEPAD.MNOTES,1))
  lnDDate2 = VAL(MLINE(NOTEPAD.MNOTES,2))
ENDIF
IF !SEEK(INVHDR.INVOICE,lcInvTemp)
  INSERT INTO (lcInvTemp)(INVOICE,SHIPAMT,DUEDATE1,DISC1,DUEDATE2,DISC2,Discount);
  VALUES(INVHDR.INVOICE,INVHDR.SHIPAMT,(INVHDR.INVDATE+lnDDate1),lnDisc1,(INVHDR.INVDATE+lnDDate2),lnDisc2,INVHDR.DISCOUNT)
ELSE
  SELECT (lcInvTemp)
  REPLACE DUEDATE1  WITH INVHDR.INVDATE+lnDDate1  ,;
          DISC1     WITH lnDisc1                  ,;
          DUEDATE2  WITH INVHDR.INVDATE+lnDDate2  ,;
          DISC2     WITH lnDisc2                  ,;
          DISCOUNT  WITH INVHDR.DISCOUNT
ENDIF
llUpdated = .T.
SELECT(lnOldAlias)
RETURN
*-- End of Function lfCalcDisc.
*!*************************************************************
*!* Name        : lfCustExmt
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Function to check if the customer is exempted oe not.
*!*************************************************************
*!* Called from : ARPINVPN.PRG
*!*************************************************************
*!* Passed      : None
*!*************************************************************
*!* Return      : True or False
*!*************************************************************
*!* Example     : =lfCustExmt()
*!*************************************************************
*!*!C128476,1
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
*!* Name        : lfGetNotes
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Fill the Array (laNoteTemp) with Notes from Notepad Template
*!***************************************************************************
*!* Called from : ARPINVPN.FRX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetNotes()
*!***************************************************************************
*!*!C128476,1
FUNCTION lfGetNotes
LNOLDALIAS = SELECT(0)
SELECT(lcInvTemp)
DECLARE laNoteTemp[13]
STORE '' TO laNoteTemp
FOR I = 1 TO 13
  laNoteTemp[I] = MLINE(MNOTES,I)
ENDFOR
SELECT(LNOLDALIAS)
*-- End of Function lfGetNotes.



*C131057,1 EIH 03/02/2006 Amendments to invoice form for panache [Begin]
*!***************************************************************************
*!* Name        : lfGetStyle
*!* Developer   : EIH - EHAB ISMAIL HAMED
*!* Date        : 03/02/2006
*!* Purpose     : Fill tow variables lcStyName ,lcPanSty 
*!***************************************************************************
*!* Called from : ARPINVPN.FRX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetStyle()
*!***************************************************************************
FUNCTION lfGetStyle
PARA lcDummy
STORE '' TO lcStyName ,lcPanSty 
FOR lnI = 1 TO 15
  lcI = PADL(lnI,2,'0')
  IF INVLINE.ACCOUNT == SUBSTR(ALLTRIM(EVALUATE('STYLE.CCUSSTY'+lcI)) ,1,5)
    lcStyName = ALLTRIM(SUBSTR(ALLTRIM(EVALUATE('STYLE.CCUSSTY'+lcI)) ,7,40)+' - ' +gfCodDes(SUBSTR(IIF(!EMPTY(INVLINE.ALTSTYLE),INVLINE.ALTSTYLE,INVLINE.STYLE) , lnClrPosGl , lnClrLnGl ),'COLOR'))
    EXIT
  ENDIF
ENDFOR
lcPanSty = IIF(EMPTY(lcStyName),ALLTRIM(SUBSTR(INVLINE.STYLE,lnStyPosGl,lnStyLnGl))+IIF(!EMPTY(INVLINE.ALTSTYLE) OR !EMPTY(INVLINE.STYLE) , " - "+  gfCodDes(SUBSTR(IIF(!EMPTY(INVLINE.ALTSTYLE),INVLINE.ALTSTYLE,INVLINE.STYLE), lnClrPosGl , lnClrLnGl ),'COLOR') , ""),'Panache Style- '+ALLTRIM(SUBSTR(INVLINE.STYLE , lnStyPosGl , lnStyLnGl )))
RETURN ''
*C131057,1 EIH 03/02/2006 [End]