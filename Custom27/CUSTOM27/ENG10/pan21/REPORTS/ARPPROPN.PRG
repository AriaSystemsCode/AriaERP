*!***************************************************************************
*! Program file  : ARPPROPN.PRG
*! Program desc. : Customized Print Pro-Forma ORDER for PANACHE [PAN21]
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
*!**************************************************************************
STORE '' to lcOrdChgNo,lcChrgDesc
STORE 0 TO lnTaxRat
*-- Open needed files
IF !USED('APVENDOR')
  = gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')
ENDIF
*-- Create Temp File
= lfCrtTemp()
SELECT(lcOrdHdr)
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO (lcOrdTemp) FROM MEMVAR
ENDSCAN

*-- Collecting Data
= lfColData()

*-- Make RelationShip Between files After collecting Data
= lfSetRela2()
SELECT(lcOrdline)
DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN
llarpinv = .F.
*-- Close the opened files
IF USED('APVENDOR')
  USE IN APVENDOR
ENDIF
USE IN (lcOrdLine)
USE IN (lcOrdHdR)
ERASE (gcWorkDir + (lcOrdLine)+'.*')
ERASE (gcWorkDir + (lcOrdHdR)+'.*')
ERASE (gcWorkDir + (lcOrdTemp)+'.*')
ERASE (gcWorkDir + (lcDetTemp)+'.*')

*!***************************************************************************
*!*! Name      : lfCrtTemp
*! Developer   : Nader Nabil (NNA)
*! Date        : 09/06/2005
*! Purpose     : Function to Creat the temp. File to hold the 
*!             : needed ORDER's information
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
SELECT(lcOrdLine)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,4]
laFileStru[lnFileStru+1,1] = 'CountryOri'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 30
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'TariffCode'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 30
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'LPRINT'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 1
laFileStru[lnFileStru+3,4] = 0
=gfCrtTmp(lcDetTemp,@laFileStru,'cOrdType+Order+cType+Store+Style+STR(LineNo,6)',lcDetTemp)

SELECT(lcOrdHdr)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+6,4]
laFileStru[lnFileStru+1,1] = 'MNOTES'
laFileStru[lnFileStru+1,2] = 'M'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'MCHRGDESC'
laFileStru[lnFileStru+2,2] = 'M'
laFileStru[lnFileStru+2,3] = 10
laFileStru[lnFileStru+2,4] = 0

laFileStru[lnFileStru+3,1] = 'MCHRGVAL'
laFileStru[lnFileStru+3,2] = 'M'
laFileStru[lnFileStru+3,3] = 10
laFileStru[lnFileStru+3,4] = 0

laFileStru[lnFileStru+4,1] = 'NON_TAX'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 15
laFileStru[lnFileStru+4,4] = 5

laFileStru[lnFileStru+5,1] = 'TAXABLE'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 15
laFileStru[lnFileStru+5,4] = 5

laFileStru[lnFileStru+6,1] = 'LPRINT'
laFileStru[lnFileStru+6,2] = 'L'
laFileStru[lnFileStru+6,3] = 1
laFileStru[lnFileStru+6,4] = 0

=gfCrtTmp(lcOrdTemp,@laFileStru,'cOrdType+Order+Store',lcOrdTemp)

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
PRIVATE lcNoteTemp,lcStyleCom,lcCountry,lnMinValue,lcChrgDesc,lcOrder,llExemted,;
        lnTaxQty,llSummed,llAddNChrg,lnStandChg,lnEuroChrg
STORE '' TO lcNoteTemp,lcStyleCom,lcChrgDesc,lcOrder
STORE 0 TO lnMinValue,lnTaxQty,lnStandChg,lnEuroChrg

STORE .F. TO llExemted,llSummed,llAddNChrg
DECLARE laChrgCode[1,2]
=gfCodDes(@laChrgCode,"CCHRGCODE")
SELECT(lcOrdLine)
SET RELATION OFF INTO (lcOrdHdr)
SELECT(lcOrdHdr)
SET RELATION TO cOrdType+Order INTO (lcOrdLine) ADDITIVE
SCAN
  lcOrder = &lcOrdHdr..Order
  llExemted = lfCustExmt()
  SELECT(lcOrdLine)
  SCAN REST WHILE CordType + order = 'O'+ lcOrder
    lcNoteTemp = ALLTRIM(ORDHDR.CCURRCODE)+'/'+ALLTRIM(CUSTOMER.CCONT_CODE)
    WAIT WINDOW 'Selecting Records For The Report ...' + &lcOrdLine..Order NOWAIT
    SCATTER MEMVAR MEMO
    lcCountry  = IIF(ALLTRIM(CUSTOMER.CCONT_CODE)<>'GB',lfGetCntry(),'')
    lcStyleCom = IIF(SEEK(&lcOrdLine..STYLE,'STYLE') AND ALLTRIM(CUSTOMER.CCONT_CODE)<>'GB',STYLE.CCOMCODE,'')
    SELECT(lcDetTemp)
    m.CountryOri = lcCountry
    m.TariffCode = lcStyleCom
    m.lPrint     = IIF(ALLTRIM(SUBSTR(&lcOrdLine..STYLE , 1 , lnMajor))='ZZ',.F.,.T.)
    APPEND BLANK
    GATHER MEMVAR MEMO
    *--Add the charges amount .. Taxable and Non Taxable
    IF &lcOrdHdr..lCarTbc
      IF &lcOrdHdr..cCurrCode='GBP'
        DECLARE laStdShipV[1,2],laChgTax[1,2],laChgVat[1,2]
        lcChgTax   = ''
        lnChgVat   = 0
        laStdShipV[1,1] = 'NSTANDCHRG'
        laStdShipV[1,2] = 'lnStandChg'
        =gfRltFld(&lcOrdHdr..ShipVia,@laStdShipV,'SHIPVIA')
        
        laChgTax[1,1]  = 'CTAXCODE'
        laChgTax[1,2]  = 'lcChgTax'
        =gfRltFld('000176',@laChgTax,'CCHRGCODE')
        
        laChgVat[1,1]  = 'NTAXRATE'
        laChgVat[1,2]  = 'lnChgVat'
        =gfRltFld(ALLTRIM(lcChgTax),@laChgVat,'CTAXCODE')
        
         lnTaxRat = IIF(lnChgVat>0,(lnStandChg*lnChgVat)/100,0)
        lnMinValue = gfGetMemVar('M_MINORDER' ,gcAct_Comp)
        IF &lcOrdHdr..OpenAmt< lnMinValue
          lcChrgDesc = gfCodDes('000176', "CCHRGCODE") 
          IF SEEK('O'+&lcOrdHdr..Order,lcOrdTemp) AND !llSummed
            SELECT(lcOrdTemp)
            REPLACE MChrgDesc WITH ALLTRIM(lcChrgDesc)                    ,;
                    MChrgVal  WITH ALLTRIM(STR(lnStandChg,8,2))           ,;
                    nTotalChg WITH nTotalChg + lnStandChg + lnTaxRat      ,;
                    Tax_Amt   WITH Tax_Amt + lnTaxRat
            llSummed = .T.
          ELSE
            IF !llSummed
              INSERT INTO (lcOrdTemp)(ORDER,OPENAMT,MChrgDesc,MChrgVal,nTotalChg,Tax_Amt) ;
              VALUES(&lcOrdHdr..Order,&lcOrdHdr..OPENAMT,lcChrgDesc,ALLTRIM(STR(lnStandChg,8,2)),lnStandChg+lnTaxRat,lnTaxRat)       
              llSummed = .T.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF ORDHDR.CCURRCODE = "EUR"
        DECLARE laStdShipV[1,2],laChgTax[1,2],laChgVat[1,2]
        lcChgTax   = ''
        lnChgVat   = 0
       
        laStdShipV[1,1] = 'NEUROCHRG '
        laStdShipV[1,2] = 'lnEuroChrg'
        =gfRltFld(&lcOrdHdr..ShipVia,@laStdShipV,'SHIPVIA')
              
        laChgTax[1,1]  = 'CTAXCODE'
        laChgTax[1,2]  = 'lcChgTax'
        =gfRltFld('000176',@laChgTax,'CCHRGCODE')
        
        laChgVat[1,1]  = 'NTAXRATE'
        laChgVat[1,2]  = 'lnChgVat'
        =gfRltFld(ALLTRIM(lcChgTax),@laChgVat,'CTAXCODE')
               
        lnTaxRat = IIF(lnChgVat>0,(lnEuroChrg*lnChgVat)/100,0)
        lnMinValue = gfGetMemVar('M_EURMNORD' ,gcAct_Comp)
        IF &lcOrdHdr..OpenAmt< lnMinValue
          lcChrgDesc = gfCodDes('000176', "CCHRGCODE") 
          IF SEEK('O'+&lcOrdHdr..Order,lcOrdTemp)
            SELECT(lcOrdTemp)
            REPLACE MChrgDesc WITH IIF(MEMLINES(MChrgDesc)>0,MChrgDesc+CHR(10)+ALLTRIM(lcChrgDesc),ALLTRIM(lcChrgDesc)) ,;
                    MChrgVal  WITH IIF(MEMLINES(MChrgVal)>0,MChrgVal+CHR(10)+ALLTRIM(STR(lnEuroChrg,8,2)),ALLTRIM(STR(lnEuroChrg,8,2))),;
                    nTotalChg WITH nTotalChg + lnEuroChrg + lnTaxRat ,;
                    Tax_Amt   WITH Tax_Amt + lnTaxRat
          ELSE
            INSERT INTO (lcOrdTemp)(ORDER,OPENAMT,MChrgDesc,MChrgVal,nTotalChg,Tax_Amt) ;
            VALUES(&lcOrdHdr..Order,&lcOrdHdr..OPENAMT,lcChrgDesc,ALLTRIM(STR(lnEuroChrg,8,2)),lnEuroChrg+lnTaxRat,lnTaxRat)       
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *--Get the Non Taxable Amount (Goods)
    FOR lnLop = 1 TO Scale.Cnt
      lcLop = ALLTRIM(STR(lnLop))
      lnTaxQty = &lcOrdLine..QTY&lcLop 
      IF !BETWEEN(lnLop,STYLE.nTAxBreak,8) AND llExemted AND lnTaxRate > 0
        REPLACE &lcOrdTemp..Non_Tax WITH &lcOrdTemp..Non_Tax + (lnTaxQty * &lcOrdLine..PRICE)
      ELSE
        IF &lcOrdLine..Qty&lcLop > 0
          REPLACE &lcOrdTemp..Taxable WITH &lcOrdTemp..Taxable + (&lcOrdLine..Qty&lcLop * &lcOrdLine..PRICE)
        ENDIF
      ENDIF
    ENDFOR
    *--Add the Non Merchandise Charge to the VAT Box, add it to Taxable goods if it taxable(Tax_Rate>0)
    *--or add it to the Non_taxable goods if Tax_rate=0
    IF lnTaxRat=0 AND !llAddNChrg AND &lcOrdTemp..OpenAmt< lnMinValue
      REPLACE &lcOrdTemp..Non_Tax WITH &lcOrdTemp..Non_Tax + IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg)
      llAddNChrg = .T.
    ELSE
      IF !llAddNChrg AND &lcOrdTemp..OpenAmt< lnMinValue
        REPLACE &lcOrdTemp..Taxable WITH &lcOrdTemp..Taxable + IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg)
        llAddNChrg = .T.
      ENDIF
    ENDIF
    *--Get NotePad Template
    IF SEEK('T'+lcNoteTemp,'NOTEPAD')
      IF SEEK('O'+&lcOrdHdr..ORDER,lcOrdTemp)
        SELECT (LCORDTEMP)
        REPLACE &LCORDTEMP..Mnotes WITH NOTEPAD.Mnotes
      ELSE
        INSERT INTO (lcOrdTemp)(MNOTES) VALUES(NOTEPAD.MNOTES)
      ENDIF
    ENDIF
    SELECT (LCORDTEMP)
    REPLACE LPRINT WITH m.lPrint
  ENDSCAN
ENDSCAN
SELECT(lcOrdHdr)
SET RELATION OFF INTO (lcOrdLine)

*--End function lfColDataN.
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
SELECT (lcOrdLine)
SET RELATION OFF INTO (lcOrdHdr)
SET RELATION OFF INTO OrdHdr
SET RELATION OFF INTO OrdLine
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SCALE

SELECT(lcOrdHdr)
CLOSE INDEX
USE IN (lcOrdHdr)
USE IN (lcOrdTemp)
USE (gcWorkDir+lcOrdTemp) IN 0 ALIAS (lcOrdHdr)
SELECT(lcOrdHdr)
SET ORDER TO TAG &lcOrdTemp
*--Deduct Discount from the goods amount(Taxable Amount)
REPLACE Taxable WITH Taxable + Discount


SELECT(lcOrdLine)
CLOSE INDEX
USE IN (lcOrdLine)
ERASE (gcWorkDir+lcOrdLine
USE IN (lcDetTemp)
USE (gcWorkDir+lcDetTemp) IN 0 ALIAS (lcOrdLine)
SELECT(lcOrdLine)
SET ORDER TO TAG &lcDetTemp

*-- Set the required relations
SET RELATION TO cOrdType+Order INTO (lcOrdHdr) ADDITIVE
SET RELATION TO cOrdType+Order INTO OrdHdr ADDITIVE
SET RELATION TO cOrdType+Order INTO OrdLine ADDITIVE
SET RELATION TO IIF(EMPTY(Store), 'M' + Account , 'S'+Account+Store) INTO CUSTOMER ADDITIVE
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

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
PRIVATE lnOldAlias,lcRecvSess,lcOldOrder,lcPoNo,lcVendCont
STORE '' TO lcRecvSess,lcOldOrder,lcPoNo,lcVendCont
lnOldAlias = SELECT(0)
IF SEEK(&lcOrdLine..STYLE,'STYLE') AND !EMPTY(STYLE.VENDOR)
  IF SEEK(STYLE.VENDOR,'APVENDOR')
    lcVendCont = APVENDOR.CADDRESS6
  ENDIF
ENDIF
SELECT(lnOldAlias)
RETURN lcVendCont

*-- End of Function lfGetCntry.
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
SELECT(lcOrdHdr)
DECLARE laNoteTemp[14]
STORE '' TO laNoteTemp
FOR I = 1 TO 14
  laNoteTemp[I] = MLINE(MNOTES,I)
ENDFOR
SELECT(LNOLDALIAS)
*-- End of Function lfGetNotes.
*!***************************************************************************
*!* Name        : lfCustExmt
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Function to check if the customer is exempted or not.
*!***************************************************************************
*!* Called from : Arppropn.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCustExmt()
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
*-- End of Function lfCustExmt.