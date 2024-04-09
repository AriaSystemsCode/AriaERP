*:***************************************************************************
*: Program file  : ARPINVDR.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR EDREAM APPAREL
*! Date          : 11/25/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : RANIA ABDEL RAZIK
*: Tracking Job Number: C102743
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVDR
*: B120823,1 ABD 12/08/2003 Some Modification to the Frx to print the SKU# Correct.
*:***************************************************************************

*llRpInvLNt  = .F.
lnAlias = SELECT()
=gfOpenFile(gcDataDir+"Salesrep","Salesrep",'SH')
SELECT (lnAlias)

*--create temp file for hold the Invline distinct with pack id
lcTempLine = gfTempName()
SELECT InvLine
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir + lcTempLine) FROM ARRAY laFileStru
INDEX ON invoice+STR(lineno,6) TAG (lcTempLine) OF (gcWorkDir + lcTempLine + '.CDX')
INDEX ON Style TAG 'Style' OF (gcWorkDir + lcTempLine + '.CDX')
SET ORDER TO TAG STYLE

SELECT INVHDR
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
*-- End Section break the relatoins.

IF USED(LCInvLine_A)
   USE IN (LCInvLine_A)
ENDIF

*--if we print standard invoices
IF lcInvKind = "S"
  lcTempHdr = 'INVHDR'
  lcInvLine = 'INVLINE'  
  *--if we print by Pack Id will acumulate all qty for the same pack id in one line
  IF lcRpPack='Y'
    SELECT (lcTempHdr)
    SCAN FOR &lcRpExp
      SELECT (lcInvLine)
      IF SEEK(&lcTempHdr..Invoice)
        SCAN REST WHILE INVOICE = &lcTempHdr..Invoice
          SCATTER MEMVAR MEMO
          SELECT (lcTempLine)
          IF !EMPTY(m.Pack_Id)
            m.Style = m.Pack_Id
            IF !SEEK(ALLTRIM(m.Style))
               APPEND BLANK
               GATHER MEMVAR MEMO
            ELSE
              REPLACE Qty1   WITH Qty1 + m.Qty1 ,;
                      Qty2   WITH Qty2 + m.Qty2 ,;
                      Qty3   WITH Qty3 + m.Qty3 ,;
                      Qty4   WITH Qty4 + m.Qty4 ,;
                      Qty5   WITH Qty5 + m.Qty5 ,;
                      Qty6   WITH Qty6 + m.Qty6 ,;
                      Qty7   WITH Qty7 + m.Qty7 ,;
                      Qty8   WITH Qty8 + m.Qty8 ,;
                      PRICE  WITH ((TotQty * Price)+ (m.TotQty * m.Price)) / (totQty + m.TotQty) ,;
                      TotQty WITH TotQty + m.TotQty 
            ENDIF
          ELSE
            APPEND BLANK
            GATHER MEMVAR MEMO
          ENDIF
        ENDSCAN  
      ENDIF
    ENDSCAN
    SELECT (lcTempLine)
    SET ORDER TO TAG (lcTempLine)
    =gfOpenFile(gcWorkDir+lcTempLine,lcTempLine,'SH', @LCInvLine_A, .T.)
  ELSE
    lcTempLine= 'INVLINE'
    =gfOpenFile(gcDataDir+'INVLINE','INVLINE','SH', @LCInvLine_A, .T.)
  ENDIF
ELSE
  *--if we print pre forma invoice
  lcTempHdr = lcInvHdrS
  lcRpExp = STRTRAN(lcRpExp,"INVHDR","INVHDRS")
  lcRpExp = lcRpExp + IIF(EMPTY(lcInvoicSd) , "" , "  AND " + lcInvoicSd) + " AND EMPTY(INVHDRS.FLAG)"
  
  SELECT INVHDRS
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir + lcInvHdrS) FROM ARRAY laFileStru
  INDEX ON INVOICE TAG (lcInvHdrS) OF (gcWorkDir + lcInvHdrS + '.CDX')
  
  SELECT INVLINES
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir + lcInvLineS) FROM ARRAY laFileStru
  INDEX ON INVOICE + STR(LINENO,6) TAG (lcInvLineS) OF (gcWorkDir + lcInvLineS + '.CDX')
  INDEX ON Style TAG 'Style' OF (gcWorkDir + lcInvLineS + '.CDX')
  SET ORDER TO TAG STYLE
  
  SELECT INVHDRS
  SET RELATION TO INVHDRS.CSADINV INTO INVLINES ADDITIVE
  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    m.Invoice = m.CSADINV
    INSERT INTO (lcInvHdrS) FROM MEMVAR 
    *-lcInvcee    >> Variable hold the invoice No.
    lcInvcee = INVHDRS.CSADINV
    SELECT INVLINES
    SCAN REST WHILE CSADINV + STR(LINENO,6) = lcInvcee AND INVHDRS.CONSOL = INVLINES.CONSOL AND INVLINES.TOTQTY > 0
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      SCATTER MEMVAR MEMO
      m.Invoice = m.CSADINV
      SELECT(lcInvLineS)
      IF lcRpPack='Y' AND !EMPTY(m.Pack_Id) 
        *--in case we print by pack _id will acumlate the qty for each pack id in one line
        m.Style = m.Pack_Id
        IF !SEEK(ALLTRIM(m.Style))
          APPEND BLANK
          GATHER MEMVAR MEMO
        ELSE
          REPLACE Qty1   WITH Qty1 + m.Qty1 ,;
                  Qty2   WITH Qty2 + m.Qty2 ,;
                  Qty3   WITH Qty3 + m.Qty3 ,;
                  Qty4   WITH Qty4 + m.Qty4 ,;
                  Qty5   WITH Qty5 + m.Qty5 ,;
                  Qty6   WITH Qty6 + m.Qty6 ,;
                  Qty7   WITH Qty7 + m.Qty7 ,;
                  Qty8   WITH Qty8 + m.Qty8 ,;
                  PRICE  WITH ((TotQty * Price)+ (m.TotQty * m.Price)) / (totQty + m.TotQty) ,;
                  TotQty WITH TotQty + m.TotQty 
        ENDIF
      ELSE
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF
    ENDSCAN
  ENDSCAN
  SELECT (lcInvLineS)
  SET ORDER TO TAG (lcInvLineS)
  =gfOpenFile(gcWorkDir+lcInvLineS,lcInvLineS,'SH', @LCInvLine_A, .T.)

  SELECT INVHDRS
  SET RELATION OFF INTO INVLINES
  lcTempLine= lcInvLineS
ENDIF

SELECT (lcTempHdr)
IF llPrntInst .OR. llRpInvNot 
  IF !llIsAparel
    SET RELATION TO '' INTO (lcTmpDbt) ADDITIVE
    SELECT (lcTmpDbt)
    SET RELATION TO IIF(CFILE_NUM = '3', &lcTempHdr..Invoice, '*') INTO ARINSTMD
    SET RELATION TO IIF(CFILE_NUM = '1', &lcTempHdr..Invoice, '*') INTO (lcTempLine) ADDITIVE
  ELSE
    SELECT (lcTempHdr)
    SET RELATION TO &lcTempHdr..Invoice INTO (lcTempLine) ADDITIVE
  ENDIF
ELSE
  SELECT (lcTempHdr)
  SET RELATION TO &lcTempHdr..Invoice INTO (lcTempLine) ADDITIVE
ENDIF
*wab

SELECT (lcTempLine)
SET RELATION TO IIF(!EMPTY(&lcTempLine..ALTSTYLE) , &lcTempLine..ALTSTYLE ,&lcTempLine..Style) INTO STYLE
SET RELATION TO "S" + &lcTempLine..Account + &lcTempLine..Style INTO SPCK_LIN ADDITIVE


SELECT STYLE
SET ORDER TO TAG SCALE IN SCALE
SET RELATION TO 'S' + Scale INTO SCALE ADDITVE

SELECT CUSTOMER
SET ORDER TO TAG CUSTOMER

SELECT (lcTempHdr)
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
IF !llIsAparel
  SET SKIP TO (lcTempLine) , SPCK_LIN
ENDIF
SET RELATION TO 'O' +&lcTempHdr..order INTO Ordhdr ADDITIVE


IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
    SET SKIP TO (lcTmpDbt) , (lcTempLine) , ARINSTMD
  ENDIF
ELSE
  IF !llIsAparel
    SET SKIP TO (lcTempLine)
  ENDIF
ENDIF


SELECT (lcTempHdr)
LOCATE

lcExp = IIF(lcInvKind = "S",lcRpExp , ".T.")
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcExp

IF gcDevice <> 'SCREEN'
  SELECT IIF(lcInvKind = "S",'INVHDR','INVHDRS')
  SCAN FOR &lcRpExp
    REPLACE PRTFLAG WITH 'P'
  ENDSCAN
ENDIF
SELECT STYLE
SET RELATION OFF INTO SCALE
SELECT (lcTmpDbt)
SET RELATION OFF INTO ARINSTMD
SELECT (lcTempHdr)
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr

IF USED(LCInvLine_A)
   USE IN (LCInvLine_A)
ENDIF

SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR
SELECT IIF(lcInvKind = "S",'INVHDR','INVHDRS')
LOCATE

*!*************************************************************
*! Name      : lfchangeAdr
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose   : Function change the customer address
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfchangeAdr
PARAMETERS lcRetrn
PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec

lnInvHdRec = IIF(EOF(lcTempHdr) , 0 , RECNO(lcTempHdr))
lnInvLnRec = IIF(EOF(lcTempLine) , 0 , RECNO(lcTempLine))
lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

*-- (LCTMPDBT) , ARINSTMD
IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
  lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
ELSE
  lnTmpDbt   = 0
  lnARINSTMD = 0
ENDIF
lnLineRec = IIF(EOF(lctempLine) , 0 , RECNO(lcTempLine))
lnHrRc    = IIF(EOF(lcTempHdr) , 0 , RECNO(lcTempHdr))
COUNT TO lnLines WHILE EVAL(lcTempLine+'.INVOICE') = EVAL(lcTempHDr+'.INVOICE')
IF lnInvLnRec > 0
  GO (lnLineRec) IN (lcTempLine)
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN (lcTempHDr)
ENDIF

*-- Fill laFactor with factor address
IF !EMPTY(EVAL(lcTempHDr+'.CFACCODE'))
  =SEEK(EVAL(lcTempHDr+'.CFACCODE'),'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(&lcTempHdr..cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(&lcTempHdr..ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(&lcTempHdr..cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

SELECT (lcTempHdr)
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER


IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    *-- DC Code
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF
  lcShpTName  = IIF(&lcTempHdr..STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  
  laShipTo[1] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))


  *put the file pointer back to the store because the only information
  *we need from the DC record is the addresss.
  IF lnCUSRec <> 0 
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcTempHdr)

IF lnTmpDbt <> 0
  GO lnTmpDbt IN (lcTmpDbt)
ENDIF
IF lnARINSTMD <> 0
  GO lnARINSTMD IN ARINSTMD
ENDIF

*-- Restore the old record pointer in INVLINE
IF lnInvLnRec = 0
  GO BOTTOM IN (lcTempLine)
  IF !EOF(lcTempLine)
    SKIP IN (lcTempLine)
  ENDIF
ELSE
  GO lnInvLnRec IN (lcTempLine)
ENDIF

*-- Restore the old record pointer in SPCK_LIN
IF lnPakLnRec = 0
  GO BOTTOM IN SPCK_LIN
  IF !EOF('SPCK_LIN')
    SKIP IN SPCK_LIN
  ENDIF
ELSE
  GO lnPakLnRec IN SPCK_LIN
ENDIF

*-- (LCTMPDBT) , ARINSTMD
RETURN ''


*!*************************************************************
*! Name      : lfClrRead
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose   : Function to delete the invoice option and clear the read.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfClrRead
PARAMETER LCDUMMY

IF lcInvKind = "P"
  For lnLenAry =  1 TO ALEN(laOGVrFlt,1)
    IF ASUBSCRIPT(laOgVrFlt,ASCAN(laOgVrFlt,'INVHDR.INVOICE'),1) = lnLenAry
      DIMENSION laCopyAry[ALEN(laOGVrFlt,1),ALEN(laOGVrFlt,2)]
      =ACOPY(laOGVrFlt , laCopyAry)
      =ADEL(laOGVrFlt , lnLenAry)
      DIMENSION laOGVrFlt[ALEN(laOGVrFlt,1)-1,ALEN(laOGVrFlt,2)]
      EXIT
    ENDIF
  ENDFOR
ELSE
  DIMENSION laOGVrFlt[ALEN(laCopyAry,1),ALEN(laCopyAry,2)]
  =ACOPY(laCopyAry , laOGVrFlt)
ENDIF
IF !USED('INVHDRS')
  =gfOpenFile(gcDataDir + "INVHDRS" , gcDataDir + "INVHDRS" ,'SH')
ENDIF
IF !USED('INVLINES')
  =gfOpenFile(gcDataDir + "INVLINES" , gcDataDir + "INVLINES" ,'SH')
ENDIF
CLEAR READ
LCDUMMY = .T.
RETURN LCDUMMY

*--End of lfClrRead.

*!*************************************************************
*! Name      : lfvInvNoSd
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvInvNoSd
PARAMETER LCDUMMY

PRIVATE lcAliasSty

lcAliasSty = ALIAS()
STORE SPACE(0) TO lcExpInv , lcInvoicSd
STORE 0 TO lnStyColor

lcBrowFlds = [cSadInv   :H = 'Invoice'     :10 ,]  +;
             [Printed =IIF(PrtFlag = 'P' , 'Yes' , 'No')  :H = 'Printed' : 10 ,]    + ;
             [InvDate   :H = 'Date'        :10 ,]  +;
             [Account   :H = 'Account'     :10 ,]  +;
             [CustPO    :H = 'Reference'   :15 ,]  +;
             [Order     :H = 'Order'       :10 ,]  +;
             [Rep1      :H = 'Sales Rep.'  :15 ,]  +;
             [Ship      :H = 'Pieces'      : 7 ,]  +;
             [ShipAmt   :H = 'Merchandise' :15  ]

SELECT INVHDRS
lcExpInv = 'FOR ' + "INVHDRS.STATUS <> 'V' AND EMPTY(INVHDRS.FLAG)"
=gfrange(lcBrowFlds,lcTempInv,"cSadInv",(lcExpInv),"","","@! XXXXXX")

DIMENSION laStylFle[1,7]
laStylFle[1,1]= "INVHDRS.CSADINV"
laStylFle[1,2]= "F"
laStylFle[1,3]= "C"
laStylFle[1,4]= .T.
laStylFle[1,5]= "In List             "
laStylFle[1,6]= lcTempInv
laStylFle[1,7]= "R"

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laBrFldFlt[2,ALEN(laBrTmpFlt,2)]
laBrFldFlt = ""
laBrFldFlt[2,1] = "INVHDRS"
laBrFldFlt[2,2] = lcBrowFlds
laBrFldFlt[2,3] = ""
laBrFldFlt[2,4] = ""
laBrFldFlt[2,5] = "laStylFle:1"

lcInvoicSd = gfGenFlt('laStylFle',.T.,.T.)
DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]
=ACOPY(laBrTmpFlt,laBrFldFlt)

SELECT INVHDRS
LOCATE
SELECT (lcAliasSty)

LCDUMMY = .T.
RETURN LCDUMMY

*------------------------
FUNCTION lfCalcTax

lnTaxable = lnTaxable + (&lcTempHdr..TAX_RATE * &lcTempLine..PRICE)
RETURN lnTaxable

*!***************************************************************
*! Name : lpPrtSku.
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the style/color Skus for a specific account.
*G000000,1 TAK 07/20/95.
*!***************************************************************
FUNCTION lfPrtSku

PRIVATE lnPrevAl
STORE ' ' TO lcStrToPrn

STORE '' TO laSku

IF ! SEEK('S'+&lcTempLine..Account+&lcTempLine..Style,'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF
lnPrevAl = SELECT (0)
SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  *=SEEK('S'+Style.Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF

  IF llExtSize
    =SEEK(TYPE+ACCOUNT+SUBSTR(Style,1,LEN(ALLTRIM(Style))-4),'SPCK_HDR')
  ELSE
    =SEEK(TYPE+ACCOUNT+STYLE,'SPCK_HDR')
  ENDIF
    
  lnDime1 = MIN(lnDime1,LEN(ALLTRIM(SPCK_HDR.SKU)))
   SCAN WHILE Type+Account+Style = 'S'+&lcTempLine..Account+&lcTempLine..Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku[lnX]=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
  =SEEK('S'+&lcTempLine..Account+&lcTempLine..Style,'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+&lcTempLine..Account+&lcTempLine..Style .AND. lnI < 9
    lcStrToPrn = 'SKU # ' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+&lcTempLine..Account+&lcTempLine..Style .AND. !EOF()
      lcI = STR(lnI,1)
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
  ENDDO  
ENDIF
SELECT (lnPrevAl)
IF EMPTY(lcStrToPrn)
  STORE '' TO laSku
  llPrtSku = .F.
  RETURN .F.
ELSE
  llPrtSku = .T.
  RETURN .T.
ENDIF
*!*************************************************************
*! Name      : lfSuprSKU
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose   : Function to determine wheather the skus option in 
*!           : the option grid will appear or not .
*!*************************************************************
*! Called from : Suppress field in the FORMCDDT file.
*!*************************************************************

FUNCTION lfSuprSKU

*-- If we do not want the SKUs options to be browsed in the option grid
*-- Flase the two SKUs variables to be used in the Forms programs.
IF !(TYPE('lcPrntSKU') = 'C' .AND. lcPrntSKU='Y')
  STORE .F. TO llRpPrtSku,llRpSkuSize
ENDIF
RETURN (TYPE('lcPrntSKU') = 'C' .AND. lcPrntSKU='Y')

*!*************************************************************
*! Name      : lfPrtNotes
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose   : Function to Evaluate Notes To be Printed 
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA,ARPINVZ .FRX (Notes Expression)
*!*************************************************************
FUNCTION lfPrtNotes
PRIVATE lcReturn
DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. SEEK('C' + &lcTempHdr..Invoice , 'NOTEPAD')

    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)
  CASE llRpInvLNt .AND. !EMPTY(&lcTempLine..note_mem) .AND. LEFT(ALLTRIM(STRTRAN(&lcTempLine..note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(&lcTempLine..note_mem)
  
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfPrtLNotes
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose   : Function to Evaluate Line Notes Only To be Printed 
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA.FRX (Dos Format)
*!*************************************************************

FUNCTION lfPrtLNotes

PRIVATE lcReturn

DO CASE
  CASE llRpInvLNt .AND. !EMPTY(&lcTempLine..note_mem) .AND. LEFT(ALLTRIM(STRTRAN(&lcTempLine..note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(&lcTempLine..note_mem)
  
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)


*!*************************************************************
*! Name        : lfSclSze
*! Developer   : Walid A. Wahab (WAB)
*! Date        : 04/07/2003
*! Purpose     : To get the scale and the size
*!*************************************************************
*! Called from : ARPINVL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSclSze

PRIVATE lcAlias  , lcX , lnCountSc , lcKey , lnSclCnt , lcScaleSz
lcOldInv   = &lcTempHdr..INVOICE
lcAlias = ALIAS()
SELECT (lcTempLine)
lcKey = Invoice+STR(LineNo,6)  
SEEK &lcTempHdr..Invoice
lnSclCnt = 0

STORE SPACE(0) TO laScale
SCAN WHILE Invoice = &lcTempHdr..Invoice  .AND. lnSclCnt < lcScalCont + 1
  IF ASCAN(laScale,&lcTempLine..SCALE) = 0 .AND. SEEK('S'+&lcTempLine..SCALE,'SCALE')
    lnSclCnt = lnSclCnt + 1
    laScale[lnSclCnt,1] = SCALE.Scale
    For lnCountSc = 1 TO 8 
      lcX = STR(lnCountSc,1)
      laScale[lnSclCnt,lnCountSc+1] = SCALE.SZ&lcX
    ENDFOR
  ENDIF
ENDSCAN    
SEEK(lcKey)     
SELECT (lcAlias)
RETURN ''
*!*************************************************************
*! Name        : lfEndLine
*! Developer   : Walid A. Wahab (WAB)
*! Date        : 04/07/2003
*! Purpose     : To get the Record count of the InvLine.DBF
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfEndLine
*!*************************************************************
FUNCTION lfEndLine
PRIVATE lcAlias
*lnInvRecNo      && Variable to hold the number of the last Invoice Lines per
                 && invoice and the variable in Syrepuvr.dbf
lcAlias = ALIAS()
lcOrder = Order()
SELECT (lcInvLine_A)
SET ORDER TO TAG (lcOrder) DESCENDING
=SEEK(&lcTempHdr..Invoice,lcInvLine_A)
lnInvRecNo = IIF(EOF(lcInvLine_A) , 0 , RECNO(lcInvLine_A))
lnInvRecNo = 3
SET ORDER TO TAG (lcOrder) ASCENDING
SELECT (lcAlias)
RETURN ''

*!**************************************************************************
*! Name      : lfGetSign
*! Developer : Walid A. Wahab (WAB)
*! Date      : 04/07/2003
*! Purpose   : Get the symbol of the used curr.
*!**************************************************************************
*! Example   : = lfGetSign()
*!**************************************************************************
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(&lcTempHdr..CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign
*-- End of lfGetSign
