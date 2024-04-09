***************************************************************************
*: PROGRAM   : ARPINVM.PRG (CNVERTED FOR 26 TO 27 FOR EILEEN FISHER)
*: MODULE    : Aria Apparel Series.
*: DATE      : 02/18/99
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: Refer to  : (C101396)
*:     Note  : For accounts with terms = 'SP'. Lines are printed with
*:             discounted price and total discounts .
*:*************************************************************************
*: Modifications      :
*B802750,1 ARH 11/02/99 Fixed the bug of sending blank pages to the printer 
*B802750,1              after finish printing the invoice
*B802848,1 ADEL 12/02/1999 Fix the bug of not deduct discount per lines for terms 'AA','ZZ'.
*C102549,1 NAD  02/14/2002 Add the Bill of lading to the invoice form 
*B605907,1 NAD  Print more than 11 characthers in the customer PO
*B605708,1 NAD  Display gets corrupted after pressing reset          
***************************************************************************

*-- If No records selected.
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  *B605708,1 NAD (Start)
  SET DEVICE TO SCREEN
  *BB605708,1 NAD (END)
  RETURN
ENDIF
*--Restore needed Tax flags.
llTax     = (gfGetMemVar('M_TAX') = 'Y')
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
lcTaxMeth = gfGetMemVar('M_TAX_METH')
*-- Get the style major and color.
*-Get the style major length
lnMajLen = LEN(gfItemMask('PM'))
*--Get the color
STORE 0 TO lnColorLen,lnNonMajSt
*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Get the Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lnNonMajSt = laMajSegs[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lnColorLen + laMajSegs[lnI-1,6] + laMajSegs[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
DECLARE laShpViaFld[1,2]
laShpViaFld[1,1] = 'CUPS'
laShpViaFld[1,2] = 'lcUpsType'
DIMENSION laAddress[1,1]
*-- Bill to AND *- Ship to Addresses.
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,laAddress
STORE '' TO lcInvoice,lcPhone,lcNote1,lcNote2
STORE 0.00 TO lnPieces, lnSubTot
llFirst = .T.
STORE SPACE(15) TO lcTerVCode,lcShiVCode,lcSpcInst
*-- End Variable declaration
SELECT INVHDR
*-- Browse the lineup message.
llLineUp  = gfModalGen('QRM40145B00006','DIALOG' ) = 1
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*------------------------------
* SECTION: MAIN LOOP
*------------------------------
SCAN WHILE INKEY() <> 32 FOR &lcRpExp
  WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
   *-----------------------------------------------------------------
   * Get and print invoice header.
   * Initialize document totals.
   *-----------------------------------------------------------------
   *-- Function to get invoice HEADER information.
   =lfGetInHdr()
   *-- Function to print invoice HEADER information.   
   =lfPrntHdr()
   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   *--Refreh the relation
   SELECT INVHDR
   GO RECNO()
   SELECT INVLINE
   ROW    = 24
   SCAN WHILE invoice+STR(lineno,6) = lcInvoice
    DO CASE
      CASE ROW >= 49
        @ ROW+1,12 SAY 'C O N T I N U E D ...'
        @ 56,05 SAY lcPhone + ' ' + lcNote1 + ' ' + lcNote2
        IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
          * @ 58,12 SAY lcRpMsg1
          @ 58,12 SAY ALLTRIM(lcRpMsg1)
          *C102549,1 NAD (Start)
          *@ 59,12 SAY lcRpMsg2
          @ 58,57 SAY  'B.O.L. # '+ ALLTRIM(InvHdr.BOL_NO)
          @ 59,12 SAY ALLTRIM(LEFT(lcRpMsg2,44))
          *C102549,1 NAD (END)
        ENDIF
        
        *C102549,1 NAD (Start)
        IF !EMPT(InvHdr.BOL_NO)
         * @ 59,57 SAY  'B.O.L. # '+ ALLTRIM(InvHdr.BOL_NO)
          *@ 58,57 SAY  'B.O.L. # '+ ALLTRIM(InvHdr.BOL_NO)
        ENDIF  
        *C102549,1 NAD   (End)
        @ 60,00 SAY INVHDR.PIKTKT
        @ 60,07 SAY SUBSTR(DTOC(INVDATE),1,5)
        @ 60,42 SAY '****'
        @ 60,48 SAY '*****'
        @ 60,76 SAY '******.**'
        =lfPrntHdr()
      CASE TOTQTY = 0
        LOOP
    ENDCASE
    *-- Say style 
    @ ROW,00 SAY SUBSTR(STYLE,1,lnMajLen)
    *-- Say color
    @ ROW,15 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    @ ROW,23 SAY SUBSTR(STYLE.DESC,1,13)
    @ ROW,37 SAY STYLE.SCALE
    @ ROW,39 SAY QTY1   PICTURE '@Z 999'
    @ ROW,43 SAY QTY2   PICTURE '@Z 999'
    @ ROW,47 SAY QTY3   PICTURE '@Z 999'
    @ ROW,51 SAY QTY4   PICTURE '@Z 999'
    @ ROW,55 SAY QTY5   PICTURE '@Z 999'
    @ ROW,59 SAY QTY6   PICTURE '@Z 999'
    @ ROW,64 SAY TOTQTY PICTURE '9999'
    * CUM INVOICE TOTALS
    *B802848,1 (Begin) Remark the following line and deduct discount for 'AA' and 'ZZ' ALSO.
    *IF (UPPER(INVHDR.cTermCode)='SP    ' .OR. UPPER(INVHDR.cTermCode)='BL    ')
    IF (UPPER(INVHDR.cTermCode)='SP    ' .OR. UPPER(INVHDR.cTermCode)='BL    ' .OR. UPPER(INVHDR.cTermCode)='AA    ' .OR. UPPER(INVHDR.cTermCode)='ZZ    ')
    *B802848,1 (End)
      lnLineTot = (PRICE-((INVHDR.DISCPCNT/100)*PRICE)) * TOTQTY
      @ ROW,69 SAY (PRICE-((INVHDR.DISCPCNT/100)*PRICE)) PICTURE '9999.99'
    ELSE
      lnLineTot = PRICE * TOTQTY
      @ ROW,69 SAY PRICE PICTURE '9999.99'
    ENDIF
    lnSubTot = lnSubTot + lnLineTot
    lnPieces = lnPieces + TOTQTY
    @ ROW,77  SAY lnLineTot  PICTURE '999999.99'
    ROW = ROW+1
    IF llLineUp AND llFirst
      EJECT
      SELECT INVHDR
      IF !BOF()
        SKIP-1
      ENDIF  
      llFirst = .F.
      EXIT
    ENDIF
    IF llLineUp
      *-- Browse 'Another lineup message'.
      lnChoice = gfModalGen('QRM40143B40012','DIALOG' )
      DO CASE
        CASE lnChoice = 3
          *B802750,1 ARH 11/02/99 (start) set the device back to screen
          SET DEVICE TO SCREEN
          *B802750,1 ARH 11/02/99 (End)
          RETURN
        CASE lnChoice = 2
          llLineUp =.F.
        OTHERWISE
          SELECT INVHDR
          IF !BOF()
            SKIP-1
          ENDIF  
          EXIT
      ENDCASE
    ENDIF
  ENDSCAN
  IF llLineUp
    LOOP
  ENDIF
  SELECT INVHDR
  ROW=ROW+1
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,63 SAY lnPieces            PICTURE '999999'
  @ ROW,77 SAY lnSubTot            PICTURE '999999.99'
  ROW=ROW+1
  *--Don't print the total discount if the term code of the invoice is "SP" or "BL" "AA" or "ZZ".
  IF DISCOUNT<>0 .AND. (UPPER(cTermCode)<>'SP' .AND. UPPER(cTermCode)<>'BL'.AND. UPPER(cTermCode)<>'AA'.AND. UPPER(cTermCode)<>'ZZ')   
    @ ROW,12 SAY 'TOTAL - D I S C O U N T'
    @ ROW,77 SAY DISCOUNT  PICTURE '999999.99'
    ROW=ROW+1
  ENDIF
  IF llTax .AND. lcTaxMeth = 'M' 
    lcRateStr = ALLTRIM(STR(TAX_RATE,5,2))
    @ ROW,12 SAY 'TOTAL - '+lcTaxDesc
    @ ROW,58 SAY lcRateStr + ' %'
    @ ROW,77 SAY TAX_AMT  PICTURE '999999.99'
    ROW = ROW + 1
  ENDIF  
  lnFrtInsu = FREIGHT + INSUR + COD
  IF lnFrtInsu <> 0
    @ ROW,12 SAY 'TOTAL - FREIGHT & INSURANCE'
    @ ROW,77 SAY lnFrtInsu  PICTURE '999999.99'
    ROW=ROW+1
  ENDIF
  IF llTax .AND. lcTaxMeth = 'A'
    lcRateStr = ALLTRIM(STR (TAX_RATE,5,2))
    @ ROW,12 SAY 'TOTAL - '+lcTaxDesc
    @ ROW,58 SAY lcRateStr + ' %'
    @ ROW,77 SAY TAX_AMT  PICTURE '999999.99'
    ROW = ROW + 1
  ENDIF  
  
  *C102549,1 NAD (Start)
  *@ 55,05 SAY lcPhone + ' ' + lcNote1 + ' ' + lcNote2
  @ 55,05 SAY ALLTRIM(lcPhone + ' ' + lcNote1 + ' ' + lcNote2)
  *C102549,1 NAD (End)
  *C102549,1 NAD (Start)
  IF !EMPT(InvHdr.BOL_NO)
     *@ 59,57 SAY  'B.O.L. # '+ ALLTRIM(InvHdr.BOL_NO)
      @ 58,57 SAY  'B.O.L. # '+ ALLTRIM(InvHdr.BOL_NO)
  ENDIF
  *C102549,1 NAD (End)
  @ 60,01 SAY PIKTKT
  @ 60,09 SAY SUBSTR(DTOC(INVDATE),1,5)
  @ 60,20 SAY lcShiVCode
  @ 60,42 SAY CARTONS                  PICTURE '@Z 999'
  @ 60,48 SAY WEIGHT                   PICTURE '@Z 99999'
  @ 60,76 SAY INVHDR.TOTALCHG         PICTURE '999999.99'
  @ 61,00 SAY ' '
  
  *C102549,1 NAD (Start)
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
  *C102549,1 NAD (End)

ENDSCAN
DO ENDREPORT

*B802750,1 ARH 11/02/99 (start) set the device back to screen
SET DEVICE TO SCREEN
*B802750,1 ARH 11/02/99 (End)

RETURN
*--------------------------------
*    END ARPINVM.PRG
*--------------------------------



*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/20/99
*! Purpose   : TO get the information of the invoice header.
*! Refer to  : (C101396)
*!*************************************************************
FUNCTION lfGetInHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,laAddress 
STORE 0.00 TO lnPieces, lnSubTot
lcInvoice  = Invoice
lcPhone    = IIF(EMPTY(PHONE),'',PHONE)
lcNote1    = IIF(NOTE1<>'*', NOTE1, '')
lcNote2    = IIF(NOTE2<>'*', NOTE2, '')
lcAccount  = ACCOUNT
lcStore    = STORE
*-- GET THE BILL TO AND SHIP ADDRESS
lcBtName = Customer.BtName
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess.
FOR lnCount = 1 TO ALEN(laAddress,1)-1
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
lcStName = IIF(EMPTY(Customer.Dba), Customer.StName, Customer.Dba)
=gfGetAdr('CUSTOMER','','','',1,'')
*--Get the Ship To adddess.
FOR lnCount = 1 TO ALEN(laAddress,1)-1
  lcCount = STR(laAddress[lnCount,1],1)
  lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR  
*-- GET THE DESCRIPTION ABOUT THE CODES
lcShiVCode = SUBSTR(gfCodDes(ShipVia ,PADR('SHIPVIA',10)),1,15)
lcTerVCode = gfCodDes(cTermCode,PADR('CTERMCODE',10))
STORE '' TO lcUpsType
=gfRltFld(ShipVia,@laShpViaFld,'SHIPVIA')
DO CASE
  CASE 'G' $ lcUpsType
    lcZone  = '('+INVHDR.UPSZONE+')'
  CASE '2' $ lcUpsType
    lcZone  = '(12)'
  CASE 'N' $ lcUpsType
    lcZone = '(22)'
  OTHERWISE
    lcZone  = ''
ENDCASE
lcShiVCode = lcShiVCode + lcZone
*--Special instruction
lcSpcInst = gfCodDes(INVHDR.SPCINST , 'SPCINST')
*--Do not get the [FACTOR] NAME & ADDRESS as the report does nor print it.
STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/20/99
*! Purpose   : TO print the invoice header.
*! Refer to  : (C101396)
*!*************************************************************
FUNCTION lfPrntHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR
@ 06,78 SAY lcInvoice
@ 09,08 SAY ShipDate
@ 09,18 SAY 'INVOICE :' + lcInvoice
@ 11,08 SAY lcBtName
@ 11,57 SAY lcStName
@ 12,08 SAY lcBtAdd1
@ 12,57 SAY lcStAdd1
@ 13,08 SAY lcBtAdd2
@ 13,57 SAY lcStAdd2
@ 14,08 SAY lcBtAdd3
@ 14,57 SAY lcStAdd3
@ 17,00 SAY ACCOUNT
@ 17,10 SAY ORDER
@ 17,20 SAY SUBSTR(lcTerVCode,1,15)
@ 17,36 SAY SUBSTR(lcShiVCode,1,15)
@ 17,52 SAY SUBSTR(STORE,1,4)
@ 17,58 SAY DEPT
@ 17,66 SAY REP1
@ 17,70 SAY REP2
*--Get 10 chrs form custpo as 27 is 15 chrs.
*B605907,1 NAD (Start) 
*@ 17,75 SAY SUBSTR(CUSTPO,1,10)
@ 17,74 SAY SUBSTR(CUSTPO,1,13)
*B605907,1 NAD (END) 
*-- PRINT SCALES

SELE SCALE
SEEK 'S'
ROW = 19
X   = 1
Y   = ' '
DO WHILE X <= 5 .AND. .NOT. EOF() .AND. TYPE='S'
  @ ROW,37 SAY SCALE+PADL(SUBSTR(ALLTRIM(SZ1),1,4),4)+;
                PADL(SUBSTR(ALLTRIM(SZ2),1,4),4)      +;
                PADL(SUBSTR(ALLTRIM(SZ3),1,4),4)      +;
                PADL(SUBSTR(ALLTRIM(SZ4),1,4),4)      +;
                PADL(SUBSTR(ALLTRIM(SZ5),1,4),4)
  ROW = ROW + 1
  X  = X + 1
  SKIP
ENDDO
SELECT (lcAlias)