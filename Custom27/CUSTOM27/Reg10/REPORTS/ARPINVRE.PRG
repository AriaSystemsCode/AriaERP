*:***************************************************************************
*: Program file  : ARPINVRE.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR REGAL ORIGINALS. (REG100)
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C101122)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" 
*! Date          : 11/15/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102040
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....gfGetZone , lFPrtSku
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVRE
*:***************************************************************************
*:MODIFICATIONS:
*:B804283,1 AME 06/25/2001  fix the bug of prrinting address and name of the first customer
*:B804283,1 AME             in all the invoices.
*:***************************************************************************
           *--Section of Initializing the variables --*
llNot_Loop = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

llTax      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)
lnTax_Desc = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
lnTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lcTax_Meth = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
llNewDoc   = .T.
lnMaxRow   = 47
llIsCanada = IIF(gcContcode = 'CANADA', .T., .F.)

           *--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
STORE TRIM(laCompAdd[4]) TO HLINE5
IF EMPTY(HLINE3)
  STORE HLINE5 TO HLINE4
  STORE ''     TO HLINE5
ENDIF

               *--Section to get the FromZone --*
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'
                  *--End Section to get the FromZone --*
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT
SCAN FOR &lcASExp
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  lcInvoice = INVOICE

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF llNewDoc
    STORE 0.00 TO lnPieces, lnSubTotal
    llDiscount = .T.
    llNewDoc   = .F.
    lcOrder    = ORDER
    lcPhone    = INVHDR.PHONE
    lcNote1    = IIF(NOTE1<>'*', NOTE1, '')
    lcNote2    = IIF(NOTE2<>'*', NOTE2, '')
    lcPikTkt   = PIKTKT
    lcAccount  = ACCOUNT
    lcStore    = STORE
    lcSeason   = SEASON
    lcDivision = CDIVISION
    
    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(lcStore = SPACE(8),'M'+lcAccount,'S'+lcAccount+lcStore)

    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
    lcBtName  = lcSolTName
    lcBtAddr1 = laSoldTo[1]
    lcBtAddr2 = laSoldTo[2]
    lcBtAddr3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
       lcBtAddr2 = laSoldTo[3]
       lcBtAddr3 = ''
    ENDIF

    lcStName  = lcShpTName
    lcStAddr1 = laShipTo[1]
    lcStAddr2 = laShipTo[2]
    lcStAddr3 = TRIM(laShipTo[3])
    IF LEN(TRIM(laShipTo[2])) =0
      lcStAddr2 = laShipTo[3]
      lcStAddr3 = ''
    ENDIF

    *** GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    lcPTerms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcPShipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    lcDvsion   = gfCodDes(INVHDR.CDIVISION,'CDIVISION')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
    
    DO CASE
      CASE 'G' $ lcUpsType
        XZN = gfGetZone(lcUpsType,lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))
        XZN = IIF(!EMPTY(XZN),'('+ XZN +')' ,XZN)
      CASE '2' $ lcUpsType
        XZN  = '(12)'
      CASE 'N' $ lcUpsType
        XZN  = '(22)'
      OTHERWISE
        XZN  = ''
    ENDCASE

    lcPShipVia = IIF(TRIM(lcPShipVia) = "N/A" , "" ,TRIM(lcPShipVia)) +XZN
    lcCurLoop = '1'
  ENDIF
  
  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()

  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
   SELECT INVLINE
   lcStore = STORE
   lnRow    = 27
   DO WHILE lcCurLoop = '1' .AND. !llNot_Loop
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> lcInvoice .OR. lnRow >= lnMaxRow
        IF lnRow >= lnMaxRow
          IF INVOICE <> lcInvoice
            EXIT
          ENDIF
          =lfGetcont()
          =lfGetInHdr()
          LOOP
        ENDIF
         EXIT
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF

      SELECT INVLINE
      lcStyDesc = IIF(EMPTY(STYLE.DESC) , SPACE(0) , SUBSTR(STYLE.DESC,1,17))
      @ lnRow,01 SAY LEFT(STYLE,12)
      @ lnRow,13 SAY SUBSTR(STYLE,14,6)
      @ lnRow,19 SAY LEFT(STYLE.SCALE,2)
      @ lnRow,21 SAY QTY1   PICTURE '@Z 99999'
      @ lnRow,26 SAY QTY2   PICTURE '@Z 99999'
      @ lnRow,31 SAY QTY3   PICTURE '@Z 99999'
      @ lnRow,36 SAY QTY4   PICTURE '@Z 99999'
      @ lnRow,41 SAY QTY5   PICTURE '@Z 99999'
      @ lnRow,46 SAY QTY6   PICTURE '@Z 99999'
      @ lnRow,51 SAY QTY7   PICTURE '@Z 99999'
      @ lnRow,56 SAY QTY8   PICTURE '@Z 99999'
      @ lnRow,61 SAY TOTQTY PICTURE '999999'

      *-- Sum INVOICE totals
      llDiscount = .T.
      lnNetPrice  = ABS((InvLine.PRICE - ((InvLine.PRICE)*(InvHdr.DiscPcnt)/100)))
      lnPieces    = lnPieces + TOTQTY

      *--Check if there is want to subtract (MERCHANDISE - DISCOUNT) and the user want to print it
      *--(llRpdisRE) then deduct the merchandise discount for each individual
      *--line item selling price and llDiscount will be .F. to prevent printing
      *--(DISCOUNT) at the footer of the invoice 
      IF (InvHdr.Discount<>0) AND llRpdisRE
        llDiscount  = .F.
      ENDIF
      lnLineTot = TOTQTY * IIF(llDiscount,PRICE,lnNetPrice)
      lnSubTotal  = lnSubTotal+lnLineTot
      @ lnRow,67  SAY IIF(llDiscount,PRICE,lnNetPrice) PICTURE '9999.99'
      @ lnRow,75  SAY lnLineTot  PICTURE '999999.99'
      lnRow = lnRow+1

     *--Procedure to print the Style/Color Sku no.
      =lFPrtSku()
      @ lnRow,1 SAY lcStyDesc
      lnRow=lnRow+1
               
      *-- GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  lcEndPage = IIF(INVLINE.INVOICE = lcInvoice ,'1','0')
  IF lcEndPage = '1' .AND. !llNot_Loop
    =lfGetcont()
    LOOP
  ENDIF

  *** Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + Key <> 'C' + lcInvoice
      SEEK 'C' + lcInvoice
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + Key = 'C' + lcInvoice
      @ lnRow,02 SAY '* -- N O T E S -- *'
      lnRow = lnRow + 1 

      DO WHILE lnNotLine <= lnMemLins
        IF lnRow >= lnMaxRow
          llNot_Loop = .T.
        ELSE
          llNot_Loop = .F.
          @ lnRow,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          lnRow = lnRow + 1
        ENDIF
        IF lnRow >= lnMaxRow
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !llNot_Loop
        @ lnRow,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        lnRow = lnRow + 1 
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
      SET MEMOWIDTH TO lnOldMemW
    ENDIF
  ENDIF
   
  @ 50,07 SAY lcPhone SIZE 1,16
   
  SELECT INVHDR
  @ 50,55 SAY 'MERCHANDISE'
  @ 50,74 SAY lnSubTotal            PICTURE '9999999.99'
   
  lnRow = 51
  @ 51,07 SAY lcNote1
  
  IF llTax .AND. lcTax_Meth = 'M'
    IF !EMPTY(lcTaxRefr)
      @ lnRow,55 SAY lcTaxRefr
      lnRow = lnRow + 1
    ENDIF
    lcString_Rate = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ lnRow ,55 SAY SUBSTR(lnTax_Desc,1,10)+' :'+lcString_Rate + ' %'
    @ lnRow ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
    lnRow = lnRow + 1
  ENDIF
  
  IF lnRow = 53
    @ 53,07 SAY lcNote2
    llNotePrnt = .T.
  ELSE
    llNotePrnt = .F.
  ENDIF
  lnAmt = FREIGHT + INSUR + COD
  IF lnAmt <> 0
     @ lnRow,55 SAY 'TOTAL - FREIGHT'
     @ lnRow,74 SAY lnAmt       PICTURE '9999999.99'
     lnRow=lnRow+1
  ENDIF
  
  *--Check if not deduct the merchandise discount for each individual line
  *--i.e llDiscount is true and (DISCOUNT<>0) print the DISCOUNT at invoice
  *--footer
  IF DISCOUNT<>0 AND llDiscount
     @ lnRow,55 SAY 'DISCOUNT'
     @ lnRow,73 SAY  DISCOUNT    PICTURE  '99999999.99'
  ENDIF

  IF !llNotePrnt
    @ 53,07 SAY lcNote2
  ENDIF

  IF llTax .AND. lcTax_Meth = 'A'
    @ 54,55 SAY lcTaxRefr
    lcString_Rate = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ 55 ,55 SAY SUBSTR(lnTax_Desc,1,10)+' :'+lcString_Rate + ' %'
    @ 55 ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
  ENDIF
  
  IF llTax .AND. llIsCanada
    lcStrRate = ALLTRIM(STR(InvHdr.nPstRate,5,2))
    lnRow1 = IIF (lcTax_Meth='A',56,55)
    @ lnRow1 ,55 SAY 'PST TAX    :' + lcStrRate + ' %'
    @ lnRow1 ,74 SAY InvHdr.nPstAmt PICTURE '9999999.99'
  ENDIF
  @ 57,18 SAY INVHDR.CARTONS   PICTURE '@Z 999'
  @ 57,62 SAY lnPieces           PICTURE '9999999'
  @ 57,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'
  
  SELECT INVHDR
  IF EOF()
    *B804283,1 AME [Start] In case of last record
    *NewDoc = .F.
    llNewDoc = .F.
    *B804283,1 AME [End]
    SET DEVICE TO SCREEN
    RETURN
  ELSE
    *B804283,1 AME [Start] Make the llNewdoc True for the next invoice.
    *NewDoc = .T.
    llNewDoc = .T.
    *B804283,1 AME [End]
  ENDIF

 IF gcDevice <> 'SCREEN'
   INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
 ENDIF

ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 11/15/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVRE.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,lcSyszone),'',&lcSyszone..ZONE)

*--End of gfGetZone.
*!*************************************************************
*! Name      : lFPrtSku
*! Developer : BASSEM RAFAAT 
*! Date      : 11/15/2000
*! Purpose   : Print the style/color Skus for a specific account.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lFPrtSku()
*!*************************************************************
FUNCTION lFPrtSku

IF ! SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  =SEEK('S'+Style.Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8                    &&Default
    lnDime2 = 8                    &&Default
  ENDIF

  DIMENSION laSku[8]
  laSku = SPACE(16)
  SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  
  lnI = 1
  = SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()
      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
    @ lnRow,1 SAY lcStrToPrn
    lnRow = lnRow + 1
  ENDDO
ELSE
  @ lnRow,1 SAY Sku
  @ lnRow,16 SAY 'CUSTOMER SKU #'
  lnRow=lnRow+1
ENDIF
RETURN

*--End of lFPrtSku.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 11/15/2000
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetInHdr()
*!*************************************************************
FUNCTION lfGetInHdr

SELECT INVHDR
@ 01,60 SAY lcInvoice
@ 01,72 SAY INVDATE
  
IF llPrntComp
  @ 03,05 SAY HLine1
ENDIF
@ 03,72 SAY SUBSTR( lcDvsion, 1,14)
  
IF llPrntComp
  @ 04,05 SAY HLINE2
ENDIF

IF llPrntComp
   @ 05,05 SAY HLINE3
ENDIF
@ 05,72 SAY ORDER

IF llPrntComp
   @ 06,05 SAY HLINE4
ENDIF
   
IF llPrntComp
  @ 07,05 SAY HLINE5 SIZE 1,16
ENDIF
@ 07,72 SAY lcPikTkt
@ 09,72 SAY APPROVAL
@ 13,09 SAY lcBtName
@ 13,55 SAY lcStName
@ 14,09 SAY lcBtAddr1
@ 14,55 SAY lcStAddr1
@ 15,09 SAY lcBtAddr2
@ 15,55 SAY lcStAddr2
@ 16,09 SAY lcBtAddr3
@ 16,55 SAY lcStAddr3
@ 21,02 SAY lcAccount
@ 21,13 SAY LEFT(CUSTPO,10)
@ 21,24 SAY lcStore
@ 21,34 SAY DEPT
@ 21,42 SAY LEFT(lcPTerms,14)
@ 21,64 SAY REP1
@ 21,67 SAY REP2
@ 21,71 SAY SUBSTR(lcPShipVia,1,14)
     
lnRow = 23

*--Section for the scale.
SELECT Invline
IF !llNot_Loop
  lnRec1=RECNO()
  lcTScale1=" "
  lcTScale2=" "
  DO WHILE .T.
    IF Invoice <> lcInvoice .or. (lcTScale1<>" " .and. lcTScale2<>" ")
      EXIT
    ENDIF
    IF lcTScale1=" "
      lcTScale1=scale
    ENDIF
    IF scale<>lcTScale1
      lcTScale2=scale
    ENDIF
    SKIP
  ENDDO
  IF BETWEEN(lnRec1,1,RECCOUNT('Invline'))
    GOTO lnRec1
  ENDIF
ENDIF

SELECT SCALE
lnRec1 = RECNO()
lnCount = 49
DO WHILE lnCount<51
  lnIndex = chr(lnCount)
  IF SEEK ('S'+lcTScale&lnIndex)
    @ lnRow,19 SAY SCALE+PADL(SZ1,5,' ') + PADL(SZ2,5,' ')+;
                         PADL(SZ3,5,' ') + PADL(SZ4,5,' ')+;
                         PADL(SZ5,5,' ') + PADL(SZ6,5,' ')+;
                         PADL(SZ7,5,' ') + PADL(SZ8,5,' ')
    lnRow = lnRow+1
  ENDIF
  lnCount = lnCount+1
ENDDO
IF BETWEEN(lnRec1,1,RECCOUNT('SCALE'))
  GOTO lnRec1
ENDIF

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 11/15/2000
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfGetcont

@ lnRow+1,12 SAY 'C O N T I N U E D ...'
SELECT INVHDR
@ 50,07 SAY lcPhone SIZE 1,16
@ 50,55 SAY 'MERCHANDISE'
@ 50,75 SAY '******'
lnRow=lnRow+1
@ 51,07 SAY lcNote1
@ 53,07 SAY lcNote2
@ 57,18 SAY INVHDR.CARTONS   PICTURE '@Z 999'
@ 57,62 SAY '******'
@ 57,75 SAY '******'

*--End of lfGetcont.