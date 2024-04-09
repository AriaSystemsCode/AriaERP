***************************************************************************
* PROG      : ARPINVJW.PRG (FOR J.W.E SILK) Converted from 26 to 27.
* DEVELOPER : Adel Mohammed El Gazzar (ADEL)
* DATE      : 04/26/1999
* DESC      : PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 5 SIZE SCALES 
* Refer to  : (C101473)
*!****************************************************************************
*: Calls :   Functions :
*:                       gfModalGen()
*:                       gfGetMemVar()
*:                       gfItemMask()
*:                       gfPhoneTem()
*:                       gfGetAdr()
*:                       gfCodDes()
*:                       gfRltFld()
*:                       lfGetInHdr()
*:                       lfPrntHdr()
*:          Procedures   lpPrtSku
*:**************************************************************************


*-- If No records selected.
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*--Restore needed Tax flags.
llTax     = (gfGetMemVar('M_TAX') = 'Y')
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
lcTaxMeth = gfGetMemVar('M_TAX_METH')
lcTaxRefr = SUBSTR(gfGetMemVar('M_TAX_REFE'),1,18)
*-- Indecate either we are dealing with a canadian company or not.
llIsCanada = (UPPER(ALLTRIM(gcContCode)) = 'CANADA')
*---Get the style major and color.
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
llNoteLoop = .F.
lnMaxRow = 47
*--- Initilize the distribution center var.
lnNotLine = 1
DIMENSION laAddress[1,1]
*-- Bill to AND *- Ship to Addresses.
STORE ' ' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcDist
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,laAddress
STORE '' TO lcInvoice,lcAccount,lcStore
STORE 0.00 TO lnPieces, lnSubTot
DECLARE laShpViaFld[1,2]
laShpViaFld[1,1] = 'CUPS'
laShpViaFld[1,2] = 'lcUpsType'
DECLARE laDivLName[1,2]
*laDivLName[1,1] = 'CDIVISION'
*laDivLName[1,2] = 'lcDivLName'
=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
llFirst = .T.
STORE SPACE(15) TO lcTerVCode,lcShiVCode,lcDivDesc
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
   *-----------------------------------------------------------------
   * Get and print invoice header.
   *-----------------------------------------------------------------
   *-- Function to get invoice HEADER information.
   =lfGetInHdr()
   *-- Function to print invoice HEADER information.   
   =lfPrntHdr()
   *---------------------------------------------------------------
   * LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   ROW    = 27
   DO WHILE !llNoteLoop
     SELECT INVLINE
     DO CASE
       CASE EOF().OR. INVOICE <> lcInvoice
         EXIT
       CASE TOTQTY = 0 
         SKIP
         LOOP
       CASE ROW >= lnMaxRow
         @ ROW+1,12 SAY 'C O N T I N U E D ...'
         SELECT INVHDR
         @ 50,01 SAY Phone  PICTURE gfPhoneTem() SIZE 1,16 
         @ 50,55 SAY 'MERCHANDISE'
         @ 50,77 SAY '******'
         ROW=ROW+1
         @ 51,07 SAY NOTE1
         @ 53,07 SAY NOTE2
         @ 58,18 SAY INVHDR.CARTONS  PICTURE '@Z 999'
         @ 58,62 SAY '******'
         @ 58,75 SAY '******'
         *-- Function to print invoice HEADER information.   
         =lfPrntHdr()
       ENDCASE
       SELECT INVLINE
       @ ROW,00 SAY STYLE
       lcColDesc = SUBSTR(gfCodDes(SUBSTR(STYLE,lnNonMajSt,lnColorLen),'COLOR'),1,9)
       @ ROW,19 SAY lcColDesc
       @ ROW,29 SAY SCALE
       @ ROW,31 SAY QTY1   PICTURE '@Z 999'
       @ ROW,35 SAY QTY2   PICTURE '@Z 999'
       @ ROW,39 SAY QTY3   PICTURE '@Z 999'
       @ ROW,43 SAY QTY4   PICTURE '@Z 999'      
       @ ROW,47 SAY QTY5   PICTURE '@Z 999'
       @ ROW,51 SAY QTY6   PICTURE '@Z 999'
       @ ROW,55 SAY QTY7   PICTURE '@Z 999'
       @ ROW,59 SAY QTY8   PICTURE '@Z 999'
       @ ROW,63 SAY TOTQTY PICTURE '9999'
       *-- CUM INVOICE TOTALS
       lnLinTot = PRICE * TOTQTY
       lnPieces = lnPieces + TOTQTY
       lnSubTot = lnSubTot+lnLinTot
       @ ROW,66  SAY PRICE     PICTURE '9999.99'
       @ ROW,74  SAY lnLinTot  PICTURE '999999.99'
       ROW = ROW+1
       DO lpPrtSku
       @ ROW,1 SAY STYLE.DESC
       ROW = ROW+1      
       IF llLineUp
         EXIT
       ENDIF
       SELECT INVLINE
       SKIP
    ENDDO
    IF llLineUp .AND. !llNoteLoop
      *-- 'Another lineup '.
      lnChoice = gfModalGen('QRM40143B40012','DIALOG' )
      DO CASE
        CASE lnChoice = 3
          RETURN
        CASE lnChoice = 2
          llLineUp =.F.
      ENDCASE
      STORE 0.00 TO lnPieces, lnSubTot
      SELECT INVHDR
      IF !BOF()
        GO TOP 
      ENDIF
      LOOP
    ENDIF
    IF llRpInvNot
      =SEEK('C' + lcInvoice,'NOTEPAD')
      IF !EOF('NOTEPAD')
        SELECT NOTEPAD
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
        @ ROW,02 SAY '* -- N O T E S -- *' 
        ROW = ROW + 1 
        DO WHILE lnNotLine <= lnMemLins
          IF ROW >= lnMaxRow
            llNoteLoop = .T.
            SELECT INVHDR
            @ ROW+1,12 SAY 'C O N T I N U E D ...'
            @ 50,07 SAY Phone  PICTURE gfPhoneTem() SIZE 1,16
            @ 50,55 SAY 'MERCHANDISE'
            @ 50,77 SAY '******'
            ROW = ROW + 1
            @ 51,07 SAY NOTE1
            @ 53,07 SAY NOTE2
            @ 58,18 SAY CARTONS PICTURE '@Z 999'
            @ 58,62 SAY '******'
            @ 58,75 SAY '******'
            *-- Function to print invoice HEADER information.   
            =lfPrntHdr()
            SELECT NOTEPAD
            LOOP
          ELSE
            llNoteLoop = .F.
            @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
            ROW = ROW + 1
          ENDIF
          lnNotLine = lnNotLine + 1
        ENDDO
        IF !llNoteLoop
          @ ROW,02 SAY '* -- END OF NOTES -- *'
          lnNotLine = 1
          ROW = ROW + 1 
        ENDIF
        SET MEMOWIDTH TO lnOldMemW        
      ENDIF
    ENDIF
    SELECT INVHDR
    @ 50,07 SAY PHONE  PICTURE gfPhoneTem() SIZE 1,16
    @ 50,55 SAY 'MERCHANDISE'
    @ 50,73 SAY lnSubTot   PICTURE '9999999.99'  
    ROW=51
    @ 51,07 SAY NOTE1
    IF llTax .AND. lcTaxMeth = 'M' 
      IF !EMPTY(lcTaxRefr)
        @ Row,55 SAY lcTaxRefr
        Row = Row + 1
      ENDIF  
      XSTRING_RATE = ALLTRIM(STR (TAX_RATE,5,2))
      @ ROW ,55 SAY SUBSTR(lcTaxDesc,1,10)+' :'+XSTRING_RATE + ' %'
      @ ROW ,73 SAY TAX_AMT   PICT '9999999.99'
      ROW = ROW + 1
    ENDIF  
    IF ROW = 53
      @ 53,07 SAY NOTE2
      llNotePrnt = .T.
    ELSE
      llNotePrnt = .F.  
    ENDIF  
    WKAMT = FREIGHT + INSUR + COD
    IF WKAMT <> 0
       @ ROW,55 SAY 'TOTAL - FREIGHT'
       @ ROW,73 SAY WKAMT       PICTURE '9999999.99'  
       ROW=ROW+1
    ENDIF
    IF DISCOUNT<>0
       @ ROW,55 SAY 'DISCOUNT'
       @ ROW,72 SAY DISCOUNT    PICTURE  '99999999.99'   
    ENDIF
    IF !llNotePrnt
      @ 53,07 SAY NOTE2
    ENDIF
    IF llTax .AND. lcTaxMeth = 'A'
      @ 54,55 SAY lcTaxRefr
      XSTRING_RATE = ALLTRIM(STR (TAX_RATE,5,2))
      @ 55 ,55 SAY SUBSTR(lcTaxDesc,1,10)+' :'+XSTRING_RATE + ' %'
      @ 55 ,73 SAY TAX_AMT   PICT '9999999.99'
    ENDIF  
    IF llTax .AND. llIsCanada 
      lcStrRate = ALLTRIM(STR(nPstRate,5,2))
      lnRow = IIF (lcTaxMeth='A',56,55)
      @ lnRow ,55 SAY 'PST TAX    :' + lcStrRate + ' %'
      @ lnRow ,73 SAY nPstAmt PICTURE '9999999.99'
    ENDIF
    @ 56,0  SAY CARTONS   PICTURE '@Z 99999'
    @ 56,6  SAY WEIGHT    PICTURE '@Z 99999'
    @ 56,64 SAY lnPieces  PICTURE '9999999'
    @ 56,73 SAY TOTALCHG  PICTURE '9999999.99'
    REPLACE PRTFLAG WITH 'P'    
ENDSCAN
DO ENDREPORT
RETURN

*!***************************************************************
*! Name      : lpPrtSku
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/20/99
*! Purpose   : TO Print the style/color Skus for a specific account..
*! Refer to  : (C101473)
*!***************************************************************
PROCEDURE lpPrtSku
PRIVATE lcAlias

lcAlias = ALIAS()
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
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
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
             'S'+InvLine.Account+InvLine.Style.AND. !EOF()
      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
    @ ROW,00 SAY lcStrToPrn
    ROW = ROW + 1
  ENDDO  
ELSE
  @ ROW,00 SAY Sku
  @ ROW,16 SAY 'CUSTOMER SKU #'
  ROW=ROW+1
ENDIF
SELECT (lcAlias)
RETURN



*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/20/99
*! Purpose   : TO get the information of the invoice header.
*! Refer to  : (C101473)
*!*************************************************************
FUNCTION lfGetInHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,laAddress 
STORE 0.00 TO lnPieces, lnSubTot
lcInvoice  = Invoice
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
IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  lcStAdd1 = OrdHdr.cAddress1
  lcStAdd2 = OrdHdr.cAddress2
  lcStAdd3 = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
ELSE
  IF INVHDR.CONSOL = 'Y' AND !EMPTY(CONSINVH.STORE)
    =SEEK('S'+lcAccount+CONSINVH.STORE,'CUSTOMER')
  ENDIF
  IF !EMPTY(CUSTOMER.DIST_CTR)
    lcDist  = CUSTOMER.DIST_CTR
    =SEEK('S'+lcAccount+lcDist,'CUSTOMER')
  ENDIF
  *--Get the distribution center address if any.
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  =gfGetAdr('Customer','','','',1,'')
  *--Get the Ship To adddess except the country.    
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ENDIF
*-- GET THE DESCRIPTION ABOUT THE CODES
lcTerVCode = SUBSTR(gfCodDes(cTermCode,PADR('CTERMCODE',10)),1,15)
lcShiVCode = SUBSTR(gfCodDes(ShipVia ,PADR('SHIPVIA',10)),1,15)
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
*--Division desc.
lcDivDesc = SUBSTR(gfCodDes(cDivision,PADR('CDIVISION',10)),1,14)
*--Do not get the [FACTOR] NAME & ADDRESS as the report does nor print it.
*--Get the division long name
*STORE '' TO lcDivLName
*=gfRltFld(OrdHdr.CDIVISION,@laDivLName,'CDIVISION ')
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/20/99
*! Purpose   : TO print the invoice header.
*! Refer to  : (C101473)
*!*************************************************************
FUNCTION lfPrntHdr
PRIVATE lcAlias

lcAlias = ALIAS()
@ 01,60 SAY lcInvoice
@ 01,71 SAY INVDATE
IF lcPrnComp = 'Y'
  HLINE1 = IIF(!EMPTY(lcDivLName),lcDivLName, lcCompName)
  @ 03,05 SAY HLINE1
ENDIF
@ 03,71 SAY lcDivDesc
IF lcPrnComp = 'Y'
  @ 04,05 SAY laCompAdd[1]
  @ 05,05 SAY laCompAdd[2]
ENDIF
@ 05,71 SAY ORDER
IF lcPrnComp = 'Y'
  @ 06,05 SAY laCompAdd[3]
  @ 07,05 SAY lcCompPhon PICTURE gfPhoneTem() SIZE 1,16
ENDIF
@ 07,71 SAY PIKTKT
@ 09,71 SAY APPROVAL
@ 13,08 SAY lcBtName
@ 13,54 SAY lcStName
@ 14,08 SAY lcBtAdd1
@ 14,54 SAY lcStAdd1
@ 15,08 SAY lcBtAdd2
@ 15,54 SAY lcStAdd2
@ 16,08 SAY lcBtAdd3
@ 16,54 SAY lcStAdd3
@ 21,02 SAY ACCOUNT
@ 21,12 SAY CUSTPO
@ 21,23 SAY IIF(!EMPTY(lcDist),lcDist,STORE)
@ 21,33 SAY DEPT                         
@ 21,42 SAY lcTerVCode
@ 21,62 SAY REP1
@ 21,66 SAY REP2
@ 21,71 SAY SUBSTR(lcShiVCode,1,14)
ROW = 23
SELECT INVLINE
STORE ' ' TO lcScale1,lcScale2
IF !llNoteLoop        
  lnRecNo = RECNO()
  DO WHILE .T.
    IF Invoice<>lcInvoice .OR. (lcScale1 <>" " .AND. lcScale2<> " ")
      EXIT
    ENDIF
    IF EMPTY(lcScale1)
      lcScale1=SCALE
    ENDIF
    IF SCALE <> lcScale1
      lcScale2 = SCALE
    ENDIF
    SKIP
  ENDDO
  GOTO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
ENDIF                                                   
SELECT SCALE
lnCol = 49
Y = ' '
DO WHILE lnCol<51
  lcColStr = CHR(lnCol)
  SEEK 'S'+lcScale&lcColStr                    
  IF !EOF()
    @ ROW,29 SAY SCALE+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                         PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                         PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                         PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
    ROW = ROW + 1
  ENDIF
  lnCol=lnCol+1
ENDDO
SELECT (lcAlias)

