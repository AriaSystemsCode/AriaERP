*:***************************************************************************
*: Program file  : ARPINVAS.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR AST.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C101008)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" 
*! Date          : 11/19/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C102024
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....gfGetZone , lFPrtSku
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVAS
*:***************************************************************************
*B606080,1 NAD 06/09/2002 Some alignments in the invoice form

           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = LEFT(gfGetMemVar('M_TAX_REFE',gcAct_Comp),18)
NEWDOC     = .T.
MAXROW     = 47
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

lcUpsType   = ''
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
  WAIT WINDOW 'Selecting Invoices for printing ...' + Invoice NOWAIT
  XINVOICE = INVOICE

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = INVHDR.PHONE
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION

    *** GET THE BILL TO AND SHIP ADDRESS
    SELE CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)

    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
    XBTNAME  = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
      XBTADDR2 = laSoldTo[3]
      XBTADDR3 = ''
    ENDIF
    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3])
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    SELECT CODES
    SET ORDER TO CODES IN CODES

    PTERMS     = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA   = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST   = gfCodDes(INVHDR.SPCINST,'SPCINST')
    PDIVISION  = gfCodDes(INVHDR.CDIVISION,'CDIVISION')
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

    PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) +XZN
    CURLOOP = '1'
  ENDIF

  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()
  
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT INVLINE
   
  XSTORE = STORE
  ROW    = 27
  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP           
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
      IF Row >= MaxRow
        IF INVOICE <> XINVOICE
          EXIT
        ENDIF
        =lfGetcont()
        =lfGetInHdr()
        *B606080,1 NAD (Start)
        ROW    = 27
        *B606080,1 NAD (End)
        LOOP
      ENDIF
      EXIT
    ENDIF
    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF
 
    SELECT STYLE
    XSTYDESC = IIF(EMPTY(STYLE.DESC) , SPACE(0) , SUBSTR(STYLE.DESC,1,17))
    lcColDes = gfCodDes(SUBSTR(INVLINE.STYLE,14,6),'COLOR')
    SELECT INVLINE
    
    @ ROW,01 SAY LEFT(STYLE,11)
    @ ROW,13 SAY SUBSTR(STYLE,14,6)    
    *B606080,1 NAD (Start)
    *@ ROW,19 SAY  STYLE.SCALE
    @ ROW,19 SAY ALLTRIM(STYLE.SCALE)
    *B606080,1 NAD (End)
    
    *B606080,1 NAD (Start)
    *@ ROW,21 SAY QTY1   PICTURE '@Z 99999'
    *@ ROW,26 SAY QTY2   PICTURE '@Z 99999'
    *@ ROW,31 SAY QTY3   PICTURE '@Z 99999'
    *@ ROW,36 SAY QTY4   PICTURE '@Z 99999'      
    *@ ROW,41 SAY QTY5   PICTURE '@Z 99999'
    *@ ROW,46 SAY QTY6   PICTURE '@Z 99999'
    *@ ROW,51 SAY QTY7   PICTURE '@Z 99999'
    *@ ROW,56 SAY QTY8   PICTURE '@Z 99999'
    *@ ROW,61 SAY TOTQTY PICTURE '999999'

    @ ROW,20 SAY QTY1   PICTURE '@Z 99999'
    @ ROW,25 SAY QTY2   PICTURE '@Z 99999'
    @ ROW,30 SAY QTY3   PICTURE '@Z 99999'
    @ ROW,35 SAY QTY4   PICTURE '@Z 99999'      
    @ ROW,40 SAY QTY5   PICTURE '@Z 99999'
    @ ROW,45 SAY QTY6   PICTURE '@Z 99999'
    @ ROW,50 SAY QTY7   PICTURE '@Z 99999'
    @ ROW,55 SAY QTY8   PICTURE '@Z 99999'
    @ ROW,60 SAY TOTQTY PICTURE '999999'
    *B606080,1 NAD (End)
    
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT
    *B606080,1 NAD (Start)
    *@ ROW,67  SAY PRICE     PICTURE '9999.99'
    @ ROW,66  SAY PRICE     PICTURE '9999.99'
    *B606080,1 NAD (End)
    @ ROW,75  SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW+1

    *--Procedure to print the Style/Color Sku no.
    =lFPrtSku()
    @ ROW,1 SAY XSTYDESC
    @ ROW,21 SAY lcColDes
    Row= Row + 1

    SELE INVLINE
    SKIP
  ENDDO
   
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
  IF ENDPAGE = '1' .AND. !XNOTE_LOOP
    =lfGetcont()
    LOOP
  ENDIF

  *** Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + Key <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + Key = 'C' + XINVOICE
      @ Row,02 SAY '* -- N O T E S -- *'
      Row = Row + 1 

      DO WHILE lnNotLine <= lnMemLins
        IF Row >= MaxRow
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ Row,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          Row = Row + 1
        ENDIF
        IF Row >= MaxRow
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !XNOTE_LOOP
        @ Row,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        Row = Row + 1 
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  @ 50,07 SAY INVHDR.PHONE SIZE 1,16
  SELECT INVHDR
  @ 50,55 SAY 'MERCHANDISE'
  @ 50,74 SAY XSUBTOTAL PICTURE '9999999.99'  

  ROW=51
  @ 51,07 SAY INVHDR.NOTE1

  IF XTAX .AND. XTAX_METH = 'M' 
    IF !EMPTY(lcTaxRefr)
      @ Row,55 SAY lcTaxRefr
      Row = Row + 1
    ENDIF  
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ ROW ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ ROW ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
    ROW = ROW + 1
  ENDIF

  IF ROW = 53
    @ 53,07 SAY INVHDR.NOTE2
    llNotePrnt = .T.
  ELSE
    llNotePrnt = .F.
  ENDIF

  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
     @ ROW,55 SAY 'TOTAL - FREIGHT'
     @ ROW,74 SAY WKAMT       PICTURE '9999999.99'  
     ROW=ROW+1
  ENDIF

  IF DISCOUNT<>0
     @ ROW,55 SAY 'DISCOUNT'
     @ ROW,73 SAY DISCOUNT    PICTURE  '99999999.99'   
  ENDIF

  IF !llNotePrnt
    @ 53,07 SAY INVHDR.NOTE2
  ENDIF

  IF XTAX .AND. XTAX_METH = 'A'
    @ 54,55 SAY lcTaxRefr         
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ 55 ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ 55 ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
  ENDIF

  IF xTAx .AND. llIsCanada 
    lcStrRate = ALLTRIM(STR(InvHdr.nPstRate,5,2))
    lnRow = IIF (xTAx_Meth='A',56,55)
    @ lnRow ,55 SAY 'PST TAX    :' + lcStrRate + ' %'
    @ lnRow ,74 SAY InvHdr.nPstAmt PICTURE '9999999.99'
  ENDIF

  
 
  *B606080,1 NAD (Start)
  *@ 57,18 SAY INVHDR.CARTONS   PICTURE '@Z 999'
  *@ 57,62 SAY XPIECES          PICTURE '9999999'
  *@ 57,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'
  
  @ 56,18 SAY INVHDR.CARTONS   PICTURE '@Z 999'
  @ 56,63 SAY XPIECES          PICTURE '9999999'
  @ 56,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'
  *B606080,1 NAD (End)




  SELECT INVHDR
  IF EOF()
    NEWDOC = .F.
    SET DEVICE TO SCREEN
    RETURN
  ELSE
    NEWDOC = .T.
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
*! Date      : 11/19/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVAS.PRG
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
*! Date      : 11/19/2000
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
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 

  DIME laSku[8]
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
  = SEEK('S'+InvLine.Account+InvLine.Style ,'Spck_Lin')
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
    @ ROW,1 SAY lcStrToPrn
    ROW = ROW + 1
  ENDDO  
ELSE
  @ ROW,1 SAY Sku
  @ ROW,16 SAY 'CUSTOMER SKU #'
  ROW=ROW+1
ENDIF
RETURN

*--End of lFPrtSku.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 11/19/2000
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
@ 01,60 SAY XINVOICE
@ 01,72 SAY INVDATE

IF llPrntComp
  @ 03,05 SAY HLINE1
ENDIF
@ 03,72 SAY SUBSTR( PDIVISION, 1, 14 )  

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
@ 07,72 SAY PIKTKT
@ 09,72 SAY APPROVAL
@ 13,09 SAY XBTNAME
@ 13,55 SAY XSTNAME
@ 14,09 SAY XBTADDR1
@ 14,55 SAY XSTADDR1
@ 15,09 SAY XBTADDR2
@ 15,55 SAY XSTADDR2
@ 16,09 SAY XBTADDR3
@ 16,55 SAY XSTADDR3
@ 21,02 SAY ACCOUNT
@ 21,13 SAY LEFT(CUSTPO,10)
@ 21,24 SAY STORE
@ 21,34 SAY DEPT
@ 21,42 SAY LEFT(PTERMS,14)
@ 21,64 SAY REP1
@ 21,67 SAY REP2
@ 21,71 SAY SUBSTR(PSHIPVIA,1,14)


ROW = 23



*--Section for the scale.
SELECT INVLINE
IF !XNOTE_LOOP        
  Trec1= RECNO()
  Tscale1=" "
  Tscale2=" "
  DO WHILE .T.
    IF Invoice<>xInvoice .OR. (Tscale1<>" " .AND. Tscale2<>" ")
      EXIT
    ENDIF
    IF Tscale1=" "
      Tscale1 = SCALE
    ENDIF
    IF SCALE <> Tscale1
      Tscale2 = SCALE
    ENDIF
    SKIP
  ENDDO
  IF BETWEEN(Trec1,1,RECCOUNT('Invline'))
    GOTO Trec1
  ENDIF
ENDIF                                 

SELECT SCALE
Trec1= RECNO()
I=49
Y=' '
DO WHILE I<51
  Ti = CHR(I)
  IF SEEK('S'+TSCALE&TI)
    @ ROW,19 SAY SCALE+Y+PADL(SZ1,5,' ')+PADL(SZ2,5,' ')+;
                         PADL(SZ3,5,' ')+PADL(SZ4,5,' ')+;
                         PADL(SZ5,5,' ')+PADL(SZ6,5,' ')+;
                         PADL(SZ7,5,' ')+PADL(SZ8,5,' ')
    Row= Row+1
  ENDIF
  I= I + 1
  
ENDDO
IF BETWEEN(Trec1,1,RECCOUNT('SCALE'))
  GOTO Trec1
ENDIF

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 11/19/2000
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
   
@ ROW+1,12 SAY 'C O N T I N U E D ...'
SELECT INVHDR
@ 50,07 SAY INVHDR.PHONE SIZE 1,16
@ 50,55 SAY 'MERCHANDISE'
@ 50,75 SAY '******'
ROW=ROW+1
@ 51,07 SAY INVHDR.NOTE1
@ 53,07 SAY INVHDR.NOTE2

*B606080,1 NAD (Start)
*@ 57,18 SAY INVHDR.CARTONS PICTURE '@Z 999'
*@ 57,62 SAY '******'
*@ 57,75 SAY '******'

@ 56,18 SAY INVHDR.CARTONS PICTURE '@Z 999'
@ 56,63 SAY '******'
@ 56,75 SAY '******'
*B606080,1 NAD (End)



*--End of lfGetcont.