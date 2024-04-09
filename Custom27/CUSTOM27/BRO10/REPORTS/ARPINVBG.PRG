*:***************************************************************************
*: Program file  : ARPINVBG.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR BROADWAY GARMENTS.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 8 SIZE SCALES 
*! Date          : 08/28/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C101910
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVBG
*:***************************************************************************

          *--Section of Initializing the variables --*
XNOTE_LOOP = .F.   && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1     && For the notepad
lcInv      = " "      && variable used in Reset the page # after eash invoice.
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC  = SUBSTR(gfGetMemVar('M_TAX_DESC',gcAct_Comp),1,10)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = SUBSTR(gfGetMemVar('M_TAX_REFE',gcAct_Comp),1,18)
NEWDOC = .T.
MAXROW = 47

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
  IF lcInv <> Invoice
    lcInv = Invoice
  ENDIF
  XINVOICE = INVOICE

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL
    NEWDOC    = .F.
    XORDER    = ORDER    
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION
    
    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
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

    *** GET THE DESCRIPTION ABOUT THE CODES
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

    CURLOOP = '1'                 && Variable to check if it is the first time for the new document.
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
   XSCALE =  ' '
   ROW    = 27
   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
     SELECT INVLINE
     IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
       IF ROW >= MAXROW
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
     @ ROW,00 SAY Style
     @ ROW,20 SAY LEFT(STYLE.SCALE,1)
      
     FOR lnCnt = 1 TO 8
       Z = STR(lnCnt,1)
       @ ROW, 17+(5*lnCnt) SAY QTY&Z.  PICTURE '@Z 9999'
     ENDFOR                  
     @ ROW,62 SAY TOTQTY PICTURE '999999'

     XLINETOT   = PRICE * TOTQTY
     XPIECES    = XPIECES + TOTQTY
     XSUBTOTAL  = XSUBTOTAL+XLINETOT            
     @ ROW,69  SAY PRICE     PICTURE '999.99'      
     @ ROW,75  SAY XLINETOT  PICTURE '999999.99'
     ROW = ROW + 1      
      
     IF SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
       @ ROW,0 SAY "SKU N# : " +;
                    LEFT(IIF(!EMPTY(Spck_Lin.Sku),Spck_Lin.Sku,Spck_Lin.Pack_Id),8)
       lnSytCol = 30
     ELSE
       lnSytCol = 0
     ENDIF
      
      @ ROW , lnSytCol SAY STYLE.Desc
      ROW = ROW + 1      
           
      SELECT INVLINE
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

  *-- Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + KEY = 'C' + XINVOICE
      @ ROW,02 SAY '* -- N O T E S -- *' 
      ROW = ROW + 1 
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !XNOTE_LOOP
        @ ROW,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        ROW = ROW + 1 
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  SELECT INVHDR
  @ 51,74 SAY XSUBTOTAL    PICTURE '9999999.99'  

  ROW=52
  @ 52,07 SAY NOTE1
  @ 52,44 SAY INVOICE

  llNotePrnt = .F.  

  IF XTAX .AND. XTAX_METH = 'M'      
    IF !EMPTY(lcTaxRefr)
      @ Row,49 SAY lcTaxRefr
      Row = Row + 1
    ENDIF        
  ENDIF  
   
  IF ROW = 53
    @ ROW,07 SAY INVHDR.NOTE2
    llNotePrnt = .T.
  ENDIF           
   
  IF XTAX .AND. XTAX_METH = 'M'      
    @ ROW ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
    ROW = ROW + 1
  ENDIF     
     
   IF !llNotePrnt AND ROW=53
     @ ROW, 07 SAY INVHDR.NOTE2
     llNotePrnt = .T.   
   ENDIF
   
   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,74 SAY WKAMT       PICTURE '9999999.99'  
      ROW=ROW+1
   ENDIF

   IF !llNotePrnt AND ROW = 53
     @ ROW,07 SAY INVHDR.NOTE2
     llNotePrnt =.T.
   ENDIF

   IF DISCOUNT<>0
      @ ROW,73 SAY DISCOUNT    PICTURE  '99999999.99'   
      ROW = ROW + 1
   ENDIF
   
   IF !llNotePrnt
     @ 53,07 SAY INVHDR.NOTE2
     llNotePrnt =.T.
   ENDIF
   
   @ 55,44 SAY INVHDR.INVDATE
    
   IF XTAX .AND. XTAX_METH = 'A'
     @ 55 ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
   ENDIF

   @ 58,6 SAY INVHDR.CARTONS    PICTURE '@Z 99999'
   @ 58,62 SAY XPIECES          PICTURE '9999999'
   @ 58,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'

   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
      NEWDOC = .T.
   ENDIF
ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfGetSzScl
*! Developer : BASSEM RAFAAT 
*! Date      : 08/29/2000
*! Purpose   : Get the first three size scale used in the invoice 
*            : to be printed on the invoice header.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetSzScl()
*!*************************************************************
FUNCTION lfGetSzScl
PRIVATE lnAlias, lcInvExp, lnScalNo

lnAlias   = SELECT()
lcInvExp  = InvLine.Invoice + STR(InvLine.LineNo,6)
lcStyExp  = Style.Style
lnScalNo  = 1
lcScalStr = ''
SELECT InvLine
IF SEEK(xInvoice)
  SCAN WHILE Invoice = xInvoice AND lnScalNo <= 3
    IF SEEK (InvLine.Style, "Style") AND ;
       SEEK ("S"+Style.Scale, "Scale")

       lcVariable  = "lcScalLn" + ALLTRIM(STR(lnScalNo))
       &lcVariable = LEFT(Style.Scale,1)                    +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz1),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz2),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz3),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz4),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz5),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz6),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz7),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz8),1,4),5)
       
         lnScalNo  = lnScalNo + 1
    ENDIF
  ENDSCAN
ENDIF
= SEEK (lcInvExp, "InvLine")
= SEEK (lcStyExp, "Style"  )
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 08/29/2000
*! Purpose   : PRINT THE HEADER OF THE INVOICE.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetInHdr()
*!*************************************************************
FUNCTION lfGetInHdr

SELECT INVHDR
@ 01,60 SAY XINVOICE
@ 01,71 SAY INVDATE
@ 03,71 SAY SUBSTR( PDIVISION, 1, 10 )
@ 05,71 SAY ORDER
@ 07,71 SAY PIKTKT        
@ 09,71 SAY APPROVAL
@ 13,08 SAY XBTNAME
@ 13,54 SAY XSTNAME
@ 14,08 SAY XBTADDR1
@ 14,54 SAY XSTADDR1
@ 15,08 SAY XBTADDR2
@ 15,54 SAY XSTADDR2
@ 16,08 SAY XBTADDR3
@ 16,54 SAY XSTADDR3  
@ 21,02 SAY ACCOUNT
@ 21,12 SAY LEFT(CUSTPO,10)
@ 21,23 SAY STORE
@ 21,33 SAY DEPT              
@ 21,42 SAY LEFT(PTERMS,14)
@ 21,63 SAY REP1                         
@ 21,67 SAY REP2  
@ 21,71 SAY SUBSTR(PSHIPVIA,1,13)

*-- PRINT SCALES
ROW = 24
IF !XNOTE_LOOP
  STORE SPACE(1) TO lcScalLn1 , lcScalLn2 , lcScalLn3
  llNoThing = lfGetSzScl()
  @ 23, 20 SAY lcScalLn1
  @ 24, 20 SAY lcScalLn2
  @ 25, 20 SAY lcScalLn3
ENDIF

Row = 27
*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 08/29/2000
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
@ 51,75 SAY '******'
ROW=ROW+1
@ 52,07 SAY NOTE1
@ 52,44 SAY INVOICE
@ 53,07 SAY NOTE2
@ 55,44 SAY INVDATE
@ 58,18 SAY CARTONS  PICTURE '@Z 999'
@ 58,62 SAY '******'
@ 58,75 SAY '******'

*--End of lfGetcont.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 08/29/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVF.PRG
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