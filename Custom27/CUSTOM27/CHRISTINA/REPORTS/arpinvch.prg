*:***************************************************************************
*: Program file  : ARPINVCH
*: Program desc. : CUSTOM INVOICE FORM
*: Date          : 06/15/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh (SSE)
*:***************************************************************************
*: Calls : 
*:    Procedures : lpGetScale
*:    Functions  : lfPrntData() 
*:               : gfGetMemVar() 
*:               : gfItemMask()
*:               : gfModalGen()
*:               : lfSolSpAdr()
*:               : gfCodDes()
*:***************************************************************************
*: Example : DO ARPINVCH
*:***************************************************************************
*: This Program is due to  CUSTOM PROGRAM FOR Christina Sportwear C101544
*:***************************************************************************
*:B802434,1 SSE 08/11/99 1) Size Scale get repeated on no. of style using this 
*:                          Scale , it should print each scale only one time 
*:                       2) Color desc. print color code attached to description
*:                          it should print color description only 
*:B603511,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.
*:B804371,1 MHM 09/20/2001 Update the field PRTFLAG.
*:***************************************************************************

XNOTE_LOOP = .F.        && Flag to indicate whether we have finished
                        && printing the Notepad or not.

*-- This commented variables is declared in the main prg. (ARPINV) as llTax , lcTaxDesc , lcTaxMeth
DIMENSION laSetups[2,2]
laSetups[1,1] = 'M_TAX_RATE'
laSetups[2,1] = 'M_TAX_REFE'
=gfGetMemVar(@laSetups,gcAct_Comp)
XTAX_RATE  = laSetups[1,2]
lcTaxRefr  = laSetups[2,2]

NEWDOC    = .T.         && To indicate whether it's a NEW INVOICE or not
MAXROW    = 47          && To initialize the MAXIMUM ROWS in a page 
lnNotLine = 1           && To store the number of notes memo lines 
llNoRec   = .F.         && If True program will not execute ENDREPORT Procedure

*-- these 5 Variables is defined in Main Prg (ARPINV) to hold COMPANY HEADING [Begin.]
HLINE1    = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
HLINE2    = TRIM(laCompAdd[1])
HLINE3    = TRIM(laCompAdd[2])
HLINE4    = TRIM(laCompAdd[3])+' '+laCompAdd[4]
HLINE5    = ALLTRIM(lcCompPhon)
*-- these 5 Variables is defined im Main Prg (ARPINV) to hold COMPANY HEADING [End.]

*-- IF HLINE3 Empty Shifts HLINE4 , HLINE5 Upward so as not to print Empty line 
IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF

*-- Get the style major and color [Begin.]
*-- Get the color
STORE 0 TO lnColorLen,lnNonMajSt

*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

*-- lnMajor Var. is declared in main prg (ARPINV) 
*-- Get the Non Major elements. [Begin.]
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = 'C'
    lnNonMajSt = laMajSeg[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSeg[lnI,1]='C',;
                 laMajSeg[lnI,3],;
                 lnColorLen + laMajSeg[lnI-1,6] + laMajSeg[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
*-- Get the Non Major elements. [End.]

*-- copy data that match criteria in OG into Temp. File
SELECT INVHDR
COPY ALL FOR &lcRpExp TO &gcWorkDir.&lcDumFile
IF !USED(lcDumFile)
  *-- there is record in SYREPUVR file for gfTempName called lcDumFile
  *-- this will be used to carry the INVOICE HEADER TEMP. File
  = gfOpenFile('&gcWorkDir.&lcDumFile',' ','EX')
ENDIF  

IF RECCOUNT(lcDumFile) = 0
  *--No records to display.
  llNoRec = .T.
  = gfModalGen('TRM00052B00000','DIALOG' )
  IF USED(lcDumFile)
    USE IN (lcDumFile)
  ENDIF
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
ELSE

  *-- Message : --LINE UP <YES>  <NO>  <QUIT>--
  llLineUp = gfModalGen('QRM40145B40012','DIALOG' ) = 1
  	
  IF !lfPrntData()
    llNoRec = .T.    && To prevent executing ENDREPORT
  ENDIF
ENDIF

IF llNoRec
  SET DEVICE TO SCREEN
  SET PRINTER TO
ENDIF
*-- End of Report.

*!*************************************************************
*! Name      : lfPrntData
*! Developer : Sameh (SSE)
*! Date      : 06/15/1999
*! Purpose   : Collecting Data for the main report
*!*************************************************************
*! Called from : ARPINVCH.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrntData
*!*************************************************************
*
FUNCTION lfPrntData
PRIVATE lcOldScale
lcOldScale = ' '
*-- These 3 Variables are declared and defined in Main Prg (ARPINV)
MSG1 = lcRpMsg1       && 1st. line Message Variable
MSG2 = lcRpMsg2       && 2nd. line Message Variable   
MSG3 = lcRpMsg1       && 3rd. line Message Variable

WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
CLEAR TYPEAHEAD
SET DEVICE TO PRINT

*-- move record pointer to TOP of lcDumFile
SELECT &lcDumFile
GO TOP

DO WHILE INKEY() <> 32
  *--if Temp File reached End of file exit Do While loop
  IF EOF()
    EXIT
  ENDIF
  XINVOICE = INVOICE


  *-- Get invoice header, line items, and financial history records.
  *-- If any records are not found, skip to next invoice.
  *-- Initialize document totals.

  IF NEWDOC           && IF same Invoice 
    PRTERR = 0
    STORE 0.00 TO XPIECES, XSUBTOTAL

    SELECT INVHDR
	
	*-- If invoice not found get information about next one.
    IF !SEEK(XINVOICE)
      SELECT &lcDumFile
      SKIP
      LOOP
    ENDIF

    *-- Store some data from INVHDR file
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = IIF(EMPTY(PHONE),'',TRANSFORM(PHONE,lcPhonPict))
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XORDER    = ORDER
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION

    *-- Get the bill to and ship to address
    *-- lfSolSpAdr called from Main Prg because it is also defined there. 
    *-- and lfAdrShift() is called within lfSolSpAdr()  
    =lfSolSpAdr()
        
    *-- Find the invoice lines
    *-- PRTERR variable is used in Aria26 to carry a value when this INVOICE
    *--  is not found in INVLINE , this variable is used to check for its value
    *-- and to generate a message for this ERROR 
    SELECT INVLINE
    IF !SEEK (XINVOICE)
      PRTERR = 2
    ENDIF
          
    *-- Get the description about the codes
    lcZone = ''
    DIMENSION laZone[1,2]

    laZone[1,1]     = 'CUPS'
    laZone[1,2]     = 'lcZone'

    *-- To get the payment Terms.
    PTERMS = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE',.T.)

    *-- to get Shipvia    
    LCSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA',.T.)

    *-- Spc Inst.
    PSPCINST= gfCodDes(INVHDR.SPCINST,'SPCINST')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')

    *-- [Factor] Name & Address
    *PRINTFCTR = IIF(XINVFACTOR='Y', .T., .F.)
    PRINTFCTR = llPrnFact          && (XINVFACTOR = 'Y')
    
    *-- initialize XFNAME,XFADDR1,XFADDR2,XFADDR3 to be used if this customer has a factor
    STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3

    *-- this loop in CASE THIS customer has a FACTOR [Begin.]
    DO WHILE PRINTFCTR
      *-- if this INVOICE for current customer has no FACTOR CODE [Begin.]
      IF EMPTY(INVHDR.CFACCODE)   && empty factor code
        PRINTFCTR = .F.           && Print Factor Flag will be FALSE
        EXIT                      && Exit this Do While
      ENDIF
      *-- if this INVOICE for current customer has no FACTOR CODE [End.]
      
      *-- if Factor Code not empty in this INVOICE seek for INVHDR.CFACCODE in SYCFACT
      SELECT SYCFACT          && this File contains all the FACTORS for all customers
      SEEK INVHDR.CFACCODE

      DO CASE
        CASE EOF()                && Case factor not found in SYCFACT
          PRINTFCTR = .F.         && Print Factor Flag will be FALSE
          STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3    && empty variables that store factor name and address

        CASE FOUND()              && Case factor is found in SYCFACT
          *-- Store factor name and address
          XFNAME  = CFACCOMP
          XFADDR1 = CADDRESS1
          XFADDR2 = CADDRESS2
          XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5

          *-- Shifts up empty variables
          IF XFADDR2 = ' '
            XFADDR2 = XFADDR3
            XFADDR3 = ' '
          ENDIF
      ENDCASE
      EXIT                        && To get out of DO While  
    ENDDO
    *-- this loop in CASE THIS customer has a FACTOR [End.]

    *-- CURRLOOP variable is like a flag it carries Value=1 to print some variables 
    *-- one time if the INVOICE moved to another page 
    CURLOOP = '1'                  
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF
  *-- End of NewDoc

  *-- Start printing
  *-- Line 3
  SELECT INVHDR
  *-- modify XINVNAME with lcPrnComp  
  *-- XINVNAME is ARIA26 variable , lcPrnComp is defined in Main Prg (ARPINV)
  *-- to indicate whether to print Company Heading or not 
  
  *-- Print page header. [Begin
  XINVNAME = lcPrnComp
  IF XINVNAME='Y'
    @ 03,05 SAY IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
    SELECT INVHDR
  ENDIF

  *-- Line 4
  IF XINVNAME='Y'
    @ 04,05 SAY HLINE2
  ENDIF

  IF PRINTFCTR
    @ 04,58 SAY 'R E M I T   T O:'
  ENDIF
  
  *-- Line 4   print INVOICE NUMBER
  @ 04,73 SAY XINVOICE
   
  *-- Line 5
  IF XINVNAME='Y'
    @ 05,05 SAY HLINE3
  ENDIF
   
  IF PRINTFCTR
    @ 05,50 SAY XFNAME
  ENDIF

  *-- Line 6
  IF XINVNAME='Y'
    @ 06,05 SAY HLINE4
  ENDIF

  IF PRINTFCTR
    @ 06,50 SAY XFADDR1
  ENDIF

  *-- Line 7
  IF XINVNAME='Y'
    @ 07,05 SAY HLINE5
  ENDIF

  IF PRINTFCTR
    @ 07,50 SAY XFADDR2
  ENDIF

  *-- Line 8
  IF PRINTFCTR
    @ 08,50 SAY XFADDR3
  ENDIF
  *-- Print page header. [End..
  
  *-- Print Customer group. [Begin
  *-- Line 10
  *@ 10,10 SAY XBTNAME
  @ 10,10 SAY lcSolTName

  *-- print only when CURLOOP = '1' (first time only)
  IF CURLOOP = '1'
    *@ 10,47 SAY XSTNAME 1
    @ 10,47 SAY lcShpTName
  ENDIF

  *-- Line 11
  *@ 11,10 SAY XBTADDR1
  @ 11,10 SAY laSoldTo[1]

  IF CURLOOP = '1'
    *@ 11,47 SAY XSTADDR1
    @ 11,47 SAY laShipTo[1]
  ENDIF

  *-- Line 12
  *@ 12,10 SAY XBTADDR2
  @ 12,10 SAY laSoldTo[2]

  IF CURLOOP = '1'
    *@ 12,47 SAY XSTADDR2 
    @ 12,47 SAY laShipTo[2]
  ENDIF

  *-- Line 13
  *@ 13,10 SAY XBTADDR3
  @ 13,10 SAY TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]

  IF CURLOOP = '1'
    *@ 13,47 SAY XSTADDR3
    @ 13,47 SAY TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
  ENDIF

  *-- Line 17  
  @ 17,00 SAY ACCOUNT
  *-- Print Customer group. [Begin
   
  *-- Print invoice information [Begin]
  @ 17,06 SAY SUBSTR(CUSTPO,1,10)
  @ 17,17 SAY IIF(CURLOOP='1',XSTORE,'')
  @ 17,26 SAY DEPT
  @ 17,32 SAY ORDER
   
  @ 17,39 SAY SUBSTR(PTERMS,1,12)
   
  @ 17,51 SAY REP1
  @ 17,55 SAY REP2
  @ 17,73 SAY SUBSTR(LCSHIPVIA,1,10)
  *-- Print invoice information [End  ]

  *-- Details [Begin]
  *-- Print scales
  lcScalStr = SPACE(0)
  lcInvNo   = INVHDR.INVOICE
  DO lpGetScale WITH lcInvNo
  SELECT SCALE
  ROW = 19  
  FOR lnI = 1 TO LEN(lcScalStr)
    lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )

    *B802434,1 add If..Endif to the print of scale [Begin]
    *-- if current scale not equal to old scale
    *-- so as to print scale only one time when it is changed
    IF lcSizeScal <> lcOldScale
      lcOldScale = lcSizeScal
      SEEK 'S' + lcSizeScal
      @ ROW,35 SAY lcSizeScal 
      I=0
      FOR T = 1 TO 6
        X=STR(T,1)
        @ ROW,37+I SAY PADL(ALLTRIM(SUBSTR(SZ&X,1,3)),3)
        I=I+4
      ENDFOR
      ROW = ROW + 1
    ENDIF
    *-- Endif of current scale not equal to old scale
    *B802434,1 add If..Endif to the print of scale [End]
  ENDFOR
  
  *-- [1] Line item print loop

  SELECT INVLINE
  XSTORE = STORE
  XSCALE =  ' '
  ROW    = 24

  *-- this Do While Loop happens only when the report is printing the Current
  *-- INVOICE in the 1st Page (Invoice is not moved to the 2nd Page) and also
  *-- when report didn't print the NotePad yet (which is expressed by XNOTE_LOOP) 
  
  *B802434,1 initialize lcOldScale [Begin]
  lcOldScale = ' '     
  *B802434,1 initialize lcOldScale [End]

  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP         
    SELECT INVLINE

    *-- if INVLINE reached EOF() or INVOICE not found in INVLINE 
    *-- or Page has reached the maximum line number (MAXROW = 47)
    IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
      EXIT
    ENDIF
    
    *-- If TotQty = 0 , move record pointer in INVLINE and loop again to check for all conditions for the new record in INVLINE
    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF

    *-- get the style from the INVLINE and Seek for it in the STYLE File 
    KEY = INVLINE.STYLE      && SSE +INVLINE.COLOR
    SELECT STYLE
    SEEK KEY
    
    *-- select again the INVLINE line after selecting STYLE
    SELECT INVLINE
    *-- Say style 
    *-- lnMajor is declared in main Prg (ARPINV)
    @ ROW,00 SAY SUBSTR(STYLE,1,lnMajor)

    *-- Say color
    @ ROW,12 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    
    *B802434 print Scale only one time if current scale not equal to old scale [Begin]
    @ ROW,35 SAY IIF(Style.Scale <> lcOldScale,SUBSTR(Style.Scale,1,1),'')
    lcOldScale = Style.Scale
    *B802434 print Scale only one time if current scale not equal to old scale [End]

    @ ROW,36 SAY Qty1   PICTURE '@Z 9999'
    @ ROW,40 SAY Qty2   PICTURE '@Z 9999'
    @ ROW,44 SAY Qty3   PICTURE '@Z 9999'
    @ ROW,48 SAY Qty4   PICTURE '@Z 9999'
    @ ROW,52 SAY Qty5   PICTURE '@Z 9999'
    @ ROW,56 SAY Qty6   PICTURE '@Z 9999'
    @ ROW,60 SAY TOTQTY PICTURE '99999'

    XLINETOT   = Price * TotQty
    XPIECES    = XPIECES + TotQty
    XSUBTOTAL  = XSUBTOTAL+XLINETOT

    @ ROW,66  SAY Price     PICTURE '9999.99'
    @ ROW,75  SAY XLINETOT  PICTURE '999999.99'
      
    ROW = ROW + 1
    @ ROW,00 SAY Style.Desc
    
    ROW = ROW + 1
    *-- This function returns Non Major Description whether it is Color or anything else
    IF !USED('ICSEGVAL')
      =gfOpenFile(gcDataDir+'ICSEGVAL','SegVal','SH')  
    ENDIF
    
    lcTemp = lfNonMjDes()
    @ ROW,00 SAY PADR(lcTemp,15)

    *-- get the PACK ID from SPCKLIN file for this Invoice for the current customer (ACCOUNT) [Begin.]
    *-- so as to print it beside the Color Code and Description
    IF SEEK('P'+INVLINE.Account+INVLINE.Style,'SPCK_LIN')
      *-- if Color Code and Description empty move to next ROW [Begin.]
      IF EMPTY(lcTemp)  
        ROW = ROW + 1    
      ENDIF
      *-- if Color Code and Description empty move to next ROW [End.]
      @ ROW,16 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
    ENDIF
    *-- get the PACK ID from SPCKLIN file for this Invoice for the current customer (ACCOUNT) [End.]   

    *-- get the SKU from SPCKLIN file for this Invoice for the current customer (ACCOUNT) [Begin.]
    IF SEEK( 'S' + InvLine.Account + InvLine.Style, 'Spck_Lin')    
      ROW = ROW + 1                && move one ROW any way 
      
      *-- If TotQty in SPCK_LIN empty print PACK_ID    
      IF Spck_Lin.TotQty = 0
        @ ROW,00 SAY 'SKU # :  ' + Spck_Lin.Pack_Id
      ELSE   && ELSE TotQty <> 0 (not empty)
        lnCol   = 0       
        lnCount = 1        && this variable is used as a counter
        
        *-- move the record pointer in the SCALE file to 
        =SEEK( 'S' + InvLine.Scale, 'Scale' )

        SELECT Spck_Lin          
        SCAN REST WHILE 'S' + Account + Style = 'S' + InvLine.Account + InvLine.Style .AND.;
          lnCount <= 8
            
          IF lnCount = 5
            ROW   = ROW + 1
            lnCol = 0
          ENDIF
          lcCount = STR( lnCount, 1 )
          @ ROW, lnCol SAY Scale.Sz&lcCount + ':' + SUBSTR(Spck_Lin.Pack_Id,1,15)

          lnCol   = lnCol   + 21
          lnCount = lnCount + 1

        ENDSCAN
          
      ENDIF
    ENDIF
    *-- get the SKU from SPCKLIN file for this Invoice for the current customer (ACCOUNT) [Begin.]

    ROW = ROW + 1

    IF llLineUp
       EXIT
    ENDIF
    *-- Get next line item
  *-- Details [End  ]

    SELECT INVLINE
    SKIP   && move pointer to anoher record in INVLINE
  ENDDO
  *-- end of DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
  *-- End line processing 

  *-- if user want to print current invoice again.
  IF llLineUp .AND. !XNOTE_LOOP
    EJECT
	*-- Message : --ANOTHER LINE UP <YES>  <NO>  <QUIT>--
    lnChoice = gfModalGen('QRM40143B40012','DIALOG' )
    
    DO CASE
       CASE lnChoice = 3
         SET DEVICE TO SCREEN
         IF USED(lcDumFile)
           USE IN (lcDumFile)
         ENDIF
         ERASE &gcWorkDir.&lcDumFile+'.DBF'
         RETURN(.F.)
       CASE lnChoice = 2
         llLineUp =.F.
    ENDCASE
    WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
    SELECT &lcDumFile
    STORE 0.00 TO XPIECES, XSUBTOTAL
    LOOP
  ENDIF

  *-- Currently Processing Regular Line Items
  *-- ENDPAGE Variable is used to indicate that it is in the same invoive to be continued
  
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')

  IF ENDPAGE = '1' .AND. !XNOTE_LOOP                 
    @ ROW+1,12 SAY 'C O N T I N U E D ...'
    SELECT INVHDR
    @ 54,01 SAY XPHONE + ' ' + INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
    IF LEN(TRIM(MSG1 + MSG2)) >0
       @ 56,10 SAY MSG1
       @ 57,10 SAY MSG2
    ENDIF

    @ 60,02 SAY INVHDR.PIKTKT
    @ 60,09 SAY ShipDate
    @ 60,19 SAY '***'
    @ 60,25 SAY '*****'
  
    @ 60,43 SAY InvHdr.InvDate
  
    @ 60,73 SAY '******.**'
    LOOP
  ENDIF

  *-- get the notes for this invoice from NOTEPAD File [Begin.]
  SELECT NOTEPAD
  lnOldMemW = SET("MEMOWIDTH")  && store the old MemoLine Width setting
  SET MEMOWIDTH TO 75
  
  IF SEEK ('C' + XINVOICE)
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)  
  ENDIF
  
  *-- Print notepad.
  IF TYPE + KEY = 'C' + XINVOICE
    @ ROW,02 SAY '* -- N O T E S -- *' 
    ROW = ROW + 1 
    DO WHILE lnNotLine <= lnMemLins 
      IF ROW >= MAXROW
        XNOTE_LOOP = .T.
        EXIT
      ELSE
        XNOTE_LOOP = .F.
        @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
        ROW = ROW + 1
      ENDIF
      lnNotLine = lnNotLine + 1
    ENDDO
     
    IF !XNOTE_LOOP
      @ ROW,02 SAY '* -- END OF NOTES -- *'
      lnNotLine = 1
      ROW = ROW + 1 
    ELSE
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR
       
      @ 54,01 SAY XPHONE + ' ' + INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
      IF LEN(TRIM(MSG1 + MSG2)) >0
        @ 56,10 SAY MSG1
        @ 57,10 SAY MSG2
      ENDIF

      @ 60,02 SAY INVHDR.PIKTKT
      @ 60,09 SAY INVHDR.ShipDate
      @ 60,19 SAY '***'
      @ 60,25 SAY '*****'

      @ 60,43 SAY InvHdr.InvDate
      @ 60,73 SAY '******.**'

      LOOP
    ENDIF
  ENDIF
  SET MEMOWIDTH TO lnOldMemW
  *-- get the notes for this invoice from NOTEPAD File [End  .]
  
  *-- invoice footer.
  SELECT INVHDR
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E' 
   
  @ ROW,59 SAY XPIECES              PICTURE '999999'
   
  @ ROW,74 SAY XSUBTOTAL            PICTURE '9999999.99'   
  ROW=ROW+1

  IF DISCOUNT<>0
    @ ROW,12 SAY 'TOTAL - D I S C O U N T'      
    @ ROW,74 SAY DISCOUNT             PICTURE '99999999.99'
      
    ROW=ROW+1
  ENDIF

  *-- if merchandise tax only.
  IF llTax .AND. lcTaxMeth = 'M' 
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ ROW,12 SAY lcTaxDesc

    IF !EMPTY(lcTaxRefr)
      @ ROW,33 SAY lcTaxRefr
    ENDIF  

    @ ROW,64 SAY XSTRING_RATE + ' %'
    @ ROW,74 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'

    ROW = ROW + 1
    IF InvHdr.nPSTAmt <> 0
      @ ROW,12 SAY 'P S T   T A X'
      @ ROW,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       
      @ ROW,74 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       
      ROW = ROW + 1
    ENDIF  

  ENDIF  
   
  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
     @ ROW,12 SAY 'TOTAL - F R E I G H T'
      
     @ ROW,74 SAY WKAMT                PICTURE '9999999.99'  &&TAK 02/02/94
      
    ROW=ROW+1
  ENDIF

  *-- if Get all taxes.
  IF llTax .AND. lcTaxMeth = 'A'
    XSTRING_RATE = ALLTRIM(STR(INVHDR.TAX_RATE,5,2))
    @ ROW,12 SAY lcTaxDesc

    IF !EMPTY(lcTaxRefr)
      @ ROW,33 SAY lcTaxRefr
    ENDIF  
    @ ROW,64 SAY XSTRING_RATE + ' %'
     
    @ ROW,74 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
     
    ROW = ROW + 1
    IF InvHdr.nPSTAmt <> 0
      @ ROW,12 SAY 'P S T   T A X'
      @ ROW,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       
      @ ROW,74 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       
      ROW = ROW + 1
    ENDIF  

  ENDIF  

  @ 54,01 SAY XPHONE + ' ' + XNOTE1 + ' ' + XNOTE2
  IF LEN(TRIM(MSG1 + MSG2)) >0
    @ 56,10 SAY MSG1
    @ 57,10 SAY MSG2
  ENDIF

  IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
    @ 59,56 SAY 'APPROVAL: ' + INVHDR.APPROVAL
  ENDIF

  @ 60,02 SAY INVHDR.PIKTKT
  @ 60,09 SAY ShipDate
  @ 60,19 SAY CARTONS            PICTURE '@Z 999'

  @ 60,25 SAY WEIGHT             PICTURE '@Z 99999'
  @ 60,43 SAY InvHdr.InvDate
  @ 60,74 SAY INVHDR.TOTALCHG    PICTURE '9999999.99' 
  
  *B603511,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.[START]
  *SELECT INVHDR 
  *SEEK XINVOICE
  *REPLACE PRTFLAG WITH 'P'
  *B603511,1 BWA 03/08/2000 [END]
  
  *B804371,1 MHM 09/20/2001  update the field PRTFLAG.[START]
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
  *B804371,1 MHM 09/20/2001  [End]
   
  *-- Get next header record (New Invoice.)
  SELECT &lcDumFile
  SKIP
  NEWDOC = .T.
ENDDO    && End of Main Do While 

*-- Close all opened files [Begin.]
USE IN (lcDumFile)
ERASE &gcWorkDir.&lcDumFile+'.DBF'
USE IN ICSEGVAL  
*-- Close all opened files [End.]

SET DEVICE TO SCREEN
RETURN
*-- End of lfPrntData.


*!*************************************************************
*! Name      : lpGetScale
*! Developer : Sameh (SSE)
*! Date      : 06/16/1999
*! Purpose   : Get the first 5 Scales in one string together
*!*************************************************************
*! Called from : Main Program (ARPINVCH)
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : lcInvNo (Invoice Number)
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpGetScale
*!*************************************************************
*
PROCEDURE lpGetScale
PARAMETERS lcInvNo
PRIVATE lnRecNo, lcAlias, lnCount
*-- Save the current alias.
lcAlias = ALIAS()

SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)
lnCount = 1

SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 5
  IF !( Scale $ lcScalStr )
    lcScalStr = lcScalStr + SUBSTR(Scale,1,1)
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp,"InvLine")

*-- Select the old alias.
SELECT (lcAlias)
*-- End of lpGetScale.

*!*************************************************************
*! Name      : lfNonMjDes
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Evaluate Non Major Code and Description
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNonMjDes()
*!*************************************************************
*
FUNCTION lfNonMjDes
PRIVATE lnI , lcTemp

STORE '' TO lcTemp , lcNonMjDes
lnI = 0
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lcTemp = ''
  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSeg[lnI,1] $ "FOTQ"
      IF SEEK(STR(lnI,1)+SUBSTR(INVLINE.STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),"ICSEGVAL")
        lcTemp = ALLTRIM(ICSEGVAL.cISgValSd)
      ENDIF
    *-- Season, Color, Division, or Style group Segment.
    CASE laMajSeg[lnI,1] $ "ZCDG"
      DO CASE
        CASE laMajSeg[lnI,1] = "Z"
          lcCodeExpr = "SEASON"    
        CASE laMajSeg[lnI,1] = "C"
          lcCodeExpr = "COLOR"    
        CASE laMajSeg[lnI,1] = "D"
          lcCodeExpr = "CDIVISION"    
        OTHERWISE
          lcCodeExpr = "CSTYGROUP"    
      ENDCASE

      *B802434,1 print color description only by changing the last flag passed to false
      *lcTemp = ALLTRIM(gfCodDes(SUBSTR(INVLINE.STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcCodeExpr,.T.))
      lcTemp = ALLTRIM(gfCodDes(SUBSTR(INVLINE.STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcCodeExpr,.F.))
      *B802434,1 print color description only by changing the last flag passed to false

    *-- Size Seqment case.
    OTHERWISE
      IF SEEK("S"+SUBSTR(INVLINE.STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),"SCALE")
        lcTemp = ALLTRIM(SCALE.cScl_desc)
      ENDIF
    
  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)

ENDFOR    && end Loop Around Non Major elements.
RETURN (lcTemp)
*-- end of lfNonMjDes.