*:****************************************************************
*: Program file  : ARPINVOL.PRG
*: Program desc. : Custom Invoice form for OLSEN 
*: System        : Aria Apparel System (A27).
*: Module        : Accounts Recevible (AR)
*: Developer     : Ahmed Abdel Naby (AAN)
*: Date          : 09/02/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*:C#  200225          : 
*:****************************************************************
*: Note : Print invoices - 66 Line/page, 8 1/2" x 11"
*:****************************************************************
*:Modifications :
*:B804443,1 10/03/2001 Fix the bug of the account is duplicated.
*:B804454,1 10/03/2001 Fix the bug print the GST tax from the setup.
*:B121750,1 NNA 03/03/2004 Fix the bug of ship to field when printing invoices
*:B123037,1 ASH 06/09/2004 Fix the bug of printing the invoice in 2 pages.
*:****************************************************************
*:
*--Initialize momory variables.
IF !USED('Syszones')
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH')
ENDIF

*--Section to get the FromZone
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED('WAREHOUS')
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH')
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, 'WAREHOUS' ),WAREHOUS.UPS,lcFromZone)
ENDIF
*--End Section to get the FromZone --*

xTax      = (gfGetMemVar('M_TAX') = 'Y')
xTax_Desc = gfGetMemVar('M_TAX_DESC')
xTax_Meth = gfGetMemVar('M_TAX_METH')
xTax_Rate = gfGetMemVar('M_TAX_RATE')

llNoRec = .F.

lcTaxRefr =gfGetMemVar('M_Tax_Refe')
llIsCanada = IIF(UPPER(ALLTRIM(gcContCode))='CANADA',.T.,.F.)

llPntONte = llRpInvLNt && Print Note Pade

*--Open needed files.

=gfOpenFile(gcdatadir+'Codes',gcdatadir+'Codes','SH')
=gfOpenFile(gcdatadir+'OrdHdr',gcdatadir+'OrdHdr','SH')
=gfOpenFile(gcdatadir+'Style',gcdatadir+'Style','SH')
=gfOpenFile(gcdatadir+'Scale',gcdatadir+'Scale','SH')
=gfOpenFile(gcdatadir+'ConsInvH',gcdatadir+'ConsInvH','SH')

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'

TmpHFile  = gfTempName()
TmpLFile  = gfTempName()

SELECT INVHDR
COPY STRUCTURE TO (gcworkdir+TmpHFile)
=gfOpenFile(gcworkdir+TmpHFile,'','SH')
SELECT INVLINE
COPY STRUCTURE TO (gcworkdir+TmpLFile)
=gfOpenFile(gcworkdir+TmpLFile,'','EX')

SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected" NOWAIT
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
SET DEVICE TO PRINT

SCAN FOR &lcASExp
  SCATTER MEMO TO laFields
  lcMemLins=IIF(SEEK('C'+INVHDR.Invoice,'NotePad'),STR(MEMLINES(NotePad.mNotes),5),SPACE(5))
  SELECT &TmpHFile    
  APPEND BLANK
  GATHER MEMO FROM laFields
  REPLACE Labels WITH 1,cAdd_Time WITH SPACE(5)+lcMemLins  
ENDSCAN
SELECT &TmpHFile 
*B121750,1 NNA 03/03/2004 (Begin) Relationship between the tmpfile and the invhdr to get the correct
*B121750,1 NNA                    Record of the invhdr
SET RELATION TO &TmpHFile..invoice INTO Invhdr ADDITIVE
*B121750,1 NNA (End)
SCAN
  SELECT InvLine
  SEEK &TmpHFile->Invoice
  SCAN WHILE Invoice=&TmpHFile->Invoice
    SCATTER MEMO TO laFields
    SELECT &TmpLFile
    APPEND BLANK
    GATHER MEMO FROM laFields
    REPLACE Dyelot WITH SPACE(9)+'1'
  ENDSCAN
ENDSCAN
SELECT &TmpLFile
INDEX ON Invoice+STR(lineNo,6) TAG &TmpLFile

*--Initialize Local variables.
STORE ''TO xStName,xStAddr1,xStAddr2,xStAddr3,;
           xBtName,xBtAddr1,xBtAddr2,xBtAddr3
STORE ''   TO PTerms,PShipVia
STORE 0.00 TO xPieces, xSubTotal
llNewDoc   = .T.
llStat     = .F.
llLoopNote = .F.
MaxRow     = 47
lnCpyCntr  = 1


WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

*--MAIN LOOP
SELECT &TmpHFile
GO TOP
DO WHILE !EOF() .AND. INKEY() <>32

  xInvoice = Invoice
  *--Get invoice header   
  IF llNewDoc
    DO lpGetHdr
  ENDIF

  *TTTT
  *lnCol = 06
  *--Print heder information.
  *@ lnCol  ,10 SAY xBtName  
  *@ lnCol  ,55 SAY SUBSTR(xStName,1,24)
  *@ lnCol+1,10 SAY xBtAddr1
  *@ lnCol+1,55 SAY SUBSTR(xStAddr1,1,24)
  *@ lnCol+2,10 SAY xBtAddr2
  *@ lnCol+2,55 SAY SUBSTR(xStAddr2,1,24)
  *@ lnCol+2,88 SAY xInvoice
  *@ lnCol+3,10 SAY xBtAddr3
  *@ lnCol+3,55 SAY SUBSTR(xStAddr3,1,24)
  *@ lnCol+5,87 SAY InvDate
  *@ lnCol+8,03 SAY Account
  *@ lnCol+8,11 SAY CustPo
  *@ lnCol+8,22 SAY OrdHdr->Store
  *@ lnCol+8,31 SAY Dept
  *@ lnCol+8,44 SAY Order
  *@ lnCol+8,54 SAY SUBSTR(PTerms,1,14)
  *@ lnCol+8,72 SAY Rep1
  *@ lnCol+8,76 SAY Rep2
  *@ lnCol+8,82 SAY PShipVia

  *TTTT
  lnCol = 7
  @ lnCol+00,11 SAY xBtName
  @ lnCol+00,49 SAY SUBSTR(xStName,1,24)
  @ lnCol+01,11 SAY xBtAddr1
  @ lnCol+01,49 SAY SUBSTR(xStAddr1,1,24)
  @ lnCol+02,11 SAY xBtAddr2
  @ lnCol+02,49 SAY SUBSTR(xStAddr2,1,24)
  @ lnCol+02,76 SAY xInvoice  
  @ lnCol+03,11 SAY xBtAddr3
  @ lnCol+03,49 SAY SUBSTR(xStAddr3,1,24)
  @ lnCol+05,76 SAY InvDate
  @ lnCol+09,03 SAY Account
  @ lnCol+09,11 SAY CustPo
  @ lnCol+09,23 SAY OrdHdr->Store
  @ lnCol+09,32 SAY Dept
  @ lnCol+09,39 SAY Order
  @ lnCol+09,48 SAY SUBSTR(PTerms,1,14)
  @ lnCol+09,63 SAY Rep1
  @ lnCol+09,66 SAY Rep2
  @ lnCol+09,71 SAY PShipVia

  Row = 23

  *--Print detail information.
  SELECT &TmpLFile
  
  IF !llLoopNote .AND. lnCpyCntr = 1
    lnFrstRec=RECNO()
  ENDIF
  SCAN WHILE Invoice = xInvoice .AND. Row < MaxRow .AND. !llLoopNote FOR TotQty <> 0
    IF VAL(Dyelot) = 1
      =SEEK(Style,'STYLE')
      @ ROW,02 SAY SUBSTR(Style,1,11)
    *TTTT
    *  @ ROW,15 SAY SUBSTR(Style,14,6)
    *  @ ROW,23 SAY STYLE.Desc
    *  @ ROW,67 SAY TotQty PICTURE '9999'
    *  IF lnCpyCntr = 1
    *    xPieces    = xPieces + TotQty
    *    xSubTotal  = xSubTotal+(Price * TotQty)
    *  ENDIF
    *  @ ROW,75 SAY Price            PICTURE '9999.99'
    *  @ ROW,87 SAY (Price * TotQty) PICTURE '999999.99'
    
      @ ROW,14 SAY SUBSTR(Style,14,6)
      @ ROW,20 SAY STYLE.Desc
      @ ROW,59 SAY TotQty PICTURE '9999'
      IF lnCpyCntr = 1
        xPieces    = xPieces + TotQty
        xSubTotal  = xSubTotal+(Price * TotQty)
      ENDIF
      @ ROW,65 SAY Price            PICTURE '9999.99'
      @ ROW,75 SAY (Price * TotQty) PICTURE '999999.99'
    
    ENDIF

    IF llPntONte .AND. MEMLINES(Note_Mem) <> 0
      llExit = .F.
      lnNoteLn = VAL(Dyelot)
      =lfPrtONte() 
      IF lnCpyCntr = lnNoCopys
        REPLACE Dyelot WITH STR(lnNoteLn,9)
      ENDIF
      IF llExit
        EXIT
      ENDIF
    ENDIF

    ROW = ROW+1
  ENDSCAN
  lnLastRec=RECNO()

  llStat = ( Invoice= xInvoice ) 
  IF llLoopNote 
    llStat = .F.
  ENDIF

  SELECT &TmpHFile
  DO WHILE ( !llStat .AND. !EMPTY(cAdd_Time) )  
    IF Row >= MaxRow .AND. lnCpyCntr = lnNoCopys
      llLoopNote = .T.
      llStat = .T.    
      EXIT
    ENDIF
    llExit = .F.
    lnNotLine = Labels
    lnMemLins = VAL(cAdd_Time)
    DO lpPrtNotPd
    IF lnCpyCntr = lnNoCopys
      REPLACE Labels WITH lnNotLine
    ENDIF
    IF llLoopNote .AND. lnNoCopys > 1 .AND. lnCpyCntr < lnNoCopys
      llExit = .T.
    ELSE
      llLoopNote =.F.    
      IF llExit
        llStat = .T.
        IF Labels <> 1
          llLoopNote = .T.
        ENDIF
      ENDIF
    ENDIF
    EXIT
  ENDDO

  DO lpFooter WITH llStat

  IF lnNoCopys > 1 .AND. lnCpyCntr < lnNoCopys
    lnCpyCntr = lnCpyCntr + 1
    SELECT &TmpLFile
    GOTO lnFrstRec
    SELECT &TmpHFile
    LOOP
  ELSE
    IF llStat  
      lnCpyCntr = 1  
      SELECT &TmpLFile
      IF !EOF()
        GOTO lnLastRec
      ENDIF
      SELECT &TmpHFile
    ENDIF
  ENDIF

  IF llStat  
    LOOP
  ELSE
    llNewDoc = .T.
    SKIP
  ENDIF 
ENDDO
SET DEVICE TO SCREEN
RETURN
*--END of Prg.

*!*************************************************************
*! Name      : lpGetHdr
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 09/02/2001
*!*************************************************************
*! Synopsis : Get information for invoice heder.
*!*************************************************************
PROCEDURE lpGetHdr

*--Get the bill to and ship to address.
SELECT CUSTOMER
SEEK IIF(&TmpHFile..STORE= SPACE(8),'M'+&TmpHFile..ACCOUNT,'S'+&TmpHFile..ACCOUNT+&TmpHFile..STORE)
      
*B804443,1 10/03/2001 Fix the bug of the account is duplicated.[START]
*=lfSolSpAdr()                && function in the main program (ARPINV) to get the adrress.
=lfSolShp()
*B804443,1 10/03/2001.[END]

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

*--Get the description about the codes.

PTERMS     = gfCodDes(&TmpHFile..CTERMCODE,'CTERMCODE')
PSHIPVIA   = gfCodDes(&TmpHFile..SHIPVIA,'SHIPVIA')
= gfRltFld(&TmpHFile..SHIPVIA , @laZone , 'SHIPVIA')
DO CASE
  CASE 'G' $ lcUpsType
    XZN = gfGetZone(lcUpsType,lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))
    XZN = IIF(!EMPTY(XZN),'('+XZN+')',XZN)
  CASE '2' $ lcUpsType
    XZN  = '(12)'
  CASE 'N' $ lcUpsType
    XZN  = '(22)'
  OTHERWISE
    XZN  = ''
 ENDCASE
PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) +XZN

SELECT &TmpLFile
SEEK xInvoice
SELECT &TmpHFile
STORE 0.00 TO xPieces,xSubTotal
llNewDoc   = .F.
lnCpyCntr  = 1
llLoopNote =.F.
RETURN

*!*************************************************************
*! Name      : gfGetZone.
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 09/02/2001
*!*************************************************************
*! Synopsis : Get the zone to be printed in the invoice format.
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'Syszones'),'',Syszones.ZONE)


*!*************************************************************
*! Name      : lpPrtNotPd
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 09/02/2001
*!*************************************************************
*! Synopsis : Print invoice notepad.
*!*************************************************************
PROCEDURE lpPrtNotPd

lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
IF lnNotLine=1 .AND. Row < MaxRow
  =SEEK('C'+xInvoice,'NotePad')
  @ ROW,02 SAY '* -- N O T E S -- *' 
  ROW = ROW + 1 
ENDIF  

DO WHILE lnNotLine <= lnMemLins
  IF Row >= MaxRow
    llExit = .T.
    EXIT
  ELSE
    @ ROW,02 SAY MLINE(NOTEPAD.mNotes,lnNotLine)
    ROW = ROW + 1
  ENDIF
  lnNotLine = lnNotLine + 1
ENDDO

IF !llExit
   @ ROW,02 SAY '* -- END OF NOTES -- *'
   lnNotLine = 1
   ROW = ROW + 1 
ENDIF
SET MEMOWIDTH TO lnOldMemW
RETURN

*!*************************************************************
*! Name      : lfPrtONte()
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 09/02/2001
*!*************************************************************
*! Synopsis : Print Order or invoice line notes.
*!*************************************************************
FUNCTION lfPrtONte

DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= MaxRow
    llExit = .T.
    EXIT
  ELSE
    ROW = ROW + 1
    @ ROW,01 SAY MLINE(Note_Mem,lnNoteLn)
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  ROW = ROW + 1
ENDIF
RETURN
*End...

*:*************************************************************
*: Name      : lfvNoCopy
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 09/02/2001
*: Purpose   : Valid function for Number of Copys
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  = lfvNoCopy ()
*:*************************************************************
*
FUNCTION lfvNoCopy
PARAMETER lcSolShp
PRIVATE lcAlias

IF lnNoCopys <= 0
 =gfModalGen("TRM000000B00000","DIALOG",'','','Numbers of copies should not be less than 1.')  
  lnNoCopys = laOldVal
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*-- End OF lfvNoCopy
*:*************************************************************



*ADEL
*!*************************************************************
*! Name      : lpFooter
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 09/02/2001
*!*************************************************************
*! Synopsis : Print footer.
*!*************************************************************
PROCEDURE lpFooter
PARA llContue

lnOldRec = 0         && Variable to hold the record no.
lcCurrInv= SPACE(6)  && Variable to hold the Current INV#
SELECT &TmpHFile
IF ! llContue
  @ ROW,20 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,57 SAY xPieces   PICTURE '999999'
  @ ROW,74 SAY xSubTotal PICTURE '9999999.99'
  ROW=ROW+1
  WKAMT = Freight + Insur + Cod
  IF WKAMT <> 0
    @ ROW,20 SAY 'TOTAL - F R E I G H T'
    @ ROW,74 SAY WKAMT PICTURE '9999999.99'
    ROW=ROW+1
  ENDIF
  IF Discount<>0
    lcCurrInv = Invoice
    SELECT InvHdr 
    lnOldRec = RECNO()
    SEEK (lcCurrInv)
    lcDiscPcnt = SPACE(10) + STR(DiscPcnt,6,2) + " %"
    IF lnOldRec <= RECCOUNT()
      GOTO lnOldRec
    ENDIF
    SELECT &TmpHFile
    @ ROW,20 SAY 'TOTAL - D I S C O U N T' + lcDiscPcnt
    @ ROW,74 SAY Discount PICTURE '9999999.99'
    ROW=ROW+1
  ENDIF
  *-- Print the tax rate and tax amount 
  IF xTax .AND. xTax_Meth = 'M'

    @ ROW,20 SAY xTax_Desc
    @ Row,34 SAY IIF(!EMPTY(lcTaxRefr),SUBSTR(lcTaxRefr,1,27),'')

    *B804454,1 10/03/2001 Fix the bug print the GST tax from the setup.[START]
    lcStrRate = STR(Tax_Rate,5,2)
    @ ROW,65 SAY lcStrRate + ' %'
    *B804454,1 10/03/2001.[END]

    @ ROW,74 SAY Tax_Amt  PICTURE '9999999.99'
    ROW = ROW + 1

    lcStrRate = STR(Tax_Rate,5,2)
    IF llIsCanada
      lcStrRate = STR(nPstRate,5,2)
      lcHtrRate = STR(nHstRate,5,2)
      @ ROW,20 SAY 'PST '
      @ ROW,65 SAY lcStrRate + ' %'
      @ Row,74 SAY nPstAmt PICTURE '9999999.99'
      ROW = ROW + 1
      @ ROW,20 SAY 'HST '
      @ ROW,65 SAY lcHtrRate + ' %'
      @ Row,74 SAY nHstAmt PICTURE '9999999.99'
      ROW = ROW + 1
    ENDIF
  ENDIF
  
  *-- Print the tax rate and tax amount 
  IF xTax .AND. xTax_Meth = 'A'
    @ ROW,20 SAY xTax_Desc
    @ Row,34 SAY IIF(!EMPTY(lcTaxRefr),SUBSTR(lcTaxRefr,1,27),'')

    *B804454,1 10/03/2001 Fix the bug print the GST tax from the setup.[START]
    lcStrRate = STR(Tax_Rate,5,2)
    @ ROW,65 SAY lcStrRate + ' %'
    *B804454,1 10/03/2001.[END]

    @ ROW,74 SAY Tax_Amt  PICTURE '9999999.99'
    ROW = ROW + 1
 
    lcStrRate = STR(Tax_Rate,5,2)
    IF llIsCanada
      lcStrRate = STR(nPstRate,5,2)
      lcHtrRate = STR(nHstRate,5,2)
      @ ROW,20 SAY 'PST TAX '
      @ ROW,65 SAY lcStrRate + ' %'
      @ Row,74 SAY nPstAmt PICTURE '9999999.99'
      ROW = ROW + 1
      @ ROW,20 SAY 'HST TAX'
      @ ROW,65 SAY lcHtrRate + ' %'
      @ Row,74 SAY nHstAmt PICTURE '9999999.99'
      ROW = ROW + 1
    ENDIF
  ENDIF  
ELSE   &&llContue=.T.
  @ ROW+1,12 SAY 'C O N T I N U E D ...'
ENDIF
   
xNote1 = IIF(Note1<>'*',Note1, '')
xNote2 = IIF(Note2<>'*',Note2, '')
@ 56,18 SAY xNote1 + ' ' + xNote2
xPhone = IIF(!EMPTY(Phone),Phone,'')
@ 57,02 SAY xPhone

IF !EMPTY(cFacCode) AND SEEK('M'+Account,"Customer")
 @ 58,02 SAY 'FACTOR # : ' + Customer.FactAcct
ENDIF

IF !EMPTY(lcRpMsg1+lcRpMsg2)
  @ 59,10 SAY lcRpMsg1
  @ 60,10 SAY lcRpMsg2
ENDIF
*B123037,1 ASH 06/09/2004 (Begin) Fix the bug of printing the invoice in 2 pages.
IF !EMPTY(Approval) .AND. UPPER(Approval)<>'DEC' .AND. !llContue
  @ 62,40 SAY 'APPROVAL: ' + Approval
ENDIF
*B123037,1 ASH 06/09/2004 (End)
@ 63,02 SAY Piktkt
@ 63,11 SAY ShipDate
IF ! llContue
  @ 63,20 SAY Cartons PICTURE '@Z 999'
  @ 63,26 SAY Weight  PICTURE '@Z 99999'
  @ 64,71 SAY TotalChg PICTURE '9999999.99'
ELSE   &&llContue=.T.
  @ 64,20 SAY '***'
  @ 64,26 SAY '*****'
  @ 64,71 SAY '******.**'
ENDIF
RETURN

*B804443,1 10/03/2001 Fix the bug of the account is duplicated.[START]
*!*************************************************************
*! Name      : lfSolShp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/03/2001
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSolShp
PARAMETER lcSolShp
PRIVATE lcAlias , lcInvHdr

STORE '' TO lcShpTName

lcInvHdr = SELECT(0)
SELECT INVHDR

DECLARE laSoldTo[5,1] , laShipTo[5,1] , laFactor[5,1]

laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laFactor = ''

IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

IF OrdHdr.Alt_ShpTo
  lcShpTName  = OrdHdr.STName  
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  laShipTo[4] = OrdHdr.cAddress4
  laShipTo[5] = OrdHdr.cAddress5
ELSE 
  lnCUSRec = 0
  lcAlias = SELECT(0)
  SELECT CUSTOMER
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
  ENDIF

  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  
  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF BETWEEN(lnCusRec , 1 , RECCOUNT('CUSTOMER'))
    GOTO lnCusRec IN CUSTOMER
  ENDIF  
  SELECT(lcAlias)
ENDIF

=lfAdrShift('laShipTo')

SELECT(lcInvHdr)
RETURN ''

*-- End of lfSolShp.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/03/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVOL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*-- End of lfAdrShift.
*B804443,1 10/03/2001.[END]