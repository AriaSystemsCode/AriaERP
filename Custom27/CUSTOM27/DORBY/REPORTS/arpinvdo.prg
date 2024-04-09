*:***************************************************************************
*: Program file  : ARPINVDO
*: Program desc. : CUSTOM INVOICE FORM (DORBY)
*: Date          : 06/24/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid (SSE)
*:***************************************************************************
*: Calls : 
*:    Procedures :  
*:    Functions  : lfMainLoop()
*:               : lfLoadVar()
*:               : lfPrintHdr()
*:				 : lfPrntBody()
*:     		     : lfPrntSize()
*:               : lfPrnLnNot()
*:               : lfPrintSku()
*:               : lfPrnNotes()  && not used according to (B802655,1 no 4)
*:               : lfClearRep()
*:***************************************************************************
*: Example : DO ARPINVDO
*:***************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR DORBY C101551
*:***************************************************************************
*:C101551,1
*:B802655,1 SSE 10/03/1999 1) print factor addresses according to the system
*:B802655,1                   Form settings
*:B802655,1 SSE 10/03/1999 2) when invoice is printed , changed print flag 
*:B802655,1                   in the grid to YES
*:B802655,1 SSE 10/03/1999 3) move the Quantity 3 char. to the right
*:B802655,1 SSE 10/03/1999 4) move the Price 2 char. to the right
*:B802655,1 SSE 10/03/1999 5) remove the order notepad from the invoice
*:B802655,1 SSE 10/03/1999 6) print Store Distribution center (DC) , if store
*:B802655,1                   has been assigned to, 
*:B802655,1 SSE 10/03/1999 7) print Customer Vendor# inspite of Customer notes 1
*:C101864,1 NAD 05/30/2000    Not to print the order line notes at all . 
*:C101864,1                   Fix Bug the program print's only one invoice .
*:C102018,1 ASH 10/22/2000 1- Print 16 characters of terms code description.
*:C102018,1                2- Print the number of cartons under the field Pkg. No. (instead of P/T #)
*:B803959,1 BWA 01/11/2001 Add the notepad to the invoice.
*:B804053,1 BWA 03/15/2001  Wrong initialize for the printing of the second batch.
*:B605241,1 SSE 02/24/2002 Fix bug of printing both Pick Ticket # and Cartons. 
*:***************************************************************************    

*---Initializing Variables [Begin.]  
STORE '' TO lcWar1,lcWar2,lcWar3,lcInvoice,lcOrder,lcAccount,lcStore,lcStyle,lcScale,lcQty
STORE 0  TO lnPageNo,lnHdrRow,lnLinRow,lnBotRow,MaxRow,lnPrXRow  
STORE 0  TO lnMemLins,lnNotLine,lnQtyNo,lnColWd 

*B804053,1 BWA 03/15/2001 Initial variable to check if it is the first batch or not in printing.[START]
STORE 0  TO lnCountDo
*B804053,1 [END]

lnInvNo = 22
llNoRec = .F.  && var declared in MAIN PRG If True program will not execute ENDREPORT Procedure 
STORE SPACE(01) TO lcName,lcAdd1,lcAdd2,lcCityCode

*C102018,1 ASH 10/22/2000 (Begin) Increase the terms to be 16 chr.
*STORE SPACE(07) TO lcTerms,lcShipVia
STORE SPACE(07) TO lcShipVia
STORE SPACE(16) TO lcTerms
*C102018,1 ASH 10/22/2000 (End)
STORE SPACE(30) TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcStName,lcStAdd1,lcStAdd2,lcStAdd3
*---Initializing Variables [End.]

*-- Get the company headings Name , Addresses [Begin.]
*-- company heading is printed at the top of each page
DIMENSION laCompAdd[5,1] , laCodeArr[2,3]
laCodeArr[1,2] = "SHIPVIA"
laCodeArr[2,2] = "CTERMCODE"

laCompAdd  = ''
=gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)
lcAddress1 = laCompAdd[1]
lcAddress2 = laCompAdd[2]
lcCity     = laCompAdd[3]
lcState    = laCompAdd[4]
lcZip      = laCompAdd[5]
lcDuns     = gfGetMemVar('XDUNS')
*-- Get the company headings Name , Addresses [End.]

*---Open necessary files [Begin.]
IF !USED('ORDLINE')
  = gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'OrdLinSt','SH')
ENDIF

IF !USED('SALESREP')  
  = gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
ENDIF

IF llMultiWH .AND. !USED('WAREHOUS')  
  =gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
ENDIF
*---Open necessary files [End.]

*-- Get the style major and color [Begin.]
STORE 0 TO lnColorLen,lnNonMajSt
*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
*-- lnMajor Var. is declared in main prg (ARPINV) 
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = 'C'
    lnNonMajSt = laMajSeg[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSeg[lnI,1]='C',;
                 laMajSeg[lnI,3],;
                 lnColorLen + laMajSeg[lnI-1,6] + laMajSeg[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
*-- Get the style major and color [End.]

*-- Main program [Begin.]
SELECT INVHDR
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")
lcRpExp = "INVOICE = '' AND " + lcRpExp
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
  IF !lfMainLoop()
    llNoRec = .T.    && To prevent executing ENDREPORT
  ENDIF
ENDIF

*-- Close Temporary file [Begin.]
IF USED(lcDumFile)
  USE IN (lcDumFile)
ENDIF  
IF FILE(gcWorkDir+lcDumFile+'.DBF')
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
ENDIF  
*-- Close Temporary file [End.]

SET DEVICE TO SCREEN
IF llNoRec  
  SET PRINTER TO
ENDIF
*-- Main program [End.]

*!**************************************************************************
*! Name      : lfMainLoop
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Execute the main report program
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : lfLoadVar() , lfPrintHdr() , lfPrntBody()  
*!**************************************************************************
*! Example     : = lfMainLoop
*!**************************************************************************
*
FUNCTION lfMainLoop
SELECT (lcDumFile)

GO TOP
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*--- Main Loop [Begin.]
llLineUp2 = .F.
SCAN REST WHILE INKEY() <> 32

  lcInvoice = InvHdr.Invoice    && store invoice no to be used in updating prtFlag field
  WAIT WINDOW 'PRINTING INVOICE ' + INVOICE + ' - <Space Bar> TO ABORT' NOWAIT
  DO WHILE .T.
    =lfLoadVar()     && Collect Data for the headings
    =lfPrintHdr()    && Print report Header     
    =lfPrntBody()    && Print report Detail
    IF llLineUp2
      llLineUp2 = .F.
      *-- Message : --ANOTHER LINE UP <YES>  <NO>  <QUIT>--
      lnChoice = gfModalGen('QRM40143B40012','DIALOG')
      DO CASE
        CASE lnChoice = 2    && press NO
          llLineUp =.F.
        CASE lnChoice = 3    && press QUIT
          SET DEVICE TO SCREEN
          IF USED(lcDumFile)
            USE IN (lcDumFile)
          ENDIF
          ERASE &gcWorkDir.&lcDumFile+'.DBF'
          RETURN(.F.)
      ENDCASE
    ENDIF
    IF llLineUp              && if chose YES before from the above message
      llLineUp2 = .T.        && rise this flag true to ask again ANOTHER LINE UP
      LOOP
    ELSE
      EXIT
    ENDIF
    EXIT
  ENDDO

  *B802655,1 Change PrtFlag in InvHdr to detect that this invoice is printed 
  IF gcDevice = 'PRINT'
    SELECT InvHdr
    
    = SEEK(lcInvoice)
    IF PrtFlag <> 'P'
      = RLOCK()        && lock current record to update PrtFlag field in invoice file
      REPLACE PrtFlag WITH 'P'
      UNLOCK
    ENDIF
  ENDIF
  *B802655,1 Change PrtFlag in InvHdr to detect that this invoice is printed

ENDSCAN
WAIT CLEAR
*-- End of lfMainLoop.
*-- Main Loop [End.]
*-- End of Report.

*!**************************************************************************
*! Name      : lfLoadVar
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Collecting Data for the main report
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : gfRltFld(),gfCodDes(),gfGetAdr(),lfAdrShift()
*!**************************************************************************
*! Example     : =lfLoadVar
*!**************************************************************************
*
FUNCTION lfLoadVar
=SEEK(&lcDumFile..Invoice,'INVHDR')
lcInvoice = INVHDR.INVOICE
lcAccount = INVHDR.ACCOUNT
lcStore   = INVHDR.STORE
lcOrder   = INVHDR.ORDER

IF llMultiWH  AND SEEK(INVHDR.CWARECODE,'WAREHOUS')
  lcWar1 = WAREHOUS.CADDRESS1
  lcWar2 = WAREHOUS.CADDRESS2
  lcWar3 = SUBSTR(ALLTRIM(WAREHOUS.CADDRESS3),1,15)+' '+SUBSTR(ALLTRIM(WAREHOUS.CADDRESS4),1,3)+' '+SUBSTR(ALLTRIM(WAREHOUS.CADDRESS5),1,10)
  IF EMPTY(lcWar2)
    lcWar2 = lcWar3
    lcWar3 = ''
  ENDIF
ENDIF

*B802655,1 print factor addresses (Option no 1)[Begin]
*-- If print factor addresses in system form settings true
IF llFactor
  IF !EMPTY(INVHDR.CFACCODE)
    =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcName     = SYCFACT.CFACCOMP
    lcAdd1     = SYCFACT.CADDRESS1
    lcAdd2     = SYCFACT.CADDRESS2
    lcCityCode = SUBSTR(SYCFACT.CADDRESS3,1,15) + ' ' + SUBSTR(SYCFACT.CADDRESS4,1,3) + ' ' + SUBSTR(SYCFACT.CADDRESS5,1,10)
  ELSE
    lcName     = lcCompName
    lcAdd1     = lcAddress1
    lcAdd2     = lcAddress2
    lcCityCode = SUBSTR(lcCity,1,15) + ' ' + SUBSTR(lcState,1,3) + ' ' + SUBSTR(lcZip,1,10)
  ENDIF
ENDIF
*-- EndIf of print factor addresses in system form settings true
*B802655,1 print factor addresses (Option no 1) [End]

IF EMPTY(lcAdd2)
  lcAdd2 = lcCityCode
  lcCityCode = ''
ENDIF

=gfRltFld(INVHDR.CDIVISION , @laDivLName , 'CDIVISION')

STORE '' TO lcShipVia , lcTerms
laCodeArr[1,1] = INVHDR.SHIPVIA
laCodeArr[2,1] = INVHDR.CTERMCODE

=gfCodDes(@laCodeArr)
lcShipVia = laCodeArr[1,3]
lcTerms   = laCodeArr[2,3]

*B802655,1 SSE Comment out, gotten from the main invoice program .
*-- Ship to Address
*IF ORDHDR.ALT_SHPTO
  *lcStName = ALLTRIM(ORDHDR.STNAME)+IIF(!EMPTY(ORDHDR.STORE),' STORE #'+ORDHDR.STORE,'')
  *lcStAdd1 = ORDHDR.CADDRESS1
  *lcStAdd2 = ORDHDR.CADDRESS2
  *lcStAdd3 = SUBSTR(ORDHDR.CADDRESS3,1,15) + ' ' + SUBSTR(ORDHDR.CADDRESS4,1,3) + ' ' + SUBSTR(ORDHDR.CADDRESS5,1,10)
*ELSE
  *lcStName = IIF(EMPTY(CUSTOMER.DBA), ALLTRIM(CUSTOMER.STNAME),ALLTRIM(CUSTOMER.DBA))+;
  *           IIF(!EMPTY(ORDHDR.STORE),' STORE #'+ORDHDR.STORE,'')  
  *=gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo)  
  *=lfAdrShift("laShipTo")
  *lcStAdd1 = laShipTo[1]
  *lcStAdd2 = laShipTo[2]
  *lcStAdd3 = laShipTo[3]
*ENDIF
*--Sold to Address
*lcBtName = CUSTOMER.BTNAME     
*=gfGetAdr('CUSTOMER' , '' , '' , '' , @laSoldTo , '2')
*=lfAdrShift("laSoldTo")
*lcBtAdd1 = laSoldTo[1]
*lcBtAdd2 = laSoldTo[2]
*lcBtAdd3 = laSoldTo[3]
*C101864,1  (Start)  Save the record number before calling the function
lnRecNo=RECNO(lcDumFile)
*C101864,1 (End)
=lfSolSpAdr()  && call this function to get the bill to & ship to address
*C101864,1 (Start) move the pointer to the saved record number
SELECT (lcDumFile)
GOTO lnRecNo 
*C101864,1 (End)
lcStName = lcShpTName
lcStAdd1 = laShipTo[1]
lcStAdd2 = laShipTo[2]
lcStAdd3 = laShipTo[3]

lcBtName = lcSolTName
lcBtAdd1 = laSoldTo[1]
lcBtAdd2 = laSoldTo[2]
lcBtAdd3 = laSoldTo[3]
*B802655,1 SSE END
*-- End of lfLoadVar.

*!**************************************************************************
*! Name      : lfPrintHdr  
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Print INVOICE HEADER
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Example     : =lfPrintHdr()
*!**************************************************************************
*
FUNCTION lfPrintHdr

lnPageNo = lnPageNo + 1

*B804053,1 BWA 03/15/2001 Increment the variable for each invoice.[START]
lnCountDo = lnCountDo + 1
*B804053,1 [END]

IF lnInvNo = 22
  lnHdrRow = 03

  *B804053,1 BWA 03/15/2001 Check if the first batch or not.[START]
  *lnLinRow = 28
  IF lnCountDo = 1
    lnLinRow = 28
  ELSE
    lnLinRow = 29
  ENDIF
  *B804053,1 [END]

  lnBotRow = 47
  lnPrXRow = 49
  MaxRow   = lnBotRow
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 07
  lnLinRow = lnHdrRow + 26
  lnBotRow = lnLinRow + 18
  MaxRow   = lnBotRow
  lnPrXRow = lnLinRow + 20
  lnInvNo  = lnInvNo  + 01
ENDIF

lcOldSize = ''
*-- print Form [Begin.]

@ lnHdrRow,04 SAY LEFT(IIF(EMPTY(lcDivLName),ALLTRIM(lcCompName),lcDivLName),38)
@ lnHdrRow,43 SAY ALLTRIM(lcName)
lnHdrRow = lnHdrRow + 1        &&Position 4

@ lnHdrRow,04 SAY lcAddress1
@ lnHdrRow,43 SAY lcAdd1
lnHdrRow = lnHdrRow + 1        &&Position 5      

@ lnHdrRow,04 SAY lcAddress2
@ lnHdrRow,43 SAY lcAdd2
lnHdrRow = lnHdrRow + 1        &&Position 6

@ lnHdrRow,04 SAY lcCity + ' ' + lcState + ' ' +lcZip
@ lnHdrRow,43 SAY lcCityCode
lnHdrRow = lnHdrRow + 2        &&Position 8

DO CASE 
  CASE INVHDR.CDIVISION = 'MD'
    lcDuns = '09-208-0480'
  CASE INVHDR.CDIVISION = 'LD'
    lcDuns = '09-208-0480'
  CASE INVHDR.CDIVISION = 'DF'
    lcDuns = '00-150-9538'
ENDCASE
@ lnHdrRow,09 SAY lcDuns
*-- print Form [End.]

lnHdrRow = lnHdrRow + 3        &&Position 11
@ lnHdrRow,56 SAY SUBSTR(lcWar1,1,20)

lnHdrRow = lnHdrRow + 1        &&Position 12
@ lnHdrRow,56 SAY SUBSTR(lcWar2,1,20)

lnHdrRow = lnHdrRow + 1        &&Position 13
@ lnHdrRow,56 SAY SUBSTR(lcWar3,1,20)

lnHdrRow = lnHdrRow + 3        &&Position 16
@ lnHdrRow,04 SAY lcBtName
@ lnHdrRow,43 SAY lcStName

lnHdrRow = lnHdrRow + 1        &&Position 17
@ lnHdrRow,04 SAY lcBtAdd1
@ lnHdrRow,43 SAY lcStAdd1 

lnHdrRow = lnHdrRow + 1        &&Position 18
@ lnHdrRow,04 SAY lcBtAdd2
@ lnHdrRow,43 SAY lcStAdd2

lnHdrRow = lnHdrRow + 1        &&Position 19
@ lnHdrRow,04 SAY lcBtAdd3
@ lnHdrRow,43 SAY lcStAdd3  

lnHdrRow = lnHdrRow + 2        &&Position 21
@ lnHdrRow,09 SAY lcAccount 

lnHdrRow = lnHdrRow + 1        &&Position 22
@ lnHdrRow,09 SAY lcStore
*B802655,1 print Customer Vendor# inspite of Customer short notes 1
*@ lnHdrRow,20 SAY IIF(SEEK('M'+lcAccount,'CUSTOMER'),SUBSTR(CUSTOMER.Note,1,9),'')
@ lnHdrRow,20 SAY IIF(SEEK('M'+lcAccount,'CUSTOMER'),SUBSTR(CUSTOMER.cCusVend,1,9),'')
*B802655,1 SSE END
@ lnHdrRow,29 SAY INVHDR.DEPT
@ lnHdrRow,43 SAY PADR(lcShipVia,15)
@ lnHdrRow,62 SAY SUBSTR(INVHDR.CUSTPO,1,10)

lnHdrRow = lnHdrRow + 3        &&Position 25
@ lnHdrRow,01 SAY INVHDR.InvDate
@ lnHdrRow,12 SAY lcInvoice
*C102018,1 ASH 10/22/2000 (Begin) Print 16 chr of the terms.
*@ lnHdrRow,22 SAY PADR(lcTerms,7)
@ lnHdrRow,22 SAY PADR(lcTerms,16)
*C102018,1 ASH 10/22/2000 (End)
@ lnHdrRow,41 SAY lcOrder
*C102018,1 ASH 10/22/2000 (Begin) Print the no. of cartons instead of P/T#.
*@ lnHdrRow,50 SAY INVHDR.PikTkt

*B605241,1 Print both Cartons and Pick Ticket #. [Begin]
*@ lnHdrRow,50 SAY INVHDR.Cartons
@ lnHdrRow,49 SAY INVHDR.PikTkt
@ lnHdrRow,56 SAY INVHDR.Cartons PICTURE '999'
*B605241,1 Print both Cartons and Pick Ticket #. [End]

*C102018,1 ASH 10/22/2000 (End)
@ lnHdrRow,60 SAY INVHDR.Rep1
@ lnHdrRow,66 SAY INVHDR.DueDate

lnHdrRow = lnHdrRow + 1        &&Position 26
@ lnHdrRow,09 SAY INVHDR.Note1+INVHDR.Note2
*-- End of lfPrintHdr.

*!**************************************************************************
*! Name      : lfPrntBody  
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Print INVOICE BODY
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : lfPrintHdr(),lfPrnLnNot(),lfPrintSku(),lfPrnNotes()
*!**************************************************************************
*! Example     : =lfPrntBody()
*!**************************************************************************
*
FUNCTION lfPrntBody
PRIVATE lnAlias

*B803959,1 BWA 01/11/2001 Add the notepad to the invoice.[START]
llPrnFot = .T.    && Variable to check if print the footer in the notepad function or not.
*B803959,1 [END]

lnAlias = SELECT(0)
SELECT INVLINE
STORE '' TO lcSize , lcOldSize
SCAN REST WHILE invoice+STR(lineno,6) = lcInvoice
  lcStyle = STYLE
  lcOrder = Order
  lcStore = STORE
  =lfPrntSize()
  lnLinRow = lnLinRow + 1
  IF lnLinRow > MaxRow             && if lines of rows exceed max row number
    =lfPrintHdr()                  && print report header again 
  ENDIF
  IF lcOldSize <> lcSize
    lcOldSize = lcSize
    @ lnLinRow,18 SAY lcSize
    lnLinRow = lnLinRow + 1
  ENDIF
  @ lnLinRow,01 SAY ALLTRIM(SUBSTR(lcStyle,1,9))
  
  *B802655,1 SSE move the Quantity 3 character to the right  
  *@ lnLinRow,07 SAY TotQty  PICTURE '99999'
  @ lnLinRow,11 SAY TotQty  PICTURE '99999'
  *B802655,1 SSE END
  lcIndex = ''
  lnPos   = 17

  FOR lnIndex = 1 TO lnQtyNo
    lcIndex = ALLTRIM(STR(lnIndex))
    @ lnLinRow,lnPos SAY Qty&lcIndex PICTURE '99999'
    lnPos = lnPos + 5
  ENDFOR

  *B802655,1 SSE move the Price 2 character to the right
  *@ lnLinRow,54 SAY Price PICTURE '999999.99'
  *@ lnLinRow,64 SAY TotQty*Price PICTURE '9999999.99'  
  @ lnLinRow,59 SAY Price PICTURE '99999.99'  
  @ lnLinRow,67 SAY TotQty*Price PICTURE '999999.99'
  *B802655,1 SSE END
  lnLinRow = lnLinRow + 1
  IF lnLinRow >MaxRow              && if lines of rows exceed max row number 
    =lfPrintHdr()                  && print report header again
  ENDIF
  @ lnLinRow,17 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)  
  @ lnLinRow,25 SAY STYLE.DESC
  *C101864,1 (Start) Not to Print the order line notes at all
  *C101864,1 Add a suppress expression in the option grid to suppress the option of 'print 
  *C101864,1 line notes' for Dorby's invoice only 
  *=lfPrnLnNot()
  *C101864,1 (End) 
  IF llRpSkuSize = .T. .AND. SEEK('S'+lcAccount+lcStyle,'SPCK_LIN')
    =lfPrintSku()
  ENDIF
ENDSCAN

lnLinRow = lnLinRow + 2

*B803959,1 BWA 01/11/2001 Add the notepad to the invoice.[START]
IF llRpInvNot AND SEEK('C' + lcInvoice,'NOTEPAD')
  llPrnFot = .F.
  =lfPrntNPad()
ENDIF

IF llPrnFot
*B803959,1 [END]

  IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
    =lfPrintHdr()                    && print report header again 
  ENDIF
  @ lnLinRow,44 SAY 'Total Merchandise :'
  @ lnLinRow,64 SAY INVHDR.SHIPAMT PICTURE '9999999.99'

  lnLinRow = lnLinRow + 1
  IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
    =lfPrintHdr()                    && print report header again 
  ENDIF
  @ lnLinRow,44 SAY 'Freight charges   :'
  @ lnLinRow,64 SAY INVHDR.FREIGHT+INVHDR.INSUR+INVHDR.COD PICTURE '9999999.99'

  IF INVHDR.DISCOUNT<>0
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
      =lfPrintHdr()                    && print report header again 
    ENDIF
    @ lnLinRow,44 SAY 'Discount          :'
    @ lnLinRow,64 SAY INVHDR.DISCOUNT PICTURE '9999999.99'
  ENDIF

  lnLinRow = lnLinRow + 1
  IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
    =lfPrintHdr()                    && print report header again 
  ENDIF
  @ lnLinRow,44 SAY 'Invoice Total     :'
  @ lnLinRow,64 SAY INVHDR.TOTALCHG PICTURE '9999999.99'

  IF !EMPTY(lcRpMsg2)
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
      =lfPrintHdr()                    && print report header again 
    ENDIF
    @ lnLinRow,01 SAY lcRpMsg2
  ENDIF
  IF !EMPTY(lcRpMsg3)
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
      =lfPrintHdr()                    && print report header again 
    ENDIF
    @ lnLinRow,01 SAY lcRpMsg3
  ENDIF

  *B802655,1 SSE remove the order notepad from the invoice
  *IF SEEK('B'+lcOrder,'NotePad')
  *  =lfPrnNotes()
  *ENDIF
  *B802655,1 SSE END

  IF !EMPTY(lcRpMsg1)
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
      =lfPrintHdr()                    && print report header again 
    ENDIF
    @ lnPrXRow,01 SAY lcRpMsg1
  ENDIF

  SELECT(lnAlias)

*B803959,1 BWA 01/11/2001 Add the notepad to the invoice.[START]
ENDIF
*B803959,1 [END]

*-- End of lfPrntBody.

*!**************************************************************************
*! Name      : lfPrntSize  
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Function to get Style Sizes in one string.
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Example     : =lfPrntSize()
*!**************************************************************************
* 
FUNCTION lfPrntSize
lcSize = ''
lcScale = STYLE.SCALE
IF SEEK('S'+ STYLE.SCALE,'SCALE')
  lnQtyNo = SCALE.Cnt
  FOR lnIndex = 1 TO SCALE.Cnt
    lcIndex = ALLTRIM(STR(lnIndex))
    lcSize  = lcSize+PADL(ALLTRIM(SCALE.SZ&lcIndex),4)+ IIF(lnIndex<>SCALE.Cnt,' ','')
  ENDFOR
ELSE
  lcSize = SPACE(40)
ENDIF
*-- End of lfPrntSize.

*!**************************************************************************
*! Name      : lfPrnLnNot
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Function to print the order line notes.
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : lfPrintHdr()
*!**************************************************************************
*! Example     : =lfPrnLnNot()
*!**************************************************************************
*  
FUNCTION lfPrnLnNot
=SEEK('O'+INVLINE.ORDER+INVLINE.STORE+INVLINE.STYLE,'ORDLINE')
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 70
lnMemLins = MEMLINES(ORDLINE.NOTE_MEM)
DO WHILE lnNotLine <= lnMemLins    
  IF !EMPTY(MLINE(ORDLINE.NOTE_MEM,lnNotLine)) 
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
      =lfPrintHdr()                    && print report header again 
    ENDIF
    @ lnLinRow,01 SAY MLINE(ORDLINE.NOTE_MEM,lnNotLine)
  ENDIF
  lnNotLine = lnNotLine + 1
ENDDO     
SET MEMOWIDTH TO lnOldMemW
*-- End of lfPrnLnNot.

*!**************************************************************************
*! Name      : lfPrintSku
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Function to print the style's Skus.
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : lfPrintHdr()
*!**************************************************************************
*! Example     : =lfPrintSku()
*!**************************************************************************
*  
FUNCTION lfPrintSku
PRIVATE lnAlias
lnAlias = SELECT(0)

*-- Getting the SKU template
lnCustRec = RECNO('CUSTOMER')
=SEEK('M'+ORDLINE.ACCOUNT,'CUSTOMER')

lcSkuTmpl = IIF(!EMPTY(CUSTOMER.SKUTMPL),CUSTOMER.SKUTMPL,'DEF')
IF SEEK('S'+lcSkuTmpl,'SKUTMPL')
  lnDime1 = SKUTMPL.LEN1+SKUTMPL.LEN2+SKUTMPL.LEN3
  lnDime2 = SKUTMPL.LEN4
ELSE
  lnDime1 = 8  &&Default
  lnDime2 = 8  &&Default
ENDIF 
IF BETWEEN(lnCustRec,1,RECCOUNT('CUSTOMER'))
  GOTO lnCustRec IN CUSTOMER
ENDIF

*-- Print the Main Sku of the style
lcSkuHdr = "SKU# : "+LEFT(SPCK_LIN.PACK_ID,SKUTMPL.LEN1)+' '+;
           SUBSTR(SPCK_LIN.PACK_ID,SKUTMPL.LEN1+1,SKUTMPL.LEN2)
lnLinRow = lnLinRow + 1
IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
  =lfPrintHdr()                    && print report header again 
ENDIF
@ lnLinRow,01 SAY ALLTRIM(lcSkuHdr)
 
*-- If the user did not select to print the sku by sizes.
IF llRpSkuSize = .F.
  RETURN
ENDIF
 
DIMENSION laSku(8,2)
STORE '' TO laSku 
STORE 0  TO lnCount 

SELECT SPCK_LIN
SCAN REST WHILE TYPE+ACCOUNT+STYLE = 'S' + lcAccount+lcStyle
  lnCount = lnCount + 1
  IF lnCount>8
    lnCount = 8
    EXIT
  ENDIF
  FOR lnQtyn = 1 TO 8
    IF EVAL('SPCK_LIN.Qty'+ALLTRIM(STR(lnQtyn))) = 1
      laSku(lnCount, 1) = STR(lnQtyn)          
      laSku(lnCount, 2) = SUBSTR(PACK_ID,lnDime1+1,lnDime2)
      EXIT
    ENDIF
  ENDFOR
ENDSCAN
=ASORT(laSku,1)
FOR lnQtyn = 1 TO lnCount
  lcQty = ALLTRIM(laSku(lnQtyn,1))
  IF !EMPTY(lcQty)
    IF InvLine.Qty&lcQty <> 0
      IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
        =lfPrintHdr()                    && print report header again 
      ENDIF
      IF lnColWd < 70
        @ lnLinRow,lnColWd SAY "S"+ALLTRIM(laSku[lnQtyn,1])+":"+;
        ALLTRIM(laSku[lnQtyn,2])                 
        lnColWd = lnColWd + LEN(ALLTRIM(laSku[lnQtyn,2])) + 4
      ELSE
        lnLinRow = lnLinRow + 1
        IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
          =lfPrintHdr()                    && print report header again 
        ENDIF
        lnColWd = 6+LEN(ALLTRIM(lcSkuHdr))+1
        @ lnLinRow,lnColWd SAY "S"+ALLTRIM(laSku[lnQtyn,1])+":"+;
        ALLTRIM(laSku[lnQtyn,2])
        lnColWd = lnColWd + LEN(ALLTRIM(laSku[lnQtyn,2])) + 4
      ENDIF
    ENDIF
  ENDIF
ENDFOR
SELECT (lnAlias)
*-- End of lfPrintSku.

*!**************************************************************************
*! Name      : lfPrnNotes
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Function to print the order's notepad.
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : lfPrintHdr()
*!**************************************************************************
*! Example     : =lfPrnNotes()
*!**************************************************************************
*not used anymore according to (B802655,1)  
FUNCTION lfPrnNotes
PRIVATE lnAlias
lnOldMemW = 0
lnMemLins = 0
lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 65
SELECT NotePad
lnMemLins = MEMLINES(NOTEPAD.MNOTES)
DO WHILE lnNotLine <= lnMemLins    
  IF !EMPTY(MLINE(MNOTES,lnNotLine))
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
      =lfPrintHdr()                    && print report header again 
    ENDIF
    @ lnLinRow,01 SAY MLINE(MNOTES,lnNotLine)
  ENDIF
  lnNotLine = lnNotLine + 1
ENDDO     
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)
*-- End of lfPrnNotes.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh (SSE)
*! Date      : 06/24/1999
*! Purpose   : Close OG Function.
*!**************************************************************************
*! Called from : OG < Close > or < Reset > Buttons.
*!**************************************************************************
*! Example     : =lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep    
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF
IF USED('SALESREP')
  USE IN SALESREP
ENDIF
IF USED('WAREHOUS')
  USE IN WAREHOUS
ENDIF
IF USED(lcDumFile)
  USE IN (lcDumFile)
ENDIF
IF FILE(gcWorkDir+lcDumFile+'.DBF')
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
ENDIF  
*-- End of lfClearRep.

*B803959,1 BWA 01/11/2001 Add the notepad to the invoice.[START]
*!**************************************************************************
*! Name      : lfPrntNPad
*! Developer : BASEEM RAFAAT ERNEST(BWA)
*! Date      : 01/11/2001
*! Purpose   : Function to print notepad.
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : ...
*!**************************************************************************
*! Example     : =lfPrntNPad()
*!**************************************************************************
FUNCTION lfPrntNPad

lnNotLine  = 1        && For the notepad
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.

SELECT NOTEPAD
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
lnMemLins = MEMLINES(NOTEPAD.MNOTES)

IF TYPE + KEY = 'C' + lcInvoice
  @ lnLinRow,02 SAY '* -- N O T E S -- *' 
  lnLinRow = lnLinRow + 1
  DO WHILE lnNotLine <= lnMemLins
    IF lnLinRow >= MAXROW
      XNOTE_LOOP = .T.
    ELSE
      XNOTE_LOOP = .F.
      @ lnLinRow,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
      lnLinRow = lnLinRow + 1
    ENDIF
    IF lnLinRow >= MAXROW
      =lfPrintHdr()
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO
  IF !XNOTE_LOOP
    @ lnLinRow,02 SAY '* -- END OF NOTES -- *'
    lnNotLine = 1
    lnLinRow = lnLinRow + 1 
    =lfGetcont()
  ELSE
    =lfGetcont()
    LOOP
  ENDIF
ENDIF
SET MEMOWIDTH TO lnOldMemW

*--End of lfPrntNPad.
*!**************************************************************************
*! Name      : lfGetcont
*! Developer : BASEEM RAFAAT ERNEST(BWA)
*! Date      : 01/11/2001
*! Purpose   : Function to print the footer.
*!**************************************************************************
*! Called from : ARPINVDO.PRG
*!**************************************************************************
*! Calls       : ...
*!**************************************************************************
*! Example     : =lfGetcont()
*!**************************************************************************
FUNCTION lfGetcont

@ lnLinRow,44 SAY 'Total Merchandise :'
@ lnLinRow,64 SAY INVHDR.SHIPAMT PICTURE '9999999.99'

lnLinRow = lnLinRow + 1
IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
  =lfPrintHdr()                    && print report header again 
ENDIF
@ lnLinRow,44 SAY 'Freight charges   :'
@ lnLinRow,64 SAY INVHDR.FREIGHT+INVHDR.INSUR+INVHDR.COD PICTURE '9999999.99'

IF INVHDR.DISCOUNT<>0
  lnLinRow = lnLinRow + 1
  IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
    =lfPrintHdr()                    && print report header again 
  ENDIF
  @ lnLinRow,44 SAY 'Discount          :'
  @ lnLinRow,64 SAY INVHDR.DISCOUNT PICTURE '9999999.99'
ENDIF

lnLinRow = lnLinRow + 1
IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
  =lfPrintHdr()                    && print report header again 
ENDIF
@ lnLinRow,44 SAY 'Invoice Total     :'
@ lnLinRow,64 SAY INVHDR.TOTALCHG PICTURE '9999999.99'

IF !EMPTY(lcRpMsg2)
  lnLinRow = lnLinRow + 1
  IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
    =lfPrintHdr()                    && print report header again 
  ENDIF
  @ lnLinRow,01 SAY lcRpMsg2
ENDIF
IF !EMPTY(lcRpMsg3)
  lnLinRow = lnLinRow + 1
  IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
    =lfPrintHdr()                    && print report header again 
  ENDIF
  @ lnLinRow,01 SAY lcRpMsg3
ENDIF

IF !EMPTY(lcRpMsg1)
  lnLinRow = lnLinRow + 1
  IF lnLinRow >MaxRow                && if lines of rows exceed max row number 
    =lfPrintHdr()                    && print report header again 
  ENDIF
  @ lnPrXRow,01 SAY lcRpMsg1
ENDIF
SELECT(lnAlias)

*--End oflfGetcont.
*B803959,1 [END]