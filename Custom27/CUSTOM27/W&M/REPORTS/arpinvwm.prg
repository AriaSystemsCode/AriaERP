*:***************************************************************************
*: Program file  : ARPINVWM
*: Program desc. : CUSTOM INVOICE FORM (W&M)
*: Date          : 06/29/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh (SSE)
*:***************************************************************************
*: Calls :  
*:         Procedures :  
*:         Functions  : lfLoadVar()
*:                      lfPrtHdr()
*:                      lfPrtDet()
*:                      lfPrtTot()
*:                      lfPrtFotNt()
*:                      lfPrtNote()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : .... 
*:***************************************************************************
*: Example : DO ARPINVWM
*:***************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR W&M C101567
*:***************************************************************************
*: Format layout :
*
*000....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+
*01
*02
*03
*04
*05
*06
*07                                                    REP1       
*08                                                    INVOICE    : ±±±±±±
*09                                                    DATE       : ±±/±±/±±
*10         DUNS # : ±±±±±±±±±±±±                      ACCOUNT    : ±±±±± 
*11                                                    STORE      : ±±±±±±±± 
*12                                                    DEPARTMENT : ±±±±± 
*13                                                    VENDOR#    : ±±±±±±±±±±
*14                                                    CUST. PO.  : ±±±±±±±±±±
*15                                            
*16                                            
*17                                            
*18         BILL TO :                         SHIP TO :                                   
*19         ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±    ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*20         ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±    ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*21         ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±    ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*22         ±±±±±±±±±± ±±± ±±±±±±±            ±±±±±±±±±± ±±± ±±±±±±±      
*23   
*24   
*25   
*26   
*27 ------------------------------------------------------------------
*28 STYLE        COLOR  STYLE DESC.        QTY  UNIT PRICE   NET PRICE
*29 ------------------------------------------------------------------
*30 ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±± 999999 99999999.99 99999999.99
*.. ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±± 999999 99999999.99 99999999.99
*.. ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±± 999999 99999999.99 99999999.99
*.. ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±± 999999 99999999.99 99999999.99
*52 ------------------------------------------------------------------
*53 TERMS : ±±±±±±±±±±±±±±± TOTAL QTY : 999999 SUB TOTAL   99999999.99
*54                                            FREIGHT         9999.99
*55                                            -----------------------
*56                                            TOTAL       99999999.99
*57 ------------------------------------------------------------------
*58  ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*59  ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*60  ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*61
*62
*63
*64
*65
*66
*:**************************************************************************
*C101599,1 SSE 08/11/99 Add the Following to be printed in the footer portion 
*                       1) Add Ship Via (Description)
*                       2) no of Cartons (from invoice header file)
*                       3) Sales Rep1 & Sales Rep2 (Code and name from invoice header file)

*---Initializing Variables [Begin.]  
lcInvoice  = ''                      && to hold invoice number
lnFrmWidth = 66                      && to hold the form width
lnLstDtRow = 52                      && to hold the last detail lines Row (Row = 52) 
lnSCol     = INT((85-lnFrmWidth)/2)  && to hold the start column
STORE 0  TO lnRow , lnTotQty , lnTotAmt , lnInvFrt
STORE SPACE(25) TO lcRep1
llNoRec = .F.  && var declared in MAIN PRG If True program will not execute ENDREPORT Procedure 
*---Initializing Variables [End.]

*C101599,1 Redimension laCodeArr to hold the ship via [Begin]
*DIMENSION laCodeArr[1,3]
DIMENSION laCodeArr[2,3]
laCodeArr[1,2] = "CTERMCODE"
laCodeArr[2,2] = "SHIPVIA"
*C101599,1 Redimension laCodeArr to hold the ship via [End]

lcDuns         = gfGetMemVar('XDUNS')   && to hold the DUNS

*-- lcType  && Variable that is hold the name of the Footer Notes memo file
*-- lcType = 'FootNote'
IF FILE(gcDataDir+lcType+'.MEM')
  RESTORE FROM gcDataDir+lcType+'.MEM' ADDITIVE
ENDIF

IF !USED('SALESREP')
  = gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
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

*-- copy data that match criteria in OG into Temp. File [Begin.]
SELECT INVHDR
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")
lcRpExp = "INVOICE = '' AND " + lcRpExp
COPY ALL FOR &lcRpExp TO &gcWorkDir.&lcDumFile
IF !USED(lcDumFile)
  *-- there is record in SYREPUVR file for gfTempName called lcDumFile
  *-- this will be used to carry the INVOICE HEADER TEMP. File
  = gfOpenFile('&gcWorkDir.&lcDumFile',' ','EX')
ENDIF  
*-- copy data that match criteria in OG into Temp. File [End.]

*-- ask about the state of Temp File (Empty/Not Empty) [Begin.]
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
*-- ask about the state of Temp File (Empty/Not Empty) [End.]

*-- Close Temporary file [Begin.]
IF USED(lcDumFile)
  USE IN (lcDumFile)
ENDIF  
IF FILE(gcWorkDir+lcDumFile+'.DBF')
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
ENDIF  
*-- Close Temporary file [End.]

SET DEVICE TO SCREEN
IF llNoRec          && QUIT push button is pressed 
  SET PRINTER TO
ENDIF

IF USED('SALESREP')
  USE IN SALESREP
ENDIF

IF USED(lcDumFile)
  SELECT (lcDumFile)
  SET RELATION TO
  USE IN (lcDumFile)
ENDIF
IF FILE(gcWorkDir+lcDumFile+'.DBF')
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
ENDIF  

*-- End of Report.


*!**************************************************************************
*! Name      : lfMainLoop
*! Developer : Sameh (SSE)
*! Date      : 06/29/1999
*! Purpose   : Execute the main report program
*!**************************************************************************
*! Called from : ARPINVWM.PRG
*!**************************************************************************
*! Calls       : lfLoadVar() , lfPrtHdr() , lfPrtDet()  
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfMainLoop
*!**************************************************************************
*
FUNCTION lfMainLoop
*-- Main loop.
SELECT (lcDumFile)
SET RELATION TO INVOICE INTO INVHDR
GO TOP

CLEAR TYPEAHEAD
SET DEVICE TO PRINT
llLineUp2 = .F.
SCAN REST WHILE INKEY() <> 32
  WAIT WINDOW 'PRINTING INVOICE ' + INVOICE + ' - <Space Bar> TO ABORT' NOWAIT
  DO WHILE .T.
    STORE 0 TO lnTotQty , lnTotAmt
    lcInvoice = Invoice          && to save the current invoice number
    =lfLoadVar()
    =lfPrtHdr()
    =lfPrtDet()
    REPLACE INVHDR.PRTFLAG WITH "P"
    IF llLineUp2
      llLineUp2 = .F.
      *-- Message : --ANOTHER LINE UP <YES>  <NO>  <QUIT>--
      lnChoice = gfModalGen('QRM40143B40012','DIALOG')
      DO CASE
        CASE lnChoice = 2    && press NO
          llLineUp =.F.
        CASE lnChoice = 3    && press QUIT
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
    ELSE                     && EXIT DO..WHILE loop 
      EXIT
    ENDIF
    EXIT      
  ENDDO    
ENDSCAN
*-- End of lfMainLoop.
*-- Main Loop [End.]


*!**************************************************************************
*! Name      : lfLoadVar
*! Developer : Sameh (SSE)
*! Date      : 06/29/1999
*! Purpose   : Collecting Data for the main report
*!**************************************************************************
*! Called from : ARPINVWM.PRG
*!**************************************************************************
*! Calls       : gfCodDes(),lfSolSpAdr()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfLoadVar
*!**************************************************************************
*
FUNCTION lfLoadVar
PRIVATE lnRecNo
lnInvFrt = INVHDR.FREIGHT            && to hold the INVHDR FREIGHT value
lcTerms = ''
laCodeArr[1,1] = INVHDR.CTERMCODE

*C101599,1 adding ShipVia from InvHdr to laCodeArr  [Begin]
laCodeArr[2,1] = INVHDR.SHIPVIA
*C101599,1 adding ShipVia from InvHdr to laCodeArr  [End]

=gfCodDes(@laCodeArr)                && this global function is modified to fill the value into array
lcTerms = laCodeArr[1,3]

lcRep1     = SPACE(25)
IF InvHdr.Rep1  = 'RS '		&& Hardcoded for the customer FAR 05/15/96
  lcRep1 = IIF(SEEK(InvHdr.Rep1,"SALESREP"), SUBSTR(Name,1,25), SPACE(25))
ENDIF

*-- Get the bill to and ship to address
*-- lfSolSpAdr called from Main Prg because it is also defined there. 
*-- and lfAdrShift() is called within lfSolSpAdr()  
lnRecNO=RECNO(lcDumFile)
=lfSolSpAdr()
*-- adjust record pointer
SELECT (lcDumFile)
IF BETWEEN(lnRecNO,1,RECCOUNT())
  GOTO lnRecNO
ENDIF
*-- End of lfLoadVar.

*!**************************************************************************
*! Name      : lfPrtHdr
*! Developer : Sameh (SSE)
*! Date      : 06/29/1999
*! Purpose   : Print the invoice header
*!**************************************************************************
*! Called from : ARPINVWM.PRG
*!**************************************************************************
*! Calls       : None
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfPrtHdr()
*!**************************************************************************
*
FUNCTION lfPrtHdr
SELECT (lcDumFile)
*-- start to print the report header till line 25 [Begin.]
@ 07,lnScol+43 SAY lcRep1
@ 08,lnSCol+43 SAY "Invoice #  : " + INVOICE
@ 09,lnSCol+43 SAY "Date       : " + DTOC(INVDATE)
@ 10,lnSCol+00 SAY "Duns # : " +lcDuns
@ 10,lnSCol+43 SAY "Account    : " + ACCOUNT
@ 11,lnSCol+43 SAY "Store      : " + STORE
@ 12,lnSCol+43 SAY "Department : " + DEPT
@ 13,lnSCol+43 SAY "Customer po: " + SUBSTR(CUSTPO,1,10)
@ 14,lnSCol+43 SAY "Vendor #   : " + SUBSTR(NOTE1,1,10)
@ 18,lnSCol+00 SAY "Bill to :" + SPACE(25) + "Ship to :"
@ 19,lnSCol+02 SAY lcSolTName + SPACE(4) + lcShpTName
@ 20,lnSCol+02 SAY PADR(laSoldTo[1],30) + SPACE(4) + PADR(laShipTo[1],30)
@ 21,lnSCol+02 SAY PADR(laSoldTo[2],30) + SPACE(4) + PADR(laShipTo[2],30)
@ 22,lnSCol+02 SAY PADR(laSoldTo[3],30) + SPACE(4) + PADR(laShipTo[3],30)
@ 23,lnSCol+00 SAY REPLICATE("-", lnFrmWidth)
@ 24,lnSCol+00 SAY "Style" + SPACE(8) + "Color" + SPACE(2) + "Style Desc." +;
                    SPACE(8) + "Qty" + SPACE(2) + "Unit price" + SPACE(3)  +;
                    "Net price"
@ 25,lnSCol+00 SAY REPLICATE("-", lnFrmWidth)
*-- start to print the report header till line 25 [End.]
lnRow = 26
*-- End of lfPrtHdr.


*!**************************************************************************
*! Name      : lfPrtDet
*! Developer : Sameh (SSE)
*! Date      : 06/29/1999
*! Purpose   : Print the invoice details (lines)
*!**************************************************************************
*! Called from : ARPINVWM.PRG
*!**************************************************************************
*: Calls : 
*:         Procedures : None.
*:         Functions  : lfPrtHdr()
*:                      lfPrtNote()
*:                      lfPrtTot()
*:                      lfPrtFotNt()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfPrtDet()
*!**************************************************************************
*
FUNCTION lfPrtDet
PRIVATE lnAlias
lnAlias = SELECT()
*-- start to print the report detail (Body) from line 25 to end of page [Begin.]
DO WHILE lcInvoice = Invoice
  SELECT INVLINE
  
  SCAN REST WHILE Invoice + STR(lineNo,6) = lcInvoice
  
    IF lnRow >= lnLstDtRow  && If current row greater or equal Last Detail Row (at row = 52)  
      EXIT                  && EXIT Scan..EndScan loop
    ENDIF
  
    @ lnRow,lnSCol+00 SAY SUBSTR(Style,1,lnMajor)
    @ lnRow,lnSCol+13 SAY SUBSTR(Style,lnNonMajSt,lnColorLen)
    @ lnRow,lnSCol+20 SAY IIF(SEEK(Style,"Style"),SUBSTR(Style.DESC,1,15),SPACE(15))
    @ lnRow,lnSCol+36 SAY TotQty       PICTURE "999999"
    @ lnRow,lnSCol+43 SAY Price        PICTURE "99999999.99"
    @ lnRow,lnSCol+55 SAY TotQty*Price PICTURE "99999999.99"
    lnRow    = lnRow + 1
    lnTotQty = lnTotQty + TotQty
    lnTotAmt = lnTotAmt + (TotQty*Price)
  
  ENDSCAN
  
  IF lcInvoice <> InvLine.Invoice   && If current invoice not equal invoice in Invoice Line File
    =lfPrtNote()                    && print current invoice note
    =lfPrtTot(.T.)                  && print Totals (parameter TRUE)
    =lfPrtFotNt()                   && print Footer Notes (3 lines) 
  ELSE                              && else current invoice equal invoice in Invoice Line File but lnRow exceeded lnLstDtRow (at Row = 52)
    =lfPrtTot(.F.)                  && call Print Total Function with PARAMETER FALSE (to print "C O N T I N U E D")
    =lfPrtFotNt()                   && print Footer Notes (3 lines) 
    =lfPrtHdr()                     && Print Invoice Header again for the same invoice
  ENDIF

ENDDO  
*-- start to print the report detail (Body) from line 25 to end of page [End.]
SELECT(lnAlias)
*-- End of lfPrtDet.


*!**************************************************************************
*! Name      : lfPrtTot
*! Developer : Sameh (SSE)
*! Date      : 06/29/1999
*! Purpose   : Print the invoice totals
*!**************************************************************************
*! Called from : ARPINVWM.PRG
*!**************************************************************************
*: Calls : 
*:         Procedures : None.
*:         Functions  : None.
*!**************************************************************************
*! Passed Parameters : llPrtTot To tell either to print the totals or to 
*!                     print the word "CONTINUED"  
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfPrtTot()
*!**************************************************************************
*
FUNCTION lfPrtTot
PARAMETERS llPrtTot
PRIVATE lcToDisp

IF llPrtTot        && IF True print Totals
  @ lnLstDtRow+1,lnSCol+00 SAY REPLICATE("-", lnFrmWidth)

  lnRow = lnRow + 1
  @ lnLstDtRow+2,lnSCol+00 SAY "Terms : " + SUBSTR(lcTerms,1,15) + SPACE(1) + "Total Qty :"
  @ lnLstDtRow+2,lnSCol+36 SAY lnTotQty PICTURE "999999"
  @ lnLstDtRow+2,lnSCol+43 SAY "Sub Total"
  @ lnLstDtRow+2,lnSCol+55 SAY lnTotAmt PICTURE "99999999.99"
  
  lnRow = lnRow + 1
  *C101599,1 Add Ship Via description to the invoice form [Begin]
  @ lnLstDtRow+3,lnSCol+00 SAY "Ship Via : " + laCodeArr[2,3]
  *C101599,1 Add Ship Via description to the invoice form [End]
  @ lnLstDtRow+3,lnSCol+43 SAY "Freight"
  @ lnLstDtRow+3,lnSCol+59 SAY lnInvFrt PICTURE "9999.99"
  
  lnRow = lnRow + 1
  *C101599,1 Add No of Cartons from InvHdr File [Begin]
  @ lnLstDtRow+4,lnSCol+00 SAY "Cartons # "
  @ lnLstDtRow+4,lnSCol+11 SAY InvHdr.Cartons PICTURE"99999"
  *C101599,1 Add No of Cartons from InvHdr File [End]
  @ lnLstDtRow+4,lnSCol+43 SAY REPLICATE("-", 23)

  lnRow = lnRow + 1
  *C101599,1 Add Sales Rep Codes and Names [Begin]
  @ lnLstDtRow+5,lnSCol+00 SAY lfRepCName('lcRepNames')
  *C101599,1 Add Sales Rep Codes and Names [End]
  @ lnLstDtRow+5,lnSCol+43 SAY "Total"
  @ lnLstDtRow+5,lnSCol+55 SAY lnTotAmt + lnInvFrt PICTURE "99999999.99"
ELSE            && Else False print the Word " *** C O N T I N U E D *** "
  lcToDisp = " *** C O N T I N U E D *** "
  @ lnLstDtRow+0,lnSCol+00 SAY REPLICATE("-", lnFrmWidth)
  @ lnLstDtRow+3,lnSCol+((lnFrmWidth-LEN(lcToDisp))/2) SAY lcToDisp
ENDIF  
*-- End of lfPrtTot.


*!**************************************************************************
*! Name      : lfPrtFotNt
*! Developer : Sameh (SSE)
*! Date      : 06/29/1999
*! Purpose   : Print the three lines notes.
*!**************************************************************************
*! Called from : ARPINVWM.PRG
*!**************************************************************************
*: Calls : 
*:         Procedures : None.
*:         Functions  : None.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfPrtFotNt()
*!**************************************************************************
*
FUNCTION lfPrtFotNt
*-- lcRpFoot1 , lcRpFoot2 , lcRpFoot3 is restored from MEM File 
@ lnLstDtRow+8,lnSCol+00 SAY REPLICATE("-", lnFrmWidth)
@ lnLstDtRow+9,lnSCol+00 SAY  lcRpFoot1
@ lnLstDtRow+10,lnSCol+00 SAY lcRpFoot2
@ lnLstDtRow+11,lnSCol+00 SAY lcRpFoot3
*-- End of lfPrtFotNt.


*!**************************************************************************
*! Name      : lfPrtNote
*! Developer : Sameh (SSE)
*! Date      : 06/29/1999
*! Purpose   : Print the invoice notes.
*!**************************************************************************
*! Called from : ARPINVWM.PRG
*!**************************************************************************
*: Calls : 
*:         Procedures : None.
*:         Functions  : None.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfPrtNote()
*!**************************************************************************
*
FUNCTION lfPrtNote
PRIVATE lnMemWidth, lnTotLines, lnCurLine
IF llRpInvNot AND SEEK("C"+&lcDumFile..INVOICE,"NOTEPAD")   && If llRpInvNot (print inoice NotePad) 
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnTotLines = MEMLINES(NotePad.mNotes)
  IF lnTotLines > 0
    IF lnRow >= lnLstDtRow
      =lfPrtTot(.F.)
      =lfPrtFotNt()
      =lfPrtHdr()
    ENDIF
    lnRow     = lnRow + 1
    lnCurLine = 1
    DO WHILE lnCurLine <= lnTotLines
      IF lnRow < lnLstDtRow
        @ lnRow,lnSCol+0 SAY MLINE(NotePad.mNotes,lnCurLine)
        lnRow     = lnRow + 1
        lnCurLine = lnCurLine + 1
      ELSE
        llNothing = lfPrtTot(.F.)
        llNothing = lfPrtFotNt()
        llNothing = lfPrtHdr()
      ENDIF
    ENDDO
    lnRow  = lnRow + 1
  ENDIF  
  SET MEMOWIDTH TO lnMemWidth
ENDIF
*-- End of lfPrtNote.

*!**************************************************************************
*! Name      : lfvFootNot
*! Developer : Sameh (SSE)
*! Date      : 06/28/1999
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!***************************************************************************
*! Called from : Option Grid    [Optional Message option]
*!***************************************************************************
*! Calls       : gfOptMsg()
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : = lfvFootNot()
*!***************************************************************************
*
FUNCTION lfvFootNot
PARAMETER lcReturn
PRIVATE laFootNote       
DECLARE laFootNote[3,2]   && Array to hold the name and length of the variables to be used in the Legal Liability screen

laFootNote[1,1] = 'lcRpFoot1'        && 1st. footer line Variable
laFootNote[2,1] = 'lcRpFoot2'        && 2nd. footer line Variable
laFootNote[3,1] = 'lcRpFoot3'        && 3rd. footer line Variable
laFootNote[1,2] = 60                 && Line length

IF FILE(gcDataDir+lcType+'.MEM')
  RESTORE FROM gcDataDir+lcType+'.MEM' ADDITIVE
ENDIF

lcReturn= gfOptMsg('laFootNote','Invoice footer notes')    && Call Function to write Invoice footer notes
    
SET MEMOWIDTH TO 60
SAVE TO gcDataDir+lcType+'.MEM' ALL LIKE lcRpFoot*

RETURN lcReturn
*-- End of lfvFootNot.


*!**************************************************************************
*! Name      : lfRepCName
*! Developer : Sameh (SSE)
*! Date      : 08/11/1999
*! Purpose   : Function to return the sales rep codes and names
*!***************************************************************************
*! Called from : lfPrtTot()
*!***************************************************************************
*! Passed Parameters : lcRepNames
*!***************************************************************************
*! Return      : Sales Rep Code and Name in one string
*!***************************************************************************
*! Example     : = lfRepCName('lcRepNames')
*!***************************************************************************
*C101599,1 this function is due to this Enhancement in this custom prog. 
FUNCTION lfRepCName
PARAMETERS lcCodeName
lcCodeName = IIF(SEEK(InvHdr.Rep1,'SalesRep'),SalesRep.RepCode + ' ' + ; 
                 SUBSTR(SalesRep.Name,1,15),'')
lcCodeName = IIF(!EMPTY(lcCodeName),ALLTRIM(lcCodeName) + ' , ' + IIF(SEEK(InvHdr.Rep2,'SalesRep'), ;
                 SalesRep.RepCode + ' ' + ALLTRIM(SUBSTR(SalesRep.Name,1,15)),''), ;
                IIF(SEEK(InvHdr.Rep2,'SalesRep'),SalesRep.RepCode + ' ' + ; 
                ALLTRIM(SUBSTR(SalesRep.Name,1,15)),''))
RETURN lcCodeName
*-- End of lfRepCName.