*:***************************************************************************
*: Program file    : POSTYHGM
*: Program desc. : P/O Header report for GMA.
*: For Report     : 
*: System          : Aria Advantage Series.
*: Module           : STYLE PURCHASE ORDER (PO)
*: Developer      : MOHAMED SHOKRY (MHM)
*:***************************************************************************
*: Modifications:
*:C102576,1 MHM 03/03/2002 Add additional filter in the grid that will enable 
*:C102576,1 MHM            user to select purchase orders that are assigned for 
*:C102576,1 MHM            particular customer warehous
*:B121028,1 MHM 01/04/2004 Add Bid Status "modified only in SYREPUVR"
*:***************************************************************************
*
XFILTER    = lcRpExp

*-- SELECT REPORT FILE & INITIALIZE MEMORY
SELECT POSHDR

*:C102576,1 MHM 03/03/2002 actviate filter in all cases Except Location [Start]
*SET FILTER TO
*XFILTER = STRTRAN(XFILTER,"POSHDR.","")
*STORE 0 TO lnTransit
*WAIT 'Selecting records for report ...' WINDOW NOWAIT
*LOCATE ALL FOR &XFILTER
WORKFILE = gfTEMPNAME()
=lfColIData()
*:C102576,1 MHM 03/03/2002 [End]

IF EOF()
  * WAIT 'NO RECORDS SELECTED FOR REPORT!' WINDOW NOWAIT
  *-- Message : There are no records to display...!
  *--                < Ok > 
  WAIT CLEAR
  =gfModalGen('TRM00052B40011','ALERT')
  
  SET DEVICE TO SCREEN
  RETURN
ENDIF

lcVenFile   = 'ApVendor'
lcVenField  = 'cVenComp'

R_TITLE  = 'STYLE PURCHASE ORDER HEADER REPORT'
XREPORT  = 'POSTYHGM'
XPRINTNOTE = lcRPNote
XTITLE     = lcRPTitle

*-- This part for sort options (Start)
SORTFIELD = ' '
BREAK     = ' '
DO CASE
  * SUMMARY BY ORDER#
  CASE lcRPSortBy = 'P'

    SORTFIELD = 'PO'

  * SUMMARY BY VENDOR ACCOUNT
  CASE lcRPSortBy = 'V'

    SORTFIELD = 'VENDOR+PO'
    BREAK = 'VENDOR'
    TOTALD = 'VENDOR'

  * SUMMARY BY PRIOIRTY
  CASE lcRPSortBy = 'D'
    SORTFIELD = 'SYS(11,COMPLETE)+VENDOR+PO'
    BREAK = 'DTOC(COMPLETE)'
    TOTALD= 'COMPLETE DATE'
  
  *:C102576,1 MHM 03/03/2002 add warehous to Sort By [Start]
  * SUMMARY BY Location
  CASE lcRPSortBy = 'W'

    SORTFIELD = 'CWARECODE+PO'
    BREAK = 'CWARECODE'
    TOTALD = 'LOCATION'
  *:C102576,1 MHM 03/03/2002 [End]

ENDCASE
*-- This part for sort options (End)

*:C102576,1 MHM 03/03/2002 we change the way of collecting data [Start]
*WORKFILE = gfTEMPNAME()
*COPY REST TO &gcWorkDir.&WORKFILE FOR &XFILTER
*:C102576,1 MHM 03/03/2002 [End]

SELECT POSHDR
SET RELATION TO

*:C102576,1 MHM 03/03/2002 we change the way of collecting data [Start]
*SELECT 0
*USE &gcWorkDir.&WORKFILE EXCLUSIVE
*:C102576,1 MHM 03/03/2002 [End]

SELECT (WORKFILE)
* SORT TO WORKFILE INDEX
IF SORTFIELD<>' '
  Z = LTRIM(STR(RECCOUNT(),7))
  WAIT 'SORTING &Z RECORDS FOR STYLE PURCHASE ORDER Header REPORT ...' WINDOW NOWAIT
  INDEX ON &SORTFIELD TAG &WORKFILE OF (gcWorkDir+WORKFILE+'.CDX')
  SET ORDER TO TAG &WORKFILE
ENDIF

SET RELATION TO VENDOR INTO &lcVenFile


SELECT (WORKFILE)
SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN
IF USED(WORKFILE)
  USE IN (WORKFILE)
ENDIF
ERASE (gcWorkDir+WORKFILE+'.DBF')
ERASE (gcWorkDir+WORKFILE+'.CDX')

*-- end of code.

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

= lfvRepForm()
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfChckAP
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : Check if the AP Module has been setuped for the system
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChckAP()
*!*************************************************************
FUNCTION lfChckAP
IF USED()
  llClose = .F.
  lcCurOrder = ORDER('SycComp')
  SET ORDER TO Ccomp_id IN SycComp
ELSE  
  llClose = .T.
  USE (gcSysHome+'SycComp') ORDER TAG Ccomp_id IN 0
ENDIF  

IF SEEK(gcAct_Comp,'SycComp') AND ('AP' $ SYCCOMP.MMODLSET) 
  llReturn = .T.
ELSE
  llReturn= .F.        
ENDIF

IF llClose
  USE IN SycComp
ELSE
  SET ORDER TO (lcCurOrder) IN SycComp  
ENDIF  

RETURN llReturn

*!*************************************************************
*! Name      : lfvRepForm
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : validate Report format option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRepForm()
*!*************************************************************

FUNCTION lfvRepForm

lnPrnNotePo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPNote'),1)

= lfOGShowGet('lcRPNote')

*!*************************************************************
*! Name      : lfvVendor
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************

FUNCTION lfvVendor

lcVenFld = VARREAD()

lcVendor = EVAL(lcVenFld)

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor 

*!*************************************************************
*! Name      : lfvPO
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : validate purchase order
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************

FUNCTION lfvPO

lcPOFld = VARREAD()

lcPONo = &lcPOFld

IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK('P'+lcPONo , 'POSHDR'))
  DO POSBrow WITH lcPONo,"",'P'
ENDIF

&lcPOFld = lcPONo

*!*************************************************************
*! Name      : lpPrint
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************

PROCEDURE lpPrint

DIMENSION XTOTAL(2,11)
XTOTAL = 0.00
PAGENO = 0
ROW    = 99
XTIME  = TIME()

SELECT &WORKFILE
GOTO TOP

HBREAK = SPACE(1)
IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF
R_WIDTH  = 'N'
SELECT &WORKFILE

*-- REPORT LOOP.
DO WHILE INKEY() <>32

  IF EMPTY(PO)
    WAIT CLEAR
  ELSE
    WAIT 'Printing PO#' + PO + ' - <SPACE BAR> to abort' WINDOW NOWAIT
  ENDIF  
  
  *:C102576,1 MHM 03/03/2002 we change the way of collecting data [Start]
  *lnTransit = lfColInTrn(PO)
  *:C102576,1 MHM 03/03/2002 [End]
   
  IF ROW >=53
    PAGENO = PAGENO+1
    IF BREAK <> ' '
      XREPORT='POSTYHRE'+'  '+'TOTAL :'+TOTALD
    ENDIF

    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
                   
    @ 06,00 SAY 'PO#    VENDOR   S DV ENTERED  COMPLETE ORDERED IN-TRANS. RECEIVED DMG/CNL   OPEN'
    ROW = 7
  ENDIF

  DO WHILE .NOT. EMPTY(BREAK)
  IF &BREAK = HBREAK
    EXIT
  ENDIF
    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('=',80)
    ROW = ROW+1

    @ ROW,000 SAY HBREAK

    @ ROW,39 SAY XTOTAL(1,6)  PICTURE '9999999'
    @ ROW,49 SAY XTOTAL(1,7)  PICTURE '9999999'
    @ ROW,58 SAY XTOTAL(1,8)  PICTURE '9999999'
    @ ROW,66 SAY XTOTAL(1,9)  PICTURE '9999999'
    @ ROW,71 SAY IIF(XTOTAL(1,10)>0,XTOTAL(1,10),0) PICTURE '9999999'

    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',80)
    ROW = ROW+1

    *-- INITIALIZE SUBTOTALS.
    X = 1
    DO WHILE X<=11
      XTOTAL(2,X) = XTOTAL(2,X) + XTOTAL(1,X)
      XTOTAL(1,X) = 0.00
      X = X + 1
    ENDDO

    HBREAK = &BREAK
    EXIT
  ENDDO
  *-- END SUBTOTALS.

  IF EOF()
    EXIT
  ENDIF

  IF ROW >= 53
    ROW = 99
    LOOP
  ENDIF

  SELECT &WORKFILE
  XPO = PO
* ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
* PO#    VENDOR   S DI ENTERED  COMPLETE ORDERED RECEIVED  DAMAGE  CANCLD.    OPEN
* 123456 12345678 X 12 99/99/99 99/99/99 1234567  1234567 1234567 1234567  1234567
* * GRAND TOTAL *                       12345678 12345678 1234567 1234567 12345678                1
*@ 06,00 SAY ' PO#    VENDOR   S DV ENTERED  COMPLETE ORDERED IN-TRANSIT RECEIVED DMG/CNL   OPEN'
  @ ROW,00 SAY PO
  @ ROW,07 SAY VENDOR
  @ ROW,16 SAY STATUS
  
  @ ROW,18 SAY SUBSTR(cDIVISION,1,2)
    
  @ ROW,21 SAY ENTERED
  @ ROW,30 SAY COMPLETE
  @ ROW,39 SAY nStyORDER         PICTURE '9999999'

  *:C102576,1 MHM 03/03/2002 we change the way of collecting data [Start]
  *@ ROW,49 SAY lnTransit         PICTURE '9999999'
  @ ROW,49 SAY nIn_Trans         PICTURE '9999999'
  *:C102576,1 MHM 03/03/2002 [End]

  @ ROW,58 SAY RECEIVE           PICTURE '9999999'
  @ ROW,66 SAY (DAMAGE + CANCEL) PICTURE '9999999'
  
  *:C102576,1 MHM 03/03/2002 we change the way of collecting data [Start]
  *@ ROW,71 SAY IIF((OPEN -lnTransit)<0,0,(OPEN -lnTransit)) PICTURE '9999999'
  @ ROW,71 SAY IIF((OPEN - nIn_Trans)<0,0,(OPEN -nIn_Trans)) PICTURE '9999999'
  *:C102576,1 MHM 03/03/2002 [End]

  XTOTAL(1,06) = XTOTAL(1,06) + nStyORDER
  
  *:C102576,1 MHM 03/03/2002 we change the way of collecting data [Start]
  *XTOTAL(1,07) = XTOTAL(1,07) + lnTransit
  XTOTAL(1,07) = XTOTAL(1,07) + nIn_Trans
  *:C102576,1 MHM 03/03/2002 [End]
  
  XTOTAL(1,08) = XTOTAL(1,08) + RECEIVE
  XTOTAL(1,09) = XTOTAL(1,09) + (DAMAGE + CANCEL)

  *:C102576,1 MHM 03/03/2002 we change the way of collecting data [Start]
  *XTOTAL(1,10) = XTOTAL(1,10) + IIF((OPEN -lnTransit) < 0,0,(OPEN -lnTransit))
  XTOTAL(1,10) = XTOTAL(1,10) + IIF((OPEN - nIn_Trans) < 0,0,(OPEN - nIn_Trans))
  *:C102576,1 MHM 03/03/2002 [End]

  ROW=ROW+1
  IF XPRINTNOTE = 'Y'
    DO PRT_NOTE WITH 'P',XPO
  ENDIF
  SELECT &WORKFILE
  SKIP
ENDDO
*------------------ END MAIN REPORT LOOP --------------------
X = 2
IF LEN(TRIM(BREAK)) =0
  X =1
ENDIF

ROW=ROW+2
@ ROW,00 SAY REPLICATE('*',80)
ROW = ROW+1

@ ROW,00 SAY '* GRAND TOTAL *'
@ ROW,38 SAY XTOTAL(X,6)  PICTURE '99999999'
@ ROW,48 SAY XTOTAL(X,7)  PICTURE '99999999'
@ ROW,58 SAY XTOTAL(X,8)  PICTURE '9999999'
@ ROW,67 SAY XTOTAL(X,9)  PICTURE '999999'
@ ROW,71 SAY IIF(XTOTAL(X,10)>0,XTOTAL(X,10),0) PICTURE '9999999'

ROW = ROW+1
@ ROW,00 SAY REPLICATE('*',80)
RETURN

***END STY6A0.PRG

*!*************************************************************
*! Name      : lfClearRep
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep
IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfAdrShift
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

FOR lnCount = 1 TO 5
  
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
*!*************************************************************
*! Name      : lfColInTrn
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/10/2001
*! Purpose   : Function to to collect in transit
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColInTrn()
*!*************************************************************
FUNCTION lfColInTrn
PARAMETER lcPo
PRIVATE lnInTran , lnCurntAls
lnCurntAls = SELECT(0)
STORE 0 TO lnInTran
SELECT POSLN
IF SEEK('P'+lcPo)
  SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+ lcPo FOR Trancd = '3'
    lnInTran = TOTQty + lnInTran
  ENDSCAN
ENDIF  
SELECT (lnCurntAls)
RETURN lnInTran

*!*************************************************************
*! Name      : lfColIData
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 03/03/2002
*! Purpose   : Function to collect Data
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColIData()
*!*************************************************************
*!C102576,1 
FUNCTION lfColIData

SELECT POSHDR
= AFIELDS(laTempStru)
DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]

*-- nIn_Trans :  field to calculate in transit
laTempStru[ALEN(laTempStru,1) ,1] = 'nIn_Trans'
laTempStru[ALEN(laTempStru,1) ,2] = 'N'
laTempStru[ALEN(laTempStru,1) ,3] = 8
laTempStru[ALEN(laTempStru,1) ,4] = 0

CREATE DBF (gcWorkDir+WORKFILE) FROM ARRAY laTempStru
=gfOpenFile(gcWorkDir+'&WORKFILE','','EX')
INDEX ON CSTYTYPE+PO+CWARECODE TAG (WORKFILE)


SELECT POSLN
SET ORDER TO 
SET RELATION TO POSLN.CSTYTYPE + POSLN.PO INTO POSHDR 

SCAN FOR &lcRpExp

  WAIT 'Selecting PO#' + PO WINDOW NOWAIT
  
  *--use Location in index if sort by Location 
  IF (lcRPSortBy = 'W') 
    llCheck = !SEEK(POSHDR.CSTYTYPE+ POSHDR.PO +POSLN.CWARECODE,WORKFILE)
  ELSE
    llCheck = !SEEK(POSHDR.CSTYTYPE+ POSHDR.PO ,WORKFILE)
  ENDIF  
  
  IF llCheck
    SELECT (WORKFILE)
    APPEND BLANK
    REPLACE cstytype  WITH POSHDR.cstytype,;
            PO        WITH POSHDR.PO,;
            Vendor    WITH POSHDR.Vendor,;
            Status    WITH POSHDR.Status,;
            cDivision WITH POSHDR.cDivision,;
            Entered   WITH POSHDR.Entered,;
            Complete  WITH POSHDR.Complete,;
            cWareCode WITH POSLN.cWareCode
            *--we need open Ask khalid tommorw
    DO CASE
      CASE POSLN.TRANCD = '1'
        REPLACE nStyOrder  WITH POSLN.TotQty,;
                Open       WITH Open+POSLN.TotQty
      CASE POSLN.TRANCD = '2'
        REPLACE Receive  WITH POSLN.TotQty,;
                Open     WITH Open - POSLN.TotQty
      CASE POSLN.TRANCD = '3'
        REPLACE nIn_Trans  WITH POSLN.TotQty
      CASE POSLN.TRANCD = '4'
        REPLACE Damage  WITH POSLN.TotQty,;
                Open    WITH Open - POSLN.TotQty
      CASE POSLN.TRANCD= '5'
        REPLACE Cancel WITH POSLN.TotQty,;
                Open   WITH Open - POSLN.TotQty
        
    ENDCASE        

  ELSE  && if found update with new Qty

    SELECT (WORKFILE)

    DO CASE
      CASE POSLN.TRANCD = '1'
        REPLACE nStyOrder  WITH nStyOrder+POSLN.TotQty,;
                Open       WITH Open+POSLN.TotQty
      CASE POSLN.TRANCD= '2'
        REPLACE Receive  WITH Receive+POSLN.TotQty,;
                Open     WITH Open - POSLN.TotQty
      CASE POSLN.TRANCD= '3'
        REPLACE nIn_Trans  WITH nIn_Trans +POSLN.TotQty
      CASE POSLN.TRANCD= '4'
        REPLACE Damage  WITH Damage +POSLN.TotQty,;
                Open    WITH Open - POSLN.TotQty
      CASE POSLN.TRANCD= '5'
        REPLACE Cancel WITH Cancel+POSLN.TotQty,;
                Open   WITH Open - POSLN.TotQty
    ENDCASE        
    
  ENDIF
  
ENDSCAN
SELECT POSLN
SET RELATION TO

SELECT (WORKFILE)
LOCATE

