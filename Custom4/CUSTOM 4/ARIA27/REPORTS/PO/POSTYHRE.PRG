*:***************************************************************************
*: Program file  : POSTYHRE
*: Program desc. : P/O Header report.
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYHRE
*:***************************************************************************
*: Modifications:
*B802104,1 BWA 04/01/99 Fix the bug of wrong alignment in short format.
*E301272,5 MAB 06/30/99 Report support multi currency format.(Header,variables and this code)
*B603348,1  ABD 03/27/2000 Increase The Picture Of The Nicost Fildes Upon Width of the filed
*B603348,1                 To 9999999.999 .
*B803328,1 ADEL 06/13/2000 Fix the bug of "Subscript out of bounds" when scrolling OG
*B803328,1                 by arranging Ask-at-Runtime varaibles in syrepuvr.
*B603713,10 HBG 11/22/2000 Increase The Picture Of The Price
*B604490,1 KHM 05/21/2001   Re-Setting the device to screen.
*B605115,1 AMH 11/11/2001 Fix the bug of not displaying the Actualized status
*B605115,1                to the status option in the option grid.
*B605729,1 KHM 03/21/2002 Fix the bug accumolating the landed cost for each PO
*B803993,1 AMH 08/07/2002 Fix the indentation of print by cost.
*B119469,1 ALB 08/28/2003 The report does not print the last 2 char.
*B120159,1 MMM 10/21/2003 Fix the bug of not displaying the Bid status on the Status option 
*B120159,1                in the option grid.  
*B124365,1 NNA 10/24/2004 Fix bug that The report does not print the last character/digit in the UK A4
*:***************************************************************************
*
XFILTER    = lcRpExp
*-- SELECT REPORT FILE & INITIALIZE MEMORY
SELECT POSHDR
SET FILTER TO
XFILTER = STRTRAN(XFILTER,"POSHDR.","")

WAIT 'Selecting records for report ...' WINDOW NOWAIT
LOCATE ALL FOR &XFILTER
IF EOF()
  *MAB WAIT 'NO RECORDS SELECTED FOR REPORT!' WINDOW NOWAIT
  *-- Message : There are no records to display...!
  *--                < Ok > 
  WAIT CLEAR
  =gfModalGen('TRM00052B40011','ALERT')
  
  *B604490,1 KHM 05/21/2001 (Begin) Set the device to screen.
  SET DEVICE TO SCREEN
  *B604490,1 KHM 05/21/2001 (End)
  RETURN
ENDIF

*E301272,5 Assign Currency variables [Begin]
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             (!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0)
llCurInGrp = !llPrintTot
*E301272,5 Assign Currency variables [End  ]

*E301272,5 Add new array hold normal data (Estimated data in currency) [Begin]
*DIMENSION laEST[5] , laLAN[5] , laACT[5]
*STORE 0 TO lnFinEst,lnFinLan,lnFinACT , laACT ,laLan ,laEst
DIMENSION laEST[5] , laLAN[5] , laACT[5] , laNormCost[6]
STORE 0 TO lnFinEst,lnFinLan,lnFinACT , laACT ,laLan ,laEst , laNormCost
*E301272,5 Add new array hold normal data (Estimated data in currency) [End  ]

lcISlbl1  = laCost[1,2]
lcISlbl2  = laCost[2,2]
lcISlbl3  = laCost[3,2]
lcISlbl4  = laCost[4,2]
lcISlbl5  = laCost[5,2]

lcVenFile   = 'ApVendor'
lcVenField  = 'cVenComp'

R_TITLE  = 'STYLE PURCHASE ORDER Header REPORT'
XREPORT  = 'POSTYHRE'
XPRINTNOTE = lcRPNote
XTITLE     = lcRPTitle
XLONG      = IIF(lcRPLngShr='L','Y','N')
lcForm     = lcRPFormat

*-- This part for sort options (Start)
SORTFIELD = ' '
BREAK     = ' '
DO CASE
  * SUMMARY BY ORDER#
  CASE lcRPSortBy = 'P'

    IF llCurInGrp
      SORTFIELD = 'CPRICECUR+PO'
    ELSE
      SORTFIELD = 'PO'
    ENDIF  

  * SUMMARY BY VENDOR ACCOUNT
  CASE lcRPSortBy = 'V'

    IF llCurInGrp
      SORTFIELD = 'VENDOR+CPRICECUR+PO'
      BREAK = 'VENDOR+ "/" + CPRICECUR'
      TOTALD = 'VENDOR/CURRENCY'
    ELSE
      SORTFIELD = 'VENDOR+PO'
      BREAK = 'VENDOR'
      TOTALD = 'VENDOR'
    ENDIF  

  * SUMMARY BY PRIOIRTY
  CASE lcRPSortBy = 'D'
    IF llCurInGrp
      SORTFIELD = 'SYS(11,COMPLETE)+CPRICECUR+VENDOR+PO'
      BREAK = 'DTOC(COMPLETE) +"/" + CPRICECUR'
      TOTALD= 'COMPLETE DATE/CURRENCY'
    ELSE
      SORTFIELD = 'SYS(11,COMPLETE)+VENDOR+PO'
      BREAK = 'DTOC(COMPLETE)'
      TOTALD= 'COMPLETE DATE'
    ENDIF  
ENDCASE
*-- This part for sort options (End)

*IF llMultCurr
  *lcHedTitle = "Print Data in : "
  *DO CASE
  *  CASE lcRpCurr = "F"  && Foreign Currency.
  *    lcHedTitle = lcHedTitle + "Foreign Currency."
  *  CASE lcRpCurr = "O"  && Original Rates
  *    lcHedTitle = lcHedTitle + "Equaivalent based on the Original transactions exchange rates."
  *  CASE lcRpCurr = "D"  && Rates on date
  *    lcHedTitle = lcHedTitle + "Equaivalent based on the exchange rates defined on "+ DTOC(ldRpExDate) + "."
  *  OTHERWISE            && Rates defined now.
  *    lcHedTitle = lcHedTitle + "Equaivalent based on the exchange rates defined now."
  *ENDCASE
*ENDIF

WORKFILE = gfTEMPNAME()
COPY REST TO &gcWorkDir.&WORKFILE FOR &XFILTER

SELECT POSHDR
SET RELATION TO

SELECT 0
USE &gcWorkDir.&WORKFILE EXCLUSIVE

* SORT TO WORKFILE INDEX
IF SORTFIELD<>' '
  Z = LTRIM(STR(RECCOUNT(),7))
  WAIT 'SORTING &Z RECORDS FOR STYLE PURCHASE ORDER Header REPORT ...' WINDOW NOWAIT
  INDEX ON &SORTFIELD TAG &WORKFILE OF (gcWorkDir+WORKFILE+'.CDX')
  SET ORDER TO TAG &WORKFILE
ENDIF

SET RELATION TO VENDOR INTO &lcVenFile

*E301272,5 Create new temp. file used in calling gfAmntDisp Function [Begin]
lnExRate = 1
IF llMultCurr 
  lcCurrFile = gfTempName()
  CREATE CURSOR (lcCurrFile) (cCurrCode C(3))
  SELECT (lcCurrFile)
  ZAP
  APPEND BLANK
ENDIF
*E301272,5 Create new temp. file used in calling gfAmntDisp Function [End  ]

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

IF llMultCurr 
  USE IN (lcCurrFile)
ENDIF
*-- end of code.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

*E301272,5 Evaluate currency filter postion. [Begin]
IF llMultCurr
  lnCurrPos  = lfItmPos('POSHDR.CPRICECUR')
ELSE
  lcRpCurr = "O"
ENDIF  
*E301272,5 Evaluate currency filter postion. [End  ]
= lfvRepForm()
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfChckAP
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
laOGObjCnt[lnPrnNotePo] = lcRPFormat = 'N'
IF lcRPFormat = 'C'
  lcRPNote = 'N'
ENDIF  
= lfOGShowGet('lcRPNote')

lnLngShrPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPLngShr'),1)
laOGObjCnt[lnLngShrPo] = lcRPFormat = 'N'
IF lcRPFormat = 'C'
  lcRPLngShr = 'L'
ENDIF  
= lfOGShowGet('lcRPLngShr')

*!*************************************************************
*! Name      : lfvVendor
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*PO#     VENDOR NAME...................    SHIP-TO NAME....................   DIVISION............   ENTERED     ±±±±±±±±±   ORDER
*STATUS  VENDOR ADDRESS................    INSURANCE.......................   TERMS...............   COMPLETE    ±±±±±±±±± RECEIVE
*        ..............................    F.O.B...........................   SHIP VIA............   AVAILABLE   ±±±±±±±±±  DAMAGE
*        ..............................    QUOTA/CATEGORY..................   CONTACT.............               ±±±±±±±±±  CANCEL
*        ..............................    L/C #................EXPIRES....   PHONE  .............               ±±±±±±±±±    OPEN
*        CONTACT                           CURRENCY.............RATE.......   ORIGIN..............                 POTOTAL
*                                          XXXXXXXXXXXXXXXXXXXX XXXXXXXXXXX

**                   MERCH    FREIGHT        TAX      QUOTA     PO TOTAL        ORDER   RECEIVE    DAMAGE    CANCEL      OPEN
* *SUB TOTALS*   999999.99  999999.99  999999.99  999999.99   9999999.99      9999999   9999999 999999999 999999999 999999999

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
	**               ±±±±±±±±±  ±±±±±±±±±  ±±±±±±±±±  ±±±±±±±±±  ±±±±±±±±±    PO TOTAL    ORDER  RECEIVE     DAMAGE     CANCEL       OPEN
* *SUB TOTALS*   999999.99  999999.99  999999.99  999999.99  999999.99  9999999.99  9999999  9999999  999999999  999999999  999999999

*112 999999.99

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
*B802104,1 BWA 04/01/99 [FIX THE WIDTH IF THE REPORT IS LONG OR SHORT]
R_WIDTH = IIF(XLONG = 'Y' , 'W' , 'N' )
*R_WIDTH = 'W'
*B802104,1 BWA 04/01/99 [END]

SELECT &WORKFILE

IF lcForm = 'C'
  DO lpCostForm
  RETURN
ENDIF

*-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
IF XLONG = 'Y'
  DIMENSION laVenAdr[5] , laCodeDesc[3,3]
  STORE '' TO laVenAdr , laCodeDesc
  laCodeDesc[1,2] = 'CTERMCODE'
  laCodeDesc[2,2] = 'SHIPVIA'
  laCodeDesc[3,2] = 'CDIVISION'
ENDIF  
*-- MAB 07/01/1999 Fast speed by calling one time. [End  ]

*-- REPORT LOOP.
DO WHILE INKEY() <>32

  *E301272,5 Detect coming Po
  IF EMPTY(PO)
    WAIT CLEAR
  ELSE
    WAIT 'Printing PO#' + PO + ' - <SPACE BAR> to abort' WINDOW NOWAIT
  ENDIF  

  IF ROW >=53
    PAGENO = PAGENO+1
    IF BREAK <> ' '
      XREPORT='POSTYHRE'+'  '+'TOTAL :'+TOTALD
    ENDIF

    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH

    IF XLONG = 'Y'
      *B603348,1 ABD Remark The next Line & Move Them To The Right to Indicate The Layout. [Begin]
      *@ 06,000 SAY 'PO#     VENDOR NAME...................    SHIP-TO NAME....................   DIVISION............   ENTERED     '+PADL(lcISLbl1,9)+'   ORDER'
      *@ 07,000 SAY 'STATUS  VENDOR ADDRESS................    INSURANCE.......................   TERMS...............   COMPLETE    '+PADL(lcISLbl2,9)+' RECEIVE'
      *@ 08,000 SAY '        ..............................    F.O.B...........................   SHIP VIA............   AVAILABLE   '+PADL(lcISLbl3,9)+'  DAMAGE'
      *@ 09,000 SAY '        ..............................    QUOTA/CATEGORY..................   CONTACT.............               '+PADL(lcISLbl4,9)+'  CANCEL'
      *@ 10,000 SAY '        ..............................    L/C #................EXPIRES....   PHONE  .............               '+PADL(lcISLbl5,9)+'    OPEN'
      
      *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
      *B603713,10 HBG 11/22/2000 moving the position of the header [Begin]      
      *@ 06,000 SAY 'PO#     VENDOR NAME...................    SHIP-TO NAME....................   DIVISION............   ENTERED      '+PADL(lcISLbl1,9)+'   ORDER'
      *@ 07,000 SAY 'STATUS  VENDOR ADDRESS................    INSURANCE.......................   TERMS...............   COMPLETE     '+PADL(lcISLbl2,9)+' RECEIVE'
      *@ 08,000 SAY '        ..............................    F.O.B...........................   SHIP VIA............   AVAILABLE    '+PADL(lcISLbl3,9)+'  DAMAGE'
      *@ 06,000 SAY 'PO#     VENDOR NAME...................    SHIP-TO NAME....................   DIVISION............ ENTERED        '+PADL(lcISLbl1,9)+'    ORDER'
      *@ 07,000 SAY 'STATUS  VENDOR ADDRESS................    INSURANCE.......................   TERMS............... COMPLETE       '+PADL(lcISLbl2,9)+'  RECEIVE'
      *@ 08,000 SAY '        ..............................    F.O.B...........................   SHIP VIA............ AVAILABLE      '+PADL(lcISLbl3,9)+'   DAMAGE'
      *@ 09,000 SAY '        ..............................    QUOTA/CATEGORY..................   CONTACT.............                '+PADL(lcISLbl4,9)+'  CANCEL'
      *@ 10,000 SAY '        ..............................    L/C #................EXPIRES....   PHONE  .............                '+PADL(lcISLbl5,9)+'    OPEN'
      *@ 09,000 SAY '        ..............................    QUOTA/CATEGORY..................   CONTACT.............                '+PADL(lcISLbl4,9)+'   CANCEL'
      *@ 10,000 SAY '        ..............................    L/C #................EXPIRES....   PHONE  .............                '+PADL(lcISLbl5,9)+'     OPEN'
      *B603713,10 [End]
      *B603348,1 ABD [End]
      *E301272,5 Say totals if currency is not forign currency

      *B124365,1 NNA 10/24/2004 (Begin) The report does not print the last character/digit in the UK A4
      *@ 06,000 SAY 'PO#     VENDOR NAME...................    SHIP-TO NAME....................   DIVISION............ ENTERED        '+PADL(lcISLbl1,9)+'   ORDER'
      *@ 07,000 SAY 'STATUS  VENDOR ADDRESS................    INSURANCE.......................   TERMS............... COMPLETE       '+PADL(lcISLbl2,9)+' RECEIVE'
      *@ 08,000 SAY '        ..............................    F.O.B...........................   SHIP VIA............ AVAILABLE      '+PADL(lcISLbl3,9)+'  DAMAGE'
      *@ 09,000 SAY '        ..............................    QUOTA/CATEGORY..................   CONTACT.............                '+PADL(lcISLbl4,9)+'  CANCEL'
      *@ 10,000 SAY '        ..............................    L/C #................EXPIRES....   PHONE  .............                '+PADL(lcISLbl5,9)+'    OPEN'
      @ 06,000 SAY 'PO#     VENDOR NAME...................  SHIP-TO NAME....................   DIVISION............ ENTERED        '+PADL(lcISLbl1,9)+'   ORDER'
      @ 07,000 SAY 'STATUS  VENDOR ADDRESS................  INSURANCE.......................   TERMS............... COMPLETE       '+PADL(lcISLbl2,9)+' RECEIVE'
      @ 08,000 SAY '        ..............................  F.O.B...........................   SHIP VIA............ AVAILABLE      '+PADL(lcISLbl3,9)+'  DAMAGE'
      @ 09,000 SAY '        ..............................  QUOTA/CATEGORY..................   CONTACT.............                '+PADL(lcISLbl4,9)+'  CANCEL'
      @ 10,000 SAY '        ..............................  L/C #................EXPIRES....   PHONE  .............                '+PADL(lcISLbl5,9)+'    OPEN'
      *B124365,1 NNA (End)

      *B119469,1 ALB The report does not print the last 2 char. [END]
      
      *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
      *@ 11,000 SAY '                                          CURRENCY.............RATE.......   ORIGIN..............                 '+;
                   IIF(llMultCurr AND lcRpCurr="F",SPACE(7),'POTOTAL')
      @ 11,000 SAY '                                        CURRENCY.............RATE.......   ORIGIN..............                 '+;
                   IIF(llMultCurr AND lcRpCurr="F",SPACE(7),'POTOTAL')
      *B124365,1 NNA (End)

      ROW = 12
    ELSE
      *B802104,1 BWA [SHIFT BACK 4 SPACES AFTER DV]
      *@ 06,00 SAY ' PO#    VENDOR   S DI     ENTERED  COMPLETE ORDERED RECEIVED  DAMAGE  CANCLD.   OPEN'
      @ 06,00 SAY ' PO#    VENDOR   S DV ENTERED  COMPLETE ORDERED RECEIVED  DAMAGE  CANCLD.   OPEN'
      *B802104,1 BWA [END]
      ROW = 7
    ENDIF
  ENDIF

  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('=',IIF(XLONG='Y',132,80) )
    ROW = ROW+1

    @ ROW,000 SAY HBREAK
    IF XLONG = 'Y'
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*             ±±±±±±±±±   ±±±±±±±±±   ±±±±±±±±±   ±±±±±±±±±   ±±±±±±±±±     PO TOTAL     ORDER   RECEIVE    DAMAGE    CANCEL      OPEN
* SUB TOTAL  9999999.99  9999999.99  9999999.99  9999999.99  9999999.99  99999999.99  99999999  99999999  99999999  99999999  99999999
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
      *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
      *E301272,5 Say totals if currency is not forign currency
      *@ ROW,012 SAY PADL(lcISLbl1,9)+SPACE(3)+PADL(lcISLbl2,9)+SPACE(3)+PADL(lcISLbl3,9)+SPACE(3)+PADL(lcISLbl4,9)+SPACE(3)+PADL(lcISLbl5,9)+SPACE(5)+;
                    IIF(llMultCurr AND lcRpCurr="F",SPACE(8),"PO TOTAL")+"     ORDER   RECEIVE    DAMAGE    CANCEL      OPEN"

      *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
      *@ ROW,012 SAY PADL(lcISLbl1,9)+SPACE(3)+PADL(lcISLbl2,9)+SPACE(3)+PADL(lcISLbl3,9)+SPACE(3)+PADL(lcISLbl4,9)+SPACE(3)+PADL(lcISLbl5,9)+SPACE(5)+;
                    IIF(llMultCurr AND lcRpCurr="F",SPACE(8),"PO TOTAL")+"     ORDER   RECEIVE    DAMAGE    CANCEL    OPEN"
      @ ROW,012 SAY PADL(lcISLbl1,9)+SPACE(3)+PADL(lcISLbl2,9)+SPACE(3)+PADL(lcISLbl3,9)+SPACE(3)+PADL(lcISLbl4,9)+SPACE(3)+PADL(lcISLbl5,9)+SPACE(5)+;
                    IIF(llMultCurr AND lcRpCurr="F",SPACE(8),"PO TOTAL")+"   ORDER   RECEIVE    DAMAGE    CANCEL    OPEN"
      *B124365,1 NNA (End)

      *B119469,1 ALB The report does not print the last 2 char. [END]
      ROW = ROW+1
      @ ROW,000 SAY 'SUB TOTAL'

      @ ROW,011 SAY XTOTAL(1,01) PICTURE '9999999.99'
      @ ROW,023 SAY XTOTAL(1,02) PICTURE '9999999.99'
      @ ROW,035 SAY XTOTAL(1,03) PICTURE '9999999.99'

      *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
      *@ ROW,047 SAY XTOTAL(1,04) PICTURE '9999999.99'
      *@ ROW,059 SAY XTOTAL(1,11) PICTURE '9999999.99'
      @ ROW,045 SAY XTOTAL(1,04) PICTURE '9999999.99'
      @ ROW,057 SAY XTOTAL(1,11) PICTURE '9999999.99'
      *B124365,1 NNA (End)

      *E301272,5 Say totals if currency is not forign currency
      IF !llMultCurr OR lcRpCurr <> "F"
        *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
        *@ ROW,071 SAY XTOTAL(1,05) PICTURE '99999999.99'
        @ ROW,069 SAY XTOTAL(1,05) PICTURE '99999999.99'
        *B124365,1 NNA (End)
      ENDIF  

      *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
      *@ ROW,084 SAY XTOTAL(1,06) PICTURE '99999999'
      *@ ROW,094 SAY XTOTAL(1,07) PICTURE '99999999'
      *@ ROW,104 SAY XTOTAL(1,08) PICTURE '99999999'
      *@ ROW,114 SAY XTOTAL(1,09) PICTURE '99999999'
      @ ROW,082 SAY XTOTAL(1,06) PICTURE '99999999'
      @ ROW,092 SAY XTOTAL(1,07) PICTURE '99999999'
      @ ROW,102 SAY XTOTAL(1,08) PICTURE '99999999'
      @ ROW,112 SAY XTOTAL(1,09) PICTURE '99999999'
      *B124365,1 NNA (End)

      *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
      *@ ROW,124 SAY XTOTAL(1,10) PICTURE '99999999'

      *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
      *@ ROW,122 SAY XTOTAL(1,10) PICTURE '99999999'
      @ ROW,120 SAY XTOTAL(1,10) PICTURE '99999999'
      *B124365,1 NNA (End)

      *B119469,1 ALB The report does not print the last 2 char. [END]
    ELSE    && IF IT IS A SHORT FORMAT
      @ ROW,38 SAY XTOTAL(1,6)  PICTURE '99999999'
      @ ROW,48 SAY XTOTAL(1,7)  PICTURE '99999999'
      @ ROW,57 SAY XTOTAL(1,8)  PICTURE '9999999'
      @ ROW,65 SAY XTOTAL(1,9)  PICTURE '9999999'
      @ ROW,73 SAY XTOTAL(1,10) PICTURE '99999999'
    ENDIF

    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',IIF(XLONG='Y',132,80) )
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
  *-- LONG FORMAT.
  XPO = PO
  IF XLONG = 'Y'

    XTERMS    = cTermCode
    XSHIPVIA  = SHIPVIA
    XDIVISION = CDIVISION

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *XVADDR1 = gfGetAdr('APVENDOR', '' , '' , '' , 1) 
    *XVADDR2 = gfGetAdr('APVENDOR', '' , '' , '' , 2)
    *XVADDR3 = gfGetAdr('APVENDOR', '' , '' , '' , 3)
    *XVADDR4 = gfGetAdr('APVENDOR', '' , '' , '' , 4)
    *XVADDR5 = gfGetAdr('APVENDOR', '' , '' , '' , 5)
    laVenAdr = ''
    =gfGetAdr('APVENDOR', '' , '' , '' , @laVenAdr) 
    =lfAdrShift('laVenAdr')  && Shift Vendor address if there is empty line.

    *IF EMPTY(XVADDR2)
    *  XVADDR2 = XVADDR3
    *  XVADDR3 = ' '
    *ENDIF

    *PTERMS    = gfCodDes(cTermCode,'CTERMCODE')
    *PSHIPVIA  = gfCodDes(ShipVia,'SHIPVIA')
    *PDIVISION = gfCodDes(cDivision,'CDIVISION')
    laCodeDesc[1,1] = CTERMCODE
    laCodeDesc[2,1] = SHIPVIA
    laCodeDesc[3,1] = CDIVISION
    =gfCodDes(@laCodeDesc)
    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
   

    SELECT &WORKFILE

    *E301272,5 Calculate amounts to be printed. [Begin]
    = lfCalAmnts()
    *E301272,5 Calculate amounts to be printed. [End  ]
    
    @ ROW,000 SAY REPLICATE('-',IIF(XLONG='Y',132,80))
    ROW = ROW+1

    @ ROW,000 SAY PO
    @ ROW,008 SAY &lcVenFile->&lcVenField

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,042 SAY SHPNAME
    @ ROW,040 SAY SHPNAME
    *B124365,1 NNA (End)

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *@ ROW,077 SAY SUBSTR(CDIVISION + PDIVISION,1,20)

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,077 SAY PADR(laCodeDesc[3,3],20)
    @ ROW,075 SAY PADR(laCodeDesc[3,3],20)
    *B124365,1 NNA (End)

    *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]

    *B603713,10 HBG 11/22/2000 Moving the position ofthe title [Begin]
    *@ ROW,100 SAY ENTERED

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,098 SAY ENTERED
    @ ROW,096 SAY ENTERED
    *B124365,1 NNA (End)

    *B603713,10 [End]

    *E301272,5 Print Calculated amount1. [Begin]
    *@ ROW,111 SAY nICost1 PICTURE '9999999.99'
    *B603348,1 ABD - Remark the next line & move them to the left to indicate the layout
    *B603348,1 ABD - & change the picture upon the width of the filed. [Begin]
    *@ ROW,111 SAY laNormCost[1] PICTURE '9999999.99'
    *B603713,10 HBG 11/22/2000 Increase The Picture of The cost item [Begin]
    *@ ROW,109 SAY laNormCost[1] PICTURE '9999999999.99'
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,107 SAY laNormCost[1] PICTURE '9999999999999.999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,103 SAY laNormCost[1] PICTURE '999999999999.999'
    @ ROW,101 SAY laNormCost[1] PICTURE '999999999999.999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    *B603713,10 [End]
    *B603348,1 ABD [End]
    *E301272,5 Print Calculated amount1. [End  ]
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,122 SAY nStyORDER PICTURE '9999999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,123 SAY nStyORDER PICTURE '9999999'
    @ ROW,121 SAY nStyORDER PICTURE '9999999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    ROW = ROW+1

    DO CASE
      CASE STATUS = 'O'
        @ ROW,00 SAY 'OPEN'
      CASE STATUS = 'C'
        @ ROW,00 SAY 'CMPLTE'
      
      *B605115,1 AMH add Actualized status [Start]
      CASE STATUS = 'A'
        @ ROW,00 SAY 'ACTUAL'
      *B605115,1 AMH [End]
      
      CASE STATUS = 'X'
        @ ROW,00 SAY 'CANCEL'
      CASE STATUS = 'S'
        @ ROW,00 SAY 'CLOSED'
      CASE STATUS = 'H'
        @ ROW,00 SAY 'HOLD'
     
      *B120159,1 MMM 10/21/2003 Fix the bug of not displaying the Bid status to the Status option 
      *                          in the option grid. [Start]
      CASE STATUS= 'B'
        @ ROW,00 SAY 'BID'
      *B120159,1 MMM [End]      
     
     ENDCASE

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *@ ROW,008 SAY XVADDR1
    @ ROW,008 SAY laVenAdr[1]
    *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,042 SAY INSURANCE
    @ ROW,040 SAY INSURANCE
    *B124365,1 NNA (End)

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *@ ROW,077 SAY SUBSTR(cTermCode + PTERMS,1,20)

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,077 SAY PADR(laCodeDesc[1,3],20)
    @ ROW,075 SAY PADR(laCodeDesc[1,3],20)
    *B124365,1 NNA (End)

    *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]
    
    *B603713,10 HBG 11/22/2000 Moving the position ofthe title [Begin]
    *@ ROW,100 SAY COMPLETE

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,098 SAY COMPLETE
    @ ROW,096 SAY COMPLETE
    *B124365,1 NNA (End)

    *B603713,10 [End]

    *E301272,5 Print Calculated amount2. [Begin]
    *@ ROW,111 SAY nICost2 PICTURE '9999999.99'  
    *B603348,1 ABD - Remark the next line & move them to the left to indicate the layout
    *B603348,1 ABD - & change the picture upon the width of the filed. [Begin]
    *@ ROW,111 SAY laNormCost[2] PICTURE '9999999.99'
    *B603713,10 HBG 11/22/2000 Increase The Picture of The cost item [Begin]
    *@ ROW,109 SAY laNormCost[2] PICTURE '9999999999.99'
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,107 SAY laNormCost[2] PICTURE '9999999999999.999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,103 SAY laNormCost[2] PICTURE '999999999999.999'
    @ ROW,101 SAY laNormCost[2] PICTURE '999999999999.999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    *B603713,10 [End]
    *B603348,1 ABD [End]
    *E301272,5 Print Calculated amount2. [End  ]
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,122 SAY RECEIVE PICTURE '9999999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,123 SAY RECEIVE PICTURE '9999999'
    @ ROW,121 SAY RECEIVE PICTURE '9999999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    ROW = ROW+1

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *@ ROW,008 SAY XVADDR2
    @ ROW,008 SAY laVenAdr[2]
    *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,042 SAY cFOB
    @ ROW,040 SAY cFOB
    *B124365,1 NNA (End)

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *@ ROW,077 SAY SUBSTR(SHIPVIA + PSHIPVIA,1,20)

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,077 SAY PADR(laCodeDesc[2,3],20)
    @ ROW,075 SAY PADR(laCodeDesc[2,3],20)    
    *B124365,1 NNA (End)
    
    *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]

    *B603713,10 HBG 11/22/2000 Moving the position ofthe title [Begin]
    *@ ROW,100 SAY AVAILABLE

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,098 SAY AVAILABLE
    @ ROW,096 SAY AVAILABLE
    *B124365,1 NNA (End)

    *B603713,10 [End]
    
    *E301272,5 Print Calculated amount3. [Begin]
    *@ ROW,111 SAY nICost3 PICTURE '9999999.99'  
    *B603348,1 ABD - Remark the next line & move them to the left to indicate the layout
    *B603348,1 ABD - & change the picture upon the width of the filed. [Begin]
    *@ ROW,111 SAY laNormCost[3] PICTURE '9999999.99'
    *B603713,10 HBG 11/22/2000 Increase The Picture of The cost item [Begin]
    *@ ROW,109 SAY laNormCost[3] PICTURE '9999999999.99'
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,107 SAY laNormCost[3] PICTURE '9999999999999.999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,103 SAY laNormCost[3] PICTURE '999999999999.999'
    @ ROW,101 SAY laNormCost[3] PICTURE '999999999999.999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    *B603713,10 [End]
    *B603348,1 ABD [End]
    *E301272,5 Print Calculated amount3. [End  ]
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,122 SAY DAMAGE PICTURE '9999999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,123 SAY DAMAGE PICTURE '9999999'
    @ ROW,121 SAY DAMAGE PICTURE '9999999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    ROW = ROW+1

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *@ ROW,008 SAY XVADDR3
    @ ROW,008 SAY laVenAdr[3]
    *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]


    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,042 SAY QUOTACAT
    *@ ROW,077 SAY SUBSTR(CONTACT,1,20)
    @ ROW,040 SAY QUOTACAT
    @ ROW,075 SAY SUBSTR(CONTACT,1,20)
    *B124365,1 NNA (End)


    *E301272,5 Print Calculated amount4. [Begin]
    *@ ROW,111 SAY nICost4  PICTURE '9999999.99' 
    *B603348,1 ABD - Remark the next line & move them to the left to indicate the layout
    *B603348,1 ABD - & change the picture upon the width of the filed. [Begin]
    *@ ROW,111 SAY laNormCost[4]  PICTURE '9999999.99'
    *B603713,10 HBG 11/22/2000 Increase The Picture of The cost item [Begin]
    *@ ROW,109 SAY laNormCost[4]  PICTURE '9999999999.99'
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,107 SAY laNormCost[4]  PICTURE '9999999999999.999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,106 SAY laNormCost[4]  PICTURE '999999999999.999'
    @ ROW,104 SAY laNormCost[4]  PICTURE '999999999999.999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    *B603713,10 [End
    *B603348,1 ABD [End]
    *E301272,5 Print Calculated amount4. [End  ]
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,122 SAY CANCEL PICTURE '9999999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,123 SAY CANCEL PICTURE '9999999'
    @ ROW,121 SAY CANCEL PICTURE '9999999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    ROW = ROW+1

    *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
    *@ ROW,008 SAY XVADDR4
    @ ROW,008 SAY laVenAdr[4]
    *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,042 SAY cLCNo
    *@ ROW,063 SAY LCEXPIRE
    @ ROW,040 SAY cLCNo
    @ ROW,061 SAY LCEXPIRE
    *B124365,1 NNA (End)

    *@ ROW,077 SAY PHONE  PICTURE "@R "+gcPhnFrmt SIZE 1,16
    *lcPhone = gfPhoneTem(PHONE)

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,077 SAY SUBSTR(PHONE,1,20) PICTURE gfPhoneTem()
    @ ROW,075 SAY SUBSTR(PHONE,1,20) PICTURE gfPhoneTem()
    *B124365,1 NNA (End)

    *E301272,5 Print Calculated amount5. [Begin]
    *@ ROW,111 SAY nICost5  PICTURE '9999999.99'
    *B603348,1 ABD - Remark the next line & move them to the left to indicate the layout
    *B603348,1 ABD - & change the picture upon the width of the filed. [Begin]
    *@ ROW,111 SAY laNormCost[5]  PICTURE '9999999.99' 
    *B603713,10 HBG 11/22/2000 Increase The Picture of The cost item [Begin]
    *@ ROW,109 SAY laNormCost[5]  PICTURE '9999999999.99'
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,107 SAY laNormCost[5]  PICTURE '9999999999999.999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,106 SAY laNormCost[5]  PICTURE '999999999999.999'
    @ ROW,104 SAY laNormCost[5]  PICTURE '999999999999.999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    *B603713,10 [Ed]
    *B603348,1 ABD [End]
    *E301272,5 Print Calculated amount5. [End  ]
    *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
    *@ ROW,122 SAY OPEN PICTURE '9999999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,123 SAY OPEN PICTURE '9999999'
    @ ROW,121 SAY OPEN PICTURE '9999999'
    *B124365,1 NNA (End)

    *B119469,1 ALB The report does not print the last 2 char. [END]
    ROW = ROW+1

    *E301272,5 Say Currency + Description if multi currency format.
    *@ ROW,042 SAY cPriceCur
    *@ ROW,063 SAY RATE PICTURE '999999.9999'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,042 SAY IIF(llMultCurr,lfCurrDesc(),cPriceCur)
    *@ ROW,063 SAY lnExRate PICTURE '999999.9999'
    *@ ROW,077 SAY SUBSTR(ORIGIN,1,20)
    @ ROW,040 SAY IIF(llMultCurr,lfCurrDesc(),cPriceCur)
    @ ROW,061 SAY lnExRate PICTURE '999999.9999'
    @ ROW,075 SAY SUBSTR(ORIGIN,1,20)
    *B124365,1 NNA (End)

    *E301272,5 Say totals if currency is not forign currency
    IF !llMultCurr OR lcRpCurr <> "F"
      *E301272,5 Print Calculated amounts. [Begin]
      *@ ROW,110 SAY POTOTAL PICTURE '99999999.99'  
      *B603348,1 ABD - Remark the next line & move them to the left to indicate the layout
      *B603348,1 ABD - & change the picture upon the width of the filed. [Begin]
      *@ ROW,110 SAY laNormCost[6] PICTURE '99999999.99'
      *B603713,10 HBG 11/22/2000 Increase The Picture of The cost item [Begin]
      *@ ROW,109 SAY laNormCost[6] PICTURE '9999999999.99'
      *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
      *@ ROW,107 SAY laNormCost[6] PICTURE '9999999999999.999'

     *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
      *@ ROW,103 SAY laNormCost[6] PICTURE '999999999999.999'
      @ ROW,107 SAY laNormCost[6] PICTURE '999999999.999'
      *B124365,1 NNA (End)

      *B119469,1 ALB The report does not print the last 2 char. [END]
      *B603713,10 [End]
      *B603348,1 ABD [End]
      *E301272,5 Print Calculated amounts. [End  ]
    ENDIF  

  ELSE            *** IF SHORT FORMAT

* ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* PO#    VENDOR   S DI ENTERED  COMPLETE ORDERED RECEIVED  DAMAGE  CANCLD.    OPEN
* 123456 12345678 X 12 99/99/99 99/99/99 1234567  1234567 1234567 1234567  1234567
* * GRAND TOTAL *                       12345678 12345678 1234567 1234567 12345678
*ahmed
*@ 06,00 SAY ' PO#    VENDOR   S DIV.   ENTERED  COMPLETE ORDERED RECEIVED  DAMAGE  CANCLD.   OPEN'
    @ ROW,01 SAY PO
    @ ROW,08 SAY VENDOR
    @ ROW,17 SAY STATUS
    
    *B802104,1 BWA 04/01/99 [MAKE THE DIVISION 2 CHR. ONLY]
    *@ ROW,19 SAY cDIVISION
    @ ROW,19 SAY SUBSTR(cDIVISION,1,2)
    *B802104,1 BWA 04/01/99 [END]
    
    *B802104,1 BWA 04/01/99 [SHIFT THIS FIELDS 4 CHR. TO ALIGN.MENT THE WIDTH]
    *@ ROW,26 SAY ENTERED
    *@ ROW,35 SAY COMPLETE
    *@ ROW,44 SAY nStyORDER PICTURE '9999999'
    *@ ROW,52 SAY RECEIVE   PICTURE '9999999'
    *@ ROW,61 SAY DAMAGE    PICTURE '9999999'
    *@ ROW,69 SAY CANCEL    PICTURE '9999999'
    *@ ROW,77 SAY OPEN      PICTURE '9999999'
    @ ROW,22 SAY ENTERED
    @ ROW,31 SAY COMPLETE
    @ ROW,40 SAY nStyORDER PICTURE '9999999'
    @ ROW,48 SAY RECEIVE   PICTURE '9999999'
    @ ROW,57 SAY DAMAGE    PICTURE '9999999'
    @ ROW,65 SAY CANCEL    PICTURE '9999999'
    @ ROW,73 SAY OPEN      PICTURE '9999999'
    *B802104,1 BWA 04/01/99 [END]
  ENDIF

  *E301272,5 Evaluate totals according to normal array values [Begin]
  *XTOTAL(1,01) = XTOTAL(1,01) + nICost1
  *XTOTAL(1,02) = XTOTAL(1,02) + nICost2
  *XTOTAL(1,03) = XTOTAL(1,03) + nICost3
  *XTOTAL(1,04) = XTOTAL(1,04) + nICost4
  *XTOTAL(1,11) = XTOTAL(1,11) + nICost5
  *XTOTAL(1,05) = XTOTAL(1,05) + POTOTAL

  XTOTAL(1,01) = XTOTAL(1,01) + laNormCost[1]
  XTOTAL(1,02) = XTOTAL(1,02) + laNormCost[2]
  XTOTAL(1,03) = XTOTAL(1,03) + laNormCost[3]
  XTOTAL(1,04) = XTOTAL(1,04) + laNormCost[4]
  XTOTAL(1,11) = XTOTAL(1,11) + laNormCost[5]
  XTOTAL(1,05) = XTOTAL(1,05) + laNormCost[6]
  *E301272,5 Evaluate totals according to normal array values [End  ]

  XTOTAL(1,06) = XTOTAL(1,06) + nStyORDER
  XTOTAL(1,07) = XTOTAL(1,07) + RECEIVE
  XTOTAL(1,08) = XTOTAL(1,08) + DAMAGE
  XTOTAL(1,09) = XTOTAL(1,09) + CANCEL
  XTOTAL(1,10) = XTOTAL(1,10) + OPEN

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
@ ROW,00 SAY REPLICATE('*',IIF(XLONG='Y',132,80))
ROW = ROW+1

IF XLONG = 'Y'
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*             ±±±±±±±±±   ±±±±±±±±±   ±±±±±±±±±   ±±±±±±±±±   ±±±±±±±±±     PO TOTAL     ORDER   RECEIVE    DAMAGE    CANCEL      OPEN
* GRAND TOT 99999999.99 99999999.99 99999999.99 99999999.99 99999999.99 999999999.99 999999999 999999999 999999999 999999999 999999999
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
  *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
  *E301272,5 Print grand total if print grand flag only. [Begin]
  *@ ROW,012 SAY IIF(llPrintTot,;
                PADL(lcISLbl1,9)+SPACE(3)+;
                PADL(lcISLbl2,9)+SPACE(3)+;
                PADL(lcISLbl3,9)+SPACE(3)+;
                PADL(lcISLbl4,9)+SPACE(3)+;
                PADL(lcISLbl5,9)+SPACE(5)+;
                "PO TOTAL",SPACE(70))+;
                "     ORDER   RECEIVE    DAMAGE    CANCEL      OPEN"

  *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
  *@ ROW,012 SAY IIF(llPrintTot,;
                PADL(lcISLbl1,9)+SPACE(3)+;
                PADL(lcISLbl2,9)+SPACE(3)+;
                PADL(lcISLbl3,9)+SPACE(3)+;
                PADL(lcISLbl4,9)+SPACE(3)+;
                PADL(lcISLbl5,9)+SPACE(5)+;
                "PO TOTAL",SPACE(70))+;
                "   ORDER   RECEIVE    DAMAGE    CANCEL      OPEN"
  @ ROW,012 SAY IIF(llPrintTot,;
                PADL(lcISLbl1,9)+SPACE(3)+;
                PADL(lcISLbl2,9)+SPACE(3)+;
                PADL(lcISLbl3,9)+SPACE(3)+;
                PADL(lcISLbl4,9)+SPACE(3)+;
                PADL(lcISLbl5,9)+SPACE(3)+;
                "PO TOTAL",SPACE(68))+;
                "    ORDER   RECEIVE    DAMAGE    CANCEL     OPEN"
  *B124365,1 NNA (End)

  *B119469,1 ALB The report does not print the last 2 char. [END]
  ROW = ROW+1
  @ ROW,000 SAY 'GRAND TOT'
  
  *E301272,5 Print grand total if print grand flag only. [Begin]
  IF llPrintTot
    @ ROW,010 SAY XTOTAL(X,01) PICTURE '99999999.99'
    @ ROW,022 SAY XTOTAL(X,02) PICTURE '99999999.99'
    @ ROW,034 SAY XTOTAL(X,03) PICTURE '99999999.99'

    *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
    *@ ROW,046 SAY XTOTAL(X,04) PICTURE '99999999.99'
    *@ ROW,058 SAY XTOTAL(X,11) PICTURE '99999999.99'
    @ ROW,044 SAY XTOTAL(X,04) PICTURE '99999999.99'
    @ ROW,056 SAY XTOTAL(X,11) PICTURE '99999999.99'
    *B124365,1 NNA (End)

    *E301272,5 Say totals if currency is not forign currency
    IF !llMultCurr OR lcRpCurr <> "F"
      *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
      *@ ROW,070 SAY XTOTAL(X,05) PICTURE '999999999.99'
      @ ROW,068 SAY XTOTAL(X,05) PICTURE '999999999.99'
      *B124365,1 NNA (End)
    ENDIF  

  ENDIF  
  *E301272,5 Print grand total if print grand flag only. [End  ]
  *B119469,1 ALB The report does not print the last 2 char. [BEGIN]
  *@ ROW,083 SAY XTOTAL(X,06) PICTURE '999999999'
  *@ ROW,093 SAY XTOTAL(X,07) PICTURE '999999999'
  *@ ROW,103 SAY XTOTAL(X,08) PICTURE '999999999'
  *@ ROW,113 SAY XTOTAL(X,09) PICTURE '999999999'
  *@ ROW,123 SAY XTOTAL(X,10) PICTURE '999999999'

  *B124365,1 NNA 10/24/2004 (Begin) Move The report 2 Chr. to the Left
  *@ ROW,081 SAY XTOTAL(X,06) PICTURE '999999999'
  *@ ROW,091 SAY XTOTAL(X,07) PICTURE '999999999'
  *@ ROW,101 SAY XTOTAL(X,08) PICTURE '999999999'
  *@ ROW,111 SAY XTOTAL(X,09) PICTURE '999999999'
  *@ ROW,121 SAY XTOTAL(X,10) PICTURE '999999999'
  @ ROW,079 SAY XTOTAL(X,06) PICTURE '999999999'
  @ ROW,089 SAY XTOTAL(X,07) PICTURE '999999999'
  @ ROW,098 SAY XTOTAL(X,08) PICTURE '999999999'
  @ ROW,109 SAY XTOTAL(X,09) PICTURE '999999999'
  @ ROW,119 SAY XTOTAL(X,10) PICTURE '999999999'
  *B124365,1 NNA (End)

  *B119469,1 ALB The report does not print the last 2 char. [END]
ELSE
  @ ROW,00 SAY '* GRAND TOTAL *'
  
  *B802104,1 BWA 04/01/99 [SHIFT THE VALUE FROM COL38 TO COL39]
  *@ ROW,38 SAY XTOTAL(X,6)  PICTURE '99999999'
  @ ROW,39 SAY XTOTAL(X,6)  PICTURE '99999999'
  *B802104,1 BWA 04/01/99 [END]
  
  @ ROW,48 SAY XTOTAL(X,7)  PICTURE '99999999'
  @ ROW,57 SAY XTOTAL(X,8)  PICTURE '9999999'
  @ ROW,65 SAY XTOTAL(X,9)  PICTURE '9999999'
  @ ROW,72 SAY XTOTAL(X,10) PICTURE '99999999'
ENDIF

ROW = ROW+1
@ ROW,00 SAY REPLICATE('*',IIF(XLONG='Y',132,80))
RETURN

***END STY6A0.PRG
*!*************************************************************
*! Name : lpCostForm.
*! Auth : Tarek Mohamed Ismael (TMI).
*! Date : 03/19/95.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : New form for this report if the user wants to print
*!            the report with costing details.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from :
*!         Program : STY6A0.PRG
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Modifications :
*!*************************************************************

PROCEDURE lpCostForm
*E301272,5 Add new arrays for landed and actual amounts [Begin]
DIMENSION laLandAmnt[6] , laActuAmnt[6]
STORE 0 TO laLandAmnt , laActuAmnt
*E301272,5 Add new arrays for landed and actual amounts [End  ]

STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
ROW = 99
XPO = PO

DO WHILE INKEY() <>32

  *E301272,5 Detect coming PO
  IF EMPTY(PO)
    WAIT CLEAR
  ELSE
    WAIT 'Printing PO#' + PO + ' - <SPACE BAR> to abort' WINDOW NOWAIT
  ENDIF  

  IF ROW >=53
    PAGENO = PAGENO+1
    XREPORT='POSTYHRE'
    IF BREAK <> ' '
      XREPORT='POSTYHRE'
    ENDIF

    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    *B802104,1 BWA 04/15/99 [FIX THE WIDTH ]
    *@ 05,00 SAY ' PO#   Status   Complete Available  Vendor    Vendore Name     Division           Work In Process Account      WHouse'
    
    *B803993,1 AMH Make the correct Indentation of header [Start]
    *@ 05,00 SAY ' PO#   Status   Complete Available  Vendor    Vendore Name     Division                        W.I.P Account                  WHouse'
    @ 05,00 SAY 'PO#    Status   Complete Available   Vendor   Vendore Name     Division                        W.I.P Account                  WHouse'
    *B803993,1 AMH [End]
    
    *B802104,1 BWA [END]
    
    @ 06,00 SAY REPLICATE('=',132)
    ROW = 7
  ENDIF

  IF EOF()
    EXIT
  ENDIF
  *B802104,1 BWA 04/15/99 [THIS COMMANDS DUPLICAET]
  IF ROW > 53
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    row = 05
    @ ROW,00 SAY ' PO#   Status   Complete Available  Vendor    Vendore Name     Division           Work In Process Account      WHouse'
    ROW = ROW +1
    @ ROW,00 SAY REPLICATE('=',132)
    row = 07
  ENDIF
  *B802104,1 BWA [END]
  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF

    ROW=ROW+2
    
    *B803993,1 AMH Make the correct Indentation of header [Start]
    *@ ROW ,11 SAY 'ESTIMATED   LANDED   ACTUAL    ACT-LAND   LAND-EST   ACT-EST    ACT/LAND    LAND/EST    ACT/EST'
    @ ROW ,14 SAY 'ESTIMATED        LANDED        ACTUAL      ACT-LAND      LAND-EST       ACT-EST ACT/LAN LAN/EST ACT/EST'
    *B803993,1 AMH [End]
    
    ROW = ROW+1
    @ ROW ,00 SAY laCost[1,2]

    HBREAK = &BREAK
    EXIT
  ENDDO

  IF ROW >= 53
    ROW = 99
    LOOP
  ENDIF
  
  *B605729,1 KHM 03/21/2002 (Begin) Reinitialize the variable to be
  *B605729,1                calculated for each PO
  STORE 0 TO laLandAmnt , laActuAmnt
  STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
  *B605729,1 KHM 03/21/2002 (End)
  
  SELECT &WORKFILE
  DO CASE
    CASE STATUS = 'H'
      lcStatus = 'HOLD'
    
    *B120159,1 MMM 10/21/2003 Fix the bug of not displaying the Bid status to the Status option 
    *                         in the option grid.[Start]
    CASE STATUS = 'B'                              
      lcStatus = 'BID'
    *B120159,1 MMM [End]
       
    *B605115,1 AMH add Actualized status [Start]
    CASE STATUS = 'A'
      lcStatus = 'ACTUAL'
    *B605115,1 AMH [End]
    
    CASE STATUS = 'X'
      lcStatus = 'CANCELLED'
    CASE STATUS = 'S'
      lcStatus = 'CLOSED'
    CASE STATUS = 'O'
      lcStatus = 'OPEN'
    CASE STATUS = 'C'
      lcStatus = 'COMPLETE'
  ENDCASE

  @ ROW , 00 SAY PO
  @ ROW , 07 SAY lcStatus
  @ ROW , 16 SAY COMPLETE
  @ ROW , 26 SAY AVAILABLE
  @ ROW , 37 SAY VENDOR

  @ ROW , 46 SAY IIF(SEEK(Vendor,'ApVendor'),SUBSTR(ApVendor.CVENCOMP,1,15),SPACE(15))
  
  PDIVISION = gfCodDes(cDivision,'CDIVISION')
  @ ROW , 63 SAY CDIVISION
  *B802104,1 BWA 04/15/99 [SUBSTR THE Division FIELD & SHIFT THE LINK_CODE]
  *@ ROW , 68 SAY gfCodDes(cDivision,'CDIVISION')

  *-- MAB 07/01/1999 Fast speed by calling one time. [Begin]
  *@ ROW , 68 SAY SUBSTR(gfCodDes(cDivision,'CDIVISION'),1,25)
  @ ROW , 68 SAY SUBSTR(PDIVISION,1,25)
  *-- MAB 07/01/1999 Fast speed by calling one time. [End  ]

  SELECT GL_LINK
  *@ ROW , 95 SAY LINK_CODE + '-'
  @ ROW , 95 SAY LINK_CODE + '-'
  *B802104,1 BWA [END]
  
  @ ROW , 87 SAY IIF(SEEK(&WORKFILE..LINK_CODE+'013'),GLACNT,'')
  @ ROW ,111 SAY &WORKFILE..CWARECODE
  SELECT &WORKFILE

  *E301272,5 Calculate amounts to be printed. [Begin]
  = lfCalAmnts()
  *E301272,5 Calculate amounts to be printed. [End  ]
  
  ROW = ROW + 2
  *B603348,1 ABD - Remark the next line & move them to the left to indicate the layout [Begin]
  *@ ROW ,11 SAY 'ESTIMATED     LANDED      ACTUAL    ACT-LAND    LAND-EST     ACT-EST   ACT/LAND   LAND/EST     ACT/EST'  
  
  *B803993,1 AMH Make the correct Indentation of header [Start]
  *IF llRpPrtdec
  *  *B603713,10 HBG 11/22/2000 move the position of the header [Begin]
  *  *@ ROW ,11 SAY 'ESTIMATED      LANDED      ACTUAL    ACT-LAND    LAND-EST     ACT-EST   ACT/LAND   LAND/EST     ACT/EST'  
  *  @ ROW ,11 SAY 'ESTIMATED    LANDED    ACTUAL   ACT-LAND       LAND-EST     ACT-EST    ACT/LAND   LAND/EST     ACT/EST'  
  *  *B603713,10 [nd]
  *ELSE
  *  *B603713,10 HBG 11/22/2000 move the position of the header [Begin]
  *  *@ ROW ,11 SAY 'ESTIMATED     LANDED      ACTUAL     ACT-LAND    LAND-EST     ACT-EST   ACT/LAND   LAND/EST     ACT/EST'
  *  @ ROW ,11 SAY 'ESTIMATED   LANDED    ACTUAL    ACT-LAND       LAND-EST     ACT-EST    ACT/LAND   LAND/EST     ACT/EST'
  *  *B603713,10 [End]
  *ENDIF
  @ ROW ,14 SAY 'ESTIMATED        LANDED        ACTUAL      ACT-LAND      LAND-EST       ACT-EST ACT/LAN LAN/EST ACT/EST'
  *B803993,1 AMH [End]
  
  *B603348,1 ABD [End]
   
  ROW = ROW + 1
  *B603713,10 HBG 11/22/2000 Print only 8 char from the item cost to [Begin]
  *@ ROW , 00 SAY laCost[1,2]
  @ ROW , 00 SAY PADR(laCost[1,2],8)
  *B603713,10 [End]
  *E301272,5 Print Calculated amount1. [Begin]

  *@ ROW , 10 SAY NICOST1                          PICTURE '9999999.999'
  *laEst[1] = laEst[1]+NICOST1
  *@ ROW , 21 SAY NLAN_COST1                       PICTURE '9999999.999'
  *laLAN[1] = laLAN[1]+NLAN_COST1
  *@ ROW , 33 SAY NACT_COST1                       PICTURE '9999999.999'
  *laACT[1] = laACT[1]+NACT_COST1
  *@ ROW , 45 SAY NACT_COST1 -  NLAN_COST1         PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  NLAN_COST1)
  *@ ROW , 57 SAY NLAN_COST1 - NICOST1             PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(NLAN_COST1 - NICOST1)
  *@ ROW , 69 SAY NACT_COST1 - NICOST1             PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST1 - NICOST1)
  *@ ROW , 84 SAY IIF(NLAN_COST1=0,'N/A',(NACT_COST1 / NLAN_COST1)*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(NICOST1=0,'N/A',(NLAN_COST1 / NICOST1)*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(NICOST1=0,'N/A',(NACT_COST1 / NICOST1)*100)   PICTURE '9999.99'

*  STORE 0 TO lnFinEst,lnFinLan,lnFinAct
  =lfSayRow(1)
  *E301272,5 Print Calculated amount1. [End  ]

  *B803993,1 AMH Make the correct Indentation of row [Start]
  *@ ROW , 117 SAY 'ORDER  :'
  *@ ROW , 125 SAY nStyORDER   PICTURE '9999999'
  @ ROW , 118 SAY 'ORDER  :'
  @ ROW , 126 SAY nStyORDER   PICTURE '999999'
  *B803993,1 AMH [End]

  ROW = ROW+1
  *B603713,10 HBG 11/22/2000 Print only 8 char from the item cost to [Begin]
  *@ ROW , 00 SAY laCost[2,2]
  @ ROW , 00 SAY PADR(laCost[2,2],8)
  *B603713,10 [End]

  *E301272,5 Print Calculated amount2. [Begin]

  *@ ROW , 10 SAY NICOST2                          PICTURE '9999999.999'
  *laEst[2] = laEst[2]+NICOST2
  *@ ROW , 21 SAY NLAN_COST2                       PICTURE '9999999.999'
  *laLAN[2] = laLAN[2]+NLAN_COST2
  *@ ROW , 33 SAY NACT_COST2                       PICTURE '9999999.999'
  *laACT[2] = laACT[2]+NACT_COST2
  *@ ROW , 45 SAY NACT_COST2 -  NLAN_COST2         PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  NLAN_COST2)
  *@ ROW , 57 SAY NLAN_COST2 - NICOST2             PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(NLAN_COST2 - NICOST2)
  *@ ROW , 69 SAY NACT_COST2 - NICOST2             PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST2 - NICOST2)
  *@ ROW , 84 SAY IIF(NLAN_COST2=0,'N/A',(NACT_COST2 / NLAN_COST2)*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(NICOST2=0,'N/A',(NLAN_COST2 / NICOST2)*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(NICOST2=0,'N/A',(NACT_COST2 / NICOST2)*100)   PICTURE '9999.99'

  =lfSayRow(2)
  *E301272,5 Print Calculated amount2. [End  ]

  *B803993,1 AMH Make the correct Indentation of row [Start]
  *@ ROW , 117 SAY 'RECEIVE:'
  *@ ROW , 125 SAY RECEIVE     PICTURE '9999999'
  @ ROW , 118 SAY 'RECEIVE:'
  @ ROW , 126 SAY RECEIVE     PICTURE '999999'
  *B803993,1 AMH [End]

  ROW = ROW+1
  *B603713,10 HBG 11/22/2000 Print only 8 char from the item cost to [Begin]
  *@ ROW , 00 SAY laCost[3,2]
  @ ROW , 00 SAY PADR(laCost[3,2],8)
  *B603713,10 [End]
  
  *E301272,5 Print Calculated amount3. [Begin]

  *@ ROW , 10 SAY NICOST3                          PICTURE '9999999.999'
  *laEst[3] = laEst[3]+NICOST3
  *@ ROW , 21 SAY NLAN_COST3                       PICTURE '9999999.999'
  *laLAN[3] = laLAN[3]+NLAN_COST3
  *@ ROW , 33 SAY NACT_COST3                       PICTURE '9999999.999'
  *laACT[3] = laACT[3]+NACT_COST3
  *@ ROW , 45 SAY NACT_COST3 -  NLAN_COST3         PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  NLAN_COST3)
  *@ ROW , 57 SAY NLAN_COST3 - NICOST3             PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(NLAN_COST3 - NICOST3)
  *@ ROW , 69 SAY NACT_COST3 - NICOST3             PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST3 - NICOST3)
  *@ ROW , 84 SAY IIF(NLAN_COST3=0,'N/A',(NACT_COST3 / NLAN_COST3)*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(NICOST3=0,'N/A',(NLAN_COST3 / NICOST3)*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(NICOST3=0,'N/A',(NACT_COST3 / NICOST3)*100)   PICTURE '9999.99'

  =lfSayRow(3)
  *E301272,5 Print Calculated amount3. [End  ]

  *B803993,1 AMH Make the correct Indentation of row [Start]
  *@ ROW , 117 SAY 'DAMAGE :'
  *@ ROW , 125 SAY DAMAGE     PICTURE '9999999'
  @ ROW , 118 SAY 'DAMAGE :'
  @ ROW , 126 SAY DAMAGE     PICTURE '999999'
  *B803993,1 AMH [End]


  ROW = ROW+1
  *B603713,10 HBG 11/22/2000 Print only 8 char from the item cost to [Begin]
  *@ ROW , 00 SAY laCost[4,2]
  @ ROW , 00 SAY PADR(laCost[4,2],8)
  *B603713,10 [End]
  
  *E301272,5 Print Calculated amount4. [Begin]

  *@ ROW , 10 SAY NICOST4                          PICTURE '9999999.999'
  *laEst[4] = laEst[4]+NICOST4
  *@ ROW , 21 SAY NLAN_COST4                       PICTURE '9999999.999'
  *laLAN[4] = laLAN[4]+NLAN_COST4
  *@ ROW , 33 SAY NACT_COST4                       PICTURE '9999999.999'
  *laACT[4] = laACT[4]+NACT_COST4
  *@ ROW , 45 SAY NACT_COST4 -  NLAN_COST4         PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  NLAN_COST4)
  *@ ROW , 57 SAY NLAN_COST4 - NICOST4             PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(NLAN_COST4 - NICOST4)
  *@ ROW , 69 SAY NACT_COST4 - NICOST4             PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST4 - NICOST4)
  *@ ROW , 84 SAY IIF(NLAN_COST4=0,'N/A',(NACT_COST4 / NLAN_COST4)*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(NICOST4=0,'N/A',(NLAN_COST4 / NICOST4)*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(NICOST4=0,'N/A',(NACT_COST4 / NICOST4)*100)   PICTURE '9999.99'

  =lfSayRow(4)
  *E301272,5 Print Calculated amount4. [End  ]

  *B803993,1 AMH Make the correct Indentation of row [Start]
  *@ ROW , 117 SAY 'CANCEL :'
  *@ ROW , 125 SAY CANCEL     PICTURE '9999999'
  @ ROW , 118 SAY 'CANCEL :'
  @ ROW , 126 SAY CANCEL     PICTURE '999999'
  *B803993,1 AMH [End]

  ROW = ROW+1
  *B603713,10 HBG 11/22/2000 Print only 8 char from the item cost to [Begin]
  *@ ROW , 00 SAY laCost[5,2]
  @ ROW , 00 SAY PADR(laCost[5,2],8)
  *B603713,10 [End]

  *E301272,5 Print Calculated amount5. [Begin]
  *@ ROW , 10 SAY NICOST5                          PICTURE '9999999.999'
  *laEst[5] = laEst[5]+NICOST5
  *@ ROW , 21 SAY NLAN_COST5                       PICTURE '9999999.999'
  *laLAN[5] = laLAN[5]+NLAN_COST5
  *@ ROW , 33 SAY NACT_COST5                       PICTURE '9999999.999'
  *laACT[5] = laACT[5]+NACT_COST5
  *@ ROW , 45 SAY NACT_COST5 -  NLAN_COST5         PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST5 -  NLAN_COST5)
  *@ ROW , 57 SAY NLAN_COST5 - NICOST5             PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(NLAN_COST5 - NICOST5)
  *@ ROW , 69 SAY NACT_COST5 - NICOST5             PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST5 - NICOST5)
  *@ ROW , 84 SAY IIF(NLAN_COST5=0,'N/A',(NACT_COST5 / NLAN_COST5)*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(NICOST5=0,'N/A',(NLAN_COST5 / NICOST5)*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(NICOST5=0,'N/A',(NACT_COST5 / NICOST5)*100)   PICTURE '9999.99'

  =lfSayRow(5)
  *E301272,5 Print Calculated amount5. [End  ]

  *B803993,1 AMH Make the correct Indentation of row [Start]
  *@ ROW , 117 SAY 'OPEN   :'
  *@ ROW , 125 SAY OPEN     PICTURE '9999999'
  @ ROW , 118 SAY 'OPEN   :'
  @ ROW , 126 SAY OPEN     PICTURE '999999'
  *B803993,1 AMH [End]

  ROW = ROW + 1
  *E301272,5 if print in any equavielent currency method [Begin]
  IF !llMultCurr OR lcRpCurr <> "F"
    
    *B803993,1 AMH Make the correct Indentation of row [Start]
    *@ ROW ,11 SAY '---------- ----------  ----------  ----------  ---------- -----------  ---------  ---------   ---------'
    lnX = 10
    FOR lnI = 1 TO 9
      lnZ = 13-(6*INT(lnI/7))
      @ ROW , lnX SAY REPLICATE('-',lnZ)
      lnX = lnX + lnZ + 1
    ENDFOR
    *B803993,1 AMH [End]
    
    ROW = ROW + 1
    @ ROW , 00 SAY 'Total Cost'

    *E301272,5 Print Calculated totals. [Begin]

    *@ ROW , 10 SAY NICOST1+NICOST2+NICOST3+NICOST4+NICOST5                  PICTURE '9999999.999'
    *lnEst = NICOST1+NICOST2+NICOST3+NICOST4+NICOST5
    *@ ROW , 21 SAY NLAN_COST1+NLAN_COST2+NLAN_COST3+NLAN_COST4+NLAN_COST5   PICTURE '9999999.999'
    *lnLand = NLAN_COST1+NLAN_COST2+NLAN_COST3+NLAN_COST4+NLAN_COST5
    *@ ROW , 33 SAY NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5   PICTURE '9999999.999'
    *lnAcT  = NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5
    *@ ROW , 45 SAY lnAct  - lnLand                                                PICTURE '9999999.999'
    *@ ROW , 57 SAY lnLand -lnEst                                                PICTURE '9999999.999'
    *@ ROW , 69 SAY lnAct  -lnEst                                                PICTURE '9999999.999'
    *@ ROW , 84 SAY IIF(lnLand=0,'N/A',(lnAcT / lnLand)*100)                 PICTURE '9999.99'
    *@ ROW , 95 SAY IIF(lnEst=0,'N/A',(lnLand / lnEst)*100)                  PICTURE '9999.99'
    *@ ROW , 107 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                  PICTURE '9999.99'

    =lfSayRow(6,.T.)
    *E301272,5 Print Calculated totals. [End  ]


    ROW = ROW + 1
  ENDIF
  *E301272,5 if print in any equavielent currency method [End  ]
    
  *B603713,10 HBG 11/22/2000 Change the positions of the lines [Begin]
  *@ ROW ,11 SAY '========== ==========  ==========  ==========  ========== =========== ==========  =========   ========= '
  
  *B803993,1 AMH Make the correct Indentation of row [Start]
  *@ ROW ,9 SAY '============ ========= =========  ========= ============= ============= ==========  =========   ========= '
  lnX = 10
  FOR lnI = 1 TO 9
    lnZ = 13-(6*INT(lnI/7))
    @ ROW , lnX SAY REPLICATE('=',lnZ)
    lnX = lnX + lnZ + 1
  ENDFOR
  *B803993,1 AMH [End]
  
  *B603713,10 [End]
  
  *E301272,5 Calculate from arrays when print rows [Begin]
  *FOR I = 1 TO 5
    *H= STR(I,1)
    *lnFinEst=lnFinEst+ NICOST&H
    *lnFinLan=lnFinLan+ NLAN_COST&H
    *lnFinAct=lnFinAct+ NACT_COST&H
  *ENDFOR
  lnFinEst=lnFinEst + laNormCost[6] 
  lnFinLan=lnFinLan + laLandAmnt[6]
  lnFinAct=lnFinAct + laActuAmnt[6]
  *E301272,5 Calculate from arrays when print rows [End  ]

  STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
  SKIP
  ROW = ROW+2
ENDDO

*******
ROW=ROW+2
@ ROW,00 SAY REPLICATE('=',132)

*E301272,5 if print in any equavielent currency method [Begin]
IF !llMultCurr OR lcRpCurr <> "F"
  ROW = ROW+1
  @ ROW,000 SAY '* GRAND TOTAL *'
  ROW = ROW+1
  
  *B803993,1 AMH Make the correct Indentation and picture of grand total [Start]
  *@ ROW , 00 SAY laCost[1,2]
  *@ ROW , 10 SAY laEST[1]                         PICTURE '9999999.999'
  *@ ROW , 21 SAY lalan[1]                         PICTURE '9999999.999'
  *@ ROW , 33 SAY laact[1]                         PICTURE '9999999.999'
  *@ ROW , 45 SAY (laAct[1] -  lalan[1])           PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(laAct[1] -  lalan[1])
  *@ ROW , 57 SAY (lalan[1] - laEst[1])            PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(lalan[1] - laEst[1])
  *@ ROW , 69 SAY (laAct[1] - laEst[1])            PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(laAct[1] - laEst[1])
  *@ ROW , 84 SAY IIF(lalan[1]=0,'N/A',(laAct[1] / lalan[1])*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(laest[1]=0,'N/A',(lalan[1] / laest[1])*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(laEst[1]=0,'N/A',(laact[1] / laest[1])*100)   PICTURE '9999.99'

  *ROW = ROW + 1
  *@ ROW , 00 SAY laCost[2,2]
  *@ ROW , 10 SAY laEST[2]                         PICTURE '9999999.999'
  *@ ROW , 21 SAY lalan[2]                         PICTURE '9999999.999'
  *@ ROW , 33 SAY laact[2]                         PICTURE '9999999.999'
  *@ ROW , 45 SAY (laAct[2] -  lalan[2])           PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(laAct[2] -  lalan[2])
  *@ ROW , 57 SAY (lalan[2] - laEst[2])            PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(lalan[2] - laEst[2])
  *@ ROW , 69 SAY (laAct[2] - laEst[2])            PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(laAct[2] - laEst[2])
  *@ ROW , 84 SAY IIF(lalan[2]=0,'N/A',(laAct[2] / lalan[2])*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(laest[2]=0,'N/A',(lalan[2] / laest[2])*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(laEst[2]=0,'N/A',(laact[2] / laest[2])*100)   PICTURE '9999.99'
  *ROW= ROW+1
  *@ ROW , 00 SAY laCost[3,2]
  *@ ROW , 10 SAY laEST[3]                         PICTURE '9999999.999'
  *@ ROW , 21 SAY lalan[3]                         PICTURE '9999999.999'
  *@ ROW , 33 SAY laact[3]                         PICTURE '9999999.999'
  *@ ROW , 45 SAY (laAct[3] -  lalan[3])           PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(laAct[3] -  lalan[3])
  *@ ROW , 57 SAY (lalan[3] - laEst[3])            PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(lalan[3] - laEst[3])
  *@ ROW , 69 SAY (laAct[3] - laEst[3])            PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(laAct[3] - laEst[3])
  *@ ROW , 84 SAY IIF(lalan[3]=0,'N/A',(laAct[3] / lalan[3])*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(laest[3]=0,'N/A',(lalan[3] / laest[3])*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(laEst[3]=0,'N/A',(laact[3] / laest[3])*100)   PICTURE '9999.99'
  *ROW= ROW+1
  *@ ROW , 00 SAY laCost[4,2]
  *@ ROW , 10 SAY laEST[4]                         PICTURE '9999999.999'
  *@ ROW , 21 SAY lalan[4]                         PICTURE '9999999.999'
  *@ ROW , 33 SAY laact[4]                         PICTURE '9999999.999'
  *@ ROW , 45 SAY (laAct[4] -  lalan[4])           PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(laAct[4] -  lalan[4])
  *@ ROW , 57 SAY (lalan[4] - laEst[4])            PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(lalan[4] - laEst[4])
  *@ ROW , 69 SAY (laAct[4] - laEst[4])            PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(laAct[4] - laEst[4])
  *@ ROW , 84 SAY IIF(lalan[4]=0,'N/A',(laAct[4] / lalan[4])*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(laest[4]=0,'N/A',(lalan[4] / laest[4])*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(laEst[4]=0,'N/A',(laact[4] / laest[4])*100)   PICTURE '9999.99'
  *ROW= ROW+1
  *@ ROW , 00 SAY laCost[5,2]
  *@ ROW , 10 SAY laEST[5]                         PICTURE '9999999.999'
  *@ ROW , 21 SAY lalan[5]                         PICTURE '9999999.999'
  *@ ROW , 33 SAY laact[5]                         PICTURE '9999999.999'
  *@ ROW , 45 SAY (laAct[5] -  lalan[5])           PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(laAct[5] -  lalan[5])
  *@ ROW , 57 SAY (lalan[5] - laEst[5])            PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(lalan[5] - laEst[5])
  *@ ROW , 69 SAY (laAct[5] - laEst[5])            PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(laAct[5] - laEst[5])
  *@ ROW , 84 SAY IIF(lalan[5]=0,'N/A',(laAct[5] / lalan[5])*100)  PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(laest[5]=0,'N/A',(lalan[5] / laest[5])*100)     PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(laEst[5]=0,'N/A',(laact[5] / laest[5])*100)   PICTURE '9999.99'
  *ROW= ROW+1

  *@ ROW ,00 SAY '           ---------- ----------   ---------  ----------   ---------  ----------    -------    -------    --------'
  *ROW = ROW + 1

  *@ ROW , 00 SAY 'Total Cost'
  *@ ROW , 10 SAY lnFinEst                     PICTURE '9999999.999'
  
  *MAB HERE I do not know Why calculate this variable.
  *lnEst = (NICOST1+NICOST2+NICOST3+NICOST4+NICOST5)

  *@ ROW , 21 SAY lnFinLan  PICTURE '9999999.999'
  
  *MAB HERE I do not know Why calculate this variable.
  *lnLand = (NLAN_COST1+NLAN_COST2+NLAN_COST3+NLAN_COST4+NLAN_COST5)
  
  *@ ROW , 33 SAY lnFinAct     PICTURE '9999999.999'

  *MAB HERE I do not know Why calculate this variable.
  *lnAcT  = (NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5)
  
  *@ ROW , 45 SAY (lnFinAct-lnFinLan)                                             PICTURE '9999999.999'
  *@ ROW , 57 SAY (lnFinLan-lnFinEst)                                             PICTURE '9999999.999'
  *@ ROW , 69 SAY (lnFinAct-lnFinEst)                                             PICTURE '9999999.999'
  *@ ROW , 84 SAY IIF(lnFinLan=0,'N/A',(lnFinAct / lnFinLan)*100)                 PICTURE '9999.99'
  *@ ROW , 95 SAY IIF(lnFinEst=0,'N/A',(lnFinLan / lnFinEst)*100)                  PICTURE '9999.99'
  *@ ROW , 107 SAY IIF(lnFinEst=0,'N/A',(lnFinAct / lnFinEst)*100)                  PICTURE '9999.99'
  
  *ROW = ROW + 1
  *@ ROW ,00 SAY '           ========== ==========  ==========   =========  ==========   =========    =======   ========    ========'
  FOR lnI = 1 TO 5
    @ ROW , 00 SAY laCost[lnI,2]
    @ ROW , 10 SAY laEST[lnI]                 PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
    @ ROW , 24 SAY lalan[lnI]                 PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
    @ ROW , 38 SAY laact[lnI]                 PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
    @ ROW , 52 SAY (laAct[lnI] -  lalan[lnI]) PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
    lnAct_Lan= lnAct_Lan+(laAct[lnI] -  lalan[lnI])
    @ ROW , 66 SAY (lalan[lnI] - laEst[lnI])  PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
    lnLan_Est=lnLan_Est+(lalan[lnI] - laEst[lnI])
    @ ROW , 80 SAY (laAct[lnI] - laEst[lnI])  PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
    lnAct_Est=lnAct_Est+(laAct[lnI] - laEst[lnI])
    @ ROW , 94 SAY IIF(lalan[lnI]=0,'N/A',(laAct[lnI] / lalan[lnI])*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
    @ ROW ,102 SAY IIF(laest[lnI]=0,'N/A',(lalan[lnI] / laest[lnI])*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
    @ ROW ,110 SAY IIF(laEst[lnI]=0,'N/A',(laact[lnI] / laest[lnI])*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
    ROW = ROW + 1
  ENDFOR
  lnX = 10
  FOR lnI = 1 TO 9
    lnZ = 13-(6*INT(lnI/7))
    @ ROW , lnX SAY REPLICATE('-',lnZ)
    lnX = lnX + lnZ + 1
  ENDFOR
  ROW = ROW + 1

  @ ROW , 00 SAY 'Total Cost'
  @ ROW , 10 SAY lnFinEst            PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
  @ ROW , 24 SAY lnFinLan            PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
  @ ROW , 38 SAY lnFinAct            PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
  @ ROW , 52 SAY (lnFinAct-lnFinLan) PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
  @ ROW , 66 SAY (lnFinLan-lnFinEst) PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
  @ ROW , 80 SAY (lnFinAct-lnFinEst) PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
  @ ROW , 94 SAY IIF(lnFinLan=0,'N/A',(lnFinAct / lnFinLan)*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
  @ ROW ,102 SAY IIF(lnFinEst=0,'N/A',(lnFinLan / lnFinEst)*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
  @ ROW ,110 SAY IIF(lnFinEst=0,'N/A',(lnFinAct / lnFinEst)*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
  ROW = ROW + 1
  lnX = 10
  FOR lnI = 1 TO 9
    lnZ = 13-(6*INT(lnI/7))
    @ ROW , lnX SAY REPLICATE('=',lnZ)
    lnX = lnX + lnZ + 1
  ENDFOR
  *B803993,1 AMH [End]

ENDIF
*E301272,5 if print in any equavielent currency method [End  ]
****
RETURN

*E301272,5 Enhancement Functions Begin.
*E301272,5 *****************************************************

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301272,5
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Example   : =lfCurrDesc()
*!*************************************************************
*!E301272,4
FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = ALLTRIM(cPriceCur)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF  
RETURN PADR(ALLTRIM(lcCurDesc),18)
*-- end of lfCurrDesc.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*E301272,5
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfFillVars
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*E301272,5
FUNCTION lfFillVars
DIMENSION laCost [5,2] , laSetUps[11,2]
laCost = SPACE(9)

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  

laSetUps[1,1]   = 'M_CITYPE1'
laSetUps[2,1]   = 'M_CITYPE2'
laSetUps[3,1]   = 'M_CITYPE3'
laSetUps[4,1]   = 'M_CITYPE4'
laSetUps[5,1]   = 'M_CITYPE5'

laSetUps[6,1]   = 'M_CISLBL1'
laSetUps[7,1]   = 'M_CISLBL2'
laSetUps[8,1]   = 'M_CISLBL3'
laSetUps[9,1]   = 'M_CISLBL4'
laSetUps[10,1]  = 'M_CISLBL5'
laSetUps[11,1]  = 'llMulCurr'

= gfGetMemVar(@laSetups)

laCost[1,1]  = ALLTRIM(laSetUps[1,2])
laCost[2,1]  = ALLTRIM(laSetUps[2,2])
laCost[3,1]  = ALLTRIM(laSetUps[3,2])
laCost[4,1]  = ALLTRIM(laSetUps[4,2])
laCost[5,1]  = ALLTRIM(laSetUps[5,2])

laCost[1,2]  = LEFT(laSetUps[6,2],9)
laCost[2,2]  = LEFT(laSetUps[7,2],9)
laCost[3,2]  = LEFT(laSetUps[8,2],9)
laCost[4,2]  = LEFT(laSetUps[9,2],9)
laCost[5,2]  = LEFT(laSetUps[10,2],9)

llMultCurr = laSetUps[11,2]

IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF
*-- end of lfFillVars.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*E301272,5
FUNCTION lfClearRep
IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT 
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF  
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfCalAmnts
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Calculte amounts in base currency to be printed.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCalAmnts()
*!*************************************************************
*E301272,5
FUNCTION lfCalAmnts

*-- If Print in forign currency.
IF lcRpCurr = "F"
  lnExRate = 1

  *-- if Costing form.
  IF lcForm = 'C'
    =lfFillCost(@laNormCost,"nFCost",.F.,@laLandAmnt,"nFLanCost",@laActuAmnt,"nFActCost")
  ELSE
    =lfFillCost(@laNormCost , "nFCost")
  ENDIF
  
ELSE
  *-- If Print in original base currency.
  IF lcRpCurr = "O"
    lnExRate = nPriceRat
    
    *-- if Costing form.
    IF lcForm = 'C'
      =lfFillCost(@laNormCost , "nICost",.F.,@laLandAmnt,"nLan_Cost",@laActuAmnt,"nAct_Cost")
    ELSE
      =lfFillCost(@laNormCost , "nICost")
    ENDIF
    
  ELSE  && print By Date or calculated now.
    STORE 1 TO lnMyRate,lnExRate

    *-- if Costing form.
    IF lcForm = 'C'
      =lfFillCost(@laNormCost,"nFCost",.T.,@laLandAmnt,"nFLanCost",@laActuAmnt,"nFActCost")
    ELSE
      =lfFillCost(@laNormCost , "nFCost",.T.)
    ENDIF
    
    lnExRate = lnMyRate
  ENDIF
ENDIF
*--end of lfCalAmnts.

*!*************************************************************
*! Name      : lfFillCost
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Fill any cost array.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillCost()
*!*************************************************************
*E301272,5
FUNCTION lfFillCost
PARAMETERS lcArray1,lcField1,llCallCurr,lcArray2,lcField2,lcArray3,lcField3
PRIVATE lnCstItem , lnCurntAls
lnCurntAls = SELECT(0)
STORE 0 TO lcArray1,lnCstItem
FOR lnCstItem = 1 TO 5
  lcCstItem = STR(lnCstItem,1)
  lcArray1[lnCstItem] = EVALUATE(lcField1+lcCstItem)
  
  IF TYPE("lcField2") = "C"
    lcArray2[lnCstItem] = EVALUATE(lcField2+lcCstItem)
  ENDIF
  
  IF TYPE("lcField3") = "C"
    lcArray3[lnCstItem] = EVALUATE(lcField3+lcCstItem)
  ENDIF

  IF llCallCurr AND (lcArray1[lnCstItem] <> 0 OR ;
                     (TYPE("lcField2") = "C" AND lcArray2[lnCstItem] <> 0) OR;
                     (TYPE("lcField3") = "C" AND lcArray3[lnCstItem] <> 0))

    *-- IF P.Price. and currency not equal base currency.
    IF laCost[lnCstItem,1] = "P" AND (cPriceCur <> gcBaseCurr)
      m.cCurrCode = cPriceCur
      =lfAmntDisp()
      lnMyRate = lnExRate
    ENDIF

    *-- IF Duty or Manufacturing (Misc.) Operations and currency not equal base currency.
    IF laCost[lnCstItem,1] $ "MD" AND (cDutyCur <> gcBaseCurr)
      m.cCurrCode = cDutyCur
      =lfAmntDisp()
    ENDIF

  ENDIF
  
  IF !llMultCurr OR lcRpCurr <> "F"
    lcArray1[6] = lcArray1[6] + lcArray1[lnCstItem]

    IF TYPE("lcField2") = "C"
      lcArray2[6] = lcArray2[6] + lcArray2[lnCstItem]
    ENDIF

    IF TYPE("lcField3") = "C"
      lcArray3[6] = lcArray3[6] + lcArray3[lnCstItem]
    ENDIF

  ENDIF
    
ENDFOR
SELECT (lnCurntAls)
*-- end of lfFillCost.

*!*************************************************************
*! Name      : lfAmntDisp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Calculate equavelent amount based on currency of transaction
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAmntDisp()
*!*************************************************************
*E301272,5
FUNCTION lfAmntDisp
SELECT (lcCurrFile)
GATHER MEMVAR
lcArray1[lnCstItem] = gfAmntDisp(lcArray1[lnCstItem],lcRpCurr,ldRpExDate,lcRpTmpNam)      
      
IF TYPE("lcField2") = "C"
  lcArray2[lnCstItem] = gfAmntDisp(lcArray2[lnCstItem],lcRpCurr,ldRpExDate,lcRpTmpNam)      
ENDIF

IF TYPE("lcField3") = "C"
  lcArray3[lnCstItem] = gfAmntDisp(lcArray3[lnCstItem],lcRpCurr,ldRpExDate,lcRpTmpNam)      
ENDIF
SELECT (lnCurntAls)
*-- end of lfAmntDisp.

*!*************************************************************
*! Name      : lfSayRow
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Say transaction Row.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSayRow()
*!*************************************************************
*E301272,5
FUNCTION lfSayRow
PARAMETERS lnAmntNum , llNotAccum
*B603348,1 ABD - Remark the next line & change the picture upon the width of the filed
*B603348,1 ABD - & move them to the left to indicate the layout if print with no desimal. [Begin]
*@ ROW , 10  SAY laNormCost[lnAmntNum]                         PICTURE '9999999.999'
*@ ROW , 21  SAY laLandAmnt[lnAmntNum]                         PICTURE '9999999.999'
*@ ROW , 33  SAY laActuAmnt[lnAmntNum]                         PICTURE '9999999.999'
*@ ROW , 45  SAY laActuAmnt[lnAmntNum] - laLandAmnt[lnAmntNum] PICTURE '9999999.999'
*@ ROW , 57  SAY laLandAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE '9999999.999'
*@ ROW , 69  SAY laActuAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE '9999999.999'
*@ ROW , 84  SAY IIF(laLandAmnt[lnAmntNum]=0,'N/A',(laActuAmnt[lnAmntNum] / laLandAmnt[lnAmntNum])*100) PICTURE '9999.99'
*@ ROW , 95  SAY IIF(laNormCost[lnAmntNum]=0,'N/A',(laLandAmnt[lnAmntNum] / laNormCost[lnAmntNum])*100) PICTURE '9999.99'
*@ ROW , 107 SAY IIF(laNormCost[lnAmntNum]=0,'N/A',(laActuAmnt[lnAmntNum] / laNormCost[lnAmntNum])*100) PICTURE '9999.99'

*B603713,10 HBG 11/22/2000  Increase The Picture Of The cost item [Begin]
*@ ROW , 10  SAY laNormCost[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'9999999.999','9999999999')

*B803993,1 AMH Make the correct Indentation of row [Start]
*@ ROW , 8 SAY laNormCost[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'9999999999.999','9999999999999')
@ ROW , 10 SAY laNormCost[lnAmntNum] PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
*B803993,1 AMH [End]

*@ ROW , 21  SAY laLandAmnt[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'9999999.999','9999999999')
*@ ROW , 33  SAY laActuAmnt[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'9999999.999','9999999999')
*@ ROW , 45  SAY laActuAmnt[lnAmntNum] - laLandAmnt[lnAmntNum] PICTURE IIF(llRpPrtdec,'9999999.999','99999999999')

*B803993,1 AMH Make the correct Indentation of row [Start]
*@ ROW , 17  SAY laLandAmnt[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'9999999.999','9999999999')
*@ ROW , 30  SAY laActuAmnt[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'9999999.999','9999999999')
*@ ROW , 40  SAY laActuAmnt[lnAmntNum] - laLandAmnt[lnAmntNum] PICTURE IIF(llRpPrtdec,'9999999.999','99999999999')
@ ROW , 24  SAY laLandAmnt[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
@ ROW , 38  SAY laActuAmnt[lnAmntNum]                         PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
@ ROW , 52  SAY laActuAmnt[lnAmntNum] - laLandAmnt[lnAmntNum] PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
*B803993,1 AMH [End]

*@ ROW , 57  SAY laLandAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE IIF(llRpPrtdec,'9999999.999','99999999999')
*@ ROW , 69  SAY laActuAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE IIF(llRpPrtdec,'9999999.999','99999999999')

*B803993,1 AMH Make the correct Indentation of row [Start]
*@ ROW , 52  SAY laLandAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE IIF(llRpPrtdec,'99999999999.999','99999999999999')
*@ ROW , 63  SAY laActuAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE IIF(llRpPrtdec,'99999999999.999','99999999999999')
@ ROW , 66  SAY laLandAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
@ ROW , 80  SAY laActuAmnt[lnAmntNum] - laNormCost[lnAmntNum] PICTURE IIF(llRpPrtdec,'999999999.999','9999999999999')
*@ ROW , 84 SAY IIF(laLandAmnt[lnAmntNum]=0,'N/A',(laActuAmnt[lnAmntNum] / laLandAmnt[lnAmntNum])*100) PICTURE '9999.99'
*@ ROW , 88 SAY IIF(laLandAmnt[lnAmntNum]=0,'N/A',(laActuAmnt[lnAmntNum] / laLandAmnt[lnAmntNum])*100) PICTURE '9999.99'
@ ROW , 94 SAY IIF(laLandAmnt[lnAmntNum]=0,'N/A',(laActuAmnt[lnAmntNum] / laLandAmnt[lnAmntNum])*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
*B803993,1 AMH [End]

*B603713,10 [End]

*B803993,1 AMH Make the correct Indentation of row [Start]
*@ ROW , 95  SAY IIF(laNormCost[lnAmntNum]=0,'N/A',(laLandAmnt[lnAmntNum] / laNormCost[lnAmntNum])*100) PICTURE '9999.99'
*@ ROW , 107 SAY IIF(laNormCost[lnAmntNum]=0,'N/A',(laActuAmnt[lnAmntNum] / laNormCost[lnAmntNum])*100) PICTURE '9999.99'
@ ROW , 102 SAY IIF(laNormCost[lnAmntNum]=0,'N/A',(laLandAmnt[lnAmntNum] / laNormCost[lnAmntNum])*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
@ ROW , 110 SAY IIF(laNormCost[lnAmntNum]=0,'N/A',(laActuAmnt[lnAmntNum] / laNormCost[lnAmntNum])*100) PICTURE IIF(llRpPrtdec,'9999.99','9999999')
*B803993,1 AMH [End]

*B603348,1 ABD [End]

IF !llNotAccum
  laEst[lnAmntNum] = laEst[lnAmntNum]+laNormCost[lnAmntNum]
  laLAN[lnAmntNum] = laLAN[lnAmntNum]+laLandAmnt[lnAmntNum]
  laACT[lnAmntNum] = laACT[lnAmntNum]+laActuAmnt[lnAmntNum]
  lnAct_Lan= lnAct_Lan+(laActuAmnt[lnAmntNum] - laLandAmnt[lnAmntNum])
  lnLan_Est=lnLan_Est+(laLandAmnt[lnAmntNum] - laNormCost[lnAmntNum])
  lnAct_Est=lnAct_Est+(laActuAmnt[lnAmntNum] - laNormCost[lnAmntNum])
ENDIF  
*-- end of lfSayRow.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1999
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
*MAB 07/01/1999 Speed Report performance.
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
