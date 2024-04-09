*:***************************************************************************
*: Program file  : MAPOHDR
*: Program desc. : Material PURCHASE ORDER HEADER
*! Date          : 02/08/1999
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (M)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAPOHDR
*:***************************************************************************
*: Modifications :
*:B802466,1 08/03/1999 Tax & freight lables are reversed re-arrange them
*:B603178,1 KAM  11/04/2000 Report support multi currency format.
*:B604627,1 HBG 17/07/2001 Fix bug of at grand total ,  the open Amnt not aligned with the title 
*:B605822,1 KHM 05/22/2002 Adding the printing of WIP link code.
*:B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars 
*:***************************************************************************

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria

PRIVATE llPrint
llPrint = .F.

*---SELECT THE FILES

SELECT APVENDOR
SELECT POFHDR
SET ORDER TO 0
SET RELATION TO
SET FILTER   TO
GOTO TOP
*B603178,1 KAM open file warehous and relate it with pofhdr to print 
*B603178,1     warehous.cDesc instead of shpname
*--open warehous file 
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')   
ENDIF
*--set relation between pofhdr and warehous
SELECT POFHDR
SET RELATION TO cWARECODE INTO WAREHOUS


*--  Define Variables .[Begin]
*--lcGrpCurr variable to hold price currency
*--lcSameGrp variable to hold logical value that indicate same group
lcGrpCurr = cPriceCur
llSameGrp = .T.

*--declare array to hold values which calculated in case forign currancy
DIMENSION laNormCost[4]
STORE 0 TO laNormCost

*B603178,1 KAM [end]

*** LOOP FOR THE REPORT CRITERIA ***

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name

R_WIDTH    = 'W'
R_TITLE  = 'MATERIAL PURCHASE ORDER HEADER REPORT'
XREPORT    = "MAPOHDR"

*-- XREPORT  variable of the option grid

XFILTER    = lcRpExp          && GET    THE XFILTER 
XSORT      = lcRpSORT         && SORT   THE TEMPFILE
XLONG      = lcRpFrmat        && SELECT LONG OR SHORT PRINT
XPRINTNOTE = llRpNote         && SELECT PRINT NOTE 
XTITLE     = lcRpoptTi        && SELECT OPTIONAL TITLE

*-- WORKFILE  Variable that hold the temporary file name

WORKFILE   = gfTempName()     && NAME OF THE TEMPORARY FILE 
*B603178,1 KAM creat temp file  
lcCurrFile = gfTempName()
*B603178,1 KAM [end]
*---- SORT TO WORKFILE INDEX
SORTFIELD = ' '
BREAK     = ' '


*B603178,1 KAM [START]  to take cpricecur in sortfield also in brake all in;
*B603178,1               multicurrancy case
*DO CASE  
*-- SUMMARY BY VENDOR
*   CASE  XSORT = 'V'
*    SORTFIELD = 'VENDOR+CMATTYPE+POMAT'
*    BREAK = 'VENDOR'
*    TOTALD = 'VENDOR'
*-- SUMMARY BY ORDER#
*   CASE  XSORT = 'P'
*     SORTFIELD = 'POMAT'
*   
*    *-- SUMMARY BY PRIOIRTY
*   CASE XSORT = 'D'
*    SORTFIELD = 'SYS(11,COMPLETE)+VENDOR+CMATTYPE+POMAT'
*    BREAK = 'DTOC(COMPLETE)'
*    TOTALD= 'COMPLETE DATA'
*ENDCASE
*-- 
DO CASE  

*-- SUMMARY BY VENDOR
  CASE  XSORT = 'V'
    IF llMultCurr .OR. lcRpCurr<> "F"
      SORTFIELD= 'VENDOR+CMATTYPE+POMAT+cPriceCur'
      BREAK = 'VENDOR+cPriceCur'
    ELSE
      SORTFIELD = 'VENDOR+CMATTYPE+POMAT'
      BREAK = 'VENDOR'
    ENDIF
    TOTALD = 'VENDOR'

*-- SUMMARY BY ORDER#
  CASE  XSORT = 'P'
    IF llMultCurr .OR. lcRpCurr<> "F"
      SORTFIELD= 'POMAT+cPriceCur'
    ELSE
      SORTFIELD = 'POMAT'
    ENDIF
    
*-- SUMMARY BY PRIOIRTY
  CASE XSORT = 'D'
    IF llMultCurr .OR. lcRpCurr<> "F"
      SORTFIELD=  'SYS(11,COMPLETE)+VENDOR+CMATTYPE+POMAT+cPriceCur'
      BREAK = 'DTOC(COMPLETE)+cPriceCur'
    ELSE
      SORTFIELD = 'SYS(11,COMPLETE)+VENDOR+CMATTYPE+POMAT'
      BREAK = 'DTOC(COMPLETE)'
    ENDIF
    TOTALD= 'COMPLETE DATA'
ENDCASE
*B603178,1 KAM[END]

*-- THE MAIN PROGRAM 
DO lpCollData

*-- PRINT THE REPORT FROM THE WORKFILE
IF llPrint
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

IF USED(WORKFILE)
  SELECT (WORKFILE)
  SET RELATION TO
  USE
ENDIF

*-- ERASE THE WORKFILE
ERASE (gcWorkDir+WORKFILE+'.DBF')
ERASE (gcWorkDir+WORKFILE+'.CDX')
*B603178,1 KAM delete cursor [start]
IF llMultCurr 

  IF USED(lcCurrFile)
    USE IN (lcCurrFile)
  ENDIF  
ENDIF
IF USED('WAREHOUS')
  USE IN ('WAREHOUS')
ENDIF
*B603178,1 KAm [end]
*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT 
*! Date      : 02/08/1999
*! Purpose   : COLLECT THE DATA 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : ......
*!*************************************************************

PROCEDURE lpCollData

SELECT POFHDR
SET FILTER TO

WAIT 'Selecting records for report ...' WINDOW NOWAIT

LOCATE ALL FOR &XFILTER
IF EOF()
  WAIT CLEAR
  WAIT "No records selected for report" WINDOW
  RETURN
ELSE

  llPRint = .T.      
  COPY REST TO &WORKFILE FOR &XFILTER
  SELECT POFHDR
  SET RELATION TO

  =gfOpenFile(WORKFILE,' ','EX')
  SELECT (WORKFILE)
  IF !EMPTY(SORTFIELD)
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT 'Sorting &Z records for material purchase order header report...' WINDOW NOWAIT
    INDEX ON &SORTFIELD TO &WORKFILE
    SET RELATION TO Vendor INTO APVENDOR
    WAIT CLEAR
  ENDIF
  *B603178,1 KAM create temp file used in calling function lfCalAmt [start]
  *B603178,1     to overcome absence nExrate field 
  IF llMultCurr 
    CREATE CURSOR (lcCurrFile) (cCurrCode C(3),nExRate N(9,4),nCurrUnit N(4,0))
    SELECT (lcCurrFile)
    ZAP
    APPEND BLANK
  ENDIF
 *B603178,1 KAM[end]
    
ENDIF

*!*************************************************************
*! Name      : lpPrint
*! Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint
*B603178,1 KAM Define variable hold warehous name
*-- lcWareName variable lcWareName hold warehous name
PRIVATE lcWareName
lcWareName = SPACE(30)
*B603178,1 KAM [end]

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                                                                                                                                                                          
*PO#    VENDOR NAME...................     SHIP-TO NAME....................  DIVISION............  ENTERED           MERCH       ORDER
*STATUS VENDOR ADDRESS................     INSURANCE.......................  TERMS...............  COMPLETE        FREIGHT     RECEIVE
*       ..............................     F.O.B...........................  SHIP VIA............                      TAX      DAMAGE
*       ..............................     QUOTA/CATEGORY..................  CONTACT.............                    QUOTA      CANCEL
*       ..............................     L/C #................EXPIRES....  PHONE  .............                  POTOTAL        OPEN
*                                           CURRENCY.............RATE.......  ORIGIN..............

**                MERCH   FREIGHT       TAX     QUOTA   PO TOTAL       ORDER     RECEIVE        DAMAGE        CANCEL          OPEN
* *SUB TOTALS*999999.99 999999.99 999999.99 999999.99 9999999.99 9999999.999 9999999.999 999999999.999 999999999.999 999999999.999
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

DIMENSION XTOTAL(2,10)
XTOTAL = 0.00
PAGENO = 0
ROW    = 99
XTIME  = TIME()
SELECT &WORKFILE
GOTO TOP
HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------

WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
DO WHILE INKEY()<>32
  *B603178,1 KAM assign space to lcWareName for each record 
  lcWareName = SPACE(30) 
  *B603178,1 KAM [end]
  
  IF ROW >=53
    PAGENO = PAGENO+1
    IF BREAK <> ' '
      XREPORT='MAT660'+'  '+'TOTAL :'+TOTALD
    ENDIF
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH

    IF XLONG = 'L'
      * B128248,1 AHU Manipulate the report to remove its extra 2 chars [Start]
      *@ 06,000 SAY 'PO#    VENDOR NAME...................     SHIP-TO NAME....................  DIVISION............  ENTERED          MERCH       ORDER'
       @ 06,000 SAY 'PO#    VENDOR NAME...................   SHIP-TO NAME....................  DIVISION............  ENTERED          MERCH       ORDER'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
      *-- HDM B802466,1 [Start] Tax & freight lables are reversed re-arrange them
      *@ 07,000 SAY 'STATUS VENDOR ADDRESS................     INSURANCE.......................  TERMS...............  COMPLETE       FREIGHT     RECEIVE'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
      *@ 07,000 SAY 'STATUS VENDOR ADDRESS................     INSURANCE.......................  TERMS...............  COMPLETE         TAX       RECEIVE'
       @ 07,000 SAY 'STATUS VENDOR ADDRESS................   INSURANCE.......................  TERMS...............  COMPLETE         TAX       RECEIVE'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]      
      *B605822,1 KHM 05/22/2002 (Begin) Adding the WIP LINK code.
      *@ 08,000 SAY '       ..............................     F.O.B...........................  SHIP VIA............                   FREIGHT    DAMAGE'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
      *@ 08,000 SAY '       ..............................     F.O.B...........................  SHIP VIA............  WIP LINK         FREIGHT    DAMAGE'
       @ 08,000 SAY '       ..............................   F.O.B...........................  SHIP VIA............  WIP LINK         FREIGHT    DAMAGE'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
      *B605822,1 KHM 05/22/2002 (End)
      
     *@ 08,000 SAY '       ..............................     F.O.B...........................  SHIP VIA............                     TAX      DAMAGE'
     *-- HDM B802466,1 [End]
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
      *@ 09,000 SAY '       ..............................     QUOTA/CATEGORY..................  CONTACT.............                   QUOTA      CANCEL'
       
       @ 09,000 SAY '       ..............................   QUOTA/CATEGORY..................  CONTACT.............                   QUOTA      CANCEL'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
      *B603178,KAM to prevent word "pototoal" from appearing in main title  [start]
      *@ 10,000 SAY '       ..............................     L/C #................EXPIRES....  PHONE  .............                 POTOTAL        OPEN'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [START]
      *@ 10,000 SAY '       ..............................     L/C #................EXPIRES....  PHONE  .............                 '+;
            *IIF(llMultCurr AND lcRpCurr="F",SPACE(7),'  POTOTAL')+SPACE(5)+'OPEN' 
       @ 10,000 SAY '       ..............................   L/C #................EXPIRES....  PHONE  .............                 '+;
          IIF(llMultCurr AND lcRpCurr="F",SPACE(7),'  POTOTAL')+SPACE(5)+'OPEN' 

            *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
      *B603178,KAM [end]
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [START]      
       
      * @ 11,000 SAY '       ..............................     CURRENCY.............RATE.......  ORIGIN..............'
        @ 11,000 SAY '       ..............................   CURRENCY.............RATE.......  ORIGIN..............'
      *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
      ROW = 12
    ELSE
      @ 05,00 SAY ' PO#    VENDOR   VENDOR NAME      S DIVISION ENTERED  COMPLETE     ORDERED    RECEIVED       DAMAGE     CANCELLED        OPEN'
      ROW = 7
    ENDIF
  ENDIF
  DO WHILE !EMPTY(BREAK) 
    IF EVAL(BREAK)= HBREAK
      EXIT
    ENDIF
    ROW=ROW+1
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [Start]
   *@ ROW,00 SAY REPLICATE('=',132)
    @ ROW,00 SAY REPLICATE('=',130)
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [End]
    ROW = ROW+1
    @ ROW,001 SAY HBREAK
    IF XLONG = 'L'
      *-- HDM B802466,1 [Start] Tax & freight lables are reversed re-arrange them
    
      *@ ROW,012 SAY '       MERCH    FREIGHT        TAX      QUOTA      PO TOTAL       ORDER      RECEIVE      DAMAGE      CANCEL        OPEN'
      *B603178,1 KAM  to prevent "pototal" word from display in subtotal title[start]
      *@ ROW,012 SAY '        MERCH        TAX    FREIGHT      QUOTA      PO TOTAL       ORDER     RECEIVE      DAMAGE      CANCEL        OPEN'
     *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
     * @ ROW,012 SAY  IIF(llMultCurr AND lcRpCurr="F",'        MERCH        '+SPACE(45)+;
                      'ORDER     RECEIVE      DAMAGE      CANCEL        OPEN',;
                      '        MERCH        TAX    FREIGHT      QUOTA      PO TOTAL       ORDER     RECEIVE      DAMAGE      CANCEL        OPEN' )            
      @ ROW,012 SAY  IIF(llMultCurr AND lcRpCurr="F",'      MERCH        '+SPACE(45)+;
                   'ORDER     RECEIVE      DAMAGE      CANCEL        OPEN',;
                   '      MERCH        TAX    FREIGHT      QUOTA      PO TOTAL       ORDER     RECEIVE      DAMAGE      CANCEL        OPEN' )            
                  
     *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [End]
      *B603178,1 KAM [end]
      *-- HDM B802466,1 [End]

      ROW = ROW+1
     *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
      *@ ROW,000 SAY '*SUB TOTAL* '
      *@ ROW,012 SAY XTOTAL(1,1)  PICTURE '999999999.999'
       @ ROW,000 SAY 'SUB TOTAL '
       @ ROW,010 SAY XTOTAL(1,1)  PICTURE '999999999.999'
      *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [End]     
      *B603178,1 KAM  to display xtotal[1,1--5] incase single curr
      *b603178,1 KAM  also in case lcrpcurr<>"f"
      IF !llMultCurr OR lcRpCurr <> "F"
      *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
       *@ ROW,025 SAY XTOTAL(1,2)  PICTURE '9999999.999'
       *@ ROW,036 SAY XTOTAL(1,3)  PICTURE '9999999.999'
       *@ ROW,047 SAY XTOTAL(1,4)  PICTURE '9999999.999'
       *@ ROW,058 SAY XTOTAL(1,5)  PICTURE '9999999999.999'
        
        @ ROW,023 SAY XTOTAL(1,2)  PICTURE '9999999.999'
        @ ROW,034 SAY XTOTAL(1,3)  PICTURE '9999999.999'
        @ ROW,045 SAY XTOTAL(1,4)  PICTURE '9999999.999'
        @ ROW,056 SAY XTOTAL(1,5)  PICTURE '9999999999.999'
      *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [END]
      ENDIF  
      *B603178,1 KAM[end]
      *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
      *@ ROW,071 SAY XTOTAL(1,6)  PICTURE '99999999.999'
      *@ ROW,084 SAY XTOTAL(1,7)  PICTURE '99999999.999'
      *@ ROW,097 SAY XTOTAL(1,8)  PICTURE '9999999.999'
      *@ ROW,109 SAY XTOTAL(1,9)  PICTURE '9999999.999'
      *@ ROW,120 SAY XTOTAL(1,10) PICTURE '99999999.999'
      
       @ ROW,069 SAY XTOTAL(1,6)  PICTURE '99999999.999'
       @ ROW,082 SAY XTOTAL(1,7)  PICTURE '99999999.999'
       @ ROW,095 SAY XTOTAL(1,8)  PICTURE '9999999.999'
       @ ROW,107 SAY XTOTAL(1,9)  PICTURE '9999999.999'
       @ ROW,118 SAY XTOTAL(1,10) PICTURE '99999999.999'

      *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [END]
    ELSE    && IF IT IS A SHORT FORMAT
      @ ROW,062  SAY XTOTAL(1,6)  PICTURE '99999999.999'     &&56
      @ ROW,074  SAY XTOTAL(1,7)  PICTURE '99999999.999'     &&68
      @ ROW,087  SAY XTOTAL(1,8)  PICTURE '99999999.999'     &&81
      @ ROW,0101 SAY XTOTAL(1,9)  PICTURE '99999999.999'     &&93
      @ ROW,113 SAY XTOTAL(1,10) PICTURE '99999999.999'     &&107
    ENDIF

    ROW = ROW+1
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,00 SAY REPLICATE('=',132)
     @ ROW,00 SAY REPLICATE('=',130)
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [End]
    ROW = ROW+1
    ******************** INITIALIZE SUBTOTALS ************
    X = 1
    DO WHILE X<=10
      XTOTAL(2,X) = XTOTAL(2,X) + XTOTAL(1,X)
      XTOTAL(1,X) = 0.00
      X = X+1
    ENDDO
    HBREAK = &BREAK
    EXIT
  ENDDO
  
  *--------------------- END SUBTOTALS ----------------------------
  IF EOF()
    EXIT
  ENDIF
  IF ROW >=53
    ROW = 99
    LOOP
  ENDIF

  SELECT &WORKFILE
  ********************** LONG FORMAT ******************************
*B603178,1 KAM call function to calculate values in case forgin currancy[start]
  IF llMultCurr AND lcRpCurr<>"F"
    *-- assign valuse from workfile to variables
    m.cCurrCode = cPriceCur
    m.nExRate   = nPriceRat
    m.nCurrUnit = nCurrUnit
    =lfCalAmt()
  ELSE
   *-- in case single curr also in case which neednot calculatio 
    laNormCost[1] =NCOST1
    laNormCost[3] =NCOST3
    laNormCost[2] =NCOST2
    laNormCost[4] =NCOST4
  ENDIF

  lcWareName = lfGetWareName(cWareCode)
*B603178,1 KAM[end]

  XPO = POMAT
  IF XLONG = 'L'

    DIMENSION laCompAdd[5]
    laCompAdd[1] = gfGetAdr('APVENDOR', '' , '' , '' , 1)
    laCompAdd[2] = gfGetAdr('APVENDOR', '' , '' , '' , 2)
    laCompAdd[3] = gfGetAdr('APVENDOR', '' , '' , '' , 3)
    laCompAdd[4] = gfGetAdr('APVENDOR', '' , '' , '' , 4)
    laCompAdd[5] = gfGetAdr('APVENDOR', '' , '' , '' , 5)
    =lfAdrShift('laCompAdd')
    
    XVADDR1 = laCompAdd[1]
    XVADDR2 = laCompAdd[2]
    XVADDR3 = laCompAdd[3]
    XVADDR4 = laCompAdd[4]
    XVADDR5 = laCompAdd[5]

    Z = ' '
*--bas
    *PTERMS    = Z + gfCodDes(cTermCode,'CTERMCODE')
    *PSHIPVIA  = Z + gfCodDes(ShipVia,'SHIPVIA')
    PTERMS    = gfCodDes(cTermCode,'CTERMCODE')
    PSHIPVIA  = gfCodDes(ShipVia,'SHIPVIA')
*--bas    
    PDIVISION = Z + gfCodDes(cDivision,'CDIVISION')

    SELECT &WORKFILE

  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,000 SAY REPLICATE('-',132)
     @ ROW,000 SAY REPLICATE('-',130)
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [End]
    ROW = ROW+1
    @ ROW,000 SAY POMAT

    @ ROW,007 SAY APVENDOR.cVenComp
    *B603178,1 KAM printing lcWareName instead of shpname
    *@ ROW,042 SAY SHPNAME
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,042 SAY  lcWareName
     @ ROW,040 SAY  lcWareName
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
    *B603178,1 KAM [end]
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,076 SAY SUBSTR(CDIVISION + PDIVISION,1,20)
     @ ROW,074 SAY SUBSTR(CDIVISION + PDIVISION,1,20)
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,098 SAY ENTERED
     @ ROW,096 SAY ENTERED
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
    *B603178,1 KAM to display value from array instead of file [START]
    *@ ROW,107 SAY NCOST1 PICTURE '999999999.999'
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,107 SAY laNormCost[1] PICTURE '999999999.999'
     @ ROW,105 SAY laNormCost[1] PICTURE '999999999.999'
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
    *B603178,1 KAM[END]
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,121 SAY NFABORDER PICTURE '9999999.999'
     @ ROW,119 SAY NFABORDER PICTURE '9999999.999'
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]    
    ROW = ROW+1

    
    * LINE #2
    DO CASE
      CASE STATUS = 'O'
        @ ROW,00 SAY 'OPEN'
      CASE STATUS = 'C'
        @ ROW,00 SAY 'CMPLTE'
      CASE STATUS = 'X'
        @ ROW,00 SAY 'CANCEL'
      CASE STATUS = 'B'
        @ ROW,00 SAY 'BID'
    ENDCASE
    
    @ ROW,007 SAY XVADDR1
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,042 SAY INSURANCE
     @ ROW,040 SAY INSURANCE
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END] 
*--bas    
    *@ ROW,076 SAY SUBSTR(CTERMCODE + PTERMS,1,20)
  *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,076 SAY SUBSTR(PTERMS,1,20)
     @ ROW,074 SAY SUBSTR(PTERMS,1,20)
  *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [end]
*--bas    
    @ ROW,098 SAY COMPLETE    
    *B603178,1 KAM  to display value from array instead of file [START]
    *@ ROW,107 SAY NCOST3 PICTURE '999999999.999'
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,107 SAY laNormCost[3] PICTURE '999999999.999'
     @ ROW,105 SAY laNormCost[3] PICTURE '999999999.999'
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [END]
    *B603178,1 KAM[END]
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,121 SAY NFBRECEIVE PICTURE '9999999.999'
     @ ROW,119 SAY NFBRECEIVE PICTURE '9999999.999'
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    ROW = ROW+1

    *LINE #3
    @ ROW,007 SAY XVADDR2
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,042 SAY cFOB
     @ ROW,040 SAY cFOB
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
*--bas    
    *@ ROW,076 SAY SUBSTR(SHIPVIA + PSHIPVIA,1,29)

    *B605822,1 KHM 05/22/2002 (Begin) Reducing the shipvia to 20 char and
    *B605822,1                adding the printing of wip link code
    *@ ROW,076 SAY SUBSTR(PSHIPVIA,1,29)
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,076 SAY SUBSTR(PSHIPVIA,1,20)
    *@ ROW,098 SAY ALLTRIM(LINK_CODE)
     @ ROW,074 SAY SUBSTR(PSHIPVIA,1,20)
     @ ROW,096 SAY ALLTRIM(LINK_CODE)
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    *B605822,1 KHM 05/22/2002 (End)
*--    
    *B603178,1 KAM to display value from array instead of file [START]
    *@ ROW,107 SAY NCOST2 PICTURE '999999999.999'
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
   * @ ROW,107 SAY laNormCost[2] PICTURE '999999999.999'
     @ ROW,105 SAY laNormCost[2] PICTURE '999999999.999'
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    *B603178,1 KAM[END]
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,121 SAY NFABDAMAGE PICTURE '9999999.999'
     @ ROW,119 SAY NFABDAMAGE PICTURE '9999999.999'
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    ROW = ROW+1

    * LINE #4
    @ ROW,007 SAY XVADDR3
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,042 SAY QUOTACAT
    *@ ROW,076 SAY CONTACT
     @ ROW,040 SAY QUOTACAT
     @ ROW,074 SAY CONTACT
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    *B603178,1 KAM to display value from array instead of file [START]
    *@ ROW,107 SAY NCOST4 PICTURE '999999999.999'        
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,107 SAY laNormCost[4] PICTURE '999999999.999'        
     @ ROW,105 SAY laNormCost[4] PICTURE '999999999.999'        
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    *B603178,1 KAM[END]
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,121 SAY NFABCANCEL PICTURE '9999999.999'
     @ ROW,119 SAY NFABCANCEL PICTURE '9999999.999'
   *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    ROW = ROW+1

    * LINE 5
    @ ROW,007 SAY XVADDR4
       *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
    *@ ROW,042 SAY cLCNO
    *@ ROW,062 SAY LCEXPIRE
    *@ ROW,076 SAY PHONE  PICTURE gfPhoneTem()
     
     @ ROW,040 SAY cLCNO
     @ ROW,060 SAY LCEXPIRE
     @ ROW,074 SAY PHONE  PICTURE gfPhoneTem()

     *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    *B603178,1 KAM do not display pototal when mulicurrancy company[start] 
    IF !llMultCurr OR lcRpCurr<>"F" 
      @ ROW,106 SAY POTOTAL PICTURE '9999999999.999'
    ENDIF
    *B603178,1 KAM [end]  
     *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
      *@ ROW,121 SAY NPO_OPEN PICTURE '9999999.999'
       @ ROW,119 SAY NPO_OPEN PICTURE '9999999.999'
     *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
    ROW = ROW+1

    * LINE 6
    @ ROW,007 SAY XVADDR5
    *B603178,1 KAM use cPriceCode instead of cCurrCode,also cPricerat[start]
    *B603178,1 KAM instead of rate 
    *@ ROW,043 SAY cCURRCODE
    *@ ROW,057 SAY RATE PICTURE '99999999999.9999'
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]
     *@ ROW,043 SAY cPRICECur
     *@ ROW,057 SAY nPriceRat PICTURE '99999999999.9999'
     @ ROW,041 SAY cPRICECur
     @ ROW,055 SAY nPriceRat PICTURE '99999999999.9999'
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]
     *B603178,1 KAM[end]
    *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [Start]     
     *@ ROW,076 SAY ORIGIN
      @ ROW,074 SAY ORIGIN
     *B128248 ,1 AHU Manipulate the report to remove its extra 2 chars [End]

  ELSE            *** IF SHORT FORMAT

*---- THE OLD REPORT
*!    ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!..  PO#    VENDOR   VENDOR NAME      S DI ENTERED  COMPLETE     ORDERED    RECEIVED       DAMAGE     CANCELLED        OPEN
*!..  ±±±±±± ±±±±±±±± ±±±±±±±±±±±±±±±± ± ±± ±±/±±/±± ±±/±±/±± ±±±±±±±±.±± ±±±±±±±±.±±  ±±±±±±±±.±±   ±±±±±±±±.±± ±±±±±±±±.±±
*!..  123456 12345678 1234567890123456 1 12 12345678 12345678 12345678901 12345678901  12345678901   12345678901 12345678901

*---- THE NEW REPORT
*!    ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!..  PO#    VENDOR   VENDOR NAME      S DIVISION ENTERED  COMPLETE     ORDERED    RECEIVED       DAMAGE     CANCELLED        OPEN
*!..  ±±±±±± ±±±±±±±± ±±±±±±±±±±±±±±±± ± ±±++++++ ±±/±±/±± ±±/±±/±± ±±±±±±±±.±± ±±±±±±±±.±±  ±±±±±±±±.±±   ±±±±±±±±K.±± ±±±±±±±±.±±
*!..  123456 12345678 1234567890123456 1 12345678 12345678 12345678 12345678901 12345678901  12345678901   12345678901 12345678901

    @ ROW,01  SAY  POMAT 
    @ ROW,08  SAY  VENDOR
    @ ROW,17  SAY  SUBSTR( APVENDOR.cVenComp,1,16)
    @ ROW,34  SAY  STATUS
    @ ROW,36  SAY  CDIVISION                            
    @ ROW,44  SAY  ENTERED                              
    @ ROW,54  SAY  COMPLETE                             
    @ ROW,63  SAY  NFABORDER    PICTURE '9999999.999'   
    @ ROW,75  SAY  NFBRECEIVE  PICTURE '9999999.999'    
    @ ROW,88  SAY  NFABDAMAGE   PICTURE '9999999.999'   
    @ ROW,102 SAY  NFABCANCEL   PICTURE '9999999.999'   
    @ ROW,114 SAY NPO_OPEN    PICTURE '9999999.999'    

  ENDIF
  *B603178,1 KAM sum subtotal values from array instead of file[START]

  *XTOTAL(1,1) = XTOTAL(1,1) + NCOST1
  *XTOTAL(1,2) = XTOTAL(1,2) + NCOST3
  *XTOTAL(1,3) = XTOTAL(1,3) + NCOST2
  *XTOTAL(1,4) = XTOTAL(1,4) + NCOST4

  XTOTAL(1,1) = XTOTAL(1,1) + laNormCost[1]
  XTOTAL(1,2) = XTOTAL(1,2) + laNormCost[3]
  XTOTAL(1,3) = XTOTAL(1,3) + laNormCost[2]
  XTOTAL(1,4) = XTOTAL(1,4) + laNormCost[4]
  *B603178,1 KAM [END]
  
  XTOTAL(1,5) = XTOTAL(1,5) + POTOTAL
  XTOTAL(1,6) = XTOTAL(1,6) + NFABORDER
  XTOTAL(1,7) = XTOTAL(1,7) + NFBRECEIVE
  XTOTAL(1,8) = XTOTAL(1,8) + NFABDAMAGE
  XTOTAL(1,9) = XTOTAL(1,9) + NFABCANCEL
  XTOTAL(1,10)= XTOTAL(1,10)+ NPO_OPEN
  ROW=ROW+1

  IF XPRINTNOTE = 'Y'
    DO PRT_NOTE WITH 'M','P'+XPO
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
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [Start]
   *@ ROW,00 SAY REPLICATE('*',132)
    @ ROW,00 SAY REPLICATE('*',130)
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [End]
ROW = ROW+1
  IF XLONG = 'L'
  
    *-- HDM B802466,1 [Start] Tax & freight lables are reversed re-arrange them
   *@ ROW,000 SAY '                   MERCH    FREIGHT        TAX      QUOTA      PO TOTAL       ORDER      RECEIVE      DAMAGE      CANCEL        OPEN'
   *B603178,1 KAM  to prevent words "TAX,FREIGHT,QUOTA,PO TOTAL" from display [start]
   *B603178,1 KAM  in main footer  
   *@ ROW,000 SAY '                    MERCH        TAX    FREIGHT      QUOTA      PO TOTAL       ORDER     RECEIVE      DAMAGE      CANCEL        OPEN'
   *B604627,1 HBG 17/07/2001 Fix bug of at grand total ,  the open Amnt not aligned with the title [Begin]
   *@ ROW,000 SAY IIF(llMultCurr ,SPACE(78)+;
                  'ORDER     RECEIVE      DAMAGE      CANCEL        OPEN',;
                  '                    MERCH        TAX    FREIGHT      QUOTA      PO TOTAL        ORDER     RECEIVE      DAMAGE      CANCEL        OPEN'  )
  *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
   *@ ROW,000 SAY IIF(llMultCurr ,SPACE(78)+;
                  'ORDER     RECEIVE      DAMAGE      CANCEL        OPEN',;
                  '                    MERCH        TAX    FREIGHT      QUOTA      PO TOTAL       ORDER     RECEIVE      DAMAGE      CANCEL        OPEN'  )
    @ ROW,000 SAY IIF(llMultCurr ,SPACE(76)+;
                  'ORDER     RECEIVE      DAMAGE      CANCEL        OPEN',;
                  '                  MERCH        TAX    FREIGHT      QUOTA      PO TOTAL       ORDER     RECEIVE      DAMAGE      CANCEL        OPEN'  )

  *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [END]
   *B604627,1 HBG 17/07/2001 [End]
   *B603178,1 KAM [end]

   *-- HDM B802466,1 [End]
  
    ROW = ROW+1
    *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
     *@ ROW,000 SAY 'GRAND TOTAL'
      @ ROW,000 SAY 'GRAND TOT'
    *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [End]
    *B603178,1 KAM to display grand totla xtotal[1,1--5] [start]
    *B603178,1 KAM only in case single currancy
    IF !llMultCurr &&OR lcRpCurr <> "F"
    *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
     *@ ROW,012 SAY XTOTAL(X,1)  PICTURE '999999999.999'
     *@ ROW,025 SAY XTOTAL(X,2)  PICTURE '9999999.999'
     *@ ROW,036 SAY XTOTAL(X,3)  PICTURE '9999999.999'
     *@ ROW,047 SAY XTOTAL(X,4)  PICTURE '9999999.999'
     *@ ROW,058 SAY XTOTAL(X,5)  PICTURE '9999999999.999'     
      
      @ ROW,010 SAY XTOTAL(X,1)  PICTURE '999999999.999'
      @ ROW,023 SAY XTOTAL(X,2)  PICTURE '9999999.999'
      @ ROW,034 SAY XTOTAL(X,3)  PICTURE '9999999.999'
      @ ROW,045 SAY XTOTAL(X,4)  PICTURE '9999999.999'
      @ ROW,056 SAY XTOTAL(X,5)  PICTURE '9999999999.999'     

    *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [End]
    
    ENDIF
   *B603178,1 KAM[end]
    *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [Start]
     *@ ROW,071 SAY XTOTAL(X,6)  PICTURE '99999999.999'  
     *@ ROW,084 SAY XTOTAL(X,7)  PICTURE '99999999.999'
     *@ ROW,097 SAY XTOTAL(X,8)  PICTURE '9999999.999'
     *@ ROW,109 SAY XTOTAL(X,9)  PICTURE '9999999.999'
     *@ ROW,120 SAY XTOTAL(X,10) PICTURE '99999999.999'
      
      @ ROW,069 SAY XTOTAL(X,6)  PICTURE '99999999.999'  
      @ ROW,082 SAY XTOTAL(X,7)  PICTURE '99999999.999'
      @ ROW,095 SAY XTOTAL(X,8)  PICTURE '9999999.999'
      @ ROW,107 SAY XTOTAL(X,9)  PICTURE '9999999.999'
      @ ROW,118 SAY XTOTAL(X,10) PICTURE '99999999.999'
    *B128248,1 AHU 05/28/2006 Manipulate the report to remove its extra 2 chars [End]   
 ELSE

    @ ROW,00  SAY '* GRAND TOTAL *'
    @ ROW,62  SAY XTOTAL(X,6)  PICTURE '99999999.999' &&64
    @ ROW,74  SAY XTOTAL(X,7)  PICTURE '99999999.999'  && 60
    @ ROW,87  SAY XTOTAL(X,8)  PICTURE '99999999.999'  &&91 
    @ ROW,101 SAY XTOTAL(X,9)  PICTURE '99999999.999'  &&94
    @ ROW,110 SAY XTOTAL(X,10) PICTURE '99999999.999'  &&118
 ENDIF

ROW = ROW+1
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [Start]
   *@ ROW,00 SAY REPLICATE('*',132)
    @ ROW,00 SAY REPLICATE('*',130)
  * B128248,1 AHU Manipulate the report to remove its extra 2 chars [Start]

*!*************************************************************
*! Name      : lfvVen
*! Developer : BASSEM RAFAAT 
*! Date      : 02/08/1999
*! Purpose   : Validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVen()
*!*************************************************************

FUNCTION lfvVen

lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)

SELECT APVENDOR
SET ORDER TO TAG VenCode 

IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor 

*!*************************************************************
*! Name      : lfvPo
*! Developer : BASSEM RAFAAT 
*! Date      : 02/08/1999
*! Purpose   : VALIDATE THE PURCHASE ORDER MATERIAL
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPo()
*!*************************************************************

FUNCTION lfvPo

lcPoBet  = VARREAD()
lcPomat  = EVAL(lcPoBet)

SELECT POFHDR
SET ORDER TO TAG POFHDR

IF !EMPTY(lcPomat).AND. ('?' $ lcPomat .OR. !SEEK('P'+lcPomat ,'POFHDR')) 
  =POFBROW ('P','',@lcPomat)
ENDIF

&lcPoBet = lcPomat 

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT 
*! Date      : 02/08/1999
*! Purpose   : SHIFT THE ADDRESS 
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : lcArrayNam
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     := lfAdrShift()
*!*************************************************************

FUNCTION lfAdrShift

PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfFillVars
*: Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 04/11/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*B603178,1
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
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

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfClearRep
*: Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*B603178,1
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
*-- End Of lfClearRep.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
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
*B603178,1
FUNCTION lfwRepWhen

**B603178,1 Evaluate Currency filter postion. [Begin]
IF llMultCurr
  lnCurrPos  = lfItmPos('POFHDR.cPriceCur')
ELSE
  lcRpCurr = "O"
ENDIF  

*-- end of lfwRepWhen.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*!B603178,1 
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.

*!*************************************************************
*! Name      : lfSetCurr
*! Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : From the FRX.
*!*************************************************************
*! Example   : =lfSetCurr()
*!*************************************************************
*B603178,1
Function lfSetCurr
lcGrpCurr = cPriceCur
llSameGrp = .T.
Return ""
*-- End Of lfSetCurr.
*!*************************************************************
*! Name      : lfSameCurr
*! Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : From the FRX.
*!*************************************************************
*! Example     : =lfSameCurr()
*!*************************************************************
*B603178,1
Function lfSameCurr
IF (lcRpCurr= "F") AND llSameGrp
  llSameGrp = (lcGrpCurr = cPriceCur)
ENDIF

Return ""

*-- End Of lfSameCurr.

*!*************************************************************
*! Name      : lfCalAmt
*! Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
*! Purpose   : to compute some values in case forign currancy
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : .
*!*************************************************************
*! Example     : =lfCalAmt()
*!*************************************************************
FUNCTION lfCalAmt
*--lcOldArea to hold old area
*--lnRecNo to hold the current postion in old area
PRIVATE lcOldArea,lnRecNo
lcOldArea=SELECT()
lnRecNo  =RECNO()
*--use temp file to overcome the absence the nExRate field from main file
*-- then gather this values from file
SELECT (lcCurrFile)
GATHER MEMVAR
SELECT (WORKFILE)
IF NCOST1>0
  laNormCost[1]=gfAmntDisp(nCost1,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,lcCurrFile)    
ENDIF
IF NCOST3>0
  laNormCost[3]=gfAmntDisp(nCost3,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,lcCurrFile)
ENDIF
IF NCOST2>0
  laNormCost[2]=gfAmntDisp(nCost2,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,lcCurrFile)
ENDIF
IF NCOST4>0
  laNormCost[4]=gfAmntDisp(nCost4,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,lcCurrFile)
ENDIF

SELECT(lcOldArea)
GOTO lnRecNo
*-- end function lfCalAmt
************************************************************************************************
*!*************************************************************
*! Name      : lfGetWareName
*! Developer : ABD ELRAHIM KOURA-(KAM)
*! Date      : 11/04/2000
*! Purpose   : to get cWareName for cWareCode
*!*************************************************************
*! Passed Parameters  : cWareCode
*!*************************************************************
*! Called from : .lpprint 
*!*************************************************************
*! Example     : x=lfGetWareName(z)
*!*************************************************************
FUNCTION lfGetWareName  
PARAMETER lcTmpWare
PRIVATE lcOldArea,lnRecNo,lcRetVal
lcOldArea=SELECT()
lnRecNo  =RECNO()
SELECT WAREHOUS
IF SEEK(lcTmpWare)
  lcRetVal=WAREHOUS.cDesc
ELSE  
  lcRetVal=SPACE(30)
ENDIF
SELECT(lcOldArea)
IF BETWEEN(lnRecNo,1,RECCOUNT(lcOldArea))
  GO lnRecNo
ENDIF
RETURN lcRetVal  
*************************************************************************************************

