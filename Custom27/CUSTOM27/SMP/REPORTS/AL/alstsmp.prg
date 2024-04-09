*:***************************************************************************
*: Program file  : ALSTSMP
*: Program desc. : ALLOCATION  STYLE LIST WITH AVAILABLE BY SIZE FOR (SMP)
*! Date          : 10/26/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : CP. # 101674
*:***************************************************************************
*: Example : DO ALSTSMP
*:***************************************************************************

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint
llPrint = .F.

*-- llMultiWH  Variable that hold the if there is multiwa or not
llMultiWH  = (ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y')

*-- Initialize variables  for the report criteria 
lcWare      = SPACE(6)                  && var.hold the warehouse 
XPIKTKT     = ''                        && var.hold the piktkt
XORDER      = ''                        && var.hold the order
XFILTER     = ''                        && var.hold the filter
XPRIORITY   = ''                        && var.hold the priority
LDATE       = {}            
HDATE       = {}
lcStyTitle  = gfItemMask('HI')          && var.hold the Style Title
lcString    = ''
lcXString   = ''

*-- Gets the value of the dates from the report criteria 
lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDLINE.PIKDATE'),1)
LDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
HDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))

*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name
*-- XTITLE   variable that hold the report date
*-- R_WIDTH  Variable that hold the report width

R_TITLE    = 'STYLES ALLOCATED '
XREPORT    = 'ALLOCATION STYLE LIST'
XTITLE     = 'DATES => FROM: ' + DTOC(LDATE) + ' THRU: ' + DTOC(HDATE)
R_WIDTH    = 'W'       && STANDARD REPORT IS 'WIDE'

DO CASE
**--- User select Picked from the Allocation status
  CASE lcRpAllST = 'P'
    XFILTER      = lcRpExp + [.AND. !EMPTY(OrdLine.PIKTKT) ] + [.AND. (OrdLine.PIKTKT) # '******' ]

**--- User select Allocated from the Allocation status
  CASE lcRpAllST = 'A'
    XFILTER      = lcRpExp +' .AND. '+ "(" + [EMPTY(OrdLine.PIKTKT) ] + [.OR. (OrdLine.PIKTKT) = '******'] + ")"
    
**--- User select Both from the Allocation status
  CASE lcRpAllST = 'B'
    XFILTER      = lcRpExp
    
ENDCASE

**--- The filter will based on (Open & Hold) orders only
XFILTER = XFILTER + [.AND. OrdHdr.STATUS $ 'OH']

*---THE MAIN PROGRAM    
DO lpCollData

*---- TO PRINT THE REPORT     

IF llPrint
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

IF USED(lcOrdlTemp)
  SELECT (lcOrdlTemp)
  SET RELATION TO
  USE 
ENDIF

*-- ERASE THE lcWorkFile
ERASE (gcWorkDir+lcOrdlTemp+'.DBF')  
ERASE (gcWorkDir+lcOrdlTemp+'.CDX')  
ERASE (gcWorkDir+lcOrdlTemp+'.FPT')  

*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT 
*! Date      : 10/26/1999
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
*! Example     : DO lpCollData
*!*************************************************************

PROCEDURE lpCollData

SELE ORDLINE
SET RELATION TO cordtype + order INTO Ordhdr ADDI
LOCATE ALL FOR &XFILTER

IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ELSE
  WAIT  'Selecting records for report ...' WINDOW NOWAIT
  llPrint = .T.
  COPY REST TO &lcOrdlTemp FOR &XFILTER 
  =gfOpenFile('&lcOrdlTemp',' ','EX')
  
  SELECT (lcOrdlTemp)
  INDEX ON Style TAG &lcOrdlTemp UNIQUE
  
  DO CASE
**--- User select ( Picked & BOTH ) from the Allocation status
  CASE lcRpAllST $ 'BP'
    INDEX ON PIKTKT+ORDER+STYLE+STR(LINENO,6) TAG &SEC_INDEX
    
**--- User select Allocated from the Allocation status
  CASE lcRpAllST = 'A'
    INDEX ON ORDER+STYLE+STR(LINENO,6) TAG &SEC_INDEX    
    
ENDCASE

  SET ORDER TO TAG &lcOrdlTemp
  SET RELATION TO cordtype + order INTO Ordhdr ADDITIVE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  GOTO TOP
ENDIF   

*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT 
*! Date      : 10/26/1999
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

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*
*0                            ALLOCATION REPORT
*1
*2
*3               Enter piktkt date range : 99/99/99  99/99/99
*5               Restart at this piktkt  : xxxxxx
*6
*7

PAGENO  = 0
ROW     = 99

DO WHILE  .T.
  IF EOF()
    EXIT
  ENDIF
  
  IF ROW > 55
    PAGENO = PAGENO + 1

    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW = 5
    ROW = ROW + 1
    @ ROW,00 SAY 'STYLE               STYLE DESCRIPTION...                    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8 TOT.AVL'
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('=',130)
    ROW = ROW + 1
  ENDIF


* 0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....+....9....+....0....+....1....+....2
* STYLE               STYLE DESCRIPTION                       SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8 TOT.AVL
* ±±±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±±±
* 123456789*123456789 123456789*123456789*123456789*123456 999999 999999 999999 999999 999999 999999 999999 999999 9999999
* ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±±±
* 0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....+....9....+....0....+....1....+....2

  @ ROW,000 SAY STYLE
  @ Row,020 SAY LEFT(Style.Desc1,36)
  @ ROW,057 SAY IIF(STYLE->STK1 - STYLE->ALO1<=0,0,STYLE->STK1 - STYLE->ALO1) PICTURE '999999'
  @ ROW,064 SAY IIF(STYLE->STK2 - STYLE->ALO2<=0,0,STYLE->STK2 - STYLE->ALO2) PICTURE '999999'
  @ ROW,071 SAY IIF(STYLE->STK3 - STYLE->ALO3<=0,0,STYLE->STK3 - STYLE->ALO3) PICTURE '999999'
  @ ROW,078 SAY IIF(STYLE->STK4 - STYLE->ALO4<=0,0,STYLE->STK4 - STYLE->ALO4) PICTURE '999999'
  @ ROW,085 SAY IIF(STYLE->STK5 - STYLE->ALO5<=0,0,STYLE->STK5 - STYLE->ALO5) PICTURE '999999'
  @ ROW,092 SAY IIF(STYLE->STK6 - STYLE->ALO6<=0,0,STYLE->STK6 - STYLE->ALO6) PICTURE '999999'
  @ ROW,099 SAY IIF(STYLE->STK7 - STYLE->ALO7<=0,0,STYLE->STK7 - STYLE->ALO7) PICTURE '999999'
  @ ROW,106 SAY IIF(STYLE->STK8 - STYLE->ALO8<=0,0,STYLE->STK8 - STYLE->ALO8) PICTURE '999999'
  @ ROW,113 SAY IIF(STYLE->TOTSTK - STYLE->TOTALO<=0,0,STYLE->TOTSTK - STYLE->TOTALO) PICTURE '9999999'
  ROW = ROW + 1
  SKIP
ENDDO

SELE (lcOrdlTemp)
SET RELATION TO IIF(EMPTY(STORE) ,;
    'M'+ACCOUNT, 'S'+ACCOUNT+STORE ) INTO CUSTOMER ADDI

SET ORDER TO TAG &SEC_INDEX
GOTO TOP
*------------------------------
* REPORT LOOP
*------------------------------
NEWPIK    = .T.
STORE 0.00 TO TOTORD, TOT_PIK, TOTVALO,TOTVALP
ROW       = 99
R_TITLE   ='OPEN PICKING TICKETS'


STORE 0 TO GTOTORD,GVALUEO,GTOT_PIK,GTOTVALP

DO WHILE .T.
   IF EOF()
     EXIT
   ENDIF
   
   *** GET ORDER HEADER
   IF NEWPIK
      XPIKTKT  = PIKTKT
      XORDER   = ORDER
      STORE 0.00 TO XTOTORD, XTOTPIK, XVALUEO,XVALUEP
      NEWPIK = .F.
   ENDIF
   IF ROW >= 55
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH  XREPORT,XTITLE,R_WIDTH
      ROW=5
   ENDIF

  IF SEEK(PIKTKT,'PIKTKT')
    lcWare = PIKTKT->CWARECODE
  ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+
*PIKTKT ORDER#  WAREHOUSE  P  SE DI    ACCT# NAME......     STORE#   PO#        START    COMPLETE
*123456 123456  123456     12 12 12345 12345 12345678901234 12345678 1234567890 MM/DD/YY MM/DD/YY
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

   ROW=ROW+1
   IF llMultiWH
     @ ROW,00 SAY 'PIKTKT ORDER#  WAREHOUSE  P  SEASON DIVI.    ACCT# NAME......     STORE#   PO#        START     COMPLETE'
   ELSE
     @ ROW,00 SAY 'PIKTKT ORDER#  P  SEASON DIVI.    ACCT# NAME......     STORE#   PO#       START     COMPLETE'
   ENDIF
   ROW=ROW+1

   @ ROW,00 SAY PIKTKT
   @ ROW,07 SAY ORDER

   IF llMultiWH
     @ ROW,15 SAY lcWare
   ENDIF
   
   @ ROW,IIF(llMultiWH,25,14) SAY ALLTRIM(ORDHDR.PRIORITY)
   @ ROW,IIF(llMultiWH,29,18) SAY ORDHDR.SEASON
   @ ROW,IIF(llMultiWH,36,25) SAY ORDHDR.CDIVISION
   @ ROW,IIF(llMultiWH,45,34) SAY ACCOUNT
   @ ROW,IIF(llMultiWH,51,40) SAY LEFT(CUSTOMER.BTNAME,14)
   @ ROW,IIF(llMultiWH,66,54) SAY STORE   
   @ ROW,IIF(llMultiWH,75,64) SAY IIF(ORDHDR.MULTIPO,LEFT(CUSTPO,10),LEFT(ORDHDR.CUSTPO,10))
   @ ROW,IIF(llMultiWH,86,74) SAY ORDHDR.START
   @ ROW,IIF(llMultiWH,96,85) SAY ORDHDR.COMPLETE
   ROW = ROW+2

   *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9..
   * GP STYLE        COLOR  STYLE DESCRIPTION... COLOR DESCR....   PRICE  ORDER   PICK  TOT-VALUE
   *  ± ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±± ±±±±.±± ±±±±±± ±±±±±± ±±±±±±±.±±
   *  X 123456789012 123456 12345678901234567890 123456789*12345 1234.99 123456 123456 1234567.99
   *  ± ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±± ±±±±.±± ±±±±±± ±±±±±± ±±±±±±±.±±
   *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9..
   
   @ ROW,01 SAY 'GP '+PADR(lcStyTitle,20)+'STYLE DESCRIPTION                      PRICE  ORDER   PICK  TOT-VALUE'
   ROW = ROW + 1

   *---------------------
   * LOOP ORDER LINES
   *---------------------
    DO WHILE &lcOrdlTemp->PIKTKT = XPIKTKT .AND. &lcOrdlTemp->ORDER = XORDER

     IF ROW >= 55
        EXIT
     ENDIF

     WKAMT  = TOTPIK * PRICE
     WPAMT  = TOTQTY * PRICE
     
     @ ROW,01 SAY GROUP
     @ ROW,04 SAY STYLE
     @ ROW,24 SAY LEFT(STYLE->DESC1,36)
     @ ROW,61 SAY PRICE        PICTURE '9999.99'
     @ ROW,69 SAY TOTQTY       PICTURE '999999'
     @ ROW,76 SAY TOTPIK       PICTURE '999999'
     ROW = ROW+1

     @ ROW,00     SAY "ORD:"
     @ ROW,05     SAY QTY1 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK1<QTY1 ,'*','')
     @ ROW,12     SAY QTY2 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK2<QTY2 ,'*','')
     @ ROW,18     SAY QTY3 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK3<QTY3 ,'*','')
     @ ROW,24     SAY QTY4 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK4<QTY4 ,'*','')
     @ ROW,30     SAY QTY5 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK5<QTY5 ,'*','')
     @ ROW,36     SAY QTY6 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK6<QTY6 ,'*','')
     @ ROW,42     SAY QTY7 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK7<QTY7 ,'*','')
     @ ROW,48     SAY QTY8 PICTURE '99999'
     @ ROW,PCOL() SAY IIF(PIK8<QTY8 ,'*','')

     @ ROW,83 SAY WPAMT    PICTURE '9999999.99'

     ROW = ROW + 1
     @ ROW,00     SAY "PIK:"
     @ ROW,05     SAY PIK1  PICTURE '99999'
     @ ROW,12     SAY PIK2  PICTURE '99999'
     @ ROW,18     SAY PIK3  PICTURE '99999'
     @ ROW,24     SAY PIK4  PICTURE '99999'
     @ ROW,30     SAY PIK5  PICTURE '99999'
     @ ROW,36     SAY PIK6  PICTURE '99999'
     @ ROW,42     SAY PIK7  PICTURE '99999'
     @ ROW,48     SAY PIK8  PICTURE '99999'
     @ ROW,83     SAY WKAMT PICTURE '9999999.99'
     ROW= ROW + 2
     
     XVALUEP  = XVALUEP + WKAMT
     XTOTPIK  = XTOTPIK + TOTPIK

     XVALUEO  = XVALUEO + WPAMT
     XTOTORD  = XTOTORD + TOTQTY

     TOTORD  = TOTORD  + TOTQTY
     TOTVALO = TOTVALO + XVALUEO

     TOT_PIK  = TOT_PIK  + TOTPIK
     TOTVALP  = TOTVALP + XVALUEP
     
       SKIP
   ENDDO

DO CASE
**--- User select Picked from the Allocation status
  CASE lcRpAllST = 'P'
    lcString  = &lcOrdlTemp..PIKTKT
    lcXString = XPIKTKT
**--- User select ( Allocated & Both ) from the Allocation status
  CASE lcRpAllST $  'AB'
    lcString  = &lcOrdlTemp..PIKTKT+&lcOrdlTemp..ORDER
    lcXString = XPIKTKT+XORDER
ENDCASE

   IF lcString <> lcXString   
     GTOTORD=GTOTORD+XTOTORD
     GVALUEO=GVALUEO+XVALUEO
     GTOT_PIK=GTOT_PIK+XTOTPIK
     GTOTVALP=GTOTVALP+XVALUEP
     NEWPIK = .T.

     @ ROW,50 SAY ' TOTAL:'
     @ ROW,57 SAY 'ORD'
     @ ROW,76 SAY XTOTORD     PICTURE '999999'
     @ ROW,83 SAY XVALUEO    PICTURE '9999999.99'
     ROW = ROW + 1
     @ ROW,57 SAY 'PIK'

     @ ROW,76 SAY XTOTPIK    PICTURE '999999'
     @ ROW,83 SAY XVALUEP     PICTURE '9999999.99'
   ELSE
     @ ROW,20 SAY 'CONTINUED NEXT PAGE..'
     @ ROW,47 SAY 'SUBTOTAL:'
     @ ROW,57 SAY 'ORD'

     @ ROW,76 SAY XTOTORD     PICTURE '999999'
     @ ROW,83 SAY XVALUEO     PICTURE '9999999.99'

     ROW = ROW + 1
     @ ROW,57 SAY 'PIK'

     @ ROW,76 SAY XTOTPIK    PICTURE '999999'
     @ ROW,83 SAY XVALUEP    PICTURE '9999999.99'
   ENDIF
   ROW = ROW+1
ENDDO

ROW = ROW+1
@ ROW,00 SAY REPLICATE('-',130)

ROW = ROW+1
@ ROW,42 SAY 'GRAND TOTAL=>'
@ ROW,57 SAY 'ORD'

@ ROW,76 SAY GTOTORD      PICTURE '999999'
@ ROW,83 SAY GVALUEO      PICTURE '9999999.99'

ROW = ROW +1
@ ROW,57 SAY 'PIK'

@ ROW,76 SAY GTOT_PIK       PICTURE '999999'
@ ROW,83 SAY GTOTVALP      PICTURE '9999999.99'

ROW = ROW+1
@ ROW,00 SAY REPLICATE('-',130)

*!*************************************************************
*! Name      : lfvPikt
*! Developer : BASSEM RAFAAT 
*! Date      : 10/26/1999
*! Purpose   : Validate the piktkt
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPikt()
*!*************************************************************

FUNCTION lfvPikt
PRIVATE lcPikFld,lcPikTkt,lnSelFile,lcPikTag
	
lcPikFld  = VARREAD()
lcPikTkt  = EVAL(lcPikFld)
lnSelFile =  SELECT(0)

SELECT PIKTKT
lcPikTag  = ORDER('PIKTKT')
SET ORDER TO TAG PIKTKT IN PIKTKT

IF !EMPTY(lcPikTkt) .AND. (!SEEK(lcPikTkt , 'PIKTKT'))
  DIMENSION laTemp[1]
  laTemp = ''     
  lcBrFields = "Piktkt:H='Piktkt',Account:H='Account',;
                Store:H='Store',Order:H='Order' "
  
   = gfBrows('','Piktkt','laTemp')
  IF !EMPTY(laTemp[1])
    lcPikTkt = laTemp[1]
  ELSE 
    lcPikTkt = ''
  ENDIF
ENDIF

&lcPikFld = lcPikTkt
SET ORDER TO lcPikTag
SELECT (lnSelFile)

