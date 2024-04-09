*:***************************************************************************
*: Program file  : ALSTLSD1
*: Program desc. : ALLOCATION  STYLE LIST WITH AVAILABLE BY SIZE.
*! Date          : 03/30/1999
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
*: Notes   : Enh. # 301184
*:***************************************************************************
*: Example : DO ALSTLSD1
*:***************************************************************************
*E301249,1 IHB 06/20/1999 Make priority field C(3) in CUSTOMER 
*E301249,1 				  and ORDHDR files
*B802656,1 BWA 10/06/1999 Fix the bug of variable lcware not found [start]
*B603225,1 BWA 10/26/1999 Fix the bug of fields(season-div-po#-start Date-compete Date)
*B603225,1                repeted for all the piktkt#
*E500323,1 SAM 01/02/2000 add filter to select Warehouses in multi warehouses (add in syrepuvr)
*B803319,1 AME 06/04/2000 Fix the bug of print one header for orders that have no piktkt
*B803319,1                i.e. Allocated Records have ****** only.
*C101829,1 HBG 07/13/2000 This report is modified for Dorby :
*C101829,1                1 - Add 2 fields to end of the "P/T header line".
*C101829,1                2 - Remove the first page that prints the style summary.
*C101829,1                3 - Allocation quantities (******) should print order each 
*C101829,1                    order information and store number for each style
*:***************************************************************************

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint
llPrint = .F.

*-- llMultiWH  Variable that hold the if there is multiwa or not
llMultiWH  = (ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y')

*-- Initialize variables  for the report criteria 
*B802656,1 BWA 10/06/1999 Fix the bug of variable lcware not found [start]
lcWare    = SPACE(6)
*B802656,1 BWA 10/06/1999 [END]
XPIKTKT   = ''
XFILTER   = ''
XPRIORITY = ''
LDATE     = {}
HDATE     = {}
*-- Gets the value of the dates from the report criteria 
IF ASCAN(laOgFxFlt,'ORDLINE.PIKDATE') = 0
  =gfModalGen('TRM00250B00000','DIALOG', 'pick ticket date range') 
  RETURN
ELSE
  lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDLINE.PIKDATE'),1)
  LDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
  HDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
  IF EMPTY(LDATE) 
    =gfModalGen('TRM00250B00000','DIALOG','pick ticket date range') 
    RETURN
  ENDIF
ENDIF

*-- lcOrdlTemp  Variable that hold the temporary file name
lcOrdlTemp = gfTempName()         && TEMPORARY FILE FOR THE ORDER LINES
SEC_INDEX  = gfTempName()         && SECOND INDEX BY PIKTKT
lcStyTitle = gfItemMask('HI')             && Style Title

*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name
*-- XTITLE   variable that hold the report date

R_TITLE    = 'STYLES ALLOCATED '
XREPORT    = 'ALSTLSD1'
XTITLE     = 'DATES => FROM: ' + DTOC(LDATE) + '   THRU: ' + DTOC(HDATE)
XFILTER    = lcRpExp + ' .AND. TOTPIK<>0 '

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
*! Date      : 03/30/1999
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
  *B803319,1 Add Order# to index AME[Start] 
  *INDEX ON PIKTKT+STYLE+STR(LINENO,6) TAG &SEC_INDEX UNIQUE
  
  *B101829, HBG 19/07/2000 Add store to the index [Begin]
  *INDEX ON PIKTKT+Order+STYLE+STR(LINENO,6) TAG &SEC_INDEX UNIQUE
  INDEX ON PIKTKT+Order+Store+STYLE+STR(LINENO,6) TAG &SEC_INDEX UNIQUE
  *B101829, HBG 19/07/2000 Add store to the index [End  ]
  
  *B803319,1 AME [End]
  SET ORDER TO TAG &lcOrdlTemp
  SET RELATION TO cordtype + order INTO Ordhdr ADDI
  *B603225,1 BWA 10/26/1999 Fix the bug of the fields repeted for all the piktkt# [START]
  *SET RELATION TO STYLE INTO STYLE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  *B603225,1 BWA 10/26/1999 [END]
  GOTO TOP
ENDIF   

*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT 
*! Date      : 03/30/1999
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

*C101829,1 HBG 07/13/2000 Array to hold the code description [Begin]
PRIVATE laCodeDesc

DIMENSION  laCodeDesc[2,3]
laCodeDesc[1,2] = 'CTERMCODE'
laCodeDesc[2,2] = 'SHIPVIA'
*C101829,1 HBG 07/13/2000 Array to hold the code description [END  ]




*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*
*0                            ALLOCATION REPORT
*1
*2
*3               Enter piktkt date range : 99/99/99  99/99/99
*5               Restart at this piktkt  : xxxxxx
*6
*7

PAGENO = 0
ROW     = 99

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
   
   *C101829,1 HBG 07/13/2000 
   laCodeDesc[1,1] = ORDHDR.CTERMCODE
   laCodeDesc[2,1] = ORDHDR.SHIPVIA
   =gfCodDes(@laCodeDesc)
   *C101829,1 HBG 07/13/2000
   
   
   *** GET ORDER HEADER
   IF NEWPIK
      XPIKTKT  = PIKTKT
      XORDER   = ORDER
      *C101829,1 HBG 07/19/2000  save the value of the store [Begin]
      XSTORE   = STORE
      *C101829,1 HBG 07/19/2000  save the value of the store [End  ]
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

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....13.......+.......20..........+.........
*PIKTKT ORDER#  WAREHOUSE  P  SE DI    ACCT# NAME......     STORE#   PO#        START    COMPLETE   CUSTOMER TERMS      SHIPVIA                        
*123456 123456  123456     12 12 12345 12345 12345678901234 12345678 1234567890 MM/DD/YY MM/DD/YY   123456              Descreption
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....13.......+.......20..........+.........

   ROW=ROW+1
   IF llMultiWH
     *B603225,1 BWA 10/26/1999 Fix the bug of the fields repeted for all the piktkt# [START]   
     *@ ROW,00 SAY 'PIKTKT ORDER#  WAREHOUSE  P  SEASON DIVI.    ACCT# NAME......     STORE#   PO#       START     COMPLETE'   
     *C101829,1 HBG 07/13/2000 Add the header of the 2 added fields in case of there is a multi ware house[Begin]
     *@ ROW,00 SAY 'PIKTKT ORDER#  WAREHOUSE  P  SEASON DIVI.    ACCT# NAME......     STORE#   PO#        START     COMPLETE'
     @ ROW,00 SAY 'PIKTKT ORDER#  WAREHOUSE  P  SEASON DIVI.    ACCT# NAME......     STORE#   PO#           START    COMPLETE    CUSTOMER TERMS      SHIPVIA'
     *C101829,1 HBG 07/13/2000 Add the header of the 2 added fields in case of there is a multi ware house[END  ]
     *B603225,1 BWA 10/26/1999 [END]
   ELSE
     *C101829,1 HBG 07/13/2000 Add the header of the 2 added fields in case of there is no multi ware house[Begin]
     *@ ROW,00 SAY 'PIKTKT ORDER#  P  SEASON DIVI.    ACCT# NAME......     STORE#   PO#       START     COMPLETE'     
     @ ROW,00 SAY 'PIKTKT ORDER#  P  SEASON DIVI.    ACCT# NAME......     STORE#   PO#          START     COMPLETE   CUSTOMER TERMS          SHIPVIA'
     *C101829,1 HBG 07/13/2000 Add the header of the 2 added fields in case of there is no multi ware house[END  ]
   ENDIF
   ROW=ROW+1

   @ ROW,00 SAY PIKTKT
   @ ROW,07 SAY ORDER

   IF llMultiWH
     @ ROW,15 SAY lcWare
   ENDIF
   
   *E301249,1 Make priority field C(3) [start]
   *@ ROW,IIF(llMultiWH,26,15) SAY ORDHDR.PRIORITY
   @ ROW,IIF(llMultiWH,26,15)-1 SAY ALLTRIM(ORDHDR.PRIORITY)
   *E301249,1 [end]
   
   @ ROW,IIF(llMultiWH,29,18) SAY ORDHDR.SEASON
   @ ROW,IIF(llMultiWH,36,25) SAY ORDHDR.CDIVISION
   @ ROW,IIF(llMultiWH,45,34) SAY ACCOUNT
   @ ROW,IIF(llMultiWH,51,40) SAY LEFT(CUSTOMER.BTNAME,14)
   *B603225,1 BWA 10/26/1999 Fix the bug of the fields repeted for all the piktkt# [START]
   *@ ROW,IIF(llMultiWH,65,54) SAY STORE
   @ ROW,IIF(llMultiWH,66,54) SAY STORE   
   @ ROW,IIF(llMultiWH,75,64) SAY IIF(ORDHDR.MULTIPO,LEFT(CUSTPO,10),LEFT(ORDHDR.CUSTPO,10))
   
   *@ ROW,IIF(llMultiWH,88,77) SAY ORDHDR.START
   *@ ROW,IIF(llMultiWH,98,87) SAY ORDHDR.COMPLETE
   @ ROW,IIF(llMultiWH,89,77) SAY ORDHDR.START
   @ ROW,IIF(llMultiWH,98,87) SAY ORDHDR.COMPLETE
   
   *C101829,1 HBG 07/13/2000 Add the 2 fields of Customer terms and Shipvia [BEGIN]
   *@ ROW,IIF(llMultiWH,120,98) SAY ORDHDR.CTERMCODE
   @ ROW,IIF(llMultiWH,110,98) SAY PADR(laCodeDesc[1,3],IIF(llMultiWH,19,24))
   
   *lcShipVia =gfCodDes(ORDHDR.SHIPVIA, "SHIPVIA") 
   *@ ROW,IIF(llMultiWH,136,114) SAY ALLTRIM(lcShipVia)
   @ ROW,IIF(llMultiWH,130,119) SAY PADR(laCodeDesc[2,3],IIF(llMultiWH,10,23))
   *C101829,1 HBG 07/13/2000 Add the 2 fields of Customer terms and Shipvia [END  ]
   
   *B603225,1 BWA 10/26/1999 [END]
   ROW = ROW+2

   *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9..
   * GP STYLE        COLOR  STYLE DESCRIPTION... COLOR DESCR....   PRICE  ORDER   PICK  TOT-VALUE
   *  ± ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±± ±±±±.±± ±±±±±± ±±±±±± ±±±±±±±.±±
   *  X 123456789012 123456 12345678901234567890 123456789*12345 1234.99 123456 123456 1234567.99
   *  ± ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±± ±±±±.±± ±±±±±± ±±±±±± ±±±±±±±.±±
   *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9..
   
   @ ROW,01 SAY 'GP '+PADR(lcStyTitle,20)+'STYLE DESCRIPTION                      PRICE  ORDER   PICK  TOT-VALUE'
   ROW = ROW + 1
   *C101829,1 HBG 07/13/2000 Add the a header for the sizes [BEGIN]
   @ ROW,07 SAY "SZ1    SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8"
   ROW = ROW + 1
   *C101829,1 HBG 07/13/2000 Add the a header for the sizes [END  ]
   *---------------------
   * LOOP ORDER LINES
   *---------------------
   *B803319,1  loop for order# if there's no Piktkt AME[Start]
   *DO WHILE &lcOrdlTemp->PIKTKT = XPIKTKT
   *B101829, HBG 19/07/2000 Add store to the condition of while loop [Begin]
   *DO WHILE IIF("******" $ &lcOrdlTemp..PIKTKT ,&lcOrdlTemp..Order = XOrder,&lcOrdlTemp->PIKTKT = XPIKTKT)
   DO WHILE IIF("******" $ &lcOrdlTemp..PIKTKT ,&lcOrdlTemp..Order+&lcOrdlTemp..Store = XOrder + XSTORE,&lcOrdlTemp->PIKTKT = XPIKTKT)
   *B101829, HBG 19/07/2000 Add store to the condition of while loop [End  ]
   *B803319,1  AME[End]
     IF TOTPIK<=0
         SKIP
        LOOP
     ENDIF
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
   *B803319,1  if there is no piktkt Print totals & update Variables when
   *B803319,1  xorder is not equal to order# in Temp File AME[Start]
   *IF &lcOrdlTemp->PIKTKT <> XPIKTKT
   *B101829, HBG 19/07/2000 Add store to the condition [Begin]
   *IF (("******" $ &lcOrdlTemp..PIKTKT) AND (&lcOrdlTemp..Order <> XOrder)) OR ;
      (&lcOrdlTemp..PIKTKT <> XPIKTKT)
   IF (("******" $ &lcOrdlTemp..PIKTKT) AND (&lcOrdlTemp..Order+&lcOrdlTemp..store <> XOrder+XSTORE)) OR ;
      (&lcOrdlTemp..PIKTKT <> XPIKTKT)      
   *B101829, HBG 19/07/2000 Add store to the condition [End  ]
   *B803319,1  AME[END]

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
*! Date      : 03/30/1999
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
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 03/30/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lnDatapos = ASCAN(laOGFxFlt,'ORDLINE.PIKDATE')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate)) + '|' + ALLTRIM(DTOC(gdSysDate))     
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF

*-- R_WIDTH  Variable that hold the report width to the print
R_WIDTH = 'W'

