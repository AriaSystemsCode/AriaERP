*:***************************************************************************
*: Program file  : RMRAREP
*: Program desc. : RETURN AUTHORIZATION REPORT FOR NIN24
*! Date          : 03/06/2007
*: System        : Aria Advantage Series.
*: Module        : RETURNS MANAGEMENT (RM)
*: Tracking #    : C200758
*: Ticket #      : T20070215.0014 
*: Developer     : TAREK MOHAMMED IBRAHIM
*  This program is a copy from the standard rmrarep.prg made originally by ( E#301177 ) with the following changes made for NIN24
*     add 2 sorts for it 
*                one by "FROM" State ( add also a filter )
*                one by entered Date 
*:***************************************************************************
*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint
llPrint    = .F.

     *** LOOP FOR THE REPORT CRITERIA ***
XSORT_BY   = lcRpSort
XTITLE     = lcRpOpTi

*- Add status to the filter exprission 
lcRpStatus = IIF(EMPTY(lcRpStatus),'OCXBE',lcRpStatus)
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),' RETAUTH.STATUS$lcRpStatus', '.AND. RETAUTH.STATUS$lcRpStatus')

IF !EMPTY(lcRpStates)
  lcRpExp = lcRpExp + IIF(!EMPTY(lcRpExp), '.AND.' , '' ) + 'PADR(ALLT(CUSTOMER.CADDRESS4),6)$lcRpStates'
ENDIF

XFILTER    = lcRpExp 

*-- lcRaTemp  Variable that hold the temporary file name
lcRaTemp = gfTempName()

*-- llMultiWH  Variable that hold the if there is multiwa or not
llMultiWH  = (ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y')

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name

R_WIDTH    =  'W'
R_TITLE    =  'RETURN AUTHORIZATION REPORT'
XREPORT    =  'RmRaRep'

*---- SORT TO  lcRaTemp FILE	  
DO CASE

*--SUMMARY BY ACCOUNT
  CASE XSORT_BY = 'A'
    lcSortBy    = 'ACCOUNT+RANO'
    
*--SUMMARY BY R\A
  CASE XSORT_BY = 'R'
    lcSortBy    = 'RANO'                   
    
*--SUMMARY BY WAREHOUSE
  CASE XSORT_BY = 'W'
    lcSortBy    = 'CWARECODE+RANO'

*--SUMMARY BY REASON CODE
  CASE XSORT_BY = 'C'
    lcSortBy    = 'REASON+RANO'  

*--SUMMARY BY STATE CODE
  CASE XSORT_BY = 'S'
    lcSortBy    = 'STATE+RANO'  

*--SUMMARY BY ENTERED DATE
  CASE XSORT_BY = 'D'
    lcSortBy    = 'DTOS(RADATE)+RANO'  

ENDCASE

*---THE MAIN PROGRAM    
DO lpCollData

*---- SORT TO  THE VALUE OF lcRaTemp FILE	  
XBREAK     = ''
XBREAK_FLD = '' 

DO CASE

*--SUMMARY BY ACCOUNT
  CASE XSORT_BY = 'A'
    XBREAK      = "ACCOUNT"
    XBREAK_FLD  =  ACCOUNT
    
*--SUMMARY BY R\A
  CASE XSORT_BY = 'R'
    XBREAK      = "  "
    XBREAK_FLD  = "  "
    
*--SUMMARY BY WAREHOUSE
  CASE XSORT_BY = 'W'
    XBREAK      = "CWARECODE"
    XBREAK_FLD  =  CWARECODE

*--SUMMARY BY REASON CODE
  CASE XSORT_BY = 'C'
    XBREAK      = "REASON"
    XBREAK_FLD  =  REASON

*--SUMMARY BY STATE CODE
  CASE XSORT_BY = 'S'
    XBREAK      = "STATE"
    XBREAK_FLD  =  STATE
    
*--SUMMARY BY ENTERED DATE
  CASE XSORT_BY = 'D'
    XBREAK      = "RADATE"
    XBREAK_FLD  =  RADATE

ENDCASE


*---- TO PRINT THE REPORT     
*---- SORT TO  lcRaTemp FILE	

IF llPrint
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF 

IF USED(lcRaTemp)
  SELECT (lcRaTemp)
  SET RELATION TO
  USE 
ENDIF

*-- ERASE THE lcWorkFile
ERASE (gcWorkDir+lcRaTemp+'.DBF')  
ERASE (gcWorkDir+lcRaTemp+'.CDX')  

*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT 
*! Date      : 03/22/1999
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

SELE RETAUTH
SET FILTER TO 
SET RELATION TO 
SET RELATION TO IIF( EMPTY(STORE) , 'M'+ACCOUNT , 'S'+ACCOUNT+STORE ) ;  
    INTO CUSTOMER
SET RELATION TO CWARECODE INTO WAREHOUS ADDITIVE

LOCATE ALL FOR &XFILTER

IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ELSE
  WAIT 'Selecting records for report ...' WINDOW NOWAIT
  llPrint  = .T.
  =lfFillTemp()
  SELE RETAUTH
  SET RELATION TO 
  
  SELECT (lcRaTemp)
  IF !EMPTY(lcSortBy)
    WAIT 'Sorting  transactions by reason code...!'WINDOW NOWAIT
    INDEX ON &lcSortBy TAG &lcRaTemp
    SET RELATION TO IIF( EMPTY(STORE) , 'M'+ACCOUNT , 'S'+ACCOUNT+STORE ) ;  
        INTO CUSTOMER
    SET RELATION TO CWARECODE INTO WAREHOUS ADDITIVE    
    GOTO TOP
    WAIT CLEAR
  ENDIF   
ENDIF
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT 
*! Date      : 03/22/1999
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

DIMENSION XTOT(2,4)
STORE 0 TO XTOT
ROW      =  99
PAGENO   =  0


SELECT (lcRaTemp)

lnStrtRow = IIF(llMultiWH,7,0)

*! The old layout.
*! ....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9
*! RANO    ACCT#    NAME................ RADATE.. INVOICE CUSTPO.... AUTH/PCS      AUTH/AMT
*! STATUS  STORE... REASON.............. VOID.... ORDER.. TRAN...... RETR/PCS      RETR/AMT RETDATE
*! ±±±±±±  ±±±±±    ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±±  ±±±±±±±±±±  ±±±±±±±    ±±±±±±±±±±
*! ±±±±±±  ±±±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±±  ±±±±±±      ±±±±±±±    ±±±±±±±±±± ±±/±±/±±

*! NOTE1:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*! NOTE2:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*! NOTE3:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*! NOTE4:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±


*!0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*! RANO   ACCT#    NAME................ RADATE.. INVOIC CUSTPO.... WAREHS AUTH/PCS   AUTH/AMT
*! STATUS STORE... REASON.............. VOID.... ORDER. TRAN......        RETR/PCS   RETR/AMT RETDATE
*!0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*! ±±±±±± ±±±±±    ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±± ±±±±±±±±±± ±±±±±±  ±±±±±±± ±±±±±±±.±±
*! ±±±±±± ±±±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±± ±±±±±±     ±±±±±±  ±±±±±±± ±±±±±±±.±± ±±/±±/±±



*!0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*! RANO   ACCT#    NAME................ RADATE.. INVOIC CUSTPO.... AUTH/PCS   AUTH/AMT
*! STATUS STORE... REASON.............. VOID.... ORDER. TRAN...... RETR/PCS   RETR/AMT RETDATE
*!0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*! ±±±±±± ±±±±±    ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±± ±±±±±±±±±±  ±±±±±±± ±±±±±±±.±±
*! ±±±±±± ±±±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±± ±±±±±±      ±±±±±±± ±±±±±±±.±± ±±/±±/±±

*! NOTE1:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*! NOTE2:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*! NOTE3:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*! NOTE4:±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*! ...................................................................................................


SCAN WHILE .T.
  IF ROW > 53
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW = 5

    @ ROW+0,01 SAY 'RANO   ACCT#    NAME................ RADATE.. INVOIC CUSTPO....'+IIF(llMultiWH,' WAREHS','')+' AUTH/PCS          AUTH/AMT'
    @ ROW+1,01 SAY 'STATUS STORE... REASON.............. VOID.... ORDER. TRAN......'+IIF(llMultiWH,SPACE(07),'')+' RETR/PCS          RETR/AMT           RETDATE'
    ROW = ROW + 3       

  ENDIF

  IF !EMPTY(XBREAK)
    IF &XBREAK <> XBREAK_FLD
      @ ROW,00 SAY REPLICATE('-',130)
      ROW = ROW + 1
      @ ROW,01 SAY XBREAK_FLD
      @ ROW,15 SAY 'SUB-TOTAL=>'
      @ ROW,29 SAY 'AUTH PCS/AMT->'
      @ ROW,45 SAY XTOT(1,1) PICTURE '999999'
      @ ROW,55 SAY XTOT(1,2) PICTURE '99999999999.99' 
      @ ROW,70 SAY 'RETR PCS/AMT->'
      @ ROW,80 SAY XTOT(1,3) PICTURE '999999'

      @ ROW,90 SAY XTOT(1,4) PICTURE '99999999999.99' 
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',130)
      ROW = ROW + 1
      XBREAK_FLD = &XBREAK
      STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
    ENDIF
  ENDIF

  @ ROW,01 SAY RANO
  @ ROW,08 SAY ACCOUNT
  @ ROW,17 SAY LEFT(CUSTOMER->STNAME,20)
  @ ROW,38 SAY RADATE
  @ ROW,47 SAY INVOICE
  @ ROW,54 SAY LEFT(CUSTPO,10)
  IF llMultiWH
    @ ROW,65 SAY cWareCode
  ENDIF
  @ ROW,66+lnStrtRow SAY AUTH     PICTURE '9999999'
  @ ROW,77+lnStrtRow SAY AUTHAMT  PICTURE '99999999999.99' 

  ROW = ROW + 1
  DO CASE
    CASE STATUS = 'O'
      @ ROW,01 SAY 'OPEN'
    CASE STATUS = 'C'
      @ ROW,01 SAY 'COMPLT'
    CASE STATUS = 'X'
      @ ROW,01 SAY 'CANCLD'
    CASE STATUS = 'B'
      @ ROW,01 SAY 'BID'
    CASE STATUS = 'E'
      @ ROW,01 SAY 'ELECT.'
  
  ENDCASE

  
  @ ROW,08 SAY STORE
  @ ROW,17 SAY LEFT(gfCodDes(REASON,'REASON'),15)
  @ ROW,38 SAY VOID
  @ ROW,47 SAY ORDER
  @ ROW,54 SAY TRAN
  @ ROW,66+lnStrtRow SAY RETURN    PICTURE '9999999'
  @ ROW,77+lnStrtRow SAY RETURNAMT PICTURE '99999999999.99'
  @ ROW,102+lnStrtRow SAY RETDATE
  ROW = ROW + 1

  IF !EMPTY(cRetNote1)
    @ ROW,01 SAY 'NOTES:'
  ENDIF

  FOR X = 1 TO 4
    Z = STR(X,1)
    IF !EMPTY(cRetNote&Z )
      @ ROW,07 SAY cRetNote&Z
      ROW = ROW + 1
    ENDIF
  ENDFOR

  @ ROW,00 SAY REPLICATE('.',130)
  ROW = ROW + 1

  *** UPDATE THE SUBTOTALS
  XTOT(1,1) = XTOT(1,1) + AUTH
  XTOT(1,2) = XTOT(1,2) + AUTHAMT
  XTOT(1,3) = XTOT(1,3) + RETURN
  XTOT(1,4) = XTOT(1,4) + RETURNAMT

  *** UPDATE THE GRAND TOTALS
  XTOT(2,1) = XTOT(2,1) + AUTH
  XTOT(2,2) = XTOT(2,2) + AUTHAMT
  XTOT(2,3) = XTOT(2,3) + RETURN
  XTOT(2,4) = XTOT(2,4) + RETURNAMT

ENDSCAN

IF !EMPTY(XBREAK)
  IF &XBREAK <> XBREAK_FLD
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    @ ROW,01 SAY XBREAK_FLD
    @ ROW,15 SAY 'SUB-TOTAL=>'
    @ ROW,29 SAY 'AUTH PCS/AMT->'
    @ ROW,45 SAY XTOT(1,1) PICTURE '999999'
    @ ROW,55 SAY XTOT(1,2) PICTURE '99999999999.99' 
    @ ROW,70 SAY 'RETR PCS/AMT->'
    
    @ ROW,80 SAY XTOT(1,3) PICTURE '999999'
    @ ROW,90 SAY XTOT(1,4) PICTURE '99999999999.99' 
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    XBREAK_FLD = &XBREAK
    STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
  ENDIF
ENDIF

@ ROW,00 SAY REPLICATE('=',130)
ROW = ROW + 1
@ ROW,10 SAY 'GRAND TOTAL=>'

@ ROW,29 SAY 'AUTH PCS/AMT->'
@ ROW,45 SAY XTOT(2,1) PICTURE '999999'
@ ROW,55 SAY XTOT(2,2) PICTURE '99999999999.99'
@ ROW,70 SAY 'RETR PCS/AMT->'

@ ROW,80 SAY XTOT(2,3) PICTURE '999999'
@ ROW,90 SAY XTOT(2,4) PICTURE '99999999999.99'
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',130)

*!*************************************************************
*! Name      : lfvWare
*! Developer : BASSEM RAFAAT 
*! Date      : 03/22/1999
*! Purpose   : Validate the warehous
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWare()
*!*************************************************************

FUNCTION lfvWare
PRIVATE lcWarFld,lcWarhous,lnSelFile,lcWareTag

lcWarFld   = VARREAD()
lcWareCode = EVAL(lcWarFld)

lnSelFile = SELECT(0)

SELECT WAREHOUS

lcWareTag = ORDER('WAREHOUS') 

SET ORDER TO TAG WAREHOUS IN WAREHOUS

IF !EMPTY(lcWareCode) .AND. ('?' $ lcWareCode .OR. !SEEK(lcWareCode , 'WAREHOUS'))
  lcWareCode =gfBrowWare(.T.)  
ENDIF

&lcWarFld = lcWareCode

SET ORDER TO lcWareTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvCus
*! Developer : BASSEM RAFAAT 
*! Date      : 03/22/1999
*! Purpose   : Validate Customer
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCus()
*!*************************************************************

FUNCTION lfvCus
PRIVATE lcCusFld,lcCustom,lnSelFile,lcCusTag

lcCusFld  = VARREAD()
lcCustom  = EVAL(lcCusFld)

lnSelFile =  SELECT(0)

SELECT CUSTOMER

lcCusTag  = ORDER('CUSTOMER')

SET ORDER TO TAG CUSTOMER IN CUSTOMER

IF !EMPTY(lcCustom) .AND. ( '?' $ lcCustom .OR. !SEEK((IIF( EMPTY(STORE) , 'M'+lcCustom , 'S'+lcCustom+STORE )) , 'CUSTOMER'))
  =CUSBROWM (@lcCustom)
ENDIF

&lcCusFld = lcCustom

SET ORDER TO lcCusTag

SELECT (lnSelFile)
******************************************************************************

*!*************************************************************
*! Name      : lfvloc
*! Developer : Sameh Al-Desouki
*! Date      : 02/28/2000
*! Purpose   : Validation function for the Location field
*!*************************************************************
*! Called from : Location field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvloc

PRIVATE lcObjName , lcObjVal , llObjRet
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Location he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*-- End of lfvloc.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry
*! Date      : 08/28/2001
*! Purpose   : Validate Reports
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*--Cheking of Modules instaled flags.
llCrIstall = (OCCURS('CR',gcCmpModules)<>0)

IF llCrIstall
  DIMENSION laSortItem[5], laSortVal[1]
  
  laSortItem[1]= 'Open'  
  laSortItem[2]= 'Complete' 
  laSortItem[3]= 'Cancel'
  laSortItem[4]= 'Electronic'
  laSortItem[5]= 'Bid'
  
ELSE
  DIMENSION laSortItem[4], laSortVal[1]
  
  laSortItem[1]= 'Open'  
  laSortItem[2]= 'Complete' 
  laSortItem[3]= 'Cancel'
  laSortItem[4]= 'Bid'
ENDIF           

*- fill the source array that holds states
IF lnOgSeting = 1
  DIMENSION laRpSStats[1],laRpTStats[1]

  lcRpStates = ''
  
  SELECT CCODE_NO + '-'+CDISCREP ;
    FROM CODES ;
    WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNSTATE  ' ;
    ORDER BY CCODE_NO ;
    INTO ARRAY laRpSStats
ENDIF

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 27/08/2001
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laSortItem,@laSortVal,'Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*--Cheking of Modules instaled flags.

llCrIstall = (OCCURS('CR',gcCmpModules)<>0)

IF !llCrIstall
  IF !EMPTY(laSortVal[1])
    FOR lnI = 1 TO ALEN(laSortVal,1)
      lcRpStatus = lcRpStatus +IIF(laSortVal[lnI] = 'Open','O',;
                                IIF(laSortVal[lnI] = 'Complete','C',;
                                IIF(laSortVal[lnI] = 'Cancel','X',;
                                IIF(laSortVal[lnI] = 'Bid','B',''))))
    ENDFOR  && end Loop to make Status expression.
  ENDIF
ELSE
  IF !EMPTY(laSortVal[1])
    FOR lnI = 1 TO ALEN(laSortVal,1)
      lcRpStatus = lcRpStatus +IIF(laSortVal[lnI] = 'Open','O',;
                                IIF(laSortVal[lnI] = 'Complete','C',;
                                IIF(laSortVal[lnI] = 'Cancel','X',;
                                IIF(laSortVal[lnI] = 'Electronic','E',;
                                IIF(laSortVal[lnI] = 'Bid','B','')))))
    ENDFOR  && end Loop to make Status expression.
  ENDIF

ENDIF  

*-- end of lfvOStatus.

*:**************************************************************************
*:* Name        : lfvState
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/06/2007
*:* Purpose     : Valid function for the selected states
*:***************************************************************************
FUNCTION lfvState

=gfMover(@laRpSStats,@laRpTStats,'States',.T.,'')  && call mover function.

lcRpStates = '|'
IF !EMPTY(laRpTStats[1])
  FOR lnI = 1 TO ALEN(laRpTStats,1)
    lcRpStates = lcRpStates + LEFT(laRpTStats[lnI],6) + '|'
  ENDFOR  
ENDIF
*-- end of lfvState.


*:**************************************************************************
*:* Name        : lfFillTemp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/06/2007
*:* Purpose     : Fill the temp file that the report will based on 
*:***************************************************************************
FUNCTION lfFillTemp
PRIVATE laStru
SELECT RETAUTH
=AFIELDS(laStru)
lnLen = ALEN(laStru,1)+1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'STATE'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

CREATE TABLE (gcWorkDir+lcRaTemp) FROM ARRAY laStru

SELECT RETAUTH
SCAN REST FOR EVAL(XFILTER)
  SCATTER MEMVAR
  m.STATE = CUSTOMER.CADDRESS4
  INSERT INTO &lcRaTemp FROM MEMVAR
ENDSCAN

*-- end of lfFillTemp.