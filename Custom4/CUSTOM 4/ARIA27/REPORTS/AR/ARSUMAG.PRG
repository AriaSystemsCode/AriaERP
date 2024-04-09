*:************************************************************************
*: Program file  : ARSUMAG.Prg
*: Program desc. : SUMMARY AGING REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AR
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 01/04/99
*: Reference     : *E301113,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARSUMAG
*:************************************************************************
*: Modifications:
*B802067,1 AMM 04/01/99 Adjust the report filter
*E301429,1 NAD 06/18/2000  Increase the amount field in some screens and reports.
*:************************************************************************
*E301429,1(Start) variable to hold the picture

lcDecPic=IIF(llRpDec,'99999999.99','99999999999')
*E301429,1(End)
ROW     = 99
PAGENO  = 0
R_WIDTH ='W'
R_TITLE = 'SUMMARY AGING REPORT'
DIMENSION XTOT(7)
STORE 0.00 TO XTOT
XAGINGTYPE  = gfGetMemvar('XAGINGTYPE',gcAct_Comp)
lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format

lcRpExp     = lcRpExp + " .AND. TYPE = 'M' .AND. !EMPTY(NETBAL) "

*B802067,1 AMM Adjust the filter
*DO CASE
  *CASE lnRpPrd = 1
    *lcRpExp = lcRpExp + " .AND. " + IIF( XAGINGTYPE = 'D' ,;
          " AGE30 > 0 .OR. AGE60 > 0 .OR. AGE90 > 0 .OR. AGE120 > 0 " ,;
          " TERAGE30 > 0 .OR. TERAGE60>0 .OR. TERAGE90>0 .OR. TERAGE120>0 " )
  *CASE lnRpPrd = 2
    *lcRpExp = lcRpExp + " .AND. " + IIF( XAGINGTYPE = 'D' ,;
          " AGE60 > 0 .OR. AGE90 > 0 .OR. AGE120 > 0 ",;
          "  TERAGE60 > 0 .OR. TERAGE90 > 0 .OR. TERAGE120 > 0 " )
  *CASE lnRpPrd = 3
    *lcRpExp = lcRpExp + " .AND. " + IIF( XAGINGTYPE = 'D' ,;
          " AGE90 > 0 .OR. AGE120 > 0",;
          " TERAGE90 > 0 .OR. TERAGE120 > 0 ")
  *CASE lnRpPrd = 4
    *lcRpExp = lcRpExp + " .AND. "+IIF( XAGINGTYPE = 'D' ,;
          " AGE120 > 0 ", "  TERAGE120 > 0 ")
*ENDCASE
DO CASE
  CASE lnRpPrd = 1
    lcRpExp = lcRpExp + " .AND. " + IIF( XAGINGTYPE = 'D' ,;
          " (AGE30 > 0 .OR. AGE60 > 0 .OR. AGE90 > 0 .OR. AGE120 > 0 )" ,;
          " (TERAGE30 > 0 .OR. TERAGE60>0 .OR. TERAGE90>0 .OR. TERAGE120>0 )" )
  CASE lnRpPrd = 2
    lcRpExp = lcRpExp + " .AND. " + IIF( XAGINGTYPE = 'D' ,;
          " (AGE60 > 0 .OR. AGE90 > 0 .OR. AGE120 > 0 )",;
          " (TERAGE60 > 0 .OR. TERAGE90 > 0 .OR. TERAGE120 > 0 )" )
  CASE lnRpPrd = 3
    lcRpExp = lcRpExp + " .AND. " + IIF( XAGINGTYPE = 'D' ,;
          " (AGE90 > 0 .OR. AGE120 > 0) ",;
          " (TERAGE90 > 0 .OR. TERAGE120 > 0) ")
  CASE lnRpPrd = 4
    lcRpExp = lcRpExp + " .AND. "+IIF( XAGINGTYPE = 'D' ,;
          " AGE120 > 0 ", "  TERAGE120 > 0 ")
ENDCASE
*B802067,1 AMM end
  
SELE CUSTOMER
LOCATE ALL FOR &lcRpExp
IF !FOUND()
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SET DEVICE TO PRINT
SCAN FOR &lcRpExp
  IF ROW >55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'ARSUMAG','',R_WIDTH

    DO CASE
      CASE XAGINGTYPE = 'D' .AND. !llRpTotal
        *E301429,1 (Start) Increase the amount field in some screens and reports.
        @ 05,01 SAY 'ACCT# NAME................ TEL.......         BALANCE      CURRENT     OVER-30     OVER-60     OVER-90    OVER-120      OPENCR'  &&MFM 02/05/94 MOVED
        *E301429,1(End)
      CASE XAGINGTYPE <> 'D' .AND. !llRpTotal
        *E301429,1 (Start) Increase the amount field in some screens and reports.
        @ 05,01 SAY 'ACCT# NAME................ TEL.......         BALANCE      CURRENT     (+1-30)    (+31-60)    (+61-90)     OVER-91      OPENCR'  &&MFM 02/05/94 MOVED
        *E301429,1 (End)
      CASE XAGINGTYPE = 'D' .AND. llRpTotal
        *E301429,1 (Start) Increase the amount field in some screens and reports.
        @ 05,01 SAY '                                              BALANCE      CURRENT     OVER-30     OVER-60     OVER-90    OVER-120      OPENCR'  &&MFM 02/05/94 MOVED
        
        *E301429,1 (End)     
      CASE XAGINGTYPE <> 'D' .AND. llRpTotal
        *E301429,1 (Start) Increase the amount field in some screens and reports.
        @ 05,01 SAY 'ACCT# NAME................ TEL.......         BALANCE      CURRENT     (+1-30)    (+31-60)    (+61-90)     OVER-91      OPENCR'  &&MFM 02/05/94 MOVED
         
        *E301429,1 (End) 
    ENDCASE

    ROW = 7
  ENDIF
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3...
*ACCT# NAME................ TEL.......        BALANCE    CURRENT    OVER-30    OVER-60    OVER-90    OVER-120      OPENCR
*ACCT# NAME................ TEL.......        BALANCE    CURRENT    (+1-30)   (+31-60)   (+61-90)     OVER-91      OPENCR
*                                             BALANCE    CURRENT    OVER-30    OVER-60    OVER-90    OVER-120      OPENCR   && MFM 01/17/94
*                                             BALANCE    CURRENT    (+1-30)   (+31-60)   (+61-90)     OVER-91      OPENCR   && MFM 01/17/94
*12345 12345678901234567890 1234567890      1234567.99 1234567.99 1234567.99 1234567.99 1234567.99 12345678.99 12345678.99
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

  IF !llRpTotal
    @ ROW,001 SAY ACCOUNT
    @ ROW,007 SAY SUBSTR(BTNAME,1,15)
    @ ROW,024 SAY IIF( EMPTY(PHONE2), PHONE1 , PHONE2 ) PICTURE lcPhonPict SIZE 1,16
    *E301429,1 (Start) Increase the amount field in some screens and reports.
    *@ ROW,043 SAY NETBAL   PICTURE '99999999.99'
    *@ ROW,056 SAY IIF(XAGINGTYPE='D' , CURRENT , TERCURRENT ) PICTURE '99999999.99'
    *@ ROW,068 SAY IIF(XAGINGTYPE='D' , AGE30   , TERAGE30   ) PICTURE '99999999.99' 
    @ ROW,043 SAY NETBAL   PICTURE lcDecPic
    @ ROW,056 SAY IIF(XAGINGTYPE='D' , CURRENT , TERCURRENT ) PICTURE lcDecPic
    @ ROW,068 SAY IIF(XAGINGTYPE='D' , AGE30   , TERAGE30   ) PICTURE lcDecPic
    
    
    *E301429,1 (End)
    IF lnRpPrd >= 2 .OR. lnRpPrd = 0 
      *E301429,1 (Start) Increase the amount field in some screens and reports.   
      *@ ROW,080 SAY IIF(XAGINGTYPE='D' , AGE60   , TERAGE60   ) PICTURE '99999999.99'
      @ ROW,080 SAY IIF(XAGINGTYPE='D' , AGE60   , TERAGE60   ) PICTURE lcDecPic
      *E301429,1 (End)
    ENDIF
    IF lnRpPrd >= 3 .OR. lnRpPrd = 0
      *E301429,1 (Start) Increase the amount field in some screens and reports.   
      *@ ROW,092 SAY IIF(XAGINGTYPE='D' , AGE90   , TERAGE90   ) PICTURE '99999999.99'
      @ ROW,092 SAY IIF(XAGINGTYPE='D' , AGE90   , TERAGE90   ) PICTURE lcDecPic
      *E301429,1 (End)  
    ENDIF
    IF INLIST(lnRpPrd,0,4)
      *E301429,1 (End) Increase the amount field in some screens and reports.   
      *@ ROW,104 SAY IIF(XAGINGTYPE='D' , AGE120  , TERAGE120  ) PICTURE '99999999.99'
      @ ROW,104 SAY IIF(XAGINGTYPE='D' , AGE120  , TERAGE120  ) PICTURE lcDecPic
      *E301429,1 (End)
    ENDIF
    *E301429,1 (Start) Increase the amount field in some screens and reports.   
    * @ ROW,116 SAY OPENCR PICTURE '99999999.99'
    @ ROW,116 SAY OPENCR PICTURE lcDecPic 
    *E301429,1 (End)  
    ROW = ROW + 1

  ENDIF            && MFM 01/17/94

  XTOT(1) = XTOT(1) + NETBAL
  XTOT(2) = XTOT(2) + IIF(XAGINGTYPE='D' , CURRENT , TERCURRENT )
  XTOT(3) = XTOT(3) + IIF(XAGINGTYPE='D' , AGE30   , TERAGE30   )
  XTOT(4) = XTOT(4) + IIF(XAGINGTYPE='D' , AGE60   , TERAGE60   )
  XTOT(5) = XTOT(5) + IIF(XAGINGTYPE='D' , AGE90   , TERAGE90   )
  XTOT(6) = XTOT(6) + IIF(XAGINGTYPE='D' , AGE120  , TERAGE120  )
  XTOT(7) = XTOT(7) + OPENCR

ENDSCAN

ROW = ROW + 1
@ ROW,00 SAY REPLICATE ('*',132)
ROW = ROW + 1
@ ROW,01 SAY '*** GRAND TOTAL ***'
*E301429,1 (Start) Increase the amount field in some screens and reports. 
*@ ROW,043 SAY XTOT(1)     PICTURE '99999999.99'
*@ ROW,056 SAY XTOT(2)     PICTURE '99999999.99'
*@ ROW,068 SAY XTOT(3)     PICTURE '99999999.99'
@ ROW,043 SAY XTOT(1)     PICTURE lcDecPic
@ ROW,056 SAY XTOT(2)     PICTURE lcDecPic
@ ROW,068 SAY XTOT(3)     PICTURE lcDecPic



*E301429,1 (End)  
IF lnRpPrd >=2 .OR. lnRpPrd = 0
  *E301429,1 (Start) Increase the amount field in some screens and reports. 
  *@ ROW,080 SAY XTOT(4)     PICTURE '99999999.99'
  @ ROW,080 SAY XTOT(4)     PICTURE lcDecPic
 
  *E301429,1 (End) Increase the amount field in some screens and reports. 
ENDIF
IF lnRpPrd >=3 .OR. lnRpPrd = 0
  *E301429,1 (Start) Increase the amount field in some screens and reports. 
  * @ ROW,092 SAY XTOT(5)     PICTURE '99999999.99'
  @ ROW,092 SAY XTOT(5)     PICTURE lcDecPic
 
  *E301429,1 (Start)
ENDIF
IF lnRpPrd >= 4 .OR. lnRpPrd = 0
  *E301429,1 (Start) Increase the amount field in some screens and reports. 
  * @ ROW,104 SAY XTOT(6)     PICTURE '99999999.99'
  @ ROW,104 SAY XTOT(6)     PICTURE lcDecPic
  
  *E301429,1 (End)  
ENDIF
*E301429,1 (Start) Increase the amount field in some screens and reports. 
*@ ROW,116 SAY XTOT(7)     PICTURE '99999999.99'
 @ ROW,116 SAY XTOT(7)     PICTURE lcDecPic

*E301429,1 (End)  
ROW = ROW + 1

@ ROW,00 SAY REPLICATE ('*',132)

DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN
