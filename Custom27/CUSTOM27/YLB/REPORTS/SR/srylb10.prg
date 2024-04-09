*:***************************************************************************
*: Program file  : SRYLB10
*: Program desc. : Custom Sales Representative commission work sheet Report FOR YLB100
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Sales Representative (SR )
*: Developer     : RAMY MABROUK  (RAMY)
*: DATE          : NOVEMBER/1999
*:***************************************************************************
*: Calls : 
*:    Procedures : RPT_HDR,ENDREPORT
*:    Functions  : lfwRepWhen,lfwOldVal,lfvRepCode,lfvDate
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : - This is the same code as 2.6 (@x,y SAY) but after making some modifications.
*:         : - We have several transaction types we seek repcomm for it, like
*:             TranTypes      Description
*:                     1      INVOICE
*:                     2      PAYMENTS
*:                     3      DEBIT ADJUSTMENT
*:                     4      CREDIT ADJUSTMENT
*:                     5      RETURN  (CREDIT MEMO)
*:                     6      VOID INVOICE
*:                     7      VOID CREDIT MEMO
*:***************************************************************************
*: Example : DO SRREPSH
*:***************************************************************************
*: This Report Program is due to E300926 ...
*:E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.
*:Modifications : 
*:B802084,1 MAB 04/01/1999 Print return merchandise amount with negative sign.
*:B802084,1 SAMEH 04/01/99 Adding Credit file with Index Tag Credit in Report Generator
*:B603202,1 BWA 10/12/1999 Fix the bug of increase the cust po width to 15 characters .
*:C101675,1 RAMY 11/02/1999 Customized the report to print the City and state from the 
*:C101675,1                 bill to address
*:***************************************************************************

*-------- Assiging 2.6 variables with its eq. in 2.7 OG  [begin ] --------
*-- Eval. From and To dates. [begin] 
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
LDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,;
            ATC('|',laOGFxFlt[lnDatePos,6])-1))

HDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],;
            ATC('|',laOGFxFlt[lnDatePos,6])+1))
*-- Eval. From and To dates. [end] 

*-- Eval. sales rep. [begin] 
lnRepPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SALESREP.REPCODE'),1)
HSALESREP = laOGFxFlt[lnRepPos,6]
*-- Eval. sales rep. [end] 

XSTATUS   = IIF(lcRpStatus = 'B','OH',lcRpStatus)  && InvHdr Status.
*-------- Assiging 2.6 variables with its eq. in 2.7 OG  [end ] --------

DIMENSION XTOTAL(2,3)
XTOTAL=0

COMFILTER = 'AMOUNT<>0 .AND.BETWEEN(DATE,LDATE,HDATE).AND.;
             STATUS $ ALLTRIM(XSTATUS)'
lcFilter = IIF(!EMPTY(HSALESREP), 'REPCODE = HSALESREP', '.T.')
    
FROMDATE = DTOC(LDATE)
THRUDATE = DTOC(HDATE)
PERIOD   = 'PERIOD: &FROMDATE - &THRUDATE'

*------ Collecting data  [Begin ] --------
WAIT WINDOW 'Locating records in file....' NOWAIT
SELECT REPCOMM                       && ADDED BY RAA ON 03/03/1993
LOCATE FOR &COMFILTER AND &lcFilter
IF EOF('REPCOMM')
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display...! ')  
  SET DEVICE TO SCREEN
  lcLastExpr = lcRpExp
  RETURN
ELSE  

  *E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[Begin
  *IF llClearFn OR !(lcRpExp == lcLastExpr)
  IF llClearFn OR llOGFltCh
  *E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[End

    IF USED(RepTemp)
      USE IN (RepTemp)
    ENDIF
    COPY REST TO (gcWorkDir+RepTemp) FOR &ComFilter AND &lcFilter
    USE (gcWorkDir+RepTemp) IN 0  
    SELECT (RepTemp)
    llClearFn  = .F.
  ELSE 
    SELECT (RepTemp)
    GO TOP
  ENDIF 
ENDIF

*------ Collecting data  [End ] --------
*------------------- END REPORT SELECTION ---------------------

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*SRREPSH                       SALESREP COMMISSION WORKSHEET REPORT
*
*
*SALESREP: 123 12345678901234567890
*
* Data printed from commission file
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*MM/DD/YY 123456 123456789012345 12345 12345678 12345678 123456 1234567890 99.99  99.99   99.99  1234567.99 1234567.99 _______________
*  DATE   TRAN#  .....DESC...... ACCT# NAME     STORE#  ORDER# CUST PO    PCNT  TRD DSC MER DSC NET-SHIP   COMMISSION AMOUNT-TO-PAY
*
* Data printed from order file for advance payments
* BOOKED   ST    ORDER#   START  COMPLETE    ACCT# .....NAME.....  STORE#   PCNT  AMT-BOOKED COMMISSION    ADVANCE  AMOUNT-TO-PAY
*MM/DD/YY        123456 MM/DD/YY MM/DD/YY    12345 123456789012345 12345678 12.34 1234567.99 1234567.99 1234567.99  _______________
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
A='SRREPSH                                        SALESREP COMMISSION WORKSHEET REPORT'

*B603202,1 BWA 10/12/1999 Fix the bug of increase the cust po width to 15 characters. [START]
*B='  DATE     TRAN#  .....DESC...... ACCT# NAME....   STORE#   ORDER# CUST PO    PCNT TRD DSC MER DSC   NET-SHIP COMMISS. AMOUNT-TO-PAY'
*C101675 RAMY Add the city and the state to the header of the report [start]
*B='  DATE   TRAN#  .....DESC... ACCT# NAME....   STORE#   ORDER# CUST PO          PCNT TRD DSC MER DSC   NET-SHIP COMMISS. AMNT TO PAY'
B='  DATE   TRAN#  ....DESC... ACCT# NAME....   CITY      ST. STORE#  ORDER# CUST PO         PCNT TRD DSC MER DSC   NET-SHIP COMMISS.'
*C101675 RAMY [end]
*B603202,1 BWA 10/12/1999 [END]

* C=' BOOKED  ST      ORDER#   START  COMPLETE    ACCT# .....NAME.....  STORE#   PCNT  AMT-BOOKED COMMISSION     ADVANCE  AMOUNT-TO-PAY'

*-- Sort Temp. Sales comm file by user selected choice [begin]
CHOICE = lcRpSortBy
DO CASE
  CASE CHOICE='I'
    SORTFIELD = 'REPCODE+TRAN+STR(RECNO(),7)'
  CASE CHOICE='O'
    SORTFIELD = 'REPCODE+ORDER+TRAN+STR(RECNO(),7)'
  CASE CHOICE='C'
    SORTFIELD = 'REPCODE+CUSTPO+TRAN+STR(RECNO(),7)'
  CASE CHOICE='D'                    && ADDED BY RAA ON 03/03/1993
    SORTFIELD = 'REPCODE+DTOS(DATE)+TRAN+STR(RECNO(),7)'
ENDCASE

WAIT WINDOW 'Sorting ' + LTRIM(STR(RECCOUNT(),9)) + ' Commission records ...' NOWAIT
SELECT (RepTemp)
INDEX ON &SORTFIELD TAG &REPTEMP
*-- Sort Temp. Sales comm file by user selected choice [begin]

IF HSALESREP = ' '
  SELECT SALESREP
  GOTO TOP
  HSALESREP = CHR(254)
ENDIF

SELECT &RepTemp
IOCOM = SELECT()

*---------------------------------------------------------------
* [REPORT] PRINT COMMISSION WORKSHEET REPORT
*---------------------------------------------------------------
ROW    = 99
NEWREP = .T.
PAGENO = 0
SET DEVICE TO PRINT

DO WHILE .T.

   SELECT SALESREP
   XSALESREP = REPCODE

   IF EOF() .OR. REPCODE>HSALESREP
      EXIT
   ENDIF

   IF NEWREP
      
      IF (HSALESREP == XSALESREP) OR EMPTY(HSALESREP)
        WAIT WINDOW 'Sales commissiom for sales representative ' + XSALESREP NOWAIT
      ENDIF
      
      STORE .F. TO GOTCOM, GOTORD, PTOTAL1, PTOTAL2
      ROW     = 99
      XTOTAL  = 0.00
      CURFILE = '&REPTEMP'

      SELECT &REPTEMP
      SEEK XSALESREP
      GOTCOM = IIF(FOUND(), .T., .F.)
      IF .NOT.GOTCOM
         SELECT SALESREP
         SKIP
         LOOP
      ENDIF
   ENDIF

   NEWREP = .F.

   IF ROW >= 45
     R_TITLE ='SALESREP COMMISSION WORKSHEET'
     XTITLE  = PERIOD
     PAGENO  = PAGENO + 1
     DO RPT_HDR WITH 'SRREPSH',XTITLE,R_WIDTH
     ROW     = 5
     NEWPAGE = .T.
   ENDIF

   *-- [COMMISSION] LOOP ALL RECORDS ON TEMP. COMM FILE FOR THIS REP
   DO WHILE GOTCOM
     SELECT &REPTEMP
     IF NEWPAGE
       NEWPAGE = .F.
       @ ROW,01 SAY 'SALESREP: '+XSALESREP+'  '+SALESREP->NAME
       STORE 0.00 TO TSHIPAMT,TCOMMDUE,TBAKORD
       ROW = ROW+2
       @ ROW,00 SAY B
       ROW = ROW+2
    ENDIF

    SELECT &REPTEMP
    IF EOF() .OR. (REPCODE <> XSALESREP)
       GOTCOM  = .F.
       PTOTAL1 = .T.
       NEWPAGE = .T.
       EXIT
    ENDIF

    *-- TEST/PRINT HEADINGS
    IF ROW > 55
      EXIT
    ENDIF

    *----------------------------------------------------------------
    * If the current record on Salescom is a summary, and the user
    * has requested detail history, switch to Saleshst.dbf
    *----------------------------------------------------------------
    SELECT &RepTemp
    CURFILE = '&REPTEMP'
    XTERMSPCT = 00.00
    SELECT INVHDR
    SEEK &REPTEMP..Tran
    lnDisc = 00.00
    IF FOUND() AND &REPTEMP..TranType $ '16'

      lnDisc = DISCPCNT
      
      XTERMSPCT =InvHdr.Trde_disc
      
      IF &REPTEMP->TRANTYPE='6'  &&CASE OF VOID
        XNETSHIP = (INVHDR->VSHIPAMT + INVHDR->VDISCOUNT)* -1
      ELSE
        XNETSHIP = IIF(INVHDR->STATUS = 'V',INVHDR->VSHIPAMT+;
                 INVHDR->VDISCOUNT,INVHDR->SHIPAMT + INVHDR->DISCOUNT)
      ENDIF

      IF XTERMSPCT <>0
        XNETSHIP = XNETSHIP - (XNETSHIP * XTERMSPCT/100)
      ENDIF
    ELSE
      
      *** 1) Print the returns in a -ve sign and the void returns
      ***    in a +ve sign
      *** 2) Print the VAMOUNT field instead of AMOUNT field if
      ***    the status of the returns is 'V' (voided)

      *-- Check for Return Merchandise module. [MAB 10/17/1998 BEGIN]
      IF ('RM' $ gcCmpModules) AND SEEK(&REPTEMP->TRAN,'RETHDR')

        XNETSHIP = IIF(RETHDR.STATUS='V',RETHDR.VAMOUNT,RETHDR.AMOUNT)
        *B802084,1 if return merchandise print negative ship amount [Begin]
        IF SEEK(&REPTEMP..Account+&REPTEMP..TRAN,'Credit')
          XNETSHIP = XNETSHIP - Credit.Dsc_Amt
        ENDIF
        XNETSHIP = XNETSHIP * IIF(&REPTEMP..TRANTYPE='5',-1,1)
        *B802084,1 if return merchandise print negative ship amount [End  ]
      
      *-- Check for Return Merchandise module. [MAB 10/17/1998 END]

      ELSE
        *** TAK 10/21/93 ***
        XNETSHIP = IIF(&REPTEMP->COMMPCNT<>0,;
                      (&REPTEMP->AMOUNT/(&REPTEMP->COMMPCNT/100)),0)
        *** END TAK 10/21/93 

        lnAlias = ALIAS()
        SELECT (REPTEMP)
        FOR lnCount = 1 TO FCOUNT()
          IF FIELD(lnCount) = 'ORGNL_CADJ' AND ORGNL_CADJ<>0
            XNETSHIP = ORGNL_CADJ
            EXIT
          ENDIF  
        ENDFOR
        SELECT (lnAlias)
      ENDIF
    
    ENDIF

    SELECT CUSTOMER
    SEEK 'M'+&REPTEMP->ACCOUNT
    XNAME = IIF(FOUND(), SUBSTR(BTNAME,1,15), '')
    
    SELECT &REPTEMP
    

    @ ROW,00 SAY DATE 
    *B603202,1 BWA 10/12/1999 Fix the bug of increase the cust po width to 15 characters. [START]    
    *@ ROW,11 SAY TRAN
    *@ ROW,18 SAY SUBSTR(DESC,1,14)
    *@ ROW,34 SAY ACCOUNT
    *- TMI 06/08/94 Print only 10 char. of acct name and print lnDisc.
    *@ ROW,40 SAY SUBSTR(XNAME,1,10)
    *@ ROW,51 SAY STORE
    *@ ROW,60 SAY ORDER
    *@ ROW,67 SAY PADR(CUSTPO,10)
    *@ ROW,77 SAY COMMPCNT             PICTURE '@Z 99.99'
    *@ ROW,85 SAY XTERMSPCT            PICTURE '@Z 99.99'
    *- TMI 06/08/94.
    *@ ROW,92 SAY lnDisc               PICTURE '@ 99.99%'
    *@ ROW,99 SAY XNETSHIP             PICTURE '@Z 9999999.99'
    *@ ROW,110 SAY AMOUNT              PICTURE '@Z 99999.99'
    *@ ROW,119 SAY '$____________'
    
    *C101675,1 RAMY 11/02/99 [start] Add the city and state fron the bill to address 
    *C101675,1 RAMY          and changed the positions of the rest of the fields
    *@ ROW,09 SAY TRAN
    *@ ROW,16 SAY SUBSTR(DESC,1,13)
    *@ ROW,29 SAY ACCOUNT
    *@ ROW,35 SAY SUBSTR(XNAME,1,10)    
    *@ ROW,46 SAY STORE
    *@ ROW,55 SAY ORDER
    *@ ROW,62 SAY CUSTPO
    *@ ROW,78 SAY COMMPCNT             PICTURE '@Z 99.99'
    *@ ROW,86 SAY XTERMSPCT            PICTURE '@Z 99.99'
    *@ ROW,93 SAY lnDisc               PICTURE '@ 99.99%'
    *@ ROW,100 SAY XNETSHIP            PICTURE '@Z 9999999.99'
    *@ ROW,111 SAY AMOUNT              PICTURE '@Z 99999.99'
    *@ ROW,120 SAY '$___________'

    @ ROW,09 SAY TRAN
    @ ROW,16 SAY SUBSTR(DESC,1,12)
    @ ROW,28 SAY ACCOUNT
    @ ROW,34 SAY SUBSTR(XNAME,1,10)
    @ ROW,45 SAY SUBSTR(CUSTOMER.cAddress3,1,9)
    @ ROW,55 SAY SUBSTR(CUSTOMER.cAddress4,1,3)
    @ ROW,59 SAY STORE
    @ ROW,67 SAY ORDER
    @ ROW,74 SAY CUSTPO
    @ ROW,80 SAY COMMPCNT             PICTURE '@Z 99.99'
    @ ROW,97 SAY XTERMSPCT            PICTURE '@Z 99.99'
    @ ROW,104 SAY lnDisc               PICTURE '@ 99.99%'
    @ ROW,111 SAY XNETSHIP            PICTURE '@Z 9999999.99'
    @ ROW,122 SAY AMOUNT              PICTURE '@Z 99999.99'
    *C101675,1 RAMY 11/02/99 [End]
    
    XTOTAL(1,1) = XTOTAL(1,1) + XNETSHIP
    XTOTAL(1,3) = XTOTAL(1,3) + AMOUNT
    ROW=ROW+1

    SELECT &REPTEMP
    SKIP
  ENDDO

  IF PTOTAL1
    PTOTAL1 = .F.
    ROW = ROW+1
    @ ROW,020 SAY XSALESREP + ' TOTALS ------------------------------------------------->'
    
    *B603202,1 BWA 10/12/1999 Fix the bug of increase the cust po width to 15 characters. [START]
    *@ ROW,99 SAY XTOTAL(1,1)     PICTURE '9999999.99'
    *@ ROW,110 SAY XTOTAL(1,3)    PICTURE '999999.99'
    *@ ROW,119 SAY '$____________'
    *RAMY
    *@ ROW,100 SAY XTOTAL(1,1)    PICTURE '9999999.99'
    *@ ROW,110 SAY XTOTAL(1,3)    PICTURE '999999.99'
    *@ ROW,120 SAY '$___________'

    @ ROW,111 SAY XTOTAL(1,1)    PICTURE '9999999.99'
    @ ROW,121 SAY XTOTAL(1,3)    PICTURE '999999.99'

    *B603202,1 BWA 10/12/1999 [END]


    
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('-',131)
    ROW = ROW+2
  ENDIF
  *--------------------- END COMMISSION LOOP ---------------------
  
  IF .NOT. GOTCOM
    NEWREP = .T.
    SELECT SALESREP
    SKIP
  ENDIF
ENDDO
*------------------------ END REPORT LOOP -------------------

*-- E300926 Added By MAB insted of old function [begin]
DO ENDREPORT
*-- E300926 Added By MAB insted of old function [end]
SET DEVICE TO SCREEN
lcLastExpr = lcRpExp
RETURN
*------------------------
*   END SRREPSH.PRG
*------------------------

*------- Functions section ---------
*----------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH    = 'W'
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(DATE() - DAY(DATE()) - (DAY(DATE() - DAY(DATE()))-1))+;
                           '|'+DTOC(DATE() - DAY(DATE()))
ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvRepCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/98
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRepCode()
*!*************************************************************
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
  SELECT SALESREP
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvRepCode.

*!*************************************************************
*! Name      : lfvDate
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/98
*! Purpose   : Validate Entered date [i.e. Not empty]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Period Option]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDate()
*!*************************************************************
FUNCTION lfvDate
IF EMPTY(EVALUATE(SYS(18)))
  WAIT WINDOW 'You must fill period range...' NOWAIT
  lcCurrObj = SYS(18)
  &lcCurrObj = laOldVal
  SHOW GET lcCurrObj
  _CUROBJ = _CUROBJ
ENDIF
*-- end of lfvDate.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/14/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.  &&You erase temporary file.
*-- Close temp. opended files, if it used.

*-- Delete temporary work file.
IF USED(RepTemp)
 USE IN (RepTemp)
 ERASE (gcWorkDir+RepTemp+'.DBF')
 ERASE (gcWorkDir+RepTemp+'.CDX')
ENDIF
*-- end of lfClearRep.
