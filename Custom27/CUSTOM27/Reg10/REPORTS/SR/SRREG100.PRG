*:**************************************************************************
*: Program file  : SRREG100
*: Program desc. : CUSTOMIZED SALESREP COMMISSION STATEMENT REPORT. 
*: System        : Aria Advantage Series.
*: Module        : Sales Representative (SR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C102039
*:**************************************************************************
*: Procedures    : lpCreatFil , lpCollData , lpPrintRep , lpPrintHdr , 
*:                 lpHeader , lpPrSubTot , lpGenExp
*:
*: Functions     : lfwRepWhen , lfItmPos , lfGetAmt , lfvRepCode , 
*:                 lfwOldVal , lfvDate
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*:B804332,1 AAN 08/21/2001 Fix the bug that the date and sales Rep. ranges are not working.
*:*****************************************************************************

*-- If user changed selected Criteria , collect data again.
IF llOGFltCh
  IF !USED(lcWorkFile) OR (USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0)
    DO lpCreatFil
  ENDIF

  PRIVATE lcDivExp , lcDateExp
  STORE ".T." TO lcDivExp , lcDateExp
  *-- If user selected division or Date in OG.
  IF ALLTRIM(laOGFxFlt[lnDivPos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnDatePos,1]) $ lcRpExp
    DO lpGenExp    
  ENDIF

  DO lpCollData          && Procedure to collect data
ENDIF
*-- EndIf of user changed selected Criteria.

SELECT (lcWorkFile)
LOCATE
*-- If End of File encountered (no Records to Display)
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE
  SET DEVICE TO PRINT
  DO lpPrintRep
  SET DEVICE TO SCREEN
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : Report When Function
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnSlRepPos = lfItmPos('REPCOMM.REPCODE')  && get Rep code Fixed filter Position.
lnDatePos  = lfItmPos('REPCOMM.DATE')     && get Period Fixed filter Position.
lnDivPos   = lfItmPos('INVHDR.CDIVISION') && get Division Fixed filter Position.

*-- Fill period date with 1st & last day from last month
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(DATE() - DAY(DATE()) - (DAY(DATE() - DAY(DATE()))-1))+;
                           '|'+DTOC(DATE() - DAY(DATE()))
ENDIF
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil

*-- Creating a temporary cursor to hold the records that will be printed.
CREATE CURSOR (lcWorkFile);
              (cRepCode C(3), cRepName C(30), cDivision C(2) , cInvoice C(6),;
               dDate D, cAccount C(5), cStore C(5),cOrder C(6),;
               nInvAmt N(10,2), nComm N(5,2), nCommAmt N(9,2),;
               cTranType C(1),DESC C(20))
INDEX ON cRepCode + cTranType TAG (lcWorkFile)
*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : Collect data and insert it into Work file.
*!**************************************************************************
*! Example   : DO lpCollData.
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lcWhileExp , lcRetHdExp , lnPipePos

lnPipePos = ATC("|",laOGFxFlt[lnDatePos,6])
lcFromDate = SUBSTR(laOGFxFlt[lnDatePos,6],1,lnPipePos-1)
lcToDate   = SUBSTR(laOGFxFlt[lnDatePos,6],lnPipePos+1)

lcWhileExp = ".T."
lcRetHdExp = STRTRAN(lcDivExp,"INVHDR.","RETHDR.")
 
SELECT RepComm
LOCATE
*-- check the salesrep if empty. 
IF EMPTY(laOGFxFlt[lnSlRepPos,6])
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + lcDateExp
ELSE
  *-- seek for SalesRep code and the from Date in the between period. 
  *SEEK laOGFxFlt[lnSlRepPos,6] + DTOS(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC("|",laOGFxFlt[lnDatePos,6])-1)))
  SEEK laOGFxFlt[lnSlRepPos,6]
  *:B804332,1 AAN Change the locate condition[Begin].
  *LOCATE REST WHILE RepCode = ALLTRIM(laOGFxFlt[lnSlRepPos,6]) AND &lcDateExp
  LOCATE REST WHILE RepCode = ALLTRIM(laOGFxFlt[lnSlRepPos,6]) FOR &lcDateExp.
  *:B804332,1 AAN Change the locate condition[End].
  lcRpExp = STRTRAN(lcRpExp,"REPCODE","REPCODE + DTOS(DATE) + TRAN + TRANTYPE")

  *-- Remove the Date Expression if user selected Sales Rep. 
  lcRpExp = STRTRAN(lcRpExp,lcDateExp,".T.")
  lcWhileExp = "RepCode = laOGFxFlt[lnSlRepPos,6] AND Date <= CTOD(lcToDate)"
ENDIF

IF lcRpStatus <> 'B'
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + "STATUS = lcRpStatus AND Amount <> 0"
ENDIF

*-- Remove the RepComm alias from lcRpExp.
lcRpExp = STRTRAN(lcRpExp,"REPCOMM.","")

*-- Looping in RepComm to get the records of the selected salesrep
*-- or all the salesreps for the invoice transactions.
SCAN REST WHILE &lcWhileExp FOR &lcRpExp
  WAIT WINDOW "Sales Rep. :" + RepCode + " Invoice : " + Tran  NOWAIT
  IF TranType $ "16"  
    IF !SEEK(Tran,'InvHdr') OR INVHDR.STATUS = "V" OR !(&lcDivExp)
      LOOP
    ENDIF
  ENDIF
  IF TranType $ "57"  
    IF SEEK(Tran,'RetHdr') AND !(&lcRetHdExp)
      LOOP
    ENDIF
  ENDIF

  *-- If Rep Code is found in Sales Rep file.
  IF SEEK(RepCode,'SalesRep')
    m.cRepCode  = RepComm.RepCode
    m.cRepName  = SalesRep.Name
    m.cDivision = IIF(RepComm.TranType $ "16",InvHdr.cDivision,IIF(RepComm.TranType $ "57",RetHdr.cDivision,""))
    m.cInvoice  = RepComm.Tran
    m.dDate     = IIF(RepComm.TranType $ "16",InvHdr.InvDate,IIF(RepComm.TranType='5',;
                  RetHdr.CrDate,IIF(RepComm.TranType = '7',RetHdr.vDate,RepComm.Date)))
    m.cAccount  = RepComm.Account
    m.cStore    = RepComm.Store
    m.cOrder    = RepComm.Order
    m.nInvAmt   = lfGetAmt()
    m.nComm     = RepComm.CommPcnt
    m.nCommAmt  = RepComm.Amount
    m.cTranType = RepComm.TranType
    m.Desc      = RepComm.Desc
    
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF            
  *-- EndIf of Rep Code is found in Sales Rep file.

ENDSCAN
WAIT CLEAR
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpPrintRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : Print report from Work file.
*!**************************************************************************
*! Example   : DO lpPrintRep.
*!**************************************************************************
*
PROCEDURE lpPrintRep
PRIVATE ROW , lnPageNo , lcSalesRep , lnSAmtSub , lnSCommSub , lnLine1 , lnLine2

lnLine1 = ((130 - (LEN(TRIM(QCOMPANY))))/2)
lnLine2 = ((130 - (LEN("Salesrep Commission Statement")))/2)

R_WIDTH  = 'W'
ROW      = 99
lnPageNo = 0

SELECT (lcWorkFile)
DO WHILE !EOF() AND INKEY() <> 32
  lcSalesRep = cRepCode

  *-- Initializing the following variable to hold the sub total per salesrep.
  STORE 0 TO lnSAmtSub,lnSCommSub
  DO lpPrintHdr
  SCAN WHILE cRepCode = lcSalesRep
    *-- Initializing the following variable to hold the sub total per 
    *-- salesrep/division.
    STORE 0 TO lnInvAmtSb,lnCommSub
  
      WAIT WINDOW "Sales Rep. :"+cRepCode+" Invoice : " +cInvoice  NOWAIT
      IF ROW >=55
        DO lpPrintHdr
      ENDIF
      =SEEK (IIF(EMPTY(cStore),'M'+cAccount,'S'+cAccount+cStore),'Customer')
      @ ROW,01  SAY cInvoice
      @ ROW,11  SAY dDate
      @ ROW,21  SAY cDivision
      @ ROW,31  SAY cAccount      
      @ ROW,41  SAY IIF(cTranType = "3","DEBIT ADJUSTEMENT",;
                    IIF(cTranType="4","CREDIT ADJUSTEMENT",;
                    LEFT(Customer.BtName,20)))
      @ ROW,63  SAY LEFT(cStore,5)      
      @ ROW,70  SAY IIF(cTranType $ "34",Desc,SUBSTR(ALLTRIM(Customer.cAddress32),1,15)+', '+SUBSTR(ALLTRIM(Customer.cAddress42),1,3))
      @ ROW,91  SAY cOrder      
      @ ROW,101 SAY nInvAmt  PICTURE "99999999.99"
      @ ROW,113 SAY nComm    PICTURE "99.99%"
      @ ROW,122 SAY nCommAmt PICTURE "9999999.99"

      *-- Subtotal of invoice amount and commission by salesrep/division
      lnSAmtSub  = lnSAmtSub  + nInvAmt
      lnSCommSub = lnSCommSub + nCommAmt
      ROW = ROW + 1
  ENDSCAN
  ROW = ROW + 1

  *-- To print subtotal per salesrep.
  DO lpPrSubTot
ENDDO
ROW = ROW + 1

WAIT CLEAR

DO ENDREPORT
*-- End of lpPrintRep.

*!**************************************************************************
*! Name      : lpPrintHdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : Print report header.
*!**************************************************************************
*! Example   : DO lpPrintHdr
*!**************************************************************************
*
PROCEDURE lpPrintHdr

lnPageNo = lnPageNo + 1
*-- To print the date,company name, pageno, report title.
DO lpHeader
ROW = 4
@ ROW,01 SAY "Sales Rep.    :  " + cRepCode + "   " + cRepName
ROW = ROW + 1
*@ ROW,01 SAY "Division Range => From " + IIF(!EMPTY(lcLowDiv),lcLowDiv,'Begin') +;
*                               " To " + IIF(!EMPTY(lcHigDiv),lcHigDiv,'End')
@ ROW,90 SAY "Date Rang => From " + lcFromDate + " To " + lcToDate
ROW = ROW + 1
@ ROW,01 SAY REPLICATE ("*",132)
ROW = ROW + 2
@ ROW,01 SAY "Tran#     Date      Division  Cust No.  Customer Name         Store  City, Province       Order#      Tran Amt. Comm %   Commission"
ROW = ROW + 1
@ ROW,01 SAY REPLICATE ("-",132)
ROW = ROW + 1
*-- End of lpPrintHdr.

*!**************************************************************************
*! Name      : lpHeader
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : To print the first header of the report.
*!**************************************************************************
*! Example   : DO lpHeader.
*!**************************************************************************
*
PROCEDURE lpHeader
@ 01,01 SAY Date()
@ 01,lnLine1  SAY QCOMPANY
@ 01,120 SAY 'PAGE#'
@ 01,126 SAY STR(lnPageNo,4)
@ 02,lnLine2 SAY "Salesrep Commission Statement"
*-- End of lpHeader.

*!**************************************************************************
*! Name      : lpPrSubTot
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : To print the subtotals
*!**************************************************************************
*! Example   : DO lpPrSubTot.
*!**************************************************************************
*
PROCEDURE lpPrSubTot
IF ROW >=55
  DO lpPrintHdr
ENDIF

@ ROW,01 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,01  SAY 'SubTotal for Salesrep : ' + lcSalesRep
@ ROW,100 SAY lnSAmtSub  PICTURE "99999999.99"
@ ROW,121 SAY lnSCommSub PICTURE "99999999.99"
ROW = ROW + 1
IF ROW >=55
  DO lpPrintHdr
ENDIF
@ ROW,01 SAY REPLICATE('-',132)
*-- End of lpPrSubTot.

*!**************************************************************************
*! Name      : lfGetAmt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : To print the Amount.
*!**************************************************************************
*! Example   : =lfGetAmt()
*!**************************************************************************
*
FUNCTION lfGetAmt
DO CASE
  CASE RepComm.TranType = "1"
    RETURN InvHdr.TotalChg
  CASE RepComm.TranType = "5"
    RETURN IIF(RetHdr.Status= "V",-RetHdr.vAmount,-RetHdr.Amount)
  CASE RepComm.TranType = "7"   
    RETURN RetHdr.vAmount
  OTHERWISE
    RETURN RepComm.Amount
ENDCASE
*-- End of lfGetAmt.

*!**************************************************************************
*! Name      : lfvRepCode
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!**************************************************************************
*! Calls     : Functions  : gfBrows
*!**************************************************************************
*! Called from : Option Grid [Sales representative Object]
*!**************************************************************************
*! Example   : = lfvRepCode()
*!**************************************************************************
*
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*-- IF Statment to check if we are going to Browse
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
    
  *-- IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- End of lfvRepCode.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : When function to save the Old value
*!**************************************************************************
*! Called from : Sales Rep option grid field.
*!**************************************************************************
*! Example     : = lfwOldVal()
*!**************************************************************************
*
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- End of lfwOldVal.

*!**************************************************************************
*! Name      : lfvDate
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/98
*! Purpose   : Validate Entered date [i.e. Not empty]
*!**************************************************************************
*! Called from : Option Grid [Period Option]
*!**************************************************************************
*! Example   : = lfvDate()
*!**************************************************************************
*
FUNCTION lfvDate
IF EMPTY(EVALUATE(SYS(18)))
  WAIT WINDOW 'You must fill period range...' NOWAIT
  lcCurrObj = SYS(18)
  &lcCurrObj = laOldVal
  SHOW GET lcCurrObj
  _CUROBJ = _CUROBJ
ENDIF
*-- End of lfvDate.

*!**************************************************************************
*! Name      : lpGenExp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/14/2000
*! Purpose   : Generate Division Expression.
*!**************************************************************************
*! Example   : DO lpGenExp
*!**************************************************************************
*
PROCEDURE lpGenExp
PRIVATE lcAlias
lcAlias = ALIAS()
    
*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1]
=ACOPY(laOGFxFlt,laTempExpr)

*-- If user selected Division.
IF ALLTRIM(laTempExpr[lnDivPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only division expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnDivPos,1),7)
  
  *-- Generate expression for Division.
  lcDivExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Division from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcDivExp,".T.")  
ENDIF  
*-- EndIf of user selected Division.  

*-- If user selected date.
IF ALLTRIM(laTempExpr[lnDatePos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only division expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnDatePos,1),7)
  
  *-- Generate expression for Division.
  lcDateExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
  *-- Remove Date expression from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcDateExp,".T.")
*-- EndIf of user selected Division.  
ENDIF
*-- EndIf user selected date.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)

SELECT (lcAlias)
*-- End of lpGenExp.