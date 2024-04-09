*:****************************************************************************
*: Program file  : ICCTSLFR.PRG
*: Program desc. : CUT AND SOLD REPORT FOR FRENCH ROOM
*: System        : Aria Apparel System (A27).
*: Module        : Inventory Contol  (IC)
*: Developer     : Ahmed Abdel Naby (AAN) Due to C#101976,1
*: Date          : 16/10/2000
*:********************************************************************
*: Example            : DO ICCTSLFR
*:********************************************************************
*:*C102029,1 HBG 01/01/2001  - Add Sub total for each style based on the Cup size.
*:						     - Add column to print 'Stock in weehs'
*:*B803977,1 AME 02/06/2001   IN YTD PRINT JUST Invoices Qyts for this year
*:B804041,1 ABD  03/01/2001 Fix bug that the Mthly AVG to calculating and print 
*:B804041,1 ABD            Based on the invoices that done during this year only.
*:B804069,1 WAB  03/19/2001 Fix of the program go thru infinite loop
*:B804520,1 BWA  11/15/2001 Fix the bug of sort the styles alphabetically within the division.
*:********************************************************************

*-- To Evaluate NonMajor Type and variables.
= (TYPE("lnMajSeg") = "C") AND lfEvalSegs()


*B803977,1 AME [Begin] Calculate the begining of the year date.
ldBDate = CTOD("01/01/"+STR(YEAR(DATE()),4))
*B803977,1 AME [End]

*-- Temp. file to hold Styles file records under a filter exp.
lcTmpStyle = gfTempName()

*-- To add filter exp. of (STYLE.Make) to main filter exp.
*-- if user select (Make)
lcMakeOp  = IIF(lcRpMake='M','','!')
*-- if user select (Both)
lcRepExpr = IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[STYLE.Make])

*-- if a user using an option (make/but)
IF !empty(lcRepExpr)
  lcRpExp =  lcrpexp + lcRepExpr
ENDIF

SELECT Style
COPY STRUCTURE TO (gcWorkDir + lcTmpStyle)

*-- Variable to know if this company is multi warehouse or not
llMultiWH = IIF (llMultWare, .T., .F.)

*-- Variables to Print SubTotal By Style.
lcStySub = ''
*C102029,1 HBG 01/01/2001 Add Variable to check if Style Desc. changed[Begin]
lcStDesSub = ''
*C102029,1 [End]

*C102029,1 HBG 01/01/2001 Add Variables to Print SubTotal of stock in week [Begin]
*STORE 0 TO lnSSubShp,lnSSubSlAvg,lnSSubSlMTD,lnSSubStk,lnSSubWip,lnSSubOrd,lnSSubOTS  
STORE 0 TO lnSSubShp,lnSSubSlAvg,lnSSubSlMTD,lnSSubStk,lnSSubWip,lnSSubOrd,lnSSubOTS,lnSSubStkWk
*-- Add Variables to Print SubTotal for sytle Desc [Begin]
STORE 0 TO lnDSubShp,lnDSubSlAvg,lnDSubSlMTD,lnDSubStk,lnDSubWip,lnDSubOrd,lnDSubOTS,lnDSubStkWk
*C102029,1 [End]

IF llMultiWH
  STORE 'N'  TO lcLoc
ENDIF

STORE SPACE(20) TO HPDESC

SET DEVICE TO Print

*-- To print the report
=lfPrntRept()

*B804041,5 wab (START) - display message if there is no record in the file and delete tempfile
*DO ENDREPORT
SELECT (lcTmpStyle)
LOCATE
IF EOF()
  SET DEVICE TO SCREEN
  =gfDialog('I','There are no records to display')
  RETURN
ELSE
  DO ENDREPORT
ENDIF
IF USED(lcTmpStyle)
  SELECT(lcTmpStyle)
  USE IN (lcTmpStyle)
ENDIF  
IF FILE(gcWorkDir+lcTmpStyle+".DBF") 
  ERASE (gcWorkDir+lcTmpStyle+".DBF")
ENDIF
IF FILE(gcWorkDir+lcTmpStyle+".FPT") 
  ERASE (gcWorkDir+lcTmpStyle+".FPT")
ENDIF
*B804041,5 wab (END) 

SET DEVICE TO Screen

*!*************************************************************
*! Name      : lfPrntRept
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 16/10/2000
*! Purpose   : To print the report.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfPrntRept()
*!*************************************************************
FUNCTION lfPrntRept

R_WIDTH  = 'W'
R_TITLE  = 'Best Sellers Re-Order Report'		&& report title
lnRow    = 99									&& variable hld a curr. row
llFound  = .F.
llFoundSty = .F.
PAGENO   = 0

*-- Open temp. file
=gfOpenFile(gcWorkDir + lcTmpStyle , '' , 'EX' )

SELECT Style
GOTO TOP

*-- to transfer a recrods from a style file ta temp. file
SCAN FOR &lcRpExp
  SCATTER MEMVAR MEMO
  *-- Message when collecting data from style file to tmp file
  WAIT WINDOW "Collecting data for style: " + Style NOWAIT
  INSERT INTO (lcTmpStyle) FROM MEMVAR
ENDSCAN
WAIT CLEAR

SELECT (lcTmpStyle)
*B804069,1 WAB (START)- add the description to the index
*INDEX ON CDivision+Style TAG (lcTmpStyle)

*B804520,1 BWA 11/15/2001 Fix the bug of sort the styles alphabetically within the division.[START]
*INDEX ON CDivision+Desc+Style TAG (lcTmpStyle)
INDEX ON CDivision + Style + Desc TAG (lcTmpStyle)
*B804520,1 BWA 11/15/2001.[END]

*B804069,1 WAB (end)
lcDivision = SPACE(6)

*-- to show a message on the task bar during the printing
lcStatuBar = SET("STATUS BAR")
lcMessIs  = SET("MESSAGE",1)

SET STATUS BAR ON
SET MESSAGE TO 'Printing - Press << Space Bar >> to abort'


*-- printing loop
DO WHILE !EOF() AND INKEY() <> 32 
  *-- Message when printing data from tmp file  
  WAIT WINDOW "Printing style: " + Style NOWAIT
  *-- Variables to acumolate each column in the report.
  *C102029,1 HBG 01/01/2001 Add Variables to Print SubTotal for stock in week [Begin]
  *STORE 0 TO lnSubShp,lnSubSlAvg,lnSubSlMTD,lnSubStk,lnSubWip,lnSubOrd,lnSubOTS
  STORE 0 TO lnSubShp,lnSubSlAvg,lnSubSlMTD,lnSubStk,lnSubWip,lnSubOrd,lnSubOTS,lnSubStkWk

  *-- Get the Style Description  
  *B804069,1 WAB (START) get the description after gettting the division
  *lcStDesSub = Desc
  *B804069,1 WAB (end)
  *C102029,1 [Begin]
  
  lcDivision = CDivision  				&& variable to hold the division
  llFound    = .F.
  =lfPrnHdr()							&& To print the report's header.
  *B804069,1 WAB (START) - add !eof to the condition
  *DO WHILE CDivision = lcDivision
  DO WHILE CDivision = lcDivision AND !EOF()
  *B804069,1 WAB - (END)
    *C102029,1 HBG 01/01/2001 To print Subtotal if Style Description changed [Begin]
    *B804069,1 WAB (START) - add !eof to the condition
    *DO WHILE Desc = lcStDesSub 
    lcStDesSub = Desc

    *B804520,1 BWA 11/15/2001 Fix the bug of sort the styles alphabetically within the division.[START]
    *DO WHILE Desc = lcStDesSub  AND !EOF()
    DO WHILE Desc = lcStDesSub  AND !EOF() AND cDivision = lcDivision
    *B804520,1 BWA 11/15/2001.[END]

    *B804069,1 WAB (END) 
    *B804041,5 wab (START) - display continuos message 
      WAIT WINDOW "Printing style: " + Style NOWAIT
    *B804041,5 wab (END)
    *C102029,1 [Begin]
      =IIF(lnRow >= 55,lfPrnHdr(),.F.)        
      *-- lcrpprtbas is a variable from O.G. to know print based on WIP or PLAN
      IF (lcrpprtbas='W' .AND. TotStk+TotWip+TotOrd = 0) .OR. ;
           (lcrpprtbas = 'P' .AND. TotPlan = 0)

        *B804069,1 WAB (START) - no need for check if the pointer in the eof()
        **B803977,1 AME [Start] CHECK THE END OF FILE.
        **SKIP
        *IF !EOF()
        *  SKIP
        **B804041,5 wab (START) if the file is in EOF() it cause infinte loop
        *ELSE
        *  EXIT
        **B804041,5 wab (end)
        *ENDIF
        **B803977,1 AME [END] 
        
        SKIP
        *B804069,1 WAB (END) 
        
        IF llFoundSty AND lcStySub <> cstymajor
          llFoundSty = .F.
          lnRow = lnRow + 1
          @ lnRow,00 SAY REPLICATE ('-',132)
          lnRow = lnRow + 1
          @ lnRow,000 SAY 'SUBTOTALS FOR STYLE '+lcStySub
          @ lnRow,044 SAY lnSSubShp   PICTURE '@Z 9999999'
          @ lnRow,052 SAY lnSSubSlAvg PICTURE '@Z 99999.99'
          @ lnRow,062 SAY lnSSubSlMTD PICTURE '@Z 9999999'
          @ lnRow,070 SAY lnSSubStk   PICTURE '@Z 9999999'
          @ lnRow,079 SAY lnSSubWip   PICTURE '@Z 9999999'
          @ lnRow,087 SAY lnSSubOrd   PICTURE '@Z 9999999'
          @ lnRow,096 SAY lnSSubOTS   PICTURE '@Z 9999999'
          *C102029,1 HBG 01/01/2001 Print SubTotal of stock in week for each style[Begin]
          @ lnRow,100 SAY lnSSubStkWk PICTURE '@Z 9999999.999'        
          *C102029,1 [End]
          lnRow = lnRow + 1
          @ lnRow,00 SAY REPLICATE ('-',132)
          lnRow = lnRow + 1
          lcStySub = cstymajor
          *C102029,1 HBG 01/01/2001 Add Variables to Print SubTotal of stock in week [Begin]
          *STORE 0 TO lnSSubShp,lnSSubSlAvg,lnSSubSlMTD,lnSSubStk,lnSSubWip,lnSSubOrd,lnSSubOTS  
          STORE 0 TO lnSSubShp,lnSSubSlAvg,lnSSubSlMTD,lnSSubStk,lnSSubWip,lnSSubOrd,lnSSubOTS,lnSSubStkWk  
          *STORE 0 TO lnDSubShp,lnDSubSlAvg,lnDSubSlMTD,lnDSubStk,lnDSubWip,lnDSubOrd,lnDSubOTS,lnDSubStkWk
          *C102029,1 [End]
        ENDIF
        LOOP
      ENDIF
    
      *-- Printing the sizes of the style horizontally.
      =SEEK('S'+Scale,'Scale')
      *B803977,1 AME [START]  THE WHOLE STYLE INSTEAD OF MAJOR SEG.
      *lcStyClr = cstymajor
      lcStyClr = STYLE
      *B803977,1 AME [END]
      FOR lnCounter = 1 TO Scale.Cnt
        =IIF(lnRow >= 55,lfPrnHdr(),.F.)
        lcCounter = STR(lnCounter,1)
        
        *B803977,1 AME [START] initial value of YTD qty. 
        *STORE 0 TO lnOTS
        STORE 0 TO lnOTS,lnYTD
        *B803977,1 AME [End] 
        
        lnOTS = IIF(lcrpprtbas='W',WIP&lcCounter,PLAN&lcCounter) + STK&lcCounter - ORD&lcCounter
        IF lnrpminqty > 0 AND lnOTS < lnrpminqty
          LOOP
        ENDIF
        llFound = .T.
        llFoundSty = .T.
        lcStySub = cstymajor
      
        @ lnRow,000 SAY SUBSTR(Style,1,LnMajorlen)
        @ lnRow,013 SAY LEFT(ALLTRIM(Desc),15)
        @ lnRow,030 SAY SUBSTR(Style,lnNonMajSt,Lncolorlen)  
        @ lnRow,037 SAY Scale.Sz&lcCounter
        
        *B803977,1 AME [START] calculate ytd Qtys
        *@ lnRow,045 SAY Shp&lcCounter PICTURE '@Z 999999'
        
        IF SEEK(Style,'InvLine')
          SELECT InvLine
          SCAN WHILE Style = &lcTmpStyle..Style
            IF InvDate >= ldBDate AND InvDate <= DATE() 
              lnYTD = lnYTD + Qty&lcCounter   &&Calculate this year invoices qty. 
            ENDIF
          ENDSCAN
        ENDIF
        @ lnRow,045 SAY lnYTD PICTURE '@Z 999999'
        SELECT (lcTmpStyle)
        *B803977,1 AME [End] 
        
                 
        *B803977,1 AME [START] calculate proper Summetions.
        *lnSubShp = lnSubShp + Shp&lcCounter
        *lnSSubShp = lnSSubShp + Shp&lcCounter
        **C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
        *lnDSubShp = lnDSubShp + Shp&lcCounter
        **C102029,1 [End]
        
        lnSubShp = lnSubShp + lnYTD
        lnSSubShp = lnSSubShp + lnYTD
        lnDSubShp = lnDSubShp + lnYTD
        *B803977,1 AME [End] 
        *-- To get the days of the month.
        ldCurrMonth = CTOD(PADL(ALLTRIM(STR(MONTH(DATE()),2)),2,'0');
                      +'/'+'01/'+RIGHT(STR(YEAR(DATE()),4),2))
  
        IF MONTH(DATE())<>12
           ldNextMonth = CTOD(PADL(ALLTRIM(STR(MONTH(DATE())+1,2)),2,'0');
                            +'/'+'01/'+RIGHT(STR(YEAR(DATE()),4),2))
        ELSE
          ldNextMonth = CTOD('01/01/'+RIGHT(STR(YEAR(DATE())+1,4),2))
        ENDIF
        lnDaysOfMn = ldNextMonth - ldCurrMonth
        lnDecimal  = ROUND((DAY(DATE()) / lnDaysOfMn),2)
        lnNoOfMnth = lnDecimal + (MONTH(DATE())-1)
             
        *B804041,1 ABD - Fix bug that the Mthly AVG to calculating and print 
        *B804041,1 ABD - Based on the invoices that done during this year only. [Begin]
        *IF SEEK(Style,'InvLine') .AND. STR(YEAR(InvLine.InvDate),4) = STR(YEAR(DATE()),4)

        lnShpQty = 0
        IF SEEK(Style,'InvLine')
          lnAlias = SELECT(0)
          SELECT InvLine
          SCAN WHILE Style = &lcTmpStyle..Style
            IF InvDate >= ldBDate AND InvDate <= DATE() 
              lnShpQty = lnShpQty + Qty&lcCounter   &&Calculate this year invoices qty. 
            ENDIF
          ENDSCAN
          SELECT(lnAlias)
        ENDIF
        
        *@ lnRow,053 SAY ROUND(Shp&lcCounter / lnNoOfMnth,2) PICTURE "@Z 9999.99"
        *lnSubSlAvg = lnSubSlAvg + ROUND(Shp&lcCounter / lnNoOfMnth,2)
        *lnSSubSlAvg = lnSSubSlAvg + ROUND(Shp&lcCounter / lnNoOfMnth,2)
        *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
        *lnDSubSlAvg = lnDSubSlAvg + ROUND(Shp&lcCounter / lnNoOfMnth,2)
        
        @ lnRow,053 SAY ROUND(lnShpQty / lnNoOfMnth,2) PICTURE "@Z 9999.99"
        lnSubSlAvg = lnSubSlAvg + ROUND(lnShpQty  / lnNoOfMnth,2)
        lnSSubSlAvg = lnSSubSlAvg + ROUND(lnShpQty / lnNoOfMnth,2)
        lnDSubSlAvg = lnDSubSlAvg + ROUND(lnShpQty  / lnNoOfMnth,2)
        *B804041,1 ABD - [End]
        
        *C102029,1 [End]
       *B804041,1 ABD - End for if statement. [Begin]
       *ENDIF
       *B804041,1 ABD - [End]

        lnQtyShp = 0
        IF SEEK(lcStyClr,'InvLine')
          SELECT InvLine
          *B803977,1 AME [Start] 
          *Qty&lcCounter FOR InvDate >= ldCurrMonth AND InvDate<= Date();
          TO lnQtyShp
          *B803977,1 AME [End] 
   
          SUM REST WHILE STYLE+INVOICE+STR(LINENO,6) = lcStyClr;
          Qty&lcCounter FOR InvDate >= DATE() - DAY(DATE()) +1 AND InvDate<= Date();
          TO lnQtyShp
               
          lnSubSlMTD = lnSubSlMTD +  lnQtyShp
          lnSSubSlMTD = lnSSubSlMTD +  lnQtyShp
          *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
          lnDSubSlMTD = lnDSubSlMTD +  lnQtyShp
          *C102029,1 [End]
          SELECT (lcTmpStyle)
        ENDIF
        @ lnRow,063 SAY lnQtyShp PICTURE '@Z 999999'
        @ lnRow,071 SAY Stk&lcCounter PICTURE '@Z 999999'
        lnSubStk = lnSubStk + Stk&lcCounter
        lnSSubStk = lnSSubStk + Stk&lcCounter
        *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
        lnDSubStk = lnDSubStk + Stk&lcCounter
        *C102029,1 [End]
      
        @ lnRow,080 SAY Wip&lcCounter PICTURE '@Z 999999'
        lnSubWip = lnSubWip + Wip&lcCounter
        lnSSubWip = lnSSubWip + Wip&lcCounter
        *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
        lnDSubWip = lnDSubWip + Wip&lcCounter
        *C102029,1 [End]
        @ lnRow,088 SAY Ord&lcCounter PICTURE '@Z 999999'
        lnSubOrd = lnSubOrd + Ord&lcCounter      
        lnSSubOrd = lnSSubOrd + Ord&lcCounter      
        *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
        lnDSubOrd = lnDSubOrd + Ord&lcCounter      
        *C102029,1 [End]
        lnTotOTS = (Stk&lcCounter + Wip&lcCounter) - Ord&lcCounter
        DO CASE
          CASE lnrpminqty = 0
            @ lnRow,097 SAY lnTotOTS PICTURE '@Z 999999'
            lnSubOTS = lnSubOTS + lnTotOTS
            lnSSubOTS = lnSSubOTS + lnTotOTS
            *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
            lnDSubOTS = lnDSubOTS + lnTotOTS
            *C102029,1 [End]
          CASE lnrpminqty < 0
            IF lnTotOTS < 0
              @ lnRow,097 SAY lnTotOTS PICTURE '@Z 999999'
              lnSubOTS = lnSubOTS + lnTotOTS
              lnSSubOTS = lnSSubOTS + lnTotOTS
              *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
              lnDSubOTS = lnDSubOTS + lnTotOTS
              *C102029,1 [End]
            ENDIF          
          OTHERWISE  
            @ lnRow,097 SAY lnTotOTS PICTURE '@Z 999999'
            lnSubOTS = lnSubOTS + lnTotOTS
            lnSSubOTS = lnSSubOTS + lnTotOTS
            *C102029,1 HBG 01/01/2001 Calculate subtotal for each Style Desc [Begin]
            lnDSubOTS = lnDSubOTS + lnTotOTS
            *C102029,1 [End]
        ENDCASE
        *C102029,1 HBG 01/01/2001 Calculate Stock in Week according to the formula 
        *C102029,1                "Stock in Week = (Qty on hand/monthly sales average)*4"
        *C102029,1                if Qty on hand and monthly sales average not = 0 [Begin]
        
        *B804041,4 ABD - Get the total shiped qty upon the year. [Begin]
        *IF Stk&lcCounter <> 0 AND ROUND(Shp&lcCounter / lnNoOfMnth,2) <> 0        
          *lnStkWek = (Stk&lcCounter/ROUND(Shp&lcCounter / lnNoOfMnth,2))*4
        lnShpQty = 0
        IF SEEK(Style,'InvLine')
          lnAlias = SELECT(0)
          SELECT InvLine
          SCAN WHILE Style = &lcTmpStyle..Style
            IF InvDate >= ldBDate AND InvDate <= DATE() 
              lnShpQty = lnShpQty + Qty&lcCounter   &&Calculate this year invoices qty. 
            ENDIF
          ENDSCAN
          SELECT(lnAlias)
        ENDIF
        IF Stk&lcCounter <> 0 AND ROUND(lnShpQty / lnNoOfMnth,2) <> 0
          lnStkWek = ROUND((Stk&lcCounter/ROUND(lnShpQty / lnNoOfMnth,2))*4,3)        
        *B804041,4 ABD - [End]
          
          @ lnRow,0100 SAY lnStkWek PICTURE '@Z 9999999.999'
          lnSubStkWk  = lnSubStkWk  + lnStkWek 
          lnDSubStkWk = lnDSubStkWk + lnStkWek 
          lnSSubStkWk = lnSSubStkWk + lnStkWek 
        ENDIF  
        *C102029,1 [End]
        lnRow = lnRow + 1
      ENDFOR  
      *B804069,1 WAB (START) - no need for check if the pointer in the eof()
      **B803977,1 AME [Start] CHECK THE END OF FILE.
      **SKIP
      *IF !EOF()
      *  SKIP
      *ENDIF
      **B803977,1 AME [END]
      SKIP
      *B804069,1 WAB (START) - no need for check if the pointer in the eof()
    
      IF lcStySub <> cstymajor
        IF llFound
          llFoundSty = .F.
          lnRow = lnRow + 1
          @ lnRow,00 SAY REPLICATE ('-',132)
          lnRow = lnRow + 1
          @ lnRow,000 SAY 'SUBTOTALS FOR STYLE '+lcStySub
          @ lnRow,044 SAY lnSSubShp   PICTURE '@Z 9999999'
          @ lnRow,052 SAY lnSSubSlAvg PICTURE '@Z 99999.99'
          @ lnRow,062 SAY lnSSubSlMTD PICTURE '@Z 9999999'
          @ lnRow,070 SAY lnSSubStk   PICTURE '@Z 9999999'
          @ lnRow,079 SAY lnSSubWip   PICTURE '@Z 9999999'
          @ lnRow,087 SAY lnSSubOrd   PICTURE '@Z 9999999'
          @ lnRow,096 SAY lnSSubOTS   PICTURE '@Z 9999999'
          *C102029,1 HBG 01/01/2001 Print SubTotal of stock in week for each Style Desc.[Begin]
          @ lnRow,100 SAY lnSSubStkWk PICTURE '@Z 9999999.999'    
          *-- Add Variables to Print SubTotal of stock in week [Begin]
          *STORE 0 TO lnSSubShp,lnSSubSlAvg,lnSSubSlMTD,lnSSubStk,lnSSubWip,lnSSubOrd,lnSSubOTS
          STORE 0 TO lnSSubShp,lnSSubSlAvg,lnSSubSlMTD,lnSSubStk,lnSSubWip,lnSSubOrd,lnSSubOTS,lnSSubStkWk
          *C102029,1 [End]
          
          lcStySub = cstymajor
          lnRow = lnRow + 1
          @ lnRow,00 SAY REPLICATE ('-',132)
          lnRow = lnRow + 1
        ENDIF
      ENDIF
    *C102029,1 HBG 01/01/2001 End The loop of printing subtotal to the style description [Begin]
    ENDDO
    *-- print SubTotal
    IF llFound
      llFoundSty = .F.
      lnRow = lnRow + 1
      @ lnRow,00 SAY REPLICATE ('-',132)
      lnRow = lnRow + 1
      @ lnRow,000 SAY 'SUBTOTALS FOR '+lcStDesSub 
      @ lnRow,044 SAY lnDSubShp   PICTURE '@Z 9999999'
      @ lnRow,052 SAY lnDSubSlAvg PICTURE '@Z 99999.99'
      @ lnRow,062 SAY lnDSubSlMTD PICTURE '@Z 9999999'
      @ lnRow,070 SAY lnDSubStk   PICTURE '@Z 9999999'
      @ lnRow,079 SAY lnDSubWip   PICTURE '@Z 9999999'
      @ lnRow,087 SAY lnDSubOrd   PICTURE '@Z 9999999'
      @ lnRow,096 SAY lnDSubOTS   PICTURE '@Z 9999999'
      @ lnRow,100 SAY lnDSubStkWK PICTURE '@Z 9999999.999'        
      STORE 0 TO lnDSubShp,lnDSubSlAvg,lnDSubSlMTD,lnDSubStk,lnDSubWip,lnDSubOrd,lnDSubOTS,lnDSubStkWk
              
      *B804069,1 WAB (START) no need here - only after gettng the division
      *lcStDesSub = Desc
      *B804069,1 WAB (START)
      lnRow = lnRow + 1
      @ lnRow,00 SAY REPLICATE ('-',132)
      lnRow = lnRow + 1
    ENDIF
    *C102029,1 [End]
  ENDDO
  
  IF llFound
    lnRow = lnRow + 1
    @ lnRow,00 SAY REPLICATE ('-',132)
    lnRow = lnRow + 1
    @ lnRow,000 SAY 'SUBTOTALS FOR DIVISION '+lcDivision
    @ lnRow,044 SAY lnSubShp   PICTURE '@Z 9999999'
    @ lnRow,052 SAY lnSubSlAvg PICTURE '@Z 99999.99'
    @ lnRow,062 SAY lnSubSlMTD PICTURE '@Z 9999999'
    @ lnRow,070 SAY lnSubStk   PICTURE '@Z 9999999'
    @ lnRow,079 SAY lnSubWip   PICTURE '@Z 9999999'
    @ lnRow,087 SAY lnSubOrd   PICTURE '@Z 9999999'
    @ lnRow,096 SAY lnSubOTS   PICTURE '@Z 9999999'
    *C102029,1 HBG 01/01/2001 Print SubTotal of stock in week for each Style[Begin]
    @ lnRow,100 SAY lnSubStkWK PICTURE '@Z 9999999.999'
    *C102029,1 [End]
    lnRow = lnRow + 1
    @ lnRow,00 SAY REPLICATE ('-',132)
  ELSE 
    @ 09,46 SAY '*******************************************************'
    @ 10,46 SAY '***** NO STYLES SELECTED TO PRINT FOR DIVISION &lcDivision *****'
    @ 11,46 SAY '*******************************************************' 
  ENDIF

  *-- Return the status bar as before
  SET MESSAGE TO lcMessIs
  SET STATUS BAR &lcStatuBar
ENDDO
WAIT CLEAR
*-- The end of the printing loop

*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 16/10/2000
*! Purpose   : To print the report's header.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO + 1    && Page no.
DO lpRptHdr WITH DATE(),lcrptitle
*C102029,1 HBG 01/01/2001  Add column to print 'Stock in weehs' [Begin]
*@ 05,00 SAY '                                              SALES    MTHLY    SALES     QTY      QTY     QTY'
*@ 06,00 SAY 'STYLE        DESCRIPTION      COLOR  SIZE   QTY YTD  SLS AVG  QTY MTD  OnHand  OnOrder    CMTD   OC QTY'
@ 05,00 SAY '                                              SALES    MTHLY    SALES     QTY      QTY     QTY              STOCK'
@ 06,00 SAY 'STYLE        DESCRIPTION      COLOR  SIZE   QTY YTD  SLS AVG  QTY MTD  OnHand  OnOrder    CMTD   OC QTY    IN WEEK'
*C102029,1 [End]
@ 07,00 SAY REPLICATE ('-',132)
lnRow = 8

*!*************************************************************
*! Name      : lpRptHdr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 16/10/2000
*! Purpose   : To print the report header
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : ldDate   : System Date
*!                      lcRptitle : Report Name
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lpRptHdr()
*!*************************************************************
PROCEDURE lpRptHdr
PARAMETER ldDate,lcRptitle

lcRptitle = ALLTRIM(lcRptitle)
R_TITLE   = ALLTRIM(R_TITLE)

*-- Wide Report
lnCol1 = ((130 - (LEN(TRIM(QCOMPANY))))/2)
lnCol2 = ((130 - (LEN(R_TITLE)))/2)
lnCol3 = ((130 - (LEN(lcRptitle)))/2)

@ 01,000 SAY ldDate
@ 01,lnCol1  SAY QCOMPANY
@ 01,120 SAY 'PAGE#'
@ 01,126 SAY STR(PAGENO,4)
@ 01,130 SAY '~'
@ 02,lnCol2  SAY R_TITLE 
@ 03,lnCol3  SAY lcRptitle
@ 04,00 SAY REPLICATE('*',132)


*:****************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:****************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfEvalSegs()
*:****************************************************************
*
FUNCTION lfEvalSegs
lnMajSeg    = gfItemMask('SM')  && No. of major segments.
lnMajorLen  =  LEN(gfItemMask("PM"))
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
       
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]

*-- Compute Variables titles. [begin]
lcStyMake  = lcStyMajor + ' (Make/Buy)'
*-- Compute Variables titles. [end]

RETURN ''

*-- End of lfEvalSegs.
*:****************************************************************
*: Name        : lfvFabric
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : Valid function of the Fabric
*:****************************************************************
*: Called from : Option grid [Fabric Get field]
*:****************************************************************
*: Calls       : FaBrow()
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Fabric he entered is not in the file.
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*-- End of lfvFabric.
*:****************************************************************
*: Name        : lfsrvSty
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : Rise change style flag, in range browse screen.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : None
*:****************************************************************
*: Example     : =lfsrvSty()
*:****************************************************************
*: Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:****************************************************************
*
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE

*-- End of lfsrvSty.
*:****************************************************************
*: Name        : lfStySum
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : sum a specific field for the current style in style file
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid,style browse calculated fields.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : Calculated field value.
*:****************************************************************
*: Example     : =lfStySum()
*:****************************************************************
*
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE cstymajor = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)

*-- End of lfStySum.

*:****************************************************************
*: Name      : lfwOldVal
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 16/10/2000
*: Purpose   : When function to get the Old value
*:****************************************************************
*: Called from : Some of the Option Grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfwOldVal()
*:****************************************************************
*
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*-- End of lfwOldVal.
