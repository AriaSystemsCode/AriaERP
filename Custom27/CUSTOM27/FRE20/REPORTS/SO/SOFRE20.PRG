*:**************************************************************************
*: Program file  : SOFRE20
*: Program desc. : Custom Order/Sales Summary Report By Month For OMS (FRE20).
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 06/18/2001
*: Reference     : C102355
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExpr , lpCollData , lpInsrtLin
*: Functions     : lfwRepWhen , lfItmPos , lfsrAcc
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*Modifications
*B605927,1 TMI Include completed orders to see the related ivoices
*B605927,4 SSH Remove the deleting setting
*B606705,1 ASH 12/04/2002 Fix the bug of wrong computing ordered and shppied quantities and amounts.
*B127260,1 EIH 06/10/2005 Fix the bug of when select division shipped amount wrong.
*:**************************************************************************


*-- Check if user didn't typed a Month Range. [Begin]
IF LEN(laOGFxFlt[lnMonthPos,6]) < 15
  *-- Message < You have to type a full month range. >
  *-- Buttons <                  OK                  >
  =gfModalGen("TRM000000B00000","DIALOG",'','','You have to type a full month range.')
  RETURN
ENDIF
*-- Check if user didn't typed a Month Range. [End]

*-- Check if user didn't typed a Date Range. [Begin]
IF LEN(ALLTRIM(laOGFxFlt[lnDatePos,6])) < 21
  *-- Message < You have to type a full date range. >
  *-- Buttons <                 OK                  >
  =gfModalGen("TRM000000B00000","DIALOG",'','','You have to type a full date range.')
  RETURN
ENDIF
*-- Check if user didn't typed a Date Range. [End]

*-- IF filter change collect data again.
IF llOGFltCh
  PRIVATE lcEnterExp , lcCanclExp , lcStartExp , lcComplExp , lcInvLnExp , ;
          lcAcctExp , lcGroupBy , lcSetDelet , lcKeyValue
  lcKeyValue = ''        
  STORE '.T.' TO lcEnterExp , lcCanclExp , lcStartExp , lcAcctExp , lcComplExp , lcInvLnExp
  
  DO lpCreatFil
  
  *-- If user Selected Account or Date Range or Month Range , remove them from lcRpExp 
  IF ALLTRIM(laOGFxFlt[lnDatePos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnAcctPos,1]) $ lcRpExp OR ;
     ALLTRIM(laOGFxFlt[lnMonthPos,1]) $ lcRpExp
    DO lpGenExpr           && get the Date expression out from lcRpExp
  ENDIF
    
  SELECT InvLine
  SET RELATION TO Invoice INTO InvHdr ADDITIVE
  
  SELECT OrdLine
  SET RELATION TO Style INTO Style ADDITIVE
*  SET RELATION TO Order + STR(LineNo,6) INTO InvLine ADDITIVE
  SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO OrdCanLn ADDITIVE
  *B606705,1 ASH 12/04/2002 (Begin) Remove the EDI order row and add Entered and cancelled amount.
  *STORE 0 TO lnEnterOrd , lnCanclOrd , lnEDIOrder
  STORE 0     TO lnEnterOrd , lnCanclOrd 
  STORE 0.00 TO lnEnterAmt, lnCanclAmt
  *B606705,1 ASH 12/04/2002 (End)
  lcGroupBy = IIF(lcRpGroup="S","cDesc",IIF(lcRpGroup="Z","cSeason",IIF(lcRpGroup="D","cDivsion","")))
  *B605927,4 SSH Remove the deleting setting
  *lcSetDelet = SET('DELETE')
  *SET DELETE OFF
  *B605927,4 SSH Remove the deleting setting
  DO lpCollData            && Collect the data for report.
  *B605927,4 SSH Remove the deleting setting
  *SET DELETE &lcSetDelet   && Restore Delete settings.
  *B605927,4 SSH Remove the deleting setting
  SELECT InvLine
  SET RELATION TO 

  SELECT OrdHdr     && New lines
  SET RELATION TO   && New Lines
  
  SELECT OrdLine
  SET RELATION TO
ENDIF
*-- Endif of Filter change.  

SELECT (lcWorkFile)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lcOrder
lcOrder = ''     

*-- If Temp. Account file is used.
IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6])>0
  SELECT OrdHdr       && new lines
  SET RELATION TO cOrdType + Order INTO OrdLine   && new lines

  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO Account INTO OrdHdr ADDITIVE

  SCAN
    SELECT OrdHdr
    SCAN REST WHILE Account = EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account')
      *B605927,1 TMI [Start] Include completed orders to see the related ivoices
      *IF OrdHdr.Status $ 'OH'  && new lines
      IF OrdHdr.Status $ 'OHC'
        *B605927,1 TMI [End  ] Include completed orders to see the related ivoices
        SELECT OrdLine
        *B606705,1 ASH 12/04/2002 (Begin) Only get Type 'O', no need for the EDI orders.
        *SCAN REST WHILE cOrdType + Order = OrdHdr.cOrdType + OrdHdr.Order FOR &lcRpExp
        SCAN REST WHILE cOrdType + Order = 'O' + OrdHdr.Order FOR &lcRpExp
        *B606705,1 ASH 12/04/2002 (End)
          WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT
          DO lpInsrtLin             && Insert Lines into Temp file.        
        ENDSCAN
      ENDIF  
      *B606705,1 ASH 12/04/2002 (Begin) Compute the cancelled qty.
      IF OrdHdr.Status = "X" AND OrdHdr.Bulk <> 'Y' AND (&lcComplExp OR &lcStartExp)
        lnCanclOrd = lnCanclOrd + OrdHdr.Book
        lnCanclAmt = lnCanclAmt + OrdHdr.cancelamt
      ENDIF
      *B606705,1 ASH 12/04/2002 (End)
    ENDSCAN
  ENDSCAN  

  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO
ELSE           && Else user did not select Account.

  SELECT OrdLine
  SET RELATION TO Account + cOrdType + Order INTO OrdHdr ADDITIVE   && New lines  
  *B605927,1 TMI [Start] Include completed orders to see the related ivoices
  *SCAN FOR cOrdType + Order + STR(LineNo,6) = '' AND &lcRpExp AND OrdHdr.Status $ 'OH'
  *B606705,1 ASH 12/04/2002 (Begin) Only get Type 'O', no need for the EDI orders.
  =SEEK('O')
  SCAN REST WHILE cOrdType + Order + STR(LineNo,6) = 'O' FOR &lcRpExp AND OrdHdr.Status $ 'OHC'
  *B606705,1 ASH 12/04/2002 (End)
  *B605927,1 TMI [End  ] Include completed orders to see the related ivoices
    WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT
    DO lpInsrtLin             && Insert Lines into Temp file.       
    *B606705,1 ASH 12/04/2002 (Begin) Compute the cancelled qty.
    IF OrdHdr.Status = "X" AND OrdHdr.Bulk <> 'Y' AND (&lcComplExp OR &lcStartExp)
      lnCanclOrd = lnCanclOrd + OrdHdr.Book
      lnCanclAmt = lnCanclAmt + OrdHdr.cancelamt
    ENDIF
    *B606705,1 ASH 12/04/2002 (End)
  ENDSCAN        
ENDIF
** If the invoice calculation based on total charge.
IF lcRpShpAmt = 'T'
  =lfShip()
ENDIF

*-- Endif of Temp. Account file empty.
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpInsrtLin
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : Insert a line into Temp work File.
*!**************************************************************************
*! Example   : DO lpInsrtLin.
*!**************************************************************************
*
PROCEDURE lpInsrtLin
PRIVATE lcAlias , llEntered , llInvIncld

lcAlias = ALIAS()
STORE .F. TO llEntered , llInclude , llInvIncld

*B605927,4 SSH Remove the deleting setting
*IF &lcStartExp
IF &lcStartExp .AND. OrdHdr.Status $ 'OH'
*B605927,4 SSH Remove the deleting setting
  llInclude = .T.
  *B606705,1 ASH 12/04/2002 (Begin) Comment this and compute it after inserting line.  
  *IF OrdHdr.Status = "X"
  *  llEntered = .T.
  *  lnCanclOrd = lnCanclOrd + OrdHdr.Book
  *ENDIF
  *B606705,1 ASH 12/04/2002 (End)
  m.cDivision = OrdHdr.cDivision
  m.cSeason   = OrdLine.Season
  m.cStyGroup = Style.cStyGroup
  m.cDesc     = Style.Desc
  m.cMonth    = PADL(ALLTRIM(STR(MONTH(OrdLine.Start),2)),2,"0")
  m.dCurrDate = OrdLine.Start
  m.cYear     = STR(YEAR(OrdLine.Start),4)  
  m.nStartQty = IIF(&lcStartExp,TotQty,0)
  m.nStartAmt = IIF(&lcStartExp,TotQty,0) * OrdLine.Price

  IF SEEK(&lcKeyValue + m.cYear + m.cMonth,lcWorkFile)
    SELECT (lcWorkFile)
    REPLACE nStartQty WITH nStartQty + m.nStartQty ,;      
            nStartAmt WITH nStartAmt + m.nStartAmt
  ELSE
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF  
ENDIF

SELECT (lcAlias)
*B605927,4 SSH Remove the deleting setting
*IF &lcComplExp. 
IF &lcComplExp. .AND. OrdHdr.Status $ 'OH'
*B605927,4 SSH Remove the deleting setting

  llInclude = .T.
  *B606705,1 ASH 12/04/2002 (Begin) Comment this and compute it after inserting line.  
  *IF OrdHdr.Status = "X" AND !llEntered
  *  lnCanclOrd = lnCanclOrd + OrdHdr.Book
  *ENDIF
  *B606705,1 ASH 12/04/2002 (End)
  m.cDivision = OrdHdr.cDivision
  m.cSeason   = OrdLine.Season
  m.cStyGroup = Style.cStyGroup
  m.cDesc     = Style.Desc
  m.cMonth    = PADL(ALLTRIM(STR(MONTH(OrdLine.Complete),2)),2,"0")
  m.dCurrDate = OrdLine.Complete
  m.cYear     = STR(YEAR(OrdLine.Complete),4)  
  m.nComplQty = IIF(&lcComplExp,TotQty,0)
  m.nComplAmt = IIF(&lcComplExp,TotQty,0) * OrdLine.Price

  IF SEEK(&lcKeyValue + m.cYear + m.cMonth,lcWorkFile)
    SELECT (lcWorkFile)
    REPLACE nComplQty WITH nComplQty + m.nComplQty ,;           
            nComplAmt WITH nComplAmt + m.nComplAmt
  ELSE
    *B606705,1 ASH 12/04/2002 (Begin) Zero this fields so that it will not saved with it's old values.
    m.nStartQty = 0
    m.nStartAmt = 0
    *B606705,1 ASH 12/04/2002 (End)
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF  
ENDIF

IF &lcEnterExp. AND !(OrdHdr.Order == lcOrder)
  lcOrder = OrdHdr.Order
  
  *-- If type of order is EDI Order.
  *B606705,1 ASH 12/04/2002 (Begin) Comment this lines, as no need to print the EDI orders.
  *IF OrdHdr.cOrdType = "T"
  *  IF !EMPTY(OrdHdr.cFromOrder)
  *    lnEDIOrder = lnEDIOrder + OrdHdr.Book
  *  ENDIF  
  *ELSE
  *B606705,1 ASH 12/04/2002 (End)
  IF OrdHdr.cOrdType = "O"
    lnEnterOrd = lnEnterOrd + OrdHdr.Book  
    lnEnterAmt = lnEnterAmt + OrdHdr.BookAmt
  ENDIF  
ENDIF

IF &lcCanclExp. 
  lnCanclOrd = lnCanclOrd + OrdCanLn.TotQty
  lnCanclAmt = lnCanclAmt + (OrdCanLn.TotQty*OrdCanLn.Price)
ENDIF

*B606705,1 ASH 12/04/2002 (Begin)Remove the relation between ordline and invline and work with scan concept to 
*B606705,1                       avoid the problem of more than an order has more than one invoice and the first one is Voided.

IF lcRpShpAmt <> 'T'
  IF SEEK(OrdLine.Order + STR(OrdLine.LineNo,6),'InvLine')
    SELECT InvLine
    SCAN REST WHILE Order + STR(LineNo,6) = OrdLine.Order + STR(OrdLine.LineNo,6)
      IF !EOF('InvHdr') AND InvHdr.Status <> 'V' AND llInclude
        IF SEEK(STR(YEAR(InvLine.InvDate),4) + PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0"),lcTmpYears)
          SELECT InvLine
          llInvIncld = .T.
          IF SEEK(&lcKeyValue + STR(YEAR(InvLine.InvDate),4) + PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0"),lcWorkFile)
            SELECT (lcWorkFile)
            REPLACE nComplQty WITH nComplQty + 0 ,;
                    nComplAmt WITH nComplAmt + 0
          ELSE
            m.cDivision = InvHdr.cDivision
            m.cSeason   = InvLine.Season
            m.cStyGroup = Style.cStyGroup
            m.cDesc     = Style.Desc
            m.cMonth    = PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0")
            m.dCurrDate = InvLine.InvDate
            m.cYear     = STR(YEAR(InvLine.InvDate),4)    
            STORE 0 TO m.nStartQty , m.nStartAmt , m.nComplQty , m.nComplAmt
            INSERT INTO (lcWorkFile) FROM MEMVAR
          ENDIF  
          IF SEEK(&lcKeyValue + STR(YEAR(InvLine.InvDate),4) + PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0"),lcTmpDet)
            SELECT (lcTmpdet)
            REPLACE nShipQty WITH nShipQty + InvLine.TotQty 
            REPLACE nShipAmt WITH nShipAmt + (InvLine.TotQty * InvLine.Price)
          ELSE
            m.cDivision = OrdHdr.cDivision
            m.cSeason   = OrdLine.Season
            m.cDesc     = Style.Desc
            m.cMonth    = PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0")
            m.cYear     = STR(YEAR(InvLine.InvDate),4)
            m.nShipQty = InvLine.TotQty
            m.nShipAmt = InvLine.TotQty * InvLine.Price
            INSERT INTO (lcTmpDet) FROM MEMVAR
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF

  SELECT InvLine
  =SEEK(OrdLine.Order + STR(OrdLine.LineNo,6))
  SCAN REST WHILE Order + STR(LineNo,6) = OrdLine.Order + STR(OrdLine.LineNo,6)
    IF !EOF('InvHdr') AND InvHdr.Status <> 'V' AND !llInvIncld AND &lcInvLnExp
      m.cDivision = OrdHdr.cDivision
      m.cSeason   = OrdLine.Season
      m.cDesc     = Style.Desc
      IF SEEK(STR(YEAR(InvLine.InvDate),4) + PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0"),lcTmpYears) AND ;
        &lcTmpYears..lMainYear
        IF SEEK(&lcKeyValue + STR(YEAR(InvLine.InvDate),4) + PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0"),lcWorkFile)
          SELECT (lcWorkFile)
          REPLACE nComplQty WITH nComplQty + 0 ,;
                  nComplAmt WITH nComplAmt + 0
        ELSE
          m.cDivision = InvHdr.cDivision
          m.cSeason   = InvLine.Season
          m.cStyGroup = Style.cStyGroup
          m.cDesc     = Style.Desc
          m.cMonth    = PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0")
          m.dCurrDate = InvLine.InvDate
          m.cYear     = STR(YEAR(InvLine.InvDate),4)
          STORE 0 TO m.nStartQty , nStartAmt , m.nComplQty , m.nComplAmt
          INSERT INTO (lcWorkFile) FROM MEMVAR
        ENDIF  
      ENDIF
    
      IF SEEK(&lcKeyValue + STR(YEAR(InvLine.InvDate),4) + PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0"),lcTmpDet)
        SELECT (lcTmpdet)
        REPLACE nShipQty WITH nShipQty + InvLine.TotQty
        REPLACE nShipAmt WITH nShipAmt + (InvLine.TotQty * InvLine.Price)
      ELSE
        m.cDivision = OrdHdr.cDivision
        m.cSeason   = OrdLine.Season
        m.cDesc     = Style.Desc
        m.cMonth    = PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0")
        m.cYear     = STR(YEAR(InvLine.InvDate),4)
        m.nShipQty = InvLine.TotQty
        m.nShipAmt = InvLine.TotQty * InvLine.Price
        INSERT INTO (lcTmpDet) FROM MEMVAR
      ENDIF
    ENDIF
  ENDSCAN  
ENDIF
*B606705,1 ASH 12/04/2002 (End)
SELECT (lcAlias)        
*-- End of lpInsrtLin.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnMonthPos = lfItmPos('ORDLINE.START') && get Style Group Fixed filter Position.
lnAcctPos = lfItmPos('ORDHDR.ACCOUNT')   && get Account Fixed filter Position.
lnDatePos = lfItmPos('ORDHDR.ENTERED')   && get Date Fixed filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : To get the position of the fixed filter in OG.
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
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
PRIVATE lcPerFrom , lcPerTo , lcLoop

*-- Create a temporary table to hold data which will be displayed in report.
CREATE TABLE (gcWorkDir+lcWorkFile) (cDivision C(6) , cSeason C(6) , cStyGroup C(6) ,;
              dCurrDate D(8) , cMonth C(2) , cYear C(4) , cDesc C(20) , nStartQty N(15) , ;
              nStartAmt N(15,2) , nComplQty N(15) , nComplAmt N(15,2))
DO CASE
  CASE lcRpGroup = "S"
    lcKeyValue = 'm.cDesc'
    INDEX ON cDesc + cYear + cMonth TAG (lcWorkFile)
  CASE lcRpGroup = "Z"
    lcKeyValue = 'm.cSeason'
    INDEX ON cSeason  + cYear + cMonth TAG (lcWorkFile)
  CASE lcRpGroup = "D"
    lcKeyValue = 'm.cDivision'
    INDEX ON cDivision + cYear + cMonth TAG (lcWorkFile)
  CASE lcRpGroup = "N"
    lcKeyValue = 'SPACE(0)'
    INDEX ON cYear + cMonth TAG (lcWorkFile)     
ENDCASE

*-- Create a temporary table to only all the years and months used.
CREATE TABLE (gcWorkDir+lcTmpYears) (cYear C(4) , cMonth C(2) , lMainYear L(1))
INDEX ON cYear + cMonth TAG (lcTmpYears)

*-- Adding records for the current year in Temp. Years file.
lcPerFrom = STRTRAN(SUBSTR(laOGFxFlt[lnMonthPos,6],1,7),"-","")   && Remove the hyphen "-"
lcPerTo   = STRTRAN(SUBSTR(laOGFxFlt[lnMonthPos,6],9,15),"-","")  && Remove the hyphen "-"  
lcLoop = lcPerFrom
DO WHILE lcLoop <= lcPerTo
  m.cYear = SUBSTR(lcLoop,1,4)
  m.cMonth = PADL(ALLTRIM(SUBSTR(lcLoop,5,2)),2,"0")
  m.lMainYear = .T.
  IF VAL(SUBSTR(lcLoop,5,2)) = 12
    lcLoop = STR(VAL(SUBSTR(lcLoop,1,4))+1,4) + "01"
  ELSE
    lcLoop = SUBSTR(lcLoop,1,4) + PADL(ALLTRIM(STR(VAL(SUBSTR(lcLoop,5,2))+1,2)),2,"0")
  ENDIF
  
  IF !SEEK(m.cYear + m.cMonth,lcTmpYears)
    INSERT INTO (lcTmpYears) FROM MEMVAR
  ENDIF   
ENDDO

*-- Adding records for the previous year in Temp. Years file.
lcPerFrom = STR(VAL(SUBSTR(lcPerFrom,1,4))-1,4) + SUBSTR(lcPerFrom,5,2)
lcPerTo   = STR(VAL(SUBSTR(lcPerTo,1,4))-1,4) + SUBSTR(lcPerTo,5,2)
lcLoop = lcPerFrom
DO WHILE lcLoop <= lcPerTo
  m.cYear = SUBSTR(lcLoop,1,4)
  m.cMonth = PADL(ALLTRIM(SUBSTR(lcLoop,5,2)),2,"0")
  m.lMainYear = .F. 
  IF VAL(SUBSTR(lcLoop,5,2)) = 12
    lcLoop = STR(VAL(SUBSTR(lcLoop,1,4))+1,4) + "01"
  ELSE  
    lcLoop = SUBSTR(lcLoop,1,4) + PADL(ALLTRIM(STR(VAL(SUBSTR(lcLoop,5,2))+1,2)),2,"0")
  ENDIF
  
  IF !SEEK(m.cYear + m.cMonth,lcTmpYears)
    INSERT INTO (lcTmpYears) FROM MEMVAR
  ENDIF   
ENDDO

*-- Adding records for the year before previous year in Temp. Years file.
lcPerFrom = STR(VAL(SUBSTR(lcPerFrom,1,4))-1,4) + SUBSTR(lcPerFrom,5,2)
lcPerTo   = STR(VAL(SUBSTR(lcPerTo,1,4))-1,4) + SUBSTR(lcPerTo,5,2)
lcLoop = lcPerFrom
DO WHILE lcLoop <= lcPerTo
  m.cYear = SUBSTR(lcLoop,1,4)
  m.cMonth = PADL(ALLTRIM(SUBSTR(lcLoop,5,2)),2,"0")
  m.lMainYear = .F.
  IF VAL(SUBSTR(lcLoop,5,2)) = 12
    lcLoop = STR(VAL(SUBSTR(lcLoop,1,4))+1,4) + "01"
  ELSE
    lcLoop = SUBSTR(lcLoop,1,4) + PADL(ALLTRIM(STR(VAL(SUBSTR(lcLoop,5,2))+1,2)),2,"0")
  ENDIF
  
  IF !SEEK(m.cYear + m.cMonth,lcTmpYears)
    INSERT INTO (lcTmpYears) FROM MEMVAR
  ENDIF   
ENDDO

*-- Create a temporary table to only all the years and months used.
CREATE TABLE (gcWorkDir+lcTmpDet) (cYear C(4) , cMonth C(2) , cDivision C(6) ,;
              cSeason C(6) , cDesc C(20) , nShipQty N(15) , nShipAmt N(15,2)) 
DO CASE
  CASE lcRpGroup="S"
    INDEX ON cDesc + cYear + cMonth TAG (lcTmpDet)
  CASE lcRpGroup="Z"
    INDEX ON cSeason  + cYear + cMonth TAG (lcTmpDet)
  CASE lcRpGroup="D"
    INDEX ON cDivision + cYear + cMonth TAG (lcTmpDet)
  CASE lcRpGroup = "N"
    INDEX ON cYear + cMonth TAG (lcTmpDet)     
ENDCASE
*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : Generate Expression
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.
=ACOPY(laBrFldFlt,laBrTmpFlt)        && Copy Browse Filter Array to Temp Array.

*-- If user selected Account.
IF ALLTRIM(laTempExpr[lnAcctPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnAcctPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Account expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnAcctPos,1),7)
  
  *-- Generate expression for Account.
  PRIVATE lcAcctExp
  lcAcctExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Account in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcAcctExp,".T.")
ENDIF  
*-- EndIf of user selected Account.

*-- If user entered date .
IF ALLTRIM(laTempExpr[lnDatePos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Date expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnDatePos,1),7)
  
  *-- Generate expression for Date.
  lcEnterExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Date from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcEnterExp,".T.")  
  
  *-- Generate expression for Cancelled date.
  lcCanclExp = STRTRAN(lcEnterExp,"ORDHDR.ENTERED","ORDCANLN.CANCELLED")
ENDIF  
*-- EndIf of user selected Date.

*-- If user selected Month Range.
IF ALLTRIM(laTempExpr[lnMonthPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnMonthPos,1),7)
  
  *-- Generate expression for Month Range.
  lcStartExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Month Range from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcStartExp,".T.")  
  
  *-- we need to generate a new Month Range Expression based on ORDLINE.START.
  PRIVATE lcSetCent , lcPerFrom , lcPerTo , lcFromDate , lcToDate
  lcSetCent = SET('CENT')
  SET CENT ON

  laOGFxFlt[1,3] = "D"
  lcPerFrom = STRTRAN(SUBSTR(laOGFxFlt[1,6],1,7),"-","")   && Remove the hyphen "-"
  lcPerTo   = STRTRAN(SUBSTR(laOGFxFlt[1,6],9,15),"-","")  && Remove the hyphen "-"

  lcFromDate = IIF(SEEK(lcPerFrom,'FsPrd'),DTOC(FsPrd.dFsppBgDt),DTOC({}))
  lcToDate = IIF(SEEK(lcPerTo,'FsPrd'),DTOC(FsPrd.dFsppEnDt),DTOC({}))
  
  laOGFxFlt[1,6] = lcFromDate+"|"+lcToDate

  *-- Generate expression for Month Range.
  lcStartExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  lcComplExp = STRTRAN(lcStartExp,"ORDLINE.START","ORDLINE.COMPLETE")

  lcInvLnExp = STRTRAN(lcStartExp,"ORDLINE.START","INVLINE.INVDATE")

  laOGFxFlt[1,1] = "INVLINE.INVDATE"
  lcFromDate = IIF(SEEK(lcPerFrom,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppBgDt,-12)),DTOC({}))
  lcToDate = IIF(SEEK(lcPerTo,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppEnDt,-12)),DTOC({}))
  laOGFxFlt[1,6] = lcFromDate+"|"+lcToDate
  
  lcInvLnExp = lcInvLnExp + [ OR ] + gfGenFlt('laOGFxFlt',.T.,.T.)
  
  lcFromDate = IIF(SEEK(lcPerFrom,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppBgDt,-24)),DTOC({}))
  lcToDate = IIF(SEEK(lcPerTo,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppEnDt,-24)),DTOC({}))
  laOGFxFlt[1,6] = lcFromDate+"|"+lcToDate

  lcInvLnExp = lcInvLnExp + [ OR ] + gfGenFlt('laOGFxFlt',.T.,.T.)
  
  SET CENT &lcSetCent
ENDIF  
*-- If user selected Month Range.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.
=ACOPY(laBrTmpFlt,laBrFldFlt)        && Restore Browse Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : To save the old value.
*!**************************************************************************
*! Example   : = lfwOldVal()
*!**************************************************************************
*
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- End of lfwOldVal.

*!**************************************************************************
*! Name      : lfvYearPrd
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : To Browse the Year-Period.
*!**************************************************************************
*! Example   : = lfvYearPrd()
*!**************************************************************************
*
FUNCTION lfvYearPrd
PRIVATE lcAlias , llPrdIsUsed
lcAlias  = ALIAS()
lcRpVar   = SYS(18)
&lcRpVar  = ALLTRIM(&lcRpVar)

SELECT FSPRD
IF (AT('?' , &lcRpVar) <> 0) .OR. (!EMPTY(STRTRAN(&lcRpVar , '-')) AND;
   !SEEK(LEFT(&lcRpVar,4) + RIGHT(&lcRpVar , 2) , 'FSPRD'))

  DECLARE laRpRetFld(2)
  lcSavFilds = lcBrFields      && Varible to save the old browse fields
  lcBrFields    = [FSPPRDID=CFISFYEAR+'-'+CFSPPRDID:H="Year-Period",DFSPPBGDT:H="Begining date",dfsppendt:H="Ending date"]

  STORE '' TO laRpRetFld[2]
  =gfBrows('FOR !EMPTY(cFspPrdId)','CFISFYEAR,CFSPPRDID',"laRpRetFld",'Fiscal Periods',.F.)
  
  IF !EMPTY(laRpRetFld[1])
    &lcRpVar = laRpRetFld[1]+'-'+laRpRetFld[2]
  ELSE  
    &lcRpVar = lcOldVal
  ENDIF  

  lcBrFields = lcSavFilds    && Restore the old browse fields 
ENDIF

IF EMPTY(STRTRAN(&lcRpVar,'-'))
  &lcRpVar=''
ENDIF

SELECT (lcAlias)          && Restore Alias.
*-- End of lfvYearPrd.

*!**************************************************************************
*! Name      : lfArrDumy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2001
*! Purpose   : Fill Sort and select arrays
*!**************************************************************************
*! Example   : = lfArrDumy()
*!**************************************************************************
*
*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
DIMENSION laSlctDesc[4,1] , laSlctVal[4,1]

*-- Fill Select by array base elements.
laSlctDesc[1,1] = lcStyMajor
laSlctDesc[2,1] = 'Season'
laSlctDesc[3,1] = 'Divsion'
laSlctDesc[4,1] = 'None'

laSlctVal[1,1]  = 'S'
laSlctVal[2,1]  = 'Z'
laSlctVal[3,1]  = 'D'
laSlctVal[4,1]  = 'N'
*-- End of lfArrDumy.

*!**************************************************************************
*! Name      : lfShip
*! Developer : Ashraf Sherif Mohamed
*! Date      : 12/25/2002
*! Purpose   : Compute ship qty and amount from invhdr in case of invoice calculation=Total Charge.
*!**************************************************************************
*! Example   : =lfShip()
*!**************************************************************************
*Compute the ship amt from InvHdr not Invline to match the sales journal report amount.

FUNCTION lfShip
lcFlt = STRTRAN(lcInvLnExp,'INVLINE','INVHDR')
lcGroup = IIF(lcRpGroup = "D","cDivision","year,month")

*B127260,1 EIH 06/10/2005 Fix the bug of when select division shipped amount wrong [Begin].

*SELECT CTOD(STR(MONTH(InvDate),2)+"\01\"+STR(YEAR(InvDate),4)) as dCurrDate,;
       cDivision,invdate,STR(YEAR(InvDate),4) as Year, STR(MONTH(InvDate),2) as Month,;
       SUM(Ship) as ship,SUM(Totalchg) as Totalchg;
  FROM INVHDR;
  WHERE &lcFlt. and INVHDR.Status <> 'V';
  GROUP BY &lcGroup. INTO Cursor TmpShip

*Set selected divison in lcRpExpDiv varaible.
STORE ' ' TO lcRpExpDiv 
lnDivPos   = lfItmPos('ORDHDR.CDIVISION')
IF lnDivPos > 0 
  DIME laDivFlt[1,7]
  =ACOPY(laOGFxFlt,laDivFlt,((lnDivPos-1)*7)+1,7)
  lcRpExpDiv = gfGenFlt('laDivFlt',.T.,.T.)
  IF !EMPTY(ALLTRIM( lcRpExpDiv))
    lcRpExpDiv = STRTRAN(lcRpExpDiv,'ORDHDR','INVHDR') + ' AND '
  ENDIF  
ENDIF
*Set the division in where condition.
SELECT CTOD(STR(MONTH(InvDate),2)+"\01\"+STR(YEAR(InvDate),4)) as dCurrDate,;
       cDivision,invdate,STR(YEAR(InvDate),4) as Year, STR(MONTH(InvDate),2) as Month,;
       SUM(Ship) as ship,SUM(Totalchg) as Totalchg;
  FROM INVHDR;
  WHERE &lcRpExpDiv  (&lcFlt.)   AND INVHDR.Status <> 'V'  ;
  GROUP BY &lcGroup. INTO Cursor TmpShip
*B127260,1 EIH  [End].


SCAN
  m.cDivision = cDivision
  m.cSeason   = ""
  m.cDesc     = ""
  m.cMonth    = PADL(ALLTRIM(Month),2,"0")
  m.cYear     = Year
  m.nShipQty  = Ship
  m.nShipAmt  = TotalChg
  INSERT INTO (lcTmpDet) FROM MEMVAR 
  m.cStyGroup = ""
  m.dCurrDate = dCurrDate
  IF SEEK(m.cYear + m.cMonth,lcTmpYears) AND &lcTmpYears..lMainYear AND !SEEK(m.cYear+m.cMonth,lcWorkFile)
    INSERT INTO (lcWorkFile) FROM MEMVAR 
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/22/2002
*! Purpose   : Function used to suppressing the field in the grid.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfClrRead()
*!*************************************************************
FUNCTION lfClrRead
CLEAR READ




*B127260,1 EIH 06/10/2005 Fix the bug of when select division shipped amount wrong [Begin].
*!******************************************************************************************************
*! Name      : lfItmPos
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate fixed filter position within array.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : ...
*!******************************************************************************************************
*! Returns            : Position
*!******************************************************************************************************
*! Example   : = lfItmPos()
*!******************************************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*B127260,1 EIH [End].
