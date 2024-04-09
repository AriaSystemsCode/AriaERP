*:**************************************************************************
*: Program file  : SOSLSMON
*: Program desc. : Order/Sales Summary Report By Month (Copied from C102355 for FRE20)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ashraf Sherif (ASH)
*: Date          : 12/18/2002
*: Reference     : N000493
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExpr , lpCollData , lpInsrtLin
*: Functions     : lfwRepWhen , lfItmPos , lfsrAcc
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*Modifications
*:B606801,1 BWA 1/2/2003 Change the header due to te currency.[FIX IN FRX]
*:B607090,1 ABD 04/21/2003 Fix more than one problem as following:-
*:B607090,1 ABD 1- Can the style code be added to this report either before or after 
*:B607090,1 ABD The style desc -as many clients use generic descriptions and need 
*:B607090,1 ABD CSTYMAJOR field printed to be able to recognise the product.
*:B607090,1 ABD 2- The ship amount on the report is including the Tax amount when it should be Net.
*:B607090,1 ABD 3- If you select to group by Style - it is grouping by Style 
*:B607090,1 ABD Description and not Cstymajor
*:B121575,1 NNA 09/02/2004 Fix Bug of showing incorrect information that by using 'cStymajor' 
*:B121575,1 NNA            instead of 'cDesc' in [Soslsmon.prg]  & [Soslsmon.Frx]
*:B607884,1 TMI 12/18/2006 Shipped qty is shown as 0 , this is due to not updating the frx when the bug B121575 made ( frx only )
*:B608099,1 NNA 05/27/2007 Fix bug that there is difference if you print invoice calculation by Shipped amount and total charges
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

*B608099,1 NNA 05/27/2007 (Begin) Open the Consinvl to get Consolidated invoices
IF !USED('CONSINVL')
  =gfOpenFile(gcDataDir+'CONSINVL',gcDataDir+'CINVLINE','SH')
ENDIF
IF lcRpShpAmt<>lcOldInv
  llOGFltCh = .T.
ENDIF
*B608099,1 NNA (End)

*B607090,1 ABD -  Set the default to ship Amount in case we change group by from None to any
*B607090,1 ABD -  Other type of the group, this bug let the program recalculate the
*B607090,1 ABD -  Data 2 times for ship amount time and total charge at the same time. [Begin]

 *B608099,1 NNA 05/27/2007 (BEGIN) Comment this line because it changes lcRpShpAmt to (S) even if I choose total charge (T)
 *lcRpShpAmt = IIF(lcRpGroup='N',lcRpShpAmt,'S')
 *B608099,1 NNA (END)

*B607090,1 ABD - [End]
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

  *B608099,1 NNA 05/27/2007 (Begin) Initialiaze relation between Invhdr and Consinvl
  SELECT CONSINVL
  SET RELATION OFF INTO INVHDR
  SET RELATION TO Invoice INTO INVHDR ADDITIVE
  *B608099,1 NNA (End)
  
  SELECT OrdLine
  SET RELATION TO Style INTO Style ADDITIVE
  SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO OrdCanLn ADDITIVE
  STORE 0     TO lnEnterOrd , lnCanclOrd 
  STORE 0.00 TO lnEnterAmt, lnCanclAmt
  *B121575,1 NNA 09/02/2004 (Begin) Group By 'cstymajor' instead of 'cDesc'
  *lcGroupBy = IIF(lcRpGroup="S","cDesc",IIF(lcRpGroup="Z","cSeason",IIF(lcRpGroup="D","cDivsion","")))
  lcGroupBy = IIF(lcRpGroup="S","cstymajor",IIF(lcRpGroup="Z","cSeason",IIF(lcRpGroup="D","cDivsion","")))  
  *B121575,1 NNA (END)

  DO lpCollData            && Collect the data for report.

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
  *B607090,1 ABD - Set order to cstymajor before show the report. [Begin]
  *DO gfDispRe WITH EVAL('lcRpName')
  *lcOldOrder = ORDER()
  SET ORDER TO (lcWorkFile)
  DO gfDispRe WITH EVAL('lcRpName')
  *SET ORDER TO &lcOldOrder
  *B607090,1 ABD - [End]
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
      IF OrdHdr.Status $ 'OHC'
        SELECT OrdLine
        SCAN REST WHILE cOrdType + Order = 'O' + OrdHdr.Order FOR &lcRpExp
          WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT
          DO lpInsrtLin             && Insert Lines into Temp file.        
        ENDSCAN
      ENDIF  
      IF OrdHdr.Status = "X" AND OrdHdr.Bulk <> 'Y' AND (&lcComplExp OR &lcStartExp)
        lnCanclOrd = lnCanclOrd + OrdHdr.Book
        lnCanclAmt = lnCanclAmt + OrdHdr.cancelamt
      ENDIF
    ENDSCAN
  ENDSCAN  

  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO
ELSE           && Else user did not select Account.
  SELECT OrdLine
  SET RELATION TO Account + cOrdType + Order INTO OrdHdr ADDITIVE   && New lines  
  =SEEK('O')
  SCAN REST WHILE cOrdType + Order + STR(LineNo,6) = 'O' FOR &lcRpExp AND OrdHdr.Status $ 'OHC'
    WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT
    DO lpInsrtLin             && Insert Lines into Temp file.       
    IF OrdHdr.Status = "X" AND OrdHdr.Bulk <> 'Y' AND (&lcComplExp OR &lcStartExp)
      lnCanclOrd = lnCanclOrd + OrdHdr.Book
      lnCanclAmt = lnCanclAmt + OrdHdr.cancelamt
    ENDIF
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

IF &lcStartExp .AND. OrdHdr.Status $ 'OH'
  llInclude = .T.
  m.cDivision = OrdHdr.cDivision
  m.cSeason   = OrdLine.Season
  m.cStyGroup = Style.cStyGroup
  m.cDesc     = Style.Desc

  *B607090,1 ABD - Assigne the style Major to save at the temp file, this will help
  *B607090,1 ABD - us to print the report index on style major. [Begin]
  M.cStyMajor = Style.cStyMajor
  *B607090,1 ABD - [End]
  
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
IF &lcComplExp. .AND. OrdHdr.Status $ 'OH'
  llInclude = .T.
  m.cDivision = OrdHdr.cDivision
  m.cSeason   = OrdLine.Season
  m.cStyGroup = Style.cStyGroup
  m.cDesc     = Style.Desc

  *B607090,1 ABD - Assigne the style Major to save at the temp file, this will help
  *B607090,1 ABD - us to print the report index on style major. [Begin]
  M.cStyMajor = Style.cStyMajor
  *B607090,1 ABD - [End]
  
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
    m.nStartQty = 0
    m.nStartAmt = 0
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF  
ENDIF

IF &lcEnterExp. AND !(OrdHdr.Order == lcOrder)
  lcOrder = OrdHdr.Order
  IF OrdHdr.cOrdType = "O"
    lnEnterOrd = lnEnterOrd + OrdHdr.Book  
    lnEnterAmt = lnEnterAmt + OrdHdr.BookAmt
  ENDIF  
ENDIF

IF &lcCanclExp. 
  lnCanclOrd = lnCanclOrd + OrdCanLn.TotQty
  lnCanclAmt = lnCanclAmt + (OrdCanLn.TotQty*OrdCanLn.Price)
ENDIF

*B608099,1 NNA 05/27/2007 (Begin) Check if order's invoice is consolidated or not , if it is then get shipped amount from 
*B608099,1 NNA            Consinvl Table instead of invline
*IF lcRpShpAmt <> 'T'
llConsol = SEEK(OrdLine.Order,'Consinvl') AND INVHDR.CONSOL='Y'
lcInvoiceNo = ''
IF lcRpShpAmt <> 'T' AND llConsol
  lcConLnExp = STRTRAN(lcInvLnExp,"INVLINE.INVDATE","CONSINVL.INVDATE")
  DO lpInsrtCon			&& Get the consolidated lines
ENDIF
IF SEEK(OrdLine.Order + STR(OrdLine.LineNo,6),'InvLine')
   SELECT InvLine
   SCAN REST WHILE Order+STR(LineNo,6)+Invoice = OrdLine.Order + STR(OrdLine.LineNo,6)
     IF !(INVLINE.INVOICE==lcInvoiceNo)
       llConsol = .F.
     ENDIF
   ENDSCAN
ENDIF
IF lcRpShpAmt <> 'T' AND !llConsol
*B608099,1 NNA (End)

  IF SEEK(OrdLine.Order + STR(OrdLine.LineNo,6),'InvLine')
    SELECT InvLine
    SCAN REST WHILE Order + STR(LineNo,6) = OrdLine.Order + STR(OrdLine.LineNo,6)

      *B608099,1 NNA 05/27/2007 (Begin) if the line in progress now is refers to a consolidated invoice then skip this line
      IF INVHDR.CONSOL='Y'
        LOOP
      ENDIF
      *B608099,1 NNA 05/27/2007 (End)
      
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

            *B607090,1 ABD - Assigne the style Major to save at the temp file, this will help
            *B607090,1 ABD - us to print the report index on style major. [Begin]
            M.cStyMajor = Style.cStyMajor
           *B607090,1 ABD - [End]
            
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

            *B607090,1 ABD - Assigne the style Major to save at the temp file, this will help
            *B607090,1 ABD - us to print the report index on style major. [Begin]
            M.cStyMajor = Style.cStyMajor
           *B607090,1 ABD - [End]
            
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

      *B608099,1 NNA 05/27/2007 (Begin) if the line in progress now is refers to a consolidated invoice then skip this line
      IF INVHDR.CONSOL='Y'
        LOOP
      ENDIF
      *B608099,1 NNA 05/27/2007 (End)
      
    IF !EOF('InvHdr') AND InvHdr.Status <> 'V' AND !llInvIncld AND &lcInvLnExp
      m.cDivision = OrdHdr.cDivision
      m.cSeason   = OrdLine.Season
      m.cDesc     = Style.Desc

      *B607090,1 ABD - Assigne the style Major to save at the temp file, this will help
      *B607090,1 ABD - us to print the report index on style major. [Begin]
      M.cStyMajor = Style.cStyMajor
      *B607090,1 ABD - [End]
      
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

          *B607090,1 ABD - Assigne the style Major to save at the temp file, this will help
          *B607090,1 ABD - us to print the report index on style major. [Begin]
          M.cStyMajor = Style.cStyMajor
          *B607090,1 ABD - [End]
          
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
        *B607090,1 ABD - Assigne the style Major to save at the temp file, this will help
        *B607090,1 ABD - us to print the report index on style major. [Begin]
        M.cStyMajor = Style.cStyMajor
        *B607090,1 ABD - [End]
        
        m.cMonth    = PADL(ALLTRIM(STR(MONTH(InvLine.InvDate),2)),2,"0")
        m.cYear     = STR(YEAR(InvLine.InvDate),4)
        m.nShipQty = InvLine.TotQty
        m.nShipAmt = InvLine.TotQty * InvLine.Price
        INSERT INTO (lcTmpDet) FROM MEMVAR
      ENDIF
    ENDIF
  ENDSCAN  
ENDIF
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
*B607090,1 ABD - Add cstymajor field to the temp file. [Begin]
*CREATE TABLE (gcWorkDir+lcWorkFile) (cDivision C(6) , cSeason C(6) , cStyGroup C(6) ,;
*              dCurrDate D(8) , cMonth C(2) , cYear C(4) , cDesc C(20) , nStartQty N(15) , ;
*              nStartAmt N(15,2) , nComplQty N(15) , nComplAmt N(15,2))

CREATE TABLE (gcWorkDir+lcWorkFile) (cDivision C(6) , cSeason C(6) , cStyGroup C(6) ,;
              dCurrDate D(8) , cMonth C(2) , cYear C(4) , cDesc C(20) , nStartQty N(15) , ;
              nStartAmt N(15,2) , nComplQty N(15) , nComplAmt N(15,2), cStyMajor C(19))
*B607090,1 ABD - [End]
DO CASE
  CASE lcRpGroup = "S"
    *B121575,1 NNA 09/02/2004 (Begin) use 'cStyMajor' instead of 'cDesc'
    *lcKeyValue = 'm.cDesc'
    *INDEX ON cDesc + cYear + cMonth TAG (lcWorkFile)
    *B607090,1 ABD - Add New Index for the temp file. [Begin]
    *INDEX ON cStyMajor + cDesc + cYear + cMonth TAG 'lcWorkFile' ADDITIVE
    lcKeyValue = 'm.cstymajor'
    INDEX ON cStyMajor + cYear + cMonth TAG (lcWorkFile)
    *B121575,1 NNA (END)
    SET ORDER TO (lcWorkFile)
    *B607090,1 ABD - [End]
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
*B121575,1 NNA 09/02/2004 (Begin) Add new field (cStymajor) to the temp file and add it to the index
*CREATE TABLE (gcWorkDir+lcTmpDet) (cYear C(4) , cMonth C(2) , cDivision C(6) ,;
              cSeason C(6) , cDesc C(20) , nShipQty N(15) , nShipAmt N(15,2)) 
CREATE TABLE (gcWorkDir+lcTmpDet) (cYear C(4) , cMonth C(2) , cDivision C(6) ,;
              cSeason C(6) , cDesc C(20) , nShipQty N(15) , nShipAmt N(15,2), cStyMajor C(19))
*B121575,1 NNA (END)
DO CASE
  CASE lcRpGroup="S"
*B121575,1 NNA 09/02/2004 (Begin) use 'cStyMajor' instead of 'cDesc'
    *INDEX ON cDesc + cYear + cMonth TAG (lcTmpDet)
    INDEX ON cStyMajor + cYear + cMonth TAG (lcTmpDet)    
*B121575,1 NNA (END)
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
*B607090,1 ABD - Add account Filed to the select statment , this will handle the 
*B607090,1 ABD - account filter , this bug is new & not include at the bug description before. [Begin]
*SELECT CTOD(STR(MONTH(InvDate),2)+"\01\"+STR(YEAR(InvDate),4)) as dCurrDate,;
*       cDivision,invdate,STR(YEAR(InvDate),4) as Year, STR(MONTH(InvDate),2) as Month,;
*       SUM(Ship) as ship,SUM(Totalchg) as Totalchg;
*  FROM INVHDR;
*  WHERE &lcFlt. and INVHDR.Status <> 'V';
*  GROUP BY &lcGroup. INTO Cursor TmpShip

*B608099,1 NNA 05/27/2007 (Begin) Check if user choose one or more accounts to get data for them or for all accounts to 
*B608099,1 NNA            Change Select statment and also add conditions to search about seasons and divisions
*SELECT Account,CTOD(STR(MONTH(InvDate),2)+"\01\"+STR(YEAR(InvDate),4)) as dCurrDate,;
        cDivision,invdate,STR(YEAR(InvDate),4) as Year, STR(MONTH(InvDate),2) as Month,;
        SUM(Ship) as ship,SUM(Totalchg) as Totalchg;
    FROM INVHDR;
    WHERE &lcFlt. .AND. INVHDR.Status <> 'V';
    GROUP BY &lcGroup. INTO Cursor TmpShip
llWorkWith = !EMPTY(laOGFxFlt[lnAcctPos,6]) .AND. USED(laOGFxFlt[lnAcctPos,6]) .AND. RECCOUNT(laOGFxFlt[lnAcctPos,6]) >= 1  
IF llWorkWith
  lcFlt = STRTRAN(lcFlt,'INVHDR','A')
  SELECT A.Account,CTOD(STR(MONTH(A.InvDate),2)+"\01\"+STR(YEAR(A.InvDate),4)) as dCurrDate,;
         A.cDivision,A.invdate,STR(YEAR(A.InvDate),4) as Year, STR(MONTH(A.InvDate),2) as Month,;
         SUM(A.Ship) as ship,SUM(A.Totalchg) as Totalchg;
    FROM INVHDR A ;
    WHERE EXISTS(SELECT * FROM laOGFxFlt[lnAcctPos,6] B WHERE A.ACCOUNT=B.ACCOUNT) .AND. ;
          IIF(!EMPTY(laOGFxFlt[3,6]),INLIST(A.cDivision,laOGFxFlt[3,6]),.T.) .AND. ;
          IIF(!EMPTY(laOGFxFlt[4,6]),INLIST(A.SEASON,laOGFxFlt[4,6]),.T.) .AND. ;
          (&lcFlt.) .AND. A.Status <> 'V';
    GROUP BY &lcGroup. INTO Cursor TmpShip
ELSE
  SELECT Account,CTOD(STR(MONTH(InvDate),2)+"\01\"+STR(YEAR(InvDate),4)) as dCurrDate,;
         cDivision,invdate,STR(YEAR(InvDate),4) as Year, STR(MONTH(InvDate),2) as Month,;
         SUM(Ship) as ship,SUM(Totalchg) as Totalchg;
    FROM INVHDR;
    WHERE IIF(!EMPTY(laOGFxFlt[3,6]),INLIST(cDivision,laOGFxFlt[3,6]),.T.) .AND. ;
          IIF(!EMPTY(laOGFxFlt[4,6]),INLIST(SEASON,laOGFxFlt[4,6]),.T.)  .AND. ;
          &lcFlt. .AND. INVHDR.Status <> 'V' ;
    GROUP BY &lcGroup. INTO Cursor TmpShip
ENDIF
*B608099,1 NNA 05/27/2007 (End)


  *B607090,1 ABD - [End]

SCAN
  *B607090,1 ABD - Check if this [begin]
  IF llWorkWith .AND. !SEEK(Account,laOGFxFlt[lnAcctPos,6])
    LOOP
  ENDIF
  *B607090,1 ABD - [End]
  m.cDivision = cDivision
  m.cSeason   = ""
  m.cDesc     = ""
*B121575,1 NNA 09/02/2004 (Begin) clear 'm.cStyMajr'
  m.cStyMajor = ""
*B121575,1 NNA (END)
  m.cMonth    = PADL(ALLTRIM(Month),2,"0")
  m.cYear     = Year
  m.nShipQty  = Ship
  m.nShipAmt  = TotalChg
  INSERT INTO (lcTmpDet) FROM MEMVAR 
  m.cStyGroup = ""
  m.dCurrDate = dCurrDate

*B608099,1 NNA 05/27/2007 (BEGIN) Seek for the index included cDivision if we chose to group by Division
*IF SEEK(m.cYear + m.cMonth,lcTmpYears) AND &lcTmpYears..lMainYear AND !SEEK(m.cYear+m.cMonth,lcWorkFile)
IF SEEK(m.cYear + m.cMonth,lcTmpYears) AND &lcTmpYears..lMainYear AND ;
  !SEEK(IIF(lcRpGroup = "D",(m.cDivision+m.cYear+m.cMonth) ,m.cYear+m.cMonth),lcWorkFile)
*B608099,1 NNA (END)

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

*!**************************************************************************
*! Name      : lpInsrtCon
*! Developer : Nader Nabil (NNA)
*! Date      : 05/25/2007
*! Purpose   : Insert the Consolidated line in the Temp File
*!**************************************************************************
*! Example   : Do lpInsrtCon
*!**************************************************************************
*
PROCEDURE lpInsrtCon
IF SEEK(OrdLine.Order + STR(OrdLine.LineNo,6),'Consinvl')
  SELECT Consinvl
  SCAN REST WHILE Order + STR(LineNo,6) = OrdLine.Order + STR(OrdLine.LineNo,6)
    IF !EOF('InvHdr') AND InvHdr.Status <> 'V' AND llInclude
      IF SEEK(STR(YEAR(Consinvl.InvDate),4) + PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0"),lcTmpYears)
        SELECT Consinvl
        llInvIncld = .T.
        IF SEEK(&lcKeyValue + STR(YEAR(Consinvl.InvDate),4) + PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0"),lcWorkFile)
          SELECT (lcWorkFile)
          REPLACE nComplQty WITH nComplQty + 0 ,;
                  nComplAmt WITH nComplAmt + 0
        ELSE
          m.cDivision = InvHdr.cDivision
          m.cSeason   = Ordline.Season
          m.cStyGroup = Style.cStyGroup
          m.cDesc     = Style.Desc
          M.cStyMajor = Style.cStyMajor
          m.cMonth    = PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0")
          m.dCurrDate = Consinvl.InvDate
          m.cYear     = STR(YEAR(Consinvl.InvDate),4)    
          STORE 0 TO m.nStartQty , m.nStartAmt , m.nComplQty , m.nComplAmt
          INSERT INTO (lcWorkFile) FROM MEMVAR
        ENDIF  
        IF SEEK(&lcKeyValue + STR(YEAR(Consinvl.InvDate),4) + PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0"),lcTmpDet)
          SELECT (lcTmpdet)
          REPLACE nShipQty WITH nShipQty + Consinvl.TotQty 
          REPLACE nShipAmt WITH nShipAmt + (Consinvl.TotQty * Consinvl.Price)
        ELSE
          m.cDivision = OrdHdr.cDivision
          m.cSeason   = OrdLine.Season
          m.cDesc     = Style.Desc
          M.cStyMajor = Style.cStyMajor
          m.cMonth    = PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0")
          m.cYear     = STR(YEAR(Consinvl.InvDate),4)
          m.nShipQty = Consinvl.TotQty
          m.nShipAmt = Consinvl.TotQty * Consinvl.Price
          INSERT INTO (lcTmpDet) FROM MEMVAR
        ENDIF
      ENDIF
      lcInvoiceNo = Consinvl.Invoice
    ENDIF
  ENDSCAN
ELSE
  llConsol = .F.
  lcInvoiceNo = ''
  RETURN
ENDIF

SELECT Consinvl
=SEEK(OrdLine.Order + STR(OrdLine.LineNo,6))
SCAN REST WHILE Order + STR(LineNo,6) = OrdLine.Order + STR(OrdLine.LineNo,6)
  IF !EOF('InvHdr') AND InvHdr.Status <> 'V' AND !llInvIncld AND &lcConLnExp
    m.cDivision = OrdHdr.cDivision
    m.cSeason   = OrdLine.Season
    m.cDesc     = Style.Desc
    M.cStyMajor = Style.cStyMajor
    
    IF SEEK(STR(YEAR(Consinvl.InvDate),4) + PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0"),lcTmpYears) AND ;
      &lcTmpYears..lMainYear
      IF SEEK(&lcKeyValue + STR(YEAR(Consinvl.InvDate),4) + PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0"),lcWorkFile)
        SELECT (lcWorkFile)
        REPLACE nComplQty WITH nComplQty + 0 ,;
                nComplAmt WITH nComplAmt + 0
      ELSE
        m.cDivision = InvHdr.cDivision
        m.cSeason   = Ordline.Season
        m.cStyGroup = Style.cStyGroup
        m.cDesc     = Style.Desc
        M.cStyMajor = Style.cStyMajor
        m.cMonth    = PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0")
        m.dCurrDate = Consinvl.InvDate
        m.cYear     = STR(YEAR(Consinvl.InvDate),4)
        STORE 0 TO m.nStartQty , nStartAmt , m.nComplQty , m.nComplAmt
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDIF  
    ENDIF
    IF SEEK(&lcKeyValue + STR(YEAR(Consinvl.InvDate),4) + PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0"),lcTmpDet)
      SELECT (lcTmpdet)
      REPLACE nShipQty WITH nShipQty + Consinvl.TotQty
      REPLACE nShipAmt WITH nShipAmt + (Consinvl.TotQty * Consinvl.Price)
    ELSE
      m.cDivision = OrdHdr.cDivision
      m.cSeason   = OrdLine.Season
      m.cDesc     = Style.Desc
      M.cStyMajor = Style.cStyMajor
      m.cMonth    = PADL(ALLTRIM(STR(MONTH(Consinvl.InvDate),2)),2,"0")
      m.cYear     = STR(YEAR(Consinvl.InvDate),4)
      m.nShipQty = Consinvl.TotQty
      m.nShipAmt = Consinvl.TotQty * Consinvl.Price
      INSERT INTO (lcTmpDet) FROM MEMVAR
    ENDIF
    lcInvoiceNo = Consinvl.Invoice
  ENDIF
ENDSCAN  

*-- End of lpInsrtCon.

*!**************************************************************************
*! Name      : lfwOldInv
*! Developer : Nader Nabil (NNA)
*! Date      : 05/25/2007
*! Purpose   : To save the old value For incoice calculation's option
*!**************************************************************************
*! Example   : = lfwOldInv()
*!**************************************************************************
*
FUNCTION lfwOldInv
lcOldInv = lcRpShpAmt
*-- End of lfwOldVal.
