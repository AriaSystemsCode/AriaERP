*:**************************************************************************
*: Program file  : SOSLSMON
*: Program desc. : Order/Sales Summary Report By Month (Copied from C102355 for FRE20)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Omar MOhammad Shaban (OMS)
*: Date          : 07/22/2005
*: Reference     : N037459
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExpr , lpCollData , lpInsrtLin
*: Functions     : lfwRepWhen , lfItmPos , lfsrAcc
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*Modifications
*C126929:AYM :: ADD NEW FORM FOR ORDER ANALYSIS BY MONTH ORDER BY CATEGORY
*:**************************************************************************
*!*	ACTIVATE WINDOW TRACE
*!*	_SCREEN.Visible = .T. 
*!*	SUSPEND

*C126929:AYM :: ADD NEW FORM FOR ORDER ANALYSIS BY MONTH ORDER BY CATEGORY[BEGIN]
IF TYPE('lCPbyMont')='C' AND lCPbyMont='Y'
  =lfPbymon()
  RETURN
ENDIF
*C126929:AYM :: ADD NEW FORM FOR ORDER ANALYSIS BY MONTH ORDER BY CATEGORY[END]

*-- Check if user didn't typed a Month Range or Date Range.
IF LEN((laOGFxFlt[lnMonthPos,6]))>1
  IF RECCOUNT(laOGFxFlt[lnMonthPos,6]) = 0
  *-- Message < You have to type a full month range. >
  *-- Buttons <                  OK                  >
   =gfModalGen("TRM000000B00000","DIALOG",'','','You have to type a full month range.')
   RETURN
  ENDIF
 ELSE 
   =gfModalGen("TRM000000B00000","DIALOG",'','','You have to type a full month range.')
   RETURN
ENDIF 

IF LEN(ALLTRIM(laOGFxFlt[lnDatePos,6])) < 21
  *-- Message < You have to type a full date range. >
  *-- Buttons <                 OK                  >
  =gfModalGen("TRM000000B00000","DIALOG",'','','You have to type a full date range.')
  RETURN
ENDIF

lcRpShpAmt = IIF(lcRpGroup='N',lcRpShpAmt,'S')

*-- IF filter change collect data again.
IF loOGScroll.llOGFltCh
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
  SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO OrdCanLn ADDITIVE
  STORE 0     TO lnEnterOrd , lnCanclOrd 
  STORE 0.00 TO lnEnterAmt, lnCanclAmt
  lcGroupBy = IIF(lcRpGroup="S","cstymajor",IIF(lcRpGroup="Z","cSeason",IIF(lcRpGroup="D","cDivsion","")))

  DO lpCollData            && Collect the data for report.

  SELECT InvLine
  SET RELATION TO 

  SELECT OrdHdr
  SET RELATION TO
  
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
  SET ORDER TO (lcWorkFile)
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/22/2005
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

*--If the invoice calculation based on total charge.
IF lcRpShpAmt = 'T'
  =lfShip()
ENDIF

*-- Endif of Temp. Account file empty.
*-- End of lpCollData.
*!**************************************************************************
*! Name      : lpInsrtLin
*! Developer :Omar MOhammad Shaban (OMS)
*! Date      : 07/22/2005
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
  m.cStyMajor = Style.cStyMajor
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
  m.cStyMajor = Style.cStyMajor
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
            m.cStyMajor = Style.cStyMajor
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
            m.cStyMajor = Style.cStyMajor
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
      M.cStyMajor = Style.cStyMajor
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
          M.cStyMajor = Style.cStyMajor
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
        m.cStyMajor = Style.cStyMajor
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
*! Name      : lfsrAcc
*! Developer :Omar MOhammad Shaban (OMS)
*! Date      : 07/22/2005
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
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
*! Developer :Omar MOhammad Shaban (OMS)
*! Date      : 07/22/2005
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
PROCEDURE lpCreatFil
PRIVATE lcPerFrom , lcPerTo , lcLoop

*-- Create a temporary table to hold data which will be displayed in the report.
CREATE TABLE (gcWorkDir+lcWorkFile) (cDivision C(6)  , cSeason C(6)     , cStyGroup C(6)    ,;
                                     dCurrDate D(8)  , cMonth C(2)      , cYear C(4)        ,;
                                     cDesc C(20)     , nStartQty N(15)  , nStartAmt N(15,2) ,;
                                     nComplQty N(15) , nComplAmt N(15,2), cStyMajor C(19))

DO CASE
  CASE lcRpGroup = "S"
    lcKeyValue = 'm.cstymajor'
    INDEX ON cStyMajor + cYear + cMonth TAG (lcWorkFile)
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
PRIVATE LcAlias
LcAlias = SELECT(0)
SELECT laOGFxFlt[lnMonthPos,6]
LOCATE
lcPerFrom = STRTRAN(EVALUATE(laOGFxFlt[lnMonthPos,6]+'.Keyexp') ,"-","")   && Remove the hyphen "-"
GOTO bottom
lcPerTo   = STRTRAN(EVALUATE(laOGFxFlt[lnMonthPos,6]+'.Keyexp'),"-","")  && Remove the hyphen "-"  

SELECT(LcAlias)

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
              cSeason C(6) , cDesc C(20) , nShipQty N(15) , nShipAmt N(15,2), cStyMajor C(19))
DO CASE
  CASE lcRpGroup="S"
    INDEX ON cStyMajor + cYear + cMonth TAG (lcTmpDet)    
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
*! Developer :Omar MOhammad Shaban (OMS)
*! Date      : 07/22/2005
*! Purpose   : Generate Expression
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.
=ACOPY(laRangeInfo,laBrTmpFlt)       && Copy Browse Filter Array to Temp Array.

*-- If user selected Account.
IF ALLTRIM(laTempExpr[lnAcctPos,1]) $ lcRpExp

  lnArrayRow = ASCAN(laRangeInfo, "CUSTOMER.ACCOUNT")
  lnArrayRow = ASUBSCRIPT(laRangeInfo, lnArrayRow , 1)
  
  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,8]
  STORE "" TO aOGFxFlt
  
  *-- Copy only Account expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnAcctPos,1),8)
  
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
  DIMENSION laOGFxFlt[1,8]
  laOGFxFlt = ""

  *-- Copy only Date expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnDatePos,1),8)
  
  *-- Generate expression for Date.
  lcEnterExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Date from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcEnterExp,".T.")  
	
  lcEnterExp = LEFT(lcEnterExp,ATC("," , lcEnterExp))  + "ALLTRIM(DTOS(CTOD([" +;
               SUBSTR(lcEnterExp ,35 , 2) + "/" + SUBSTR(lcEnterExp ,37 , 2) + "/" + SUBSTR(lcEnterExp ,31 , 4) + "])))" + "," +;
               "ALLTRIM(DTOS(CTOD([" +;
               SUBSTR(lcEnterExp ,46 , 2) + "/" + SUBSTR(lcEnterExp ,48 , 2) + "/" + SUBSTR(lcEnterExp ,42 , 4) + "]))))"

  *-- Generate expression for Cancelled date.
  lcCanclExp = STRTRAN(lcEnterExp,"ORDHDR.ENTERED","ORDCANLN.CANCELLED")
ENDIF  
*-- EndIf of user selected Date.

*-- If user selected Month Range.
IF ALLTRIM(laTempExpr[lnMonthPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,8]
  laOGFxFlt = ""

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnMonthPos,1),8)
  
  *-- Generate expression for Month Range.
  lcStartExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Month Range from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcStartExp,".T.")  
  
  *-- we need to generate a new Month Range Expression based on ORDLINE.START.
  PRIVATE lcSetCent , lcPerFrom , lcPerTo , lcFromDate , lcToDate
  lcSetCent = SET('CENT')
  SET CENT ON

  laOGFxFlt[1,3] = "D"
  PRIVATE LcAlias
  LcAlias = SELECT(0)
  SELECT laOGFxFlt[lnMonthPos,6]
  LOCATE
  lcPerFrom = STRTRAN(EVALUATE(laOGFxFlt[lnMonthPos,6]+'.Keyexp') ,"-","")   && Remove the hyphen "-"
  GOTO bottom
  lcPerTo   = STRTRAN(EVALUATE(laOGFxFlt[lnMonthPos,6]+'.Keyexp'),"-","")  && Remove the hyphen "-"  
  SELECT(LcAlias)

  lcFromDate = IIF(SEEK(lcPerFrom,'FsPrd'),DTOC(FsPrd.dFsppBgDt),DTOC({}))
  lcToDate = IIF(SEEK(lcPerTo,'FsPrd'),DTOC(FsPrd.dFsppEnDt),DTOC({}))

  laOGFxFlt[1,1] = "ORDLINE.START"
  laOGFxFlt[1,5] = "Between"
  laOGFxFlt[1,6] = lcFromDate+"|"+lcToDate

  *-- Generate expression for Month Range.
  lcStartExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  lnComaSE = ATC("," , lcStartExp)
  lcStartExp = LEFT(lcStartExp,lnComaSE) + "ALLTRIM(DTOS(CTOD([" + SUBSTR(lcStartExp ,lnComaSE+1 , 10) + "])))" + "," + "ALLTRIM(DTOS(CTOD([" + SUBSTR(lcStartExp ,lnComaSE+12 , 10) + "]))))"
  
  lcComplExp = STRTRAN(lcStartExp,"ORDLINE.START","ORDLINE.COMPLETE")

  lcInvLnExp = STRTRAN(lcStartExp,"ORDLINE.START","INVLINE.INVDATE")

  laOGFxFlt[1,1] = "INVLINE.INVDATE"
  lcFromDate = IIF(SEEK(lcPerFrom,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppBgDt,-12)),DTOC({}))
  lcToDate = IIF(SEEK(lcPerTo,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppEnDt,-12)),DTOC({}))
  laOGFxFlt[1,6] = lcFromDate+"|"+lcToDate
  
  lcPrevusYr = gfGenFlt('laOGFxFlt',.T.,.T.)
  lnComaPy = ATC("," , lcPrevusYr)
  lcPrevusYr = LEFT(lcPrevusYr,lnComaPy) + "ALLTRIM(DTOS(CTOD([" + SUBSTR(lcPrevusYr ,lnComaPy+1 , 10) + "])))" + "," + "ALLTRIM(DTOS(CTOD([" + SUBSTR(lcPrevusYr ,lnComaPy+12 , 10) + "]))))"
  lcInvLnExp = lcInvLnExp + [ OR ] + lcPrevusYr
  lcFromDate = IIF(SEEK(lcPerFrom,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppBgDt,-24)),DTOC({}))
  lcToDate = IIF(SEEK(lcPerTo,'FsPrd'),DTOC(GOMONTH(FsPrd.dFsppEnDt,-24)),DTOC({}))
  laOGFxFlt[1,6] = lcFromDate+"|"+lcToDate

  lc2PrvusYr = gfGenFlt('laOGFxFlt',.T.,.T.)
  lnComaPy = ATC("," , lc2PrvusYr)
  lc2PrvusYr = LEFT(lc2PrvusYr,lnComaPy) + "ALLTRIM(DTOS(CTOD([" + SUBSTR(lc2PrvusYr ,lnComaPy+1 , 10) +  "])))" + "," + "ALLTRIM(DTOS(CTOD([" + SUBSTR(lc2PrvusYr ,lnComaPy+12 , 10) + "]))))"
  lcInvLnExp = lcInvLnExp + [ OR ] + lc2PrvusYr
  
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
*! Name      : lfArrDumy
*! Developer :Omar MOhammad Shaban (OMS)
*! Date      : 07/22/2005
*! Purpose   : Fill select by and sort by arrays.
*!**************************************************************************
*! Example   : = lfArrDumy()
*!**************************************************************************
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
FUNCTION lfShip
lcFlt = STRTRAN(lcInvLnExp,'INVLINE','INVHDR')
lcGroup = IIF(lcRpGroup = "D","cDivision","year,month")

PRIVATE lnLevel
lnLevel = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70

SELECT Account,CTOD(STR(MONTH(InvDate),2)+"\01\"+STR(YEAR(InvDate),4)) as dCurrDate,;
       cDivision,invdate,STR(YEAR(InvDate),4) as Year, STR(MONTH(InvDate),2) as Month,;
       SUM(Ship) as ship,SUM(Totalchg) as Totalchg;
  FROM INVHDR;
  WHERE &lcFlt. .AND. INVHDR.Status <> 'V';
  GROUP BY &lcGroup. INTO Cursor TmpShip
  llWorkWith = !EMPTY(laOGFxFlt[lnAcctPos,6]) .AND. USED(laOGFxFlt[lnAcctPos,6]) .AND. RECCOUNT(laOGFxFlt[lnAcctPos,6]) >= 1  

SET ENGINEBEHAVIOR &lnLevel

SCAN
  IF llWorkWith .AND. !SEEK(Account,laOGFxFlt[lnAcctPos,6])
    LOOP
  ENDIF
  m.cDivision = cDivision
  m.cSeason   = ""
  m.cDesc     = ""
  m.cStyMajor = ""
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

*--End of lfShip.
*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer :Omar MOhammad Shaban (OMS)
*! Date      : 07/22/2005
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
FUNCTION lfwRepWhen

lnMonthPos = lfItmPos("FSPRD.CFISFYEAR +'-'+ FSPRD.CFSPPRDID") && get Style Group Fixed filter Position.
lnAcctPos  = lfItmPos('ORDHDR.ACCOUNT')   && get Account Fixed filter Position.
lnDatePos  = lfItmPos('ORDHDR.ENTERED')   && get Date Fixed filter Position.

*-- End of lfwRepWhen.
*!**************************************************************************
*! Name      : lfItmPos
*! Developer :Omar MOhammad Shaban (OMS)
*! Date      : 07/22/2005
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfItmPos.
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

loOGScroll.ClearRead()

*-- End of lfClrRead.


*C126929:AYM :: ADD NEW FORM FOR ORDER ANALYSIS BY MONTH ORDER BY CATEGORY[BEGIN]

*!*************************************************************
*! Name      : lfPbymon
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 03/07/2006
*! Purpose   : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPbymon()
*!*************************************************************
FUNCTION lfPbymon

  *-- Variable declarations
  STORE "" TO lcAccount
  STORE .F. TO llACCOUNT
  DIMENSION lamon(6,4)
 
  *.. Create laMon array which stores data about the six monthes
  =lfFillamon(ldBegin)
    
  *.. Create a Final cursor to be displayed  
  CREATE CURSOR FOrdHdr (cat_code c(6),categ c(50),m1id n(2),m1open n(10,3),m1openamt n(10,3),m2id n(2),m2open n(10,3),m2openamt n(10,3);
                         ,m3id n(2),m3open n(10,3),m3openamt n(10,3),m4id n(2),m4open n(10,3),m4openamt n(10,3);
                         ,m5id n(2),m5open n(10,3),m5openamt n(10,3);
                         ,m6id n(2),m6open n(10,3),m6openamt n(10,3))

   lcWhere = "OrdHdr.Cordtype='O' AND OrdHdr.Status "
   lcWhere = lcWhere+IIF(ALLTRIM(lcStatus)='A'," $('OH')","='"+lcStatus+"'")
   lcWhere =lcWhere +" AND BETWEEN(OrdHdr.entered,CTOD('"
   lcWhere =lcWhere + DTOC(lamon(1,3))+"'),CTOD('"+DTOC(lamon(6,4)) +"'))"
  
  SELECT FOrdHdr 
  =CURSORSETPROP("Buffering",3) 
  INDEX on cat_code TAG Catind
  =CURSORSETPROP("Buffering",5) 
  SELECT ORDHDR
  SCAN FOR EVALUATE(LCRPEXP)AND EVALUATE(lcWhere)
    lcindx     = CEILING(ASCAN(laMon,MONTH(ordhdr.ENTERED))/4)
    lcMnopen   = "m"+ALLTRIM(STR(lcindx))+"open"
    lcMnopenamt= "m"+ALLTRIM(STR(lcindx))+"openamt"
    IF !EMPTY(ordhdr.CORDERCAT)
      lcCat= ALLTRIM(gfCodDes(ordhdr.CORDERCAT, 'CORDERCAT'))
      lcCatcode=ALLTRIM(ordhdr.CORDERCAT)
    ELSE 
      lcCat="      "  &&.. 6 EMPTY CHAR FOR ORDERS WITH NO CATEGORIES
      lcCatcode="      "
    ENDif  
    SELECT FOrdHdr 
    *..IF Category found REPLACE ELSE INSERT
    IF  SEEK(lcCatcode)
      Replace &lcMnopen WITH &lcMnopen+ordhdr.open ,&lcMnopenamt WITH &lcMnopenamt+ordhdr.openamt
    ELSE    
      INSERT INTO FOrdHdr (cat_code,categ,&lcMnopen,&lcMnopenamt)VALUES (lcCatcode,lcCat ,ordhdr.open ,ordhdr.openamt)
    ENDIF
  ENDSCAN

SELECT FOrdHdr
SUM m1open+m2open+m3open+m4open+m5open+m6open TO lnOpen
SUM m1openamt+m2openamt+m3openamt+m4openamt+m5openamt+m6openamt TO lnOpenamt

SET ORDER  TO Catind
lcMessage = "Number of lines:" + ALLTRIM(STR(RECCOUNT()))+"   will be printed in :  "+ALLTRIM(STR(_PAGETOTAL))+"  Pages"
WAIT WINDOW lcMessage nowait
lcFrmNam='SOOPNORD'
=gfCrtFrm(lcFrmNam,'',llOGRefForm)
SELECT FOrdHdr
GO TOP
 
DO gfDispRe WITH EVAL('lcFrmNam')

*!*************************************************************
*! Name      : lfFormName
*! Developer : AYMAN MAHMOUD AHMED
*! Date      : 03/07/2006
*! Purpose   : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFormName()
*!*************************************************************
FUNCTION lfFormName
  RETURN 'SOSLSMON'





*!*************************************************************
*! Name      : lfFillamon
*! Developer : AYMAN MAHMOUD AHMED
*! Date      : 03/07/2006
*! Purpose   : - Create an array of the six month with details
*              - lfFillamon fill the month array with
*              - [ month no ,month name, first day in monthh, last day in month ]
*! lfFillamon  Create an array of the six month with details
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillamon()
*!*************************************************************
FUNCTION lfFillamon
LPARAMETERS LDATE

Local lnBmon,lnyears
lnBmon =MONTH(lDate)
lnyears=YEAR(lDate)

FOR I =1 TO 6
  laMon(i,1)=lnBmon
  laMon(i,3)=CTOD(ALLTRIM(STR(lnBmon))+"-01-"+ALLTRIM(STR(lnyears)))     
  laMon(i,4)=CTOD(ALLTRIM(STR(lnBmon))+"-"+ALLTRIM(STR(lfFdom(lnBmon,lnyears)))+"-"+ALLTRIM(STR(lnyears)))       
  laMon(i,2)=CMONTH(CTOD(ALLTRIM(STR(lnBmon))+"-01-"+ALLTRIM(STR(lnyears))))
  IF lnBmon<12
    lnBmon=lnBmon+1
  ELSE
    lnBmon =1
    lnyears=lnyears+1
  ENDIF
ENDFOR


*!*************************************************************
*! Name      : lffmod 
*! Developer : AYMAN MAHMOUD AHMED
*! Date      : 03/07/2006
*! Purpose   : lffmod get last day in month
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lffmod ()
*!*************************************************************

FUNCTION lfFdom
LPARAMETERS lnMon,lnyear

LOCAL ldLday
DO case
  CASE lnMon=1 OR lnMon=3  OR lnMon=5 OR lnMon=7 OR lnMon=8 OR lnMon=10 OR lnMon=12
    ldLday=31
  CASE lnMon=4 OR lnMon=6 OR lnMon=9 OR lnMon=11
    ldLday=30
  CASE lnMon=2
    IF MOD(lnyear,4)<>0
      ldLday=28
    ELSE
      ldLday=29
    ENDIF
ENDCASE

RETURN ldLday

*!*************************************************************
*! Name      : lfVdate 
*! Developer : AYMAN MAHMOUD AHMED
*! Date      : 03/07/2006
*! Purpose   : validates if Begin Date is empty
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfVdate ()
*!*************************************************************

FUNCTION lfVdate

IF TYPE('ldbegin') = 'D' AND EMPTY(ldBegin)
  RETURN .f.
ENDIF
*!*************************************************************
*! Name      : LFVPMON
*! Developer : AYMAN MAHMOUD AHMED
*! Date      : 03/07/2006
*! Purpose   : REFRESH OPTION GRID
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = LFVPMON()
*!*************************************************************

FUNCTION LFVPMON
ClearRead()
*C126929:AYM :: ADD NEW FORM FOR ORDER ANALYSIS BY MONTH ORDER BY CATEGORY[END]


