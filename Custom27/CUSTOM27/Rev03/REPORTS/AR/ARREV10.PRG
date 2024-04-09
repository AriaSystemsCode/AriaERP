*:**************************************************************************
*: Program file  : ARREV10
*: Program desc. : Sales Summary Report By Month For Revue.
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 12/24/2001
*: Reference     : C102490
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExpr , lpCollData , lpInsrtLin
*: Functions     : lfwRepWhen , lfItmPos
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*

*-- IF filter change collect data again.
IF llOGFltCh
  PRIVATE lcInvHdExp , lcRetHdExp
  STORE '.T.' TO lcInvHdExp , lcRetHdExp

  DO lpCreatFil          && Create the temp file and fill it with the years selected from Option grid.
  DO lpGenExpr           && Get the Date expression out from lcRpExp.
  DO lpCollData          && Collect the data for report.
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
*! Date      : 12/24/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
SELECT InvHdr
SCAN FOR &lcInvHdExp AND Status <> 'V'
  WAIT WINDOW 'Collecting data from Invoice # ' + Invoice NOWAIT  
  IF SEEK(STR(year(InvDate),4)+PADL(MONTH(InvDate),2,'0'),(lcWorkFile))
    m.nGrossAmt = ShipAmt + Discount
    m.nGrossQty = Ship
    SELECT (lcWorkFile)
    REPLACE nGrossAmt WITH nGrossAmt + m.nGrossAmt , ;
            nGrossQty WITH nGrossQty + m.nGrossQty
  ENDIF
ENDSCAN

SELECT RetHdr
SCAN FOR &lcRetHdExp AND Status <> 'V'
  WAIT WINDOW 'Collecting data from Credit Memo # ' + CrMemo NOWAIT
  IF SEEK(STR(year(CrDate),4)+PADL(MONTH(CrDate),2,'0'),(lcWorkFile))
    m.nCredtAmt = Amount
    m.nCredtQty = Pieces
    SELECT (lcWorkFile)
    REPLACE nCredtAmt WITH nCredtAmt + m.nCredtAmt , ;
            nCredtQty WITH nCredtQty + m.nCredtQty
  ENDIF
ENDSCAN 
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/24/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnMonthPos = lfItmPos('INVHDR.INVDATE') && get Style Group Fixed filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/24/2001
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
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/24/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
PRIVATE lcAlias , lcPerFrom , lcPerTo , lcLoop , lcSetDate
lcAlias = ALIAS()

*-- Create a temporary table to hold data which will be displayed in report.
CREATE TABLE (gcWorkDir+lcWorkFile) (cMonth C(2) , cYear C(4) , nGrossQty N(15) , ;
              nGrossAmt N(15,2) , nCredtQty N(15) , nCredtAmt N(15,2) , cMonthStr C(9))
INDEX ON cYear + cMonth TAG (lcWorkFile)     

*-- Adding records for the current year in Temp. Years file.
IF LEN(laOGFxFlt[lnMonthPos,6]) < 15    && if user didn't type a year , month
  PRIVATE llFisHdr
  llFisHdr = gfOpenFile(gcDataDir+'FisHd','','SH')
  
  LOCATE FOR cFisYStat = "C"
  IF FOUND() AND SEEK(cFisFYear,'FsPrd')
    SELECT FsPrd
    SCAN REST WHILE cFisFYear + cFspPrdId = FisHd.cFisFYear
      IF !EMPTY(cFspPrdId)
        m.cYear = cFisFYear
        m.cMonth = cFspPrdId
        m.cMonthStr = UPPER(CMONTH(dFspPbgDt))
        STORE 0 TO m.nGrossQty , m.nGrossAmt , m.nCredtQty , m.nCredtAmt
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDIF  
    ENDSCAN
  ENDIF
  
  IF llFisHdr
    USE IN FisHd
  ENDIF
ELSE
  lcPerFrom = STRTRAN(SUBSTR(laOGFxFlt[lnMonthPos,6],1,7),"-","")   && Remove the hyphen "-"
  lcPerTo   = STRTRAN(SUBSTR(laOGFxFlt[lnMonthPos,6],9,15),"-","")  && Remove the hyphen "-"  
  
  lcSetDate = SET('DATE')
  SET DATE TO AMERICAN
  
  lcLoop = lcPerFrom
  DO WHILE lcLoop <= lcPerTo
    m.cMonthStr = UPPER(CMONTH(CTOD(SUBSTR(lcLoop,5,2)+"/01/"+SUBSTR(lcLoop,1,4))))
    m.cYear = SUBSTR(lcLoop,1,4)
    m.cMonth = PADL(ALLTRIM(SUBSTR(lcLoop,5,2)),2,"0")
    STORE 0 TO m.nGrossQty , m.nGrossAmt , m.nCredtQty , m.nCredtAmt
    IF VAL(SUBSTR(lcLoop,5,2)) = 12
      lcLoop = STR(VAL(SUBSTR(lcLoop,1,4))+1,4) + "01"
    ELSE
      lcLoop = SUBSTR(lcLoop,1,4) + PADL(ALLTRIM(STR(VAL(SUBSTR(lcLoop,5,2))+1,2)),2,"0")
    ENDIF

    IF !SEEK(m.cYear + m.cMonth,lcWorkFile)
      INSERT INTO (lcWorkFile) FROM MEMVAR
    ENDIF
  ENDDO
  
  SET DATE TO &lcSetDate
ENDIF

SELECT (lcAlias)
*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/24/2001
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

*-- If user selected Month Range.
IF ALLTRIM(laTempExpr[lnMonthPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnMonthPos,1),7)
  
  *-- we need to generate a new Month Range Expression based on INVHDR.INVDATE.
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
  lcInvHdExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  lcRetHdExp = STRTRAN(lcInvHdExp,"INVHDR.INVDATE","RETHDR.CRDATE")

  SET CENT &lcSetCent
ENDIF  
*-- If user selected Month Range.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/24/2001
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
*! Date      : 12/24/2001
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
