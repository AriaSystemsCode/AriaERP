*:***************************************************************************
*: Program file        : SOOPEN
*: Program description : OPEN ORDER BY ITEM
*: Module              : SALES ORDER (SO)
*: Developer           : Tarek Noaman Ahmed
*: Tracking Job Number : C126903
*: Date                : 14/3/2006
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe
*:***************************************************************************
*: Called From: Option Grid
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOOPEN
*:***************************************************************************
*:Modifications : ....
*:***************************************************************************
*: OG Filters
*: OrdHdr.Order       : Order Range
*: OrdHdr.Start       : Start Date
*: OrdHdr.Complete    : Complete Date
*: OrdHdr.Entered     : Entered Date
*: OrdHdr.Cancelled   : Cancelled Date
*: OrdHdr.Status      : Order Status
*: OrdHdr.cDivision   : Order Header Division
*: OrdHdr.Account     : Order Customer
*: OrdHdr.Season      : Order Season
*: OrdHdr.Spcinst     : Order Special Instruction
*: OrdHdr.ShipVia     : Order Ship Via
*: OrdHdr.CORDERCAT   : Order Header Category
*: OrdHdr.Priority    : Order Priority
*: Style.Style        : Order Style
*: OrdLine.Color      : Order Color
*: Customer.Region    : Region
*:**************************************************************************************************

lcStTime = TIME()
STORE 0  TO lnClrPo,lnNonMajPo
STORE "" TO lcNonMajPi,lcFreeClr,lcNonMajT
=lfNonMaj()
lnNonMajPi = LEN(ALLTRIM(lcNonMajPi))

llDummy  = loOgScroll.llOGFltCh AND lfCollData()

lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT FOrder
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Order Line(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

DO gfDispRe WITH EVAL('lcRpName')


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Tarek Noaman
*! Date      : 
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
FUNCTION lfwRepWhen


*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Noaman
*! Date      : 14/3/2006
*! Purpose   : Collect Report Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData
CREATE CURSOR FOrder (order C(6),account C(5),btname C(30),style c(19),desc C(20),start D,entered D,cancelled D,price N(12,2), ;
                      book1 N(6,0),book2 N(6,0),book3 N(6,0),book4 N(6,0),book5 N(6,0),book6 N(6,0),book7 N(6,0),book8 N(6,0), ;
                      qty1  N(6,0),qty2  N(6,0),qty3  N(6,0),qty4  N(6,0),qty5  N(6,0),qty6  N(6,0),qty7  N(6,0),qty8  N(6,0))
SELECT FOrder
INDEX ON SUBSTR(style,1,lnClrPo-2)+order+SUBSTR(style,lnClrPo) TAG rpt_idx
SELECT Style
SET ORDER TO STYLE
SELECT Ordhdr
SET RELATION TO cordtype+order INTO Ordline ADDITIVE
SET RELATION TO 'M'+account INTO customer ADDITIVE
SELECT Ordline
SET RELATION TO style INTO Style ADDITIVE

lcRpExp2 = lfCreateExp()
SELECT Ordhdr
SCAN FOR EVALUATE(lcRpExp2) AND Ordhdr.status <> 'X'
  SELECT Ordline
  IF SEEK(Ordhdr.cordtype+Ordhdr.order)
    SCAN WHILE Ordline.cordtype+Ordline.order = Ordhdr.cordtype+Ordhdr.order FOR EVALUATE(lcRpExp)
      SCATTER MEMVAR memo
      INSERT INTO FOrder FROM MEMVAR
      REPLACE FOrder.btname    WITH Customer.btname, ;
              FOrder.desc      WITH Style.desc, ;
              FOrder.entered   WITH Ordhdr.entered, ;
              FOrder.cancelled WITH Ordhdr.cancelled
    ENDSCAN
  ENDIF
ENDSCAN
SELECT style
SET ORDER TO CSTYLE

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Tarek Noaman
*! Date      : 14/3/2006
*! Purpose   : 
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfNonMaj

LOCAL llStopConc

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg = gfItemMask('SM', '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))  && No. of major segments.

DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg, '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(EMPTY(lnNonMajPo), laMajSeg[lnI,4], lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

lcColorTt = 'Only these ' + ALLTRIM(lcNonMajT) + 's'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''


*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfCreateExp
*! Developer : Tarek Noaman
*! Date      : 14/3/2006
*! Purpose   : Get Report Expression without Ordline Part
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCreateExp
lnPos = 0
lnPos = ATC("INLIST(PADR(SUBSTR(ORDLINE.STYLE",lcRpExp)
IF lnPos = 0
  lnPos = ATC("BETWEEN(PADR(SUBSTR(ORDLINE.STYLE",lcRpExp)
ENDIF
IF lnPos = 0
  lnPos = ATC("INLIST(SUBSTR(Ordline.Style",lcRpExp)
ENDIF
IF lnPos = 0
  lnPos = ATC("BETWEEN(SUBSTR(Ordline.Style",lcRpExp)
ENDIF
RETURN IIF(lnPos=0,lcRpExp,SUBSTR(lcRpExp,1,lnPos-1) + ".T.")
