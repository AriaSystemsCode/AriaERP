*:***************************************************************************
*: Program file  : ICSUMSTK.PRG
*: Program desc. : CUSTOMIZED SUMMARY FINISHED GOODS STOCK FOR COL17.
*: Date          : 07/04/2002
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C200366
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSUMSTK
*:***************************************************************************
*: Modifications :
*:***************************************************************************

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*--THE STYLE LENGTH
STORE 0 TO lnLenth
lnLenth    = LEN(gfItemMask('PM'))

*--Change the string of the Lcrpexp in case there is warehouse in it.
IF 'WAREHOUS.CWARECODE' $ lcRpExp
  lcRpExp    = STRTRAN(lcRpExp,"WAREHOUS.CWARECODE","STYLE.CDEFWARE")
  lnWarePos  = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'WAREHOUS.CWARECODE'),1)
ENDIF

*--The main functions of the report.
=lfCreatTmp()
=lfColctDat()

*-- Browsing the report.
SELECT (lcLinesTmp)
LOCATE
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
  *DO gfDispRe WITH EVALUATE('lcRpForm')
  DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + "&lcLinesTmp..TotValu <> 0 "
ENDIF
SET CENTURY &lcCentury

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/07/2002
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

*--Creat this temp. file to collect the data.
CREATE Table (lcLinesTmp) (Style C(lnLenth) , Location C(6) , StyGroup C(6) , StyDesc C(20) ,;
                           UntCost N(6,2)   , Quantit N(9)  , TotValu N(12,2)   )

INDEX ON Style + Location TAG LocStyle OF (gcWorkDir + lcLinesTmp + ".CDX")
INDEX ON Location + StyGroup + Style TAG LocGSty OF (gcWorkDir + lcLinesTmp + ".CDX")

CREATE Table (lcLinsJl) (Style C(lnLenth) , Location C(6) , StyGroup C(6) , StyDesc C(20) ,;
                         UntCost N(6,2)   , Quantit N(9)  , TotValu N(12,2)   )

INDEX ON Style + Location TAG LocGSty OF (gcWorkDir + lcLinsJl + ".CDX")

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/07/2002
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfcolctDat()
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcStyleFr

*--lcStyleFr variable hold the style.
STORE SPACE(lnLenth) TO lcStyleFr

*--Section for collecting the data.
SELECT STYLE
SCAN FOR &lcRpexp

  WAIT WINDOW "Collecting data for style " + STYLE.STYLE NOWAIT

  IF lcStyleFr # LEFT(STYLE.STYLE,lnLenth)
    lcStyleFr = LEFT(STYLE.STYLE,lnLenth)

    SELECT (lcLinesTmp)
    APPEND BLANK
    REPLACE Style    WITH LEFT(STYLE.STYLE,lnLenth) ,;
            StyDesc  WITH STYLE.DESC                ,;
            Location WITH STYLE.CDEFWARE            ,;
            StyGroup WITH STYLE.cstygroup
  ENDIF

  *--Section to get the Total value from Styinvjl file.
  STORE 0 TO lnTotVal
  IF SEEK(STYLE.STYLE + STYLE.CDEFWARE , 'STYINVJL')
    SELECT STYINVJL
    SCAN REST WHILE STYLE + CWARECODE + CSESSION + DTOS(DTRDATE) + CTRCODE + STR(LINENO,6) = STYLE.STYLE + STYLE.CDEFWARE
      lnTotVal = lnTotVal + STYINVJL.NSTKVAL
    ENDSCAN
  ENDIF

  *--Section to get the Quantity from Stydye file.
  STORE 0 TO lnQuantty
  IF SEEK(STYLE.STYLE + STYLE.CDEFWARE , 'STYDYE')
    SELECT STYDYE
    SCAN REST WHILE STYLE + CWARECODE + DYELOT = STYLE.STYLE + STYLE.CDEFWARE
      lnQuantty = lnQuantty + STYDYE.TOTSTK
    ENDSCAN
  ENDIF

  SELECT (lcLinesTmp)
  REPLACE Quantit WITH Quantit + lnQuantty  ,;
          TotValu WITH TotValu + lnTotVal
  REPLACE UntCost WITH IIF( Quantit = 0 , 0 , TotValu / Quantit)

ENDSCAN

*--This code in case there are transaction in stores different from the store in the style file.
PRIVATE lcOrder
SELECT (lcLinesTmp)
lcOrder = ORDER()
SET ORDER TO TAG LocStyle

STORE SPACE(lnLenth) TO lcStyleFr
SELECT STYDYE
SET RELATION TO STYLE INTO STYLE ADDITIVE
LOCATE

lcRpExp = STRTRAN(lcRpExp,"INLIST(STYLE.CSTYMAJOR","INLIST(PADR(LEFT(STYDYE.STYLE,lnLenth),19)")
lcRpExp = STRTRAN(lcRpExp,"INLIST(STYLE.CDEFWARE","INLIST(STYDYE.CWARECODE")

SCAN FOR &lcRpExp
  IF !SEEK(LEFT(STYDYE.STYLE,lnLenth) + STYDYE.CWARECODE, lcLinesTmp) AND STYDYE.TOTSTK # 0
    IF lcStyleFr # LEFT(STYDYE.STYLE,lnLenth) + STYDYE.CWARECODE
      lcStyleFr = LEFT(STYDYE.STYLE,lnLenth) + STYDYE.CWARECODE

      SELECT (lcLinsJl)
      APPEND BLANK
      REPLACE Style    WITH LEFT(STYDYE.STYLE,lnLenth) ,;
              StyDesc  WITH STYLE.DESC                 ,;
              Location WITH STYDYE.CWARECODE           ,;
              StyGroup WITH STYLE.cstygroup
    ENDIF

    *--Section to get the Total value from Styinvjl file.
    IF SEEK(LEFT(STYDYE.STYLE,lnLenth) + STYDYE.CWARECODE , lcLinsJl)
      SELECT (lcLinsJl)
      REPLACE Quantit WITH Quantit + STYDYE.TOTSTK  ,;
              TotValu WITH TotValu + STYDYE.NSTKVAL
    ENDIF
  ENDIF
ENDSCAN

SELECT STYDYE
SET RELATION TO

SELECT (lcLinsJl)
SCAN
  REPLACE UntCost WITH IIF( Quantit = 0 , 0 , TotValu / Quantit)
ENDSCAN

SELECT (lcLinesTmp)
APPEND FROM (lcLinsJl)

SELECT (lcLinesTmp)
SET ORDER TO TAG &lcOrder

WAIT CLEAR
*--End of lfcolctDat.
*!**************************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/07/2002
*! Purpose   : Go top in the style IN RANGE.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSty.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/07/2002
*! Purpose   : Get major seg. picture.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*--ENd of lfMajPic.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/07/2002
*! Purpose   : Validate Style.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style')
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/07/2002
*! Purpose   : To get the style major segement title.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtlGet.
*!*************************************************************
*! Name      : lfvWareHos
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/07/2002
*! Purpose   : Validate Warehouse Code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvWareHos()
*!*************************************************************
FUNCTION lfvWareHos
PRIVATE lcWareHous , lcTag

lcWareHous = VARREAD()
lcTag      = ORDER('WAREHOUS')
SET ORDER TO WAREHOUS IN WAREHOUS

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHous.,'WAREHOUS')
    &lcWareHous = WAREHOUS.cWareCode
  ELSE
    &lcWareHous = gfBroWWare(.T.)
  ENDIF
ELSE
  &lcWareHous = lcOldWare
ENDIF
SET ORDER TO &lcTag IN WAREHOUS

*-- End of lfvWareHos.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/07/2002
*! Purpose   : Validate fabric.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

lcFabObj  = VARREAD()
lcFab     = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF

lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC')
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF

SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF

*--End of lfvFabric.