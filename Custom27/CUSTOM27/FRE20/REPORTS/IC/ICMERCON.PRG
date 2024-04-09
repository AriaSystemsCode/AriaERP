*:***************************************************************************
*: Program file  : ICMERCON.PRG
*: Program desc. : CUSTOMIZED Merchandise Control Report FOR FRE20.
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102605
*:***************************************************************************
*: Calls :
*:    Functions : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICMERCON.PRG
*:***************************************************************************
*: Modifications :
*****************************************************************************

*--THE COLOR LENGTH
STORE 0 TO lnClrLnFr , lnClrPosFr
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnFr  = LEN(laItemSeg[lnCount,3])
    lnClrPosFr = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*--The main functions of the report.
=lfCreatTmp()
=lfRelation()
=lfColctDat()

*--Section of the recap data.
IF llRpRecap
  SELECT (lcLinesTmp)
  LOCATE
  REPLACE ALL TYPE WITH "A"
  
  SELECT (lcLinsRcap)
  APPEND FROM (lcLinesTmp)
  REPLACE ALL TYPE WITH "B"

  SELECT (lcLinesTmp)
  APPEND FROM (lcLinsRcap)
ENDIF

*-- Browsing the report.
SELECT (lcLinesTmp)
LOCATE
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

DO gfDispRe WITH EVALUATE('lcRpForm')
SET CENTURY &lcCentury

*--Section break the relations.
SELECT STYLE
SET RELATION TO

SELECT POSLN
SET RELATION TO

SELECT ORDLINE
SET RELATION TO

SELECT INVLINE
SET RELATION TO

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
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

*--Creat this temp. file to collect all the transations.
CREATE Table (lcLinesTmp) (Style C(19)      , StyDesc C(20)     , Color C(6)        , ClrDesc C(30)     ,;
                           Season C(6)      , cstyGrop C(6)     , UntOwn N(5)       , UntSldPrm N(5)    ,;
                           UntSldOp N(5)    , BalToSell N(5)    , OpnWip N(5)       , TotRcpts N(5)     ,;
                           TotShip N(6)     , InStock N(5)      , OpnOrd N(5)       , AprvdOrd N(5)     ,;
                           AllocOrd N(5)    , CrdtOrd N(5)      , Rtrns N(4)        , PriceNoA N(12,2)  ,;
                           VaLUntOwn N(10)  , VaLUntSPrm N(10)  , ValUntSOp N(10)   , ValBalToSl N(10)  ,;
                           ValOpnWip N(10)  , ValTotRcpt N(10)  , ValTotShip N(10)  , ValInStock N(10)  ,;
                           ValOpnOrd N(10)  , ValAprvOrd N(10)  , ValAlocOrd N(10)  , ValCrdtOrd N(10)  ,;
                           ValRtrns N(10)  , llPrnSty L        , Type C(1))
ZAP

IF llRpRecap
  INDEX ON Type + Season + cstyGrop + Style TAG SeGrpSty OF (gcWorkDir + lcLinesTmp + ".CDX")

  CREATE Table (lcLinsRcap) (Style C(19)      , StyDesc C(20)     , Color C(6)        , ClrDesc C(30)     ,;
                             Season C(6)      , cstyGrop C(6)     , UntOwn N(5)       , UntSldPrm N(5)    ,;
                             UntSldOp N(5)    , BalToSell N(5)    , OpnWip N(5)       , TotRcpts N(5)     ,;
                             TotShip N(6)     , InStock N(5)      , OpnOrd N(5)       , AprvdOrd N(5)     ,;
                             AllocOrd N(5)    , CrdtOrd N(5)      , Rtrns N(4)        , PriceNoA N(12,2)  ,;
                             VaLUntOwn N(10)  , VaLUntSPrm N(10)  , ValUntSOp N(10)   , ValBalToSl N(10)  ,;
                             ValOpnWip N(10)  , ValTotRcpt N(10)  , ValTotShip N(10)  , ValInStock N(10)  ,;
                             ValOpnOrd N(10)  , ValAprvOrd N(10)  , ValAlocOrd N(10)  , ValCrdtOrd N(10)  ,;
                             ValRtrns N(10)   , llPrnSty L        , Type C(1))
  ZAP
  INDEX ON Type + Season + cstyGrop + Style TAG (lcLinsRcap) OF (gcWorkDir + lcLinsRcap + ".CDX")

ELSE
  INDEX ON Season + cstyGrop + Style TAG SeGrpSty OF (gcWorkDir + lcLinesTmp + ".CDX")
ENDIF

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
*! Purpose   : Function to create the needed relations.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT STYLE
IF !('STYLE.STYLE' $ SET('RELATION'))
  SET RELATION TO STYLE.STYLE INTO POSLN ADDITIVE
  SET RELATION TO STYLE.STYLE INTO ORDLINE ADDITIVE
  SET RELATION TO STYLE.STYLE INTO INVLINE ADDITIVE
  SET RELATION TO STYLE.STYLE INTO RETLINE ADDITIVE

  SELECT POSLN
  SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE

  SELECT ORDLINE
  SET RELATION TO "O" + ORDLINE.ORDER INTO ORDHDR ADDITIVE

  SELECT INVLINE
  SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE

  SELECT RETLINE
  SET RELATION TO RETLINE.CRMEMO INTO RETHDR ADDITIVE
ENDIF

*--End of lfRelation.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
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

  SELECT (lcLinesTmp)
  APPEND BLANK
  REPLACE Style      WITH LEFT(STYLE.STYLE,lnLenth)                     ,;
          StyDesc    WITH STYLE.DESC                                    ,;
          Color      WITH SUBSTR(STYLE.STYLE , lnClrPosFr , lnClrLnFr ) ,;
          Season     WITH STYLE.SEASON                                  ,;
          cstyGrop   WITH STYLE.cstygroup                               ,;
          InStock    WITH STYLE.TOTSTK                                  ,;
          ValInStock WITH ValInStock + ( STYLE.TOTSTK * STYLE.PriceA )  ,;
          OpnWip     WITH STYLE.TOTWIP                                  ,;
          ValOpnWip  WITH ValOpnWip  + ( STYLE.TOTWIP * STYLE.PriceA )  ,;
          PriceNoA   WITH STYLE.PriceA                                  ,;
          ClrDesc    WITH gfCodDes(SUBSTR(Style.STYLE , lnClrPosFr , lnClrLnFr ),'COLOR')

  IF lcStyleFr # LEFT(STYLE.STYLE,lnLenth)
    REPLACE llPrnSty WITH .T.
    lcStyleFr = LEFT(STYLE.STYLE,lnLenth)
  ENDIF


  IF SEEK(STYLE.STYLE , "POSLN")
    SELECT POSLN
    SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = STYLE.STYLE
      IF POSLN.TRANCD = "1" AND POSHDR.STATUS $ "OH"
        SELECT (lcLinesTmp)
        REPLACE UntOwn    WITH UntOwn + POSLN.TOTQTY ,;
                VaLUntOwn WITH VaLUntOwn + ( POSLN.TOTQTY * STYLE.PriceA)
      ENDIF

      *--For all status.
      IF POSLN.TRANCD = "2"
        SELECT (lcLinesTmp)
        REPLACE TotRcpts   WITH TotRcpts + POSLN.TOTQTY ,;
                ValTotRcpt WITH ValTotRcpt + ( POSLN.TOTQTY * STYLE.PriceA)
      ENDIF

    ENDSCAN
  ENDIF


  IF SEEK(STYLE.STYLE , "ORDLINE")
    SELECT ORDLINE
    SCAN REST WHILE STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER + STORE + STR(LINENO,6) = STYLE.STYLE
      IF ORDHDR.STATUS $ "OH"
        SELECT (lcLinesTmp)
        REPLACE OpnOrd     WITH OpnOrd     + ORDLINE.TOTQTY                                                     ,;
                ValOpnOrd  WITH ValOpnOrd  + ( ORDLINE.TOTQTY * ORDLINE.PRICE)                                  ,;
                AprvdOrd   WITH IIF(ORDHDR.STATUS = "O" , AprvdOrd + ORDLINE.TOTQTY , 0 )                       ,;
                ValAprvOrd WITH IIF(ORDHDR.STATUS = "O" , ValAprvOrd + ( ORDLINE.TOTQTY * ORDLINE.PRICE) , 0 )  ,;
                CrdtOrd    WITH IIF(ORDHDR.STATUS = "H" , CrdtOrd + ORDLINE.TOTQTY , 0 )                        ,;
                ValCrdtOrd WITH IIF(ORDHDR.STATUS = "H" , ValCrdtOrd + ( ORDLINE.TOTQTY * ORDLINE.PRICE) , 0 )  ,;
                AllocOrd   WITH IIF(ORDHDR.STATUS = "O" , AllocOrd + ORDLINE.TOTPIK , 0 )                       ,;
                ValAlocOrd WITH IIF(ORDHDR.STATUS = "O" , ValAlocOrd + ( ORDLINE.TOTPIK * ORDLINE.PRICE) , 0 )

        IF (ORDLINE.PRICE * .9 ) > STYLE.PRICEA
          SELECT (lcLinesTmp)
          REPLACE UntSldPrm  WITH UntSldPrm + ORDLINE.TOTBOOK ,;
                  VaLUntSPrm WITH VaLUntSPrm + ( ORDLINE.TOTBOOK * ORDLINE.PRICE)
        ENDIF

        IF (ORDLINE.PRICE * .9 ) <= STYLE.PRICEA
          SELECT (lcLinesTmp)
          REPLACE UntSldOp  WITH UntSldOp  + ORDLINE.TOTBOOK ,;
                  ValUntSOp WITH ValUntSOp + ( ORDLINE.TOTBOOK * ORDLINE.PRICE)
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF

  IF SEEK(STYLE.STYLE , "INVLINE")
    SELECT INVLINE
    SCAN REST WHILE STYLE + INVOICE + STR(LINENO,6) = STYLE.STYLE FOR INVHDR.STATUS # "V"
      IF INVDATE <= DATE()
        SELECT (lcLinesTmp)
        REPLACE TotShip    WITH TotShip + INVLINE.TOTQTY ,;
                ValTotShip WITH ValTotShip + ( INVLINE.TOTQTY * lfPrcOrd() )
      ENDIF
    ENDSCAN
  ENDIF

  IF SEEK(STYLE.STYLE , "RETLINE")
    SELECT RETLINE
    SCAN REST WHILE STYLE + CRMEMO = STYLE.STYLE FOR RETHDR.STATUS # "V"
      SELECT (lcLinesTmp)
      REPLACE Rtrns    WITH Rtrns + RETLINE.TOTQTY ,;
              ValRtrns WITH ValRtrns + ( RETLINE.TOTQTY * RETLINE.PRICE)
    ENDSCAN
  ENDIF

  SELECT (lcLinesTmp)
  REPLACE BalToSell  WITH OpnWip + InStock - OpnOrd ,;
          ValBalToSl WITH ValBalToSl + ( (OpnWip + InStock - OpnOrd) * STYLE.PriceA )
  
ENDSCAN

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfPrcOrd
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
*! Purpose   : To get the ordline price for the style.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrcOrd()
*!*************************************************************
FUNCTION lfPrcOrd
PRIVATE lcAliasOrd , lcRecVal , lnPricOrd

lnPricOrd = 0
lcAliasOrd = SELECT(0)

SELECT ORDLINE
lcRecVal = EVAL(KEY())
IF SEEK(STYLE.STYLE , "ORDLINE")
  SCAN REST WHILE STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER + STORE + STR(LINENO,6) = STYLE.STYLE
    IF ORDLINE.ORDER = INVLINE.ORDER
      lnPricOrd = ORDLINE.PRICE
    ENDIF
  ENDSCAN
ENDIF
=SEEK(lcRecVal)

SELECT(lcAliasOrd)
RETURN lnPricOrd

*--End of lfPrcOrd.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
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
      *-- this means that another type is found rather than color or free.
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
*lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
lcColorTt = ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfMajTtGet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
*! Purpose   : To get the style major segement title.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
*! Purpose   : Get major seg. picture.
*!*************************************************************
*! Called from : Option Grid.
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

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
*! Purpose   : Go top in the style IN RANGE.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSetSty()
*!*************************************************************
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
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/29/2002
*! Purpose   : Validate style.
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
