*:***************************************************************************
*: Program file  : ICCOST
*: Program desc. : Costing
*: System        : Aria 4 XP
*: Module        : Inventory Control (IC)
*: Developer     : Wael M. Abo-Shawareb (WSH)
*: Date          : 05/23/2006
*: REF           : 037689,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCOST
*:***************************************************************************
*: Modifications:
*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[T20090601.0002]
*! B608930,2 MMT 07/19/2009 Fix bug of Error NiCost8 i not Found[T20090601.0002]
*! B608930,3 MMT 07/27/2009 Fix bug of Wrong Markup Value[T20090601.0002]
*:***************************************************************************

STORE ''  TO lcIcType1, lcIcType2, lcIcType3, lcIcType4, lcIcType5, lcIcType6, lcIcType7
STORE ''  TO lcMcType1, lcMcType2, lcMcType3, lcMcType4, lcMcType5, lcMcType6, lcMcType7
STORE ''  TO lcCISLBL1, lcCISLBL2, lcCISLBL3, lcCISLBL4, lcCISLBL5, lcCISLBL6, lcCISLBL7
STORE ''  TO lcCMSLBL1, lcCMSLBL2, lcCMSLBL3, lcCMSLBL4, lcCMSLBL5, lcCMSLBL6, lcCMSLBL7
STORE ''  TO lcStyFltr, lcFabFltr, lcClrFltr, lcGrpFltr, lcSesFltr, lcDivFltr
STORE ''  TO lcStyMark, lcCostMth, lcItemType
STORE '0' TO lcFType, lcTType, lcMType, lcMType2, lcSType, lcPType, lcDType

*-- lcRepNmTtl hold the header of the non major segment for the frx
*-- llDontPrn  if there is no records matched the criteria it is .F.
*-- lcTime     hold time
*-- lnMajLen   hold major segment lenght
llDontPrn  = .F.
lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
lcFabPic   =  gfItemMask("PM", "", "0002")
lnFabLen   =  LEN(SUBSTR(lcFabPic,4))

*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
IF llMultCur
  =gfOpenTable(OAriaApplication.SysPath+"SycCurr","CCURRCODE")
ENDIF 
*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]


=lfFillVars()

IF loOgScroll.llOGFltCh
  lcStTime = TIME()
  
  *-- Create the Temp File
  =lfCreateTemp()
  
  *-- Get OG Filters
  =lfGetFilters()
  
  *-- Collect Data for the Report
  =lfCollect()
  
  lcEdTime   = TIME()
  lnInterval = lfCollTime(lcStTime, lcEdTime)
  
  WAIT WINDOW "Selected " + ALLTRIM(STR(RECCOUNT(lcCostTmp))) + " Records in " + ALLTRIM(STR(lnInterval,6,2)) + " Seconds..." NOWAIT
ENDIF

IF !USED(lcCostTmp)
  USE (oAriaApplication.WorkDir + lcCostTmp + ".DBF") IN 0
ENDIF

SELECT (lcCostTmp)
LOCATE

IF EOF()
  =gfModalGen('TRM00052B00000', 'DIALOG')
  USE IN (lcCostTmp)
  RETURN
ENDIF
USE IN (lcCostTmp)

*-- Create report parameters and cursors arrays
=lfAdjustCRSettings()

=gfDispRe()

RETURN

*!*************************************************************
*! Name      : lfCollect
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/12/2006
*! Purpose   : Collecting Data
*!*************************************************************
*! Example   : = lfCollect()
*!*************************************************************
FUNCTION lfCollect

WAIT WINDOW "Collecting data for the report..." NOWAIT

PRIVATE lcBOMStat

*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
*lcBOMStat = "SELECT BOM.cInvType, BOM.cItmMajor, BOM.cCstShtTyp, BOM.cCstSht_Id, BOM.Typ, BOM.cItmMask," +;
            "       BOM.mfgCode, BOM.cInvTypC, BOM.[Item], BOM.nLineNo, BOM.cCatgTyp, BOM.nBOMTotQty," +;
            "       BOM.UntCost, BOM.TotCost, BOM.[Desc]" +;
            "  FROM BOMHEADR (INDEX = BOMHEADR)" +;
            " INNER JOIN BOM (INDEX = MULTIBOM)" +;
            "    ON BOMHEADR.cInvType = BOM.cInvType AND " +;
            "       BOMHEADR.cItmMajor = BOM.cItmMajor AND " +;
            "       BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND " +;
            "       BOMHEADR.cCstSht_Id = BOM.cCstSht_Id" +;
            " WHERE BOMHEADR.cInvType = '0001' AND " +;
                    IIF(!EMPTY(lcStyFltr) AND USED(lcStyFltr), "BOMHEADR.cItmMajor = ?m.cStyMajor AND ", "") +;
            "       BOMHEADR.cCstShtTyp = '" + IIF(lcRPDomImp = 'B', 'I', 'M') + "' AND " +;
            "       BOMHEADR.lDefCstSht = 1"
lcBOMStat = "SELECT BOM.cInvType, BOM.cItmMajor, BOM.cCstShtTyp, BOM.cCstSht_Id, BOM.Typ, BOM.cItmMask," +;
            "       BOM.mfgCode, BOM.cInvTypC, BOM.[Item], BOM.nLineNo, BOM.cCatgTyp, BOM.nBOMTotQty," +;
            "       BOM.UntCost, BOM.TotCost, BOM.[Desc],BOM.NCURRUNIT,BOM.NEXRATE,BOM.CCURRCODE " +;
            "  FROM BOMHEADR (INDEX = BOMHEADR)" +;
            " INNER JOIN BOM (INDEX = MULTIBOM)" +;
            "    ON BOMHEADR.cInvType = BOM.cInvType AND " +;
            "       BOMHEADR.cItmMajor = BOM.cItmMajor AND " +;
            "       BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND " +;
            "       BOMHEADR.cCstSht_Id = BOM.cCstSht_Id" +;
            " WHERE BOMHEADR.cInvType = '0001' AND " +;
                    IIF(!EMPTY(lcStyFltr) AND USED(lcStyFltr), "BOMHEADR.cItmMajor = ?m.cStyMajor AND ", "") +;
            "       BOMHEADR.cCstShtTyp = '" + IIF(lcRPDomImp = 'B', 'I', 'M') + "' AND " +;
            "       BOMHEADR.lDefCstSht = 1"
           
*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]
SELECT Style
=gfSetOrder(IIF(lcRPPrntBy = 'C', "STYLE", "CSTYLE"))


LOCAL lcPrevMajor
lcPrevMajor = SPACE(19)

*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
lcStyMajPrev = SPACE(19)
*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]
*-- If user select Styles from OG ...
IF !EMPTY(lcStyFltr) AND USED(lcStyFltr)
  SELECT (lcStyFltr)
  SCAN
    m.cStyMajor = EVALUATE(lcStyFltr + '.CSTYMAJOR')
    
    SELECT STYLE
    =gfSeek(SUBSTR(m.cStyMajor, 1, lnMajLen))
    
    WAIT WINDOW  lcMajTtl + ': ' + m.cStyMajor NOWAIT
        
    SELECT STYLE
    SCAN REST WHILE STYLE = SUBSTR(m.cStyMajor, 1, lnMajLen) FOR &lcRpExp
      IF !lfMatchCond()
        LOOP
      ENDIF
    
      
      *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
      *IF lcRPPrntBy = 'C' AND STYLE.lDetCost AND lcPrevMajor <> Style.cStyMajor
      IF (lcRPPrntBy = 'C' OR llMultCur) AND STYLE.lDetCost AND lcPrevMajor <> Style.cStyMajor 
      *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]
        SELECT BOM
        =gfSQLRun(lcBOMStat, "BOM")
        lcPrevMajor = STYLE.cStyMajor
      ENDIF
	
      
      =lfStyCollect()
       *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
      lcStyMajPrev  = lcPrevMajor 
       *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]
    ENDSCAN
  ENDSCAN
ELSE
  *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
  *IF lcRPPrntBy = 'C'
  IF lcRPPrntBy = 'C' OR llMultCur
  *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]
    SELECT BOM
    =gfSQLRun(lcBOMStat, "BOM")
  ENDIF
  
  SELECT STYLE
  llExitLoop = !gfGoTop()
  DO WHILE !llExitLoop
    llLoop = !(&lcRpExp.)
    llLoop = llLoop OR !lfMatchCond()
    IF llLoop
      SELECT STYLE
      llExitLoop = !gfGoNext()
      LOOP
    ENDIF  
    
    SELECT Style
    =lfStyCollect()
    
    SELECT STYLE
    llExitLoop = !gfGoNext()
  ENDDO
ENDIF

SELECT Style
=gfSetOrder("CSTYLE")

*!*************************************************************
*! Name      : lfMatchCond
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/12/2006
*! Purpose   : Match record with selected Filters
*!*************************************************************
*! Example   : =lfMatchCond()
*!*************************************************************
FUNCTION lfMatchCond

*-- If record does not match OG Selection, don't insert..
IF lcRPPrntBy = 'C' AND !EMPTY(lcClrFltr) AND !SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen), lcClrFltr)
  RETURN .F.
ENDIF
IF !EMPTY(lcFabFltr) AND !SEEK(IIF(!EMPTY(STYLE.cPriFabric), LEFT(STYLE.cPriFabric, 12), STYLE.Fabric), lcFabFltr)
  RETURN .F.
ENDIF
IF !EMPTY(lcSesFltr) AND !SEEK(STYLE.SEASON, lcSesFltr)
  RETURN .F.
ENDIF
IF !EMPTY(lcDivFltr) AND !SEEK(STYLE.CDIVISION, lcDivFltr)
  RETURN .F.
ENDIF
IF !EMPTY(lcGrpFltr) AND !SEEK(STYLE.cStyGroup, lcGrpFltr)
  RETURN .F.
ENDIF

IF lnClrPo <> 0 AND !EMPTY(lcClrFltr) AND !SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen), lcClrFltr)
  RETURN .F.
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfStyCollect
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/06/2006
*! Purpose   : Function to get a style data.
*!*************************************************************
*! Example   : =lfStyCollect()
*!*************************************************************
FUNCTION lfStyCollect

LOCAL lnAlias
lnAlias = SELECT(0)

=gfSeek('S' + STYLE.Scale, "SCALE")

SELECT STYLE
SCATTER MEMVAR
*!*  m.cSesDesc = IIF(STYLE.SEASON = 'Y ', 'YEAR ROUND', lfCodeDesc(STYLE.SEASON, "SEASON"))
*!*  m.cDivDesc = lfCodeDesc(STYLE.CDIVISION, "CDIVISION")
*!*  m.cGrpDesc = lfCodeDesc(STYLE.CSTYGROUP, "CSTYGROUP")
m.lDetCost = .F.
m.COLOR    = SUBSTR(STYLE.Style, lnNonMajPo)
m.Sz1      = SCALE.Sz1
m.Sz2      = SCALE.Sz2
m.Sz3      = SCALE.Sz3
m.Sz4      = SCALE.Sz4
m.Sz5      = SCALE.Sz5
m.Sz6      = SCALE.Sz6
m.Sz7      = SCALE.Sz7
m.Sz8      = SCALE.Sz8

IF lcRPDomImp = 'M'
  m.NICost1 = m.NMCost1
  m.NICost2 = m.NMCost2
  m.NICost3 = m.NMCost3
  m.NICost4 = m.NMCost4
  m.NICost5 = m.NMCost5
  m.NICost6 = m.NMCost6
  m.NICost7 = m.NMCost7
ENDIF
m.TotCost  = m.NICost1 + m.NICost2 + m.NICost3 + m.NICost4 + m.NICost5 + m.NICost6 + m.NICost7
m.StTotCst = m.TotCost


*! B608930,3 MMT 07/27/2009 Fix bug of Wrong Markup Value[Start]
IF llMultCur AND IIF(lcRPPrntBy = 'C',.T.,lcStyMajPrev <> Style.cStyMajor)
   m.NICost1 = 0
   m.NICost2 = 0
   m.NICost3 = 0
   m.NICost4 = 0
   m.NICost5 = 0
   m.NICost6 = 0
   m.NICost7 = 0
   SELECT BOM
  =SEEK('0001' + STYLE.cStyMajor)
  SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) = '0001' + STYLE.cStyMajor ;
	FOR LIKE(STRTRAN(cItmMask, '*','?'), STYLE.Style) AND INLIST(BOM.Typ,'1','2','3','4','5','6','7')
    lcCurrency  = BOM.CCURRCODE
    lcTyp = BOM.Typ
	lnCurrUnit = Bom.nCurrUnit
	lcUnitSign = "\"
	lcRateSign = "\"
	lnExRate = Bom.nExRate
    IF  gfSEEK(lcCurrency , 'SycCurr')
      lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
    ENDIF 
    m.TotCost = ROUND(EVALUATE('BOM'+'.TotCost'+lcRateSign+;
                                        STR(lnExRate ,9,4)+lcUnitSign+ STR(lnCurrUnit)),2) 
    m.nICost&lcTyp = m.nICost&lcTyp + m.TotCost           
  ENDSCAN           
  m.TotCost  = m.NICost1 + m.NICost2 + m.NICost3 + m.NICost4 + m.NICost5 + m.NICost6 + m.NICost7
  m.StTotCst = m.TotCost
ENDIF 
SELECT STYLE
*! B608930,3 MMT 07/27/2009 Fix bug of Wrong Markup Value[End]


*-- Calculate Markup
IF PRICEA <> 0 .AND. (IIF(lcRPMarkUp = "A", m.AVE_COST <> 0, m.TOTCOST <> 0))
  IF lcStyMark = 'T'
    m.MARKA = ((PRICEA - IIF(lcRPMarkUp = "A", m.AVE_COST, m.TOTCOST)) / ;
              (IIF(lcCostMth <> 'S', m.AVE_COST, m.TOTCOST))) * 100
  ELSE
    m.MARKA = ((PRICEA - IIF(lcRPMarkUp = "A", m.AVE_COST, m.TOTCOST)) / PRICEA) * 100
  ENDIF
ELSE
  m.MARKA = 0
ENDIF

IF PRICEB <> 0 .AND. (IIF(lcRPMarkUp = "A", m.AVE_COST <> 0, m.TOTCOST <> 0))
  IF lcStyMark = 'T'
    m.MARKB = ((PRICEB - IIF(lcRPMarkUp = "A", m.AVE_COST, m.TOTCOST)) / ;
              (IIF(lcCostMth <> 'S', m.AVE_COST, m.TOTCOST))) * 100
  ELSE
    m.MARKB = ((PRICEB - IIF(lcRPMarkUp = "A", m.AVE_COST, m.TOTCOST)) / PRICEB) * 100
  ENDIF
ELSE
  m.MARKB = 0
ENDIF

IF PRICEC <> 0 .AND. (IIF(lcRPMarkUp = "A", m.AVE_COST <> 0, m.TOTCOST <> 0))
  IF lcStyMark = 'T'
    m.MARKC = ((PRICEC - IIF(lcRPMarkUp = "A", m.AVE_COST, m.TOTCOST)) / ;
              (IIF(lcRPMarkUp = "A", m.AVE_COST,  m.TOTCOST))) * 100
  ELSE
    m.MARKC = ((PRICEC - IIF(lcRPMarkUp = "A", m.AVE_COST, m.TOTCOST)) / PRICEC) * 100
  ENDIF
ELSE
  m.MARKC = 0
ENDIF

*! B608930,3 MMT 07/27/2009 Fix bug of Wrong Markup Value[Start]
*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
*!*	IF llMultCur AND IIF(lcRPPrntBy = 'C',.T.,lcStyMajPrev <> Style.cStyMajor)
*!*	   m.NICost1 = 0
*!*	   m.NICost2 = 0
*!*	   m.NICost3 = 0
*!*	   m.NICost4 = 0
*!*	   m.NICost5 = 0
*!*	   m.NICost6 = 0
*!*	   m.NICost7 = 0
*!*	   SELECT BOM
*!*	  =SEEK('0001' + STYLE.cStyMajor)
*!*	  *! B608930,2 MMT 07/19/2009 Fix bug of Error NiCost8 i not Found[Start]
*!*	*!*	  SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) = '0001' + STYLE.cStyMajor ;
*!*	*!*		FOR LIKE(STRTRAN(cItmMask, '*','?'), STYLE.Style)
*!*	  SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) = '0001' + STYLE.cStyMajor ;
*!*		FOR LIKE(STRTRAN(cItmMask, '*','?'), STYLE.Style) AND INLIST(BOM.Typ,'1','2','3','4','5','6','7')
*!*	  *! B608930,2 MMT 07/19/2009 Fix bug of Error NiCost8 i not Found[End]	
*!*	    lcCurrency  = BOM.CCURRCODE
*!*	    lcTyp = BOM.Typ
*!*		lnCurrUnit = Bom.nCurrUnit
*!*		lcUnitSign = "\"
*!*		lcRateSign = "\"
*!*		lnExRate = Bom.nExRate
*!*	    IF  gfSEEK(lcCurrency , 'SycCurr')
*!*	      lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
*!*	    ENDIF 
*!*	    m.TotCost = ROUND(EVALUATE('BOM'+'.TotCost'+lcRateSign+;
*!*	                                        STR(lnExRate ,9,4)+lcUnitSign+ STR(lnCurrUnit)),2) 
*!*	    m.nICost&lcTyp = m.nICost&lcTyp + m.TotCost           
*!*	  ENDSCAN           
*!*	  m.TotCost  = m.NICost1 + m.NICost2 + m.NICost3 + m.NICost4 + m.NICost5 + m.NICost6 + m.NICost7
*!*	  m.StTotCst = m.TotCost
*!*	ENDIF 
*! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]
*! B608930,3 MMT 07/27/2009 Fix bug of Wrong Markup Value[End]


llFound = .F.
IF lcRPPrntBy = 'C' AND STYLE.lDetCost
  SELECT BOM
  =SEEK('0001' + STYLE.cStyMajor)
  SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) = '0001' + STYLE.cStyMajor ;
            FOR   &lcItemType .AND. (LIKE(STRTRAN(cItmMask, '*','?'), STYLE.Style) OR (Typ $ lcMType+lcMType2))

    llFound  = .T.
    
    lnItmLen = IIF(cInvTypC = '0002', lnFabLen, lnMajLen)
    lcClr    = SUBSTR(Item, lnItmLen + 2)
    lnStrt   = ATC("*", lcClr)
    lnLen    = RATC("*", lcClr) - ATC("*", lcClr) + 1
    lcClr    = IIF(lnStrt = 0, lcClr, SUBSTR(lcClr, 1, lnStrt - 1) + SUBSTR(m.Color, lnStrt, lnLen) + SUBSTR(lcClr, lnStrt + lnLen))
    
    m.Item       = SUBSTR(BOM.Item, 1, lnItmLen + 1) + lcClr
    m.cCatgTyp   = BOM.cCatgTyp
    m.nBOMTOTQTY = BOM.nBOMTOTQTY
    m.UNTCOST    = BOM.UNTCOST
    m.TotCost    = BOM.TotCost
    m.BOMDesc    = IIF(!EMPTY(BOM.mfgCode), BOM.Desc, m.Item)
    m.lDetCost   = .T.
    
    *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[Start]
    IF llMultCur AND oAriaApplication.BaseCurrency <> BOM.CCURRCODE
	  lcCurrency  = BOM.CCURRCODE
	  lcTyp = BOM.Typ
	  lnCurrUnit = Bom.nCurrUnit
	  lcUnitSign = "\"
	  lcRateSign = "\"
	  lnExRate = Bom.nExRate
      IF  gfSEEK(lcCurrency , 'SycCurr')
        lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
      ENDIF 
      m.UNTCOST = ROUND(EVALUATE('BOM'+'.UNTCOST'+lcRateSign+;
                                        STR(lnExRate ,9,4)+lcUnitSign+ STR(lnCurrUnit)),2)    
      m.TotCost = ROUND(EVALUATE('BOM'+'.TotCost'+lcRateSign+;
                                        STR(lnExRate ,9,4)+lcUnitSign+ STR(lnCurrUnit)),2) 
	  
    ENDIF 
    *! B608930,1 MMT 07/08/2009 Fix bug of wrong cost if Cost Entred in Foreign currency[End]
    SELECT (lcCostTmp)
    APPEND BLANK
    GATHER MEMVAR
  ENDSCAN
ENDIF






IF !llFound
  SELECT (lcCostTmp)
  APPEND BLANK
  GATHER MEMVAR
ENDIF

SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfFillVars
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/08/2006
*! Purpose   : Fill needed variables
*!**************************************************************************
*! Example   : =lfFillVars()
*!**************************************************************************
FUNCTION lfFillVars

*-- Fill Cost Item Types variables
DIMENSION laSetUps[30,2]
laSetUps[01,1]  = 'M_CMTYPE1'
laSetUps[02,1]  = 'M_CMTYPE2'
laSetUps[03,1]  = 'M_CMTYPE3'
laSetUps[04,1]  = 'M_CMTYPE4'
laSetUps[05,1]  = 'M_CMTYPE5'
laSetUps[06,1]  = 'M_CMTYPE6'
laSetUps[07,1]  = 'M_CMTYPE7'

laSetUps[08,1]  = 'M_CITYPE1'
laSetUps[09,1]  = 'M_CITYPE2'
laSetUps[10,1]  = 'M_CITYPE3'
laSetUps[11,1]  = 'M_CITYPE4'
laSetUps[12,1]  = 'M_CITYPE5'
laSetUps[13,1]  = 'M_CITYPE6'
laSetUps[14,1]  = 'M_CITYPE7'

laSetUps[15,1]  = 'M_CISLBL1'
laSetUps[16,1]  = 'M_CISLBL2'
laSetUps[17,1]  = 'M_CISLBL3'
laSetUps[18,1]  = 'M_CISLBL4'
laSetUps[19,1]  = 'M_CISLBL5'
laSetUps[20,1]  = 'M_CISLBL6'
laSetUps[21,1]  = 'M_CISLBL7'

laSetUps[22,1]  = 'M_CMSLBL1'
laSetUps[23,1]  = 'M_CMSLBL2'
laSetUps[24,1]  = 'M_CMSLBL3'
laSetUps[25,1]  = 'M_CMSLBL4'
laSetUps[26,1]  = 'M_CMSLBL5'
laSetUps[27,1]  = 'M_CMSLBL6'
laSetUps[28,1]  = 'M_CMSLBL7'

laSetUps[29,1]  = 'M_STYMARK'
laSetUps[30,1]  = 'M_COST_MET'

= gfGetMemVar(@laSetups)

lcIcType1 = laSetUps[01,2]
lcIcType2 = laSetUps[02,2]
lcIcType3 = laSetUps[03,2]
lcIcType4 = laSetUps[04,2]
lcIcType5 = laSetUps[05,2]
lcIcType6 = laSetUps[06,2]
lcIcType7 = laSetUps[07,2]

lcMcType1 = laSetUps[08,2]
lcMcType2 = laSetUps[09,2]
lcMcType3 = laSetUps[10,2]
lcMcType4 = laSetUps[11,2]
lcMcType5 = laSetUps[12,2]
lcMcType6 = laSetUps[13,2]
lcMcType7 = laSetUps[14,2]

lcCISLBL1 = LEFT(laSetUps[15,2],9)
lcCISLBL2 = LEFT(laSetUps[16,2],9)
lcCISLBL3 = LEFT(laSetUps[17,2],9)
lcCISLBL4 = LEFT(laSetUps[18,2],9)
lcCISLBL5 = LEFT(laSetUps[19,2],9)
lcCISLBL6 = LEFT(laSetUps[20,2],9)
lcCISLBL7 = LEFT(laSetUps[21,2],9)

lcCMSLBL1 = LEFT(laSetUps[22,2],9)
lcCMSLBL2 = LEFT(laSetUps[23,2],9)
lcCMSLBL3 = LEFT(laSetUps[24,2],9)
lcCMSLBL4 = LEFT(laSetUps[25,2],9)
lcCMSLBL5 = LEFT(laSetUps[26,2],9)
lcCMSLBL6 = LEFT(laSetUps[27,2],9)
lcCMSLBL7 = LEFT(laSetUps[28,2],9)

lcStyMark = laSetUps[29,2]
lcCostMth = laSetUps[30,2]

IF lcRPDomImp = 'M'
  FOR I=1 TO 5
    Z=STR(I,1)
    DO CASE
     CASE lcMcType&Z = 'F'
       lcFType = Z
     CASE lcMcType&Z = 'T'
       lcTType = Z
     CASE lcMcType&Z = 'M' .AND. lcMType = '0'
       lcMType = Z
     CASE lcMcType&Z = 'S'
       lcSType = Z
     CASE lcMcType&Z = 'M' .AND. lcMType <> '0'
       lcMType2 = Z
    ENDCASE 
  ENDFOR
ELSE
  FOR I = 1 TO 5
    Z = STR(I,1)
    DO CASE
     CASE lcIcType&Z = 'P'
       lcPType = Z
     CASE lcIcType&Z = 'S'
       lcSType = Z
     CASE lcIcType&Z = 'D'
       lcDType = Z
     CASE lcIcType&Z = 'F'
       lcFType = Z
     CASE lcIcType&Z = 'M'
       lcMType  = Z
       lcMType2 = Z
    ENDCASE 
  ENDFOR
ENDIF

IF BETWEEN(lcRPPrnItm, "1", "7")
 lcItemType = "TYP = lcRPPrnItm"
ELSE
 lcItemType = IIF(lcRPPrnItm = 'A', '.T.', '.F.')
ENDIF

RETURN

*!**************************************************************************
*! Name      : lfGetFilters
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/08/2006
*! Purpose   : Adjust OG Filters
*!**************************************************************************
*! Example   : =lfGetFilters()
*!**************************************************************************
FUNCTION lfGetFilters

LOCAL lnAlias, lcCurName, llFound, lcCond, lcStatus
lnAlias = SELECT(0)

*-- Doemstic or Importing Flag Filter
*** lcRpExp = IIF(lcRPDomImp = "M", "STYLE.MAKE", "!STYLE.MAKE")
lcRpExp = ".T."

*-- Status Filter
lcStatus = lfCheckFilter(3, "STYLE.STATUS")
lcRpExp  = lcRpExp + IIF(EMPTY(lcStatus), "", " AND STYLE.STATUS $ '" + lcStatus + "'")

*-- NonMajor Filter
lcCond  = lfCheckFilter(3, "SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)")
lcRpExp = lcRpExp + IIF(EMPTY(lcCond), "", " AND SUBSTR(STYLE.Style," + lnNonMajPo + "," + lnFreeLen + ") = '" + lcCond + "'")

*-- Style Filter
lcCurName = lfCheckFilter(3, 'STYLE.CSTYMAJOR')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcStyFltr = lcCurName
  SELECT (lcStyFltr)
  INDEX ON cStyMajor TAG (lcStyFltr)
ELSE
  IF TYPE("lcStyFltr") = "C" AND USED(lcStyFltr)
    USE IN (lcStyFltr)
  ENDIF
  lcStyFltr = ''
ENDIF

*-- Fabric Filter
lcCurName = lfCheckFilter(3, 'STYLE.FABRIC')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcFabFltr = lcCurName
  SELECT (lcFabFltr)
  INDEX ON cStyMajor TAG (lcFabFltr)
ELSE
  IF TYPE("lcFabFltr") = "C" AND USED(lcFabFltr)
    USE IN (lcFabFltr)
  ENDIF
  lcFabFltr = ''
ENDIF

*-- Color Filter
IF lnClrPo <> 0
  lcCond = lfCheckFilter(3, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')
  IF !EMPTY(lcCond)
    lcClrFltr = loOgScroll.gfTempName()
    CREATE CURSOR (lcClrFltr) (Color C(6))
    DIMENSION laValues[1]
    =gfSubStr(lcCond, @laValues, '|')
    SELECT (lcClrFltr)
    INDEX ON Color TAG (lcClrFltr)
    FOR lnI = 1 TO ALEN(laValues,1)
      APPEND BLANK
      REPLACE Color WITH laValues[lnI]
    ENDFOR
  ELSE
    IF TYPE("lcClrFltr") = "C" AND USED(lcClrFltr)
      USE IN (lcClrFltr)
    ENDIF
    lcClrFltr = ''
  ENDIF
ENDIF

*-- Groups Filter
lcCond = lfCheckFilter(3, 'STYLE.CSTYGROUP')
IF !EMPTY(lcCond)
  lcGrpFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcGrpFltr) (cStyGroup C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcGrpFltr)
  INDEX ON cStyGroup TAG (lcGrpFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE cStyGroup WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcGrpFltr") = "C" AND USED(lcGrpFltr)
    USE IN (lcGrpFltr)
  ENDIF
  lcGrpFltr = ''
ENDIF

*-- Season Filter
lcCond = lfCheckFilter(3, 'STYLE.SEASON')
IF !EMPTY(lcCond)
  lcSesFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcSesFltr) (Season  C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcSesFltr)
  INDEX ON SEASON TAG (lcSesFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE SEASON WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcSesFltr") = "C" AND USED(lcSesFltr)
    USE IN (lcSesFltr)
  ENDIF
  lcSesFltr = ''
ENDIF

*-- Division Filter
lcCond = lfCheckFilter(3, 'STYLE.CDIVISION')
IF !EMPTY(lcCond)
  lcDivFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcDivFltr) (CDIVISION C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcDivFltr)
  INDEX ON CDIVISION TAG (lcDivFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE CDIVISION WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcDivFltr") = "C" AND USED(lcDivFltr)
    USE IN (lcDivFltr)
  ENDIF
  lcDivFltr = ''
ENDIF

SELECT (lnAlias)
RETURN
*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
FUNCTION lfCollTime
LPARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart   = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd     = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))

RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*************************************************************
*! Name      : lfCreateTemp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/12/2006
*! Purpose   : Create temp file and add needed fields
*!*************************************************************
FUNCTION lfCreateTemp

*-- Create the temporary file from the ORDCANLN table and add some necessary fields to it.
SELECT STYLE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

DIMENSION laFileStru[lnFileStru+18,18]
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'StTotCst'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 11
laFileStru[lnFileStru,4] = 3
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'cSesDesc'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'cDivDesc'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'cGrpDesc'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'Color'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'Item'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 19
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'cCatgTyp'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'BOMDesc'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 20
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'nBOMTOTQTY'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 7
laFileStru[lnFileStru,4] = 3
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'UNTCOST'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 11
laFileStru[lnFileStru,4] = 3
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru,1] = 'Sz1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz2'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz3'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz4'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz5'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz6'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz7'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz8'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

FOR lnI = 7 TO 16
  FOR lnJ = 1 TO lnFileStru
    STORE '' TO laFileStru[lnJ,lnI]
  ENDFOR
ENDFOR
FOR lnI = 1 TO lnFileStru
  STORE 0 TO laFileStru[lnI,17], laFileStru[lnI,18]
ENDFOR

=gfCrtTmp(lcCostTmp, @laFileStru, "Style+Color+cCatgTyp+Item", lcCostTmp)

SELECT (lcCostTmp)
SET ORDER TO (lcCostTmp)

RETURN
*-- End of lfCreateTemp.

*!*************************************************************
*! Name      : lfCodeDesc
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : Get code description
*!*************************************************************
*! Example   : =lfCodeDesc(Reason, "REASON")
*!*************************************************************
FUNCTION lfCodeDesc
LPARAMETERS lcCodeVal, lcCodeFld

LOCAL lnAlias, lcRetVal
lnAlias  = SELECT(0)
lcRetVal = ''

SELECT Codes
IF gfSeek("N" + lcCodeVal + "N" + lcCodeFld)
  lcRetVal = cDiscrep
ENDIF

SELECT (lnAlias)
RETURN lcRetVal

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/12/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcCostTmp + ".DBF"

DIMENSION loOgScroll.laCRParams[13,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'Style Costing Report'

loOgScroll.laCRParams[2,1] = 'op_Title'
loOgScroll.laCRParams[2,2] = lcRPTitle

loOgScroll.laCRParams[3,1] = 'StyleTtl'
loOgScroll.laCRParams[3,2] = lcMajTtl

loOgScroll.laCRParams[4,1] = 'ColorTtl'
loOgScroll.laCRParams[4,2] = lcNonMajT

loOgScroll.laCRParams[5,1] = 'SortBy'
loOgScroll.laCRParams[5,2] = IIF(lcRPPrntBy = 'C', 'C', 'S')

loOgScroll.laCRParams[6,1] = 'Currency'
loOgScroll.laCRParams[6,2] = oAriaApplication.BaseCurrency

lcLabel = "lcC" + IIF(lcRPDomImp = 'M', 'M', 'I') + "SLBL"
FOR lnI = 1 TO 7
  lcI = ALLTRIM(STR(lnI))
  loOgScroll.laCRParams[6 + lnI, 1] = 'CostTtl' + lcI
  loOgScroll.laCRParams[6 + lnI, 2] = ALLTRIM(EVALUATE(lcLabel + lcI))
ENDFOR

loogScroll.cCROrientation = 'L'

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
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

*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

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
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajT) + 's'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/12/2006
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

= gfOpenTable(oAriaApplication.DataDir + 'CODES', 'CODES', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'STYLE', 'STYLE', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'BOM', 'MULTIBOM', 'SH')

*-- Check the cost access
lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')

laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')

laOGObjCnt[4] = .F.
= lfOGShowGet('lcRPPrnItm')
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvDomImp
*! Developer : Wael M. ABo-Shawareb (WSH)
*! Date      : 06/08/2006
*! Purpose   : validate Domestic/Import Style Selection
*!*************************************************************
*! Example   : = lfvDomImp()
*!*************************************************************
FUNCTION lfvDomImp
PARAMETER lcLoadOG

*-- Use To Display OG PopUp Get Cost No.     Get Setup Values
DIMENSION  laRPCstDsp[9,1], laRPCstRet[9,1], laCstSetUp[7,2]

*-- Get Cost Labels From Setup for either Manufatured or Imported Styles
lcImpOrMfg = IIF(TYPE("lcRPDomImp") # "C" .OR. lcRPDomImp = "M","M","I")
FOR lnNo = 1 TO 7
  laCstSetUp[lnNo,1]  = 'M_C'+ lcImpOrMfg +'SLBL'+STR(lnNo,1)
ENDFOR
= gfGetMemVar(@laCstSetUp)

*-- Fill Popup and Equivelant Cost No. arrays
FOR lnNo = 1 TO 7
  laRPCstDsp[lnNo,1] = laCstSetUp[lnNo,2]
  laRPCstRet[lnNo,1] = STR(lnNo,1)
ENDFOR

*-- Add None and All to the arrays
laRPCstDsp[8,1] = "None"
laRPCstDsp[9,1] = "All"
laRPCstRet[8,1] = 'N'
laRPCstRet[9,1] = 'A'

*-- If this variable is defined that means that the OG is active
*-- and we have to refresh the Print Item PopUp
IF TYPE("lcLoadOG") = "C"  .AND. lcLoadOG # "Y"
  = lfOGShowGet('lcRPPrnItm')
ENDIF

*!*************************************************************
*! Name      : lfvPrintBy
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate print by option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrintBy()
*!*************************************************************

FUNCTION lfvPrintBy

lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

IF lcRPPrntBy = 'C'
  DO CASE
    CASE lcFreeClr = 'C'
      laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
      = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
      laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
      = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    CASE lcFreeClr = 'F'
      laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
      = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
      laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
      = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  ENDCASE
  laOGObjCnt[4] = .T.
  = lfOGShowGet('lcRPPrnItm')
ELSE
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
  = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
  = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
  laOGObjCnt[4] = .F.
  = lfOGShowGet('lcRPPrnItm')
ENDIF

*!*************************************************************
*! Name      : lfAdjPrnArr
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Prepare the arrays that hold the print by values 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjPrnArr()
*!*************************************************************
FUNCTION lfAdjPrnArr

DIMENSION laRPPrnDsp[2,1],laRPPrnRet[2,1]
laRPPrnDsp[1,1] = lcMajTtl
laRPPrnDsp[2,1] = lcNonMajT

laRPPrnRet[1,1] = 'S'
laRPPrnRet[2,1] = 'C'

=lfvDomImp("Y")

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
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

*!*************************************************************
*! Name      : lfStySum
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/12/2006
*! Purpose   : Sum a specific field for the current style in style file.
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar

PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
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
*--End of lfStySum.

*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/12/2006
*! Purpose   : 
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