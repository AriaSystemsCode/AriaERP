*:**********************************************************************
*: Program file        : MAMATRQ.PRG
*: Program description : Material Requirements Report.
*:                     : Time Phased Material Requirements Report.
*: Module              : Materials (MA)
*: Developer           : Wael M. Abo-Shawareb (WSH)
*: Tracking Job Number : 037635
*: Date                : 06/09/2005
*:**********************************************************************
*: Calls:
*:   Screens: MAEDTPJ.SPX  Edit projection.
*:          : MAEDTFB.SPX  Edit -ve remaining materials.
*:   Reports: MAMATRQ.FRX  Summary format.
*:          : MAMATRQH.FRX Detail Short format.
*:          : MAMATRQL.FRX Detail Long format.
*B607923,1 MMT 01/05/2007 Material Requirement Report not Calculating Correctly   T20061214.0019
*B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale T20070116.0006
*B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP T20070221.0012
*B608117,1 MMT 06/08/2007 fix bug of not taking c/t into consioderation while Calc. WIP [T20070530.0025]
*B608122,1 TMI 06/13/2007 Call the gfSequence global function with passing the new parameters lcTranType,lcTable,lcTag ( T20070524.0013)
*B608152,1 MMT 07/04/2007 fix bug of error while saving filters in Mat. Req. rep   [T20070605.0014]
*B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [T20090522.0004]
*B609094,1 WAM 11/19/2009 Fix bug of duplicate yield printed when select style major in the option grid [T20091113.0002]
*B609356,1 SMA 07/25/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*B609665,1 WAM 08/23/2011 Create one PO for each vendor\complete date currency [ T20110707.0010]
*E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10
*B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[T2012326.0001]
*B609959,1 MMT 06/12/2012 Incorrect Remaining MT When same style is found in PO more than once[T20120529.0001]
*B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001]
*B610568,1 TMI 10/29/2013 "fixing the Requirement report error that appears when selecting "Time phased MRP" as yes" [T20131029.0001 task] 
*:**********************************************************************
*:
lcTime = SECONDS()
*-- intializing all variables.
IF TYPE('lcTmpRmain') $ 'UL'
  lcTmpRmain = ''
ENDIF

IF TYPE('lcMatReq') $ 'UL'
  lcMatReq = ''
ENDIF

IF TYPE('lcTmpItems') $ 'UL'
  lcTmpItems = ''
ENDIF

IF TYPE('lcPOTmpHD') $ 'UL'
  lcPOTmpHD = ''
ENDIF

IF TYPE('lcPOTmpLN') $ 'UL'
  lcPOTmpLN = ''
ENDIF

*-- Add variable to save the MRP report
PRIVATE llSaveMrp,lcMrp,laSaveVar
STORE .F. TO llSaveMrp
STORE SPACE(0) TO lcMrp
DECLARE laSaveVar[20,2]
laSaveVar[01,1] = 'lcDescrp'
laSaveVar[02,1] = 'lnOnHand'
laSaveVar[03,1] = 'lnOnOrdr'
laSaveVar[04,1] = 'lnAvl1'
laSaveVar[05,1] = 'lnAvl2'
laSaveVar[06,1] = 'lnAvl3'
laSaveVar[07,1] = 'lnAvl4'
laSaveVar[08,1] = 'lnAvl5'
laSaveVar[09,1] = 'lnAvl6'
laSaveVar[10,1] = 'lnAvl7'
laSaveVar[11,1] = 'lnAvl8'
laSaveVar[12,1] = 'lcFDesc'
laSaveVar[13,1] = 'lcClrDsc'
laSaveVar[14,1] = 'lcFabVen'
laSaveVar[15,1] = 'lnConv'
laSaveVar[16,1] = 'lnLeadTm'
laSaveVar[17,1] = 'lnFabcost'
laSaveVar[18,1] = 'lcUOMBuy'
laSaveVar[19,1] = 'lnfUOM'
laSaveVar[20,1] = 'llOTC'

STORE 0 TO lnOTotRem , lnHTotRem , lnPTotRem , lnOIssued , lnHIssued , lnPIssued, lnWorkDay
STORE '' TO lcFDesc , lcClrDsc

*-- Print the fabric openqty by yards for Mexx
lnfUOM = 1

*-- Add variable to get the use vendor referance setup [Start]
llVenRef = (gfGetMemVar('M_VENREF  ') = 'Y')

*--Use extended size scale.
llMScale  = gfGetMemVar('M_USEEXSSC')

*--Show OTC equal to grid variable.
llOtc      = (lcRpReqBas='O')
lcBaseProj = lcRpBasePj

*--Need to update plane in style file.
llUpdPlan  = .F.

*--Flag for style by location or no (Lcation was entered in grid).
llByLoctn  = ('STYDYE.' $ loOGScroll.lcRpExp)

*--Flag for fabric by location or no (Lcation was entered in grid).
llByFabLoc = ('ITEMLOC.' $ loOGScroll.lcRpExp)

lcForWare = IIF(llByLoctn, "SEEK(cWareCode, lcLocFile)" , ".T." )

*-- Create Tables objects for required tables...
PRIVATE loStyle,loStydye,loBom,loScale,loFabDye, loPofLn,loMmfgOrdh,loMmfgOrdd,loFabric,loOrdHdr,loCutPick
PRIVATE loBomLine,loCtktBom,loPosHdr, loFISHD, loFORCAST, loOrdLine, loVendMatH, loApVendor
PRIVATE loPosLn,loCutTktH,loCutTktl,loCodes,MfgOprHd, loWareHous, loInvLine, loVendMatL
STORE .NULL. TO loStyle,loStydye,loBom,loScale,loFabDye,loUOM,loPofLn,loPofHdr, loApVendor
STORE .NULL. TO loBomLine,loCtktBom,loPosHdr, loFISHD, loFORCAST, loOrdLine, loVendMatH, loOrdHdr
STORE .NULL. TO loPosLn,loCutTktH,loCutTktl,loCodes,MfgOprHd, loWareHous, loInvLine, loVendMatL

loBomLine  = CREATEOBJECT('RemoteTable', 'BOMLINE', 'BOMLINE', 'BOMLINE', SET("Datasession"))
loCtktBom  = CREATEOBJECT('RemoteTable', 'CTKTBOM', 'CTKTBOM', 'CTKTBOM', SET("Datasession"))
loFISHD    = CREATEOBJECT('RemoteTable', 'FISHD', 'COMPFYEAR', 'FISHD', SET("Datasession"))
loWareHous = CREATEOBJECT('RemoteTable', 'WAREHOUS', 'WAREHOUS', 'WAREHOUSE', SET("Datasession"))
loFORCAST  = CREATEOBJECT('RemoteTable', 'FORCAST', 'FORCAST', 'FORCAST', SET("Datasession"))
loOrdLine  = CREATEOBJECT('RemoteTable', 'ORDLINE', 'ORDLINES', 'ORDLINE', SET("Datasession"))
loInvLine  = CREATEOBJECT('RemoteTable', 'InvLine', 'INVLINES', 'InvLine', SET("Datasession"))
loCodes    = CREATEOBJECT('RemoteTable', 'CODES', 'IDRLTFNAME', 'MACODES', SET("Datasession"))
loPosHdr   = CREATEOBJECT('RemoteTable', 'POSHDR', 'POSHDR', 'POSHDR', SET("Datasession"))
loPosLn    = CREATEOBJECT('RemoteTable', 'POSLN', 'POSLNS', 'POSLN', SET("Datasession"))
loCutTktH  = CREATEOBJECT('RemoteTable', 'POSHDR', 'POSHDR', 'CUTTKTH', SET("Datasession"))
loCutTktl  = CREATEOBJECT('RemoteTable', 'POSLN', 'POSLNS', 'CUTTKTL', SET("Datasession"))
loFabric   = CREATEOBJECT('RemoteTable', 'ITEM', 'STYLE', 'FABRIC', SET("Datasession"))
loStyle    = CREATEOBJECT('RemoteTable', 'STYLE', 'STYLE', 'STYLE', SET("Datasession"))
loScale    = CREATEOBJECT('RemoteTable', 'SCALE', 'SCALE', 'SCALE', SET("Datasession"))
loBom      = CREATEOBJECT('RemoteTable', 'BOM', 'MULTIBOM', 'BOM', SET("Datasession"))
loStyDye   = CREATEOBJECT('RemoteTable', 'STYDYE', 'STYDYE', 'STYDYE', SET("Datasession"))
loFabDye   = CREATEOBJECT('RemoteTable', 'ITEMLOC', 'STYDYE', 'FABDYE', SET("Datasession"))
loUom      = CREATEOBJECT('RemoteTable', 'UOM', 'UOMCODE', 'UOM', SET("Datasession"))
loMmfgOrdh = CREATEOBJECT('RemoteTable', 'POSHDR', 'POSHDR', 'MMFGORDH', SET("Datasession"))
loMmfgOrdd = CREATEOBJECT('RemoteTable', 'POSLN', 'POSLN', 'MMFGORDD', SET("Datasession"))
loPofLn    = CREATEOBJECT('RemoteTable', 'POSLN', 'POSLNS', 'POFLN', SET("Datasession"))
loPofHdr   = CREATEOBJECT('RemoteTable', 'POSHDR', 'POSHDR', 'POFHEADR', SET("Datasession"))
loApVendor = CREATEOBJECT('RemoteTable', 'APVENDOR', 'VENCODE', 'APVENDOR', SET("Datasession"))
loOrdHdr   = CREATEOBJECT('RemoteTable', 'ORDHDR', 'ORDHDR', 'ORDHDR', SET("Datasession"))
loMfgOprHd = CREATEOBJECT('RemoteTable', 'MFGOPRHD', 'MFGOPRHD', 'MFGOPRHD', SET("Datasession"))

IF llVenRef
  loVendMatH = CREATEOBJECT('RemoteTable','VENDMATH','VENDMATH','VENDMATH',SET("Datasession"))
  loVendMatL = CREATEOBJECT('RemoteTable','VENDMATL','MATCOL','VENDMATL',SET("Datasession"))
ENDIF


*--Program variabels.
*--Read the Style major part length and item title.
lcMjrPct  = gfItemMask('PM')
lcStyTtl  = gfItemMask('HI')
lCnMJPct  = gfItemMask('PN')
lnMajorLn = LEN(lcMjrPct)
lnNMjrLn  = LEN(lCnMJPct)

llFromOTS    = .F.
lnFMajLen    = LEN(gfItemMask('PM', '', '0002'))
lcSeparator  = SUBSTR(gfItemMask('PM'), lnMajorLn + 1, 1)
lcFSeparator = SUBSTR(gfItemMask('PM', '', '0002'), lnFMajLen + 1, 1)
Trim_Invt    = (gfGetMemVar('M_TINVT', gcAct_Comp) = 'Y')
llMFIstall = (OCCURS('MF', gcCmpModules) <> 0)
llPOIstall = (OCCURS('PO', gcCmpModules) <> 0)

*-- Calc months end dates.
STORE 0 TO lnMonthEd, lnMonthEd
STORE '' TO lcperlen
lcperlen = gfGetMemVar('M_OTSPRIOD', gcAct_Comp)
=lfGetStrt()

IF llRPTimeP
  *-- Add variable to hold the differance between sunday and the frist working day.
  LOCAL lnI
  llFound   = .F.
  lnWorkDay = 0
  lcCurYear = SPACE(4)

  SELECT FISHD
  lcYear = STR(YEAR(gdSysDate),4)
  IF loFISHD.SEEK(lcYear) AND CFISYSTAT = 'C'
    llFound = .T.
  ELSE
    IF loFISHD.GOTOP()
      DO WHILE CFISYSTAT # 'C'
        IF !loFISHD.GONEXT()
          EXIT
        ENDIF
      ENDDO
    ENDIF
    llFound = !EOF() AND CFISYSTAT = 'C'
  ENDIF

  IF llFound
    IF EMPTY(CFISNONWD)
      lnWorkDay = 1
    ELSE
      FOR lnI = 1 TO LEN(CFISNONWD)
        lnWorkDay = MAX(lnWorkDay,VAL(SUBSTR(CFISNONWD,lnI,1))+1)
      ENDFOR
      lnWorkDay = MOD(lnWorkDay,7)
    ENDIF
    lcCurYear = CFISFYEAR
  ENDIF

  ldThisDate = lfWeekStr(ldRPStart)

  IF lcRpReqBas # 'F'
    ldThisDate  = ldThisDate - lnMonthEd
    lnFrcstPrds = 10
  ELSE
    lnFrcstPrds = INT((ldRpFrcstTo - ldThisDate) / lnMonthEd) + 1
  ENDIF
  lnFrcstLins = INT((lnFrcstPrds - 1) / 10) + 1

  DIMENSION laMonths[lnFrcstLins * 10]
  laMonths = ''

  FOR lnI = 1 TO lnFrcstPrds
    *-- Get months titles for the first Item/color only as they are fixed.
    IF EMPTY(laMonths[lnI])
      laMonths[lnI] = DTOC(ldThisDate + (lnMonthEd * (lnI - 1)))
    ENDIF
  ENDFOR
  ldLastDay = ldThisDate + lnFrcstPrds * lnMonthEd + IIF(lcRpReqBas = 'F', - 1, 365.25 * 4000)
ENDIF


*-- Create cursor to calculate the required by style/color in case of print both WIP and plan
IF llRPTimeP AND lcRpReqBas = 'B'
  lcStMatReq = loOGScroll.gfTempName()
  CREATE CURSOR (lcStMatReq) (CSTYLE C(19),nPeriod N(2,0),NWIP1 N(7,0),NWIP2 N(7,0),NWIP3 N(7,0),NWIP4 N(7,0),;
    NPLAN1 N(7,0),NPLAN2 N(7,0),NPLAN3 N(7,0),NPLAN4 N(7,0),;
    NYEALD N(7,3),DLEADTIME1 D,DLEADTIME2 D,DLEADTIME3 D,DLEADTIME4 D)
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON STR(nPeriod,2) + CSTYLE TAG (lcStMatReq) OF (lcStMatReq)
  INDEX ON STR(nPeriod,2) + CSTYLE TAG (lcStMatReq)
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDIF

IF lcRpReqBas = 'F'
  lcTmpFrCst = loOGScroll.gfTempName()
  lcStyTmp   = loOGScroll.gfTempName()

  CREATE CURSOR (lcTmpFrCst) (STYLE C(19),cYear C(4), cWeek C(2), Qty1 N(8,0), Qty2 N(8,0), Qty3 N(8,0),;
    Qty4 N(8,0), Qty5 N(8,0), Qty6 N(8,0), Qty7 N(8,0), Qty8 N(8,0))
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON Style+cYear+cWeek TAG (lcTmpFrCst) OF (lcTmpFrCst)
  INDEX ON STYLE+cYear+cWeek TAG (lcTmpFrCst)
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]

  CREATE CURSOR (lcStyTmp) (STYLE C(19), dFrDate D, Plan1 N(7,0), Plan2 N(7,0), Plan3 N(7,0),;
    Plan4 N(7,0), Plan5 N(7,0), Plan6 N(7,0), Plan7 N(7,0), Plan8 N(7,0), TotPlan N(7,0))
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON Style+DTOS(dFrDate) TAG (lcStyTmp) OF (lcStyTmp)
  INDEX ON STYLE+DTOS(dFrDate) TAG (lcStyTmp)
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDIF

*-- Print the existing MRP report.
IF lcRpMrpBas = 'E'
  = lfCrtTmp(IIF(llRPTimeP, 'T', 'M'))
  = lfPrntMrp()
  RETURN
ENDIF

IF loOgScroll.llOGFltCh
  *--Create MR temp file with structure.
  = lfCrtTmp(IIF(llRPTimeP, 'T', 'M'))

  llNoStyFlter =.T.
  lnRow = ASCAN(loOGScroll.laFltExp,'STYLE')
  IF lnRow > 0
    lnRow = ASUBSCRIPT(loOGScroll.laFltExp,lnRow,1)
    lcStyleFilter = loOGScroll.laFltExp[lnRow,2]
    llNoStyFlter = EMPTY(lcStyleFilter)
  ENDIF

  *--Create and Open items temp file.
  IF !llNoStyFlter
    IF EMPTY(lcTmpItems)
      lcTmpItems = loOGScroll.gfTempName()
    ENDIF

    IF USED(lcTmpItems)
      USE IN (lcTmpItems)
    ENDIF
    SELECT (lcMatReq)
    COPY STRUCTURE TO (oAriaApplication.WorkDir+lcTmpItems)
    USE (oAriaApplication.WorkDir+lcTmpItems) IN 0 EXCLUSIVE

    SELECT (lcTmpItems)
    =CURSORSETPROP("Buffering", 3)
    INDEX ON Typ+ITEM TAG (lcTmpItems)
  ENDIF

  *********************************************************************
  *    First Part Steps   *
  *********************************************************************
  *- Calculating the total yield (qty/unit) in Cost sheet.
  *- Scan for styles with selected criteria.
  *- Check if style major has a cost sheet.
  *- Append or Update record in (lcMatReq) file.
  *- Update the total yield (qty/utit) in (lcMatReq) file.
  *********************************************************************
  *-Check for Fabrics exp.
  llFabrics = ATC('lcFabrics',loOGScroll.lcRpExp)> 0
  IF llFabrics
    lnFabricsCnd = ASCAN(loogScroll.laOgFxFlt,'lcFabrics',1,ALEN(loOgScroll.laOgFxFlt),0,9)
    lcFabricsFile = loOGScroll.laOgFxFlt[lnFabricsCnd,6]
    IF !EMPTY(lcFabricsFile)
      SELECT &lcFabricsFile
      INDEX ON cStyMajor TAG &lcFabricsFile
    ENDIF
  ENDIF

  *-Check for Fabric Colors exp.
  llFabColor = ATC('lcFabClr',loOGSCroll.lcRpExp) > 0
  IF llFabColor
    lnFColorCond = ASCAN(loogScroll.laOgFxFlt,'lcFabClr',1,ALEN(loOgScroll.laOgFxFlt),0,9)
    lcFColorCond = loOGScroll.laOgFxFlt[lnFColorCond,6]
    IF !EMPTY(lcFColorCond)
      lcFClrFile = loOGScroll.gfTempName()
      =gfCrtTmp(lcFClrFile, "(Color C(6))", , "", .F.)

      SELECT &lcFClrFile
      lnSepOccur1 = OCCURS("|",lcFColorCond)
      IF lnSepOccur1 = 0
        lcColor = lcFColorCond
        INSERT INTO &lcFClrFile (COLOR) VALUES (lcColor)
      ELSE
        FOR lnColors = 1 TO lnSepOccur1 + 1
          lcColor = IIF(lnColors=1,SUBSTR(lcFColorCond,1,6),SUBSTR(lcFColorCond,ATC('|',lcFColorCond,lnColors-1)+1,6))
          INSERT INTO &lcFClrFile (COLOR) VALUES (lcColor)
        ENDFOR
      ENDIF
      SELECT &lcFClrFile
      INDEX ON COLOR TAG &lcFClrFile
    ENDIF
  ENDIF

  *-check for style group expression.
  llGroup = ATC('STYLE.CSTYGROUP',loOGSCroll.lcRpExp)>0
  IF llGroup
    lnGroupCondition = ASCAN(loogScroll.laOgFxFlt,'STYLE.CSTYGROUP',1,ALEN(loOgScroll.laOgFxFlt),0,9)
    lcGroupCondition = loOGScroll.laOgFxFlt[lnGroupCondition,6]
    IF !EMPTY(lcGroupCondition)
      lcGroupFile = loOGScroll.gfTempName()
      gfCrtTmp(lcGroupFile,"(cStyGroup C(6))",,"",.F.)
      SELECT &lcGroupFile
      lnSepOccur1 = OCCURS("|",lcGroupCondition)
      IF lnSepOccur1 = 0
        lcGroup = lcGroupCondition
        INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
      ELSE
        FOR lnGroups = 1 TO lnSepOccur1+1
          lcGroup = IIF(lnGroups=1,SUBSTR(lcGroupCondition,1,6),SUBSTR(lcGroupCondition,ATC('|',lcGroupCondition,lnGroups-1)+1,6))
          INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
        ENDFOR
      ENDIF
      SELECT &lcGroupFile
      INDEX ON cStyGroup TAG &lcGroupFile
    ENDIF
  ENDIF

  *-- Check for season expression.
  llSeason = ATC('STYLE.SEASON',loOGSCroll.lcRpExp)> 0
  IF llSeason
    lnSeasonCondition = ASCAN(loOGScroll.laOgFxFlt,'STYLE.SEASON')
    lnSeasonCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnSeasonCondition,1)
    lcSeasonCondition = loOGScroll.laOgFxFlt[lnSeasonCondition,6]
    IF !EMPTY(lcSeasonCondition)
      lcSeasonFile = loOGScroll.gfTempName()
      gfCrtTmp(lcSeasonFile,"(Season C(6))",,"",.F.)
      lnSepOccur1 = OCCURS("|",lcSeasonCondition)
      IF lnSepOccur1 = 0
        lcSeason = lcSeasonCondition
        INSERT INTO &lcSeasonFile (Season) VALUES (lcSeason)
      ELSE
        FOR lnSeasons = 1 TO lnSepOccur1+1
          lcSeason = IIF(lnSeasons=1,SUBSTR(lcSeasonCondition,1,6),SUBSTR(lcSeasonCondition,ATC('|',lcSeasonCondition,lnSeasons-1)+1,6))
          INSERT INTO &lcSeasonFile (Season) VALUES (lcSeason)
        ENDFOR
      ENDIF
      SELECT &lcSeasonFile
      INDEX ON Season TAG &lcSeasonFile
    ENDIF
  ENDIF

  *-- Check for division condition
  llDivision = ATC('STYLE.CDIVISION',loOGSCroll.lcRpExp)> 0
  IF llDivision
    lnDivisionCondition = ASCAN(loOGScroll.laOgFxFlt,'STYLE.CDIVISION')
    lnDivisionCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnDivisionCondition,1)
    lcDivisionCondition = loOGScroll.laOgFxFlt[lnDivisionCondition,6]
    IF !EMPTY(lcDivisionCondition)
      lcDivisionFile = loOGScroll.gfTempName()
      gfCrtTmp(lcDivisionFile,"(Division C(6))",,"",.F.)
      lnSepOccur1 = OCCURS("|",lcDivisionCondition)
      IF lnSepOccur1 = 0
        lcDivision = lcDivisionCondition
        INSERT INTO &lcDivisionFile (Division) VALUES (lcDivision)
      ELSE
        FOR lnDivisions = 1 TO lnSepOccur1+1
          lcDivision = IIF(lnDivisions=1,SUBSTR(lcDivisionCondition,1,6),SUBSTR(lcDivisionCondition,ATC('|',lcDivisionCondition,lnDivisions-1)+1,6))
          INSERT INTO &lcDivisionFile (Division) VALUES (lcDivision)
        ENDFOR
      ENDIF
      SELECT &lcDivisionFile
      INDEX ON Division TAG &lcDivisionFile
    ENDIF
  ENDIF

  *-- Check for Style Major
  llStyMajor = ATC('STYLE.CSTYMAJOR',loOGScroll.lcRpExp)> 0
  IF llStyMajor
    lnStyMajorCnd = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.CSTYMAJOR'),1)
    lcStyMajorFile = loOGScroll.laOgFxFlt[lnStyMajorCnd,6]
    IF !EMPTY(lcStyMajorFile)
      SELECT &lcStyMajorFile
      INDEX ON cStyMajor TAG &lcStyMajorFile
    ENDIF
  ENDIF

  *-- Check for style primary fabric
  llPrmFab = ATC('STYLE.FABRIC',loOGScroll.lcRpExp)> 0
  IF llPrmFab
    lnPrmFabCnd = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.FABRIC'),1)
    lcPrmFabFile = loOGScroll.laOgFxFlt[lnPrmFabCnd,6]
    IF !EMPTY(lcPrmFabFile)
      SELECT &lcPrmFabFile
      INDEX ON cStyMajor TAG &lcPrmFabFile
    ENDIF
  ENDIF

  *-- Check for style colors
  llColor = ATC('SUBSTR(STYLE.STYLE',loOGSCroll.lcRpExp) > 0
  IF llColor
    lnColorCondition = ASCAN(loogScroll.laOgFxFlt,'SUBSTR(STYLE.STYLE',1,ALEN(loOgScroll.laOgFxFlt),0,9)
    lcColorCondition = loOGScroll.laOgFxFlt[lnColorCondition,6]
    IF !EMPTY(lcColorCondition)
      lcColorFile = loOGScroll.gfTempName()
      gfCrtTmp(lcColorFile,"(Color C(6))",,"",.F.)
      SELECT &lcColorFile
      lnSepOccur1 = OCCURS("|",lcColorCondition)
      IF lnSepOccur1 = 0
        lcColor = lcColorCondition
        INSERT INTO &lcColorFile (COLOR) VALUES (lcColor)
      ELSE
        FOR lnColors = 1 TO lnSepOccur1+1
          lcColor = IIF(lnColors=1,SUBSTR(lcColorCondition,1,6),SUBSTR(lcColorCondition,ATC('|',lcColorCondition,lnColors-1)+1,6))
          INSERT INTO &lcColorFile (COLOR) VALUES (lcColor)
        ENDFOR
      ENDIF
      SELECT &lcColorFile
      INDEX ON COLOR TAG &lcColorFile
    ENDIF
  ENDIF

  lcPatCond = loOGScroll.laOgFxFlt[ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.PATTERN'),1),6]

  *--Get styles that match the warecode condition if some style locations are selected only
  lcSelStyles = ""
  lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYDYE.CWARECODE'),1)
  lcLocFile = loOGScroll.laOgFxFlt[lnLocCondition,6]

  *--If Style Location Are selected.
  IF !EMPTY(lcLocFile)
    lcSelStyles = loOGScroll.gfTempName()
    SELECT STYLE FROM STYDYE INNER JOIN &lcLocFile ON STYDYE.cWareCode = &lcLocFile..cWareCode;
      WHERE !EMPTY(STYDYE.Dyelot) INTO DBF oAriaApplication.WorkDir+lcSelStyles+".DBF"
    SELECT &lcSelStyles
    INDEX ON STYLE TAG &lcSelStyles
  ENDIF

  *--Get Fabrics that match the warecode condition if some fabric locations are selected only
  lcSelFabrics = ""
  lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'ITEMLOC.CWARECODE'),1)
  lcFLocFile = loOGScroll.laOgFxFlt[lnLocCondition,6]

  *--If Style Location Are selected.
  IF !EMPTY(lcFLocFile)
    lcFSQLLoc = loOgScroll.gfSQLTempName('', 'cWareCode C(6)', lcFLocFile, 'cWareCode') && SQL Temp File

    lcStat = "SELECT STYLE" +;
      "  FROM ITEMLOC" +;
      " INNER JOIN " + lcFSQLLoc +;
      "    ON ITEMLOC.cWareCode = " + ALLTRIM(lcFSQLLoc) + ".cWareCode" + ;
      " WHERE Dyelot <> ''"

    lnResult = loOgScroll.oRDA.SqlRun(lcStat, lcSelFabrics, , oAriaApplication.ActiveCompanyConStr, 3, "SAVE",)
    IF lnResult <> 1
      *-- SQL connection error. can't open the report
      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF
    =CURSORSETPROP("Buffering", 3, lcSelFabrics)
    SELECT (lcSelFabrics)
    INDEX ON STYLE TAG &lcSelFabrics
  ENDIF

  SELECT (lcMatReq)
  SET ORDER TO TAG MATReq

  *--Sort by fabric location in case print by warehouse Yes.
  IF llRpByWare
    SET ORDER TO TAG MRFABLOC
  ENDIF

  LOCAL llLoop

  loStyle.SetOrder("STYLE")

  *-- If style majors selected in OG filter...
  IF llStyMajor
    SELECT (lcStyMajorFile)
    SCAN
      IF loStyle.SEEK(PADR(cStyMajor, lnMajorLn))
        SELECT STYLE
        SCAN REST WHILE STYLE = PADR(EVALUATE(lcStyMajorFile + '.cStyMajor'), lnMajorLn)
          llLoop = !EMPTY(lcSelStyles) AND !SEEK(STYLE.STYLE,lcSelStyles)
          llLoop = llLoop OR (!llRpActSty .AND. (STYLE.STATUS = 'X'))
          llLoop = llLoop OR (!EMPTY(lcPatCond) AND STYLE.PATTERN <> lcPatCond)
          llLoop = llLoop OR (llGroup AND !SEEK(STYLE.cStyGroup,lcGroupFile))
          llLoop = llLoop OR (llSeason AND !SEEK(STYLE.Season,lcSeasonFile))
          llLoop = llLoop OR (llDivision AND !SEEK(STYLE.cDivision,lcDivisionFile))
          llLoop = llLoop OR (llPrmFab AND !SEEK(STYLE.Fabric,lcPrmFabFile))
          llLoop = llLoop OR (llColor AND !SEEK(SUBSTR(STYLE.STYLE,lnClrSrt,lnclrEnd),lcColorFile))

          IF llLoop
            LOOP
          ENDIF

          WAIT WINDOW 'Collecting the cost sheet information for Style : ' + STYLE.STYLE NOWAIT

          *--Check if the style has a cost sheet.
          lcStyMajor  = SUBSTR(STYLE.STYLE,1,lnMajorLn)
          IF !loBom.SEEK("0001"+lcStyMajor)
            EXIT
          ENDIF

          *--Current style code and style scale.
          lcCStyle = STYLE.STYLE
          lnCnt    = IIF(loScale.SEEK("S"+STYLE.SCALE),SCALE.CNT,8)

          SELECT BOM
          SCAN REST WHILE cInvType+cItmmajor+Typ+cItmMask+MfgCode+cInvTypC+ITEM = '0001' + SUBSTR(lcCStyle,1,lnMajorLn) ;
              FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
              cCatgTyp $ IIF(lcRpCompn = 'A', IIF(llRPTimeP, 'FT', 'FTS'), lcRpCompn)

            *--Don't include non inventory trims if not setup to use it.
            IF (cCatgTyp='T' AND !Trim_Invt AND !llRpInvItm)
              LOOP
            ENDIF

            *--Read an item code and an item color[lcItem,lcIClr].
            STORE '' TO lcItem
            IF !lfReadItem()
              LOOP
            ENDIF

            *--If component is fabric check fabric and colors in grid filter.
            *-- Check also for tims
            IF llByFabLoc AND (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt))
              lcFabStyle = lcItem

              IF !SEEK(lcFabStyle, lcSelFabrics)
                LOOP
              ENDIF
            ENDIF

            *-- Check if material/Color entered in grid is valid.
            *-- Don't include non inventory trims if not in the selection critria.
            IF (cCatgTyp = 'F' OR cCatgTyp = 'T')
              IF llFabColor AND !SEEK(SUBSTR(lcItem,lnFClrSrt,lnFclrEnd),lcFClrFile)
                LOOP
              ENDIF
            ENDIF

            IF llFabrics AND !SEEK(SUBSTR(Bom.ITEM,1,lnFMajLen),lcFabricsFile)
              LOOP
            ENDIF

            SELECT (lcMatReq)

            *--Append or Update record in (lcMatReq) file.
            =lfUpdReq()

            *--Append item/color used in style cost sheet in temp items file.
            IF !llNoStyFlter AND !(STYLE.MAKE)
              =lfUpdItem()
            ENDIF
          ENDSCAN
        ENDSCAN
      ENDIF
    ENDSCAN
  ELSE
    *-- IF fabric majors selected in OG filter...
    IF llFabrics AND !EMPTY(lcFabricsFile)
      loBOM.SetOrder("MITEMTYP")
      SELECT (lcFabricsFile)
      SCAN
        *IF loBOM.SEEK('0002' + SUBSTR(cStyMajor, 1, lnFMajLen))
        *  SCAN REST WHILE cInvTypC+Item+Typ+cInvType+cItmMajor+cCstShtTyp+cCstSht_Id+cItmMask = '0002' + SUBSTR(&lcFabricsFile..cStyMajor, 1, lnFMajLen);
        FOR   cCatgTyp $ IIF(lcRpCompn = 'A', IIF(llRPTimeP, 'FT', 'FTS'), lcRpCompn)
        m.ItmCode = SUBSTR(cStyMajor, 1, lnFMajLen)
        m.ItmLike = m.ItmCode + '%'
        IF loBOM.SQLRun("SELECT * FROM BOM (INDEX = MITEMTYP) WHERE cInvTypC = '0002' AND Item LIKE ?m.ItmLike AND cCatgTyp IN ('" + IIF(lcRpCompn = 'A', "F' , 'T", lcRpCompn) + "')")
          SELECT BOM
          LOCATE
          SCAN
            lcStyCode = SUBSTR(BOM.CITMMASK,1,lnMajorLn)
            lcStyLike = STRTRAN(BOM.cItmMask,'*','?')

            SELECT STYLE
            =loStyle.SEEK(lcStyCode)
            SCAN REST WHILE STYLE = lcStyCode FOR LIKE(lcStyLike, STYLE)
              llLoop = BOM.cCatgTyp = 'T' AND !Trim_Invt AND !llRpInvItm
              llLoop = llLoop OR (llStyMajor AND !EMPTY(lcStyMajorFile) AND !SEEK(STYLE.cStyMajor,lcStyMajorFile))
              llLoop = llLoop OR (!EMPTY(lcSelStyles) AND !SEEK(STYLE.STYLE,lcSelStyles))
              llLoop = llLoop OR (!llRpActSty .AND. (STYLE.STATUS = 'X'))
              llLoop = llLoop OR (!EMPTY(lcPatCond) AND STYLE.PATTERN <> lcPatCond)
              llLoop = llLoop OR (llGroup AND !SEEK(STYLE.cStyGroup,lcGroupFile))
              llLoop = llLoop OR (llSeason AND !SEEK(STYLE.Season,lcSeasonFile))
              llLoop = llLoop OR (llDivision AND !SEEK(STYLE.cDivision,lcDivisionFile))
              llLoop = llLoop OR (llPrmFab AND !SEEK(STYLE.Fabric,lcPrmFabFile))
              llLoop = llLoop OR (llColor AND !SEEK(SUBSTR(STYLE.STYLE,lnClrSrt,lnclrEnd),lcColorFile))

              IF llLoop
                LOOP
              ENDIF

              *--Current style code and style scale.
              lcCStyle = STYLE.STYLE
              lnCnt    = IIF(loScale.SEEK("S"+STYLE.SCALE),SCALE.CNT,8)

              WAIT WINDOW 'Collecting the cost sheet information for Style: ' + lcCStyle NOWAIT

              *--Read an item code and an item color[lcItem,lcIClr].
              SELECT BOM
              STORE '' TO lcItem
              IF !lfReadItem()
                LOOP
              ENDIF

              *--If component is fabric check fabric and colors in grid filter.
              *-- Check also for tims
              IF llByFabLoc AND (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt))
                lcFabStyle = lcItem
                IF !SEEK(lcFabStyle, lcSelFabrics)
                  LOOP
                ENDIF
              ENDIF

              *-- Check if material/Color entered in grid is valid.
              *-- Don't include non inventory trims if not in the selection critria.
              IF (cCatgTyp = 'F' OR cCatgTyp = 'T')
                IF llFabColor AND !SEEK(SUBSTR(lcItem,lnFClrSrt,lnFclrEnd),lcFClrFile)
                  LOOP
                ENDIF
              ENDIF

              SELECT (lcMatReq)

              *--Append or Update record in (lcMatReq) file.
              =lfUpdReq()

              *--Append item/color used in style cost sheet in temp items file.
              IF !llNoStyFlter AND !(STYLE.MAKE)
                =lfUpdItem()
              ENDIF
            ENDSCAN
          ENDSCAN
        ENDIF
      ENDSCAN
    ELSE
      *-- IF no styles or fabrics selected in OG filter
      loStyle.GoTop()
      SELECT STYLE
      DO WHILE .T.
        *- check that current style is in the cursor we got from STYDYE file (if style locations selected only)
        lcCStyle = STYLE.STYLE

        IF !EMPTY(lcSelStyles)
          IF !SEEK(STYLE.STYLE,lcSelStyles)
            IF !loStyle.GoNext()
              EXIT
            ELSE
              LOOP
            ENDIF
          ENDIF
        ENDIF

        *- check that current style match selected Pattern (if any)
        IF !EMPTY(lcPatCond)
          IF STYLE.PATTERN <> lcPatCond
            IF !loStyle.GoNext()
              EXIT
            ELSE
              LOOP
            ENDIF
          ENDIF
        ENDIF

        *- If not include cancelled styles,check that current style status not Cancelled
        IF !llRpActSty .AND. (STYLE.STATUS = 'X')
          IF !loStyle.GoNext()
            EXIT
          ELSE
            LOOP
          ENDIF
        ENDIF

        *- check that current style match selected Groups (if any)
        IF llGroup
          IF !SEEK(STYLE.cStyGroup,lcGroupFile)
            IF !loStyle.GoNext()
              EXIT
            ELSE
              LOOP
            ENDIF
          ENDIF
        ENDIF

        *- check that current style match selected Seasons (if any)
        IF llSeason
          IF !SEEK(STYLE.Season,lcSeasonFile)
            IF !loStyle.GoNext()
              EXIT
            ELSE
              LOOP
            ENDIF
          ENDIF
        ENDIF

        *- check that current style match selected Divisions (if any)
        IF llDivision
          IF !SEEK(STYLE.cDivision,lcDivisionFile)
            IF !loStyle.GoNext()
              EXIT
            ELSE
              LOOP
            ENDIF
          ENDIF
        ENDIF

        *- check that current style match selected Primary Fabrics(if any)
        IF llPrmFab
          IF !SEEK(STYLE.Fabric,lcPrmFabFile)
            IF !loStyle.GoNext()
              EXIT
            ELSE
              LOOP
            ENDIF
          ENDIF
        ENDIF

        *- check that current style match selected Style colors (if any)
        IF llColor
          IF !SEEK(SUBSTR(STYLE.STYLE,lnClrSrt,lnclrEnd),lcColorFile)
            IF !loStyle.GoNext()
              EXIT
            ELSE
              LOOP
            ENDIF
          ENDIF
        ENDIF

        WAIT WINDOW 'Collecting the cost sheet information for Style : ' + STYLE.STYLE NOWAIT

        *--Check if the style has a cost sheet.
        lcStyMajor  = SUBSTR(STYLE.STYLE,1,lnMajorLn)
        IF !loBom.SEEK("0001"+lcStyMajor)
          IF !loStyle.GoNext()
            EXIT
          ELSE
            LOOP
          ENDIF
        ENDIF

        *--Current style code and style scale.
        lcCStyle = STYLE.STYLE
        lnCnt    = IIF(loScale.SEEK("S"+STYLE.SCALE),SCALE.CNT,8)

        *-- walid in this code a relation between Fabric and Bom must be created [begin]
        SELECT BOM
        SCAN REST WHILE cInvType+cItmmajor+Typ+cItmMask+MfgCode+cInvTypC+ITEM = '0001' + SUBSTR(lcCStyle,1,lnMajorLn) ;
            FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
            cCatgTyp $ IIF(lcRpCompn = 'A', IIF(llRPTimeP, 'FT', 'FTS'), lcRpCompn)
          *--Don't include non inventory trims if not setup to use it.
          IF (cCatgTyp='T' AND !Trim_Invt AND !llRpInvItm)
            LOOP
          ENDIF

          *--Read an item code and an item color[lcItem,lcIClr].
          STORE '' TO lcItem
          IF !lfReadItem()
            LOOP
          ENDIF

          *--If component is fabric check fabric and colors in grid filter.
          *-- Check also for tims [Start]
          IF llByFabLoc AND (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt))
            lcFabStyle = lcItem
            IF !SEEK(lcFabStyle, lcSelFabrics)
              LOOP
            ENDIF
          ENDIF

          *-- Check if material/Color entered in grid is valid.
          *-- Don't include non inventory trims if not in the selection critria.
          IF (cCatgTyp = 'F' OR cCatgTyp='T')
            IF llFabColor
              IF !SEEK(SUBSTR(lcItem,lnFClrSrt,lnFclrEnd),lcFClrFile)
                LOOP
              ENDIF
            ENDIF

            IF llFabrics
              IF !SEEK(SUBSTR(Bom.ITEM,1,lnFMajLen),lcFabricsFile)
                LOOP
              ENDIF
            ENDIF
          ENDIF

          SELECT (lcMatReq)

          *--Append or Update record in (lcMatReq) file.
          =lfUpdReq()

          *--Append item/color used in style cost sheet in temp items file.
          IF !llNoStyFlter AND !(STYLE.MAKE)
            =lfUpdItem()
          ENDIF
        ENDSCAN

        IF !loStyle.GoNext()
          EXIT
        ELSE
          LOOP
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  WAIT CLEAR

  *--Check requirements existance.
  GO TOP IN (lcMatReq)
  IF EOF(lcMatReq)
    *--No cost sheet information found for any of the selected
    *--styles or fabrics 'in location'.
    *--                           [Ok]
    =gfModalGen('TRM36107B36000','DIALOG',IIF(llByLoctn OR llByFabLoc,'in location',''))
    RETURN
  ENDIF

  *********************************************************************
  *      Second Part Steps      *
  *********************************************************************
  *- Calculating the total yield (qty/utit) in Cost sheet.
  *- Scan for selected items in preivius step.
  *- Scan for styles out of the selected criteria for this items.
  *- Append or Update record in (lcMatReq) file.
  *- Update the total yield (qty/utit) in (lcMatReq) file.
  *********************************************************************

  *--Start select styles that use the same item components and
  *--out of the grid filter.
  *--Only if there is a filter used ,becouse if not this styles allready
  *--selected if preivus part.

  loStyle.SetOrder("STYLE")

  IF !llNoStyFlter
    loBom.SetOrder("MBOMITEM")
    SELECT (lcTmpItems)
    SCAN
      IF cCatgTyp = 'S'
        lnAstPos = AT('*', ITEM)
        lcKeyCnd = Typ + '0001' + IIF(lnAstPos = 0, ITEM, ALLTRIM(SUBSTR(ITEM, 1, lnAstPos - 1)))
      ELSE
        lcKeyCnd = Typ + '0002' + ITEM
      ENDIF
      lcWhlCnd = "Typ + cInvTypC + Item = lcKeyCnd"

      WAIT WINDOW 'Checking for styles useing material : ' + PADR(ITEM, 19) NOWAIT

      SELECT BOM

      =loBom.SEEK(lcKeyCnd)
      SCAN WHILE &lcWhlCnd ;
          FOR LIKE(STRTRAN(ITEM,'*','?'),&lcTmpItems..ITEM) AND ;
          (SUBSTR(ITEM, lnFClrSrt, lnFClrEnd) = '******' OR SUBSTR(ITEM, lnFClrSrt, lnFClrEnd) = SUBSTR(&lcTmpItems..ITEM, lnFClrSrt, lnFClrEnd))

        *--Current item code and an item color.
        lcItem = &lcTmpItems..ITEM

        *--Read the style code and an style color for Exist Item.
        lnAstPos  = AT('*',cItmMask)
        lcStyMask = IIF(lnAstPos = 0 , cItmMask ,;
          ALLTRIM(SUBSTR(cItmMask,1,lnAstPos-1)) )
        lcStyClr  = ""

        IF CCATGTYP $ 'FT'
          IF lnAstPos <> 0 AND lnClrSrt <> 0 AND SUBSTR(cItmMask, lnClrSrt, lnClrEnd) = '*'
            lcStyClr = SUBSTR(&lcTmpItems..ITEM, lnFClrSrt, lnFClrEnd)
          ENDIF
        ELSE
          IF lnAstPos <> 0
            lcStyClr = SUBSTR(lcItem,lnAstPos,LEN(lcItem)-IIF(llMScale,3,0))
          ENDIF
        ENDIF

        *--Check if style is out of filter.
        SELECT STYLE
        =loStyle.SEEK(lcStyMask)

        IF llByLoctn
          SELECT STYDYE
          =SEEK(lcStyMask, lcSelStyles)

          *-- select the style file before the scan
          SELECT STYLE
        ENDIF

        *-- Add new option to the material requirment report to
        *-- Get the styles outside the filter or not  Variable Is llRpIncSty.
        IF llRpIncSty
          *-- Add the style status to the filter Exprition.
          SCAN WHILE STYLE = lcStyMask FOR LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE) AND ;
              SUBSTR(STYLE,lnClrSrt,lnClrEnd) = lcStyClr
            *--Current style code and style scale.
            lcCStyle = STYLE.STYLE

            llLoop = (!llRpActSty .AND. (STYLE.STATUS = 'X'))
            llLoop = llLoop OR (!EMPTY(lcPatCond) AND STYLE.PATTERN <> lcPatCond)
            llLoop = llLoop OR (llGroup AND !SEEK(STYLE.cStyGroup,lcGroupFile))
            llLoop = llLoop OR (llSeason AND !SEEK(STYLE.Season,lcSeasonFile))
            llLoop = llLoop OR (llDivision AND !SEEK(STYLE.cDivision,lcDivisionFile))
            llLoop = llLoop OR (llPrmFab AND !SEEK(STYLE.Fabric,lcPrmFabFile))
            llLoop = llLoop OR (llColor AND !SEEK(SUBSTR(STYLE.STYLE,lnClrSrt,lnclrEnd),lcColorFile))
            *B609094,1 WAM 11/19/2009 Fix bug of duplicate yield printed when select style major in the option grid
            llLoop = llLoop OR (SEEK(STYLE.STYLE,lcMatReq,'Mrstyitm'))
            *B609094,1 WAM 11/19/2009 (End)

            IF llLoop
              LOOP
            ENDIF

            lnCnt = IIF(loScale.SEEK("S"+STYLE.SCALE), SCALE.CNT, 8)

            *--Append or Update record in (lcMatReq) file.
            *-- in case select by location we must set the pointer in the stydye to the correct location
            SELECT (lcMatReq)

            IF llByLoctn AND SEEK(lcCStyle, lcSelStyles)
              =lfUpdReq()
            ENDIF

            IF !llByLoctn
              =lfUpdReq()
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDSCAN

    WAIT CLEAR

    USE IN (lcTmpItems)
    ERASE (oAriaApplication.WorkDir+lcTmpItems+'.DBF')
    ERASE (oAriaApplication.WorkDir+lcTmpItems+'.CDX')
  ENDIF

  *********************************************************************
  *  Third Part Steps   *
  *********************************************************************
  *- Calculating the Open-to-Cut for the selected styles.
  *- Scan for styles selected in privius steps.
  *- Calculate ordered pieces.
  *- Calculate work in prosses pieces.
  *- Calculate Requirements Pieces per size.
  *- Compute the Projection
  *- Update style with Requirements pieces.
  *********************************************************************

  SELECT (lcMatReq)

  *-- Calculating the requirements in case of Detail line layout
  IF lcRpRName = 'MAMATRQD'
    =lfPrntDtLn()
    RETURN
  ENDIF

  *--Initilize report variables and arrays.

  *--[1] Ordered pieces for Open and Open+Hold orders.
  DECLARE laOrders[2,9]               && Row =1 OPEN ,2=HOLD+OPEN

  *--[2] WIP open pieces for Actualize,Open,Hold C/t.
  DECLARE laWIP[3,9]                  && ROW =1 'A', =2 'O', =3 'H'

  *--[3] Pieces requirements for Open and Open+Hold orders or Plane.
  DECLARE laRequr[3,9]                 && Row =1 OPEN ,2=HOLD+OPEN, 3=PLANE

  *--Thermometer counter.
  lnRecCount = RECCOUNT()
  lnCurNum   = 0

  IF llRPTimeP
    loPOFLn.SetOrder("POSLNS")

    lcTmpMatReq = loOGScroll.gfTempName()

    SELECT (lcMatReq)
    SET ORDER TO TAG MATREQ
  ELSE
    *--Set tag on styles unique.
    SET ORDER TO TAG MRStyle
  ENDIF

  loOrdHdr.SetOrder("ORDHDR")
  loOrdLine.SetOrder("ORDLINES")
  SELECT (lcMatReq)
  LOCATE



  SCAN FOR IIF(llRPTimeP, nLineNo = 1, .T.)
    WAIT WINDOW 'Calculating requirements for Fabric: ' + ITEM NOWAIT  && Temp message

    SELECT (lcMatReq)

    IF llRPTimeP
      DO lpAllQty
    ELSE
      *--Current style code.
      lcCStyle = STYLE
      lnCurNum = lnCurNum + 1

      *--Style scale count.
      lnCnt     = CNT
      lcChkFile = 'STYLE'
      loStyle.SEEK(lcCStyle)

      *--Initilize variables and arrayes for starting calculation.
      laOrders  = 0        && Ordered pieces for Open and Open+Hold orders.
      laWIP     = 0        && WIP pieces.
      laRequr   = 0        && Requirements pieces.

      *--Calculate ordered pieces.
      *--Not need to calculate ordered pieces in WIP case
      IF lcRpReqBas <> 'W' .AND. &lcChkFile..TotOrd <> 0
        =lfChkOrder()
      ENDIF

      *--Append style with Requirements.
      *- Don't take the styles with zero requirements if the report
      *- setup to not print zero requirements.

      *-- Consider case of fabric in C/T cost sheet not in the BOM file for WIP.
      IF lcRpReqBas = 'W'
        =lfWipReq()
      ELSE
        SELECT (lcMatReq)
        SET ORDER TO TAG MRStyItm
        SEEK lcCStyle

        *--Start update the style with the qty.
        SCAN WHILE STYLE = lcCStyle
          *-- Reset laWip And laRequr arrayes
          FOR lnXLoop = 1 TO 3
            FOR lnYLoop = 1 TO 9
              laWIP[lnXLoop,lnYLoop] = 0
            ENDFOR
          ENDFOR

          FOR lnXLoop = 1 TO 3
            FOR lnYLoop = 1 TO 9
              laRequr[lnXLoop,lnYLoop] = 0
            ENDFOR
          ENDFOR

          **--For cutting ticket.
          *=lfChk_CT()
          *--For Purchase order.
          =lfChk_PO()

          *--Calculate Requirements Pieces per size.
          FOR I=1 TO lnCnt
            Z=STR(I,1)
            *--Read the style Stock pieces.
            lnStock = IIF(&lcChkFile..Stk&Z  > 0, &lcChkFile..Stk&Z , 0)

            *--[1] Requirements Pieces for OPEN orders.
            lnReqPieces = laOrders[1,I]-(lnStock+laWIP[1,I]+IIF(lcRpReqBas='O',laWIP[2,I],0))
            laRequr[1,I] = IIF(lcRpReqBas='O',lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
            laRequr[1,I] = MAX( laRequr[1,I] , 0 )
            laRequr[1,9] = laRequr[1,9] + laRequr[1,I]

            *--[2] Requirements Pieces for OPEN+HOLD orders.
            lnReqPieces = laOrders[2,I]-(lnStock+laWIP[1,I]+IIF(lcRpReqBas='O',laWIP[2,I],0))
            laRequr[2,I] = IIF(lcRpReqBas='O',lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
            laRequr[2,I] = MAX( laRequr[2,I] , 0 )
            laRequr[2,9] = laRequr[2,9] + laRequr[2,I]

            *--[3] Requirements Pieces for Projection (dependent).
            *--Compute the Projection (Depends on requirements calculated before).
            *--or depens on plane if select projection for plane.
            DO CASE
              *--[1] Projection base on Open.
            CASE lcRpBasePj = 'O'
              laRequr[3,I] = laRequr[1,I] + ROUND(laRequr[1,I] * (lnRpPrIncr/100),0)
              *--[2] Projection base on Open+Hold.
            CASE lcRpBasePj = 'H'
              laRequr[3,I] = laRequr[2,I] + ROUND(laRequr[2,I] * (lnRpPrIncr/100),0)
              *--[3] Requirements Pieces for Plan.
            CASE lcRpBasePj = 'P'
              *-- Taking into consideration the percentage.
              laRequr[3,I] = IIF(STYLE.Plan&Z > 0, STYLE.Plan&Z + ROUND(STYLE.Plan&Z*((lnRpPrIncr/100)),0), 0)
            ENDCASE
            laRequr[3,9] = laRequr[3,9] + laRequr[3,I]
          ENDFOR

          REPLACE OReq1    WITH laRequr[1,1],;
            OReq2    WITH laRequr[1,2],;
            OReq3    WITH laRequr[1,3],;
            OReq4    WITH laRequr[1,4],;
            OReq5    WITH laRequr[1,5],;
            OReq6    WITH laRequr[1,6],;
            OReq7    WITH laRequr[1,7],;
            OReq8    WITH laRequr[1,8],;
            OReqTot  WITH laRequr[1,9],;
            OHReq1   WITH laRequr[2,1],;
            OHReq2   WITH laRequr[2,2],;
            OHReq3   WITH laRequr[2,3],;
            OHReq4   WITH laRequr[2,4],;
            OHReq5   WITH laRequr[2,5],;
            OHReq6   WITH laRequr[2,6],;
            OHReq7   WITH laRequr[2,7],;
            OHReq8   WITH laRequr[2,8],;
            OHReqTot WITH laRequr[2,9],;
            nProj1   WITH laRequr[3,1],;
            nProj2   WITH laRequr[3,2],;
            nProj3   WITH laRequr[3,3],;
            nProj4   WITH laRequr[3,4],;
            nProj5   WITH laRequr[3,5],;
            nProj6   WITH laRequr[3,6],;
            nProj7   WITH laRequr[3,7],;
            nProj8   WITH laRequr[3,8],;
            PROJECT  WITH laRequr[3,9]

          *--Update the open WIP pieces.
          *-- Added to update open wip pieces per size not only the total.
          REPLACE nOpnWip1 WITH laWIP[2,1],;
            nOpnWip2 WITH laWIP[2,2],;
            nOpnWip3 WITH laWIP[2,3],;
            nOpnWip4 WITH laWIP[2,4],;
            nOpnWip5 WITH laWIP[2,5],;
            nOpnWip6 WITH laWIP[2,6],;
            nOpnWip7 WITH laWIP[2,7],;
            nOpnWip8 WITH laWIP[2,8],;
            nOpnWip  WITH laWIP[2,9]
        ENDSCAN
        SET ORDER TO TAG MRStyle
        SEEK lcCStyle
      ENDIF
    ENDIF
  ENDSCAN

  WAIT CLEAR
ENDIF

SELECT (lcMatReq)
SET ORDER TO TAG MATREQ

*--Check if there is requirement.
IF !llRPTimeP AND !llRpZeroRq
  SET FILTER TO OHReqTot <> 0
ENDIF
LOCATE

*--Check requirements existance.
IF EOF()
  *--None of the selected styles have any requirements.
  *--                     [Ok]
  =gfModalGen('TRM36108B36000','DIALOG')
  RETURN
ENDIF

*--If Edit Projection is required.
IF llRpEditPj
  =lfEditProj()
ENDIF

*--Open -ve remaining items file.
lcTmpRmain = loOGScroll.gfTempName()
SELECT (lcMatReq)

=lfCrtTmp('R')
SET ORDER TO 1

*--Collecting -ve remaining fabrics variables.
STORE '' TO lcUOMBuy
lnFabcost = 0

IF llVenRef
  IF TYPE("loVendMatH") # 'O'
    loVendMatH = CREATEOBJECT('RemoteTable','VENDMATH','VENDMATH','VENDMATH',SET("Datasession"))
  ENDIF

  IF TYPE("loVendMatL") # 'O'
    loVendMatL = CREATEOBJECT('RemoteTable','VENDMATL','MATCOL','VENDMATL',SET("Datasession"))
  ENDIF
ENDIF



DO WHILE .T.
  SELECT (lcMatReq)
  *-- Sort by fabric location in case print by warehouse Yes [Start]
  IF lcRpReqBas = 'W'
    IF llRpByWare
      SET ORDER TO TAG MRFABLOC
    ELSE
      SET ORDER TO TAG MATREQ
    ENDIF
  ENDIF

  *--Print reports.
  *WAIT WINDOW 'Printing....' NOWAIT

  IF !lfPrintRep()
    *-- Message : There are no records to display...!
    = gfModalGen('TRM00052B40011','ALERT')
    EXIT
  ENDIF

  IF !llRPTimeP
    *--Do you wish to reprint the report with edited projections.
    *--                        [ Yes / No ]
    lnChoice=gfModalGen('TRM36109B36001','DIALOG')
    IF lnChoice = 1
      *--Edit Projection.
      =lfEditProj()

      *--Proceed with printing report ?
      *--         [ Yes / No ]
      lnChoice = gfModalGen('TRM36110B36001','DIALOG')
    ENDIF
  ELSE
    lnChoice = 2
  ENDIF

  IF lnChoice = 2
    *-- Save the MRP# of the report
    IF !llRPTimeP AND llMrp .AND. !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
      =lfSaveMrp()
    ENDIF

    *--Ceck if there is negative remainig in some materials.
    GO TOP IN (lcTmpRmain)
    IF !EOF(lcTmpRmain)
      *--Negative remaining quantity on some materials. Create P/Os ?
      *--                        [ Yes / No ]
      lnChoice=gfModalGen('QRM36111B36001','DIALOG')
      IF lnChoice = 1
        *-- Save the MRP# of the report.
        IF !llRPTimeP AND llMrp .AND. !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
          =lfSaveMrp()
        ENDIF

        WAIT WINDOW 'Generate P/Os...' NOWAIT

        IF lfGenMPo()
          EXIT
        ENDIF
      ENDIF
    ENDIF

    USE IN (lcTmpRmain)
    ERASE (gcWorkDir+lcTmpRmain+'.DBF')
    ERASE (gcWorkDir+lcTmpRmain+'.CDX')
    EXIT
  ENDIF
ENDDO

*--Update plane in style file.
*-- Do you wish to update the projection to the style master file.
*--                        [ Yes / No ]
IF llUpdPlan AND gfModalGen('QRM36112B36001','DIALOG') = 1
  =lfUpdStyle()
ENDIF

*--Close all Open File Objects.
STORE .NULL. TO loStyle,loStydye,loBom,loScale,loFabDye,loUOM,loFORCAST,loCodes,loWareHous, loInvLine, loVendMatH
STORE .NULL. TO loPofLn,loPofHdr,loMmfgOrdh,loMmfgOrdd,loFabric,loOrdHdr,loCutPick, loOrdLine, loVendMatL, loApVendor

RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfwRepWhen

IF !llFrstTime
  *-05/23/2005 HFK
  lcTmpFab = loOGScroll.gfTempName()
  lcSelected = " SELECT ITEMLOC.TOTWIP,ITEMLOC.TOTSTK,ITEM.CSTYMAJOR AS FABRIC FROM ITEM (INDEX = CSTYLE)INNER JOIN ITEMLOC (INDEX = STYDYE) ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEM.CDEFWARE = ITEMLOC.CWARECODE "
  lcWhereCondition = " ITEMLOC.DYELOT = '   ' AND ITEM.CINVTYPE = 0002 AND ITEMLOC.CINVTYPE = 0002 "
  lcSqlStatement = lcSelected + " WHERE " + lcWhereCondition
  lnResult1 = loOGScroll.orda.SqlRun (lcSqlStatement,lcTmpFab,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  IF lnResult1 >= 1
    lnBuffering = CURSORGETPROP("Buffering",lcTmpFab)
    =CURSORSETPROP("Buffering",3,lcTmpFab)
    SELECT (lcTmpFab)
    INDEX ON Fabric TAG &lcTmpFab
    SET ORDER TO TAG &lcTmpFab
  ENDIF
ENDIF

loStyDye   = CREATEOBJECT('RemoteTable','STYDYE','STYDYE','STYDYE',SET("Datasession"))
loStyle    = CREATEOBJECT("RemoteTable","STYLE","STYLE","STYLE",SET("Datasession"))
loFabDye   = CREATEOBJECT('RemoteTable','ITEMLOC','STYDYE','FABDYE',SET("Datasession"))
loFabric   = CREATEOBJECT('RemoteTable','ITEM','STYLE','FABRIC',SET("Datasession"))
loWareHous = CREATEOBJECT('RemoteTable','WAREHOUS','WAREHOUS','WAREHOUS',SET("Datasession"))
loOrdHdr   = CREATEOBJECT('RemoteTable','ORDHDR','ORDHDR','ORDHDR',SET("Datasession"))

*--Temp files names.
lcMatReq   = loOGScroll.gfTempName()
lcTmpItems = loOGScroll.gfTempName()
lcOldValue = ' '

*--Get the style major picture.
lcStyPict = '@! '+gfItemMask('PM')

*B608152,1 MMT  07/04/2007 fix bug of error while saving filters in Mat. Req. rep [Start]
*llRPTimeP = .F.
*B608152,1 MMT  07/04/2007 fix bug of error while saving filters in Mat. Req. rep [End]

RETURN

*!*************************************************************
*! Name      : lfFillVars
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Update needed variables in OG Init
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfFillVars

=lfGetColor()
=lfGetFrmat()
=lfGetBase()

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfGetColor

DIMENSION laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF

  *-- Check for existance of color segment in style structure.
  IF llExtSizSc .AND. laMajSeg[lnCnt,1] = 'S'
    *--Get the size length and width.
    lnSizePos = laMajSeg[lnCnt,4]
    lnSizeLen = LEN(laMajSeg[lnCnt,3])
  ENDIF
ENDFOR

*-- Get Color Position for Fabrics
=gfItemMask(@laMajSeg, '', '0002')
FOR lnCnt = 1 TO ALEN(laMajSeg, 1)
  *-- Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1] = 'C'
    *-- Get the color length and width.
    lnFClrSrt = laMajSeg[lnCnt,4]
    lnFClrEnd = LEN(laMajSeg[lnCnt,3])
  ENDIF
ENDFOR

lnFMajLen = LEN(gfItemMask("PM", '', '0002'))

RETURN

*!*************************************************************
*! Name      : lfUpdReq
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Update material requirement file.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfUpdReq

IF !llRPTimeP
  SELECT (lcMatReq)
  *-- Consider case of print by warehouse Yes.
  IF llRpByWare .AND. BOM.CCATGTYP $ 'FT'
    llFound = .F.
    IF !EMPTY(BOM.COPRCODE) .AND. loCodes.SEEK('NYMFGCODE')
      SELECT MACODES
      LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYMFGCODE';
        FOR CCODE_NO = BOM.COPRCODE .AND. CRLTD_NAM = 'CCONTCODE'
      IF FOUND() .AND. !EMPTY(CRLTD_VLU) .AND. loApVendor.SEEK(PADR(CRLTD_VLU,8)) .AND. !EMPTY(APVENDOR.CWARECODE)
        llFound = .T.
      ENDIF
    ENDIF
    SELECT (lcMatReq)
  ENDIF

  IF !SEEK(BOM.cCatgTyp+IIF(llRpByWare .AND. BOM.CCATGTYP $ 'FT',;
      IIF(llByFabLoc,'N'+lcFLoctn,IIF(llFound,'N'+APVENDOR.CWARECODE,'Y'+SPACE(6))),'')+;
      lcItem+lcCStyle)
    m.cUomCode = Bom.cUomCode
    loUom.SQLRUN("SELECT cUOM_V FROM UOM (INDEX = UOMCODE) WHERE cUOMCode = ?m.cUomCode")
    SELECT (lcMatReq)
    APPEND BLANK
    REPLACE STYLE     WITH lcCStyle     ,;
      Typ       WITH BOM.Typ      ,;
      cCatgTyp  WITH BOM.cCatgTyp ,;
      ITEM      WITH lcItem       ,;
      DESC      WITH BOM.DESC     ,;
      Uom       WITH IIF(!EOF("UOM"), Uom.cUom_V, ""),;
      CNT       WITH IIF(lnCnt=0,8,lnCnt),;
      lStyMake  WITH STYLE.MAKE   ,;
      cWareCode WITH ""

    *-- Consider case of print by warehouse Yes
    IF llRpByWare .AND. CCATGTYP $ 'FT'
      IF llByFabLoc
        REPLACE CFABLOC  WITH lcFLoctn,;
          CINHOUSE WITH 'N'
      ELSE
        IF llFound
          REPLACE CFABLOC  WITH APVENDOR.CWARECODE,;
            CINHOUSE WITH 'N'
        ELSE
          REPLACE CINHOUSE WITH 'Y'
        ENDIF
      ENDIF
    ENDIF
    REPLACE LBOM WITH .T.
  ENDIF

  *--Update the bom unit qty.
  IF !EMPTY(BOM.mSizes)
    lcAvlSizes = ALLTRIM(SUBSTR(MLINE(BOM.mSizes,1),AT('~',MLINE(BOM.mSizes,1))+1))
  ELSE
    *--All Sizes.
    lcAvlSizes = "1,2,3,4,5,6,7,8"
  ENDIF

  FOR I = 1 TO lnCnt
    lcSz = STR(I,1)
    IF lcSz $ lcAvlSizes
      REPLACE Qty&lcSz WITH Qty&lcSz + BOM.nBOMTotQty

      *-- Consider case of print by warehouse Yes
      REPLACE YIELD&lcSz WITH YIELD&lcSz + BOM.nBOMTotQty
    ENDIF
  ENDFOR
ELSE
  IF BOM.cCatGTyp $ 'FT' AND !SEEK(lcItem)
    APPEND BLANK
    REPLACE nLineNo  WITH 1,;
      ITEM     WITH lcItem ,;
      DESC     WITH BOM.DESC
  ENDIF
ENDIF

RETURN

*!*************************************************************
*! Name      : lfReadItem
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : read style major and color
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
FUNCTION lfReadItem

*--Get style component code.
IF cCatGTyp = 'S'
  *--If style size not used in cost sheet.
  IF !(STYLE.SCALE $ BOM.MSIZES)
    RETURN .F.
  ENDIF

  *--Get an equevelent item non major part.
  lcCompNmj = ''

  *--Non major length without scale segment.
  lnNmjPart = lnNMjrLn - IIF(llMScale,3,0)
  FOR lnI = 1 TO lnNmjPart
    lcCutChr  = SUBSTR(BOM.ITEM, lnMajorLn + 1 + lnI, 1)
    lcCompNmj = lcCompNmj + ;
      IIF(lcCutChr = '*', SUBSTR(STYLE.STYLE, lnMajorLn + 1 + lnI, 1), lcCutChr)
  ENDFOR
  lcItem = SUBSTR(ITEM, 1, lnMajorLn + 1) + lcCompNmj

  *--Get an equivalent item scale part.
  IF llMScale
    lcEqSCSz = STYLE.SCALE
    FOR lnMI = 1 TO MEMLINES(BOM.MSZCROSREF)
      lcMemSLine = MLINE(BOM.MSZCROSREF, lnMI)
      IF STYLE.SCALE $ lcMemSLine
        lcEqSCSz = SUBSTR(lcMemSLine, AT('~', lcMemSLine) + 1, 3)
        EXIT
      ENDIF
    ENDFOR
    lcItem = lcItem + lcEqSCSz
  ENDIF

  *--Check existance of style component.
  lnRcSv = IIF(!EOF('STYLE'), loStyle.RECNO(), 0)

  lcStyOrder = loStyle.lcTagName
  loStyle.SetOrder('STYLE')
  llSComFund = gfSEEK(lcItem, 'STYLE')

  IF !EMPTY(lnRcSv)
    loStyle.GoRec(lnRcSv)
  ENDIF
  loStyle.SetOrder(lcStyOrder)

  SELECT BOM
  IF !llSComFund
    RETURN .F.
  ENDIF
ELSE  && CCATGTYP $ 'FT'

  *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[START]
  *lcItem  = SUBSTR(BOM.Item, 1, lnMajorLn + 1)
  *lcColor = SUBSTR(Bom.Item, lnMajorLn + 2)
  *lcIClr  = IIF(lcColor = '*', SUBSTR(lcCStyle, lnClrSrt, lnClrEnd), lcColor)
  lcItem  = SUBSTR(BOM.ITEM, 1, lnFMajLen)
  lcColor = SUBSTR(Bom.ITEM, lnFMajLen + 2)
  lcIClr  = IIF(lcColor = '*', SUBSTR(lcCStyle, lnClrSrt-1, lnClrEnd+1), SUBSTR(Bom.ITEM, lnFMajLen + 1))
  *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

  lcItem  = lcItem + lcIClr
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfUpdItem
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Update items temp file.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
FUNCTION lfUpdItem

IF !SEEK(BOM.Typ+lcItem, lcTmpItems)
  SELECT (lcTmpItems)
  APPEND BLANK
  REPLACE Typ      WITH BOM.Typ      ,;
    cCatgTyp WITH BOM.cCatgTyp ,;
    ITEM     WITH lcItem
ENDIF
RETURN
*!
*!*************************************************************
*! Name      : lfChkOrder
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get the ordered pieces.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : laOrders[]
*!*************************************************************
FUNCTION lfChkOrder

PRIVATE lnAlias
lnAlias = SELECT()

lcTempOrd = loOGScroll.gfTempName()
lcTempOrd = 'OrdLine'

lnRow = ASCAN(loOGScroll.laFltExp,'ORDLINE')
IF lnRow > 0
  lnRow = ASUBSCRIPT(loOGScroll.laFltExp,lnRow,1)
  lcOrdLineFilter = loOGScroll.laFltExp[lnRow,2]
  lcRpExp1 = lcOrdLineFilter
ELSE
  lcRpExp1 = ".T."
ENDIF

loOrdLine.SEEK(lcCStyle)
SELECT (lcTempOrd)
SCAN WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcCStyle FOR &lcForWare
  loOrdHdr.SEEK('O'+ORDER)
  *--Check orders filter.
  IF ! &lcRpExp1
    LOOP
  ENDIF

  IF OrdHdr.STATUS $ 'OH'
    FOR I=1 TO lnCnt
      Z=STR(I,1)
      IF OrdHdr.STATUS = 'O'
        laOrders[1,I]=laOrders[1,I]+&lcTempOrd..Qty&Z
      ENDIF
      IF OrdHdr.STATUS $ 'OH'
        laOrders[2,I]=laOrders[2,I]+&lcTempOrd..Qty&Z
      ENDIF
    ENDFOR
    laOrders[1,9]=laOrders[1,1]+laOrders[1,2]+laOrders[1,3]+laOrders[1,4]+laOrders[1,5]+laOrders[1,6]+laOrders[1,7]+laOrders[1,8]
    laOrders[2,9]=laOrders[2,1]+laOrders[2,2]+laOrders[2,3]+laOrders[2,4]+laOrders[2,5]+laOrders[2,6]+laOrders[2,7]+laOrders[2,8]
    IF &lcChkFile..TotOrd=laOrders[2,9]
      EXIT
    ENDIF
  ENDIF
ENDSCAN
SELECT (lnAlias)
RETURN
*!
*!*************************************************************
*! Name      : lfChk_CT
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get WIP pieces from cuttkts.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : laWIP[]
*!*************************************************************
*!
FUNCTION lfChk_CT

PRIVATE lnAlias
lnAlias = SELECT()

loCutTktl.SEEK("0001"+lcCStyle+"PU")
SELECT CUTTKTL
SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = "0001"+lcCStyle+"PU" FOR &lcForWare
  loCutTktH.SEEK(CUTTKTL.cBusDocu+CUTTKTL.cStyType+CUTTKTL.PO)
  IF CUTTKTH.STATUS $ 'AOH'
    IF CUTTKTH.STATUS <> 'H' .AND. !loBomLine.SEEK('M'+'1'+CUTTKTH.PO+STR(CUTTKTL.LINENO,6)+&lcMatReq..Typ+'0001'+&lcMatReq..STYLE+IIF(&lcMatReq..cCatGTyp = 'S', '0001', '0002')+&lcMatReq..ITEM)
      LOOP
    ENDIF

    FOR I=1 TO lnCnt
      Z=STR(I,1)
      DO CASE
      CASE CUTTKTH.STATUS = 'A'
        laWIP[1,I]=IIF(TranCd = '1', (laWIP[1,I] + MAX(Qty&Z,0)) ,;
          (MAX(laWIP[1,I]-ABS(Qty&Z),0) ) )

      CASE CUTTKTH.STATUS = 'O'
        laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] + MAX(Qty&Z,0)) ,;
          (MAX(laWIP[2,I]-ABS(Qty&Z),0) ) )
      CASE CUTTKTH.STATUS = 'H'
        laWIP[3,I]=IIF(TranCd = '1', (laWIP[3,I] + MAX(Qty&Z,0)) ,;
          (MAX(laWIP[3,I]-ABS(Qty&Z),0) ) )
      ENDCASE
    ENDFOR
    IF &lcChkFile..TotWip=laWIP[1,9]+laWIP[2,9]+laWIP[3,9]
      EXIT
    ENDIF
  ENDIF
ENDSCAN
FOR I=1 TO lnCnt
  laWIP[1,9]=laWIP[1,9]+laWIP[1,I]
  laWIP[2,9]=laWIP[2,9]+laWIP[2,I]
  laWIP[3,9]=laWIP[3,9]+laWIP[3,I]
ENDFOR
SELECT (lnAlias)
RETURN
*!
*!*************************************************************
*! Name      : lfChk_PO
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get WIP pieces from P/Os.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : laWIP[]
*!*************************************************************
*!
FUNCTION lfChk_PO

PRIVATE lnAlias
lnAlias = SELECT()

loPosLn.SEEK("0001"+lcCStyle)
SELECT POSLN
SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = "0001"+lcCStyle;
    FOR cStyType $ 'PUN' AND !(TranCd $ '36') AND &lcForWare
  loPosHdr.SEEK(Posln.cBusDocu+Posln.cStyType+Posln.PO)
  IF POSHDR.STATUS $ 'OHA'

    *B608117,1 MMT 06/08/2007 fix bug of not taking c/t into consioderation while Calc. WIP[Start]
    *IF POSHDR.STATUS <> 'H' .AND. !loBomLine.SEEK('I'+'1'+POSHDR.PO+STR(POSLN.lineno,6)+&lcMatReq..Typ+'0001'+&lcMatReq..style+IIF(&lcMatReq..cCatGTyp = 'S', '0001', '0002')+&lcMatReq..item)
    IF POSHDR.STATUS <> 'H' .AND. !loBomLine.SEEK(IIF(Posln.cStyType = 'U','M','I')+'1'+POSHDR.PO+STR(POSLN.LINENO,6)+&lcMatReq..Typ+'0001'+&lcMatReq..STYLE+IIF(&lcMatReq..cCatGTyp = 'S', '0001', '0002')+&lcMatReq..ITEM)
      *B608117,1 MMT 06/08/2007 fix bug of not taking c/t into consioderation while Calc. WIP[End]

      LOOP
    ENDIF
    lcSign  = IIF(cBusDocu = 'R', '-', '+')
    lcOSign = IIF(cBusDocu = 'R', '+', '-')
    FOR I=1 TO lnCnt
      Z=STR(I,1)
      DO CASE
      CASE POSHDR.STATUS = 'A'
        laWIP[1,I]=IIF(TranCd = '1', (laWIP[1,I] &lcSign MAX(Qty&Z,0)) ,;
          (MAX(laWIP[1,I] &lcOSign ABS(Qty&Z),0) ) )
      CASE POSHDR.STATUS = 'O'
        laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] &lcSign MAX(Qty&Z,0)) ,;
          (MAX(laWIP[2,I] &lcOSign ABS(Qty&Z),0) ) )
      CASE POSHDR.STATUS = 'H'
        laWIP[3,I]=IIF(TranCd = '1', (laWIP[3,I] &lcSign MAX(Qty&Z,0)) ,;
          (MAX(laWIP[3,I] &lcOSign ABS(Qty&Z),0)))
      ENDCASE
    ENDFOR
  ENDIF
ENDSCAN
FOR I=1 TO lnCnt
  laWIP[1,9]=laWIP[1,9]+laWIP[1,I]
  laWIP[2,9]=laWIP[2,9]+laWIP[2,I]
  laWIP[3,9]=laWIP[3,9]+laWIP[3,I]
ENDFOR
SELECT (lnAlias)
RETURN
*!
*!*************************************************************
*! Name      : lfEditProj
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Edit Projection.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Call               : ARIABROW() , lfvEdtPrj()
*!*************************************************************
*!
FUNCTION lfEditProj

lcBrFields = "Style    :R :H=lcStyHdr    :30,"+;
  "lcDesc = lfGetStyDesc(Style)  :R :H='Description' :41,"+;
  "OReqTot  :R :H='Open'      :12,"+;
  "OHReqTot :R :H='Open+Hold' :12,"+;
  "Project  :R :H='Projected' :12 "

SELECT (lcMatReq)
SET ORDER TO TAG MRStyle

LOCATE

=ARIABROW('','Edit projection',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'=lfvEdtPrj()',;
  'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok')

RETURN

*!*************************************************************
*! Name      : lfGetStyDesc
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/30/2005
*! Purpose   : Get Style Description
*!*************************************************************
FUNCTION lfGetStyDesc
LPARAMETERS lcStyle

loStyle.SEEK(lcStyle)

RETURN STYLE.Desc1

*!
*!*************************************************************
*! Name      : lfvEdtPrj
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Modify Projection, Valid of edit projection.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfvEdtPrj

PRIVATE lnCurrRec, lcCurrSty

SELECT (lcMatReq)

llOk = .F.

loStyle.SEEK(STYLE)

SCATTER FIELDS nProj1,nProj2,nProj3,nProj4,nProj5,nProj6,nProj7,nProj8,PROJECT TO laPrjn

DO FORM (gcRepHome+"MA\MAEDTPJ") WITH STYLE.STYLE, STYLE.Desc1, 'laPrjn'

IF llOk

  *-- Aply the modified projected figures to all the
  *-- cost items for the edited style.
  SELECT (lcMatReq)

  lnCurrRec = RECNO()
  lcCurrSty = STYLE
  SET ORDER TO TAG MrStyItm
  SEEK lcCurrSty

  SCAN REST WHILE STYLE+cCatgTyp+ITEM = lcCurrSty
    GATHER FROM laPrjn FIELDS nProj1,nProj2,nProj3,nProj4,nProj5,nProj6,nProj7,nProj8,PROJECT
  ENDSCAN

  *-- Restore the old tag and re-positioning the record pointer.
  SET ORDER TO TAG MRStyle
  IF BETWEEN(lnCurrRec,1,RECCOUNT())
    GOTO lnCurrRec
  ENDIF

  llUpdPlan = .T.
ENDIF

SELECT (lcMatReq)
RETURN .F.

*!*************************************************************
*! Name      : lfPrintRep
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Start printing.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfPrintRep()
*!*************************************************************
*!
FUNCTION lfPrintRep

*-- consider case of Detail line layout [Start]
IF (lcRpRName # 'MAMATRQD')
  *--Clear -ve remain file from previous items Printed.
  SELECT (lcTmpRmain)
  ZAP
ENDIF

SELECT (lcMatReq)
SET ORDER TO TAG MATREQ

IF !llRPTimeP
  *-- consider case of Detail line layout
  IF lcRpRName = 'MAMATRQD'
    SET ORDER TO TAG MATREQ
  ENDIF

  *-- Sort by fabric location in case print by warehouse Yes [Start]
  IF llRpByWare
    SET ORDER TO TAG MRFABLOC

    *-- consider case of Detail line layout [Start]
    IF lcRpRName = 'MAMATRQD'
      SET ORDER TO TAG MRFABLOCDT
    ENDIF
  ENDIF

  *--Reset report calculated fields.
  IF lcRpRName <> 'MAMATRQD'
    REPLACE ALL nYTOWIP  WITH 0,;
      nYTOWIP1 WITH 0,;
      nYTOWIP2 WITH 0,;
      nYTOWIP3 WITH 0,;
      nYTOWIP4 WITH 0,;
      nYTOWIP5 WITH 0,;
      nYTOWIP6 WITH 0,;
      nYTOWIP7 WITH 0,;
      nYTOWIP8 WITH 0,;
      nNetReq  WITH 0,;
      nNetReq1 WITH 0,;
      nNetReq2 WITH 0,;
      nNetReq3 WITH 0,;
      nNetReq4 WITH 0,;
      nNetReq5 WITH 0,;
      nNetReq6 WITH 0,;
      nNetReq7 WITH 0,;
      nNetReq8 WITH 0,;
      nUsedReq1 WITH 0,;
      nUsedReq2 WITH 0,;
      nUsedReq3 WITH 0,;
      nUsedReq4 WITH 0,;
      nUsedReq5 WITH 0,;
      nUsedReq6 WITH 0,;
      nUsedReq7 WITH 0,;
      nUsedReq8 WITH 0,;
      nUsedReq  WITH 0

  ENDIF

  *-- Empty the POCT field [Start]
  REPLACE ALL POCT WITH ''

  *--Initialize printing report variables.
  STORE 0   TO lnOnHand,lnOnOrdr,lnConv,lnLeadTm,lnYield,lnYield1,lnYield2,;
    lnAvl1,lnAvl2,lnAvl3,lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8
  STORE " " TO lcDescrp,lcFabVen,lcOldItem,lcSDescrp,lcTkTRUn,;
    lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8

  *--[1] Fabrics requirement report.
  SET KEY TO 'F'

  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    SET ORDER TO MatreqC
  ENDIF

  llFromOTS = .F.

  IF SEEK('F')
    *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
    IF loogscroll.cTextRepType == "EXCEL"
      =lfPrpToXls()
      SELECT (lcMatReq)
      =SEEK('F')
    ENDIF   
    *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]
    DO gfDispRe WITH EVAL('lcRpRname')
    *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[Start]
    loogscroll.lAdditive = .T.
    *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[END]
  ENDIF

  *--[2] Trims requirement report.
  SELECT (lcMatReq)
  SET KEY TO 'T'
  IF SEEK('T')
    *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
    IF loogscroll.cTextRepType == "EXCEL"
      =lfPrpToXls()
      SELECT (lcMatReq)
      =SEEK('T')
    ENDIF   
    *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]
    *--Initialize printing variables
    =lfInitVar()
    DO gfDispRe WITH EVAL('lcRpRname')
    *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[Start]
    loogscroll.lAdditive = .T.
    *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[END]
  ENDIF

  *--[3] Style components requirement report.
  SELECT (lcMatReq)
  SET KEY TO 'S'
  IF SEEK('S')
    *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
    IF loogscroll.cTextRepType == "EXCEL"
      =lfPrpToXls()
      SELECT (lcMatReq)
      =SEEK('S')
    ENDIF   
    *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]
    *--Initialize printing variables
    =lfInitVar()
    DO gfDispRe WITH EVAL('lcRpRname')
  ENDIF
  SET KEY TO
  *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[Start]
  loogscroll.lAdditive = .F.
  *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[END]

ELSE
  SET ORDER TO TAG MATREQ2
  SET FILTER TO TOTAL <> 0
  LOCATE

  IF EOF()
    SET FILTER TO
    RETURN .F.
  ENDIF
  *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
  IF loogscroll.cTextRepType == "EXCEL"
    =lfPrpToXls()
    SELECT (lcMatReq)
    LOCATE
  ENDIF   
  *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]
  =gfDispRe(lcRpRName)

  SET FILTER TO
ENDIF

RETURN
*!
*!*************************************************************
*! Name      : lfInitVar
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to initialize report calculated varbls.
*!*************************************************************
FUNCTION lfInitVar
STORE 0   TO lnOnHand,lnOnOrdr,lnConv,lnLeadTm,lnYield,lnYield1,lnYield2,;
  lnAvl1,lnAvl2,lnAvl3,lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8
STORE " " TO lcDescrp,lcFabVen,lcOldItem,lcSDescrp,lcTkTRUn,;
  lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8
RETURN

*!*************************************************************
*! Name      : lfUpdStyle
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Update style plane with projection.
*!*************************************************************
FUNCTION lfUpdStyle

WAIT WINDOW 'Updating projections to the style master file ...' NOWAIT

SELECT (lcMatReq)
SET ORDER TO TAG MRStyItm
SCAN
  SELECT STYLE
  =loStyle.SEEK(&lcMatReq..STYLE)

  =RLOCK()
  REPLACE PLAN1   WITH &lcMatReq..nProj1,;
    PLAN2   WITH &lcMatReq..nProj2,;
    PLAN3   WITH &lcMatReq..nProj3,;
    PLAN4   WITH &lcMatReq..nProj4,;
    PLAN5   WITH &lcMatReq..nProj5,;
    PLAN6   WITH &lcMatReq..nProj6,;
    PLAN7   WITH &lcMatReq..nProj7,;
    PLAN8   WITH &lcMatReq..nProj8,;
    TOTPLAN WITH &lcMatReq..PROJECT
  UNLOCK
ENDSCAN

LOCAL lcTranCode

*-- Begin Updating Transaction
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '', .T.)

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  RETURN .F.
ENDIF

=loStyle.TABLEUPDATE(lcTranCode)

lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode, .T.)
IF lnConnHandler <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
  RETURN .F.
ENDIF

SELECT (lcMatReq)
SET ORDER TO TAG MATREQ
WAIT CLEAR
RETURN

****************************

*** FRX FUNCTIONS

****************************
*!
*!*************************************************************
*! Name      : lfGetInform
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Get Fabric,Trim or Style comp information like
*!             description,onhand and onorder.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
*B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP T20070221.0012 [Start]
*FUNCTION lfGetInfo
FUNCTION lfGetInform
*B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP T20070221.0012 [End]


SELECT (lcMatReq)

*-- Restore the memory variables in case of MRP existing
IF lcRpMrpBas = 'E'

  *B609665,1 WAM 08/23/2011 Check if the memo field is not empty
  IF !EMPTY(MFLTVAL)
    *B609665,1 WAM 08/23/2011 (End)

    RESTORE FROM MEMO MFLTVAL ADDITIVE
    FOR lnI = 1 TO 20
      &laSaveVar[lnI,1]. = laSaveVar[lnI,2]
    ENDFOR
    *B609665,1 WAM 08/23/2011 Check if the memo field is not empty
  ENDIF
  *B609665,1 WAM 08/23/2011 (End)
  RETURN ''
ENDIF

IF cCatgTyp = 'S'
  IF loStyle.SEEK(ITEM)
    lcDescrp  = STYLE.Desc1
    lnOnHand  = STYLE.TotStk
    lnOnOrdr  = STYLE.TotWip
    lnLeadTm  = STYLE.LeadTime
    lnFabcost = STYLE.nICost1
    FOR lnI=1 TO 8
      lcZ=STR(lnI,1)
      lnAvl&lcZ = ( STYLE.Stk&lcZ + STYLE.WIP&lcZ)
    ENDFOR
  ELSE
    STORE 0   TO lnOnHand, lnOnOrdr, lnConv, lnLeadTm, lnFabcost
    STORE " " TO lcDescrp, lcFabVen, lcUOMBuy
  ENDIF
ELSE

  *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
  IF loFabric.SEEK("0002"+ITEM)
    *!*	  lcSeekItem = ''
    *!*	  lnAstPosition = AT('*', Item)
    *!*	  lcSeekItem = IIF(lnAstPosition = 0, Item, ALLTRIM(SUBSTR(Item, 1, lnAstPosition - 2)))
    *!*	  IF loFabric.Seek("0002"+lcSeekItem)
    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

    *-- Define a variable to hold Fabric description
    lcFDesc    = FABRIC.DESC

    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
    *lcFabColor = SUBSTR(Fabric.Style,lnClrSrt,lnClrEnd)
    lcFabColor = SUBSTR(Fabric.STYLE,lnFClrSrt,lnFClrEnd)
    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

    lcClrDsc   = gfCodDes(lcFabColor,'COLOR     ')
    lcFabVen   = FABRIC.Vendor
    lnConv     = IIF(loUom.SEEK(FABRIC.cConvBuy),Uom.nConf,1)
    lnLeadTm   = FABRIC.LeadTime
    lnFabcost  = FABRIC.nICost1
    lcUOMBuy   = IIF(loUom.SEEK(FABRIC.cConvBuy),Uom.cUom_b,"")

    *-- Print the fabric openqty by yards for Mexx
    lnfUOM   = IIF(loUom.SEEK(FABRIC.cConvBuy),Uom.nConf,1)
    lcDescrp = gfCodDes(lcFabColor,'COLOR     ')+' - '+FABRIC.DESC

    *-- If fabric location was selected get the onhand and onorder from there.
    *-- Include the trim location
    lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'ITEMLOC.CWARECODE'),1)
    lcFLocFile     = loOGScroll.laOgFxFlt[lnLocCondition,6]

    *-- If Style Location Are selected.
    IF !EMPTY(lcFLocFile)
      SELECT &lcFLocFile
      INDEX ON cWareCode TAG &lcFLocFile
    ENDIF

    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
    =loFABDYE.SEEK(Fabric.cInvType+Fabric.STYLE)
    SELECT FABDYE
    SUM TOTSTK, TOTWIP REST WHILE cInvType+STYLE+cWareCode+Dyelot = Fabric.cInvType+Fabric.STYLE;
      FOR EMPTY(Dyelot) TO lnOnHand, lnOnOrdr

    *!*	    IF lnAstPosition > 0
    *!*	      =loFABDYE.SEEK(Fabric.cInvType+lcSeekItem)
    *!*	      SELECT FABDYE
    *!*	      SUM TOTSTK, TOTWIP REST WHILE cInvType+Style+cWareCode+Dyelot = Fabric.cInvType+lcSeekItem;
    *!*	        FOR EMPTY(Dyelot) TO lnOnHand, lnOnOrdr

    *!*	    ELSE
    *!*	      =loFABDYE.SEEK(Fabric.cInvType+Fabric.Style)
    *!*	      SELECT FABDYE
    *!*	      SUM TOTSTK, TOTWIP REST WHILE cInvType+Style+cWareCode+Dyelot = Fabric.cInvType+Fabric.Style;
    *!*	        FOR EMPTY(Dyelot) TO lnOnHand, lnOnOrdr
    *!*	    ENDIF
    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]
  ELSE
    STORE 0   TO lnOnHand, lnOnOrdr ,lnConv ,lnLeadTm  ,lnFabcost
    STORE " " TO lcDescrp, lcFabVen,lcUOMBuy
  ENDIF
ENDIF

*-- Save memory variables to restore them when print existing MRP report [Start]
FOR lnI = 1 TO 20
  laSaveVar[lnI,2] = EVALUATE(laSaveVar[lnI,1])
ENDFOR

SELECT (lcMatReq)
SAVE TO MEMO MFLTVAL ALL LIKE laSaveVar

RETURN ''
*!
*!*************************************************************
*! Name      : lfLGetInfo
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/98
*! Purpose   : Get Info on line level (Style/item).
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
FUNCTION lfLGetInfo

*B609665,1 WAM 08/23/2011 Add style tag
*IF loStyle.Seek(Style)
IF loStyle.SEEK(STYLE,'Style')
  *B609665,1 WAM 08/23/2011 (End)

  lcSDescrp = STYLE.DESC
  loScale.SEEK('S'+STYLE.SCALE)
  FOR lnI=1 TO 8
    lcI = STR(lnI,1)
    lcSize&lcI = SCALE.Sz&lcI
  ENDFOR
ELSE
  lcDescrp = " "
ENDIF

*--Average Yield.
lnYield  = (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/CNT

RETURN ''
*!
*!*************************************************************
*! Name      : lfvReq
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Calculate the requirement use (pieces*yield)
*!*************************************************************
*! Passed Parameters  : Complute
*!                      Requirement yield
*!                             'O' for open order
*!                             'H' for open+hold
*!                             'P' for projection
*!                     'S' Open Wip yield
*!*************************************************************
*! Returns            : lnItmRec -> requirement for item/color.
*!*************************************************************
FUNCTION lfvReq
PARAMETERS lcForTyp

lnItmRec = 0


DO CASE
CASE lcForTyp='O'
  *- get the requirement by size yield insted of ave yield.
  lnItmRec = (oreq1*Qty1)+(oreq2*Qty2)+(oreq3*Qty3)+(oreq4*Qty4)+;
    (oreq5*Qty5)+(oreq6*Qty6)+(oreq7*Qty7)+(oreq8*Qty8)

  *-- Consider the WIP case [Start]
  IF lcRpReqBas = 'W'
    lnItmRec = ((oHreq1*(YIELD1-QTY1))+(OREQ1*QTY1))+((oHreq2*(YIELD2-QTY2))+(OREQ2*QTY2))+;
      ((oHreq3*(YIELD3-QTY3))+(OREQ3*QTY3))+((oHreq4*(YIELD4-QTY4))+(OREQ4*QTY4))+;
      ((oHreq5*(YIELD5-QTY5))+(OREQ5*QTY5))+((oHreq6*(YIELD6-QTY6))+(OREQ6*QTY6))+;
      ((oHreq7*(YIELD7-QTY7))+(OREQ7*QTY7))+((oHreq8*(YIELD8-QTY8))+(OREQ8*QTY8))
  ENDIF
CASE lcForTyp='H'
  lnItmRec = (oHreq1*Qty1)+(oHreq2*Qty2)+(oHreq3*Qty3)+(oHreq4*Qty4)+;
    (oHreq5*Qty5)+(oHreq6*Qty6)+(oHreq7*Qty7)+(oHreq8*Qty8)

  *-- Consider the WIP case [Start]
  IF lcRpReqBas = 'W'
    lnItmRec = (oHreq1*YIELD1)+(oHreq2*YIELD2)+(oHreq3*YIELD3)+(oHreq4*YIELD4)+;
      (oHreq5*YIELD5)+(oHreq6*YIELD6)+(oHreq7*YIELD7)+(oHreq8*YIELD8)
  ENDIF
CASE lcForTyp='P'
  lnItmRec = (NPROJ1*Qty1)+(NPROJ2*Qty2)+(NPROJ3*Qty3)+(NPROJ4*Qty4)+;
    (NPROJ5*Qty5)+(NPROJ6*Qty6)+(NPROJ7*Qty7)+(NPROJ8*Qty8)

  *-- Consider the WIP case.
  IF lcRpReqBas = 'W' .AND. !llUpdPlan
    IF lcRpBasePj = 'O'
      lnItmRec = 0
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        lnohreq = EVALUATE('OHREQ'+lcI) + ROUND(EVALUATE('OHREQ'+lcI) * (lnRpPrIncr/100),0)
        lnoxreq = EVALUATE('OREQ'+lcI)  + ROUND(EVALUATE('OREQ'+lcI)  * (lnRpPrIncr/100),0)
        lnItmRec = lnItmRec + (lnohreq*(EVALUATE('YIELD'+lcI+'-QTY'+lcI))) +;
          (lnoxreq*EVALUATE('QTY'+lcI))
      ENDFOR
    ENDIF
    IF lcRpBasePj = 'H'
      lnItmRec = (NPROJ1*YIELD1)+(NPROJ2*YIELD2)+(NPROJ3*YIELD3)+(NPROJ4*YIELD4)+;
        (NPROJ5*YIELD5)+(NPROJ6*YIELD6)+(NPROJ7*YIELD7)+(NPROJ8*YIELD8)
    ENDIF
  ENDIF
CASE lcForTyp='S'
  lnItmRec = nYToWIP
ENDCASE

RETURN (lnItmRec)

*!*************************************************************
*! Name      : lfvReqln
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Calculate the requirement use per size.
*!*************************************************************
*! Passed Parameters  : lcForTyp > Requirement for type
*!                             'O' for open order
*!                             'H' for open+hold
*!                             'P' for projection
*!                             'S' Open Wip yield
*!                     lnRSize > Size no to get req. for it.
*!*************************************************************
*! Returns            : lnItmRec -> requirement for item/color.
*!*************************************************************
FUNCTION lfvReqLn
PARAMETERS lcForTyp, lnRSize

lcRSize  = STR(lnRSize,1)
lnItmRec = 0

DO CASE
CASE lcForTyp='O'
  lnItmRec = (OReq&lcRSize * Qty&lcRSize)
CASE lcForTyp='H'
  lnItmRec = (OHReq&lcRSize * Qty&lcRSize)
CASE lcForTyp='P'
  lnItmRec = (nProj&lcRSize * Qty&lcRSize)
  *--Open Wip yards on P/o or C/t. passed as 'S'
CASE lcForTyp='S'
  lnItmRec = nYToWIP&lcRSize
ENDCASE

RETURN (lnItmRec)

*!*************************************************************
*! Name      : lfRqIsUsd
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Get issed Used and Net required qty.
*!*************************************************************
*! Passed Parameters  : lcPrStyle -> Current style
*!                      lcPrItem  -> Current Item
*!                      lcPrIClr  -> Current Item color
*!                      llPrMake  -> Style Make (Dom. or Imp.)
*!                      lcPrTyp   -> Item type
*!                      lnPrOpnWip-> Open Style WIP.
*!                      llClcIssue-> Need to calculate issue.
*!*************************************************************
*! Returns            : space(0) to not print anything in repo.
*!*************************************************************
*! Example   :
*! lfRqIsUsd(EVAL(lcMatReq+'.Style'),EVAL(lcMatReq+'.Item'),;
*!            EVAL(lcMatReq+'.IClr'),EVAL(lcMatReq+'.lStyMake'),;
*!            EVAL(lcMatReq+'.Typ'),EVAL(lcMatReq+'.nOpnWip'),;
*!           (EVAL(lcMatReq+'.Item')+EVAL(lcMatReq+'.Iclr')<>lcOldItem))
*!*************************************************************
FUNCTION lfRqIsUsd
PARAMETERS lcPrStyle,lcPrItem,lcPrIClr,llPrMake,lcPrTyp,lnPrOpnWip,llClcIssue


IF lcRpMrpBas = 'E'
  RETURN ''
ENDIF


IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  SELECT (lcMatReq)
  lnXRecNo = RECNO()
  SET ORDER TO TAG Matreq
ENDIF

*--Hold old item/color code.
lcOldItem = lcPrItem + lcPrIClr
lcOldItem = lcPrItem

*--If no open wip qty exist and no need to calculate issue then exit.
IF lnPrOpnWip = 0  AND ! llClcIssue
  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    SELECT (lcMatReq)
    GO lnXRecNo
    SET ORDER TO TAG MatreqC
    SET KEY TO cCatgTyp
    GO lnXRecNo
  ENDIF
  *-- Calculate the net req. by style [Start]
  =lfUpdNetRq()
  RETURN ''
ENDIF

*--Work files.
IF !loPosLn.SEEK('0001'+lcPrStyle)
  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    SELECT (lcMatReq)
    GO lnXRecNo
    SET ORDER TO TAG MatreqC
    SET KEY TO cCatgTyp
    GO lnXRecNo
  ENDIF
  RETURN ''
ELSE
  lcSvTkt = "  "
  lnAlias = SELECT()

  SELECT (lcMatReq)
  REPLACE nYTOWIP  WITH 0,;
    nYTOWIP1 WITH 0,;
    nYTOWIP2 WITH 0,;
    nYTOWIP3 WITH 0,;
    nYTOWIP4 WITH 0,;
    nYTOWIP5 WITH 0,;
    nYTOWIP6 WITH 0,;
    nYTOWIP7 WITH 0,;
    nYTOWIP8 WITH 0,;
    nUsedReq WITH 0,;
    nUsedReq1 WITH 0,;
    nUsedReq2 WITH 0,;
    nUsedReq3 WITH 0,;
    nUsedReq4 WITH 0,;
    nUsedReq5 WITH 0,;
    nUsedReq6 WITH 0,;
    nUsedReq7 WITH 0,;
    nUsedReq8 WITH 0,;
    nNetReq  WITH 0,;
    nNetReq1 WITH 0,;
    nNetReq2 WITH 0,;
    nNetReq3 WITH 0,;
    nNetReq4 WITH 0,;
    nNetReq5 WITH 0,;
    nNetReq6 WITH 0,;
    nNetReq7 WITH 0,;
    nNetReq8 WITH 0,;
    PoCt     WITH ''

  SELECT POSLN
  SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = '0001' + lcPrStyle FOR TranCd = '1'
    IF !loPOSHDR.SEEK(cBusDocu+cStyType+PO) OR !(POSHDR.STATUS $ 'O')
      LOOP
    ENDIF
    *--Tiket type.
    lcTktType = IIF(POSLN.cStyType = 'U', 'M', 'I')

    *-- Caluclate the WIP for each PO or CT because the program multiply the
    *-- BOM.untiQty * the wip witch is rong because the Untiqty changed from Po/ct to another
    DECLARE laWipPln[9]
    laWipPln = 0
    =lfGtWipPln()
    IF laWipPln[9] <> 0
      *--Seek on full expresion in BomLine.

      *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [Start]
      *lcBomLnKey = lcTktType+'1'+PO+STR(POSLN.LineNo,6)+lcPrTyp+'0001'+lcPrStyle+IIF(EVALUATE(lcMatReq+'.cCatGTyp') = 'S','0001','0002')+lcPrItem
      lcBomLnKey = lcTktType+'1'+POSLN.PO+STR(POSLN.LINENO,6)+lcPrTyp+'0001'+lcPrStyle+IIF(EVALUATE(lcMatReq+'.cCatGTyp') = 'S','0001','0002')+lcPrItem
      *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [End]

      SELECT BOMLINE
      IF loBomLine.SEEK(lcBomLnKey)
        IF llRpByWare .AND. CCATGTYP $ 'FT'
          IF !EMPTY(COPRCODE)
            IF loMfgOprHd.SEEK(CIMTYP+CTKTNO+COPRCODE)
              IF !EMPTY(MFGOPRHD.CCONTCODE)
                IF loApVendor.SEEK(MFGOPRHD.CCONTCODE)
                  llFound = !EMPTY(APVENDOR.CWARECODE)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          IF llFound
            IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'N'+APVENDOR.CWARECODE
              LOOP
            ENDIF
          ELSE
            IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'Y'+SPACE(6)
              LOOP
            ENDIF
          ENDIF
        ENDIF
        lnBomUqt = BOMLINE.UnitQty
        SELECT (lcMatReq)
        REPLACE nYTOWIP  WITH nYTOWIP  + (laWipPln[9] * lnBomUqt),;
          nYTOWIP1 WITH nYTOWIP1 + (laWipPln[1] * lnBomUqt ),;
          nYTOWIP2 WITH nYTOWIP2 + (laWipPln[2] * lnBomUqt ),;
          nYTOWIP3 WITH nYTOWIP3 + (laWipPln[3] * lnBomUqt ),;
          nYTOWIP4 WITH nYTOWIP4 + (laWipPln[4] * lnBomUqt ),;
          nYTOWIP5 WITH nYTOWIP5 + (laWipPln[5] * lnBomUqt ),;
          nYTOWIP6 WITH nYTOWIP6 + (laWipPln[6] * lnBomUqt ),;
          nYTOWIP7 WITH nYTOWIP7 + (laWipPln[7] * lnBomUqt ),;
          nYTOWIP8 WITH nYTOWIP8 + (laWipPln[8] * lnBomUqt )
      ENDIF
    ENDIF


    *-- Fix bug of duplicating the issue and remaining Qty [Begin]
    *-- in case of po the next command will prevent lineno mre than 1
    IF loCtktBom.SEEK(lcTktType+POSLN.PO+lcPrTyp+IIF(EVALUATE(lcMatReq+'.cCatGTyp') = 'S','0001','0002')+lcPrItem)
      *--If style component need to calculate the net required by size ,
      *--used in remaining.
      IF EVALUATE(lcMatReq+'.cCatgTyp') = 'S'
        STORE 0 TO lnIssue,lnNRequ,lnNrqu1,lnNrqu2,lnNrqu3,lnNrqu4,lnNrqu5,lnNrqu6,lnNrqu7,lnNrqu8
        STORE 0 TO lnIssue1,lnIssue2,lnIssue3,lnIssue4,lnIssue5,lnIssue6,lnIssue7,lnIssue8

        SELECT CTKTBOM
        SUM REST Used_Qty,Used_Qty1,Used_Qty2,Used_Qty3,Used_Qty4,Used_Qty5,Used_Qty6,Used_Qty7,Used_Qty8,;
          MAX(Req_Qty-Used_Qty,0),MAX(Req_Qty1-Used_Qty1,0),MAX(Req_Qty2-Used_Qty2,0),;
          MAX(Req_Qty3-Used_Qty3,0),MAX(Req_Qty4-Used_Qty4,0),MAX(Req_Qty5-Used_Qty5,0),;
          MAX(Req_Qty6-Used_Qty6,0),MAX(Req_Qty7-Used_Qty7,0),MAX(Req_Qty8-Used_Qty8,0);
          TO lnIssue,lnIssue1,lnIssue2,lnIssue3,lnIssue4,lnIssue5,lnIssue6,lnIssue7,lnIssue8,;
          lnNRequ,lnNrqu1,lnNrqu2,lnNrqu3,lnNrqu4,lnNrqu5,lnNrqu6,lnNrqu7,lnNrqu8 ;
          WHILE cIMTyp+Cuttkt+typ+ITEM = lcTktType+POSLN.PO+lcPrTyp+lcPrItem

        SELECT (lcMatReq)
        lnSavRec = RECNO()
        lcSekKey = cCatgTyp + ITEM

        SEEK lcSekKey
        SCAN REST WHILE cCatgTyp + ITEM = lcSekKey
          IF !(lcTktType+POSLN.PO $ PoCt )
            REPLACE  nUsedReq WITH nUsedReq + lnIssue,;
              nUsedReq1 WITH nUsedReq1 + lnIssue1,;
              nUsedReq2 WITH nUsedReq2 + lnIssue2,;
              nUsedReq3 WITH nUsedReq3 + lnIssue3,;
              nUsedReq4 WITH nUsedReq4 + lnIssue4,;
              nUsedReq5 WITH nUsedReq5 + lnIssue5,;
              nUsedReq6 WITH nUsedReq6 + lnIssue6,;
              nUsedReq7 WITH nUsedReq7 + lnIssue7,;
              nUsedReq8 WITH nUsedReq8 + lnIssue8,;
              nNetReq  WITH nNetReq  + lnNRequ,;
              nNetReq1 WITH nNetReq1 + lnNrqu1,;
              nNetReq2 WITH nNetReq2 + lnNrqu2,;
              nNetReq3 WITH nNetReq3 + lnNrqu3,;
              nNetReq4 WITH nNetReq4 + lnNrqu4,;
              nNetReq5 WITH nNetReq5 + lnNrqu5,;
              nNetReq6 WITH nNetReq6 + lnNrqu6,;
              nNetReq7 WITH nNetReq7 + lnNrqu7,;
              nNetReq8 WITH nNetReq8 + lnNrqu8,;
              PoCt     WITH poCt + lcTktType+POSLN.PO+','
          ENDIF
        ENDSCAN
        GOTO lnSavRec
      ELSE

        STORE 0 TO lnIssue,lnNRequ

        *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [Start]
        *This modification done because the report get wrong issued values in case of
        *same material is existing in more than one style in the same PO
        SELECT (lcMatReq)
        lcFindEXP = IIF(llRpByWare,'cCatgTyp+cInHouse+cFabLoc+Item','cCatgTyp+Item')
        lcExpLocate =IIF(llRpByWare,cCatgTyp+cInHouse+cFabLoc+ITEM,cCatgTyp+ITEM)
        lnSavOldRec = RECNO()
        LOCATE
        LOCATE FOR &lcFindEXP = lcExpLocate  AND lcTktType+POSLN.PO $ PoCt AND RECNO()< lnSavOldRec
        IF !FOUND()
          SELECT (lcMatReq)
          GO RECORD lnSavOldRec
          *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[End]

          SELECT CTKTBOM
          SUM REST Used_Qty,MAX(Req_Qty-Used_Qty,0) TO lnIssue,lnNRequ ;
            WHILE cIMTyp+Cuttkt+typ+ITEM = lcTktType+POSLN.PO+lcPrTyp+lcPrItem
          *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [Start]
        ENDIF
        SELECT (lcMatReq)
        GO RECORD lnSavOldRec
        *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [End]


        *-- Not include style outside filter in the net req.
        =lfGetNetRq()


        SELECT (lcMatReq)
        lnSavRec = RECNO()

        lcSekKey = IIF(llRpByWare,cCatgTyp+cInHouse+cFabLoc+ITEM,cCatgTyp+ITEM)
        lcWhrCnd = IIF(llRpByWare,'cCatgTyp+cInHouse+cFabLoc+Item','cCatgTyp+Item')+'=lcSekKey'

        SEEK lcSekKey

        *-- before accumulate the usedqty and the netreq qty we must check frst
        *-- if this PO/Ct is already acumulate to the file
        SCAN REST WHILE &lcWhrCnd. FOR STYLE = lcPrStyle
          *B609959,1 MMT 06/12/2012 Incorrect Remaining MT When same style is found in PO more than once[Start]
          *!*	          IF !(lcTktType+POSLN.PO $ PoCt )
          *!*	            REPLACE nUsedReq WITH nUsedReq + lnIssue,;
          *!*	                    nNetReq  WITH nNetReq  + lnNRequ,;
          *!*	                    PoCt     WITH poCt + lcTktType + POSLN.PO + ','
          *!*	          ENDIF
          IF !(lcTktType+POSLN.PO $ PoCt )
            REPLACE nUsedReq WITH nUsedReq + lnIssue,;
              PoCt     WITH poCt + lcTktType + POSLN.PO + ','
          ENDIF
          REPLACE nNetReq  WITH nYTOWip - nUsedReq
          *B609959,1 MMT 06/12/2012 Incorrect Remaining MT When same style is found in PO more than once[END]
        ENDSCAN
        GOTO lnSavRec
      ENDIF
    ENDIF
    lcSvTkt = POSLN.PO
  ENDSCAN
ENDIF

IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  SELECT (lcMatReq)
  GO lnXRecNo
  SET ORDER TO TAG MatreqC
  SET KEY TO cCatgTyp
  GO lnXRecNo
ENDIF
SELECT (lnAlias)
RETURN ''
*!
*!*************************************************************
*! Name      : lfRemain
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Compute remaining and check -ve remaining.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Call               : lfSavNRItm()
*!*************************************************************
*! Returns            : lnRemain -> Remaining.
*!*************************************************************
FUNCTION lfRemain
PARAMETERS lcForTyp

*--Initilize remaining as zero.
STORE 0 TO lnRemReq , lnRemain, lnTotAvail, lnTotReq
DIMENSION laRemain[8]
STORE 0 TO laRemain

IF lcRpBasePj = 'P' AND lcForTyp = 'P'
  lnRemain = (lnOnHand + lnOnOrdr) - lnPReq
ELSE
  IF EVAL(lcMatReq+'.cCatgTyp') = 'S'
    *-- Calculate the remainning per size and accumulate it.
    FOR lnI = 1 TO 8
      lcZ=STR(lnI,1)
      lnRequr  = EVALUATE('ln'+lcForTyp+'Req'+lcZ)
      lnRemReq = IIF(lcRpReqBas='O', lnRequr , ABS( lnRequr - lnYTWip&lcZ ))

      *--Read Remaining and acumulate for all sizes.

      *WSH
      *lnRemain = lnRemain +;
      (lnAvl&lcZ - ( lnRemReq + EVALUATE(lcMatReq+'.nNetReq'+lcZ)))
      laRemain[lnI] = lnAvl&lcZ - (lnRemReq + IIF(lcRpReqBas = 'O', -EVALUATE(lcMatReq+'.nUsedReq'+lcZ), EVALUATE(lcMatReq+'.nNetReq'+lcZ)))
      lnRemain      = lnRemain + laRemain[lnI]
      *WSH

    ENDFOR
  ELSE
    lnRemReq = IIF(lcRpReqBas = 'O' ,EVALUATE('ln'+lcForTyp+'Req'),;
      ABS(EVALUATE('ln'+lcForTyp+'Req') - lnYTWip))
    *--Read Remaining.
    *-- Calculate the net req. by style.

    *WSH
    *lnRemain = (lnOnHand + lnOnOrdr) - (lnRemReq + lnNetReq)

    *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [Start]
    *lnRemain = (lnOnHand + lnOnOrdr) - (lnRemReq + IIF(lcRpReqBas = 'O', -lnUsedReq, lnNetReq))
    lnRemain = (lnOnHand+lnOnOrdr) - ( lnRemReq + lnNetReq )
    *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [End]

    *WSH

  ENDIF
ENDIF

*--Check negative remaining for fabrics and trims.

*WSH
*IF EVALUATE(lcMatReq+'.cCatgTyp') $ 'FT' AND lcForTyp = lcRpRmChk AND lnRemain < 0
IF lcForTyp = lcRpRmChk AND lnRemain < 0
  *WSH

  *-- Take remaining qty insted of requirement.
  lnRequired = ABS(lnRemain)
  IF lcRpMrpBas = 'N'
    =lfSavNRItm()
  ENDIF
ENDIF

*B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
DO CASE
CASE  lcForTyp = 'O'
  REPLACE nORemin WITH lnRemain IN (lcMatReq)
CASE  lcForTyp = 'H'
  REPLACE nHRemin WITH lnRemain IN (lcMatReq)
CASE  lcForTyp = 'P'
  REPLACE nPRemin WITH lnRemain IN (lcMatReq)
OTHERWISE
ENDCASE
*B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]


RETURN (lnRemain)

*!*************************************************************
*! Name      : lfSavNRItm
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Save item if negative remaining.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
FUNCTION lfSavNRItm

*--Update -ve remaining only for Imported materials.

*WSH
IF EVALUATE(lcMatReq + '.cCatGTyp') = 'S'
  IF !loStyle.SEEK(EVALUATE(lcMatReq + '.Item')) OR STYLE.MAKE
    RETURN
  ENDIF
ELSE
  IF !loFABRIC.SEEK('0002' + EVALUATE(lcMatReq + '.Item')) OR FABRIC.MAKE
    RETURN
  ENDIF
ENDIF
*WSH

SCATTER MEMVAR

SELECT (lcTmpRmain)
lnConv = IIF(lnConv = 0, 1, lnConv)

IF !SEEK(EVALUATE(lcMatReq + '.cCatGTyp') + EVALUATE(lcMatReq + '.Item') + IIF(llRPTimeP, DTOC(ldComplete), ''))
  APPEND BLANK

  *-- Consider case of use vendor referance [Start]
  *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
  *!*	  IF EVALUATE(lcMatReq + '.cCatGTyp') <> 'S' AND llVenRef .AND. loVENDMATL.SEEK(PADR(lcFabVen,8)+PADR(EVALUATE(lcMatReq+'.ITEM'),7)+;
  *!*	                         SUBSTR(EVALUATE(lcMatReq+'.ITEM'), lnFClrSrt, lnFClrEnd))
  IF EVALUATE(lcMatReq + '.cCatGTyp') <> 'S' AND llVenRef .AND. loVENDMATL.SEEK(PADR(lcFabVen,8)+;
      PADR(SUBSTR(EVALUATE(lcMatReq+'.ITEM'),1,lnFMajLen),19)+;
      PADR(SUBSTR(EVALUATE(lcMatReq+'.ITEM'), lnFClrSrt, lnFClrEnd),6))

    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]

    REPLACE Fabric     WITH &lcMatReq..ITEM,;
      Vendor     WITH lcFabVen ,;
      LeadTime   WITH VENDMATL.LEADTIME , ;
      COMPLETE   WITH IIF(llRpTimeP, ldComplete, oAriaApplication.SystemDate + LeadTime),;
      Available  WITH IIF(llRpTimeP, ldAvailable, oAriaApplication.SystemDate + LeadTime),;
      UOMBuy     WITH lcUOMBuy,;
      nFabTotQty WITH ROUND((lnRequired/lnConv),3),;
      nFabcost   WITH VENDMATL.NFABCOST,;
      DDELIVDATE WITH COMPLETE,;
      CVENFAB    WITH VENDMATL.CVENFAB,;
      CVENCOLR   WITH VENDMATL.CVENCOLR,;
      cCatGTyp   WITH EVALUATE(lcMatReq + '.cCatGTyp')
  ELSE
    REPLACE Fabric     WITH &lcMatReq..ITEM,;
      Vendor     WITH lcFabVen ,;
      LeadTime   WITH lnLeadTm ,;
      COMPLETE   WITH IIF(llRpTimeP, ldComplete, oAriaApplication.SystemDate + LeadTime),;
      Available  WITH IIF(llRpTimeP, ldAvailable, oAriaApplication.SystemDate + LeadTime),;
      UOMBuy     WITH lcUOMBuy,;
      nFabTotQty WITH ROUND((lnRequired/lnConv),3),;
      nFabcost   WITH lnFabcost,;
      DDELIVDATE WITH COMPLETE,;
      cCatGTyp   WITH EVALUATE(lcMatReq + '.cCatGTyp')
  ENDIF

  IF EVALUATE(lcMatReq + '.cCatGTyp') = 'S'
    REPLACE nQty1 WITH ABS(laRemain[1]),;
      nQty2 WITH ABS(laRemain[2]),;
      nQty3 WITH ABS(laRemain[3]),;
      nQty4 WITH ABS(laRemain[4]),;
      nQty5 WITH ABS(laRemain[5]),;
      nQty6 WITH ABS(laRemain[6]),;
      nQty7 WITH ABS(laRemain[7]),;
      nQty8 WITH ABS(laRemain[8])
  ELSE
    REPLACE nQty1 WITH nFabTotQty
  ENDIF
ELSE
  REPLACE nFabTotQty WITH nFabTotQty + ROUND((lnRequired / IIF(cCatGTyp = 'S', 1, lnConv)), 3)

  IF EVALUATE(lcMatReq + '.cCatGTyp') = 'S'
    REPLACE nQty1 WITH nQty1 + ABS(laRemain[1]),;
      nQty2 WITH nQty2 + ABS(laRemain[2]),;
      nQty3 WITH nQty3 + ABS(laRemain[3]),;
      nQty4 WITH nQty4 + ABS(laRemain[4]),;
      nQty5 WITH nQty5 + ABS(laRemain[5]),;
      nQty6 WITH nQty6 + ABS(laRemain[6]),;
      nQty7 WITH nQty7 + ABS(laRemain[7]),;
      nQty8 WITH nQty8 + ABS(laRemain[8])
  ELSE
    REPLACE nQty1 WITH nFabTotQty
  ENDIF
ENDIF

SELECT (lcMatReq)
RETURN

*!*************************************************************
*! Name      : lfGenMPo
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Generate Material P/O.
*!  GENERATE PURCHASE ORDER(S) FOR MATERIAL(S) FOR WHICH THERE IS
*!                  NEGATIVE REMAINING QUANTITY IF ANY.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
FUNCTION lfGenMPo

IF !loApVendor.GoTop()
  *--The vendor file is empty, you cannot create PO's .
  =gfModalGen('TRM36128B36000','DIALOG')
  RETURN .F.
ENDIF

*--Read the multi currency and multi warehouse setup.
*--Rights for edit exch. rates.
llEditExRt = gfGetMemVar('llEditExRa')
llMulCurr  = gfGetMemVar('llMulCurr')
llWareHous = (gfGetMemVar('M_WareHouse')='Y')

*-- Save the company setup ( Enter Ma Po No Manual ) into llGenMApon
*---llGenMaPoN = .T.  -----> generated Po no
*---llGenMaPoN = .F.  -----> manuall Po no
llGENMAPON = gfGetMemVar('M_GENMAPON') = 'N'

*WSH
llGENSTPON = gfGetMemVar('M_GENSTORN') = 'N'

=loCodes.SEEK('DSHIPVIA', "CCODE_NO")
lcShipVia = EVALUATE(loCodes.lcCursorView + '.cCode_No')
*WSH

loOgScroll.llOGFltCh = .T.

loWareHous.GoTop()

SELECT (lcTmpRmain)
SCAN
  lcFbCVen = Vendor

  IF cCatGTyp = 'S'
    lcFabKey = '0001' + Fabric

    WAIT WINDOW 'Collecting data for Style : ' + Fabric NOWAIT

    =loStyle.SEEK(Fabric)
    lnFabCost = STYLE.nICost1
  ELSE
    lcFabKey = '0002' + Fabric

    WAIT WINDOW 'Collecting data for Fabric : ' + Fabric NOWAIT

    =loFabric.SEEK(lcFabKey)

    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
    *lnFabCost = IIF(llVenRef .AND. loVENDMATL.SEEK(lcFbCVen + SUBSTR(Fabric, 1, 7) + SUBSTR(Fabric, lnFClrSrt, lnClrEnd)),;
    NFABCOST, FABRIC.nICost1)
    lnFabCost = IIF(llVenRef .AND. loVENDMATL.SEEK(PADR(lcFbCVen,8) + PADR(SUBSTR(Fabric, 1, lnFMajLen),19) +;
      PADR(SUBSTR(Fabric, lnFClrSrt, lnClrEnd),6)),;
      NFABCOST, FABRIC.nICost1)
    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]
  ENDIF

  =RLOCK()

  *-- Consider case of use vendor referance
  IF cCatGTyp <> 'S'
    REPLACE cFabGrade  WITH FABRIC.CStyGrade,;
      cWareCode  WITH IIF(!llWareHous,WAREHOUS.cWareCode,''),;
      cPriceCur  WITH IIF(EMPTY(FABRIC.cPriceCur),oAriaApplication.BaseCurrency,FABRIC.cPriceCur),;
      cDutyCur   WITH IIF(EMPTY(FABRIC.cDutyCur) ,oAriaApplication.BaseCurrency,FABRIC.cDutyCur),;
      nFabcost   WITH lnFabCost,;
      nItem_Tax  WITH FABRIC.nICost2,;
      nItm_Frt   WITH FABRIC.nICost3,;
      nItemQuota WITH FABRIC.nICost4
  ELSE
    REPLACE cFabGrade  WITH STYLE.CStyGrade,;
      cWareCode  WITH IIF(!llWareHous,WAREHOUS.cWareCode,''),;
      cPriceCur  WITH IIF(EMPTY(STYLE.cPriceCur),oAriaApplication.BaseCurrency,STYLE.cPriceCur),;
      cDutyCur   WITH IIF(EMPTY(STYLE.cDutyCur) ,oAriaApplication.BaseCurrency,STYLE.cDutyCur),;
      nFabcost   WITH lnFabCost,;
      nItem_Tax  WITH STYLE.nICost2,;
      nItm_Frt   WITH STYLE.nICost3,;
      nItemQuota WITH STYLE.nICost4,;
      nFCost5    WITH STYLE.nICost5,;
      nFCost6    WITH STYLE.nICost6,;
      nFCost7    WITH STYLE.nICost7
  ENDIF

  UNLOCK

  *--Search For Last Price This Material Was Purchased At From This Vendor,
  *--If it Was Never Purchased Before, Take the Price From Fabric file.
  SELECT POFLN
  lcMPO = SPACE(6)

  *!*	  =loPOFLN.SEEK(lcFabKey+'P')
  *!*
  *!*	  *-- get the correct vendor
  *!*	  SCAN WHILE cInvType+Style+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = lcFabKey + 'P';
  *!*	         FOR cStyType $ 'PM' AND Trancd = '1' AND Vendor = PADR(lcFbCVen,8)
  *!*	    lcMPO   = PO
  *!*	    lnCost1 = nFCost1
  *!*	    lnCost2 = nFCost2
  *!*	    lnCost3 = nFCost3
  *!*	    lnCost4 = nFCost4
  *!*	    lnCost5 = nFCost5
  *!*	    lnCost6 = nFCost6
  *!*	    lnCost7 = nFCost7
  *!*	  ENDSCAN

  SELECT (lcTmpRmain)
  IF !EMPTY(lcMPO)
    =loPOFHDR.SEEK('P' + IIF(cCatGTyp = 'S', 'P', 'M') + lcMPO)

    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
    *lnFabCost = IIF(cCatGTyp <> 'S' .AND. llVenRef .AND. loVENDMATL.SEEK(lcFbCVen + SUBSTR(Fabric, 1 , 7) + SUBSTR(Fabric, lnFClrSrt, lnFClrEnd)),;
    NFABCOST, lnCost1)
    lnFabCost = IIF(cCatGTyp <> 'S' .AND. llVenRef .AND. loVENDMATL.SEEK(PADR(lcFbCVen,8) +;
      PADR(SUBSTR(Fabric, 1 , lnFMajLen),19) + PADR(SUBSTR(Fabric, lnFClrSrt, lnFClrEnd),6)),;
      NFABCOST, lnCost1)
    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]
    =RLOCK()

    *-- get the vendor from the POFHdr File instead of the POFln.
    *-- Consider case of use vendor referance.
    REPLACE Vendor     WITH POFHEADR.Vendor,;
      cPriceCur  WITH POFHEADR.cPriceCur,;
      cDutyCur   WITH POFHEADR.cDutyCur,;
      nFabcost   WITH lnFabCost,;
      nItem_Tax  WITH lnCost2,;
      nItm_Frt   WITH lnCost3,;
      nItemQuota WITH lnCost4,;
      nfCost5    WITH lnCost5,;
      nfCost6    WITH lnCost6,;
      nfCost7    WITH lnCost7

    UNLOCK
  ELSE
    IF llMulCurr AND !EMPTY(lcFbCVen) AND ;
        loAPVENDOR.SEEK(lcFbCVen) AND !EMPTY(APVENDOR.cCurrCode)
      =RLOCK()
      REPLACE cPriceCur WITH IIF(!EMPTY(APVENDOR.cCurrCode),APVENDOR.cCurrCode,cPriceCur)
      UNLOCK
    ENDIF
  ENDIF

  *--Update the Currency Rate and units.
  STORE 1 TO lnCurrUnt1, lnCurrUnt2
  STORE 1 TO lnPRate, lnDRate
  IF llMulCurr
    lnPRate = gfChkRate('lnCurrUnt1',cPriceCur,oAriaApplication.SystemDate,llEditExRt,oAriaApplication.ActiveCompanyID,.F.)
    lnDRate = gfChkRate('lnCurrUnt2',cDutyCur ,oAriaApplication.SystemDate,llEditExRt,oAriaApplication.ActiveCompanyID,.F.)
  ENDIF
  =RLOCK()
  REPLACE nPriceRat  WITH IIF(lnPRate=0,1,lnPRate),;
    nCurrUnit  WITH lnCurrUnt1,;
    nDutyRat   WITH IIF(lnDRate=0,1,lnDRate),;
    nDCurUnit  WITH lnCurrUnt2
  UNLOCK

ENDSCAN

*--Initialize the currency method and symbol.
STORE '' TO lcPMethod,lcPUnMeth
STORE SET('CURRENCY',1) TO lcPBseSmbl

lcBrFields = "lnType=IIF(cCatGTyp = 'S', 'Style Component', IIF(cCatGTyp = 'F', 'Fabric', 'Trim'))  :R :H='Item Type',"+;
  "Fabric     :R :H='Item',"+;
  "Vendor     :R :H='Vendor',"+;
  "LeadTime   :R :H='LdTime' ,"+;
  IIF(llVenRef,"dDelivDate :R :H='Delivry' ,","Complete   :R :H='Complete' ,")+;
  "nFabcost   :R :H='Price' ,"+;
  "lnBsPrc=lfGetEquv(cPriceCur,nFabcost,nPriceRat,nCurrUnit) :R :H=lcPBseSmbl+' Price'  :P='999999.99',"+;
  "nQty1 :R :H='Qty. 1 Needed',"+;
  "nQty2 :R :H='Qty. 2 Needed',"+;
  "nQty3 :R :H='Qty. 3 Needed',"+;
  "nQty4 :R :H='Qty. 4 Needed',"+;
  "nQty5 :R :H='Qty. 5 Needed',"+;
  "nQty6 :R :H='Qty. 6 Needed',"+;
  "nQty7 :R :H='Qty. 7 Needed',"+;
  "nQty8 :R :H='Qty. 8 Needed',"+;
  "nFabTotQty :R :H='Tot. Qty. Needed',"+;
  "UOMBUY     :R :H='UOMBuy'    ,"+;
  "cPriceCur  :R :H='PriceCur.' ,"+;
  "nPriceRat  :R :H='PriceRate' ,"+;
  "cDutyCur   :R :H='Duty Cur.' ,"+;
  "nDutyRat   :R :H='Duty Rate' "

IF llVenRef
  lcBrFields = lcBrFields + ",cVenFab :R :H='Supp. Item' :10,cVenColr :R :H='Supp. Color' :10"
ENDIF

SELECT (lcTmpRmain)
GO TOP
DO WHILE .T.
  lcCentry= SET('CENTURY')
  SET CENTURY ON

  =ARIABROW('','Negative Remaining Materials and Style Components',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'=lfvEdtMat()',;
    'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok' )

  *-- Restore the saved century setting .
  SET CENTURY  &lcCentry

  *--Check for any valid record (nothing selected).
  *-- Allow Price field to be empty.[Begin]
  LOCATE FOR !EMPTY(Vendor) AND nFabTotQty <> 0
  IF !FOUND()
    *--No vendor,price or quantity needed has been defined for these materials.
    *--Do you wish to Assign?   <Assign><Cancel>
    IF gfModalGen('INM36129B36004','DIALOG') = 1
      LOOP
    ELSE
      RETURN .F.
    ENDIF
  ENDIF

  LOCATE FOR EMPTY(Vendor)
  IF FOUND() AND gfModalGen('QRM36122B36001','DIALOG') = 2
    *---Some materials has no vendor,therefor no purchase orders will be generated for this Materials! YES/NO
    LOOP
  ENDIF

  *--Generate Same P/O for this Key :
  *--KEY VENDOR+CFABGRADE+COMPLETE+CPRICECUR+CDUTYCUR+NPRICERAT+NDUTYRAT

  *--Note that the materials with same vendor,quality,currency and completion date will be joinded in same P/O.
  *-- "<Generate> <Modify> <Cancel>"
  lnSeletn = gfModalGen('QRM36123B36009','DIALOG')
  DO CASE
  CASE lnSeletn = 1
    EXIT
  CASE lnSeletn = 3
    RETURN .F.
  ENDCASE
ENDDO

*-- Let the complete date be the max of delivry dates
*-- for same vendor in case of use vendor referance.
IF !llRPTimeP AND llVenRef
  SELECT VENDOR, MAX(DDELIVDATE) AS DCOMPDATE;
    FROM (lcTmpRmain);
    GROUP BY VENDOR;
    INTO CURSOR TMPVENDCOM

  SELECT TMPVENDCOM
  SCAN
    SELECT (lcTmpRmain)
    REPLACE ALL COMPLETE WITH TMPVENDCOM.DCOMPDATE FOR VENDOR = TMPVENDCOM.VENDOR
  ENDSCAN

  USE IN TMPVENDCOM
  SELECT (lcTmpRmain)
ENDIF

*--Open temp P/O Heder.
lcPOTmpHD = loOGScroll.gfTempName()

=lfCrtTmp('P')

*--Open temp P/O Lines.
lcPOTmpLN = loOGScroll.gfTempName()
SELECT POFLN
COPY STRUCTURE TO (oAriaApplication.WorkDir + lcPOTmpLN)

= gfOpenFile(oAriaApplication.WorkDir + lcPOTmpLN, '', 'EX')
INDEX ON PO+STR(LINENO,6) TAG (lcPOTmpLN)

*-- Open codes file.
IF TYPE('loCodes') <> 'O'
  loCodes = CREATEOBJECT('RemoteTable','CODES','CODES','MACODES',SET("Datasession"))
  llOpenCodes = .T.
ENDIF

*WSH
loBOMHEADR = CREATEOBJECT('RemoteTable', 'BOMHEADR', 'BOMHEADR', 'BOMHEADR', SET("Datasession"))
*WSH

*--Read Default.
STORE '' TO lcDEFDiv, lcDEFTerms, lcDEFShpVa

IF TYPE("loCodes") # 'O'
  loCodes    = CREATEOBJECT('RemoteTable','CODES','CCODE_NO','MACODES',SET("Datasession"))
ENDIF

lcOldTag = loCodes.lcTagName
=loCodes.SetOrder('CCODE_NO')

IF loCodes.SEEK('D' + PADR('CTERMCODE', 10))
  lcDEFTerms = MACODES.cCode_No
ENDIF

IF loCodes.SEEK('D' + PADR('CDIVISION', 10))
  lcDEFDiv = MACODES.cCode_No
ENDIF

IF loCodes.SEEK('D' + PADR('SHIPVIA', 10))
  lcDEFShpVa = MACODES.cCode_No
ENDIF

=loCodes.SetOrder(IIF(EMPTY(lcOldTag), '', lcOldTag))

*-- Allow Price field to be empty.
SELECT (lcTmpRmain)
SCAN FOR !EMPTY(Vendor) AND nFabTotQty <> 0

  *B609665,1 WAM 08/23/2011 Create one PO for each vendor\complete date currency. Use the Proper key expression
  *lcPOKey = cCatGTyp+Vendor+cFabGrade+DTOC(Complete)+cPriceCur+cDutyCur+STR(nPriceRat,9,4)+STR(nDutyRat,9,4)
  lcPOKey = Vendor+cFabGrade+DTOC(COMPLETE)+cPriceCur+cDutyCur+STR(nPriceRat,9,4)+STR(nDutyRat,9,4)
  *B609665,1 WAM 08/23/2011 (End)


  =loWAREHOUS.SEEK(cWareCode)
  =loAPVENDOR.SEEK(Vendor)

  SELECT (lcPOTmpHD)
  IF SEEK(lcPOKey)
    lnQtyToAcc =  &lcTmpRmain..nFabTotQty

    REPLACE LASTLINE    WITH LASTLINE + 1,;
      nStyOrder   WITH nStyOrder + lnQtyToAcc,;
      OPEN        WITH OPEN + lnQtyToAcc,;
      NFCOST1     WITH NFCOST1 + (lnQtyToAcc * &lcTmpRmain..nFabcost),;
      NFCOST2     WITH NFCOST2 + (lnQtyToAcc * &lcTmpRmain..nItem_Tax),;
      NFCOST3     WITH NFCOST3 + (lnQtyToAcc * &lcTmpRmain..nItm_Frt),;
      NFCOST4     WITH NFCOST4 + (lnQtyToAcc * &lcTmpRmain..nItemQuota),;
      NFCOST5     WITH NFCOST5 + (lnQtyToAcc * &lcTmpRmain..nFCost5),;
      NFCOST6     WITH NFCOST6 + (lnQtyToAcc * &lcTmpRmain..nFCost6),;
      NFCOST7     WITH NFCOST7 + (lnQtyToAcc * &lcTmpRmain..nFCost7),;
      NICOST1     WITH NICOST1 + (nStyOrder * lfGetEquv(&lcTmpRmain..cPriceCur,&lcTmpRmain..nFabcost,&lcTmpRmain..nPriceRat,&lcTmpRmain..nCurrUnit)),;
      NICOST2     WITH NICOST2 + (nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItem_Tax,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
      NICOST3     WITH NICOST3 + (nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItm_Frt,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
      NICOST4     WITH NICOST4 + (nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItemQuota,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
      NICOST5     WITH NICOST5 + (nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost5,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
      NICOST6     WITH NICOST6 + (nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost6,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
      NICOST7     WITH NICOST7 + (nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost7,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
      POTOTAL     WITH NICOST1+NICOST2+NICOST3+NICOST4+NICOST5+NICOST6+NICOST7
  ELSE
    *-- in case manual ma po no call local function lfsequence()
    *B608122,1 TMI [Start] Call the gfSequence global function with passing the new parameters lcTranType,lcTable,lcTag ( T20070524.0013)
    *IF EVALUATE(lcTmpRmain + '.cCatGTyp') = 'S'
    *  lcMatPo = IIF(llGENSTPON, gfSequence('PO'), lfSequence())
    *ELSE
    *  lcMatPo = IIF(llGENMAPON, gfSequence('POMAT'), lfSequence())
    *ENDIF
    LOCAL lcDiv
    lcDiv = IIF(EMPTY(APVENDOR.CDIVISION),lcDEFDiv,APVENDOR.CDIVISION)
    IF EVALUATE(lcTmpRmain + '.cCatGTyp') = 'S'
      lcMatPo = IIF(llGENSTPON, gfSequence('PO','','',lcDiv,'','PP','POSHDR','POSHDR'), lfSequence())
    ELSE
      lcMatPo = IIF(llGENMAPON, gfSequence('POMAT','','',lcDiv,'','PM','POSHDR','POSHDR'), lfSequence())
    ENDIF
    *B608122,1 TMI [End  ]

    APPEND BLANK
    REPLACE PO          WITH lcMatPo,;
      CBUSDOCU    WITH 'P',;
      CSTYTYPE    WITH IIF(&lcTmpRmain..cCatGTyp = 'S', 'P', 'M'),;
      STATUS      WITH IIF(EVALUATE(lcTmpRmain + '.cCatGTyp') = 'S', 'H', lcRpGenSt),;  && WSH
    VENDOR      WITH &lcTmpRmain..Vendor,;
      ENTERED     WITH oAriaApplication.SystemDate,;
      COMPLETE    WITH &lcTmpRmain..COMPLETE ,;
      Available   WITH &lcTmpRmain..Available ,;
      CPRICECUR   WITH &lcTmpRmain..cPriceCur,;
      CDUTYCUR    WITH &lcTmpRmain..cDutyCur ,;
      NPRICERAT   WITH &lcTmpRmain..nPriceRat,;
      CSTYGRADE   WITH &lcTmpRmain..cFabGrade,;
      NDUTYRAT    WITH &lcTmpRmain..nDutyRat ,;
      LASTLINE    WITH 1,;
      CONTACT     WITH APVENDOR.CVENCONT,;
      PHONE       WITH APVENDOR.CPHONENO,;
      CDIVISION   WITH IIF(EMPTY(APVENDOR.CDIVISION),lcDEFDiv,APVENDOR.CDIVISION),;
      CTERMCODE   WITH IIF(EMPTY(APVENDOR.CTERMCODE),lcDEFTerms,APVENDOR.CTERMCODE),;
      SHIPVIA     WITH lcDEFShpVa,;
      cVendName   WITH APVENDOR.cVenComp
    REPLACE nStyOrder   WITH &lcTmpRmain..nFabTotQty,;
      OPEN        WITH nStyOrder ,;
      NFCOST1     WITH nStyOrder * &lcTmpRmain..nFabcost  ,;
      NFCOST2     WITH nStyOrder * &lcTmpRmain..nItem_Tax ,;
      NFCOST3     WITH nStyOrder * &lcTmpRmain..nItm_Frt  ,;
      NFCOST4     WITH nStyOrder * &lcTmpRmain..nItemQuota,;
      NFCOST5     WITH nStyOrder * &lcTmpRmain..nFCost5,;
      NFCOST6     WITH nStyOrder * &lcTmpRmain..nFCost6,;
      NFCOST7     WITH nStyOrder * &lcTmpRmain..nFCost7,;
      NICOST1     WITH nStyOrder * lfGetEquv(&lcTmpRmain..cPriceCur,&lcTmpRmain..nFabcost,&lcTmpRmain..nPriceRat,&lcTmpRmain..nCurrUnit),;
      NICOST2     WITH nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItem_Tax,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
      NICOST3     WITH nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItm_Frt,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
      NICOST4     WITH nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItemQuota,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
      NICOST5     WITH nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost5,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
      NICOST6     WITH nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost6,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
      NICOST7     WITH nStyOrder * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost7,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
      POTOTAL     WITH NICOST1+NICOST2+NICOST3+NICOST4+NICOST5+NICOST6+NICOST7
    IF Entered > COMPLETE
      REPLACE Entered WITH COMPLETE
    ENDIF

    *-- Address1 field in POFHDR should be Address1 of the Warehouse not the description [Begin]
    REPLACE CWARECODE   WITH &lcTmpRmain..cWareCode,;
      COUTADDR1   WITH WAREHOUSE.CADDRESS1,;
      COUTADDR2   WITH WAREHOUSE.CADDRESS2,;
      COUTADDR3   WITH WAREHOUSE.CADDRESS3,;
      COUTADDR4   WITH WAREHOUSE.CADDRESS4,;
      COUTADDR5   WITH WAREHOUSE.CADDRESS5,;
      LINK_CODE   WITH 'DEFDEF',;
      CADD_USER   WITH oAriaApplication.User_ID,;
      DADD_DATE   WITH oAriaApplication.SystemDate,;
      CADD_TIME   WITH TIME()

    *-- Save the MRP# of the report in the temp MPO header file.
    IF llSaveMrp
      REPLACE CMRP WITH lcMrp
    ENDIF

    *WSH
    REPLACE ShipVia   WITH lcShipVia,;
      nCurrUnit WITH 1,;
      nDCurUnit WITH 1
    *WSH

  ENDIF

  *--Update Material P/O lines.
  *-- We have to make sure that we have the correct data in fabric File
  SELECT (lcPOTmpLN)
  APPEND BLANK

  IF &lcTmpRmain..cCatGTyp = 'S'
    =loStyle.SEEK(&lcTmpRmain..Fabric)

    *WSH
    =loBOMHEADR.SEEK('0001' + STYLE.cStyMajor)
    SELECT BOMHEADR
    LOCATE REST WHILE cInvType + cItmMajor + cCstShtTyp + cCstSht_Id = '0001' + STYLE.cStyMajor FOR lDefCstSht
    SELECT (lcPOTmpLN)
    *WSH

    REPLACE CBUSDOCU    WITH 'P',;
      CSTYTYPE    WITH 'P',;
      PO          WITH &lcPOTmpHD..PO,;
      TRANCD      WITH '1',;
      LINENO      WITH &lcPOTmpHD..LASTLINE,;
      VENDOR      WITH &lcPOTmpHD..VENDOR,;
      CINVTYPE    WITH '0001',;
      STYLE       WITH &lcTmpRmain..Fabric,;
      PATTERN     WITH STYLE.PATTERN,;
      REFERENCE   WITH 'Refer To Style Component Requirements',;
      QTY1        WITH &lcTmpRmain..nQty1,;
      QTY2        WITH &lcTmpRmain..nQty2,;
      QTY3        WITH &lcTmpRmain..nQty3,;
      QTY4        WITH &lcTmpRmain..nQty4,;
      QTY5        WITH &lcTmpRmain..nQty5,;
      QTY6        WITH &lcTmpRmain..nQty6,;
      QTY7        WITH &lcTmpRmain..nQty7,;
      QTY8        WITH &lcTmpRmain..nQty8,;
      TOTQTY      WITH &lcTmpRmain..nFabTotQty,;
      CWARECODE   WITH &lcPOTmpHD..cWareCode

    *WSH
    REPLACE SCALE      WITH STYLE.SCALE,;
      Gros_Price WITH STYLE.Gros_Price,;
      cCstSht_Id WITH BOMHEADR.cCstSht_Id
    *WSH

  ELSE
    =loFABRIC.SEEK('0002'+&lcTmpRmain..Fabric)

    *WSH
    =loBOMHEADR.SEEK('0002' + STYLE.cStyMajor)
    SELECT BOMHEADR
    LOCATE REST WHILE cInvType + cItmMajor + cCstShtTyp + cCstSht_Id = '0001' + Fabric.cStyMajor FOR lDefCstSht
    SELECT (lcPOTmpLN)
    *WSH

    REPLACE CBUSDOCU    WITH 'P',;
      CSTYTYPE    WITH 'M',;
      PO          WITH &lcPOTmpHD..PO,;
      TRANCD      WITH '1',;
      LINENO      WITH &lcPOTmpHD..LASTLINE,;
      VENDOR      WITH &lcPOTmpHD..VENDOR,;
      CINVTYPE    WITH '0002',;
      STYLE       WITH &lcTmpRmain..Fabric,;
      PATTERN     WITH FABRIC.PATTERN,;
      REFERENCE   WITH 'Refer To Material Requirements',;
      WIDTH       WITH FABRIC.cItemFld1,;
      QTY1        WITH &lcTmpRmain..nFabTotQty,;
      TOTQTY      WITH &lcTmpRmain..nFabTotQty,;
      CWARECODE   WITH &lcPOTmpHD..cWareCode

    *WSH
    REPLACE SCALE      WITH Fabric.SCALE,;
      Gros_Price WITH Fabric.Gros_Price,;
      cCstSht_Id WITH BOMHEADR.cCstSht_Id
    *WSH

    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
    IF llVenRef
      REPLACE CVENFAB    WITH  &lcTmpRmain..CVENFAB,;
        CVENCOLR   WITH  &lcTmpRmain..CVENCOLR

    ENDIF
    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]

  ENDIF

  REPLACE NFCOST1     WITH &lcTmpRmain..nFabcost ,;
    NFCOST2     WITH &lcTmpRmain..nItem_Tax,;
    NFCOST3     WITH &lcTmpRmain..nItm_Frt ,;
    NFCOST4     WITH &lcTmpRmain..nItemQuota,;
    NFCOST5     WITH &lcTmpRmain..nFCost5,;
    NFCOST6     WITH &lcTmpRmain..nFCost6,;
    NFCOST7     WITH &lcTmpRmain..nFCost7,;
    NICOST1     WITH lfGetEquv(&lcTmpRmain..cPriceCur,&lcTmpRmain..nFabcost,&lcTmpRmain..nPriceRat,&lcTmpRmain..nCurrUnit),;
    NICOST2     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItem_Tax,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
    NICOST3     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItm_Frt,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
    NICOST4     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItemQuota,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
    NICOST5     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost5,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
    NICOST6     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost6,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
    NICOST7     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nFCost7,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
    COMPLETE    WITH EVALUATE(lcTmpRmain+'.dDelivDate'),;
    CSTYGRADE   WITH &lcTmpRmain..cFabGrade,;
    CADD_USER   WITH oAriaApplication.User_ID,;
    DADD_DATE   WITH oAriaApplication.SystemDate,;
    CADD_TIME   WITH TIME()

  *WSH
  REPLACE ShipVia WITH lcShipVia
  *WSH

ENDSCAN

*--Edit warehous and some additional P/O information.
lcBrTtl   = 'Generated P/Os'

SELECT (lcPOTmpHD)
GO TOP

DO FORM (gcRepHome + "MA\MaPOMR")

*-- in case of manual ma po no replace the temp poline with MA PO NO save it i nte temp headre

IF !llGENMAPON
  GO TOP
  SCAN FOR cStyType = 'M'
    SELECT (lcPOTmpLN)
    REPLACE ALL PO WITH &lcPOTmpHD..cTmpPoMat FOR PO = &lcPOTmpHD..PO

    SELECT (lcPOTmpHD)
    REPLACE PO WITH cTmpPoMat
  ENDSCAN
ENDIF

IF !llGENSTPON
  GO TOP
  SCAN FOR cStyType = 'P'
    SELECT (lcPOTmpLN)
    REPLACE ALL PO WITH &lcPOTmpHD..cTmpPoMat FOR PO = &lcPOTmpHD..PO

    SELECT (lcPOTmpHD)
    REPLACE PO WITH cTmpPoMat
  ENDSCAN
ENDIF

LOCAL lcTranCode

*-- Begin Updating Transaction
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '')

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  RETURN .F.
ENDIF

=loSTYDYE.SetOrder("StyDye")

*--Start update the master files.
SELECT (lcPOTmpLN)
SCAN

  *-1) Update FABRIC and FABDYE file.-------------------------------
  lnOnOrder = 0
  IF cStyType = 'M'
    IF loFABRIC.SEEK('0002'+STYLE)
      lnConv    = IIF(loUOM.SEEK(FABRIC.CCONVBUY), UOM.nConF, 1)
      lnOnOrder = &lcPOTmpLN..TotQty * lnConv

      REPLACE cUOMCode WITH FABRIC.CCONVBUY
    ENDIF
  ELSE
    IF loStyle.SEEK(STYLE)
      lnConv    = 1
      lnOnOrder = &lcPOTmpLN..TotQty * lnConv
      lcUOMBuy  = ''
      =gfGetUOMData(@lcUOMBuy, '', '', 1, .F.)
      REPLACE cUOMCode WITH lcUOMBuy
    ENDIF
  ENDIF

  SELECT (lcPOTmpHD)
  LOCATE FOR PO = &lcPOTmpLN..PO

  lcHdrLoc = cWareCode

  *WSH
  REPLACE &lcPOTmpLN..cWareCode WITH lcHdrLoc
  *WSH

  IF &lcPOTmpLN..cStyType = 'P'
    loStyle.REPLACE("Wip1 WITH WIP1 + " + STR(&lcPOTmpLN..Qty1) + "," +;
      "Wip2 WITH WIP2 + " + STR(&lcPOTmpLN..Qty2) + "," +;
      "Wip3 WITH WIP3 + " + STR(&lcPOTmpLN..Qty3) + "," +;
      "Wip4 WITH WIP4 + " + STR(&lcPOTmpLN..Qty4) + "," +;
      "Wip5 WITH WIP5 + " + STR(&lcPOTmpLN..Qty5) + "," +;
      "Wip6 WITH WIP6 + " + STR(&lcPOTmpLN..Qty6) + "," +;
      "Wip7 WITH WIP7 + " + STR(&lcPOTmpLN..Qty7) + "," +;
      "Wip8 WITH WIP8 + " + STR(&lcPOTmpLN..Qty8) + "," +;
      "TotWip WITH TOTWIP + " + STR(lnOnOrder))
    SELECT STYDYE
    IF !EMPTY(lcHdrLoc)
      IF !loSTYDYE.SEEK(&lcPOTmpLN..STYLE + lcHdrLoc)
        loSTYDYE.APPEND()
        SELECT (loSTYDYE.lcCursorUpdate)
        REPLACE STYLE      WITH &lcPOTmpLN..STYLE,;
          cWareCode  WITH lcHdrLoc,;
          Ave_Cost   WITH STYLE.ave_cost
      ENDIF
      loStyDye.REPLACE("Wip1 WITH WIP1 + " + STR(&lcPOTmpLN..Qty1) + "," +;
        "Wip2 WITH WIP2 + " + STR(&lcPOTmpLN..Qty2) + "," +;
        "Wip3 WITH WIP3 + " + STR(&lcPOTmpLN..Qty3) + "," +;
        "Wip4 WITH WIP4 + " + STR(&lcPOTmpLN..Qty4) + "," +;
        "Wip5 WITH WIP5 + " + STR(&lcPOTmpLN..Qty5) + "," +;
        "Wip6 WITH WIP6 + " + STR(&lcPOTmpLN..Qty6) + "," +;
        "Wip7 WITH WIP7 + " + STR(&lcPOTmpLN..Qty7) + "," +;
        "Wip8 WITH WIP8 + " + STR(&lcPOTmpLN..Qty8) + "," +;
        "TotWip WITH TOTWIP + " + STR(lnOnOrder))
    ENDIF
  ELSE
    SELECT FABDYE
    IF !EMPTY(lcHdrLoc)
      IF !loFABDYE.SEEK(&lcPOTmpLN..cInvType + &lcPOTmpLN..STYLE + lcHdrLoc)
        loFABDYE.APPEND()
        SELECT (loFABDYE.lcCursorUpdate)
        REPLACE cInvType   WITH &lcPOTmpLN..cInvType,;
          STYLE      WITH &lcPOTmpLN..STYLE,;
          cWareCode  WITH lcHdrLoc,;
          Ave_Cost   WITH FABRIC.ave_cost

        *loFABDYE.REPLACE()
      ENDIF
      loFABDYE.REPLACE("Wip1 WITH WIP1 + " + STR(lnOnOrder) + ", TotWip WITH TOTWIP + " + STR(lnOnOrder))
    ENDIF
  ENDIF

  *-2) Update APVENDOR file.---------------------------------------
  SELECT APVENDOR
  IF loAPVENDOR.SEEK(&lcPOTmpLN..Vendor)
    loAPVENDOR.REPLACE("nVenOpnPO WITH nVenOpnPO + " + lcPOTmpLN + ".nICost1")
  ENDIF
ENDSCAN


*!*	LOCAL lcTranCode

*!*	*-- Begin Updating Transaction
*!*	lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '')

*!*	*-- Check Resule for Begin Transaction
*!*	IF TYPE('lcTranCode') = 'N'
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
*!*	  RETURN .F.
*!*	ENDIF


*-3) Update POFLN file.---------------------------------------------
loStyle.TABLEUPDATE(lcTranCode)
loStyDye.TABLEUPDATE(lcTranCode)
loFABDYE.TABLEUPDATE(lcTranCode)
loAPVENDOR.TABLEUPDATE(lcTranCode)

SELECT (loPOFLN.lcCursorUpdate)

APPEND FROM (oAriaApplication.WorkDir+lcPOTmpLN)

loPOFLN.TABLEUPDATE(lcTranCode)

*-4) Update POFHDR file.--------------------------------------------
SELECT (loPOFHDR.lcCursorUpdate)

APPEND FROM (oAriaApplication.WorkDir+lcPOTmpHD)

loPOFHDR.TABLEUPDATE(lcTranCode)

lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnHandler <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
  RETURN .F.
ENDIF

*!*  *--Would you like to preview/print the purchase orders just created?
*!*  *--<Preview>  <Print>  <None>.
*!*  lnSelDev = gfModalGen('QRM36125B36010','DIALOG')
*!*  IF lnSelDev <> 3
*!*
*!*    *WSH
*!*    *IF lnSelDev = 1
*!*    *  gcDevice = 'SCREEN'
*!*    *ELSE
*!*    *  IF !pSetup(.T., .F.)
*!*    *    RETURN
*!*    *  ENDIF
*!*    *ENDIF
*!*    *WSH
*!*
*!*    *--Set the print po report tag.
*!*
*!*    *WSH
*!*    *SET ORDER TO TAG POFLN IN POFLN
*!*    *WSH
*!*
*!*    SELECT (lcPOTmpHD)
*!*
*!*    *WSH
*!*    *INDEX ON POMat TAG (lcPOTmpHD)
*!*    INDEX ON PO TAG (lcPOTmpHD)
*!*    *WSH
*!*
*!*    *--Run Print Material P/O form program...
*!*    lcRpRName   = 'MAMATPA'
*!*    lcSavTmpFrm = lcOGTmpForm
*!*    lcOGTmpForm = ' '
*!*
*!*    *-- Open Company File.
*!*
*!*    *WSH
*!*    *IF !USED('SYCCOMP')
*!*    *  USE (oAriaApplication.SysPath+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
*!*    *ENDIF
*!*    lcFormName    = 'MAMATPA'
*!*    lcPRGName     = 'MAMATP'
*!*    lcRPForm      = 'MAMATPA'
*!*    lcTypeCond    = '.T.'
*!*    lcPOCondition = '.T.'
*!*    *WSH
*!*
*!*    DO (gcRepHome+'MA\MAMATP') WITH lcPOTmpHD
*!*
*!*    lcOGTmpForm = lcSavTmpFrm
*!*    lcRpRName   = 'MAMATRQ'
*!*  ENDIF

*--Remove the P/O temp files.
USE IN (lcPOTmpLN)
USE IN (lcPOTmpHD)

*WSH
loBOMHEADR = .NULL.
*WSH

ERASE (oAriaApplication.WorkDir+lcPOTmpHD+'.DBF')
ERASE (oAriaApplication.WorkDir+lcPOTmpHD+'.CDX')
ERASE (oAriaApplication.WorkDir+lcPOTmpLN+'.DBF')
ERASE (oAriaApplication.WorkDir+lcPOTmpLN+'.CDX')

RETURN .T.

*!*************************************************************
*! Name      : lfvEdtMat
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Edit -ve Remaining line information.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvEdtMat()
*!*************************************************************
FUNCTION lfvEdtMat

*--Initialize screen variables.
llOk = .F.
STORE 1 TO lnCurrUnt1, lnCurrUnt2

SCATTER MEMVAR

lnSavRec = RECNO()

IF m.cCatGTyp = 'S'
  DO FORM (gcRepHome + "MA\MASTYINFO")
ELSE
  DO FORM (gcRepHome + "MA\MAFABINFO")
ENDIF

IF llOk
  IF Vendor <> m.Vendor OR cPriceCur <> m.cPriceCur OR nPriceRat <> m.nPriceRat
    SELECT (lcTmpRmain)
    LOCATE FOR Vendor = m.Vendor AND (cPriceCur <> m.cPriceCur OR nPriceRat <> m.nPriceRat)

    IF FOUND()
      *--This vendor has other PO lines. Replace all with new currency/rate ?","Yes;No
      IF gfModalGen('QRM36126B36001','DIALOG') = 1
        *-- Added to update the duty currency also for the common vendor.
        REPLACE ALL cPriceCur WITH m.cPriceCur,;
          cDutyCur  WITH m.cDutyCur,;
          nPriceRat WITH m.nPriceRat,;
          nDutyRat  WITH m.nDutyRat,;
          nCurrUnit WITH lnCurrUnt1,;
          nDCurUnit WITH lnCurrUnt2 FOR Vendor = m.Vendor
      ENDIF
    ENDIF
  ENDIF

  SELECT (lcTmpRmain)
  GOTO lnSavRec

  *-- the delivry date must be same as complete date
  *-- ic case of not use vendor referance [Start]
  IF !llVenRef
    m.dDelivDate = m.Complete
  ENDIF
  *B609665,1 WAM 08/23/2011 Update NQTY1 field
  IF m.cCatGTyp <> 'S'
    m.nQty1=m.nFabTotQty
  ENDIF
  *B609665,1 WAM 08/23/2011 (End)

  GATHER MEMVAR
  REPLACE nCurrUnit  WITH lnCurrUnt1,;
    nDCurUnit  WITH lnCurrUnt2
ENDIF

RETURN .F.

*!
*!*************************************************************
*! Name      : lfGetEquv
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get equivelent cost in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfGetEquv()
*!*************************************************************
FUNCTION lfGetEquv
PARA lcPcrcy,lnFrnCost,lnCurRate,lnCurUnt

lcPMethod = gfGetExSin(@lcPUnMeth,lcPcrcy)
lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
RETURN lnEquCost

*!
*!*************************************************************
*! Name      : lfvEdtLoc
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 07/03/2005
*! Purpose   : Validate the location.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvEdtLoc()
*!*************************************************************
FUNCTION lfvEdtLoc

IF TYPE('loWareHous') <> 'O'
  loWareHous = CREATEOBJECT('RemoteTable','WAREHOUS','WAREHOUS','WAREHOUSE',SET("Datasession"))
ENDIF

LOCAL lcWarCd
lcWarCd = &lcPOTmpHD..cWareCode

IF !loWareHous.SEEK(lcWarCd)
  lcWarCd = gfBrowWare(.F.)
ENDIF

*-- Address1 field in POFHDR should be Address1 of the Warehouse not the description.
SELECT (lcPOTmpHD)
IF !EMPTY(lcWarCd)
  REPLACE cWareCode WITH lcWarCd,;
    COUTADDR1 WITH WAREHOUSE.CADDRESS1,;
    COUTADDR2 WITH WAREHOUSE.CADDRESS2,;
    COUTADDR3 WITH WAREHOUSE.CADDRESS3,;
    COUTADDR4 WITH WAREHOUSE.CADDRESS4,;
    COUTADDR5 WITH WAREHOUSE.CADDRESS5
ELSE
  RETURN 0
ENDIF

RETURN 1
*!
*!*************************************************************
*! Name      : lfvOKFab
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/30/2005
*! Purpose   : Valid Ok in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvOKFab()
*!*************************************************************
FUNCTION lfvOKFab

IF m.nFabTotQty = 0
  *--Total quantity is 0 !! This line will not be included in the P/O.
  = gfModalGen('INM36127B36000','DIALOG')

  *--Update Changes.
  GATHER MEMVAR
  RETURN
ENDIF
llOk = .T.

RETURN

*!*************************************************************
*! Name        : lfDefForm
*! Developer   : Walid (wam)
*! Date        : 02/03/1999
*! Purpose     : Return form name (either O.T.S. or Requirement) Form
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Form Name
*!*************************************************************
*!
FUNCTION lfDefForm

PRIVATE lcFormName
lcFormName = 'MAMATRQ'

RETURN lcFormName

*!
*!*************************************************************
*! Name      : lfGetRemn
*! Developer : Hossam El Etreby [HDM]
*! Date      : 01/11/2000
*! Purpose   : Function to get the remaining qty[By Fabric/Trim]
*!*************************************************************
*! Called from : MAMATRQF.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetRemn()
*!*************************************************************
*!
FUNCTION lfGetRemn

PRIVATE lnTmp

lnTmp     = lfRemain('O')
lnOTotRem = lnOTotRem + lnTmp

lnTmp     = lfRemain('H')
lnHTotRem = lnHTotRem + lnTmp

lnTmp     = lfRemain('P')
lnPTotRem = lnPTotRem + lnTmp

lnOIssued = lnOIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))
lnHIssued = lnHIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))
lnPIssued = lnPIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))

RETURN ''

*!*************************************************************
*! Name      : lfReset
*! Developer : Hossam El Etreby[HDM]
*! Date      : 01/11/2000
*! Purpose   : Function to RESET ISSUED AND REMAINING VARIABLES
*!*************************************************************
*! Called from : MAMATRQC.FRX & MAMATRQF.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfReset
STORE 0 TO lnOIssued , lnHIssued , lnPIssued , lnOTotRem , lnHTotRem , lnPTotRem
RETURN ''

*!
*!**************************************************************************
*! Name      : lfSequence
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/06/2000
*! Purpose   : create a temp. sequence no for PO
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*!
FUNCTION lfSequence
PRIVATE lnSequence
lnSequence = 0
SCAN
  lnSequence = MAX(lnSequence,VAL(PO))
ENDSCAN
=SEEK(SPACE(20))
RETURN PADL(ALLTRIM(STR(lnSequence+1)),6,"0")
*!
*!*************************************************************
*! Name      : lfvOkPO
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : Valid function for the ok button in the generated Po's screen.
*! Ref       : B803005,1
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*!
FUNCTION lfvOkPO

*!*  IF !llGENMAPON .AND. !lfvTmpPoMa(.T.)
*!*    *-Material Po. Number have not been defined in some generates Pos.Please enter the Material Po.Number.
*!*    = gfModalGen('INM36172B36000','DIALOG')
*!*    RETURN
*!*  ENDIF

*--Check the warehous is not empty.
IF llWareHous
  LOCATE FOR EMPTY(cWareCode)
  IF FOUND()
    *-Some generated P/Os has not defined shipto location, Please enter the shipto location.
    = gfModalGen('INM36124B36000','DIALOG')
    RETURN
  ELSE
    ClearRead()
  ENDIF
ENDIF
*!
*!**************************************************************************
*! Name      : lfvTmpPoMa
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/20/2000
*! Purpose   : validate MA po no enterd by user
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Parameters: llCheckAll = .T. if it called to chaeck all records in the file
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfSequence()
*!**************************************************************************
FUNCTION lfvTmpPoMa
PARAMETER llCheckAll
PRIVATE lnMessage

*B609665,1 WAM 08/23/2011 Don't validate PO if PO# is generated automatically
IF llGENMAPON AND llGENSTPON
  RETURN
ENDIF
*B609665,1 WAM 08/23/2011 (End)

IF !llCheckAll
  *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
  *!*	  lcTmpPoMat = EVALUATE(SYS(18))
  *!*	  lcField    = SYS(18)
  lcTmpPoMat = &lcPOTmpHD..cTmpPoMat
  *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]
ELSE
  lcTmpPoMat = ''
  GO TOP
ENDIF
llRet=.T.

SCAN REST
  IF llCheckAll
    lcTmpPoMat = &lcPOTmpHD..cTmpPoMat
  ENDIF
  lnRecNo = RECNO()
  COUNT TO lnRecord FOR cTmpPoMat = lcTmpPoMat
  GO RECORD lnRecNo

  *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
  *!*	  IF lnRecord > 1  OR SEEK("P"+lcTmpPoMat,"POFHDR")
  IF lnRecord > 1  OR loPosHdr.SEEK('P'+IIF(&lcPOTmpHD..cStyType = 'M', 'M', 'P')+lcTmpPoMat,"POSHDR")
    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]

    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34023B34000','DIALOG',"Material Po. "))

    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
    REPLACE cTmpPoMat WITH  SPACE(6)
    *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]

    llRet=.F.
  ENDIF
  IF EMPTY(lcTmpPoMat)
    *-You cannot leave the Materila Po. number empty.
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34021B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF llRet AND LEN(ALLTRIM(lcTmpPoMat)) < 6
    *- "MA PO  number must be six digits.
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34022B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF !llCheckAll
    EXIT
  ENDIF
ENDSCAN
IF !llRet .AND. !llCheckAll
  lcTmpPoMat = ''

  *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
  *REPLACE &lcField WITH lcTmpPoMat
  REPLACE cTmpPoMat WITH lcTmpPoMat
  *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]

  KEYBOARD CHR(15)
  llRet = .T.
ENDIF
RETURN llRet

*!*************************************************************
*! Name      : lfGtWipPln
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/10/2005
*! Purpose   : caclculate the WIP for specific PO/CT
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .F.
*!*************************************************************
*!
FUNCTION lfGtWipPln

lcTranNo = cBusDocu+cStyType+PO
*B609959,1 MMT 06/12/2012 Incorrect Remaining MT When same style is found in PO more than once[Start]
lnPoLineNo = LINENO
*B609959,1 MMT 06/12/2012 Incorrect Remaining MT When same style is found in PO more than once[END]
*B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[Start]
*IF loCutTktl.Seek(lcTranNo)
IF loCutTktl.SEEK(lcTranNo,'POSLN')
  *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[End]

  SELECT CUTTKTL

  *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[Start]
  *SCAN REST WHILE cBusDocu+cStyType+Po = lcTranNo AND !(TranCd $ '36') AND &lcForWare
  *B609959,1 MMT 06/12/2012 Incorrect Remaining MT When same style is found in PO more than once[Start]
  *SCAN REST WHILE cBusDocu+cStyType+Po = lcTranNo FOR  Style=lcPrStyle AND !(TranCd $ '36') AND &lcForWare
  SCAN REST WHILE cBusDocu+cStyType+Po = lcTranNo FOR  STYLE=lcPrStyle AND LINENO = lnPoLineNo AND !(TranCd $ '36') AND &lcForWare
    *B609959,1 MMT 06/12/2012 Incorrect Remaining MT When same style is found in PO more than once[END]
    *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[End]

    lcSign  = IIF(cBusDocu='R','-','+')
    lcOSign = IIF(cBusDocu='R','+','-')
    FOR I=1 TO 8
      Z = STR(I,1)
      laWipPln[I] = IIF(TranCd = '1', (laWipPln[I] &lcSign MAX(Qty&Z,0)) ,;
        (MAX(laWipPln[I] &lcOSign ABS(Qty&Z),0) ) )
    ENDFOR
  ENDSCAN
ENDIF

FOR I=1 TO 8
  laWipPln[9]=laWipPln[9]+laWipPln[I]
ENDFOR

RETURN
*!
*!*************************************************************
*! Name      : lfStySum
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/08/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*!
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar

PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE STYLE = ALLTRIM(lcSty)
SELECT STYLE
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
*-- end of lfStySum.
*!
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/08/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*!
FUNCTION lfSRSty
PARAMETERS lcParm
DO CASE
CASE lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major
  *-- unique index.
  USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
  llChStyle = .T.
CASE lcParm = 'R'  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.
*!
*!*************************************************************
*! Name      : lfvReqBase
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/04/2002
*! Purpose   : Validate Requirement Based on option
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfvReqBase

IF lcRpReqBas <> 'W'
  llRpByWare = .F.
ENDIF
loOGScroll.RefreshScroll()
*-- end of lfvReqBase.

*!
*!*************************************************************
*! Name      : lfWipReq
*! Developer : Wael M. ABo-Shawareb (WSH)
*! Date      : 11/05/2005
*! Purpose   : Calculate the required in case of WIP
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfWipReq

PRIVATE lnAlias, lcHdrFile, lcLineFile, lcTranFld, lcNewTran, lnLineNo, lcStatus

lnAlias   = SELECT(0)
lcNewTran = SPACE(8)
lnLineNo  = 0
lcStatus  = SPACE(1)
lcHdrFile = loPosHdr.lcCursorView

SELECT POSLN
loPosLn.SEEK("0001"+lcCStyle)
SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = '0001'+lcCStyle ;
    FOR   CSTYTYPE $ 'PUN' .AND. !(TRANCD $ '36') .AND. &lcForWare
  loPosHdr.SEEK(Posln.cBusDocu+Posln.cStyType+Posln.PO)
  IF !(POSHDR.STATUS $ 'AOH')
    LOOP
  ENDIF
  lcSign  = IIF(cBusDocu = 'R', '-', '+')
  lcOSign = IIF(cBusDocu = 'R', '+', '-')
  IF POSLN.cBusDocu+POSLN.cStyType+POSLN.PO # lcNewTran
    IF !EMPTY(lcNewTran)
      *-- Pass the transaction number and line number to get the correct req. [Start]
      =lfGetWipRq(lcNewTran,lnLineNo,lcStatus)
    ENDIF
    lcNewTran = POSLN.cBusDocu+POSLN.cStyType+POSLN.PO
    lnLineNo  = POSLN.LINENO
    lcStatus  = POSHDR.STATUS

    SELECT POSLN
    IF !lfUpdWipRq()
      LOOP
    ENDIF
  ENDIF

  FOR I=1 TO lnCnt
    Z=STR(I,1)
    DO CASE
    CASE POSHDR.STATUS = 'A'
      laWIP[1,I]=IIF(POSLN.TranCd = '1', (laWIP[1,I] &lcSign. MAX(POSLN.Qty&Z,0)) ,;
        (MAX(laWIP[1,I] &lcOSign. ABS(POSLN.Qty&Z),0)))
      laWIP[1,9]=IIF(POSLN.TranCd = '1', (laWIP[1,9] &lcSign. MAX(POSLN.Qty&Z,0)) ,;
        (MAX(laWIP[1,9] &lcOSign. ABS(POSLN.Qty&Z),0)))
    CASE POSHDR.STATUS = 'O'
      laWIP[2,I]=IIF(POSLN.TranCd = '1', (laWIP[2,I] &lcSign. MAX(POSLN.Qty&Z,0)) ,;
        (MAX(laWIP[2,I] &lcOSign. ABS(POSLN.Qty&Z),0)))
      laWIP[2,9]=IIF(POSLN.TranCd = '1', (laWIP[2,9] &lcSign. MAX(POSLN.Qty&Z,0)) ,;
        (MAX(laWIP[2,9] &lcOSign. ABS(POSLN.Qty&Z),0)))
    CASE EVALUATE(lcHdrFile+'.Status') = 'H'
      laWIP[3,I]=IIF(POSLN.TranCd = '1', (laWIP[3,I] &lcSign. MAX(POSLN.Qty&Z,0)) ,;
        (MAX(laWIP[3,I] &lcOSign. ABS(POSLN.Qty&Z),0) ) )
      laWIP[3,9]=IIF(POSLN.TranCd = '1', (laWIP[3,9] &lcSign. MAX(POSLN.Qty&Z,0)) ,;
        (MAX(laWIP[3,9] &lcOSign. ABS(POSLN.Qty&Z),0)))
    ENDCASE
  ENDFOR
ENDSCAN
IF !EMPTY(lcNewTran)
  *--Pass the transaction number and line number to get the correct req. [Start]
  =lfGetWipRq(lcNewTran,lnLineNo,lcStatus)
ENDIF

SELECT (lnAlias)
RETURN
*-- end of lfWipReq.
*!
*!*************************************************************
*! Name      : lfUpdWipRq
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/18/2005
*! Purpose   : Calculate the required in case of WIP
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfUpdWipRq

llFabrics = ATC('lcFabrics',loOGScroll.lcRpExp)> 0
IF llFabrics
  lnFabricsCnd = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'lcFabrics'),1)
  lcFabricsFile = loOGScroll.laOgFxFlt[lnFabricsCnd,6]
  IF !EMPTY(lcFabricsFile)
    SELECT &lcFabricsFile
    INDEX ON cStyMajor TAG &lcFabricsFile
  ENDIF
ENDIF

lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'ITEMLOC.CWARECODE'),1)
lcFLocFile = loOGScroll.laOgFxFlt[lnLocCondition,6]

*-- If Style Location Are selected.
IF !EMPTY(lcFLocFile)
  SELECT &lcFLocFile
  INDEX ON cWareCode TAG &lcSelFabrics
ENDIF
lnColorCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'lcFabClr'),1)
lcFabricColors = loOGScroll.laOgFxFlt[lnLocCondition,6]

lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYDYE.CWARECODE'),1)
lcLocFile = loOGScroll.laOgFxFlt[lnLocCondition,6]
*-- If Style Location Are selected.
IF !EMPTY(lcLocFile) .AND. RECCOUNT(lcLocFile) > 0
  *- Create a remote table from Stydye
  llByLoctn = .T.
ENDIF

loStyle.SEEK(POSLN.STYLE)
IF EVALUATE(lcHdrFile+'.STATUS') <> 'H'
  IF loBomLine.SEEK(IIF(POSLN.cStyType='U','M1','I1')+POSLN.PO+STR(POSLN.LINENO,6))
    SELECT (lcMatReq)
    IF llRpByWare
      SET ORDER TO TAG MRFABLOC
    ELSE
      SET ORDER TO TAG MATREQ
    ENDIF
    SELECT BOMLINE
    SCAN REST WHILE cimtyp+ctype+ctktno+STR(LINENO,6)=IIF(POSLN.cStyType='U','M','I')+'1'+POSLN.PO+STR(POSLN.LINENO,6);
        FOR STYLE = lcCStyle .AND. cCatgTyp $ IIF(lcRpCompn='A','FTS',lcRpCompn)

      loCtktBom.SEEK(CIMTYP+CTKTNO+CBOMTYP+cInvTypC+ITEM)
      IF llRpByWare .AND. CCATGTYP $ 'FT'
        IF !EMPTY(COPRCODE) .AND. loMfgOprHd.SEEK(CIMTYP+CTKTNO+COPRCODE)
          IF !EMPTY(MFGOPRHD.CCONTCODE) .AND. loApVendor.SEEK(MFGOPRHD.CCONTCODE)
            llFound = !EMPTY(APVENDOR.CWARECODE)
          ENDIF
        ENDIF
        IF llByFabLoc
          IF llFound
            IF !SEEK(APVENDOR.CWARECODE,&lcFLocFile)
              LOOP
            ENDIF
          ELSE
            LOOP
          ENDIF
        ENDIF
      ENDIF
      *-- Add only fabrics/trims in the selection critria [Start]
      IF !SEEK(CCATGTYP+IIF(llRpByWare .AND. CCATGTYP $ 'FT',;
          IIF(llFound,'N'+APVENDOR.CWARECODE,'Y'+SPACE(6)),'')+ITEM+STYLE,lcMatReq)
        IF Bomline.cCatgTyp $ 'FT'
          *B609665,1 WAM 08/23/2011 Get item color using the fabric code structure
          *IF (llFabrics AND !SEEK(SUBSTR(ITEM,1,lnFMajLen),lcFabricsFile)) .OR. (!EMPTY(lcFabricColors) AND !(SUBSTR(ITEM,lnClrSrt,lnClrEnd) $ lcFabricColors))
          IF (llFabrics AND !SEEK(SUBSTR(ITEM,1,lnFMajLen),lcFabricsFile)) .OR. (!EMPTY(lcFabricColors) AND !(SUBSTR(ITEM,lnFClrSrt,lnFClrEnd) $ lcFabricColors))
            *B609665,1 WAM 08/23/2011 (End)

            LOOP
          ENDIF
        ENDIF

        IF llByLoctn  && if user seelcted style locations append in the Temp file
          SELECT (lcLocFile)
          SCAN
            SELECT (lcMatReq)
            APPEND BLANK
            REPLACE STYLE     WITH BOMLINE.STYLE       ,;
              Typ       WITH BOMLINE.CBOMTYP     ,;
              cCatgTyp  WITH BOMLINE.cCatgTyp    ,;
              ITEM      WITH BOMLINE.ITEM,;
              DESC      WITH CTKTBOM.DESC        ,;
              Uom       WITH IIF(loUom.SEEK(CTKTBOM.cUomCode),Uom.cUom_V,""),;
              CNT       WITH IIF(lnCnt=0,8,lnCnt),;
              lStyMake  WITH STYLE.MAKE          ,;
              LGETTHIS  WITH .T.                 ,;
              cWareCode WITH &lcLocFile..cWareCode

            IF llRpByWare .AND. CCATGTYP $ 'FT'
              IF llFound
                REPLACE CFABLOC  WITH APVENDOR.CWARECODE,;
                  CINHOUSE WITH 'N'
              ELSE
                REPLACE CINHOUSE WITH 'Y'
              ENDIF
            ENDIF
            FOR I=1 TO lnCnt
              lcSz = STR(I,1)
              IF lcSz $ BOMLINE.CSIZES
                REPLACE Qty&lcSz WITH BOMLINE.UNITQTY
              ENDIF
            ENDFOR
          ENDSCAN
        ELSE && If user didn't select style locations
          SELECT (lcMatReq)
          APPEND BLANK
          REPLACE STYLE     WITH BOMLINE.STYLE       ,;
            Typ       WITH BOMLINE.CBOMTYP     ,;
            cCatgTyp  WITH BOMLINE.cCatgTyp    ,;
            ITEM      WITH BOMLINE.ITEM,;
            DESC      WITH CTKTBOM.DESC        ,;
            Uom       WITH IIF(loUom.SEEK(CTKTBOM.cUomCode),Uom.cUom_V,""),;
            CNT       WITH IIF(lnCnt=0,8,lnCnt),;
            lStyMake  WITH STYLE.MAKE          ,;
            LGETTHIS  WITH .T.                 ,;
            cWareCode WITH ""

          IF llRpByWare .AND. CCATGTYP $ 'FT'
            IF llFound

              REPLACE CFABLOC  WITH APVENDOR.CWARECODE,;
                CINHOUSE WITH 'N'
            ELSE
              REPLACE CINHOUSE WITH 'Y'
            ENDIF
          ENDIF
          FOR I=1 TO lnCnt
            lcSz = STR(I,1)
            IF lcSz $ BOMLINE.CSIZES
              REPLACE Qty&lcSz WITH BOMLINE.UNITQTY
            ENDIF
          ENDFOR

        ENDIF
      ELSE
        SELECT (lcMatReq)
        REPLACE LGETTHIS WITH .T.
      ENDIF
    ENDSCAN
  ELSE
    RETURN .F.
  ENDIF
ENDIF
FOR lnXLoop = 1 TO 3
  FOR lnYLoop = 1 TO 9
    laWIP[lnXLoop,lnYLoop] = 0
  ENDFOR
ENDFOR
RETURN .T.
*-- end of lfUpdWipRq.
*!
*!*************************************************************
*! Name      : lfGetWipRq
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/18/2005
*! Purpose   : Calculate the required in case of WIP
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetWipRq()
*!*************************************************************
*!
FUNCTION lfGetWipRq

*-- Add parameters to hold the transaction number and line number [Start]
PARAMETERS lcNewTran,lnLineNo,lcStatus

SELECT (lcMatReq)
SET ORDER TO TAG MRSTYITM
SEEK lcCStyle
loStyle.SEEK(lcCStyle)
SCAN REST WHILE STYLE = lcCStyle
  IF !LGETTHIS .AND. IIF(lcStatus='H',!LBOM,.T.)
    LOOP
  ENDIF
  lcSeekItem = ITEM
  loBomLine.SEEK(IIF(SUBSTR(lcNewTran,2,1)='U','M','I')+'1'+SUBSTR(lcNewTran,3)+STR(lnLineNo,6)+TYP+'0001'+lcCStyle+IIF(cCatgTyp='S','0001','0002')+lcSeekItem)

  FOR I=1 TO lnCnt
    lcSz = STR(I,1)
    IF EVALUATE('OHREQ'+lcSz) <> 0
      IF lcSz $ BOMLINE.CSIZES
        REPLACE YIELD&lcSz WITH (EVALUATE('YIELD'+lcSz+'*OHREQ'+lcSz)+;
          (BOMLINE.UNITQTY*laRequr[2,I]))/;
          (EVALUATE('OHREQ'+lcSz)+laRequr[2,I])
      ELSE
        IF lcStatus <> 'H'
          REPLACE YIELD&lcSz WITH EVALUATE('YIELD'+lcSz+'*OHREQ'+lcSz)/;
            (EVALUATE('OHREQ'+lcSz)+laRequr[2,I])
        ELSE
          REPLACE YIELD&lcSz WITH (EVALUATE('YIELD'+lcSz+'*OHREQ'+lcSz)+;
            (EVALUATE('QTY'+lcSz)*laRequr[2,I]))/;
            (EVALUATE('OHREQ'+lcSz)+laRequr[2,I])
        ENDIF
      ENDIF
    ELSE
      IF lcSz $ BOMLINE.CSIZES
        REPLACE YIELD&lcSz WITH BOMLINE.UNITQTY
      ENDIF
    ENDIF
  ENDFOR

  *--Calculate Requirements Pieces per size.
  FOR lnXLoop = 1 TO 3
    FOR lnYLoop = 1 TO 9
      laRequr[lnXLoop,lnYLoop] = 0
    ENDFOR
  ENDFOR
  FOR I=1 TO lnCnt
    Z=STR(I,1)
    *--[1] Requirements Pieces for OPEN orders.
    laRequr[1,I] = MAX( laWIP[1,I]+laWIP[2,I] , 0 )
    laRequr[1,9] = laRequr[1,9] + laRequr[1,I]

    *--[2] Requirements Pieces for OPEN+HOLD orders.
    laRequr[2,I] = MAX( laWIP[1,I]+laWIP[2,I]+laWIP[3,I] , 0 )
    laRequr[2,9] = laRequr[2,9] + laRequr[2,I]

    *--[3] Requirements Pieces for Projection (dependent).
    *--Compute the Projection (Depends on requirements calculated before).
    *--or depens on plane if select projection for plane.
    DO CASE
      *--[1] Projection base on Open.
    CASE lcRpBasePj = 'O'
      laRequr[3,I] = laRequr[1,I] + ROUND(laRequr[1,I] * (lnRpPrIncr/100),0)
      *--[2] Projection base on Open+Hold.
    CASE lcRpBasePj = 'H'
      laRequr[3,I] = laRequr[2,I] + ROUND(laRequr[2,I] * (lnRpPrIncr/100),0)
      *--[3] Requirements Pieces for Plane.
    CASE lcRpBasePj = 'P'
      laRequr[3,I] = IIF(STYLE.Plan&Z > 0, STYLE.Plan&Z + ROUND(STYLE.Plan&Z*((lnRpPrIncr/100)),0), 0)
    ENDCASE
    laRequr[3,9] = laRequr[3,9] + laRequr[3,I]
  ENDFOR

  REPLACE OReq1    WITH OReq1 + laRequr[1,1],;
    OReq2    WITH OReq2 + laRequr[1,2],;
    OReq3    WITH OReq3 + laRequr[1,3],;
    OReq4    WITH OReq4 + laRequr[1,4],;
    OReq5    WITH OReq5 + laRequr[1,5],;
    OReq6    WITH OReq6 + laRequr[1,6],;
    OReq7    WITH OReq7 + laRequr[1,7],;
    OReq8    WITH OReq8 + laRequr[1,8],;
    OReqTot  WITH OReqTot + laRequr[1,9],;
    OHReq1   WITH OHReq1 + laRequr[2,1],;
    OHReq2   WITH OHReq2 + laRequr[2,2],;
    OHReq3   WITH OHReq3 + laRequr[2,3],;
    OHReq4   WITH OHReq4 + laRequr[2,4],;
    OHReq5   WITH OHReq5 + laRequr[2,5],;
    OHReq6   WITH OHReq6 + laRequr[2,6],;
    OHReq7   WITH OHReq7 + laRequr[2,7],;
    OHReq8   WITH OHReq8 + laRequr[2,8],;
    OHReqTot WITH OHReqTot + laRequr[2,9],;
    nProj1   WITH nProj1 + laRequr[3,1],;
    nProj2   WITH nProj2 + laRequr[3,2],;
    nProj3   WITH nProj3 + laRequr[3,3],;
    nProj4   WITH nProj4 + laRequr[3,4],;
    nProj5   WITH nProj5 + laRequr[3,5],;
    nProj6   WITH nProj6 + laRequr[3,6],;
    nProj7   WITH nProj7 + laRequr[3,7],;
    nProj8   WITH nProj8 + laRequr[3,8],;
    PROJECT  WITH PROJECT + laRequr[3,9]

  *--Update the open WIP pieces.
  REPLACE nOpnWip1 WITH nOpnWip1 + laWIP[2,1],;
    nOpnWip2 WITH nOpnWip2 + laWIP[2,2],;
    nOpnWip3 WITH nOpnWip3 + laWIP[2,3],;
    nOpnWip4 WITH nOpnWip4 + laWIP[2,4],;
    nOpnWip5 WITH nOpnWip5 + laWIP[2,5],;
    nOpnWip6 WITH nOpnWip6 + laWIP[2,6],;
    nOpnWip7 WITH nOpnWip7 + laWIP[2,7],;
    nOpnWip8 WITH nOpnWip8 + laWIP[2,8],;
    nOpnWip  WITH nOpnWip + laWIP[2,9],;
    LGETTHIS WITH .F.
ENDSCAN
SET ORDER TO TAG MRStyle
SEEK lcCStyle
*-- end of lfGetWipRq.
*!
*!*************************************************************
*! Name      : lfGetNetRq
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/23/2005
*! Purpose   : Calculate the net required
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfGetNetRq

PRIVATE lnAlias
lnAlias = SELECT(0)
STORE 0 TO lnNRequ

IF loBomLine.SEEK(lcTktType+'1'+POSLN.PO+STR(POSLN.LINENO,6)+lcPrTyp+'0001'+lcPrStyle+IIF(EVALUATE(lcMatReq+'.cCatgTyp')='S','0001','0002')+lcPrItem)
  SELECT BOMLINE
  SUM REST ITEMQTY TO lnNRequ;
    WHILE cimtyp+ctype+ctktno+STR(LINENO,6)+cbomtyp+cInvType+STYLE+cInvTypC+ITEM+mfgcode =;
    lcTktType+'1'+POSLN.PO+STR(POSLN.LINENO,6)+lcPrTyp+'0001'+lcPrStyle+IIF(EVALUATE(lcMatReq+'.cCatgTyp')='S','0001','0002')+lcPrItem
ENDIF

*B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[Start]
*lnNRequ = MAX(lnNRequ-lnIssue,0)
lnNRequ = lnNRequ-lnIssue
*B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[End]


SELECT (lnAlias)
*!
*!*************************************************************
*! Name      : lfUpdNetRq
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/23/2005
*! Purpose   : Update the net required by style
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfUpdNetRq


lcWrkLnFl = 'POSLN'
lcWrkHdFl = 'POSHDR'
IF !loPosLn.SEEK('0001'+lcPrStyle)
  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    SELECT (lcMatReq)
    GO lnXRecNo
    SET ORDER TO TAG MatreqC
    SET KEY TO cCatgTyp
    GO lnXRecNo
  ENDIF
  RETURN
ENDIF

lcSvTkt = "  "
lnAlias = SELECT(0)

SELECT POSLN
SCAN
  lcTktType = IIF(POSLN.cStyType = 'U', 'M', 'I')


  loPosHdr.SEEK(Posln.cBusDocu+Posln.cStyType+Posln.PO)
  IF !(POSHDR.STATUS $ 'O' AND TranCd = '1')
    LOOP
  ENDIF
  *--Seek on full expresion in BomLine.
  *-cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
  IF loBomLine.SEEK(IIF(cStyType='U','M','I')+'1'+PO+STR(POSLN.LINENO,6))
    SELECT BOMLINE
    IF llRpByWare .AND. CCATGTYP $ 'FT'
      IF !EMPTY(COPRCODE) .AND. loMfgOprHd.SEEK(CIMTYP+CTKTNO+COPRCODE)
        IF !EMPTY(MFGOPRHD.CCONTCODE) .AND. loApVenDor.SEEK(MFGOPRHD.CCONTCODE)
          llFound = !EMPTY(APVENDOR.CWARECODE)
        ENDIF
      ENDIF
      IF llFound
        IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'N'+APVENDOR.CWARECODE
          LOOP
        ENDIF
      ELSE
        IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'Y'+SPACE(6)
          LOOP
        ENDIF
      ENDIF
    ENDIF
  ENDIF



  IF loCtktBom.SEEK(IIF(POSLN.cStyType='U','M','I')+POSLN.PO+lcPrTyp+IIF(EVALUATE(lcMatReq+'.cCatgTyp') = 'S', '0001', '0002')+lcPrItem)
    SELECT CtktBom
    IF !(EVALUATE(lcMatReq+'.cCatgTyp') = 'S')
      STORE 0 TO lnIssue,lnNRequ

      *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [Start]
      *!*	      *B607923,1 MMT 01/05/2007 Material Requirement Report not Calculating CorrectlyT20061214.0019 [Start]
      *!*        SELECT CTKTBOM
      *!*        SUM REST Used_Qty,MAX(Req_Qty-Used_Qty,0) TO lnIssue,lnNRequ ;
      *!*    		          WHILE cIMTyp+Cuttkt+typ+Item = lcTktType+POSLN.PO+lcPrTyp+lcPrItem

      *!*	      *B607923,1 MMT 01/05/2007 Material Requirement Report not Calculating CorrectlyT20061214.0019 [end]
      *This modification done because the report get wrong issued values in case of
      *same material is existing in more than one style in the same PO
      SELECT (lcMatReq)
      lcFindEXP = IIF(llRpByWare,'cCatgTyp+cInHouse+cFabLoc+Item','cCatgTyp+Item')
      lcExpLocate =IIF(llRpByWare,cCatgTyp+cInHouse+cFabLoc+ITEM,cCatgTyp+ITEM)
      lnSavOldRec = RECNO()
      LOCATE
      LOCATE FOR &lcFindEXP = lcExpLocate  AND lcTktType+POSLN.PO $ PoCt AND RECNO()< lnSavOldRec
      IF !FOUND()
        SELECT (lcMatReq)
        GO RECORD lnSavOldRec
        *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP[End]

        SELECT CTKTBOM
        SUM REST Used_Qty,MAX(Req_Qty-Used_Qty,0) TO lnIssue,lnNRequ ;
          WHILE cIMTyp+Cuttkt+typ+ITEM = lcTktType+POSLN.PO+lcPrTyp+lcPrItem
        *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [Start]
      ENDIF
      SELECT (lcMatReq)
      GO RECORD lnSavOldRec
      *B608012,1 MMT 03/22/2007 fix bug of wrong Remain in case of OTC and WIP [End]



      =lfGetNetRq()
      SELECT (lcMatReq)
      lnSavRec = RECNO()
      lcSekKey = IIF(llRpByWare,cCatgTyp+cInHouse+cFabLoc+ITEM,cCatgTyp+ITEM)
      lcWhrCnd = IIF(llRpByWare,'cCatgTyp+cInHouse+cFabLoc+Item','cCatgTyp+Item')+'=lcSekKey'

      SEEK lcSekKey
      SCAN REST WHILE &lcWhrCnd. FOR STYLE = lcPrStyle
        IF !(lcTktType+POSLN.PO $ PoCt)
          *B607923,1 MMT 01/05/2007 Material Requirement Report not Calculating CorrectlyT20061214.0019 [Start]
          *REPLACE nNetReq  WITH nNetReq  + lnNRequ,;
          PoCt     WITH poCt + lcTktType + POSLN.PO+','

          REPLACE nUsedReq WITH nUsedReq + lnIssue,;
            nNetReq  WITH nNetReq  + lnNRequ,;
            PoCt     WITH poCt + lcTktType + POSLN.PO + ','

          **B607923,1 MMT 01/05/2007 Material Requirement Report not Calculating CorrectlyT20061214.0019 [End]
        ENDIF
      ENDSCAN
      GOTO lnSavRec
    ENDIF
  ENDIF
  lcSvTkt = POSLN.PO
ENDSCAN

IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  SELECT (lcMatReq)
  GO lnXRecNo
  SET ORDER TO TAG MatreqC
  SET KEY TO cCatgTyp
  GO lnXRecNo
ENDIF
SELECT (lnAlias)
*!
*!*************************************************************
*! Name      : lfSaveMrp
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Save the MRP report.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfSaveMrp

PRIVATE lnAlias,lnReccount,lnCurNum,lnFltCnt,lnI,lcOrder
PRIVATE lnSep,lcvEntries,lnvEntries,lavEntries,lcVarType,lcFieldNam
lnAlias = SELECT(0)

IF TYPE("loMrpHdr") # 'O'
  loMrpHdr = CREATEOBJECT('RemoteTable','MRPHDR','MRPHDR','MRPHDR',SET("Datasession"))
ENDIF

IF TYPE("loMrpLn") # 'O'
  loMrpLn = CREATEOBJECT('RemoteTable','MRPLN','MRPLN','MRPLN',SET("Datasession"))
ENDIF

LOCAL lcStat
lcStat = "SELECT *" +;
  "  FROM SYREPUVR" +;
  " WHERE CREP_ID+PADR(SUBSTR(MFLD_NAME,1,100),100,' ') = 'MAMATRQ '+ALLTRIM(MFLD_NAME)"

lnResult = loOgScroll.oRDA.SqlRun(lcStat, "TMPUVR", , oAriaApplication.cAria4Sysfiles, 3, "SAVE",)
IF lnResult <> 1
  RETURN
ENDIF

SELECT TMPUVR
=CURSORSETPROP("Buffering", 3)
INDEX ON CREP_ID+PADR(SUBSTR(MFLD_NAME,1,100),100,' ') TAG CREP_ID

IF TYPE("loMrpFlt") # 'O'
  loMrpFlt = CREATEOBJECT('RemoteTable','MRPFLT','MRPFLT','MRPFLT',SET("Datasession"))
ENDIF

IF TYPE("loSYDFIELD") # 'O'
  loSYDFIELD = CREATEOBJECT('RemoteTable','SYDFIELD','CFLD_NAME','SYDFIELD',SET("Datasession"))
ENDIF

*-- Update the header file.
SELECT MRPHDR

loMRPHDR.APPEND()
SELECT (loMRPHDR.lcCursorUpdate)

lcMrp = gfSequence('CMRP')
REPLACE CMRP WITH lcMrp
SAVE TO MEMO MFLTVAL ALL LIKE l?Rp*

=gfAdd_Info(loMRPHDR.lcCUrsorUpdate)

*-- Update the lines file.
SELECT (lcMatReq)
LOCATE

*--Thermometer counter.
lnRecCount = RECCOUNT()
lnCurNum   = 0
SCAN
  lnCurNum = lnCurNum + 1
  *-=gfThermo(lnRecCount,lnCurNum ,"Save the MRP data",'MRP # : '+lcMrp)
  SCATTER MEMO MEMVAR

  loMRPLN.APPEND()
  SELECT (loMRPLN.lcCursorUpdate)
  REPLACE CMRP      WITH lcMrp      ,;
    STYLE     WITH M.STYLE    ,;
    ITEM      WITH SUBSTR(M.ITEM, 1, IIF(M.CCATGTYP = 'S', lnMajorLn, lnFMajLen)),;
    ICLR      WITH SUBSTR(M.ITEM, IIF(M.CCATGTYP = 'S', lnClrSrt, lnFClrSrt), IIF(M.CCATGTYP = 'S', lnClrEnd, lnFClrEnd)),;
    DESC      WITH M.DESC     ,;
    CWARECODE WITH M.CWARECODE,;
    TYP       WITH M.TYP      ,;
    CCATGTYP  WITH M.CCATGTYP ,;
    CNT       WITH M.CNT      ,;
    UOM       WITH M.UOM

  REPLACE NQTY1     WITH M.QTY1     ,;
    NQTY2     WITH M.QTY2     ,;
    NQTY3     WITH M.QTY3     ,;
    NQTY4     WITH M.QTY4     ,;
    NQTY5     WITH M.QTY5     ,;
    NQTY6     WITH M.QTY6     ,;
    NQTY7     WITH M.QTY7     ,;
    NQTY8     WITH M.QTY8
  REPLACE NOREQ1    WITH M.OREQ1    ,;
    NOREQ2    WITH M.OREQ2    ,;
    NOREQ3    WITH M.OREQ3    ,;
    NOREQ4    WITH M.OREQ4    ,;
    NOREQ5    WITH M.OREQ5    ,;
    NOREQ6    WITH M.OREQ6    ,;
    NOREQ7    WITH M.OREQ7    ,;
    NOREQ8    WITH M.OREQ8    ,;
    NOREQTOT  WITH M.OREQTOT
  REPLACE NOHREQ1   WITH M.OHREQ1   ,;
    NOHREQ2   WITH M.OHREQ2   ,;
    NOHREQ3   WITH M.OHREQ3   ,;
    NOHREQ4   WITH M.OHREQ4   ,;
    NOHREQ5   WITH M.OHREQ5   ,;
    NOHREQ6   WITH M.OHREQ6   ,;
    NOHREQ7   WITH M.OHREQ7   ,;
    NOHREQ8   WITH M.OHREQ8   ,;
    NOHREQTOT WITH M.OHREQTOT
  REPLACE NPROJ1    WITH M.NPROJ1   ,;
    NPROJ2    WITH M.NPROJ2   ,;
    NPROJ3    WITH M.NPROJ3   ,;
    NPROJ4    WITH M.NPROJ4   ,;
    NPROJ5    WITH M.NPROJ5   ,;
    NPROJ6    WITH M.NPROJ6   ,;
    NPROJ7    WITH M.NPROJ7   ,;
    NPROJ8    WITH M.NPROJ8   ,;
    NPROJECT  WITH M.PROJECT
  REPLACE NOPNWIP   WITH M.NOPNWIP  ,;
    NYTOWIP   WITH M.NYTOWIP  ,;
    NUSEDREQ  WITH M.NUSEDREQ ,;
    NUSEDREQ1  WITH M.NUSEDREQ1 ,;
    NUSEDREQ2  WITH M.NUSEDREQ2 ,;
    NUSEDREQ3  WITH M.NUSEDREQ3 ,;
    NUSEDREQ4  WITH M.NUSEDREQ4 ,;
    NUSEDREQ5  WITH M.NUSEDREQ5 ,;
    NUSEDREQ6  WITH M.NUSEDREQ6 ,;
    NUSEDREQ7  WITH M.NUSEDREQ7 ,;
    NUSEDREQ8  WITH M.NUSEDREQ8 ,;
    NNETREQ   WITH M.NNETREQ  ,;
    LSTYMAKE  WITH M.LSTYMAKE ,;
    NNETREQ1  WITH M.NNETREQ1 ,;
    NNETREQ2  WITH M.NNETREQ2 ,;
    NNETREQ3  WITH M.NNETREQ3 ,;
    NNETREQ4  WITH M.NNETREQ4 ,;
    NNETREQ5  WITH M.NNETREQ5 ,;
    NNETREQ6  WITH M.NNETREQ6 ,;
    NNETREQ7  WITH M.NNETREQ7 ,;
    NNETREQ8  WITH M.NNETREQ8
  REPLACE NOPNWIP1  WITH M.NOPNWIP1 ,;
    NOPNWIP2  WITH M.NOPNWIP2 ,;
    NOPNWIP3  WITH M.NOPNWIP3 ,;
    NOPNWIP4  WITH M.NOPNWIP4 ,;
    NOPNWIP5  WITH M.NOPNWIP5 ,;
    NOPNWIP6  WITH M.NOPNWIP6 ,;
    NOPNWIP7  WITH M.NOPNWIP7 ,;
    NOPNWIP8  WITH M.NOPNWIP8
  REPLACE NYTOWIP1  WITH M.NYTOWIP1 ,;
    NYTOWIP2  WITH M.NYTOWIP2 ,;
    NYTOWIP3  WITH M.NYTOWIP3 ,;
    NYTOWIP4  WITH M.NYTOWIP4 ,;
    NYTOWIP5  WITH M.NYTOWIP5 ,;
    NYTOWIP6  WITH M.NYTOWIP6 ,;
    NYTOWIP7  WITH M.NYTOWIP7 ,;
    NYTOWIP8  WITH M.NYTOWIP8 ,;
    MPOCT     WITH M.PoCt     ,;
    MFLTVAL   WITH M.MFLTVAL
  REPLACE CFABLOC   WITH M.CFABLOC  ,;
    CINHOUSE  WITH M.CINHOUSE ,;
    LGETTHIS  WITH M.LGETTHIS ,;
    LBOM      WITH M.LBOM     ,;
    NYIELD1   WITH M.YIELD1   ,;
    NYIELD2   WITH M.YIELD2   ,;
    NYIELD3   WITH M.YIELD3   ,;
    NYIELD4   WITH M.YIELD4   ,;
    NYIELD5   WITH M.YIELD5   ,;
    NYIELD6   WITH M.YIELD6   ,;
    NYIELD7   WITH M.YIELD7   ,;
    NYIELD8   WITH M.YIELD8
  REPLACE ORDER     WITH M.ORDER    ,;
    COMPLETE  WITH M.COMPLETE ,;
    ONHAND    WITH M.ONHAND   ,;
    ONORDER   WITH M.ONORDER  ,;
    ETA       WITH M.ETA      ,;
    LMULTIPO  WITH M.LMULTIPO ,;
    LNOTPRINT WITH M.LNOTPRINT,;
    ITEMSTK   WITH M.ITEMSTK

  =gfAdd_Info(loMRPLN.lcCursorUpdate)
ENDSCAN
*-=gfThermo(100,100)

*-- Update the OG selections.
*--Thermometer counter.
lnFltCnt = ALEN(laOgObjType,1)

FOR lnI = 1 TO lnFltCnt
  *-=gfThermo(lnFltCnt,lnI ,"Save the OG selections",'MRP # : '+lcMrp)
  SELECT MRPFLT

  loMRPFLT.APPEND()
  SELECT (loMRPFLT.lcCursorUpdate)

  *B609665,1 WAM 08/23/2011 Fix saving fixed filter
  *REPLACE CMRP WITH lcMrp,;
  MFLD_NAME WITH IIF(UPPER(laOgObjType[lnI,1])='loOGScroll.laOgFxFlt',;
  EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','1]')),laOgObjType[lnI,1])
  REPLACE CMRP WITH lcMrp,;
    MFLD_NAME WITH IIF( UPPER('laOgFxFlt') $ UPPER(laOgObjType[lnI,1]) ,;
    EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','1]')),laOgObjType[lnI,1])
  *B609665,1 WAM 08/23/2011 (End)


  *-- Get the filter header and value.
  *-- Add Flag to know if the record found in SYREPUVR [Begin]
  llNotFound = .F.
  llFound = .F.

  *B609665,1 WAM 08/23/2011 Fix saving fixed filter
  *IF SEEK('MAMATRQ '+PADR(MFLD_NAME,100), 'TMPUVR') .OR.;
  *   SEEK('MAMATRQ '+ALLTRIM(MFLD_NAME), 'TMPUVR')
  SELECT 'TMPUVR'
  LOCATE FOR UPPER(MFLD_NAME) = ALLTRIM(UPPER(MRPFLT.MFLD_NAME))
  SELECT (loMRPFLT.lcCursorUpdate)
  IF FOUND('TMPUVR')
    *B609665,1 WAM 08/23/2011 (End)

    *-- Locate for records belong to Aria27 only [Begin]
    IF !EMPTY(TMPUVR.CVER) AND TMPUVR.CVER <> 'A27'
      lcAlias     = ALIAS()
      lcMFLD_NAME = MFLD_NAME

      SELECT TMPUVR
      LOCATE WHILE crep_id+PADR(SUBSTR(mfld_name,1,100),100," ") = 'MAMATRQ '+PADR(lcMFLD_NAME,100) FOR (EMPTY(CVER) OR CVER = 'A27')
      IF !FOUND()
        LOCATE WHILE crep_id+PADR(SUBSTR(mfld_name,1,100),100," ") = 'MAMATRQ '+ALLTRIM(MFLD_NAME) FOR (EMPTY(CVER) OR CVER = 'A27')
      ENDIF
      IF FOUND()
        llFound = .T.
      ENDIF
      SELECT (lcAlias)
    ELSE
      llFound = .T.
    ENDIF
    IF llFound
      REPLACE CFLD_HEAD WITH IIF(TMPUVR.CFLD_HEAD='~',;
        EVALUATE(ALLTRIM(SUBSTR(TMPUVR.CFLD_HEAD,2))),;
        TMPUVR.CFLD_HEAD)
      DO CASE
        *B609665,1 WAM 08/23/2011 Fix saving fixed filter
        *CASE UPPER(laOgObjType[lnI,1]) = 'loOGScroll.laOgFxFlt'  && case of fixed filter
      CASE UPPER('laOgFxFlt') $ UPPER(laOgObjType[lnI,1])   && case of fixed filter
        *B609665,1 WAM 08/23/2011 (End)

        lcVarType = UPPER(ALLTRIM(EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','5]'))))
        DO CASE
        CASE lcVarType = 'BETWEEN'
          lnSep = AT('|',EVALUATE(laOgObjType[lnI,1]))
          IF lnSep > 0 .AND. LEN(EVALUATE(laOgObjType[lnI,1])) > 1
            REPLACE MFLTVAL WITH 'From '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),1,lnSep-1)+;
              ' To '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),lnSep+1)
          ENDIF
        CASE lcVarType = 'IN LIST'
          IF EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','7]')) = 'R'
            IF USED(EVALUATE(laOgObjType[lnI,1]))
              SELECT (EVALUATE(laOgObjType[lnI,1]))
              lcFieldNam = EVALUATE(laOgObjType[lnI,1]) + '.' +;
                FIELD(1,EVALUATE(laOgObjType[lnI,1]))
              SCAN
                SELECT (loMRPFLT.lcCursorUpdate)
                REPLACE MFLTVAL WITH MFLTVAL+IIF(EMPTY(MFLTVAL),'',',')+EVALUATE(lcFieldNam)
              ENDSCAN
              SELECT (loMRPFLT.lcCursorUpdate)
            ENDIF
          ELSE
            REPLACE MFLTVAL WITH STRTRAN(EVALUATE(laOgObjType[lnI,1]),'|',',')
          ENDIF
        OTHERWISE
          lcVarType = TYPE('EVALUATE(laOgObjType[lnI,1])')
          DO CASE
          CASE lcVarType = 'C'
            REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,1])
          CASE lcVarType = 'N'
            REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,1]))
          CASE lcVarType = 'D'
            REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,1]))
          ENDCASE
        ENDCASE
      CASE UPPER(laOgObjType[lnI,2]) = 'INVB'       && case of popup
        IF EMPTY(TMPUVR.MVENTRIES)  && case of yes/no
          IF EVALUATE(laOgObjType[lnI,2]) = 1
            REPLACE MFLTVAL WITH 'Yes'
          ELSE
            REPLACE MFLTVAL WITH 'No'
          ENDIF
        ELSE  && case of valid entries
          IF EMPTY(TMPUVR.CVLDENTTYP)
            lnSep = AT('~',TMPUVR.MVENTRIES)
            IF lnSep > 0
              lcvEntries = SUBSTR(TMPUVR.MVENTRIES,1,lnSep-1)
              lnvEntries = OCCURS('|',lcvEntries)
              DIMENSION lavEntries[lnvEntries+1]
              lavEntries=''
              =gfSubStr(lcvEntries,@lavEntries,"|")
              REPLACE MFLTVAL WITH lavEntries[EVALUATE(laOgObjType[lnI,2])]
            ENDIF
          ELSE
            lnSep = AT('~',TMPUVR.MVENTRIES)
            IF lnSep > 0
              lcvEntries = SUBSTR(TMPUVR.MVENTRIES,1,lnSep-1)
              REPLACE MFLTVAL WITH EVALUATE(lcvEntries+'['+STR(EVALUATE(laOgObjType[lnI,2]))+']')
            ENDIF
          ENDIF
        ENDIF
      OTHERWISE
        *B609665,1 WAM 08/23/2011 Fix saving fixed filter
        *!*	          lcVarType = TYPE('EVALUATE(laOgObjType[lnI,2])')
        *!*	          DO CASE
        *!*	            CASE lcVarType = 'C'
        *!*	              REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,2])
        *!*	            CASE lcVarType = 'N'
        *!*	              REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,2]))
        *!*	            CASE lcVarType = 'D'
        *!*	              REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,2]))
        *!*	          ENDCASE
        lcVarType = TYPE('EVALUATE(laOgObjType[lnI,1])')
        DO CASE
        CASE lcVarType = 'C'
          REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,1])
        CASE lcVarType = 'N'
          REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,1]))
        CASE lcVarType = 'D'
          REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,1]))
        CASE lcVarType = 'L'
          REPLACE MFLTVAL WITH IIF(EVALUATE(laOgObjType[lnI,1]),'TRUE','FALSE')
        ENDCASE
        *B609665,1 WAM 08/23/2011 Fix saving fixed filter
      ENDCASE
      *-- End Locate for records belong to Aria27 only [Begin]
    ELSE
      llNotFound = .T.
    ENDIF
  ELSE
    llNotFound = .T.
  ENDIF

  *-- If the record found in SYREPUVR [Begin]
  IF llNotFound
    *B609665,1 WAM 08/23/2011 Fix saving fixed filter
    *IF UPPER(laOgObjType[lnI,1]) = 'loOGScroll.laOgFxFlt'
    IF UPPER('laOgFxFlt') $ UPPER(laOgObjType[lnI,1])
      *B609665,1 WAM 08/23/2011 (End)

      lnSep = AT('.',MFLD_NAME)

      *E303030,1 BEGIN
      *IF lnSep > 0 .AND. LEN(MFLD_NAME) > lnSep .AND.;
      *loSYDFIELD.SEEK(PADR(SUBSTR(MFLD_NAME,lnSep+1),10))
      IF lnSep > 0 .AND. LEN(MFLD_NAME) > lnSep .AND.;
          loSYDFIELD.SEEK(PADR(SUBSTR(MFLD_NAME,lnSep+1),oAriaApplication.FieldW))
        *E303030,1 END

        REPLACE CFLD_HEAD WITH SYDFIELD.CFLD_HEAD
        lcVarType = UPPER(ALLTRIM(EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','5]'))))
        DO CASE
        CASE lcVarType = 'LIKE'
          lcVarType = TYPE('EVALUATE(laOgObjType[lnI,1])')
          DO CASE
          CASE lcVarType = 'C'
            REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,1])
          CASE lcVarType = 'N'
            REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,1]))
          CASE lcVarType = 'D'
            REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,1]))
          ENDCASE
        CASE lcVarType = 'BETWEEN'
          lnSep = AT('|',EVALUATE(laOgObjType[lnI,1]))
          IF lnSep > 0 .AND. LEN(EVALUATE(laOgObjType[lnI,1])) > 1
            REPLACE MFLTVAL WITH 'From '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),1,lnSep-1)+;
              ' To '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),lnSep+1)
          ENDIF
        CASE lcVarType = 'IN LIST'
          REPLACE MFLTVAL WITH STRTRAN(EVALUATE(laOgObjType[lnI,1]),'|',',')
        ENDCASE
      ENDIF
    ENDIF
  ENDIF
  =gfAdd_Info(loMRPFLT.lcCursorUpdate)
ENDFOR

LOCAL lcTranCode

*-- Begin Updating Transaction
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '', .T.)

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  RETURN .F.
ENDIF

=loMRPFLT.TABLEUPDATE(lcTranCode)
=loMRPHDR.TABLEUPDATE(lcTranCode)
=loMRPLN.TABLEUPDATE(lcTranCode)

lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode, .T.)
IF lnConnHandler <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
  =oAriaApplication.RemoteSystemData.RollBackTran(lcTranCode, .T.)
  RETURN .F.
ENDIF

STORE .T. TO llSaveMrp
=gfModalGen('TRM36194B00000','DIALOG',lcMrp)

SELECT (lnAlias)
*-- end of lfSaveMrp.

*!
*!*************************************************************
*! Name      : lfvMrpBas
*! Developer : Ahmed Maher - AMH
*! Date      : 04/09/2003
*! Purpose   : Validate the MRP base on option
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvMrpBas()
*!*************************************************************
*!
FUNCTION lfvMrpBas

IF lcRpRName = 'MAMATRQD'
  lcRpReqBas = 'O'
ENDIF

=lfGetBase(lcRpRName='MAMATRQD', llRPTimeP)
loOGScroll.RefreshScroll()

*!
*!*************************************************************
*! Name      : lfPrntMrp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Print the existing Mrp report
*!*************************************************************
*! Example   : =lfPrntMrp()
*!*************************************************************
FUNCTION lfPrntMrp

*--Collecting -ve remaining fabrics variables.
STORE '' TO lcUOMBuy
lnFabcost = 0

llVenRef   = (gfGetMemVar('M_VENREF')='Y')
IF llVenRef
  IF TYPE("loVendMath") # 'O'
    loVendMath = CREATEOBJECT('RemoteTable','VENDMATH','VENDMATH','VENDMATH',SET("Datasession"))
  ENDIF

  IF TYPE("loVendMatl") # 'O'
    loVendMatl = CREATEOBJECT('RemoteTable','VENDMATL','MATCOL','VENDMATL',SET("Datasession"))
  ENDIF
ENDIF

*-- Create cursor to print the first page of MRP report.
lcMrpCurs = loOGScroll.gfTempName()
=lfCrtTmp('C')

SELECT (lcMrpCurs)
SET ORDER TO TAG (lcMrpCurs)

STORE .F. TO llOpenVend,llOpenPfln,llPrintRep
IF TYPE('loApVendor') <> 'O'
  loApVendor = CREATEOBJECT('RemoteTable','APVENDOR','VENCODE','APVENDOR',SET("Datasession"))
ENDIF

IF TYPE('loPofLn') <> 'O'
  loPofLn  = CREATEOBJECT('RemoteTable','POSLN','POSLNS','POFLN',SET("Datasession"))
ENDIF

*-- Get the selected MRPs if any
lcMrpFile = ''
llMrpRang = .F.
lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'MRPHDR.CMRP'),1)
IF lnPosition > 0
  lcMrpFile = loOGScroll.laOgFxFlt[lnPosition,6]
  llMrpRang = IIF(!EMPTY(lcMrpFile) .AND. USED(lcMrpFile) .AND. RECCOUNT(lcMrpFile)>0,.T.,.F.)
  IF llMrpRang
    lcSqlMrps = loOgScroll.gfSQLTempName('','MRP C(6)',lcMrpFile,'CMRP')
  ENDIF
ENDIF

*-- Get the selected Vendor if any
llVendRang = .F.

lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'POFHDR.VENDOR'),1)
IF lnPosition > 0
  lcVendFile = loOGScroll.laOgFxFlt[lnPosition,6]
  llVendRang = IIF(!EMPTY(lcVendFile) .AND. USED(lcVendFile) .AND. RECCOUNT(lcVendFile)>0,.T.,.F.)
  IF llVendRang
    SELECT (lcVendFile)
    =CURSORSETPROP("Buffering",3)
    INDEX ON CVENDCODE TAG &lcVendFile
    SET ORDER TO TAG &lcVendFile
  ENDIF
ENDIF

*-- Get the selected MPos if any
lcMPoFile = ''
llMPoRang = .F.
lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'POFHDR.PO'),1)
IF lnPosition > 0
  lcMPoFile = loOGScroll.laOgFxFlt[lnPosition,6]
  llMPoRang = IIF(!EMPTY(lcMPoFile) .AND. USED(lcMPoFile) .AND. RECCOUNT(lcMPoFile)>0,.T.,.F.)
  IF llMPoRang
    SELECT (lcMPoFile)
    =CURSORSETPROP("Buffering",3)
    INDEX ON PO TAG &lcMPoFile
    SET ORDER TO TAG &lcMPoFile
  ENDIF
ENDIF

IF TYPE("loMrpHdr") # 'O'
  loMrpHdr = CREATEOBJECT('RemoteTable','MRPHDR','MRPHDR','MRPHDR',SET("Datasession"))
ENDIF

IF TYPE("loMrpLn") # 'O'
  loMrpLn = CREATEOBJECT('RemoteTable','MRPLN','MRPLN','MRPLN',SET("Datasession"))
ENDIF

IF TYPE('loPofHdr') <> 'O'
  loPofHdr = CREATEOBJECT('RemoteTable','POSHDR','POSHDR','POFHEADR',SET("Datasession"))
ENDIF

IF !llMrpRang
  lcMrpFile = 'MRPHDR'
ENDIF

loPOFHDR.SetOrder("MRP")

SELECT (lcMrpFile)
LOCATE

loMrpHDr.GoTop()

DO WHILE !EOF(lcMrpFile)
  lcMrp = CMRP

  IF !llMrpRang
    =loMrpHDr.SEEK(CMRP)
  ENDIF

  SELECT (lcMatReq)
  ZAP

  IF llMPoRang
    IF loPOFHDR.SEEK(lcMrp)
      SELECT POFHEADR
      LOCATE REST WHILE CMRP+cBusDocu+cStyType+PO = lcMrp;
        FOR SEEK(PO, lcMPoFile) .AND. IIF(llVendRang, SEEK(VENDOR, lcVendFile), llMPoRang)
      IF FOUND()
        =lfGetMrp(lcMrp)
      ENDIF
    ENDIF
  ELSE
    IF !llVendRang
      =lfGetMrp(lcMrp)
    ELSE
      IF loPOFHDR.SEEK(lcMrp)
        SELECT POFHEADR
        LOCATE REST WHILE CMRP+cBusDocu+cStyType+PO = lcMrp FOR SEEK(VENDOR, lcVendFile)
        IF FOUND()
          =lfGetMrp(lcMrp)
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  *--Print reports.
  SELECT (lcMatReq)
  SET ORDER TO TAG MATREQ

  *--Check if there is requirement.
  IF !llRpZeroRq
    IF lcRpRName = 'MAMATRQD'
      SET FILTER TO !LNOTPRINT
    ELSE
      SET FILTER TO OHReqTot <> 0
    ENDIF
  ENDIF
  GO TOP

  *--Check requirements existance.
  IF !EOF()
    *WAIT WINDOW  'Printing....' NOWAIT
    *--[1] MRP first page.
    IF TYPE("loMrpFlt") # 'O'
      loMrpFlt = CREATEOBJECT('RemoteTable','MRPFLT','MRPFLT','MRPFLT',SET("Datasession"))
    ENDIF
    SELECT (lcMrpCurs)
    ZAP

    loMrpFlt.SEEK(lcMrp)
    SELECT MRPFLT
    SCAN REST WHILE CMRP = lcMrp
      SELECT (lcMrpCurs)
      APPEND BLANK
      REPLACE CTYPE     WITH '1',;
        CFLD_HEAD WITH MRPFLT.CFLD_HEAD,;
        MFLTVAL   WITH MRPFLT.MFLTVAL
    ENDSCAN

    *- Restore order of apvendor file and set to another
    lcOrderVnd = loApVendor.lcTagName
    loApVendor.SetOrder('VENCODE')

    *- Restore order of pofHdr file and set to another
    lcOrderLn = loPofLn.lcTagName
    loPofLn.SetOrder('POSLN')

    *- Restore order of pofln file and set to another
    SELECT POFHEADR
    lcOrderHd = loPOFHDR.lcTagName
    loPOFHDR.SetOrder("MRP")
    IF loPOFHDR.SEEK(lcMrp)
      SCAN REST WHILE CMRP = lcMrp
        loApVendor.SEEK(VENDOR)

        IF loPofLn.SEEK(cBusDocu+cStyType+PO)
          SELECT POFLN
          SCAN REST WHILE cStyType = POFHEADR.cStyType
            SELECT (lcMrpCurs)
            IF !SEEK('2'+POFLN.PO+POFLN.STYLE)
              APPEND BLANK
              REPLACE CTYPE     WITH '2',;
                CPOMAT    WITH POFHEADR.PO,;
                CVENDCODE WITH POFHEADR.VENDOR,;
                CVENCOMP  WITH APVENDOR.CVENCOMP,;
                CFABRIC   WITH POFLN.STYLE


              *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
              *              IF llVenRef .AND. loVendMatL.SEEK(CVENDCODE+SUBSTR(CFABRIC,1,7)+SUBSTR(CFABRIC,lnFClrSrt,lnFClrEnd))
              IF llVenRef .AND. loVendMatL.SEEK(PADR(CVENDCODE,8)+PADR(SUBSTR(CFABRIC,1,lnFMajLen),19)+;
                  PADR(SUBSTR(CFABRIC,lnFClrSrt,lnFClrEnd),6))
                *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]

                REPLACE CVENFAB  WITH VENDMATL.CVENFAB,;
                  CVENCOLR WITH VENDMATL.CVENCOLR
              ENDIF
            ENDIF

            IF POFLN.TRANCD = '1'
              REPLACE NONORDER  WITH NONORDER + POFLN.TOTQTY
            ELSE
              REPLACE NRECEIVED WITH NRECEIVED+ POFLN.TOTQTY
            ENDIF
            REPLACE NPENDING WITH NONORDER - NRECEIVED
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF

    *- Restore order of apvendor file and set to another
    loApVendor.SetOrder(lcOrderVnd)
    loPofLn.SetOrder(lcOrderLn)
    loPOFHDR.SetOrder(lcOrderHd)

    lcCurForm = lcRpRname
    lcRpRname = 'MAMRP'

    =gfCrtFrm(lcRpRname,"",llOGRefForm)
    =lfRepPltFr(lcRpRname)

    SELECT (lcMrpCurs)

    LOCATE
    IF !EOF()
      DO gfDispRe WITH EVALUATE('lcRpRname')
    ENDIF

    lcRpRname = lcCurForm

    *--[2] Fabrics requirement report.
    SELECT (lcMatReq)
    SET ORDER TO TAG MATREQ

    *--Initialize printing report variables.
    STORE 0   TO lnOnHand,lnOnOrdr,lnConv,lnLeadTm,lnYield,lnYield1,lnYield2,;
      lnAvl1,lnAvl2,lnAvl3,lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8
    STORE " " TO lcDescrp,lcFabVen,lcOldItem,lcSDescrp,lcTkTRUn,;
      lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8

    RESTORE FROM MEMO MFLTVAL ADDITIVE
    FOR lnI = 1 TO 20
      &laSaveVar[lnI,1]. = laSaveVar[lnI,2]
    ENDFOR

    SET KEY TO 'F'

    IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
      SET ORDER TO MatreqC
    ENDIF

    IF SEEK('F')
      =gfCrtFrm(lcRpRname,"",llOGRefForm)
      =lfRepPltFr(lcRpRname)
      SELECT (lcMatReq)
      DO gfDispRe WITH EVAL('lcRpRname')
      *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[Start]
      loogscroll.lAdditive = .T.
      *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[END]

    ENDIF

    *--[3] Trims requirement report.
    SELECT (lcMatReq)
    SET KEY TO 'T'
    IF SEEK('T')
      *--Initialize printing variables
      =gfCrtFrm(lcRpRname,"",llOGRefForm)
      =lfRepPltFr(lcRpRname)
      SELECT (lcMatReq)
      =lfInitVar()
      DO gfDispRe WITH EVAL('lcRpRname')
      *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[Start]
      loogscroll.lAdditive = .T.
      *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[END]

    ENDIF

    *--[4] Style components requirement report.
    SELECT (lcMatReq)
    SET KEY TO 'S'
    IF SEEK('S')
      *--Initialize printing variables
      =gfCrtFrm(lcRpRname,"",llOGRefForm)
      =lfRepPltFr(lcRpRname)
      SELECT (lcMatReq)
      =lfInitVar()
      DO gfDispRe WITH EVAL('lcRpRname')
    ENDIF
    *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[Start]
    loogscroll.lAdditive = .F.
    *B609906,1 MMT 05/10/2012 Fix bug of overwritting excel file when export Materail requiremnts report[END]
    SET KEY TO
    STORE .T. TO llPrintRep
  ENDIF

  SELECT (lcMrpFile)

  IF llMrpRang
    SKIP
    LOOP
  ELSE
    IF !loMrp.GoNext()
      EXIT
    ELSE
      LOOP
    ENDIF
  ENDIF
ENDDO

IF !llPrintRep
  *--None of the selected styles have any requirements.
  *--                     [Ok]
  =gfModalGen('TRM00052B00000','DIALOG' )
ENDIF

IF llMPoRang .OR. !EMPTY(lcVendFile)
  loPofHdr.SetOrder("POSHDR")
ENDIF

*--Close all Open Files.
IF USED(lcMatReq)
  USE IN (lcMatReq)
ENDIF
ERASE (oAriaApplication.WorkDir+lcMatReq+'.DBF')
ERASE (oAriaApplication.WorkDir+lcMatReq+'.CDX')

IF USED(lcMrpCurs)
  USE IN (lcMrpCurs)
ENDIF


RETURN
*!
*!*************************************************************
*! Name      : lfGetMrp
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Get data of the MRP report.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfGetMrp
PARAMETERS lcMrp

PRIVATE lnReccount,lnCurNum
IF loMrpHDr.SEEK(lcMrp)
  *-- Restore the memory variables.
  SELECT MRPHDR
  RESTORE FROM MEMO MFLTVAL ADDITIVE
  lcRpMrpBas = 'E'

  *-- Get the lines of MRP report.
  IF loMrpLn.SEEK(lcMrp)
    *--Thermometer counter.
    COUNT ALL TO lnRecCount
    lnCurNum   = 0
    SELECT MRPLN

    SCAN WHILE cMrp = lcMrp
      lnCurNum = lnCurNum + 1
      *-=gfThermo(lnRecCount,lnCurNum ,"Collecting the MRP data",'MRP # : '+lcMrp)
      SCATTER MEMO MEMVAR
      SELECT (lcMatReq)
      APPEND BLANK
      REPLACE STYLE     WITH M.STYLE    ,;
        ITEM      WITH PADR(M.ITEM, IIF(M.CCATGTYP = 'S', lnMajorLn, lnFMajLen)) + IIF(M.CCATGTYP = 'S', lcSeparator, lcFSeparator) + M.ICLR,;
        DESC      WITH M.DESC     ,;
        CWARECODE WITH M.CWARECODE,;
        TYP       WITH M.TYP      ,;
        CCATGTYP  WITH M.CCATGTYP ,;
        CNT       WITH M.CNT      ,;
        UOM       WITH M.UOM

      REPLACE QTY1      WITH M.NQTY1    ,;
        QTY2      WITH M.NQTY2    ,;
        QTY3      WITH M.NQTY3    ,;
        QTY4      WITH M.NQTY4    ,;
        QTY5      WITH M.NQTY5    ,;
        QTY6      WITH M.NQTY6    ,;
        QTY7      WITH M.NQTY7    ,;
        QTY8      WITH M.NQTY8
      REPLACE OREQ1     WITH M.NOREQ1   ,;
        OREQ2     WITH M.NOREQ2   ,;
        OREQ3     WITH M.NOREQ3   ,;
        OREQ4     WITH M.NOREQ4   ,;
        OREQ5     WITH M.NOREQ5   ,;
        OREQ6     WITH M.NOREQ6   ,;
        OREQ7     WITH M.NOREQ7   ,;
        OREQ8     WITH M.NOREQ8   ,;
        OREQTOT   WITH M.NOREQTOT
      REPLACE OHREQ1    WITH M.NOHREQ1  ,;
        OHREQ2    WITH M.NOHREQ2  ,;
        OHREQ3    WITH M.NOHREQ3  ,;
        OHREQ4    WITH M.NOHREQ4  ,;
        OHREQ5    WITH M.NOHREQ5  ,;
        OHREQ6    WITH M.NOHREQ6  ,;
        OHREQ7    WITH M.NOHREQ7  ,;
        OHREQ8    WITH M.NOHREQ8  ,;
        OHREQTOT  WITH M.NOHREQTOT
      REPLACE NPROJ1    WITH M.NPROJ1   ,;
        NPROJ2    WITH M.NPROJ2   ,;
        NPROJ3    WITH M.NPROJ3   ,;
        NPROJ4    WITH M.NPROJ4   ,;
        NPROJ5    WITH M.NPROJ5   ,;
        NPROJ6    WITH M.NPROJ6   ,;
        NPROJ7    WITH M.NPROJ7   ,;
        NPROJ8    WITH M.NPROJ8   ,;
        PROJECT   WITH M.NPROJECT
      REPLACE NOPNWIP   WITH M.NOPNWIP  ,;
        NYTOWIP   WITH M.NYTOWIP  ,;
        NUSEDREQ  WITH M.NUSEDREQ ,;
        NUSEDREQ1  WITH M.NUSEDREQ1 ,;
        NUSEDREQ2  WITH M.NUSEDREQ2 ,;
        NUSEDREQ3  WITH M.NUSEDREQ3 ,;
        NUSEDREQ4  WITH M.NUSEDREQ4 ,;
        NUSEDREQ5  WITH M.NUSEDREQ5 ,;
        NUSEDREQ6  WITH M.NUSEDREQ6 ,;
        NUSEDREQ7  WITH M.NUSEDREQ7 ,;
        NUSEDREQ8  WITH M.NUSEDREQ8 ,;
        NNETREQ   WITH M.NNETREQ  ,;
        LSTYMAKE  WITH M.LSTYMAKE ,;
        NNETREQ1  WITH M.NNETREQ1 ,;
        NNETREQ2  WITH M.NNETREQ2 ,;
        NNETREQ3  WITH M.NNETREQ3 ,;
        NNETREQ4  WITH M.NNETREQ4 ,;
        NNETREQ5  WITH M.NNETREQ5 ,;
        NNETREQ6  WITH M.NNETREQ6 ,;
        NNETREQ7  WITH M.NNETREQ7 ,;
        NNETREQ8  WITH M.NNETREQ8
      REPLACE NOPNWIP1  WITH M.NOPNWIP1 ,;
        NOPNWIP2  WITH M.NOPNWIP2 ,;
        NOPNWIP3  WITH M.NOPNWIP3 ,;
        NOPNWIP4  WITH M.NOPNWIP4 ,;
        NOPNWIP5  WITH M.NOPNWIP5 ,;
        NOPNWIP6  WITH M.NOPNWIP6 ,;
        NOPNWIP7  WITH M.NOPNWIP7 ,;
        NOPNWIP8  WITH M.NOPNWIP8
      REPLACE NYTOWIP1  WITH M.NYTOWIP1 ,;
        NYTOWIP2  WITH M.NYTOWIP2 ,;
        NYTOWIP3  WITH M.NYTOWIP3 ,;
        NYTOWIP4  WITH M.NYTOWIP4 ,;
        NYTOWIP5  WITH M.NYTOWIP5 ,;
        NYTOWIP6  WITH M.NYTOWIP6 ,;
        NYTOWIP7  WITH M.NYTOWIP7 ,;
        NYTOWIP8  WITH M.NYTOWIP8 ,;
        POCT      WITH M.MPoCt    ,;
        MFLTVAL   WITH M.MFLTVAL

      REPLACE CFABLOC   WITH M.CFABLOC  ,;
        CINHOUSE  WITH M.CINHOUSE ,;
        LGETTHIS  WITH M.LGETTHIS ,;
        LBOM      WITH M.LBOM     ,;
        YIELD1    WITH M.NYIELD1  ,;
        YIELD2    WITH M.NYIELD2  ,;
        YIELD3    WITH M.NYIELD3  ,;
        YIELD4    WITH M.NYIELD4  ,;
        YIELD5    WITH M.NYIELD5  ,;
        YIELD6    WITH M.NYIELD6  ,;
        YIELD7    WITH M.NYIELD7  ,;
        YIELD8    WITH M.NYIELD8

      REPLACE ORDER     WITH M.ORDER    ,;
        COMPLETE  WITH M.COMPLETE ,;
        ONHAND    WITH M.ONHAND   ,;
        ONORDER   WITH M.ONORDER  ,;
        ETA       WITH M.ETA      ,;
        LMULTIPO  WITH M.LMULTIPO ,;
        LNOTPRINT WITH M.LNOTPRINT,;
        ITEMSTK   WITH M.ITEMSTK
    ENDSCAN
    *- =gfThermo(100,100)
    SET KEY TO
  ENDIF
ENDIF

*-- end of lfGetMrp.
*!
*!*************************************************************
*! Name      : lfsrvMrp
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the MrpHdr file when browse the Mrps.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*!
FUNCTION lfSrvMrp
PARAMETERS lcParm
DO CASE
CASE lcParm = 'S'  && Set code
  SELECT MRPHDR
  LOCATE
ENDCASE
*-- end of lfsrvMrp.
*!
*!*************************************************************
*! Name      : lfsrvPo
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the PofHdr file when browse the MPOs.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*!
FUNCTION lfSrvPo
PARAMETERS lcParm
DO CASE
CASE lcParm = 'S'  && Set code
  SELECT POSHDR
  LOCATE
ENDCASE
*-- end of lfsrvPof.
*!
*!*************************************************************
*! Name      : lfsrvOrd
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the OrdHdr file when browse the SOs.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*!
FUNCTION lfSrvOrd
PARAMETERS lcParm
DO CASE
CASE lcParm = 'S'  && Set code
  SELECT ORDHDR
  LOCATE
CASE lcParm = 'R'  && Reset code
CASE lcParm = 'V'  && Valid code
ENDCASE
*-- end of lfsrvOrd.
*!
*!*************************************************************
*! Name      : lfPrntDtLn
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/14/2003
*! Purpose   : Print the Detail Line layout
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfPrntDtLn

SET ORDER TO TAG MATREQDT

IF lcRpReqBas = 'W'
  IF llRpByWare
    SET ORDER TO TAG MRFABLOCDT
  ENDIF

  lcCutFile = ''
  llCutRang = .F.
  lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'CUTTKTH.CUTTKT'),1)
  IF lnPosition > 0
    lcCutFile = loOGScroll.laOgFxFlt[lnPosition,6]
    llCutRang = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
  ENDIF

  lcPosFile = ''
  llPosRang = .F.
  lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'POSHDR.PO'),1)
  IF lnPosition > 0
    lcPosFile = loOGScroll.laOgFxFlt[lnPosition,6]
    llPosRang = IIF(!EMPTY(lcPosFile) .AND. USED(lcPosFile) .AND. RECCOUNT(lcPosFile)>0,.T.,.F.)
  ENDIF

  ldCutFrom = {}
  ldCutTo   = {}
  llCutDate = .F.
  lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'CUTTKTH.ENTERED'),1)
  IF lnPosition > 0
    ldCutFrom = CTOD(SUBSTR(loOGScroll.laOgFxFlt[lnPosition,6],1,10))
    ldCutTo   = CTOD(SUBSTR(loOGScroll.laOgFxFlt[lnPosition,6],12))
    llCutDate = !EMPTY(ldCutFrom) .OR. !EMPTY(ldCutTo)
  ENDIF

  ldPosFrom = {}
  ldPosTo   = {}
  llPosDate = .F.
  lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'POSHDR.ENTERED'),1)
  IF lnPosition > 0
    ldPosFrom = CTOD(SUBSTR(loOGScroll.laOgFxFlt[lnPosition,6],1,10))
    ldPosTo   = CTOD(SUBSTR(loOGScroll.laOgFxFlt[lnPosition,6],12))
    llPosDate = !EMPTY(ldPosFrom) .OR. !EMPTY(ldPosTo)
  ENDIF
ELSE
  lcOrdFile = ''
  llOrdRang = .F.
  lnPosition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'ORDHDR.ORDER'),1)
  IF lnPosition > 0
    lcOrdFile = loOGScroll.laOgFxFlt[lnPosition,6]
    llOrdRang = IIF(!EMPTY(lcOrdFile) .AND. USED(lcOrdFile) .AND. RECCOUNT(lcOrdFile)>0,.T.,.F.)
  ENDIF
ENDIF
lcStyleKey = SPACE(38)

*--Thermometer counter.
lnRecCount = RECCOUNT()
lnCurNum   = 0
SCAN
  IF !EMPTY(lcStyleKey) .AND. ITEM+STYLE == lcStyleKey
    LOOP
  ELSE
    lcStyleKey = ITEM+STYLE

    REPLACE LNOTPRINT WITH .T.
  ENDIF
  lnCurNum = lnCurNum + 1
  *-=gfThermo(lnRecCount,lnCurNum ,"Calculating requirements",'Style : '+Style)
  lcStyle = STYLE
  IF lcRpReqBas = 'W'
    =lfGetWipDt()
  ELSE
    =lfGetOrdDt()
  ENDIF
ENDSCAN

WAIT CLEAR

*--Get the OnHand and the OnOrder
STORE .F. TO llOpenPfHd,llOpenPfln,llOpenMfgH,llOpenMfgD

IF TYPE('loPofHdr') <> 'O'
  loPofHdr = CREATEOBJECT('RemoteTable','POSHDR','POSHDR','POFHEADR',SET("Datasession"))
  llOpenPfHd = .T.
ENDIF

IF TYPE('loPofLn') <> 'O'
  loPofLn = CREATEOBJECT('RemoteTable','POSLN','POSLNS','POFLN',SET("Datasession"))
  llOpenPfln = .T.
ENDIF

IF TYPE('loMmfgOrdh') <> 'O'
  loMmfgOrdh = CREATEOBJECT('RemoteTable','POSHDR','POSHDR','MMFGORDH',SET("Datasession"))
  llOpenMfgH = .T.
ENDIF

IF TYPE('loMmfgOrdd') <> 'O'
  loMmfgOrdd = CREATEOBJECT('RemoteTable','POSLN','POSLN','MMFGORDD',SET("Datasession"))
  llOpenMfgD = .T.
ENDIF

*--Open -ve remaining items file.
lcTmpRmain = loOGScroll.gfTempName()
SELECT (lcMatReq)
=lfCrtTmp('R')
SET ORDER TO 1

*--Collecting -ve remaining fabrics variables.
STORE '' TO lcUOMBuy
lnFabcost = 0

llVenRef   = (gfGetMemVar('M_VENREF  ')='Y')
IF llVenRef
  IF TYPE("loVendMatH") # 'O'
    loVendMatH = CREATEOBJECT('RemoteTable','VENDMATH','VENDMATH','VENDMATH',SET("Datasession"))
  ENDIF

  IF TYPE("loVendMatL") # 'O'
    loVendMatL = CREATEOBJECT('RemoteTable','VENDMATL','MATCOL','VENDMATL',SET("Datasession"))
  ENDIF
ENDIF

SELECT (lcMatReq)
lcStyleKey = SPACE(32)

*--Thermometer counter.
lnRecCount = RECCOUNT()
lnCurNum   = 0
lnItemStk  = 0
lcItemKey  = SPACE(25)
IF TYPE('loFabric') <> 'O'
  loFabric = CREATEOBJECT('RemoteTable','ITEM','STYLE','FABRIC',SET("Datasession"))
ENDIF

*-Get cursor from Itemloc containing the Onhand and Onorder qty[Start]
lcQtyStat = "SELECT Style, Sum(Totstk) AS OnHand, Sum(Totwip) AS OnOrder FROM Itemloc (INDEX = STYDYE) WHERE cInvType = '0002' AND Dyelot = '' Group By Style"
lcTempQty = loOGScroll.gfTempName()
lnResult1 = loOGScroll.orda.SqlRun (lcQtyStat,lcTempQty,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))

IF lnResult1 >= 1
  lnBuffering = CURSORGETPROP("Buffering",lcTempQty)
  =CURSORSETPROP("Buffering", 3, lcTempQty)
  SELECT (lcTempQty)
  INDEX ON STYLE TAG &lcTempQty
  SET ORDER TO TAG &lcTempQty
ENDIF

*-Get cursor from Itemloc containing the Onhand and Onorder qty
SELECT (lcMatReq)

SCAN FOR !LNOTPRINT
  lcFabricID = ITEM
  loFabric.SEEK('0002'+lcFabricID)

  =SEEK(lcFabricID, lcTempQty)

  IF ITEM+STYLE <> lcStyleKey
    lcStyleKey = ITEM+STYLE
    ldStart    = {}
    lnOnHand   = &lcTempQty..OnHand

    IF llByFabLoc
      loFabDye.SEEK('0002'+lcFabricID+cFabLoc)
      lnOnHand = FABDYE.TotStk
    ENDIF

    IF llRpByWare .AND. BOM.CCATGTYP $ 'FT' .AND. !EMPTY(CFABLOC)
      loFabDye.SEEK('0002'+lcFabricID+cFabLoc)
      lnOnHand = FABDYE.TotStk
    ENDIF

    llFrstSty = .F.
  ELSE
    ldStart = ldEnd + 1
  ENDIF

  IF ITEM <> lcItemKey
    lcItemKey = ITEM
    lnItemStk = lnOnHand
    llFrstSty = .T.
  ELSE
    lnItemStk = 0
  ENDIF

  lnCurNum = lnCurNum + 1
  *-  =gfThermo(lnRecCount,lnCurNum ,"Calculating OnHand and OnOrder",'Style : '+Style)
  ldEnd    = COMPLETE
  lcPoMat  = SPACE(6)
  llUpdate = .F.
  lnConf = IIF(loUom.SEEK(FABRIC.cConvBuy), Uom.nConf, "")

  IF loPofLn.SEEK('0002' + EVALUATE(lcMatReq+'.ITEM'))
    SELECT POFLN
    lcPoMatKey = SPACE(27)
    ldDelvDate = {}

    SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = '0002' + EVALUATE(lcMatReq+'.ITEM') FOR cStyType $ 'MF'
      IF STYLE+cBusDocu+cStyType+PO # lcPoMatKey
        lcPoMatKey = STYLE+cBusDocu+cStyType+PO
        ldDelvDate = COMPLETE
      ENDIF

      IF loPofHdr.SEEK(cBusDocu+cStyType+PO) .AND. POFHEADR.STATUS = 'O' .AND. POFHEADR.CWARECODE = EVALUATE(lcMatReq+'.CFABLOC');
          .AND. IIF(llRpByWare .AND. !EMPTY(EVALUATE(lcMatReq+'.CFABLOC')),;
          POFHEADR.CWARECODE=EVALUATE(lcMatReq+'.CFABLOC'),.T.)

        IF BETWEEN(IIF(EMPTY(ldDelvDate),POFHEADR.COMPLETE,ldDelvDate),ldStart,ldEnd)
          IF EMPTY(lcPoMat)
            lcPoMat = PO
          ELSE
            IF PO <> lcPoMat
              lcPoMat = '******'
            ENDIF
          ENDIF
          SELECT (lcMatReq)
          llUpdate = .T.
          REPLACE ONHAND   WITH lnOnHand,;
            ONORDER  WITH MAX(ONORDER+(POFLN.TOTQTY*lnConf*IIF(POFLN.TRANCD='1',1,-1)),0),;
            ITEMSTK  WITH lnItemStk+IIF(llFrstSty,ONORDER,0),;
            LMULTIPO WITH (lcPoMat = '******'),;
            ETA      WITH IIF(EMPTY(ldDelvDate),POFHEADR.COMPLETE,ldDelvDate)
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF

  SELECT (lcMatReq)
  IF !llUpdate
    REPLACE ONHAND  WITH lnOnHand,;
      ITEMSTK WITH lnItemStk
  ENDIF
  lnOnHand = ONHAND + ONORDER - NYTOWIP

  IF ORDER = 'ZZZZZ*'
    IF ONORDER = 0
      REPLACE LNOTPRINT WITH .T.
    ENDIF

    IF lnOnHand < 0 .AND. lcRpMrpBas = 'N'
      lcFabVen   = FABRIC.Vendor
      lnConv     = lnConf
      lnLeadTm   = FABRIC.LeadTime
      lnFabcost  = FABRIC.nIcost1
      lcUOMBuy   = IIF(loUom.SEEK(FABRIC.cConvBuy),Uom.cUom_b,"")
      lnRequired = ABS(lnOnHand)
      =lfSavNRItm()
    ENDIF
  ENDIF
ENDSCAN

WAIT CLEAR

SELECT (lcMatReq)

*--Check if there is requirement.
SET FILTER TO !LNOTPRINT
LOCATE

*--Check requirements existance.
IF EOF()
  *--None of the selected styles have any requirements.
  *--                     [Ok]
  =gfModalGen('TRM36108B36000','DIALOG')
  RETURN
ENDIF

SELECT (lcMatReq)
IF lcRpReqBas = 'W'
  IF llRpByWare
    SET ORDER TO TAG MRFABLOCDT
  ELSE
    SET ORDER TO TAG MATREQDT
  ENDIF
ENDIF

*--Print reports.
*WAIT WINDOW 'Printing....' NOWAIT

=lfPrintRep()

IF !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
  =lfSaveMrp()
ENDIF

*--Ceck if there is negative remainig in some materials.
GO TOP IN (lcTmpRmain)
IF !EOF(lcTmpRmain)
  *--Negative remaining quantity on some materials. Create P/Os ?
  *--                        [ Yes / No ]
  lnChoice=gfModalGen('QRM36111B36001','DIALOG')
  IF lnChoice = 1
    IF !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
      =lfSaveMrp()
    ENDIF
    WAIT WINDOW 'Generate P/Os...' NOWAIT
    IF lfGenMPo()
      RETURN
    ENDIF
  ENDIF
ENDIF
USE IN (lcTmpRmain)
ERASE (oAriaApplication.WorkDir+lcTmpRmain+'.DBF')
ERASE (oAriaApplication.WorkDir+lcTmpRmain+'.CDX')

*--Close all Open Files.
IF USED(lcMatReq)
  USE IN (lcMatReq)
ENDIF
ERASE (oAriaApplication.WorkDir+lcMatReq+'.DBF')
ERASE (oAriaApplication.WorkDir+lcMatReq+'.CDX')

*-- end lfPrntDtLn.
*!
*!*************************************************************
*! Name      : lfGetWipDt
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/14/2005
*! Purpose   : Get the wip req. in case of Detail Line layout
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*!
FUNCTION lfGetWipDt

lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYDYE.CWARECODE'),1)
lcLocFile = loOGScroll.laOgFxFlt[lnLocCondition,6]
*-- If Style Location Are selected.
IF !EMPTY(lcLocFile)
  SELECT &lcLocFile
  INDEX ON cWwareCode TAG &lcLocFile
ENDIF
lcForWare = IIF(llByLoctn ," SEEK(cWareCode,lcLocFile) " , ".T." )

PRIVATE lnAlias,llFirst
lnAlias  = SELECT(0)
llFirst  = .T.
llAddRec = .F.

IF loPosLn.SEEK('0001'+lcStyle)
  lcPosNo = SPACE(8)
  SELECT PosLn
  SCAN REST WHILE cInvType+STYLE+cBusDocu+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = '0001' + lcStyle;
      FOR &lcForWare. .AND. !(TRANCD $ '36')
    loPosHdr.SEEK(PosLn.cBusDocu+PosLn.cStyType+PosLn.PO)
    *--Check POs filter.
    IF llPosDate .AND. !BETWEEN(POSHDR.ENTERED,ldPosFrom,ldPosTo)
      LOOP
    ENDIF
    IF POSHDR.STATUS $ 'HOA'
      SELECT (lcMatReq)
      IF llFirst
        llFirst = .F.
        lcPosNo = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO
      ELSE
        IF POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO <> lcPosNo
          lcPosNo = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO
          SCATTER MEMO MEMVAR
          STORE 0 TO M.OREQ1,M.OREQ2,M.OREQ3,M.OREQ4,M.OREQ5,M.OREQ6,M.OREQ7,M.OREQ8,M.OREQTOT
          INSERT INTO (lcMatReq) FROM MEMVAR
        ENDIF
      ENDIF
      lnSign = IIF(POSLN.TRANCD = '1', 1, -1)
      IF !llAddRec
        llAddRec = IIF(llPosRang, SEEK(POSHDR.PO,lcPosFile), !llCutRang)
      ENDIF
      REPLACE ORDER     WITH POSHDR.PO,;
        COMPLETE  WITH POSHDR.ENTERED,;
        LNOTPRINT WITH IIF(llPosRang,!SEEK(POSHDR.PO,lcPosFile),llCutRang),;
        OReq1     WITH MAX(OREQ1+(POSLN.QTY1*lnSign),0),;
        OReq2     WITH MAX(OREQ2+(POSLN.QTY2*lnSign),0),;
        OReq3     WITH MAX(OREQ3+(POSLN.QTY3*lnSign),0),;
        OReq4     WITH MAX(OREQ4+(POSLN.QTY4*lnSign),0),;
        OReq5     WITH MAX(OREQ5+(POSLN.QTY5*lnSign),0),;
        OReq6     WITH MAX(OREQ6+(POSLN.QTY6*lnSign),0),;
        OReq7     WITH MAX(OREQ7+(POSLN.QTY7*lnSign),0),;
        OReq8     WITH MAX(OREQ8+(POSLN.QTY8*lnSign),0),;
        OReqTot   WITH OREQ1+OREQ2+OREQ3+OREQ4+OREQ5+OREQ6+OREQ7+OREQ8,;
        NYTOWIP   WITH (OREQ1*QTY1)+(OREQ2*QTY2)+(OREQ3*QTY3)+(OREQ4*QTY4)+;
        (OREQ5*QTY5)+(OREQ6*QTY6)+(OREQ7*QTY7)+(OREQ8*QTY8)
    ENDIF
  ENDSCAN
ENDIF

IF llAddRec
  =lfAddLstRc()
ELSE
  SELECT (lcMatReq)
  REPLACE LNOTPRINT WITH .T.
ENDIF

SELECT POSLN
SET RELATION TO

SELECT (lnAlias)
*-- end lfGetWipDt.
*!
*!*************************************************************
*! Name      : lfGetOrdDt
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/14/2003
*! Purpose   : Get the wip req. in case of Detail Line layout
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
FUNCTION lfGetOrdDt

PRIVATE lnAlias,llFirst

IF loOrdLine.lcTagName <> 'ORDLINES'
  lcOrdLineTag = loOrdLine.lcTagName
ELSE
  lcOrdLineTag = "ORDLINES"
ENDIF
loOrdLine.SetOrder('ORDLINES')

lnAlias  = SELECT(0)
llFirst  = .T.
llAddRec = .F.

lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYDYE.CWARECODE'),1)
lcLocFile = loOGScroll.laOgFxFlt[lnLocCondition,6]
*-- If Style Location Are selected.
IF !EMPTY(lcLocFile)
  SELECT &lcLocFile
  INDEX ON cWwareCode TAG &lcLocFile
ENDIF
lcForWare = IIF(llByLoctn ," SEEK(cWareCode,lcLocFile) " , ".T." )

lnOrdExp = ASCAN(loOGScroll.laFltExp,'ORDLINE')
IF lnOrdExp > 0
  lnOrdExp = ASUBSCRIPT(loOGScroll.laFltExp,lnOrdExp,1)
  lcOrdLineFlt = loOGScroll.laFltExp[lnOrdExp,2]
ELSE
  lcOrdLineFlt = " .T. "
ENDIF

lcOrdPastFlt = IIF(llRpPastCm," ORDHDR.COMPLETE >=gdSysDate", ".T.")

IF loOrdLine.SEEK(lcStyle)
  SELECT ORDLINE
  SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = lcStyle;
      FOR &lcForWare.
    loOrdHdr.SEEK('O'+OrdLine.ORDER)

    *--Check orders filter.
    IF !EVALUATE(lcOrdLineFlt)
      LOOP
    ENDIF

    IF OrdHdr.STATUS $ IIF(llRpHldOrd,'OH','O') AND &lcOrdPastFlt
      SELECT (lcMatReq)
      IF llFirst
        llFirst = .F.
      ELSE
        SCATTER MEMO MEMVAR
        INSERT INTO (lcMatReq) FROM MEMVAR
      ENDIF

      IF !llAddRec
        llAddRec = IIF(llOrdRang,SEEK(ORDHDR.ORDER,lcOrdFile),.T.)
      ENDIF

      REPLACE ORDER     WITH ORDHDR.ORDER,;
        COMPLETE  WITH ORDHDR.COMPLETE,;
        LNOTPRINT WITH IIF(llOrdRang,!SEEK(ORDHDR.ORDER,lcOrdFile),.F.),;
        OReq1     WITH ORDLINE.QTY1,;
        OReq2     WITH ORDLINE.QTY2,;
        OReq3     WITH ORDLINE.QTY3,;
        OReq4     WITH ORDLINE.QTY4,;
        OReq5     WITH ORDLINE.QTY5,;
        OReq6     WITH ORDLINE.QTY6,;
        OReq7     WITH ORDLINE.QTY7,;
        OReq8     WITH ORDLINE.QTY8,;
        OReqTot   WITH ORDLINE.TOTQTY,;
        NYTOWIP   WITH (OREQ1*QTY1)+(OREQ2*QTY2)+(OREQ3*QTY3)+(OREQ4*QTY4)+;
        (OREQ5*QTY5)+(OREQ6*QTY6)+(OREQ7*QTY7)+(OREQ8*QTY8)
    ENDIF
  ENDSCAN
ENDIF

IF llAddRec
  =lfAddLstRc()
ELSE
  SELECT (lcMatReq)
  REPLACE LNOTPRINT WITH .T.
ENDIF

loOrdLine.SetOrder('&lcOrdLineTag')
SELECT (lnAlias)
*-- end lfGetOrdDt.
*!
*!*************************************************************
*! Name      : lfAddLstRc
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/14/2003
*! Purpose   : Add the last record of style
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfAddLstRc

SELECT (lcMatReq)
IF llRpByWare
  SCATTER FIELDS CCATGTYP,CINHOUSE,CFABLOC,ITEM,STYLE MEMVAR

  SET ORDER TO TAG MRFABLOCDT DESCENDING

  SEEK m.cCatgTyp+m.cInHouse+m.cFabLoc+m.Item+m.Style
ELSE
  SCATTER FIELDS CCATGTYP,ITEM,STYLE MEMVAR

  SET ORDER TO TAG MATREQDT DESCENDING

  SEEK m.cCatgTyp+m.Item+m.Style
ENDIF
SCATTER MEMO MEMVAR
INSERT INTO (lcMatReq) FROM MEMVAR
REPLACE ORDER     WITH 'ZZZZZ*',;
  COMPLETE  WITH COMPLETE+(365.25*400),;
  LNOTPRINT WITH .F.,;
  NYTOWIP   WITH 0,;
  OREQTOT   WITH 0
IF llRpByWare
  SET ORDER TO TAG MRFABLOCDT ASCENDING
ELSE
  SET ORDER TO TAG MATREQDT ASCENDING
ENDIF
*-- end of lfAddLstRc.
*!
*!*************************************************************
*! Name      : lfGetFrmat
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2003
*! Purpose   : Get the report format
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfGetFrmat
LPARAMETERS llTimePhase

IF llTimePhase
  DECLARE laRpFrmDsc[2,1],laRpFrmId[2,1]

  *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
  loogScroll.cCROrientation = 'L'
  *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

  laRpFrmDsc[1,1] = 'Summary'
  laRpFrmDsc[2,1] = 'Detail'

  laRpFrmId[1,1] = 'MATIMREQ'
  laRpFrmId[2,1] = 'MATIMRQD'
ELSE
  DECLARE laRpFrmDsc[IIF(llMrp,4,3),1],laRpFrmId[IIF(llMrp,4,3),1]

  *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
  loogScroll.cCROrientation = 'P'
  *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

  laRpFrmDsc[1,1] = 'Summary'
  laRpFrmDsc[2,1] = 'Detail Short'
  laRpFrmDsc[3,1] = 'Detail Long'
  IF llMrp
    laRpFrmDsc[4,1] = 'Detail Lines'
  ENDIF

  laRpFrmId[1,1] = 'MAMATRQ'
  laRpFrmId[2,1] = 'MAMATRQH'
  laRpFrmId[3,1] = 'MAMATRQL'
  IF llMrp
    laRpFrmId[4,1] = 'MAMATRQD'
  ENDIF

ENDIF

*!
*!*************************************************************
*! Name      : lfGetBase
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/10/2003
*! Purpose   : Get the Requirement Based on popup.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfGetBase
PARAMETERS llDet, llTimePhase

IF llTimePhase
  DECLARE laRpBasDsc[4,1],laRpBasId[4,1]

  laRpBasDsc[1,1] = 'WIP'
  laRpBasDsc[2,1] = 'Plan'
  laRpBasDsc[3,1] = 'Wip+Plan'
  laRpBasDsc[4,1] = 'Forecast'

  laRpBasId[1,1] = 'W'
  laRpBasId[2,1] = 'P'
  laRpBasId[3,1] = 'B'
  laRpBasId[4,1] = 'F'

ELSE
  DECLARE laRpBasDsc[IIF(llDet,2,3),1],laRpBasId[IIF(llDet,2,3),1]

  laRpBasDsc[1,1] = 'OTC'
  laRpBasDsc[2,1] = 'WIP'
  IF !llDet
    laRpBasDsc[3,1] = 'Both'
  ENDIF

  laRpBasId[1,1] = 'O'
  laRpBasId[2,1] = 'W'
  IF !llDet
    laRpBasId[3,1] = 'B'
  ENDIF
ENDIF

*!
*!*************************************************************
*! Name      : lfvTimePhase
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Valid function for the Print Time Phase Option
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfvTimePhase

IF llRPTimeP
  lcRpRName  = 'MATIMREQ'
  lcRpReqBas = 'F'
ELSE
  lcRpRName  = 'MAMATRQ'
  lcRpReqBas = 'B'
ENDIF

=lfGetFrmat(llRPTimeP)
=lfGetBase(lcRpRName='MAMATRQD', llRPTimeP)

loOGScroll.RefreshScroll()

*!
*!*************************************************************
*! Name      : lfSRVFab
*: Developer : AHMED MAHER (AMH)
*! Date      : 06/29/2003
*! Purpose   : control browsing primary fabric and validate
*!           : selecting it in inlist function.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
*!
FUNCTION lfSRVFab
PARAMETERS lcParm

PRIVATE lcAlias,llHaveSty
DO CASE
CASE lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to primary fabric
  *-- unique index.
  USE (oAriaApplication.DataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
  SELECT FABRIC
  SET ORDER TO TAG cFabric
  SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
  GO TOP IN FABRIC
CASE lcParm = 'R'  && Reset code
  USE IN FABRIC_X
  SELECT FABRIC
  SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.
*!
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*!
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
LOCAL lnAlias
lnAlias = SELECT()
lnTotcomp = 0

SELECT(lcTmpFab)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcTmpFab)
  LOCATE
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE Fabric = lcFab
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN INT(lnTotcomp)
*!
*!*************************************************************
*! Name      : lfSegInfo
*! Developer : Nader Nabil (NNA)
*! Date      : 09/09/2004
*! Purpose   : Check if The item code Structure found or not.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns : False or True
*!*************************************************************
*!
FUNCTION lfSegInfo
lnAlias=SELECT()

PRIVATE loIciStru
IF TYPE('loIciStru') <> 'O'
  loIciStru = CREATEOBJECT('RemoteTable','ICISTRU','Segno','ICISTRU',SET("Datasession"))
ENDIF

IF !loIciStru.SEEK('U1')
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  llOgtrmnat = .T.
  ClearRead()
ENDIF
SELECT(lnalias)
RETURN
*-- End of function lfSegInfo.
*!
*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Heba Fathi
*! Date      : 09/01/2004
*! Purpose   : Create Temp. Files Structure.
*!*************************************************************
*! Parameters : 'R' -- > lcTmpRmain File
*!              'M' -- > lcMatReq   File
*!              'C' -- > lcMrpCurs  File
*!              'P' -- > lcPOTmpHD  File
*!*************************************************************
*!
FUNCTION lfCrtTmp
PARAMETERS lcType

lnAlias = SELECT(0)
DO CASE
CASE lcType = 'R'
  DIMENSION laTmpRmain[35,4]

  laTmpRmain[1,1] = 'Fabric'
  laTmpRmain[1,2] = 'C'
  laTmpRmain[1,3] = 19
  laTmpRmain[1,4] = 0

  laTmpRmain[2,1] = 'DEMO'  && Delete this field
  laTmpRmain[2,2] = 'C'
  laTmpRmain[2,3] = 6
  laTmpRmain[2,4] = 0

  laTmpRmain[3,1] = 'Vendor'
  laTmpRmain[3,2] = 'C'
  laTmpRmain[3,3] = 8
  laTmpRmain[3,4] = 0

  laTmpRmain[4,1] = 'nFabTotQty'
  laTmpRmain[4,2] = 'N'
  laTmpRmain[4,3] = 11
  laTmpRmain[4,4] = 3

  laTmpRmain[5,1] = 'LeadTime'
  laTmpRmain[5,2] = 'N'
  laTmpRmain[5,3] = 3
  laTmpRmain[5,4] = 0

  laTmpRmain[6,1] = 'Complete'
  laTmpRmain[6,2] = 'D'
  laTmpRmain[6,3] = 8
  laTmpRmain[6,4] = 0

  laTmpRmain[7,1] = 'UOMBuy'
  laTmpRmain[7,2] = 'C'
  laTmpRmain[7,3] = 3
  laTmpRmain[7,4] = 0

  laTmpRmain[8,1] = 'cFabGrade'
  laTmpRmain[8,2] = 'C'
  laTmpRmain[8,3] = 1
  laTmpRmain[8,4] = 0

  laTmpRmain[9,1] = 'nfabcost'
  laTmpRmain[9,2] = 'N'
  laTmpRmain[9,3] = 9
  laTmpRmain[9,4] = 3

  laTmpRmain[10,1] = 'cPriceCur'
  laTmpRmain[10,2] = 'C'
  laTmpRmain[10,3] = 3
  laTmpRmain[10,4] = 0

  laTmpRmain[11,1] = 'nPriceRat'
  laTmpRmain[11,2] = 'N'
  laTmpRmain[11,3] = 9
  laTmpRmain[11,4] = 4

  laTmpRmain[12,1] = 'nCurrUnit'
  laTmpRmain[12,2] = 'N'
  laTmpRmain[12,3] = 6
  laTmpRmain[12,4] = 0

  laTmpRmain[13,1] = 'cDutyCur'
  laTmpRmain[13,2] = 'C'
  laTmpRmain[13,3] = 3
  laTmpRmain[13,4] = 0

  laTmpRmain[14,1] = 'nDutyRat'
  laTmpRmain[14,2] = 'N'
  laTmpRmain[14,3] = 9
  laTmpRmain[14,4] = 4

  laTmpRmain[15,1] = 'nDCurUnit'
  laTmpRmain[15,2] = 'N'
  laTmpRmain[15,3] = 6
  laTmpRmain[15,4] = 0

  laTmpRmain[16,1] = 'nItm_Frt'
  laTmpRmain[16,2] = 'N'
  laTmpRmain[16,3] = 9
  laTmpRmain[16,4] = 3

  laTmpRmain[17,1] = 'nItem_Tax'
  laTmpRmain[17,2] = 'N'
  laTmpRmain[17,3] = 9
  laTmpRmain[17,4] = 3

  laTmpRmain[18,1] = 'nItemQuota'
  laTmpRmain[18,2] = 'N'
  laTmpRmain[18,3] = 9
  laTmpRmain[18,4] = 3

  laTmpRmain[19,1] = 'cWareCode'
  laTmpRmain[19,2] = 'C'
  laTmpRmain[19,3] = 6
  laTmpRmain[19,4] = 0

  laTmpRmain[20,1] = 'dDelivDate'
  laTmpRmain[20,2] = 'D'
  laTmpRmain[20,3] = 8
  laTmpRmain[20,4] = 0

  laTmpRmain[21,1] = 'cVenFab'
  laTmpRmain[21,2] = 'C'
  laTmpRmain[21,3] = 10
  laTmpRmain[21,4] = 0

  laTmpRmain[22,1] = 'cVenColr'
  laTmpRmain[22,2] = 'C'
  laTmpRmain[22,3] = 10
  laTmpRmain[22,4] = 0

  laTmpRmain[23,1] = 'Available'
  laTmpRmain[23,2] = 'D'
  laTmpRmain[23,3] = 8
  laTmpRmain[23,4] = 0

  *WSH
  *gfCrtTmp(lcTmpRmain,@laTmpRmain,"Fabric+DTOC(Complete)",lcTmpRmain,.F.)
  laTmpRmain[24,1] = 'cCatGTyp'
  laTmpRmain[24,2] = 'C'
  laTmpRmain[24,3] = 1
  laTmpRmain[24,4] = 0

  FOR lnI = 1 TO 8
    laTmpRmain[lnI+24,1] = 'nQty' + ALLTRIM(STR(lnI))
    laTmpRmain[lnI+24,2] = 'N'
    laTmpRmain[lnI+24,3] = 11
    laTmpRmain[lnI+24,4] = 3
  ENDFOR

  laTmpRmain[33,1] = 'nFCost5'
  laTmpRmain[33,2] = 'N'
  laTmpRmain[33,3] = 9
  laTmpRmain[33,4] = 2

  laTmpRmain[34,1] = 'nFCost6'
  laTmpRmain[34,2] = 'N'
  laTmpRmain[34,3] = 9
  laTmpRmain[34,4] = 3

  laTmpRmain[35,1] = 'nFCost7'
  laTmpRmain[35,2] = 'N'
  laTmpRmain[35,3] = 9
  laTmpRmain[35,4] = 3

  gfCrtTmp(lcTmpRmain,@laTmpRmain,"cCatGTyp+Fabric+DTOC(Complete)",lcTmpRmain,.F.)
  *WSH
CASE lcType = 'M'
  DIMENSION laFlSruc[104,4]
  *--First array element [Name].
  laFlSruc[1,1] = "STYLE"
  laFlSruc[2,1] = "ITEM"
  laFlSruc[3,1] = "ICLR"   && WSH ... Don't need this field
  laFlSruc[4,1] = "DESC"
  laFlSruc[5,1] = "CWARECODE"
  laFlSruc[6,1] = "TYP"
  laFlSruc[7,1] = "CCATGTYP"
  laFlSruc[8,1] = "CNT"
  laFlSruc[9,1] = "UOM"
  laFlSruc[10,1]= "QTY1"
  laFlSruc[11,1]= "QTY2"
  laFlSruc[12,1]= "QTY3"
  laFlSruc[13,1]= "QTY4"
  laFlSruc[14,1]= "QTY5"
  laFlSruc[15,1]= "QTY6"
  laFlSruc[16,1]= "QTY7"
  laFlSruc[17,1]= "QTY8"
  laFlSruc[18,1]= "OREQ1"
  laFlSruc[19,1]= "OREQ2"
  laFlSruc[20,1]= "OREQ3"
  laFlSruc[21,1]= "OREQ4"
  laFlSruc[22,1]= "OREQ5"
  laFlSruc[23,1]= "OREQ6"
  laFlSruc[24,1]= "OREQ7"
  laFlSruc[25,1]= "OREQ8"
  laFlSruc[26,1]= "OREQTOT"
  laFlSruc[27,1]= "OHREQ1"
  laFlSruc[28,1]= "OHREQ2"
  laFlSruc[29,1]= "OHREQ3"
  laFlSruc[30,1]= "OHREQ4"
  laFlSruc[31,1]= "OHREQ5"
  laFlSruc[32,1]= "OHREQ6"
  laFlSruc[33,1]= "OHREQ7"
  laFlSruc[34,1]= "OHREQ8"
  laFlSruc[35,1]= "OHREQTOT"
  laFlSruc[36,1]= "NPROJ1"
  laFlSruc[37,1]= "NPROJ2"
  laFlSruc[38,1]= "NPROJ3"
  laFlSruc[39,1]= "NPROJ4"
  laFlSruc[40,1]= "NPROJ5"
  laFlSruc[41,1]= "NPROJ6"
  laFlSruc[42,1]= "NPROJ7"
  laFlSruc[43,1]= "NPROJ8"
  laFlSruc[44,1]= "PROJECT"
  laFlSruc[45,1]= "NOPNWIP"
  laFlSruc[46,1]= "NYTOWIP"
  laFlSruc[47,1]= "NUSEDREQ"
  laFlSruc[48,1]= "NNETREQ"
  laFlSruc[49,1]= "LSTYMAKE"
  laFlSruc[50,1]= "NNETREQ1"
  laFlSruc[51,1]= "NNETREQ2"
  laFlSruc[52,1]= "NNETREQ3"
  laFlSruc[53,1]= "NNETREQ4"
  laFlSruc[54,1]= "NNETREQ5"
  laFlSruc[55,1]= "NNETREQ6"
  laFlSruc[56,1]= "NNETREQ7"
  laFlSruc[57,1]= "NNETREQ8"
  laFlSruc[58,1]= "NOPNWIP1"
  laFlSruc[59,1]= "NOPNWIP2"
  laFlSruc[60,1]= "NOPNWIP3"
  laFlSruc[61,1]= "NOPNWIP4"
  laFlSruc[62,1]= "NOPNWIP5"
  laFlSruc[63,1]= "NOPNWIP6"
  laFlSruc[64,1]= "NOPNWIP7"
  laFlSruc[65,1]= "NOPNWIP8"
  laFlSruc[66,1]= "NYTOWIP1"
  laFlSruc[67,1]= "NYTOWIP2"
  laFlSruc[68,1]= "NYTOWIP3"
  laFlSruc[69,1]= "NYTOWIP4"
  laFlSruc[70,1]= "NYTOWIP5"
  laFlSruc[71,1]= "NYTOWIP6"
  laFlSruc[72,1]= "NYTOWIP7"
  laFlSruc[73,1]= "NYTOWIP8"

  *-- add a memo filed to hold the CT/Po that already calculate their issued qty
  laFlSruc[74,1] = "PoCt"
  laFlSruc[74,2] = "M"
  laFlSruc[74,3] = "10"
  laFlSruc[74,4] = "0"

  *-- Add New fieldS to save the fabric location [Start]
  laFlSruc[75,1] = "CFABLOC"
  laFlSruc[75,2] = "C"
  laFlSruc[75,3] = "6"
  laFlSruc[75,4] = "0"

  laFlSruc[76,1] = "CINHOUSE"
  laFlSruc[76,2] = "C"
  laFlSruc[76,3] = "1"
  laFlSruc[76,4] = "0"

  laFlSruc[77,1] = "LGETTHIS"
  laFlSruc[77,2] = "L"
  laFlSruc[77,3] = "1"
  laFlSruc[77,4] = "0"

  laFlSruc[78,1] = "LBOM"
  laFlSruc[78,2] = "L"
  laFlSruc[78,3] = "1"
  laFlSruc[78,4] = "0"
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laFlSruc[78+lnI,1] = 'YIELD'+lcI
    laFlSruc[78+lnI,2] = 'N'
    laFlSruc[78+lnI,3] = 7
    laFlSruc[78+lnI,4] = 3
  ENDFOR

  *-- Add new field to save memory variables
  laFlSruc[87,1] = "MFltVal"
  laFlSruc[87,2] = "M"
  laFlSruc[87,3] = "10"
  laFlSruc[87,4] = "0"

  laFlSruc[88,1] = "ORDER"
  laFlSruc[88,2] = "C"
  laFlSruc[88,3] = "6"
  laFlSruc[88,4] = "0"

  laFlSruc[89,1] = "COMPLETE"
  laFlSruc[89,2] = "D"
  laFlSruc[89,3] = "8"
  laFlSruc[89,4] = "0"

  laFlSruc[90,1] = "ONHAND"
  laFlSruc[90,2] = "N"
  laFlSruc[90,3] = "12"
  laFlSruc[90,4] = "3"

  laFlSruc[91,1] = "ONORDER"
  laFlSruc[91,2] = "N"
  laFlSruc[91,3] = "12"
  laFlSruc[91,4] = "3"

  laFlSruc[92,1] = "ETA"
  laFlSruc[92,2] = "D"
  laFlSruc[92,3] = "8"
  laFlSruc[92,4] = "0"

  laFlSruc[93,1] = "LMULTIPO"
  laFlSruc[93,2] = "L"
  laFlSruc[93,3] = "1"
  laFlSruc[93,4] = "0"

  laFlSruc[94,1] = "LNOTPRINT"
  laFlSruc[94,2] = "L"
  laFlSruc[94,3] = "1"
  laFlSruc[94,4] = "0"

  laFlSruc[95,1] = "ITEMSTK"
  laFlSruc[95,2] = "N"
  laFlSruc[95,3] = "12"
  laFlSruc[95,4] = "3"

  laFlSruc[96,1] = "DEMO"    && Remove this field
  laFlSruc[96,2] = "C"
  laFlSruc[96,3] = "35"
  laFlSruc[96,4] = "0"

  laFlSruc[97,1]= "NUSEDREQ1"
  laFlSruc[98,1]= "NUSEDREQ2"
  laFlSruc[99,1]= "NUSEDREQ3"
  laFlSruc[100,1]= "NUSEDREQ4"
  laFlSruc[101,1]= "NUSEDREQ5"
  laFlSruc[102,1]= "NUSEDREQ6"
  laFlSruc[103,1]= "NUSEDREQ7"
  laFlSruc[104,1]= "NUSEDREQ8"

  *--Second array element [Type].
  STORE "C" TO laFlSruc[1,2] ,laFlSruc[2,2] ,laFlSruc[3,2] ,laFlSruc[4,2],;
    laFlSruc[5,2] ,laFlSruc[6,2] ,laFlSruc[7,2] ,laFlSruc[9,2]
  STORE "N" TO laFlSruc[8,2] ,laFlSruc[10,2],laFlSruc[11,2],laFlSruc[12,2],;
    laFlSruc[13,2],laFlSruc[14,2],laFlSruc[15,2],laFlSruc[16,2],;
    laFlSruc[17,2],laFlSruc[18,2],laFlSruc[19,2],laFlSruc[20,2],;
    laFlSruc[21,2],laFlSruc[22,2],laFlSruc[23,2],laFlSruc[24,2],;
    laFlSruc[25,2],laFlSruc[26,2],laFlSruc[27,2],laFlSruc[28,2],;
    laFlSruc[29,2],laFlSruc[30,2],laFlSruc[31,2],laFlSruc[32,2],;
    laFlSruc[33,2],laFlSruc[34,2],laFlSruc[35,2],laFlSruc[36,2],;
    laFlSruc[37,2],laFlSruc[38,2],laFlSruc[39,2],laFlSruc[40,2],;
    laFlSruc[41,2],laFlSruc[42,2],laFlSruc[43,2],laFlSruc[44,2],;
    laFlSruc[45,2],laFlSruc[46,2],laFlSruc[47,2],laFlSruc[48,2],;
    laFlSruc[97,2],laFlSruc[98,2],laFlSruc[99,2],laFlSruc[100,2],;
    laFlSruc[101,2],laFlSruc[102,2],laFlSruc[103,2],laFlSruc[104,2]
  STORE "L" TO laFlSruc[49,2]
  STORE "N" TO laFlSruc[50,2],laFlSruc[51,2],laFlSruc[52,2],laFlSruc[53,2],;
    laFlSruc[54,2],laFlSruc[55,2],laFlSruc[56,2],laFlSruc[57,2],;
    laFlSruc[58,2],laFlSruc[59,2],laFlSruc[60,2],laFlSruc[61,2],;
    laFlSruc[62,2],laFlSruc[63,2],laFlSruc[64,2],laFlSruc[65,2],;
    laFlSruc[66,2],laFlSruc[67,2],laFlSruc[68,2],laFlSruc[69,2],;
    laFlSruc[70,2],laFlSruc[71,2],laFlSruc[72,2],laFlSruc[73,2]
  *--Thered array element [Length].
  STORE  1  TO laFlSruc[6,3] ,laFlSruc[7,3] ,laFlSruc[8,3],laFlSruc[49,3]
  STORE  3  TO laFlSruc[9,3]
  STORE  6  TO laFlSruc[3,3] ,laFlSruc[5,3] ,laFlSruc[18,3],laFlSruc[19,3],;
    laFlSruc[20,3],laFlSruc[21,3],laFlSruc[22,3],laFlSruc[23,3],;
    laFlSruc[24,3],laFlSruc[25,3],laFlSruc[27,3],laFlSruc[28,3],;
    laFlSruc[29,3],laFlSruc[30,3],laFlSruc[31,3],laFlSruc[32,3],;
    laFlSruc[33,3],laFlSruc[34,3],laFlSruc[36,3],laFlSruc[37,3],;
    laFlSruc[38,3],laFlSruc[39,3],laFlSruc[40,3],laFlSruc[41,3],;
    laFlSruc[42,3],laFlSruc[43,3]
  STORE  7  TO laFlSruc[10,3],laFlSruc[11,3],laFlSruc[12,3],laFlSruc[13,3],;
    laFlSruc[14,3],laFlSruc[15,3],laFlSruc[16,3],laFlSruc[17,3],;
    laFlSruc[26,3],laFlSruc[35,3],laFlSruc[44,3],laFlSruc[45,3],;
    laFlSruc[58,3],laFlSruc[59,3],laFlSruc[60,3],laFlSruc[61,3],;
    laFlSruc[62,3],laFlSruc[63,3],laFlSruc[64,3],laFlSruc[65,3],;
    laFlSruc[66,3],laFlSruc[67,3],laFlSruc[68,3],laFlSruc[69,3],;
    laFlSruc[70,3],laFlSruc[71,3],laFlSruc[72,3],laFlSruc[73,3]

  STORE 19  TO laFlSruc[1,3] ,laFlSruc[2,3]
  STORE 12  TO laFlSruc[46,3],laFlSruc[47,3],laFlSruc[48,3],laFlSruc[57,3],;
    laFlSruc[50,3],laFlSruc[51,3],laFlSruc[52,3],laFlSruc[53,3],;
    laFlSruc[54,3],laFlSruc[55,3],laFlSruc[56,3],;
    laFlSruc[97,3],laFlSruc[98,3],laFlSruc[99,3],laFlSruc[100,3],;
    laFlSruc[101,3],laFlSruc[102,3],laFlSruc[103,3],laFlSruc[104,3]

  STORE 20  TO laFlSruc[4,3]

  *--Forth array element [Decemal].
  STORE  0  TO laFlSruc[1,4] ,laFlSruc[2,4] ,laFlSruc[3,4] ,laFlSruc[4,4], ;
    laFlSruc[5,4] ,laFlSruc[6,4] ,laFlSruc[7,4] ,laFlSruc[8,4], ;
    laFlSruc[9,4] ,laFlSruc[18,4],laFlSruc[19,4],laFlSruc[20,4],;
    laFlSruc[21,4],laFlSruc[22,4],laFlSruc[23,4],laFlSruc[24,4],;
    laFlSruc[25,4],laFlSruc[26,4],laFlSruc[27,4],laFlSruc[28,4],;
    laFlSruc[29,4],laFlSruc[30,4],laFlSruc[31,4],laFlSruc[32,4],;
    laFlSruc[33,4],laFlSruc[34,4],laFlSruc[35,4],laFlSruc[36,4],;
    laFlSruc[37,4],laFlSruc[38,4],laFlSruc[39,4],laFlSruc[40,4],;
    laFlSruc[41,4],laFlSruc[42,4],laFlSruc[43,4],laFlSruc[44,4],;
    laFlSruc[45,4],laFlSruc[49,4],laFlSruc[64,4],laFlSruc[65,4],;
    laFlSruc[58,4],laFlSruc[59,4],laFlSruc[60,4],laFlSruc[61,4],;
    laFlSruc[62,4],laFlSruc[63,4]

  STORE  3  TO laFlSruc[10,4],laFlSruc[11,4],laFlSruc[12,4],laFlSruc[13,4],;
    laFlSruc[14,4],laFlSruc[15,4],laFlSruc[16,4],laFlSruc[17,4],;
    laFlSruc[46,4],laFlSruc[47,4],laFlSruc[48,4],laFlSruc[73,4],;
    laFlSruc[50,4],laFlSruc[51,4],laFlSruc[52,4],laFlSruc[53,4],;
    laFlSruc[54,4],laFlSruc[55,4],laFlSruc[56,4],laFlSruc[57,4],;
    laFlSruc[66,4],laFlSruc[67,4],laFlSruc[68,4],laFlSruc[69,4],;
    laFlSruc[70,4],laFlSruc[71,4],laFlSruc[72,4],;
    laFlSruc[97,4],laFlSruc[98,4],laFlSruc[99,4],laFlSruc[100,4],;
    laFlSruc[101,4],laFlSruc[102,4],laFlSruc[103,4],laFlSruc[104,4]

  *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
  nRows    = ALEN(laFlSruc,1)
  nColumns = ALEN(laFlSruc,2)
  DIMENSION laFlSruc[nRows + 9,nColumns]
  nCurrent_Row = nRows

  &&1.1 Fabric OnHold: That gets its value from the variable lnOnHand
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOnHand"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.2 Fabric OnOrder: That gets its value from the variable lnOnOrdr
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOnOrdr"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.3 Fabric Available: That gets its value from the variable lnOnHand + lnOnOrdr
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOnAvlb"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.4 Required Open Orders: that gets its value by calling the function lfVReq('O')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOReq"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.5 Required (Open + Hold orders): that gets its value by calling the function lfVReq('H')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nHReq"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.6 Required Projected: that gets its value by calling the function lfVReq ('P')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nPReq"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.7 Remain Open Orders: that gets its value by calling the function lfRemain('O')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nORemin"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.8 Remain (Open + Hold orders): that gets its value by calling the function lfRemain('H')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nHRemin"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.9 Remain Projected: that gets its value by calling the function lfRemain('P')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nPRemin"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]


  gfCrtTmp(lcMatReq,@laFlSruc,,"",.F.)
  SELECT &lcMatReq

  INDEX ON cCatgTyp+ITEM+STYLE TAG Matreq ADDITIVE
  INDEX ON STYLE UNIQUE TAG Mrstyle ADDITIVE
  INDEX ON STYLE+cCatgTyp+ITEM TAG Mrstyitm ADDITIVE

  *-- Add New Index to sort by fabric location
  INDEX ON CCATGTYP+CINHOUSE+CFABLOC+ITEM+STYLE TAG MRFABLOC ADDITIVE

  *-- Add New Index for Detail line layout
  INDEX ON CCATGTYP+ITEM+STYLE+DTOS(COMPLETE)+ORDER TAG MATREQDT ADDITIVE
  INDEX ON CCATGTYP+CINHOUSE+CFABLOC+ITEM+STYLE+DTOS(COMPLETE)+ORDER TAG MRFABLOCDT ADDITIVE

  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    INDEX ON cCatgTyp+ITEM+STYLE TAG MatreqC ADDITIVE
  ENDIF

CASE lcType = 'T'
  DIMENSION laFlSruc[55,4]

  *--First array element [Name].
  laFlSruc[1,1] = "STYLE"
  laFlSruc[2,1] = "ITEM"
  laFlSruc[3,1] = "DESC"
  laFlSruc[4,1] = "CWARECODE"
  laFlSruc[5,1] = "TYP"
  laFlSruc[6,1] = "CCATGTYP"
  laFlSruc[7,1] = "CNT"
  laFlSruc[8,1] = "nLineNo"

  laFlSruc[9,1] = "CURRHND"
  laFlSruc[10,1]= "CURRORD"
  laFlSruc[11,1]= "CURRREQ"
  laFlSruc[12,1]= "CURRBAL"

  laFlSruc[13,1]= "ONHND1"
  laFlSruc[14,1]= "ONORD1"
  laFlSruc[15,1]= "ONREQ1"
  laFlSruc[16,1]= "ONBAL1"

  laFlSruc[17,1]= "ONHND2"
  laFlSruc[18,1]= "ONORD2"
  laFlSruc[19,1]= "ONREQ2"
  laFlSruc[20,1]= "ONBAL2"

  laFlSruc[21,1]= "ONHND3"
  laFlSruc[22,1]= "ONORD3"
  laFlSruc[23,1]= "ONREQ3"
  laFlSruc[24,1]= "ONBAL3"

  laFlSruc[25,1]= "ONHND4"
  laFlSruc[26,1]= "ONORD4"
  laFlSruc[27,1]= "ONREQ4"
  laFlSruc[28,1]= "ONBAL4"

  laFlSruc[29,1]= "ONHND5"
  laFlSruc[30,1]= "ONORD5"
  laFlSruc[31,1]= "ONREQ5"
  laFlSruc[32,1]= "ONBAL5"

  laFlSruc[33,1]= "ONHND6"
  laFlSruc[34,1]= "ONORD6"
  laFlSruc[35,1]= "ONREQ6"
  laFlSruc[36,1]= "ONBAL6"

  laFlSruc[37,1]= "ONHND7"
  laFlSruc[38,1]= "ONORD7"
  laFlSruc[39,1]= "ONREQ7"
  laFlSruc[40,1]= "ONBAL7"

  laFlSruc[41,1]= "ONHND8"
  laFlSruc[42,1]= "ONORD8"
  laFlSruc[43,1]= "ONREQ8"
  laFlSruc[44,1]= "ONBAL8"

  laFlSruc[45,1]= "ONHND9"
  laFlSruc[46,1]= "ONORD9"
  laFlSruc[47,1]= "ONREQ9"
  laFlSruc[48,1]= "ONBAL9"

  laFlSruc[49,1]= "TOTAL"

  LOCAL lnI
  FOR lnI = 1 TO 7
    laFlSruc[lnI,2] = 'C'
    laFlSruc[lnI,4] = 0
  ENDFOR

  laFlSruc[8,2] = 'N'
  laFlSruc[8,3] = 4
  laFlSruc[8,4] = 0

  FOR lnI = 9 TO 49
    laFlSruc[lnI,2] = 'N'
    laFlSruc[lnI,3] = 12
    laFlSruc[lnI,4] = 3
  ENDFOR

  laFlSruc[50,1]= "CRECTYPE"
  laFlSruc[50,2]= "C"
  laFlSruc[50,3]= 1
  laFlSruc[50,4]= 0

  laFlSruc[51,1]= "CTYPE"
  laFlSruc[51,2]= "C"
  laFlSruc[51,3]= 7
  laFlSruc[51,4]= 0

  laFlSruc[52,1]= "CTRANCD"
  laFlSruc[52,2]= "C"
  laFlSruc[52,3]= 6
  laFlSruc[52,4]= 0

  laFlSruc[53,1]= "DDATE"
  laFlSruc[53,2]= "D"
  laFlSruc[53,3]= 8
  laFlSruc[53,4]= 0

  laFlSruc[54,1]= "CREF"
  laFlSruc[54,2]= "C"
  laFlSruc[54,3]= 16
  laFlSruc[54,4]= 0

  laFlSruc[55,1]= "CSTYLES"
  laFlSruc[55,2]= "C"
  laFlSruc[55,3]= 16
  laFlSruc[55,4]= 0

  *--Third array element [Length].
  STORE 19 TO laFlSruc[1,3], laFlSruc[2,3]
  STORE 20 TO laFlSruc[3,3]
  STORE  6 TO laFlSruc[4,3]
  STORE 1  TO laFlSruc[5,3], laFlSruc[6,3], laFlSruc[7,3]
  STORE 3  TO laFlSruc[8,3]
  *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
  nRows    = ALEN(laFlSruc,1)
  nColumns = ALEN(laFlSruc,2)
  DIMENSION laFlSruc[nRows + 9,nColumns]
  nCurrent_Row = nRows

  &&1.1 Fabric OnHold: That gets its value from the variable lnOnHand
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOnHand"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.2 Fabric OnOrder: That gets its value from the variable lnOnOrdr
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOnOrdr"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.3 Fabric Available: That gets its value from the variable lnOnHand + lnOnOrdr
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOnAvlb"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.4 Required Open Orders: that gets its value by calling the function lfVReq('O')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nOReq"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.5 Required (Open + Hold orders): that gets its value by calling the function lfVReq('H')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nHReq"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.6 Required Projected: that gets its value by calling the function lfVReq ('P')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nPReq"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.7 Remain Open Orders: that gets its value by calling the function lfRemain('O')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nORemin"
  laFlSruc[nCurrent_Row,2]= "N"
  *B610568,1 TMI 10/29/2013 [Start]   correct the field width and decimals
  *laFlSruc[nCurrent_Row,3]= 203
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3
  *B610568,1 TMI 10/29/2013 [End  ] 
  

  &&1.8 Remain (Open + Hold orders): that gets its value by calling the function lfRemain('H')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nHRemin"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  &&1.9 Remain Projected: that gets its value by calling the function lfRemain('P')
  nCurrent_Row = nCurrent_Row + 1
  laFlSruc[nCurrent_Row,1]= "nPRemin"
  laFlSruc[nCurrent_Row,2]= "N"
  laFlSruc[nCurrent_Row,3]= 20
  laFlSruc[nCurrent_Row,4]= 3

  *B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]
  gfCrtTmp(lcMatReq,@laFlSruc,,"",.F.)
  SELECT (lcMatReq)
  INDEX ON ITEM+cRecType+DTOS(DDATE)+STR(nLineNo,4) TAG Matreq ADDITIVE
  INDEX ON STR(nLineNo,4)+ITEM+CRECTYPE+CTRANCD TAG MATREQ1 ADDITIVE
  INDEX ON SUBSTR(ITEM,1,lnFMajLen)+STR(nLineNo,4)+SUBSTR(ITEM,lnFMajLen+2)+CRECTYPE+CTRANCD TAG MATREQ2 ADDITIVE

CASE lcType = 'C'
  DIMENSION laFlSruc[13,4]

  laFlSruc[1,1] = 'cType'
  laFlSruc[1,2] = 'C'
  laFlSruc[1,3] = 1
  laFlSruc[1,4] = 0

  laFlSruc[2,1] = 'cPoMat'
  laFlSruc[2,2] = 'C'
  laFlSruc[2,3] = 6
  laFlSruc[2,4] = 0

  laFlSruc[3,1] = 'cVendCode'
  laFlSruc[3,2] = 'C'
  laFlSruc[3,3] = 8
  laFlSruc[3,4] = 0

  laFlSruc[4,1] = 'cVenComp'
  laFlSruc[4,2] = 'C'
  laFlSruc[4,3] = 30
  laFlSruc[4,4] = 0

  laFlSruc[5,1] = 'cFabric'
  laFlSruc[5,2] = 'C'
  laFlSruc[5,3] = 19
  laFlSruc[5,4] = 0

  laFlSruc[6,1] = 'DEMO'
  laFlSruc[6,2] = 'C'
  laFlSruc[6,3] = 6
  laFlSruc[6,4] = 0

  laFlSruc[7,1] = 'cVenFab'
  laFlSruc[7,2] = 'C'
  laFlSruc[7,3] = 10
  laFlSruc[7,4] = 0

  laFlSruc[8,1] = 'cVenColr'
  laFlSruc[8,2] = 'C'
  laFlSruc[8,3] = 10
  laFlSruc[8,4] = 0

  laFlSruc[9,1] = 'nOnOrder'
  laFlSruc[9,2] = 'N'
  laFlSruc[9,3] = 11
  laFlSruc[9,4] = 3

  laFlSruc[10,1] = 'nReceived'
  laFlSruc[10,2] = 'N'
  laFlSruc[10,3] = 11
  laFlSruc[10,4] = 3

  laFlSruc[11,1] = 'nPending'
  laFlSruc[11,2] = 'N'
  laFlSruc[11,3] = 11
  laFlSruc[11,4] = 3

  laFlSruc[12,1] = 'cFld_Head'
  laFlSruc[12,2] = 'C'
  laFlSruc[12,3] = 25
  laFlSruc[12,4] = 0

  laFlSruc[13,1] = 'MFltVal'
  laFlSruc[13,2] = 'M'
  laFlSruc[13,3] = 0
  laFlSruc[13,4] = 0

  gfCrtTmp(lcMrpCurs,@laFlSruc,"cType+cPoMat+cFabric",lcMrpCurs,.T.)

CASE lcType = 'P'
  SELECT POFHEADR
  = AFIELDS(laTempStru)

  lnFlds = ALEN(laTempStru,1) + 1

  DIMENSION laTempStru[lnFlds,18]
  laTempStru[lnFlds,1] = 'cVendName'
  laTempStru[lnFlds,2] = 'C'
  laTempStru[lnFlds,3] = 30
  laTempStru[lnFlds,4] = 0

  STORE .F. TO laTempStru[lnFlds,5], laTempStru[lnFlds,6]
  STORE 0 TO laTempStru[lnFlds,17], laTempStru[lnFlds,18]

  FOR lnI = 7 TO 16
    STORE "" TO laTempStru[lnFlds,lnI]
  ENDFOR

  *-- add field cTmpPoMat ( temp MA Po No ) in case of generate ma Po No manually
  IF !llGENMAPON OR !llGENSTPON
    lnFlds = ALEN(laTempStru,1) + 1

    DIMENSION laTempStru[lnFlds, 18]
    *-- cTmpPoMat :  field used to hold the manual po no
    laTempStru[lnFlds,1] = 'cTmpPoMat'
    laTempStru[lnFlds,2] = 'C'
    laTempStru[lnFlds,3] = 6
    laTempStru[lnFlds,4] = 0

    STORE .F. TO laTempStru[lnFlds,5], laTempStru[lnFlds,6]
    STORE 0 TO laTempStru[lnFlds,17], laTempStru[lnFlds,18]

    FOR lnI = 7 TO 16
      STORE "" TO laTempStru[lnFlds,lnI]
    ENDFOR
  ENDIF

  gfCrtTmp(lcPOTmpHD,@laTempStru,"Vendor+cStyGrade+DTOC(Complete)+cPriceCur+cDutyCur+STR(nPriceRat,9,4)+STR(nDutyRat,9,4)",lcPOTmpHD)
ENDCASE

SELECT (lnAlias)
RETURN

*!***********************************************************************
* T I M E     P H A S E D    M A T E R I A L    R E Q U I R E M E N T S *
*!***********************************************************************

*!*************************************************************
*! Name      : lpAllQty
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Get All Qty.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOpenFiles
*!*************************************************************
PROCEDURE lpAllQty

LOCAL lnOnHand, lnFrom, lcFrom, lnBalance
lnOnHand = 0

SELECT (lcMatReq)
IF !EMPTY(CRECTYPE)
  RETURN
ENDIF

PRIVATE lcRecKey
lcRecKey = ITEM+cRecType+DTOS(DDATE)

*-- Calculate OnOrder quantities for all periods...
=lfCalcOnOrd(IIF(lcRpReqBas = 'F', ldThisDate, {}), ldLastDay)

*-- Calculate Required quantities for all periods...
=lfCalcReq(IIF(lcRpReqBas = 'F', ldThisDate, {}), ldLastDay)

*-- Get OnHand from Itemloc file...
IF loFabDye.SEEK('0002' + ITEM)
  SELECT FABDYE
  SUM TotStk REST WHILE cInvType+STYLE+cWareCode+Dyelot = '0002' + Fabric.STYLE FOR EMPTY(Dyelot) TO lnOnHand
ENDIF

*-- Update quantites for First Period...
SELECT (lcMatReq)
=SEEK(lcRecKey+STR(1,4))
REPLACE CurrHnd WITH lnOnHand,;
  CurrBal WITH CurrHnd + CurrOrd - CurrReq,;
  TOTAL   WITH TOTAL + CurrHnd + CurrOrd + CurrReq,;
  OnHnd1  WITH IIF(lcRpReqBas = 'F', MAX(CurrHnd + CurrOrd - CurrReq, 0), CurrHnd + CurrOrd - CurrReq),;
  CTYPE   WITH 'On Hand'

LOCAL lnPeriod, lcPeriod, lnPrdLine
lnPrdLine = 1
m.Item    = ITEM
m.Desc    = DESC
m.Total   = 0

*-- Accumulate quantites for next Periods...
FOR lnFrom = 1 TO lnFrcstPrds - 1
  lnPeriod = MOD(lnFrom, 10)
  lcPeriod = STR(lnPeriod, 1)

  IF lnPeriod = 0
    REPLACE CurrBal WITH CurrHnd + CurrOrd - CurrReq,;
      TOTAL   WITH TOTAL + CurrHnd + CurrOrd + CurrReq,;
      CTYPE   WITH 'On Hand'

    REPLACE OnHnd1 WITH IIF(lcRpReqBas = 'F', MAX(CurrHnd + CurrOrd - CurrReq, 0), CurrHnd + CurrOrd - CurrReq)
  ELSE
    lnBalance = EVALUATE('OnHnd' + lcPeriod) + EVALUATE('OnOrd' + lcPeriod) - EVALUATE('OnReq' + lcPeriod)
    REPLACE ('OnBal' + lcPeriod) WITH lnBalance,;
      TOTAL WITH TOTAL + EVALUATE('OnHnd' + lcPeriod) + EVALUATE('OnOrd' + lcPeriod) + EVALUATE('OnReq' + lcPeriod),;
      CTYPE WITH 'On Hand'

    IF lnPeriod = 9 AND lnPrdLine < lnFrcstLins
      lnPrdLine = lnPrdLine + 1
      m.Total   = m.Total + TOTAL
      IF !SEEK(lcRecKey+STR(lnPrdLine,4))
        APPEND BLANK
        REPLACE nLineNo WITH lnPrdLine,;
          ITEM    WITH m.Item,;
          DESC    WITH m.Desc
      ENDIF
      REPLACE CurrHnd WITH MAX(lnBalance,0),;
        TOTAL   WITH m.Total
    ELSE
      IF lnPeriod < 9
        REPLACE ('OnHnd' + STR(lnPeriod + 1, 1)) WITH IIF(lcRpReqBas = 'F', MAX(lnBalance,0), lnBalance)
      ENDIF
    ENDIF
  ENDIF
ENDFOR

SELECT (lcMatReq)
=SEEK(lcRecKey+STR(1,4))

RETURN

*!*************************************************************
*! Name      : lfCalcOnOrd
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Get all Qty.
*!*************************************************************
*! Example   : =lfCalcOnOrd()
*!*************************************************************
FUNCTION lfCalcOnOrd
PARAMETERS ldReqFrom, ldReqTo

LOCAL lcCurrPrd, OnOrdFld, lnConv, lnOnOrder, lcLastUOM, lcPoMat, lnPrdLine, lnPrdNo
lnConv    = 1
lnOnOrder = 0
lcPoMat   = SPACE(8)  && Variable to save the last Mat. PO added
lcLastUOM = ''

IF !loFabric.SEEK('0002'+&lcMatReq..ITEM, .F., .T.)
  RETURN
ENDIF

IF loPOFLn.SEEK('0002'+&lcMatReq..ITEM)
  SELECT POFLN
  SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = '0002' + &lcMatReq..ITEM
    IF cBusDocu+cStyType+PO <> lcPoMat
      IF !EMPTY(lcPoMat)
        IF lcRpRName = 'MATIMRQD'
          SELECT (lcMatReq)
          APPEND BLANK
          REPLACE nLineNo   WITH lnPrdLine,;
            ITEM      WITH SUBSTR(lcRecKey,1,19),;
            CRECTYPE  WITH 'M',;
            CTYPE     WITH 'Mat. PO',;
            DDATE     WITH ldCompDate,;
            CTRANCD   WITH SUBSTR(lcPoMat,3),;
            TOTAL     WITH lnMatPo
          REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH lnMatPo
          SELECT POFLN
        ENDIF

        *--Update main record...
        SELECT (lcMatReq)
        =SEEK(lcRecKey+STR(lnPrdLine,4))

        REPLACE (OnOrdFld) WITH EVALUATE(OnOrdFld) + lnMatPo
        SELECT POFLN
      ENDIF
      loPOFHDR.SEEK(cBusDocu + cStyType + PO, .F., .T.)

      lcPoMat    = cBusDocu+cStyType+PO
      lnMatPo    = 0
      ldCompDate = IIF(lcRpReqBas = 'F', POFHEADR.Available, POFHEADR.COMPLETE)

      *--Get the period for the current date
      lnPrdNo   = INT(MAX(ldCompDate - ldThisDate, 0) / lnMonthEd)
      lnPrdNo   = IIF(lcRpReqBas = 'F', lnPrdNo, MIN(lnPrdNo, 9))
      lnPrdLine = INT(lnPrdNo / 10) + 1
      lcCurrPrd = ALLTRIM(STR(MOD(lnPrdNo, 10)))
      OnOrdFld  = IIF(lcCurrPrd = '0', 'CurrOrd', 'OnOrd' + lcCurrPrd)
    ENDIF

    IF POFHEADR.STATUS $'OH' AND BETWEEN(ldCompDate, ldReqFrom, ldReqTo) AND cBusDocu $ 'PR' AND cStyType $ 'MF'
      IF !(POFLN.cUOMCode == lcLastUOM)
        lcLastUOM = POFLN.cUOMCode
        lnConv    = 1
        IF loUOM.SEEK(POFLN.cUOMCode, .F., .T.)
          lnConv = UOM.nConF
        ENDIF
      ENDIF

      IF POFLN.cBusDocu = 'P'
        lnMatPo = lnMatPo + (TOTQTY * IIF(TranCd = '1', lnConv, -lnConv))
      ELSE
        lnMatPo = lnMatPo + (TOTQTY * IIF(TranCd = '1', -lnConv, lnConv))
      ENDIF
    ENDIF
  ENDSCAN

  IF !EMPTY(lcPoMat)
    IF lcRpRName = 'MATIMRQD'
      SELECT (lcMatReq)
      APPEND BLANK
      REPLACE nLineNo   WITH lnPrdLine,;
        ITEM      WITH SUBSTR(lcRecKey,1,19),;
        CRECTYPE  WITH 'M',;
        CTYPE     WITH 'Mat. PO',;
        DDATE     WITH ldCompDate,;
        CTRANCD   WITH SUBSTR(lcPoMat,3),;
        TOTAL     WITH lnMatPo
      REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH lnMatPo
    ENDIF

    *--Update main record...
    SELECT (lcMatReq)
    IF !SEEK(lcRecKey+STR(lnPrdLine,4))
      =SEEK(lcRecKey+STR(1,4))
      m.Item = ITEM
      m.Desc = DESC

      APPEND BLANK
      REPLACE nLineNo WITH lnPrdLine,;
        ITEM    WITH m.Item,;
        DESC    WITH m.Desc
    ENDIF
    REPLACE (OnOrdFld) WITH EVALUATE(OnOrdFld) + lnMatPo
  ENDIF
ENDIF

RETURN

*!*************************************************************
*! Name      : lfCalcReq
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Get Required Qty.
*!*************************************************************
*! Example   : =lfCalcReq()
*!*************************************************************
FUNCTION lfCalcReq
PARAMETERS ldReqFrom, ldReqTo



LOCAL lnAlias, lnCount, lnRequied[lnFrcstPrds], lnPlanReq[lnFrcstPrds], lnCurrPrd, lnPrdLine, lcCurrPrd, lnPeriod, lnPrdNo, lcI, OnReqFld
lnAlias   = SELECT()
lnRequied = 0
lnMFReq   = 0

*--Calculate the required based on (WIP/Plan/Both)
lnPlanReq = 0
IF lcRpReqBas = 'B'
  IF lnMonthEd / 7 > 1
    DECLARE laWeeks[lnMonthEd / 7 * lnFrcstPrds,2]
    STORE 0 TO laWeeks
  ENDIF
  SELECT (lcStMatReq)
  ZAP
ENDIF

IF lcRpReqBas $ 'WBF'
  IF llPOIstall OR llMFIstall
    =loPOSLN.SetOrder('POSLN')
    =loCTKTBOM.SetOrder('CTKTITEM')

    **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
    *!*	    lcSeekItem = ''
    *!*	    lnAstPosition = AT('*', &lcMatReq..Item)
    *!*	    lcSeekItem = IIF(lnAstPosition = 0, &lcMatReq..Item, ALLTRIM(SUBSTR(&lcMatReq..Item, 1, lnAstPosition - 2)))
    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

    *-- Calculate the required at the correct interval of date.

    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
    =loCTKTBOM.SEEK('0002'+&lcMatReq..ITEM)
    *=loCTKTBOM.SEEK('0002'+lcSeekItem)
    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

    SELECT Ctktbom
    *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
    SCAN REST WHILE cInvType+ITEM+cIMTyp+CutTkt+MfgCode+Dyelot = '0002' + &lcMatReq..ITEM + IIF(lcRpReqBas = 'F', 'M', '');
        FOR cCatgTyp $ 'FT'
      *SCAN REST WHILE cInvType+Item+cIMTyp+CutTkt+MfgCode+Dyelot = '0002' + lcSeekItem + IIF(lcRpReqBas = 'F', 'M', '');
      FOR cCatgTyp $ 'FT'

      *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

      lcStyType = IIF(cIMTyp = 'I', 'P', 'U')
      IF loPOSHDR.SEEK('P'+lcStyType+CUTTKT, .F., .T.) .AND. !(POSHDR.STATUS $ 'SX') .AND. loPOSLN.SEEK('P'+lcStyType+CUTTKT)
        lnLeadTime = 0
        lcStyles   = ''
        lcStyleKey = SPACE(19)
        llStyleFul = .F.

        IF lcRpRName = 'MATIMRQD'
          SELECT POSLN
          SCAN REST WHILE cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+TranCD = 'P'+lcStyType+CTKTBOM.CUTTKT
            IF SUBSTR(STYLE,1,lnMajorLn) <> lcStyleKey
              lcStyleKey = SUBSTR(STYLE,1,lnMajorLn)
              IF EMPTY(lcStyles)
                lcStyles   = ALLTRIM(SUBSTR(STYLE,1,lnMajorLn))
                llStyleFul = (LEN(lcStyles) >= 16)
              ELSE
                IF !llStyleFul
                  IF LEN(lcStyles + ',' + ALLTRIM(SUBSTR(STYLE,1,lnMajorLn))) > 16
                    lcStyles   = lcStyles + ',...'
                    llStyleFul = .T.
                  ELSE
                    lcStyles   = lcStyles + ',' + ALLTRIM(SUBSTR(STYLE,1,lnMajorLn))
                    llStyleFul = (LEN(lcStyles)=16)
                  ENDIF
                ELSE
                  IF lcRpRName # 'MATIMRQD' AND lcRpReqBas = 'F'
                    EXIT
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

            IF lcRpReqBas # 'F' AND loStyle.SEEK(STYLE, .F., .T.)
              lnLeadTime = MAX(lnLeadTime,STYLE.LEADTIME)
            ENDIF
          ENDSCAN
        ENDIF

        SELECT CTKTBOM
        ldLeadTime = IIF(lcRpReqBas = 'F', POSHDR.START, lfHolDay(POSHDR.COMPLETE - lnLeadTime, POSHDR.COMPLETE))
        lnPrdNo    = INT(MAX(ldLeadTime - (ldThisDate + IIF(lcRpReqBas = 'F', 0, 7)), 0) / lnMonthEd)
        lnPrdNo    = IIF(lcRpReqBas = 'F', lnPrdNo, MIN(lnPrdNo, 9))
        lnPrdLine  = INT(lnPrdNo / 10) + 1
        lnCurrPrd  = MOD(lnPrdNo, 10) + 1
        lcCurrPrd  = STR(lnCurrPrd - 1, 1)

        **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
        *lnWeekNo   = 1 + 4 * (lnCurrPrd - 1)
        lnWeekNo   = 1 + IIF(lnMonthEd / 7 > 1,lnMonthEd / 7,4) * (lnCurrPrd - 1)
        **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

        ldStrDat   = IIF(lnCurrPrd = 1, ldReqFrom, ldThisDate + (lnCurrPrd - 1) * lnMonthEd + IIF(lcRpReqBas = 'F', 0, 7))
        ldEndDat   = IIF(lnCurrPrd = lnFrcstPrds, ldReqTo, ldStrDat + lnMonthEd - 1)

        IF lcRpReqBas = 'F' AND !EMPTY(POSHDR.STYLE)
          lcItemSty = SUBSTR(POSHDR.STYLE, 1, lnMajorLn) + lcSeparator + SUBSTR(&lcMatReq..ITEM, lnFClrSrt, lnFClrEnd)
          llPOSCond = BETWEEN(ldLeadTime, ldReqFrom, ldReqTo) AND loForCast.SEEK(lcItemSty + PADR(YEAR(ldLeadTime),4) + PADL(WEEK(ldLeadTime),2), 'FORCAST', .T.)
        ELSE
          llPOSCond = BETWEEN(ldLeadTime, ldReqFrom, ldReqTo)
        ENDIF

        IF llPOSCond
          IF lcRpRName = 'MATIMRQD'
            SELECT (lcMatReq)
            APPEND BLANK
            REPLACE nLineNo   WITH lnPrdLine,;
              ITEM      WITH SUBSTR(lcRecKey,1,19),;
              CRECTYPE  WITH IIF(POSHDR.cStyType = 'U', 'C', 'P'),;
              CTYPE     WITH IIF(POSHDR.cStyType = 'U', 'C/T', 'Sty. PO'),;
              DDATE     WITH ldLeadTime,;
              CTRANCD   WITH POSHDR.PO,;
              CSTYLES   WITH lcStyles,;
              CREF      WITH lfGetRef('2'+POSHDR.PO),;
              TOTAL     WITH -MAX(CTKTBOM.Req_Qty - CTKTBOM.Used_Qty,0)
            REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH -MAX(CTKTBOM.Req_Qty - CTKTBOM.Used_Qty,0)
            SELECT CTKTBOM
          ENDIF
          lnRequied[lnPrdNo + 1] = lnRequied[lnPrdNo + 1] + MAX(Req_Qty - Used_Qty, 0)

          IF lcRpReqBas = 'B' .AND. lnMonthEd / 7 > 1
            DO CASE
            CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
              laWeeks[lnWeekNo,1]     = laWeeks[lnWeekNo,1]     + MAX(Req_Qty-Used_Qty,0)
            CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(lnCurrPrd = 10 .AND. lnMonthEd = 14,ldEndDat,ldStrDat+13))
              laWeeks[lnWeekNo + 1,1] = laWeeks[lnWeekNo + 1,1] + MAX(Req_Qty-Used_Qty,0)
            CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
              laWeeks[lnWeekNo + 2,1] = laWeeks[lnWeekNo + 2,1] + MAX(Req_Qty-Used_Qty,0)
            CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(lnCurrPrd = 10,ldEndDat,ldStrDat+27))
              laWeeks[lnWeekNo + 3,1] = laWeeks[lnWeekNo + 3,1] + MAX(Req_Qty-Used_Qty,0)
            ENDCASE
          ENDIF

          IF lcRpReqBas = 'B' .AND. loBOMLINE.SEEK(CIMTYP+'1'+CUTTKT)
            SELECT BOMLINE
            SCAN REST WHILE cimtyp+ctype+ctktno+STR(LINENO,6)+cbomtyp+cInvType+STYLE+cInvTypC+ITEM+mfgcode =;
                CTKTBOM.CIMTYP+'1'+CTKTBOM.CUTTKT;
                FOR ITEM = &lcMatReq..ITEM
              SELECT (lcStMatReq)
              IF SEEK(BOMLINE.STYLE)
                FOR lnI = 1 TO lnMonthEd / 7
                  lcI = STR(lnI,1)
                  ldLastDate = ldStrDat + (7 * lnI) - 1

                  IF lnPrdNo = lnFrcstPrds - 1
                    IF (lnI = 1 .AND. lnMonthEd = 7) .OR. (lnI = 2 .AND. lnMonthEd = 14) .OR. (lnI = 4)
                      ldLastDate = ldEndDat
                    ENDIF
                  ENDIF

                  IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                    REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + BOMLINE.STYQTY
                    EXIT
                  ENDIF
                ENDFOR
              ELSE
                FOR lnI = 1 TO lnMonthEd/7
                  lcI = STR(lnI,1)
                  ldLastDate = ldStrDat+(7*lnI)-1
                  IF lnCurrPrd = lnFrcstPrds - 1
                    IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                      ldLastDate = ldEndDat
                    ENDIF
                  ENDIF
                  IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                    APPEND BLANK
                    REPLACE CSTYLE       WITH BOMLINE.STYLE,;
                      nPeriod      WITH lnPrdNo + 1,;
                      ('NWIP'+lcI) WITH BOMLINE.STYQTY
                    EXIT
                  ENDIF
                ENDFOR
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN

    =loPOSLN.SetOrder("POSLNS")
  ENDIF
ENDIF

SELECT BOM
lcOrder = loBom.lcTagName
=loBOM.SetOrder('MITEMTYP')

SELECT (lcMatReq)

IF loBom.SEEK('0002'+SUBSTR(ITEM, 1, lnFMajLen))
  SELECT BOM
  SCAN REST WHILE cInvTypC+ITEM+Typ+cInvType+cItmMajor+cCstShtTyp+cCstSht_Id+cItmMask = '0002'+SUBSTR(&lcMatReq..ITEM, 1, lnFMajLen)
    lcIClr = SUBSTR(ITEM, lnFClrSrt, lnFClrEnd)

    IF lcIClr = SUBSTR(&lcMatReq..ITEM, lnFClrSrt, lnFClrEnd) .OR. lcIClr = "******"
      SELECT STYLE
      =loStyle.SEEK(SUBSTR(BOM.CITMMASK, 1, lnMajorLn))

      *-- consider case of color length less than 6 chracters
      *B609665,1 WAM 08/23/2011 Get item color using the fabric code structure
      *SCAN REST WHILE STYLE = SUBSTR(BOM.CITMMASK,1,lnMajorLn);
      FOR LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE) .AND.;
      IIF(SUBSTR(BOM.Item, lnFClrSrt, lnFClrEnd)='******',SUBSTR(STYLE,lnClrSrt,lnClrEnd)=SUBSTR(EVALUATE(lcMatReq + '.Item'),lnClrSrt,lnClrEnd),.T.)
      SCAN REST WHILE STYLE = SUBSTR(BOM.CITMMASK,1,lnMajorLn);
          FOR LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE) .AND.;
          IIF(SUBSTR(BOM.ITEM, lnFClrSrt, lnFClrEnd)='******',SUBSTR(STYLE,lnClrSrt,lnClrEnd)=SUBSTR(EVALUATE(lcMatReq + '.Item'),lnFClrSrt,lnFClrEnd),.T.)
        *B609665,1 WAM 08/23/2011 (End)

        lcSlctStyl = '0001' + STYLE

        *-- Run the MPS Process to get forecast Cutting Tickets for each Style...
        IF lcRpReqBas = 'F' AND (llMFIstall OR llPOIstall)
          =lfStyleReq(SUBSTR(lcSlctStyl, 5), ldReqFrom, ldReqTo)

          SELECT (lcStyTmp)
          IF SEEK(SUBSTR(lcSlctStyl, 5))
            SCAN REST WHILE STYLE+DTOS(dFrDate) = SUBSTR(lcSlctStyl, 5)
              lnPrdNo = INT(MAX(dFrDate - ldThisDate, 0) / lnMonthEd)
              lnPrdNo = IIF(lcRpReqBas = 'F', lnPrdNo, MIN(lnPrdNo, 9))

              IF lcRpRName = 'MATIMRQD'
                lnPrdLine  = INT(lnPrdNo / 10) + 1
                lnCurrPrd  = MOD(lnPrdNo, 10) + 1
                lcCurrPrd  = STR(lnCurrPrd - 1, 1)

                SELECT (lcMatReq)
                APPEND BLANK
                REPLACE nLineNo   WITH lnPrdLine,;
                  ITEM      WITH SUBSTR(lcRecKey,1,19),;
                  CRECTYPE  WITH 'F',;
                  CTYPE     WITH 'Forecast',;
                  DDATE     WITH EVALUATE(lcStyTmp + '.dFrDate'),;
                  CTRANCD   WITH '',;
                  CSTYLES   WITH SUBSTR(lcSlctStyl,5,lnMajorLn),;
                  CREF      WITH '',;
                  TOTAL     WITH -(EVALUATE(lcStyTmp + '.TotPlan') * BOM.Nbomtotqty)
                REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH -(EVALUATE(lcStyTmp + '.TotPlan') * BOM.Nbomtotqty)
              ENDIF

              SELECT (lcStyTmp)
              lnRequied[lnPrdNo + 1] = lnRequied[lnPrdNo + 1] + (TotPlan * BOM.Nbomtotqty)
            ENDSCAN
          ENDIF
        ENDIF

        IF (llMFIstall OR llPOIstall) .AND. loPOSLN.SEEK(lcSlctStyl)
          *--Variable to save the last Sty. PO added [Start]
          lcPO    = SPACE(8)
          lcPOLTm = SPACE(8)

          SELECT POSLN
          SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TranCD = lcSlctStyl FOR cBusDocu = 'P' AND cStyType $ IIF(lcRpReqBas = 'F', 'U', 'PU') AND TranCd = '1'
            *--Calculate the required at the correct interval of date
            IF cBusDocu+cStyType+PO <> lcPOLTm
              lcPOLTm    = cBusDocu+cStyType+PO
              lnLeadTime = 0
              lcPOKey    = EVALUATE(KEY())

              loPOSHDR.SEEK(cBusDocu + cStyType + PO, .F., .T.)

              IF lcRpReqBas # 'F'
                *-- Use the loCutTkt Object as Temp Object to prevent multiple Seeks in loPOSLN Object
                loCutTktL.SetOrder("POSLN")
                IF loCutTktL.SEEK(lcPOLTm)
                  SELECT CUTTKTL
                  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO = lcPOLTm
                    IF SEEK(STYLE,'STYLE')
                      lnLeadTime = MAX(lnLeadTime,STYLE.LEADTIME)
                    ENDIF
                  ENDSCAN
                ENDIF
                loCutTktL.SetOrder("POSLNS")

                SELECT POSLN
                ldLeadTime = lfHolDay(POSHDR.COMPLETE-lnLeadTime,POSHDR.COMPLETE)
              ELSE
                ldLeadTime = POSHDR.START
              ENDIF
            ENDIF

            IF POSHDR.STATUS $ 'BH' AND;
                BETWEEN(ldLeadTime, ldReqFrom, ldReqTo) AND;
                IIF(lcRpReqBas = 'F', loForCast.SEEK(STYLE.STYLE + PADR(YEAR(ldLeadTime),4) + PADL(WEEK(ldLeadTime),2), 'FORCAST', .T.), .T.)
              IF cBusDocu+cStyType+PO <> lcPO
                IF !EMPTY(lcPO) .AND. lcRpRName = 'MATIMRQD'
                  SELECT (lcMatReq)
                  SET ORDER TO MATREQ1
                  IF SEEK(STR(lnPrdLine,4) + SUBSTR(lcRecKey, 1, 19) + IIF(POSHDR.cStyType = 'U', 'C', 'P') + lcPO)
                    REPLACE TOTAL WITH TOTAL - lnPO
                    REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH EVALUATE(IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) - lnPO

                    lcStyles = ALLTRIM(SUBSTR(lcSlctStyl,5,lnMajorLn))
                    lnStyles = LEN(lcStyles)
                    IF lcStyles + ',' $ ALLTRIM(CSTYLES) .OR.;
                        RIGHT(ALLTRIM(CSTYLES),lnStyles) = lcStyles
                      *-- Do Nothing
                    ELSE
                      IF LEN(ALLTRIM(CSTYLES) + ',' + lcStyles) > 16
                        REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',...'
                      ELSE
                        REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',' + lcstyles
                      ENDIF
                    ENDIF
                  ELSE
                    APPEND BLANK
                    REPLACE nLineNo   WITH lnPrdLine,;
                      ITEM      WITH SUBSTR(lcRecKey,1,19),;
                      CRECTYPE  WITH IIF(POSHDR.cStyType = 'U', 'C', 'P'),;
                      CTYPE     WITH IIF(SUBSTR(lcPO, 2,1) = 'U', 'C/T', 'Sty. PO'),;
                      DDATE     WITH ldCompDate,;
                      CTRANCD   WITH SUBSTR(lcPO, 3, 6),;
                      CSTYLES   WITH SUBSTR(lcSlctStyl,5,lnMajorLn),;
                      CREF      WITH lfGetRef(IIF(SUBSTR(lcPO, 2, 1) = 'P', '2', '1') + SUBSTR(lcPO, 3, 6)),;
                      TOTAL     WITH -lnPO
                    REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH -lnPO
                  ENDIF
                  SET ORDER TO MATREQ
                  SELECT POSLN
                ENDIF
                lcPO = POSHDR.cBusDocu + POSHDR.cStyType + POSHDR.PO
                lnPO = 0
                ldCompDate = IIF(lcRpReqBas = 'F', ldLeadTime, ldLeadTime - 7)
                lnPrdNo    = INT(MAX(ldLeadTime - (ldThisDate + IIF(lcRpReqBas = 'F', 0, 7)), 0) / lnMonthEd)
                lnPrdNo    = IIF(lcRpReqBas = 'F', lnPrdNo, MIN(lnPrdNo, 9))
                lnPrdLine  = INT(lnPrdNo / 10) + 1
                lnCurrPrd  = MOD(lnPrdNo, 10) + 1
                lcCurrPrd  = STR(lnCurrPrd - 1, 1)

                **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
                *lnWeekNo   = 1 + 4 * (lnCurrPrd - 1)
                lnWeekNo   = 1 + IIF(lnMonthEd / 7 > 1,lnMonthEd / 7,4) * (lnCurrPrd - 1)
                **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]

                ldStrDat   = IIF(lnCurrPrd = 1, ldReqFrom, ldThisDate + (lnCurrPrd - 1) * lnMonthEd + IIF(lcRpReqBas = 'F', 0, 7))
                ldEndDat   = IIF(lnCurrPrd = lnFrcstPrds, ldReqTo, ldStrDat + lnMonthEd - 1)
              ENDIF

              IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                *-- Consider case of extended size scale
                lnMemWidth = SET('MEMOWIDTH')
                SET MEMOWIDTH TO 40
                FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                  lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                  IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                      IIF(llExtSizSc,SUBSTR(SUBSTR(lcSlctStyl,5),lnSizePos,lnSizeLen)=;
                      SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                    FOR lnI = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                      lcI = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnI,1)
                      IF VAL(lcI) > 0
                        lnPO = lnPO + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                        lnRequied[lnCurrPrd] = lnRequied[lnCurrPrd] + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                      ENDIF
                    ENDFOR
                  ENDIF
                ENDFOR
                SET MEMOWIDTH TO lnMemWidth
              ELSE
                lnPO = lnPO + (Totqty * BOM.Nbomtotqty)
                lnRequied[lnPrdNo + 1] = lnRequied[lnPrdNo + 1] + (Totqty * BOM.Nbomtotqty)
              ENDIF

              *-- Get the Week required.
              IF lcRpReqBas = 'B' .AND. lnMonthEd / 7 > 1
                DO CASE
                CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
                  laWeeks[lnWeekNo,1] = laWeeks[lnWeekNo,1] + (Totqty * BOM.Nbomtotqty)
                CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(lnCurrPrd = 10 .AND. lnMonthEd=14,ldEndDat,ldStrDat+13))
                  laWeeks[lnWeekNo + 1,1] = laWeeks[lnWeekNo + 1,1] + (Totqty * BOM.Nbomtotqty)
                CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
                  laWeeks[lnWeekNo + 2,1] = laWeeks[lnWeekNo + 2,1] + (Totqty * BOM.Nbomtotqty)
                CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(lnCurrPrd = 10,ldEndDat,ldStrDat+27))
                  laWeeks[lnWeekNo + 3,1] = laWeeks[lnWeekNo + 3,1] + (Totqty * BOM.Nbomtotqty)
                ENDCASE
              ENDIF

              *-- Get the style/color/Week required.
              IF lcRpReqBas = 'B'
                SELECT (lcStMatReq)
                IF SEEK(STR(lnCurrPrd,2) + POSLN.STYLE)
                  FOR lnI = 1 TO lnMonthEd / 7
                    lcI = STR(lnI,1)
                    ldLastDate = ldStrDat + (7 * lnI) - 1
                    IF lnPrdNo = lnFrcstPrds - 1
                      IF (lnI = 1 .AND. lnMonthEd = 7) .OR. (lnI = 2 .AND. lnMonthEd = 14) .OR. (lnI = 4)
                        ldLastDate = ldEndDat
                      ENDIF
                    ENDIF

                    IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                      IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)

                        *-- Consider case of extended size scale.
                        lnMemWidth = SET('MEMOWIDTH')
                        SET MEMOWIDTH TO 40
                        FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                          lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                          IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                              IIF(llExtSizSc,SUBSTR(SUBSTR(lcSlctStyl,5),lnSizePos,lnSizeLen)=;
                              SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                            FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                              lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                              IF VAL(lcK) > 0
                                REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('POSLN.QTY'+lcK)
                              ENDIF
                            ENDFOR
                          ENDIF
                        ENDFOR
                        SET MEMOWIDTH TO lnMemWidth
                      ELSE
                        REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + POSLN.TOTQTY
                      ENDIF
                      EXIT
                    ENDIF
                  ENDFOR
                ELSE
                  FOR lnI = 1 TO lnMonthEd/7
                    lcI = STR(lnI,1)
                    ldLastDate = ldStrDat+(7*lnI)-1
                    IF lnPrdNo = lnFrcstPrds - 1
                      IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                        ldLastDate = ldEndDat
                      ENDIF
                    ENDIF
                    IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                      APPEND BLANK
                      REPLACE nPeriod WITH lnPrdNo + 1
                      IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)

                        *-- Consider case of extended size scale.
                        lnMemWidth = SET('MEMOWIDTH')
                        SET MEMOWIDTH TO 40
                        FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                          lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                          IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                              IIF(llExtSizSc,SUBSTR(SUBSTR(lcSlctStyl,5),lnSizePos,lnSizeLen)=;
                              SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                            FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                              lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                              IF VAL(lcK) > 0
                                REPLACE CSTYLE       WITH POSLN.STYLE,;
                                  ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('POSLN.QTY'+lcK)
                              ENDIF
                            ENDFOR
                          ENDIF
                        ENDFOR
                        SET MEMOWIDTH TO lnMemWidth
                      ELSE
                        REPLACE CSTYLE       WITH POSLN.STYLE,;
                          ('NWIP'+lcI) WITH POSLN.TOTQTY
                      ENDIF
                      EXIT
                    ENDIF
                  ENDFOR
                ENDIF
              ENDIF
            ENDIF
          ENDSCAN

          *-- Add detail data
          IF !EMPTY(lcPO) .AND. lcRpRName = 'MATIMRQD'
            SELECT (lcMatReq)
            SET ORDER TO MATREQ1
            IF SEEK(STR(lnPrdLine,4) + SUBSTR(lcRecKey, 1, 19) + IIF(POSHDR.cStyType = 'U', 'C', 'P') + lcPO)
              REPLACE TOTAL WITH TOTAL - lnPO
              REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH EVALUATE(IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) - lnPO

              lcStyles = ALLTRIM(SUBSTR(lcSlctStyl,5,lnMajorLn))
              lnStyles = LEN(lcStyles)
              IF lcStyles + ',' $ ALLTRIM(CSTYLES) .OR.;
                  RIGHT(ALLTRIM(CSTYLES),lnStyles) = lcStyles
                *-- Do nothing
              ELSE
                IF LEN(ALLTRIM(CSTYLES) + ',' + lcStyles) > 16
                  REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',...'
                ELSE
                  REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',' + lcstyles
                ENDIF
              ENDIF
            ELSE
              APPEND BLANK
              REPLACE nLineNo   WITH lnPrdLine,;
                ITEM      WITH SUBSTR(lcRecKey,1,19),;
                CRECTYPE  WITH IIF(POSHDR.cStyType = 'U', 'C', 'P'),;
                CTYPE     WITH IIF(SUBSTR(lcPO, 2,1) = 'U', 'C/T', 'Sty. PO'),;
                DDATE     WITH ldCompDate,;
                CTRANCD   WITH SUBSTR(lcPO, 3, 6),;
                CSTYLES   WITH SUBSTR(lcSlctStyl,5,lnMajorLn),;
                CREF      WITH lfGetRef(IIF(SUBSTR(lcPO, 2, 1) = 'P', '2', '1') + SUBSTR(lcPO, 3, 6)),;
                TOTAL     WITH -lnPO
              REPLACE (IIF(lcCurrPrd = '0', 'CurrHnd', 'OnHnd' + lcCurrPrd)) WITH -lnPO
            ENDIF
            SET ORDER TO MATREQ
          ENDIF
        ENDIF

        IF lcRpReqBas $ 'PB'
          lcSlctStyl = STYLE
          IF loFORCAST.SEEK(lcSlctStyl)
            SELECT FORCAST
            SCAN REST WHILE STYLE+STR(NYEAR,4)+STR(NWEEK,2) = lcSlctStyl
              ldWeekDay  = CTOD('01/01/'+STR(NYEAR,4))+(7*(NWEEK-1))
              ldWeekDay  = lfWeekStr(ldWeekDay)
              ldLeadTime = lfHolDay(ldWeekDay-STYLE.LEADTIME,ldWeekDay)
              lnPrdNo    = MIN(INT(MAX(ldLeadTime - (ldThisDate + IIF(lcRpReqBas = 'F', 0, 7)), 0) / lnMonthEd), 9)
              lnPrdLine  = INT(lnPrdNo / 10) + 1
              lnCurrPrd  = MOD(lnPrdNo, 10) + 1
              lcCurrPrd  = STR(lnCurrPrd - 1, 1)

              **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
              *lnWeekNo   = 1 + 4 * (lnCurrPrd - 1)
              lnWeekNo   = 1 + IIF(lnMonthEd / 7 > 1,lnMonthEd / 7,4) * (lnCurrPrd - 1)
              **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]
              ldStrDat   = IIF(lnCurrPrd = 1, ldReqFrom, ldThisDate + (lnCurrPrd - 1) * lnMonthEd + IIF(lcRpReqBas = 'F', 0, 7))
              ldEndDat   = IIF(lnCurrPrd = lnFrcstPrds, ldReqTo, ldStrDat + lnMonthEd - 1)

              IF BETWEEN(ldLeadTime, ldReqFrom, ldReqTo)
                IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                  *--Consider case of extended size scale
                  lnMemWidth = SET('MEMOWIDTH')
                  SET MEMOWIDTH TO 40
                  FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                    lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                    IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                        IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                        SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                      FOR lnI = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                        lcI = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnI,1)
                        IF VAL(lcI) > 0
                          lnPlanReq[lnCurrPrd] = lnPlanReq[lnCurrPrd] + (EVALUATE('NFORQTY'+lcI) * BOM.Nbomtotqty)
                        ENDIF
                      ENDFOR
                    ENDIF
                  ENDFOR
                  SET MEMOWIDTH TO lnMemWidth
                ELSE
                  lnPlanReq[lnPrdNo + 1] = lnPlanReq[lnPrdNo + 1] + (nForTotqty * BOM.Nbomtotqty)
                ENDIF

                IF lcRpReqBas = 'B' .AND. lnMonthEd / 7 > 1
                  DO CASE
                  CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
                    laWeeks[lnWeekNo,2] = laWeeks[lnWeekNo,2] + (nForTotqty * BOM.Nbomtotqty)
                  CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(lnCurrPrd = 10 .AND. lnMonthEd=14,ldEndDat,ldStrDat+13))
                    laWeeks[lnWeekNo + 1,2] = laWeeks[lnWeekNo + 1,2] + (nForTotqty * BOM.Nbomtotqty)
                  CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
                    laWeeks[lnWeekNo + 2,2] = laWeeks[lnWeekNo + 2,2] + (nForTotqty * BOM.Nbomtotqty)
                  CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(lnCurrPrd = 10,ldEndDat,ldStrDat+27))
                    laWeeks[lnWeekNo + 3,2] = laWeeks[lnWeekNo + 3,2] + (nForTotqty * BOM.Nbomtotqty)
                  ENDCASE
                ENDIF

                IF lcRpReqBas = 'B'
                  SELECT (lcStMatReq)
                  IF SEEK(lcSlctStyl)
                    FOR lnI = 1 TO lnMonthEd / 7
                      lcI = STR(lnI,1)
                      ldLastDate = ldStrDat + (7 * lnI) - 1
                      IF lnPrdNo = lnFrcstPrds - 1
                        IF (lnI=1 .AND. lnMonthEd = 7) .OR. (lnI = 2 .AND. lnMonthEd = 14) .OR. (lnI = 4)
                          ldLastDate = ldEndDat
                        ENDIF
                      ENDIF
                      IF BETWEEN(ldLeadTime,ldStrDat + (7 * (lnI - 1)), ldLastDate) .OR. EMPTY(ldStrDat)
                        IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)

                          *--Consider case of extended size scale
                          lnMemWidth = SET('MEMOWIDTH')
                          SET MEMOWIDTH TO 40
                          FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                            lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                            IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                              FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                IF VAL(lcK) > 0
                                  REPLACE NYEALD            WITH BOM.NBOMTOTQTY,;
                                    ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                                    ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + EVALUATE('FORCAST.NFORQTY'+lcK)
                                ENDIF
                              ENDFOR
                            ENDIF
                          ENDFOR
                          SET MEMOWIDTH TO lnMemWidth
                        ELSE
                          REPLACE NYEALD            WITH BOM.NBOMTOTQTY,;
                            ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                            ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + FORCAST.NFORTOTQTY
                        ENDIF
                        EXIT
                      ENDIF
                    ENDFOR
                  ELSE
                    FOR lnI = 1 TO lnMonthEd / 7
                      lcI = STR(lnI, 1)
                      ldLastDate = ldStrDat + (7 * lnI) - 1

                      IF lnPrdNo = lnFrcstPrds - 1
                        IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                          ldLastDate = ldEndDat
                        ENDIF
                      ENDIF

                      IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                        APPEND BLANK
                        REPLACE nPeriod WITH lnPrdNo + 1
                        IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)

                          *--Consider case of extended size scale
                          lnMemWidth = SET('MEMOWIDTH')
                          SET MEMOWIDTH TO 40
                          FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                            lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                            IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                              FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                IF VAL(lcK) > 0
                                  REPLACE CSTYLE            WITH FORCAST.STYLE,;
                                    NYEALD            WITH BOM.NBOMTOTQTY,;
                                    ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                                    ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + EVALUATE('FORCAST.NFORQTY'+lcK)
                                ENDIF
                              ENDFOR
                            ENDIF
                          ENDFOR
                          SET MEMOWIDTH TO lnMemWidth
                        ELSE
                          REPLACE CSTYLE            WITH FORCAST.STYLE,;
                            NYEALD            WITH BOM.NBOMTOTQTY,;
                            ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                            ('NPLAN'+lcI)     WITH FORCAST.NFORTOTQTY
                        ENDIF
                        EXIT
                      ENDIF
                    ENDFOR
                  ENDIF
                ENDIF
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
loBom.SetOrder(lcOrder)

*--Return the maximum of required of WIP and required of Plan
IF lcRpReqBas = 'B'
  DECLARE laPlan[lnFrcstPrds,4],laStyles[lnFrcstPrds,4],laStyleKey[lnFrcstPrds,4],laStyleFul[lnFrcstPrds,4],laLeadTime[lnFrcstPrds,4]
  STORE 0 TO laPlan
  STORE SPACE(0) TO laStyles
  STORE SPACE(19) TO laStyleKey

  FOR lnI = 1 TO lnFrcstPrds
    ldStrDat = IIF(lnI = 1, ldReqFrom, ldThisDate + (lnI - 1) * lnMonthEd + IIF(lcRpReqBas = 'F', 0, 7))
    ldEndDat = IIF(lnI = lnFrcstPrds, ldReqTo, ldStrDat + lnMonthEd - 1)
    STORE ldEndDat TO laLeadTime[lnI,1], laLeadTime[lnI,2], laLeadTime[lnI,3], laLeadTime[lnI,4]
  ENDFOR

  SELECT (lcStMatReq)
  SCAN
    FOR lnI = 1 TO 4
      lcI = STR(lnI,1)
      laLeadTime[nPeriod,lnI] = ldEndDat

      IF EVALUATE('NPLAN'+lcI) > EVALUATE('NWIP'+lcI)
        IF SUBSTR(CSTYLE,1,lnMajorLn) <> laStyleKey[nPeriod,lnI]
          laStyleKey[nPeriod,lnI] = SUBSTR(CSTYLE,1,lnMajorLn)

          IF EMPTY(laStyles[nPeriod,lnI])
            laStyles[nPeriod,lnI]   = ALLTRIM(SUBSTR(CSTYLE,1,lnMajorLn))
            laStyleFul[nPeriod,lnI] = (LEN(laStyles[nPeriod,lnI]) >= 16)
          ELSE
            IF !laStyleFul[nPeriod,lnI]
              IF LEN(laStyles[nPeriod,lnI] + ',' + ALLTRIM(SUBSTR(CSTYLE,1,lnMajorLn))) > 16
                laStyles[nPeriod,lnI]   = laStyles[nPeriod,lnI] + ',...'
                laStyleFul[nPeriod,lnI] = .T.
              ELSE
                laStyles[nPeriod,lnI]   = laStyles[nPeriod,lnI] + ',' + ALLTRIM(SUBSTR(CSTYLE,1,lnMajorLn))
                laStyleFul[nPeriod,lnI] = (LEN(laStyles[nPeriod,lnI])=16)
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        laPlan[nPeriod,lnI]     = laPlan[nPeriod,lnI] + ((EVALUATE('NPLAN'+lcI) - EVALUATE('NWIP'+lcI)) * NYEALD)
        laLeadTime[nPeriod,lnI] = MIN(laLeadTime[nPeriod,lnI],EVALUATE('DLEADTIME'+lcI))
      ENDIF
    ENDFOR
  ENDSCAN
ENDIF

*--Update main record...
FOR lnPeriod = 1 TO lnFrcstPrds
  lnPrdLine = INT((lnPeriod - 1) / 10) + 1
  lcPeriod  = ALLTRIM(STR(MOD(lnPeriod - 1, 10)))
  OnReqFld  = IIF(lcPeriod = '0', 'CurrReq', 'OnReq' + lcPeriod)

  IF lcRpReqBas = 'B' .AND. lnMonthEd / 7 > 1
    IF lcRpRName = 'MATIMRQD'
      **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
      *FOR lnI = 1 TO ALEN(laWeeks,1)
      FOR lnI = 1 TO lnMonthEd / 7
        *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]
        SELECT (lcMatReq)
        APPEND BLANK
        REPLACE nLineNo  WITH lnPrdLine,;
          ITEM     WITH SUBSTR(lcRecKey,1,19),;
          CRECTYPE WITH 'D',;
          CTYPE    WITH 'Plan',;
          DDATE    WITH IIF(lnPeriod = lnFrcstPrds, {}, laLeadTime[lnPeriod,lnI]),;
          CTRANCD  WITH '',;
          CSTYLES  WITH laStyles[lnPeriod,lnI],;
          TOTAL    WITH -laPlan[lnPeriod,lnI]
        REPLACE (IIF(lcPeriod = '0', 'CurrHnd', 'OnHnd' + lcPeriod)) WITH -laPlan[lnPeriod,lnI]
      ENDFOR
    ENDIF
    lnRequied[lnPeriod] = 0

    **B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[Start]
    *FOR lnI = 1 TO ALEN(laWeeks,1)
    FOR lnI = 1 TO IIF(lnMonthEd / 7 > 1,lnMonthEd / 7,4)
      * lnRequied[lnPeriod] = lnRequied[lnPeriod] + laWeeks[lnI + 4 * (lnPeriod - 1),1] + laPlan[lnPeriod,lnI]
      lnRequied[lnPeriod] = lnRequied[lnPeriod] + laWeeks[lnI + IIF(lnMonthEd / 7 > 1,lnMonthEd / 7,4) * (lnPeriod - 1),1] + laPlan[lnPeriod,lnI]
      *B607945,1 MMT 01/22/2007 fix bug of wrong on hand in case of extended size scale[End]
    ENDFOR

  ELSE
    IF lcRpReqBas = 'B'
      IF lcRpRName = 'MATIMRQD'
        SELECT (lcMatReq)
        APPEND BLANK
        REPLACE nLineno  WITH lnPrdLine,;
          ITEM     WITH SUBSTR(lcRecKey,1,19),;
          CRECTYPE WITH 'D',;
          CTYPE    WITH 'Plan',;
          DDATE    WITH IIF(lnPeriod = lnFrcstPrds,{},laLeadTime[lnPeriod,1]),;
          CTRANCD  WITH '',;
          CSTYLES  WITH laStyles[lnPeriod,1],;
          TOTAL    WITH -laPlan[lnPeriod,1]
        REPLACE (IIF(lcPeriod = '0', 'CurrHnd', 'OnHnd' + lcPeriod)) WITH -laPlan[lnPeriod,1]
      ENDIF
      lnRequied[lnPeriod] = lnRequied[lnPeriod] + laPlan[lnPeriod,1]
    ELSE
      IF lcRpReqBas = 'P'
        lnRequied[lnPeriod] = MAX(lnRequied[lnPeriod], lnPlanReq[lnPeriod])
      ENDIF
    ENDIF
  ENDIF

  SELECT (lcMatReq)
  IF !SEEK(lcRecKey+STR(lnPrdLine,4))
    =SEEK(lcRecKey+STR(1,4))
    m.Item = ITEM
    m.Desc = DESC

    APPEND BLANK
    REPLACE nLineno  WITH lnPrdLine,;
      ITEM     WITH m.Item,;
      DESC     WITH m.Desc
  ENDIF

  REPLACE (OnReqFld) WITH EVALUATE(OnReqFld) + lnRequied[lnPeriod]
ENDFOR

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfGetRef
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/16/2002
*! Purpose   : Get the account and SO number for C/T or PO
*!*************************************************************
*! Passed Parameters : lcSeek = expration to seek in cutpick file
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetRef()
*!*************************************************************
FUNCTION lfGetRef
LPARAMETERS lcSeek

LOCAL lcRef, lcOrder, lnAlias
lcRef = ''

IF TYPE("loOrdHdr") # 'O'
  loOrdHdr   = CREATEOBJECT('RemoteTable', 'ORDHDR', 'ORDHDR', 'ORDHDR', SET("Datasession"))
ENDIF

IF TYPE("loCUTPICK") # 'O'
  loCUTPICK     = CREATEOBJECT('RemoteTable', 'CUTPICK', 'CUTPICK', 'CUTPICK', SET("Datasession"))
ENDIF

IF loCUTPICK.SEEK(lcSeek) .AND. loORDHDR.SEEK('O'+CUTPICK.ORDER, .F., .T.)
  lcRef   = ORDHDR.ACCOUNT + ',' + CUTPICK.ORDER
  lcOrder = CUTPICK.ORDER
  lnAlias = SELECT(0)

  SELECT CUTPICK
  LOCATE REST WHILE TranCd+cTktNo+STYLE = lcSeek FOR ORDER <> lcOrder
  IF FOUND()
    lcRef = lcRef + ',...'
  ENDIF

  SELECT (lnAlias)
ENDIF

RETURN lcRef

*!*************************************************************
*! Name      : lfWeekStr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Get the start working day.
*!*************************************************************
FUNCTION lfWeekStr
LPARAMETERS ldDay

IF lcRpReqBas = 'F'
  ldRetDay = ldDay - DOW(ldDay) + 1
ELSE
  ldRetDay = ldDay - DOW(ldDay) + lnWorkDay + 1
ENDIF

IF ldDay < ldRetDay
  ldRetDay = ldRetDay - 7
ENDIF

RETURN ldRetDay

*!*************************************************************
*! Name      : lfHolDay
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Get the start day of interval of date after removing the Holidaies from it.
*!*************************************************************
FUNCTION lfHolDay
LPARAMETERS ldFromDate, ldToDate

LOCAL ldCheckDay, lnWeekEnd, lcWeekEnd, lnAlias
lnAlias    = SELECT(0)
ldCheckDay = ldToDate

DO WHILE ldCheckDay >= ldFromDate
  *-- Check if it is weekend day
  lnWeekEnd = DOW(ldCheckDay) - 1
  lnWeekEnd = IIF(lnWeekEnd = 0, 7, lnWeekEnd)
  lcWeekEnd = STR(lnWeekEnd,1)

  SELECT FISHD
  IF loFISHD.GOTOP()
    DO WHILE !BETWEEN(ldCheckDay, DFISBGDAT, DFISENDAT)
      IF !loFISHD.GONEXT()
        EXIT
      ENDIF
    ENDDO
  ENDIF

  IF !EOF() AND BETWEEN(ldCheckDay, DFISBGDAT, DFISENDAT) AND lcWeekEnd $ FISHD.CFISNONWD
    ldFromDate = ldFromDate - 1
    ldCheckDay = ldCheckDay - 1
    LOOP
  ENDIF

  *-- Check if it is holiday
  IF loFISHD.SEEK(FISHD.CFISFYEAR+DTOS(ldCheckDay))
    ldFromDate = ldFromDate - 1
  ENDIF
  ldCheckDay = ldCheckDay - 1
ENDDO

SELECT (lnAlias)
RETURN ldFromDate

*!*************************************************************
*! Name      : lfGetStrt
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Get Start and End of the month.
*!*************************************************************
*! Example     : =lfGetStrt()
*!*************************************************************
FUNCTION lfGetStrt

IF lcRpReqBas = 'F'
  lnMonthEd = 7
  RETURN
ENDIF

DO CASE
  *-- Calculate the end of period based on the O.T.S setting
CASE lcPerLen = 'W'
  lnMonthEd = 7
CASE lcPerLen = 'E'
  lnMonthEd = 14
OTHERWISE
  lnMonthEd = 28
ENDCASE

RETURN

*!*************************************************************
*! Name      : lfGetDesc
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/26/2005
*! Purpose   : Get Fabric and Color Description.
*!*************************************************************
*! Example   : =lfGetDesc()
*!*************************************************************
FUNCTION lfGetDesc

=loFabric.SEEK('0002' + ITEM)

lcDesc      = Fabric.DESC
lcColorDesc = SUBSTR(gfCodDes(SUBSTR(ITEM, lnFClrSrt, lnClrEnd), 'COLOR'), 1, 15)

RETURN lcDesc + " " + lcColorDesc

*!*************************************************************
*! Name      : lfStyleReq
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/30/2005
*! Purpose   : Get style forcast quantitiy
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfStyleReq(loFormSet, lcStyle)
*!*************************************************************
FUNCTION lfStyleReq
LPARAMETERS lcStyle, ldFromDate, ldToDate

LOCAL llForcastLines, lnAlias, lcOrdTag, lcInvTag
lnAlias = SELECT(0)

*!*  SELECT (lcTmpFrCst)
*!*  ZAP

lcOrdTag = loOrdLine.lcTagName
lcInvTag = loInvLine.lcTagName

=loOrdLine.SetOrder("ORDLINES")
=loInvLine.SetOrder("INVLINEO")

llForcastLines  = .F.
IF loForCast.SEEK(lcStyle)
  IF loPOSLN.SEEK('0001'+lcStyle+'PU')
    SELECT POSLN
    DO WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+lcStyle+'PU'
      lcPo  = PO
      llWIP = loPosHdr.SEEK('PU'+lcPo) AND BETWEEN(POSHDR.START, ldFromDate, ldToDate) AND POSHDR.STATUS <> 'X'

      STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8

      SCAN REST WHILE cInvType+STYLE+cBusDocu+cStyType+PO+STR(LINENO,6)+TRANCD = '0001'+lcStyle+'PU'+lcPo FOR llWIP
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          lnQty&lcCount = lnQty&lcCount + IIF(TranCd = '1', Qty&lcCount , -1 * Qty&lcCount)
        ENDFOR
      ENDSCAN

      IF !SEEK(lcStyle+PADR(YEAR(POSHDR.START),4)+PADL(WEEK(POSHDR.START),2,'0'), lcTmpFrCst)
        INSERT INTO (lcTmpFrCst) (STYLE, cYear, cWeek) VALUES (lcStyle,PADR(YEAR(POSHDR.START),4),PADL(WEEK(POSHDR.START),2,'0'))
      ENDIF
      REPLACE Qty1 WITH Qty1 + lnQty1,;
        Qty2 WITH Qty2 + lnQty2,;
        Qty3 WITH Qty3 + lnQty3,;
        Qty4 WITH Qty4 + lnQty4,;
        Qty5 WITH Qty5 + lnQty5,;
        Qty6 WITH Qty6 + lnQty6,;
        Qty7 WITH Qty7 + lnQty7,;
        Qty8 WITH Qty8 + lnQty8 IN (lcTmpFrCst)
    ENDDO
  ENDIF

  SELECT FORCAST
  LOCATE REST WHILE STYLE+STR(nyear,4)+STR(nweek,2) = lcStyle ;
    FOR   BETWEEN(nYear, YEAR(ldFromDate) + 1, YEAR(ldToDate) - 1) OR;
    (nYear = YEAR(ldFromDate) AND nWeek >= WEEK(ldFromDate)) OR;
    (nYear = YEAR(ldToDate) AND nWeek <= WEEK(ldToDate))
  IF FOUND()
    lnStk1 = STYLE.Stk1
    lnStk2 = STYLE.Stk2
    lnStk3 = STYLE.Stk3
    lnStk4 = STYLE.Stk4
    lnStk5 = STYLE.Stk5
    lnStk6 = STYLE.Stk6
    lnStk7 = STYLE.Stk7
    lnStk8 = STYLE.Stk8
    SCAN REST WHILE STYLE+STR(nyear,4)+STR(nweek,2) = lcStyle ;
        FOR   BETWEEN(nYear, YEAR(ldFromDate) + 1, YEAR(ldToDate) - 1) OR;
        (nYear = YEAR(ldFromDate) AND nWeek >= WEEK(ldFromDate)) OR;
        (nYear = YEAR(ldToDate) AND nWeek <= WEEK(ldToDate))
      lnForCst1 = nForQty1
      lnForCst2 = nForQty2
      lnForCst3 = nForQty3
      lnForCst4 = nForQty4
      lnForCst5 = nForQty5
      lnForCst6 = nForQty6
      lnForCst7 = nForQty7
      lnForCst8 = nForQty8
      ldForCstD = lfvWeek(PADR(nYear,4), nWeek)
      IF loOrdLine.SEEK(lcStyle)
        SELECT OrdLine
        SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = lcStyle
          IF BETWEEN(COMPLETE, ldForCstD, ldForCstD+6)
            lnForCst1 = lnForCst1 - OrdLine.Qty1
            lnForCst2 = lnForCst2 - OrdLine.Qty2
            lnForCst3 = lnForCst3 - OrdLine.Qty3
            lnForCst4 = lnForCst4 - OrdLine.Qty4
            lnForCst5 = lnForCst5 - OrdLine.Qty5
            lnForCst6 = lnForCst6 - OrdLine.Qty6
            lnForCst7 = lnForCst7 - OrdLine.Qty7
            lnForCst8 = lnForCst8 - OrdLine.Qty8
            IF loInvLine.SEEK(OrdLine.ORDER+STR(OrdLine.LINENO,6))
              SELECT INVLINE
              SCAN REST WHILE ORDER+STR(LINENO,6)+invoice = OrdLine.ORDER+STR(OrdLine.LINENO,6)
                IF BETWEEN(InvDate,ldForCstD,ldForCstD+6)
                  lnForCst1 = lnForCst1 - InvLine.Qty1
                  lnForCst2 = lnForCst2 - InvLine.Qty2
                  lnForCst3 = lnForCst3 - InvLine.Qty3
                  lnForCst4 = lnForCst4 - InvLine.Qty4
                  lnForCst5 = lnForCst5 - InvLine.Qty5
                  lnForCst6 = lnForCst6 - InvLine.Qty6
                  lnForCst7 = lnForCst7 - InvLine.Qty7
                  lnForCst8 = lnForCst8 - InvLine.Qty8
                ENDIF
              ENDSCAN
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF

      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        lnForCst&lcCount = MAX(lnForCst&lcCount,0)
        IF lnStk&lcCount >= lnForCst&lcCount
          lnPlan&lcCount = 0
          lnStk&lcCount  = lnStk&lcCount - lnForCst&lcCount
        ELSE
          lnPlan&lcCount = lnForCst&lcCount - lnStk&lcCount
          lnStk&lcCount  = 0
          SELECT (lcTmpFrCst)
          =SEEK(lcStyle)
          SCAN REST WHILE STYLE = lcStyle FOR (VAL(cYear) < Forcast.nYear OR (VAL(cYear) = Forcast.nYear AND VAL(cWeek) <= Forcast.nWeek)) AND lnPlan&lcCount > 0
            IF lnPlan&lcCount - Qty&lcCount >= 0
              lnPlan&lcCount = lnPlan&lcCount - Qty&lcCount
              REPLACE Qty&lcCount WITH 0
            ELSE
              REPLACE Qty&lcCount WITH Qty&lcCount - lnPlan&lcCount
              lnPlan&lcCount = 0
            ENDIF
          ENDSCAN
        ENDIF
      ENDFOR

      IF lnPlan1+lnPlan2+lnPlan3+lnPlan4+lnPlan5+lnPlan6+lnPlan7+lnPlan8 > 0
        m.Style   = FORCAST.STYLE
        m.dFrDate = ldForCstD
        m.PLAN1   = lnPlan1
        m.PLAN2   = lnPlan2
        m.PLAN3   = lnPlan3
        m.PLAN4   = lnPlan4
        m.PLAN5   = lnPlan5
        m.PLAN6   = lnPlan6
        m.PLAN7   = lnPlan7
        m.PLAN8   = lnPlan8
        m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8
        llForcastLines = .T.
        INSERT INTO (lcStyTmp) FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

=loOrdLine.SetOrder(lcOrdTag)
=loInvLine.SetOrder(lcInvTag)

SELECT (lnAlias)
RETURN llForcastLines

*!*************************************************************
*! Name      : lfvWeek
*: Developer : Wael Ali MOhamed
*: DATE      : 06/30/2005
*! Purpose   : GEt Week start date
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcYear, lnWeek
*!*************************************************************
*! Returns   : None.
*!*************************************************************
FUNCTION lfvWeek
LPARAMETERS lcYear, lnWeek

ldDate      = CTOD('01/01/' + lcYear)
ldEndDate   = GOMONTH(ldDate, 12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate - DOW(ldEndDate)) - (ldDate - DOW(ldDate) + 1)) / 7), 1)
St_Day      = ldDate - DOW(ldDate)+1

IF lnWeek = 1
  ldDispDate = St_Day
ELSE
  ldDispDate = ((lnWeek - 1) * 7) + St_Day
ENDIF

RETURN ldDispDate

*!*************************************************************
*! Name      : lfTRemain
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 07/06/2005
*! Purpose   : Compute remaining for Time Phased MRP
*!*************************************************************
*! Returns   : lnRemain -> Remaining.
*!*************************************************************
FUNCTION lfTRemain

IF !EMPTY(CRECTYPE) OR !loFabric.SEEK('0002' + ITEM)
  RETURN CurrBal
ENDIF

PRIVATE lnRequired, lcFDesc, lcFabColor, lcClrDsc, lcFabVen, lnConv, lnLeadTm, lnFabcost, lcUOMBuy, ldComplete, ldAvailable
lcFDesc    = FABRIC.DESC
lcFabColor = SUBSTR(Fabric.STYLE, lnFClrSrt, lnFClrEnd)
lcClrDsc   = gfCodDes(lcFabColor, 'COLOR     ')
lcFabVen   = FABRIC.Vendor
lnLeadTm   = FABRIC.LeadTime
lnFabcost  = FABRIC.nICost1
lnConv     = 1
lcUOMBuy   = ''

IF loUOM.SEEK(FABRIC.cConvBuy)
  lnConv   = Uom.nConf
  lcUOMBuy = Uom.cUom_b
ENDIF


*B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [Start]
*IF llVenRef .AND. loVENDMATL.SEEK(PADR(lcFabVen,8)+PADR(ITEM,7) + SUBSTR(ITEM, lnFClrSrt, lnFClrEnd))
IF llVenRef .AND. loVENDMATL.SEEK(PADR(lcFabVen,8)+PADR(SUBSTR(ITEM,1,lnFMajLen),19) + PADR(SUBSTR(ITEM, lnFClrSrt, lnFClrEnd),6))
  *B608879,1 MMT 05/30/2009 Fix bug of error while update while saving POs [End]

  lnLeadTm = VENDMATL.LEADTIME
ENDIF

LOCAL lnPeriod, lcPeriod, lnRemain, llLastGenerated, ldFirstDay
ldFirstDay = lfWeekStr(ldRPStart)

FOR lnPeriod = 0 TO 9
  lcPeriod    = ALLTRIM(STR(lnPeriod))
  ldComplete  = ldFirstDay + (lnPeriod * lnMonthEd) - lnLeadTm
  ldAvailable = ldFirstDay + (lnPeriod * lnMonthEd)

  IF lnPeriod = 0
    lnRemain = CurrHnd + CurrOrd - CurrReq
  ELSE
    *lnRemain = IIF(!llLastGenerated, OnHnd&lcPeriod, MAX(OnHnd&lcPeriod, 0)) + OnOrd&lcPeriod - OnReq&lcPeriod
    lnRemain = IIF(lcRpReqBas = 'F', OnHnd&lcPeriod, lnRemain) + OnOrd&lcPeriod - OnReq&lcPeriod
  ENDIF

  IF lnRemain > 0
    llLastGenerated = .F.
  ENDIF

  *--Check negative remaining for fabrics and trims.
  IF lnRemain < 0
    *IF ldComplete >= oAriaApplication.SystemDate
    llLastGenerated = .T.

    *-- Take remaining qty insted of requirement.
    lnRequired = ABS(lnRemain)
    IF lcRpMrpBas = 'N'
      =lfSavNRItm()
    ENDIF

    lnRemain = 0
    *ENDIF
  ENDIF
ENDFOR

RETURN CurrBal

*!*************************************************************
*! Name      : lfvStDate
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 07/06/2005
*! Purpose   : Valid function for Start and Forcast Dates in OG
*!*************************************************************
*! Returns   : lfvStDate
*!*************************************************************
FUNCTION lfvStDate
LPARAMETERS lcDateType

IF lcDateType = 'S'
  ldRpFrDate = ldRpStart
ELSE
  ldRpStart   = ldRpFrDate
  ldRpFrcstTo = ldRpFrDate + 63
  loOgScroll.RefreshScroll()
ENDIF

RETURN

*!*************************************************************
*! Name      : lfFillVar
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 08/26/2012
*! Purpose   : Fill variables used on footer
*!*************************************************************
*! Returns   :
*!*************************************************************
FUNCTION lfFillVar
*B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]
  
REPLACE nOnHand WITH  lnOnHand IN (lcMatReq)
REPLACE nOnOrdr WITH  lnOnOrdr IN (lcMatReq)
REPLACE nOnAvlb WITH (lnOnHand + lnOnOrdr) IN (lcMatReq)

REPLACE NOREQ WITH lnOReq IN (lcMatReq)
REPLACE nHReq WITH lnHReq IN (lcMatReq)
REPLACE nPReq WITH lnPReq IN (lcMatReq)

*B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]

RETURN
*!*************************************************************
*! Name      : lfPrpToXls
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 08/26/2012
*! Purpose   : Prepare data to be exported to Excel
*!*************************************************************
*! Returns   :
*!*************************************************************
FUNCTION lfPrpToXls
*B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][Begin]

IF loogscroll.cTextRepType == "EXCEL"

  lcFileOld   = oAriaApplication.gcoutFile
  lcOldDevice = oAriaApplication.gcDevice
  lcOldType  = loogscroll.cTextRepType

  oAriaApplication.gcDevice = 'FILE'
  oAriaApplication.gcoutFile = oAriaApplication.Workdir+loogscroll.gftempName()+'.TXT'
  loogscroll.cTextRepType = ""
  loogScroll.cCROrientation = 'P'
  loOGScroll.cCRPapersize = 'LETTER'

  SELECT (lcMatReq)
  SET STEP ON 
DEBUG  
  DO gfDispRe WITH EVAL('lcRpRname') 
  SELECT (lcMatReq)
  oAriaApplication.gcDevice  = lcOldDevice
  oAriaApplication.gcoutFile = lcFileOld
  loogscroll.cTextRepType    = lcOldType
ENDIF
*B610055,1 HIA 08/23/2012 Material Requirments report - Missing fields when export to Excel [T20120719.0001][End]

RETURN