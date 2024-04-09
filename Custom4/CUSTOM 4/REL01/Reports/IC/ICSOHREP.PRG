***********************************************************************
*:  Program File: ICSOHRE.PRG
*:  Desc.       : Custom SOH report for RELIQ
*:  System      : Aria 4XP
*:  Developer   : MMT - Mariam Mazhar
*:  Date        : 07/14/2021
*:  Reference   : C202426
*:************************************************************************ 
#INCLUDE R:\ARIA4XP\REPORTS\icsrank.H

WAIT WINDOW 'Collecting report data...' NOWAIT 
lcCursorStyle = ''
llStySelect = .F.
lnPosStyle = ASCAN(loOgScroll.laOGFXFlt,"STYLE.CSTYMAJOR")
IF lnPosStyle > 0
  lnPosStyle = ASUBSCRIPT(loOGScroll.laOGFXFlt,lnPosStyle,1)
  lcCursorStyle= loOgScroll.laOGFXFlt[lnPosStyle,6]
  IF !EMPTY(lcCursorStyle) AND USED(lcCursorStyle)
    SELECT(lcCursorStyle)
    LOCATE
    IF !EOF()
      llStySelect = .T.
    ENDIF
  ENDIF
ENDIF
lcDivs = ''
llUseDiv = .F.

lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'STYLE.CDIVISION'),1)
IF lnPosition > 0
  lcDivs = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  lcDivFile = loogscroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.) AND lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
ENDIF

*--season
lcSeasons = ''
llUseSeas = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'STYLE.SEASON'),1)
IF lnPosition > 0
  lcSeasons = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  lcSeaFile = loogscroll.gfTempName()
  llUseSeas = IIF(LEN(lcSeasons)>0,.T.,.F.) AND lfConvertToCursor(lcSeasons,'SEASON',lcSeaFile)
ENDIF


*--Group
lcGroups = ''
llUseGroup = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGroups  = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  lcGrpFile = loogscroll.gfTempName()
  llUseGroup = IIF(LEN(lcGroups)>0,.T.,.F.) AND lfConvertToCursor(lcGroups,'CSTYGROUP',lcGrpFile)
ENDIF


*color
lcColor = ''
llUseColor = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
IF lnPosition > 0
  lcColor    = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  lcClrFile  = loogscroll.gfTempName()
  llUseColor  = IIF(LEN(lcColor)>0,.T.,.F.) AND lfConvertToCursor(lcColor,'COLOR',lcClrFile)
ENDIF


*Pattern
lcPattern = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'STYLE.PATTERN'),1)
IF lnPosition > 0
  lcPattern  = LOOGSCROLL.laOGVRFlt[lnPosition,6]
ENDIF

lcWhereCondition = "!DELETED() "

IF llUseDiv 
  lcWhereCondition = lcWhereCondition + IIF(!EMPTY(lcWhereCondition),' AND ','')+"SEEK(STYLE.cDivision,'"+lcDivFile +"')"
ENDIF

IF llUseSeas 
  lcWhereCondition = lcWhereCondition + IIF(!EMPTY(lcWhereCondition),' AND ','')+"SEEK(STYLE.SEASON,'"+lcSeaFile +"')"
ENDIF

IF llUseGroup 
  lcWhereCondition = lcWhereCondition + IIF(!EMPTY(lcWhereCondition),' AND ','')+"SEEK(STYLE.CSTYGROUP,'"+lcGrpFile +"')"
ENDIF

IF llStySelect 
  lcWhereCondition = lcWhereCondition + IIF(!EMPTY(lcWhereCondition),' AND ','')+"SEEK(STYLE.CSTYMAJOR,'"+lcCursorStyle+"')"
ENDIF

IF !EMPTY(lcPattern)
  lcWhereCondition = lcWhereCondition + IIF(!EMPTY(lcWhereCondition),' AND ','')+"STYLE.PATTERN ='"+lcPattern+"'"
ENDIF

IF llUseColor  
  lcWhereCondition = lcWhereCondition + IIF(!EMPTY(lcWhereCondition),' AND ','')+"SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'"+lcClrFile  +"')"
ENDIF

IF !USED('STYLE')
  =gfOpenTable("Style","Style",'SH')
ENDIF
  
lcTempFile = gfTempName()
SELECT * FROM STYLE WHERE &lcWhereCondition. INTO DBF (oAriaApplication.WorkDir+lcTempFile+".DBF") 

IF RECCOUNT(lcTempFile) = 0
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF 

DIMENSION loOGScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOGScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcTempFile+'.DBF'
USE IN (lcTempFile)
= gfDispRe("ICSOH")

RETURN 
*!*	oCrystal = CreateObject("CrystalRunTime.Application")
*!*	*oCrystal = CreateObject("CrystalDesignRunTime.Application.13")
*!*	lcReportFileName = ADDBS(UPPER(oAriaApplication.ClientReportHome))+"\IC\ICSOH.rpt"
*!*	oReport  = oCrystal.OpenReport(lcReportFileName)
*!*	LOCAL lcDataPath As String
*!*	oReport.EnableParameterPrompting = .F.
*!*	LOCAL lcLastSubReport, lnTableIndex
*!*	lcLastSubReport = ""
*!*	lnTableIndex    = 0
*!*	 =gfOpenTable("Style","Style",'SH')
*!*	lcTempFile = gfTempName()
*!*	SELECT * FROM STYLE WHERE !DELETED() INTO DBF (oAriaApplication.WorkDir+lcTempFile+".DBF") 
*!*	lnTableIndex = 1
*!*	oReport.Database.Tables.Item[lnTableIndex].Setlogoninfo (oAriaApplication.WorkDir+lcTempFile+".DBF")
*!*	oReport.Database.Tables.Item[lnTableIndex].SetTableLocation (oAriaApplication.WorkDir+lcTempFile+".DBF",'','')
*!*	lcConnBuff ="Provider=VFPOLEDB.1;Data Source="+oAriaApplication.WorkDir+lcTempFile+".DBF"+";Password=''"
*!*	oReport.Database.Tables.Item[lnTableIndex].SetTableLocation (JUSTSTEM(oAriaApplication.WorkDir+lcTempFile+".DBF"),JUSTSTEM(oAriaApplication.WorkDir+lcTempFile+".DBF"),lcConnBuff)
*!*	oReport.Database.Tables.Item(lnTableIndex).TestConnectivity()
*!*	oReport.DiscardSavedData()
*!*	WAIT WINDOW 'Collecting report data...' NOWAIT 
*!*	*oReport.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
*!*	oReport.CaseInsensitiveSQLData = .T.
*!*	loViewer = NEWOBJECT("crviewer",ADDBS(oAriaApplication.ClassDir) + "Crystal.vcx")
*!*	WITH loViewer
*!*	    .caption  = "Stock on Hand Report"
*!*	    .oCrystal = oCrystal 
*!*	    .oReport  = oReport
*!*	    .mSetup()
*!*	    .Show()
*!*	    .Destroy()
*!*	  ENDWITH
*!*	  oReport  = NULL
*!*	  oCrystal = NULL
*!*	  WAIT CLEAR 
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
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
*  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
  lcColorTlt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ONLYCOLORS,oAriaApplication.GetHeaderText("LANG_ONLYCOLORS",AHEADERFILE))+' ' +;
			   ALLTRIM(lcNonMajTl) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S,oAriaApplication.GetHeaderText("LANG_S",AHEADERFILE))
  * N000682 ,1 Thabet Handle globalization issues [END]
  *-- Compute Free/Color Items in Style code Structure. [End]

RETURN ''
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 09/14/2005
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
CASE   ALLTRIM(lcFieldName) = 'COLOR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYGROUP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

ENDCASE
= gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
FUNCTION lfwRepWhen