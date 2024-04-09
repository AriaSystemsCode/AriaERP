*!*****************************************************************************************
*! Name      : PMPRJSCH.prg
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/10/2009
*! Purpose   : Scheduling From Request builder
*! Entry no. : N037574
*!*****************************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include r:\aria4xp\reports\sm\PMPRJSCH.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
FUNCTION LFWHENFN
DIMENSION laPrjDes[2],laPrjVal[2]
STORE '' TO laPrjDes,laPrjVal
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	laPrjDes[1] = 'Style'
*!*	laPrjVal[1] = 'S'
*!*	laPrjDes[2] = 'Other'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[1] = LANG_STYLE
laPrjDes[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STYLE,oAriaApplication.GetHeaderText("LANG_STYLE",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laPrjVal[1] = 'S'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[2] = LANG_OTHER
laPrjDes[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OTHER,oAriaApplication.GetHeaderText("LANG_OTHER",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/11/2013 Globalization changes[END]
laPrjVal[2] = 'H'
lnValCnt = 3

IF 'MF' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+3],laPrjVal[ALEN(laPrjVal)+3]
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	  laPrjDes[lnValCnt] = 'Cutting Ticket'
*!*	  laPrjVal[lnValCnt] = 'C'
*!*	  lnValCnt = lnValCnt + 1
*!*	  laPrjDes[lnValCnt] = 'Adornment Order'
*!*	  laPrjVal[lnValCnt] = 'A'
*!*	  lnValCnt = lnValCnt + 1
*!*	  laPrjDes[lnValCnt] = 'Dye Order'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_CUTTINGTICKET
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUTTINGTICKET,oAriaApplication.GetHeaderText("LANG_CUTTINGTICKET",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  laPrjVal[lnValCnt] = 'C'
  lnValCnt = lnValCnt + 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_ADORMENTORDER
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ADORMENTORDER,oAriaApplication.GetHeaderText("LANG_ADORMENTORDER",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  laPrjVal[lnValCnt] = 'A'
  lnValCnt = lnValCnt + 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_DYEORDER
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DYEORDER,oAriaApplication.GetHeaderText("LANG_DYEORDER",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[END]
  laPrjVal[lnValCnt] = 'D'
  lnValCnt = lnValCnt + 1
ENDIF

IF 'MA' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+1],laPrjVal[ALEN(laPrjVal)+1]
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  *laPrjDes[lnValCnt] = 'Material'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_MATERIAL
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MATERIAL,oAriaApplication.GetHeaderText("LANG_MATERIAL",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  laPrjVal[lnValCnt] = 'M'
  lnValCnt = lnValCnt + 1
  =gfOpenTable("ITEM",'Cstyle')
ENDIF

IF 'PO' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+3],laPrjVal[ALEN(laPrjVal)+3]
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	  laPrjDes[lnValCnt] = 'Purchase Order'
*!*	  laPrjVal[lnValCnt] = 'P'
*!*	  lnValCnt = lnValCnt + 1
*!*	  laPrjDes[lnValCnt] = 'Inter-Location P/O'
*!*	  laPrjVal[lnValCnt] = 'N'
*!*	  lnValCnt = lnValCnt + 1
*!*	  laPrjDes[lnValCnt] = 'Return P/O'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_PO
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PO,oAriaApplication.GetHeaderText("LANG_PO",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  laPrjVal[lnValCnt] = 'P'
  lnValCnt = lnValCnt + 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_INTERPO
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INTERPO,oAriaApplication.GetHeaderText("LANG_INTERPO",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  laPrjVal[lnValCnt] = 'N'
  lnValCnt = lnValCnt + 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_RETPO
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RETPO,oAriaApplication.GetHeaderText("LANG_RETPO",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  laPrjVal[lnValCnt] = 'R'
  lnValCnt = lnValCnt + 1
ENDIF

IF 'SO' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+2],laPrjVal[ALEN(laPrjVal)+2]
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	  laPrjDes[lnValCnt] =  'Sales Order'
*!*	  laPrjVal[lnValCnt] = 'O'
*!*	  lnValCnt = lnValCnt + 1
*!*	  laPrjDes[lnValCnt] = 'EDI temporary Order '
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] =  LANG_SO
laPrjDes[lnValCnt] =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SO,oAriaApplication.GetHeaderText("LANG_SO",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  laPrjVal[lnValCnt] = 'O'
  lnValCnt = lnValCnt + 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laPrjDes[lnValCnt] = LANG_EDITEMP
laPrjDes[lnValCnt] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_EDITEMP,oAriaApplication.GetHeaderText("LANG_EDITEMP",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[ENd]
  laPrjVal[lnValCnt] = 'T'
  lnValCnt = lnValCnt + 1
ENDIF

IF (TYPE('llSchedule') <> 'U' AND  llSchedule) AND (TYPE('lcPrj_Typ') <> 'U') AND (TYPE('lcXmlFlName') <> 'U')
  lcRpPrjTyp = lcPrj_Typ
  ldRpSchDt = dSch_Date
  lcTempPrj = loogscroll.gfTempName()
  CREATE CURSOR (lcTempPrj ) (KeyExp C(20))
  INSERT INTO (lcTempPrj ) VALUES (PADR(lcPrj_ID,6)+'-'+lcStyle)
  lnPrjId= ASCAN(loOgScroll.laOgFXFlt,"PMPRJHD.CPRJ_ID")
  IF lnPrjId > 0
    lnPrjId = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPrjId,1)
    loOgScroll.laOgFxFlt[lnPrjId,6] = lcTempPrj
  ENDIF
  lcXmlStr = loogScroll.convertvariablestoxml()
  STRTOFILE(lcXmlStr,oAriaApplication.WorkDir+lcXmlFlName+'.xml')
ENDIF

*!*************************************************************
*! Name      : lfVPrjType
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Validate Project type
*!*************************************************************
FUNCTION lfVPrjType

lnPrjId= ASCAN(loOgScroll.laOgFXFlt,"PMPRJHD.CPRJ_ID")


IF lnPrjId > 0
  lnPrjId = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPrjId,1)
  lcCursorPrj= loOgScroll.laOgFxFlt[lnPrjId,6]
  SELECT(lcCursorPrj)
  ZAP
  clearread()
ENDIF
