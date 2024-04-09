*:***************************************************************************
*: Program file  : ALSTLS1
*: Program desc. : ALLOCATION  STYLE LIST WITH AVAILABLE BY SIZE.
*! Date          : 07/17/2006
*: System        : Aria 4XP.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Issue Number  : Enh. N301184
*:***************************************************************************
*: Example : DO ALSTLS1
*:***************************************************************************
*: MODIFICATIONS:
*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[T20080716.0014]
*! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[T20100121.0032]
*! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign
*:***************************************************************************

*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
*! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[Start]
*lcMyLogo=loogscroll.lcLogoPath
*! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[End]
* N000862 ,1 Thabet Handle globalization issues [Start]
#Include R:\Aria4xp\Reports\Al\alstls1.h
* N000862 ,1 Thabet Handle globalization issues [END]
loOgScroll.lcOGLastForm ='ALSTLS1'

*! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[Start]
*!*	IF looGScroll.llShowLogo
*!*	  IF TYPE("looGScroll.lcLogoPath") = 'C' .AND. !EMPTY(looGScroll.lcLogoPath) .AND. !ISNULL(looGScroll.lcLogoPath) .AND.  (UPPER(RIGHT(looGScroll.lcLogoPath,3)) == 'BMP')
*!*	    IF looGScroll.FileExist(looGScroll.lcLogoPath)
*!*	      LOCAL loOleObj
*!*	      lcReportName = looGScroll.lcOGLastForm
*!*	      lcReportFileName = oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                       '\' + lcReportName + '.RPT'
*!*
*!*	      IF FILE(lcReportFileName)
*!*	      loMainCr = CREATEOBJECT('CrystalRuntime.Application')
*!*	      loMain = CREATEOBJECT('CrystalRuntime.Report')
*!*	      loMain = loMainCr.OpenReport(lcReportFileName)
*!*	      FOR lnI = 1 TO loMain.Sections.Item[2].ReportObjects.Count
*!*	          IF UPPER(loMain.Sections.Item[2].ReportObjects[lnI].Name) = 'PICTURE1'
*!*	      loMain.Sections.Item[2].DeleteObject (loMain.Sections.Item[2].ReportObjects[lnI])
*!*	      EXIT
*!*	        ENDIF
*!*	      ENDFOR
*!*
*!*	        loOleObj = loMain.Sections.Item[2].AddPictureObject(looGScroll.lcLogoPath, 20, 20)
*!*	        loOleObj.Width  = 1500
*!*	      loOleObj.Height = 1500
*!*	        loMain.Sections.Item[2].Height = loOleObj.Height + 500

*!*	      lcTempName = "_"+looGScroll.lcOGLastForm
*!*
*!*	      loMain.Save(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                       '\' + lcTempName + '.RPT')
*!*
*!*	        COPY FILE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                       '\' + lcTempName + '.RPT') TO  (lcReportFileName)
*!*
*!*	        ERASE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                       '\' + lcTempName + '.RPT')
*!*
*!*	     ENDIF
*!*	    ENDIF
*!*	  ELSE
*!*	    LOCAL loOleObj
*!*	    lcReportName = looGScroll.lcOGLastForm
*!*	    lcReportFileName = oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                     '\' + lcReportName + '.RPT'
*!*
*!*	    IF FILE(lcReportFileName)
*!*	      loMainCr = CREATEOBJECT('CrystalRuntime.Application')
*!*	      loMain = CREATEOBJECT('CrystalRuntime.Report')
*!*	      loMain = loMainCr.OpenReport(lcReportFileName)
*!*	      FOR lnI = 1 TO loMain.Sections.Item[2].ReportObjects.Count
*!*	        IF UPPER(loMain.Sections.Item[2].ReportObjects[lnI].Name) = 'PICTURE1'
*!*	          loMain.Sections.Item[2].DeleteObject (loMain.Sections.Item[2].ReportObjects[lnI])
*!*	           EXIT
*!*	        ENDIF
*!*	      ENDFOR
*!*	      lcTempName = "_"+looGScroll.lcOGLastForm
*!*	      loMain.Save(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                      '\' + lcTempName + '.RPT')
*!*
*!*	      COPY FILE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                       '\' + lcTempName + '.RPT') TO  (lcReportFileName)
*!*
*!*	      ERASE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                       '\' + lcTempName + '.RPT')
*!*	    ENDIF
*!*	  ENDIF
*!*	  loogscroll.lcLogoPath=''
*!*	ELSE
*!*	  LOCAL loOleObj
*!*	  lcReportName = looGScroll.lcOGLastForm
*!*	  lcReportFileName = oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                   '\' + lcReportName + '.RPT'
*!*
*!*	  IF FILE(lcReportFileName)
*!*	    loMainCr = CREATEOBJECT('CrystalRuntime.Application')
*!*	  loMain = CREATEOBJECT('CrystalRuntime.Report')
*!*	    loMain = loMainCr.OpenReport(lcReportFileName)
*!*	    FOR lnI = 1 TO loMain.Sections.Item[2].ReportObjects.Count
*!*	      IF UPPER(loMain.Sections.Item[2].ReportObjects[lnI].Name) = 'PICTURE1'
*!*	        loMain.Sections.Item[2].DeleteObject (loMain.Sections.Item[2].ReportObjects[lnI])
*!*	         EXIT
*!*	      ENDIF
*!*	    ENDFOR
*!*	    lcTempName = "_"+looGScroll.lcOGLastForm
*!*	    loMain.Save(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                    '\' + lcTempName + '.RPT')
*!*
*!*	    COPY FILE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                     '\' + lcTempName + '.RPT') TO  (lcReportFileName)
*!*
*!*	    ERASE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
*!*	                     '\' + lcTempName + '.RPT')

*!*	  ENDIF
*!*	ENDIF
loOgScroll.llShowlogoPerPage = .T.
*! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[End]
*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]


*! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [Start]
lnMajLen  = LEN(gfItemMask("PM"))
STORE  0 TO lnClrLen ,lnClrPos

DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
    EXIT
  ENDIF
ENDFOR
*! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [End]

IF llOgFltCh
  llDonprnt=.F.
  IF llFrsTime
    *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
    *lcMyLogo=loogscroll.lcLogoPath
    *loogscroll.lcLogoPath=''
    *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]

    llFrsTime =.F.

  ENDIF
  lnMajSeg  = LEN(gfItemMask('PM'))
  lnNmaj    = LEN(gfItemMask('PN'))

  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.PIKDATE'),1)
  LDATE = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
  HDATE = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)
  lcPERIOD   = IIF(EMPTY(LDATE)AND EMPTY(HDATE),'','Period: &LDATE- &HDATE')

  * Pickticket Filter
  lcPikFltr= lfCheckFilter(3, 'ORDLINE.PIKTKT')
  llPikFltr   = !EMPTY(lcPikFltr) AND USED(lcPikFltr) AND RECCOUNT(lcPikFltr) > 0
  IF llPikFltr
    SELECT (lcPikFltr)
    INDEX ON PIKTKT TAG (lcPikFltr)
  ELSE
    IF TYPE("lcPikFltr") = "C" AND USED(lcPikFltr)
      USE IN (lcPikFltr)
    ENDIF
    lcPikFltr= ''
  ENDIF

  llMultiWH  = (ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y')
  IF llMultiWH
    * Warehouse Filter
    lcWarFltr= lfCheckFilter(3, 'ORDLINE.CWARECODE')
    llWarFltr   = !EMPTY(lcWarFltr) AND USED(lcWarFltr) AND RECCOUNT(lcWarFltr) > 0
    IF llWarFltr
      SELECT (lcWarFltr)
      INDEX ON CWARECODE TAG (lcWarFltr)
    ELSE
      IF TYPE("lcWarFltr") = "C" AND USED(lcWarFltr)
        USE IN (lcWarFltr)
      ENDIF
      lcWarFltr= ''
    ENDIF
  ENDIF
  *Date Filter
  lcDatFil=''
  IF !EMPTY(LDATE)
    lcDatFil=" BETWEEN(PIKDATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"')) "
  ELSE
    IF  !EMPTY(HDATE)
      lcDatFil=" PIKDATE<=CTOD('"+HDATE+"') "
    ENDIF
  ENDIF
  llDatFil=IIF(EMPTY(lcDatFil),.F.,.T.)
  * PRIORITY Filter
  lcPriorty= lfCheckFilter(1, 'ORDHDR.PRIORITY')
  llPriorty=IIF(EMPTY(lcPriorty),.F.,.T.)

  *-- lcOrdlTemp  Variable that hold the temporary file name
  lcOrdlTemp = loOgScroll.gfTempName()         && TEMPORARY FILE FOR THE ORDER LINES
  SEC_INDEX  = loOgScroll.gfTempName()         && SECOND INDEX BY PIKTKT
  lcStyTitle = gfItemMask('HI')             && Style Title



  lcWorkfile =loOgScroll.gfTempName()

  *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
  *lcLogotmp=loOgScroll.gfTempName()
  *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]

  =lfBuildTmp()
  =CollData()
  SELECT (lcWorkfile )
  IF !RECCOUNT()>0
    llDonprnt=.T.
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')

    *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
    *! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[Start]
    *loogscroll.lcLogoPath =lcMyLogo
    *! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[End]
    *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]

    RETURN
  ENDIF


  INDEX ON SER+PIKTKT+ORDER+STORE+STYLE+STR(LINENO,6) TAG (lcWorkfile )

  =lfAdjustCRSettings()
  IF USED(lcWorkfile )
    USE IN (lcWorkfile )
  ENDIF

  *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
  *!*  IF USED(lcLogotmp)
  *!*      USE IN (lcLogotmp)
  *!*  ENDIF
  *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]

  =gfDispRe()



ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')

    *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
    *! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[Start]
    *loogscroll.lcLogoPath =lcMyLogo
    *! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[End]
    *: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]

    RETURN
  ELSE
    =gfDispRe()
  ENDIF
ENDIF  &&FILTER CHANGE

*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
*! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[Start]
*loogscroll.lcLogoPath =lcMyLogo
*! B609133,1 MMT 01/27/2010 Fix bug of wrong logo on Some report on SAAS[End]
*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]
*!*************************************************************
*! Name      : CollData
*! Developer : BASSEM RAFAAT
*! Date      : 07/18/2006
*! Purpose   : COLLECT THE DATA
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =CollData()
*!*************************************************************

FUNCTION  CollData

=CREATEFIL()
SELECT (lcOrdlTemp)
INDEX ON STYLE TAG &lcOrdlTemp UNIQUE
INDEX ON PIKTKT+ORDER+STORE+STYLE+STR(LINENO,6) TAG &SEC_INDEX UNIQUE

SET ORDER TO TAG &lcOrdlTemp
SET RELATION TO cordtype + ORDER INTO Ordhdr ADDI

SET RELATION TO STYLE INTO STYLE ADDITIVE

GOTO TOP

SCAN WHILE STYLE=''
  m.STYLE=LEFT(STYLE,lnMajSeg  )
  
  
  *! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [Start]
  *M.COLOR = RIGHT(STYLE,LNnMAJ+1)
  M.COLOR = Substr(STYLE,lnClrPOS,lnClrLen)
  *! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [End]
   
  m.DESC1=LEFT(STYLE.DESC1,28)
  m.STK1= IIF(STYLE.STK1 - STYLE.ALO1<=0,0,STYLE.STK1 - STYLE.ALO1)
  m.STK2= IIF(STYLE.STK2 - STYLE.ALO2<=0,0,STYLE.STK2 - STYLE.ALO2)
  m.STK3= IIF(STYLE.STK3 - STYLE.ALO3<=0,0,STYLE.STK3 - STYLE.ALO3)
  m.STK4= IIF(STYLE.STK4 - STYLE.ALO4<=0,0,STYLE.STK4 - STYLE.ALO4)
  m.STK5= IIF(STYLE.STK5 - STYLE.ALO5<=0,0,STYLE.STK5 - STYLE.ALO5)
  m.STK6= IIF(STYLE.STK6 - STYLE.ALO6<=0,0,STYLE.STK6 - STYLE.ALO6)
  m.STK7= IIF(STYLE.STK7 - STYLE.ALO7<=0,0,STYLE.STK7 - STYLE.ALO7)
  m.STK8= IIF(STYLE.STK8 - STYLE.ALO8<=0,0,STYLE.STK8 - STYLE.ALO8)
  m.TOTSTK= IIF(STYLE.TOTSTK - STYLE.TOTALO<=0,0,STYLE.TOTSTK - STYLE.TOTALO)
  m.ALO1= MAX(STYLE.ALO1,0)
  m.ALO2= MAX(STYLE.ALO2,0)
  m.ALO3= MAX(STYLE.ALO3,0)
  m.ALO4= MAX(STYLE.ALO4,0)
  m.ALO5= MAX(STYLE.ALO5,0)
  m.ALO6= MAX(STYLE.ALO6,0)
  m.ALO7= MAX(STYLE.ALO7,0)
  m.ALO8= MAX(STYLE.ALO8,0)
  m.TOTALO= MAX(STYLE.TOTALO,0)
  m.SER='1'
  m.ST = 'L'

  SELECT (LCWORKFILE)
  APPEND BLANK
  GATHER MEMO  MEMVAR
ENDSCAN



*SEC 2
SELE (lcOrdlTemp)
SET RELATION TO IIF(EMPTY(STORE) ,'M'+ACCOUNT, 'S'+ACCOUNT+STORE ) INTO CUSTOMER ADDI
SET ORDER TO TAG &SEC_INDEX
GOTO TOP
STORE 0 TO M.STK1, M.STK2, M.STK3, M.STK4, M.STK5, M.STK6, M.STK7, M.STK8, M.TOTSTK
STORE 0 TO M.ALO1, M.ALO2, M.ALO3, M.ALO4, M.ALO5, M.ALO6, M.ALO7, M.ALO8, M.TOTALO

SCAN WHILE PIKTKT+ORDER+STORE+STYLE+STR(LINENO,6)=''
  m.PIKTKT= PIKTKT
  m.ORDER= ORDER
  IF llMultiWH
    m.CWARECODE= IIF(SEEK(PIKTKT,'PIKTKT'),PIKTKT.CWARECODE,'')
  ENDIF
  m.PRIORITY  =ALLTRIM(ORDHDR.PRIORITY)
  m.SEASON    = ORDHDR.SEASON
  m.CDIVISION = ORDHDR.CDIVISION
  m.ACCOUNT   = ACCOUNT
  m.CUSTOMER  = LEFT(CUSTOMER.BTNAME,25)
  m.STORE     = STORE
  m.PO        =IIF(ORDHDR.MULTIPO,LEFT(CUSTPO,10),LEFT(ORDHDR.CUSTPO,10))
  m.START     = ORDHDR.START
  m.COMPLETE  =ORDHDR.COMPLETE
  m.SER       ='2'
  m.GROUP= GROUP
  m.STYLE=LEFT(STYLE,lnMajSeg  )
  
  *! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [Start]
  *M.COLOR = RIGHT(STYLE,LNnMAJ+1)
  M.COLOR = Substr(STYLE,lnClrPOS,lnClrLen)
  *! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [End]
  
  m.DESC1= LEFT(STYLE.DESC1,28)
  m.PRICE=PRICE
  m.QTY1= QTY1
  m.QTY2= QTY2
  m.QTY3= QTY3
  m.QTY4= QTY4
  m.QTY5= QTY5
  m.QTY6= QTY6
  m.QTY7= QTY7
  m.QTY8= QTY8
  m.TOTQTY= TOTQTY
  m.PIK1= PIK1
  m.PIK2= PIK2
  m.PIK3= PIK3
  m.PIK4= PIK4
  m.PIK5= PIK5
  m.PIK6= PIK6
  m.PIK7= PIK7
  m.PIK8= PIK8
  m.TOTPIK=TOTPIK
  m.LINENO=LINENO
  m.ST = 'L'
  m.SORDER=TOTQTY *PRICE
  m.SPICK =TOTPIK*PRICE
  SELECT (LCWORKFILE)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN


*!*************************************************************
*! Name      : lfvPikt
*! Developer : BASSEM RAFAAT
*! Date      : 03/30/1999
*! Purpose   : Validate the piktkt
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPikt()
*!*************************************************************

FUNCTION lfvPikt
PRIVATE lcPikFld,lcPikTkt,lnSelFile,lcPikTag

lcPikFld  = VARREAD()
lcPikTkt  = EVAL(lcPikFld)
lnSelFile =  SELECT(0)

SELECT PIKTKT
lcPikTag  = ORDER('PIKTKT')
SET ORDER TO TAG PIKTKT IN PIKTKT

IF !EMPTY(lcPikTkt) .AND. (!SEEK(lcPikTkt , 'PIKTKT'))
  DIMENSION laTemp[1]
  laTemp = ''
  * N000862 ,1 Thabet Handle globalization issues [Start]
  * lcBrFields = "Piktkt:H='Piktkt',Account:H='Account', Store:H='Store',Order:H='Order' "
  lcBrFields = "Piktkt:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PIKTKT,oAriaApplication.GetHeaderText("LANG_PIKTKT",AHEADERFILE))+"',Account:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNT,oAriaApplication.GetHeaderText("LANG_ACCOUNT",AHEADERFILE))+"', Store:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STORE,oAriaApplication.GetHeaderText("LANG_STORE",AHEADERFILE))+"',Order:H='"+LANG_ORDER+"' "
  * N000862 ,1 Thabet Handle globalization issues [END]
  = gfBrows('','Piktkt','laTemp')
  IF !EMPTY(laTemp[1])
    lcPikTkt = laTemp[1]
  ELSE
    lcPikTkt = ''
  ENDIF
ENDIF

&lcPikFld = lcPikTkt
SET ORDER TO lcPikTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 03/30/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen




*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
*DIMENSION loOgScroll.laCRTables[2]
DIMENSION loOgScroll.laCRTables[1]
*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]

*! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [Start]
*DIMENSION loOgScroll.laCRParams[7,2]
DIMENSION loOgScroll.laCRParams[9,2]
*! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [End]

loOgScroll.lcOGLastForm ='ALSTLS1'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"

*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
*loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcLogotmp+ ".DBF"
*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]


loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'Salesrep Commission Worksheet'

loOgScroll.laCRParams[2,1] = 'SortBy'
loOgScroll.laCRParams[2,2] = ' '

loOgScroll.laCRParams[3,1] = 'Period'
loOgScroll.laCRParams[3,2] = lcPERIOD

loOgScroll.laCRParams[4,1] = 'LCAVAL'
loOgScroll.laCRParams[4,2] = lcRpAloAvi

loOgScroll.laCRParams[5,1] = 'lcStyTitle'
loOgScroll.laCRParams[5,2] = lcStyTitle

loOgScroll.laCRParams[6,1] = 'lnMajSeg'
loOgScroll.laCRParams[6,2] = lnMajSeg

loOgScroll.laCRParams[7,1] = 'lnNmaj'
loOgScroll.laCRParams[7,2] = lnNmaj


*! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [Start]
loOgScroll.laCRParams[8,1] = 'lnClrPos'
loOgScroll.laCRParams[8,2] = lnClrPos

loOgScroll.laCRParams[9,1] = 'lnClrLen'
loOgScroll.laCRParams[9,2] = lnClrLen
*! B610309,1 HIA 04/17/2013 T20130329.0171 - Allocation Report (F#1) - when exporting to Excel, Color column becomes a formula because of an = sign [End]

*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   :
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[58,18] ,laTempCOM[1,18],laTempLINE[1,18],laTempHDR[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM,laTempLINE,laTempHDR
lcExcStat = SET('EXACT')
SET EXACT ON
SELECT STYLE
=OGAFIELDS(@laTempCOM)
laTempStru[1,1]  = 'STYLE'
laTempStru[2,1]  = 'DESC1'
laTempStru[3,1]  = 'STK1'
laTempStru[4,1]  = 'STK2'
laTempStru[5,1]  = 'STK3'
laTempStru[6,1]  = 'STK4'
laTempStru[7,1]  = 'STK5'
laTempStru[8,1]  = 'STK6'
laTempStru[9,1]  = 'STK7'
laTempStru[10,1] = 'STK8'
laTempStru[11,1] = 'TOTSTK'
laTempStru[12,1]  = 'ALO1'
laTempStru[13,1]  = 'ALO2'
laTempStru[14,1]  = 'ALO3'
laTempStru[15,1]  = 'ALO4'
laTempStru[16,1]  = 'ALO5'
laTempStru[17,1]  = 'ALO6'
laTempStru[18,1]  = 'ALO7'
laTempStru[19,1]  = 'ALO8'
laTempStru[20,1] = 'TOTALO'
*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 20
  lnFldRow = ASCAN(laTempCOM,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCOM,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCOM[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCOM[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCOM[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

SELECT ORDLINE
=OGAFIELDS(@laTempLINE)
laTempStru[21,1]  = 'ORDER'
laTempStru[22,1]  = 'PIKTKT'
laTempStru[23,1]  = 'QTY1'
laTempStru[24,1]  = 'QTY2'
laTempStru[25,1]  = 'QTY3'
laTempStru[26,1]  = 'QTY4'
laTempStru[27,1]  = 'QTY5'
laTempStru[28,1]  = 'QTY6'
laTempStru[29,1]  = 'QTY7'
laTempStru[30,1] = 'QTY8'
laTempStru[31,1] = 'TOTQTY'
laTempStru[32,1]  = 'PIK1'
laTempStru[33,1]  = 'PIK2'
laTempStru[34,1]  = 'PIK3'
laTempStru[35,1]  = 'PIK4'
laTempStru[36,1]  = 'PIK5'
laTempStru[37,1]  = 'PIK6'
laTempStru[38,1]  = 'PIK7'
laTempStru[39,1]  = 'PIK8'
laTempStru[40,1] = 'TOTPIK'
laTempStru[41,1] = 'ACCOUNT'
laTempStru[42,1] = 'STORE'
laTempStru[43,1] = 'PO'
laTempStru[44,1] = 'GROUP'
laTempStru[45,1] = 'PRICE'
laTempStru[46,1] = 'LINENO'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 21 TO 46
  lnFldRow = ASCAN(laTempLINE,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempLINE,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempLINE[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempLINE[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempLINE[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

SELECT ORDHDR
=OGAFIELDS(@laTempHDR)
laTempStru[47,1]  = 'START'
laTempStru[48,1]  = 'PRIORITY'
laTempStru[49,1]  = 'CDIVISION'
laTempStru[50,1]  = 'SEASON'
laTempStru[51,1]  = 'CWARECODE'
laTempStru[52,1]  = 'COMPLETE'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 46 TO 52
  lnFldRow = ASCAN(laTempHDR,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempHDR,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempHDR[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempHDR[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempHDR[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)


laTempStru[53,1] = 'CUSTOMER'
laTempStru[53,2] = 'C'
laTempStru[53,3] = 30
laTempStru[53,4] = 0

laTempStru[54,1] = 'SORDER'
laTempStru[54,2] = 'N'
laTempStru[54,3] = 10
laTempStru[54,4] = 2

laTempStru[55,1] = 'SPICK'
laTempStru[55,2] = 'N'
laTempStru[55,3] = 10
laTempStru[55,4] = 2

laTempStru[56,1] = 'SER'
laTempStru[56,2] = 'C'
laTempStru[56,3] = 1
laTempStru[56,4] = 0

laTempStru[57,1] = 'ST'
laTempStru[57,2] = 'C'
laTempStru[57,3] = 1
laTempStru[57,4] = 0

laTempStru[58,1] = 'COLOR'
laTempStru[58,2] = 'C'
laTempStru[58,3] = LNnMAJ+2
laTempStru[58,4] = 0
=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.F.)
SET EXACT &lcExcStat


*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[Start]
*!*  lcPath=oAriaApplication.WorkDir +  lcLogotmp+ ".DBF"

*!*  CREATE TABLE (lcPath) (ST C(1),mygenfield G)
*!*  SELECT (lcLogotmp)
*!*  APPEND BLANK
*!*  REPLACE ST  WITH 'L'

*!*  IF TYPE("lcMyLogo") = 'C' .AND. !EMPTY(lcMyLogo) .AND. !ISNULL(lcMyLogo) .AND.  (UPPER(RIGHT(lcMyLogo,3)) == 'BMP')
*!*    IF loogscroll.FileExist(lcMyLogo) .AND. loogscroll.llShowLogo
*!*        APPEND GENERAL mygenfield FROM (lcMyLogo)
*!*    ENDIF
*!*  ENDIF
*: B608649,1 MMT 08/11/2008 Fix bug of Error while preview due to logo field in RPT[End]

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



*************************************************************
*! Name      : CREATEFIL
*! Developer : AYMAN MAHMOUD (AYM)
*! Date      : 07/19/2006
*! Purpose   : Create temp file
*!*************************************************************
FUNCTION CREATEFIL
* N000862 ,1 Thabet Handle globalization issues [Start]
*WAIT  Lang_Selecting_records_for_report WINDOW NOWAIT
WAIT  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selecting_records_for_report,oAriaApplication.GetHeaderText("Lang_Selecting_records_for_report",AHEADERFILE)) WINDOW NOWAIT
* N000862 ,1 Thabet Handle globalization issues [Start]

lcFilter=' .t. '
IF llPikFltr
  lcFilter=lcFilter+" AND SEEK(PIKTKT,lcPikFltr) "
ENDIF

IF  llMultiWH  AND llWarFltr
  lcFilter=lcFilter+" AND SEEK(CWARECODE,lcWarFltr) "
ENDIF

IF llPriorty
  lcFilter=lcFilter+" AND ORDHDR.PRIORITY='"+lcPriorty+"'"
ENDIF
IF llDatFil
  lcFilter=lcFilter+" AND "+lcDatFil
ENDIF
lcFilter=lcFilter+' .AND. TOTPIK<>0 '

SELE ORDLINE
LOCATE
SET RELATION TO cordtype + ORDER INTO Ordhdr ADDI
COPY REST TO &lcOrdlTemp FOR  &lcFilter
=gfOpenFile('&lcOrdlTemp',' ','EX')








