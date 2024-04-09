*:***************************************************************************
*: Program file  : ICCTSLK3.PRG
*: Program desc. : Style Cut & Sold [KAZ300]
*: TRACK NO      : C200798
*: System        : Aria4XP
*: Module        : Inventory Control (IC)
*: Developer     : Walid Hamed (WLD)
*! Date          : 06/17/2007
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCTSLK3
*:***************************************************************************
*: Modification:
*:B608157,1 WLD 07/10/2007 The customer has styles with single quote so we handled it by adding another single  quote
*:***************************************************************************

*--Initialize the variables.

IF llOgFltCh

  STORE 0 TO GRAOTS,GRAORD,GRASTK,GRAWIP
  STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
  STORE '' TO lcRpStatus
  =lfChkStrct()
  llExtSize = gfGetMemVar('M_USEEXSSC')
  llDontPrn = .F.

  lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
  XTITLE   = lcRPTitle
  =lfcreatfilter()
  XFILTER = lcRPExp
  WAIT WINDOW 'Collecting data....' NOWAIT
  FStyle= loOGScroll.gfTempName()
  WORKTEMP = loOGScroll.gfTempName()
  lcWorkfile  = loOGScroll.gfTempName()
  TMPSCAL     = loOGScroll.gfTempName()
  LCTMPFILE   = loOGScroll.gfTempName()
  lcCtkBaz    = loOGScroll.gfTempName()

  =lfBuildTmp()
  IF lfCreatFiles()
    IF lpCollecData()
    ELSE
      RETURN .F.
    ENDIF
  ELSE
    RETURN .F.
  ENDIF
  WAIT CLEAR

  IF USED(FStyle)
    USE IN (FStyle)
  ENDIF
  IF !llDontPrn
    IF USED(LCTMPFILE)
      USE IN (LCTMPFILE)
    ENDIF
    =gfDispRe()
  ENDIF
ELSE  &&FILTERCHANGE
  IF  llDontPrn
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')

  ELSE
    =gfDispRe()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Walid Hamed (WLD)
*! Date      : 06/17/2007
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
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
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
  lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
  *-- Compute Free/Color Items in Style Structure. [End]

  RETURN ''

  *!*************************************************************
  *! Name      : lfwRepWhen
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Option Grid When function
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : gfUserPriv
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfwRepWhen()
  *!*************************************************************

FUNCTION lfwRepWhen
  *-- Check the cost access

  llCostAccs = gfUserPriv('IC','ICSTYLE')

  lnClrSgPo = ASUBSCRIPT(loOGScroll.laOGFxFlt,;
    ASCAN(loOGScroll.laOGFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

IF !llFirst 
	lcSqlStatment   = "SELECT ItemLoc.STYLE,ItemLoc.TOTWIP,ItemLoc.TOTSTK,ItemLoc.TOTORD,WIP1,DYELOT,ITEM.CSTYMAJOR   FROM ItemLoc(INDEX = STYDYE),ITEM(INDEX = CSTYLE) WHERE ITEMLOC.dyelot ='' AND ITEMLOC.STYLE = ITEM.STYLE AND ITEMLOC.CINVTYPE = ITEM.CINVTYPE "
	lcSqlStatment   = lcSqlStatment   +" AND ITEM.CINVTYPE = 0002"
	lcCursorLoc = loOgScroll.gfTempName()
	lcTable ='ItemLoc'
	lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursorLoc,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
	                                      'BROWSE',SET("DATASESSION"))

	IF lnConnectionHandlar >= 1
	  SELECT(lcCursorLoc)
	  lnBuffering = CURSORGETPROP("Buffering",lcCursorLoc)
	   =CURSORSETPROP("Buffering",3,lcCursorLoc)
	  SELECT (lcCursorLoc)
	  INDEX ON CSTYMAJOR TAG &lcCursorLoc
	  SET ORDER TO TAG &lcCursorLoc
	  SET RELATION TO 
	  LOCATE 
	ENDIF 
	llFirst = .T.
ENDIF 

  *-- end of lfwRepWhen.

  *!*************************************************************
  *! Name      : lfMajPic
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Get major seg. picture
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

  *!*************************************************************
  *! Name      : lfTermnate
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Function to terminate the report if neither
  *!             the "Sales Order" nor the "Point of Sale" modules
  *!             is installed.
  *!*************************************************************
  *! Called from : Default value of the report variable (llTermnate).
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfTermnate

  *-- If neither the "Sales Order" nor the "Point of Sale" module is installed
  IF !('SO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules)
    *** Message : "Since neither the Sales Order nor the Point of Sale module"
    ***           "is installed, you cannot run this report.                 "
    ***
    *** Buttons : "                         < Ok >                           "
    =gfModalgen("TRM42171B00000","ALERT")

    *-- Terminate the Option Grid.
    llOgTrmnat = .T.
  ENDIF    && End of IF !('SO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules)
  *!**************************************************************************
  *! Name      : lfSetSTY
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Go top in the style IN RANGE
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

  *!*************************************************************
  *! Name      : lfChkStrct
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Get the Style and Color Length.
  *!*************************************************************
  *! Calls     :
  *!         Procedures : ....
  *!         Functions  : ....
  *!*************************************************************
  *! Called from        : ALPKLSNK.PRG
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns     : None
  *!*************************************************************
  *! Example     : =lfChkStrct()
  *!*************************************************************
FUNCTION lfChkStrct
  *--THE COLOR LENGTH
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lnClrLnGl  = LEN(laItemSeg[lnCount,3])
      lnClrPosGL = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR

  *--THE STYLE LENGTH
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='F'
      lnStyLnGl  = LEN(laItemSeg[lnCount,3])
      lnStyPosGl = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR

  *--THE SCALE LENGTH
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='S'
      lnScaLnGl  = LEN(laItemSeg[lnCount,3])
      lnScaPosGl = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR

  *--End of lfChkStrct.

  *************************************************************
  *! Name      : lfBuildTmp
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Create Temp table
  *!*************************************************************
FUNCTION lfBuildTmp

  lcExcStat = SET('EXACT')
  SET EXACT ON
  DIMENSION laTempStru[88,18]

  STORE '' TO laTempStru

  laTempStru[1 ,1] = 'STYLE'
  laTempStru[1 ,2] = 'C'
  laTempStru[1 ,3] = 19
  laTempStru[1 ,4] = 0
  laTempStru[2 ,1] = 'DESC'
  laTempStru[2 ,2] = 'C'
  laTempStru[2 ,3] = 50
  laTempStru[2 ,4] = 0
  laTempStru[3 ,1] = 'FABRIC'
  laTempStru[3 ,2] = 'C'
  laTempStru[3 ,3] = 7
  laTempStru[3 ,4] = 0
  laTempStru[4 ,1] = 'PRICE'
  laTempStru[4 ,2] = 'N'
  laTempStru[4 ,3] = 9
  laTempStru[4 ,4] = 2
  laTempStru[5 ,1] = 'SCALE'
  laTempStru[5 ,2] = 'C'
  laTempStru[5 ,3] = 3
  laTempStru[5 ,4] = 0

  laTempStru[6 ,1] = 'CUT1'
  laTempStru[7 ,1] = 'CUT2'
  laTempStru[8 ,1] = 'CUT3'
  laTempStru[9 ,1] = 'CUT4'
  laTempStru[10 ,1] = 'CUT5'
  laTempStru[11 ,1] = 'CUT6'
  laTempStru[12 ,1] = 'CUT7'
  laTempStru[13 ,1] = 'CUT8'
  laTempStru[14 ,1] = 'TOTCUT'
  laTempStru[15 ,1] = 'UNT1'
  laTempStru[16 ,1] = 'UNT2'
  laTempStru[17 ,1] = 'UNT3'
  laTempStru[18 ,1] = 'UNT4'
  laTempStru[19 ,1] = 'UNT5'
  laTempStru[20 ,1] = 'UNT6'
  laTempStru[21 ,1] = 'UNT7'
  laTempStru[22 ,1] = 'UNT8'
  laTempStru[23 ,1] = 'TOTUNT'
  laTempStru[24 ,1] = 'WIP1'
  laTempStru[25 ,1] = 'WIP2'
  laTempStru[26 ,1] = 'WIP3'
  laTempStru[27 ,1] = 'WIP4'
  laTempStru[28 ,1] = 'WIP5'
  laTempStru[29 ,1] = 'WIP6'
  laTempStru[30 ,1] = 'WIP7'
  laTempStru[31 ,1] = 'WIP8'
  laTempStru[32 ,1] = 'TOTWIP'
  laTempStru[33 ,1] = 'RCVWH1'
  laTempStru[34 ,1] = 'RCVWH2'
  laTempStru[35 ,1] = 'RCVWH3'
  laTempStru[36 ,1] = 'RCVWH4'
  laTempStru[37 ,1] = 'RCVWH5'
  laTempStru[38 ,1] = 'RCVWH6'
  laTempStru[39 ,1] = 'RCVWH7'
  laTempStru[40 ,1] = 'RCVWH8'
  laTempStru[41 ,1] = 'TOTRCVWH'
  laTempStru[42 ,1] = 'BOK1'
  laTempStru[43 ,1] = 'BOK2'
  laTempStru[44 ,1] = 'BOK3'
  laTempStru[45 ,1] = 'BOK4'
  laTempStru[46 ,1] = 'BOK5'
  laTempStru[47 ,1] = 'BOK6'
  laTempStru[48 ,1] = 'BOK7'
  laTempStru[49 ,1] = 'BOK8'
  laTempStru[50 ,1] = 'TOTBOK'
  laTempStru[51,1] = 'OHOTS1'
  laTempStru[52,1] = 'OHOTS2'
  laTempStru[53,1] = 'OHOTS3'
  laTempStru[54,1] = 'OHOTS4'
  laTempStru[55,1] = 'OHOTS5'
  laTempStru[56,1] = 'OHOTS6'
  laTempStru[57,1] = 'OHOTS7'
  laTempStru[58,1] = 'OHOTS8'
  laTempStru[59,1] = 'TOTOHOTS'
  laTempStru[60,1] = 'OOTS1'
  laTempStru[61,1] = 'OOTS2'
  laTempStru[62,1] = 'OOTS3'
  laTempStru[63,1] = 'OOTS4'
  laTempStru[64,1] = 'OOTS5'
  laTempStru[65,1] = 'OOTS6'
  laTempStru[66,1] = 'OOTS7'
  laTempStru[67,1] = 'OOTS8'
  laTempStru[68,1] = 'TOTOOTS'

  FOR I = 6 TO 68
    laTempStru[I ,2] = 'N'
    laTempStru[I ,3] = 8
    laTempStru[I ,4] = 0
  ENDFOR
  laTempStru[69 ,1] = 'CutEnt'
  laTempStru[69 ,2] = 'D'
  laTempStru[69 ,3] = 8
  laTempStru[69 ,4] = 0
  laTempStru[70 ,1] = 'UntEnt'
  laTempStru[70 ,2] = 'D'
  laTempStru[70 ,3] = 8
  laTempStru[70 ,4] = 0

  laTempStru[71 ,1] = 'BCUT1'
  laTempStru[72 ,1] = 'BCUT2'
  laTempStru[73 ,1] = 'BCUT3'
  laTempStru[74 ,1] = 'BCUT4'
  laTempStru[75 ,1] = 'BCUT5'
  laTempStru[76 ,1] = 'BCUT6'
  laTempStru[77 ,1] = 'BCUT7'
  laTempStru[78 ,1] = 'BCUT8'
  laTempStru[79 ,1] = 'BTOTCUT'
  laTempStru[80 ,1] = 'BUNT1'
  laTempStru[81 ,1] = 'BUNT2'
  laTempStru[82 ,1] = 'BUNT3'
  laTempStru[83 ,1] = 'BUNT4'
  laTempStru[84 ,1] = 'BUNT5'
  laTempStru[85 ,1] = 'BUNT6'
  laTempStru[86 ,1] = 'BUNT7'
  laTempStru[87 ,1] = 'BUNT8'
  laTempStru[88 ,1] = 'BTOTUNT'

  FOR I = 71 TO 88
    laTempStru[I ,2] = 'N'
    laTempStru[I ,3] = 8
    laTempStru[I ,4] = 0
  ENDFOR


  gfCrtTmp(lcWorkfile ,@laTempstru,,"",.F.)

  SELECT(lcWorkfile)
  INDEX ON STYLE TAG (lcWorkfile)

  SET EXACT &lcExcStat

  *************************************************************
  *! Name      : lfAdjustCRSettings
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : To set the report data files and parameters
  *!*************************************************************
FUNCTION lfAdjustCRSettings

  DIMENSION loOgScroll.laCRTables[2]
  DIMENSION loOgScroll.laCRParams[2,2]

  loOGScroll.cCROrientation='L'
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  TMPSCAL+ ".DBF"
  loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"

  loOgScroll.lcOGLastForm ='ICCTSLK3'

  loOgScroll.laCRParams[1,1] = 'OpTitle'
  loOgScroll.laCRParams[1,2] = lcRpTitle

  loOgScroll.laCRParams[2,1] = 'ReportName'
  loOgScroll.laCRParams[2,2]= 'SPECIAL CUT & SOLD - SUMMARY REPORT'


  *!*************************************************************
  *! Name      : lfCreatFiles
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Creating files and relation
  *!*************************************************************
FUNCTION lfCreatFiles

  lcSylefields= "STYLE,SCALE,MAKE,DESC,FABRIC,PRICEA, "
  lcSylefields= lcSylefields+ " SHP1,SHP2,SHP3,SHP4,SHP5,SHP6,SHP7,SHP8, "
  lcSylefields= lcSylefields+ " ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8, "
  lcSylefields= lcSylefields+ " WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8, "
  lcSylefields= lcSylefields+ " TOTSHP, TOTORD, TOTWIP"

  SELECT &lcSylefields. FROM STYLE WHERE STYLE.STYLE <>'********' AND  &XFILTER  INTO CURSOR &FStyle. READWRITE

  SELECT (FStyle)
  GO TOP
  IF EOF()
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    llDontPrn = .T.
    RETURN
  ENDIF
  INDEX ON STYLE TAG STYLE
  SET ORDER TO STYLE

  SELECT ORDHDR
  SELECT ORDLINE
  SET RELATION TO cOrdType + ORDER INTO ORDHDR ADDITIVE

  RETURN .T.
  *!*************************************************************
  *! Name      : lfcreatfilter
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Creating filter
  *!*************************************************************
FUNCTION lfcreatfilter

  *Creting filter exp   .. begin
  lcseek=" .T. "
  *Major filter
  lcCurFltr= lfCheckFilter(1, 'STYLE.CSTYMAJOR')
  llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
  IF llCurFltr
    SELECT (lcCurFltr)
    INDEX ON CSTYMAJOR TAG (lcCurFltr)
    lcseek=lcseek+" AND SEEK(STYLE.CSTYMAJOR ,'"+lcCurFltr+"')"
  ELSE
    IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
      USE IN (lcCurFltr)
    ENDIF
    lcCurFltr= ''
  ENDIF

  *Fabric Filter
  lcCurFltr= lfCheckFilter(1, 'STYLE.FABRIC')
  llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
  IF llCurFltr
    SELECT (lcCurFltr)
    INDEX ON CSTYMAJOR TAG (lcCurFltr)
    lcseek=lcseek+" AND SEEK(PADR(STYLE.FABRIC,19),'"+lcCurFltr+"')"
  ELSE
    IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
      USE IN (lcCurFltr)
    ENDIF
    lcCurFltr= ''
  ENDIF


  * Check if there is a filter on Style CDIVISION
  lcCurName = lfCheckFilter(1, 'STYLE.CDIVISION')
  lcDiv   = loOgScroll.gfTempName()
  llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
  IF llDiv
    SELECT (lcDiv)
    INDEX ON CDivision TAG (lcDiv)
    lcseek=lcseek+" AND SEEK(STYLE.CDIVISION,'"+lcDiv+"')"
  ENDIF

  * Check if there is a filter on Style SEASON
  lcCurName = lfCheckFilter(1, 'STYLE.SEASON')
  lcSea  = loOgScroll.gfTempName()
  llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON")
  IF llSea
    SELECT (lcSea  )
    INDEX ON SEASON TAG (lcSea  )
    lcseek=lcseek+" AND SEEK(STYLE.SEASON,'"+lcSea+"')"
  ENDIF

  * Check if there is a filter on Style Group
  lcCurName = lfCheckFilter(1, 'STYLE.CSTYGROUP')
  lcGrp  = loOgScroll.gfTempName()
  llGrp   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcGrp  ,"Group")
  IF llGrp
    SELECT (lcGrp  )
    INDEX ON GROUP TAG (lcGrp  )
    lcseek=lcseek+" AND SEEK(STYLE.CSTYGROUP,'"+lcGrp+"')"
  ENDIF


  * Check if there is a filter on Style COLOR
  lcCurName = lfCheckFilter(1, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')

  lcCol  = loOgScroll.gfTempName()
  llCol   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol  ,"Color")
  IF llCol
    SELECT (lcCol  )
    INDEX ON COLOR TAG (lcCol  )
    lcseek=lcseek+" AND SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),'"+lcCol  +"')"
  ENDIF

  * PATTERN Filter
  lcPattern= lfCheckFilter(1, 'STYLE.PATTERN')
  llPattern=IIF(EMPTY(lcPattern),.F.,.T.)
  IF llPattern
    lcSeek=lcSeek +" AND STYLE.PATTERN='"+lcPattern+"'"
  ENDIF
  
  *Status Filter
  lcRpStatus = lfCheckFilter(1, 'STYLE.STATUS')
  lcSeek= lcSeek+ IIF(!EMPTY(lcRpStatus),' .AND. Style.Status $ lcRpStatus','')
  *Dom\Imp Filter
  lcSeek= lcSeek + IIF(lcRPDomImp = 'D',' .AND. STYLE.Make ' , IIF(lcRPDomImp = 'I',' .AND.  !STYLE.Make ' ,' .AND.  .T. '))

  lcRPExp = lcSeek

  *!*************************************************************
  *! Name      : lpCollecData
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Collecting data in Temp. File
  *!*************************************************************

PROCEDURE lpCollecData
  PRIVATE llHastran
  *-- Headers variabls.
  lcCutHdr = 'CUT QUANTITY BASE ............'
  lcUntHdr = 'UNIT BASE BOOKINGS ............'
  lcCut1Hd = 'CUT QUANTITY BASE AT '
  lcUnt1Hd = 'UNIT BASE BOOKINGS UP TO '
  lcWipHdr = 'W.I.P ............'
  lcRecHdr = 'UNITS RECEVIED IN W.H. ............'
  lcBokHdr = 'BOOKINGS ............'
  lcHldHdr = 'O.T.S. (OPEN+HOLD ORDERS) ............'
  lcOpnHdr = 'O.T.S. (OPEN ORDERS ONLY) ............'

  SELECT (FSTYLE)
  XSTYLE=SPACE(19)

  SCAN
    IF INKEY()=32
      RETURN
    ENDIF
    xStyle = STYLE
    WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
    SCATTER MEMVAR MEMO
    m.desc= ALLT(DESC)
    m.Price= PriceA
    lnCost = PriceA
    SELECT &LCWORKFILE
    APPEND BLANK
    GATHER MEMVAR MEMO

    * 'UNITS RECEVIED IN W.H. ............'
    =lfRcvWhs()
    * 'BOOKINGS ............'
    =lfQtyBok()
    * 'O.T.S. (OPEN+HOLD ORDERS) ............'
    =lfOHots()
    * 'O.T.S. (OPEN ORDERS ONLY) ............'
    =lfOots()
    * 'CUT QUANTITY BASE ...... and ... UNIT BASE BOOKINGS..........
    =lfQtyBase()
  ENDSCAN

  SELECT (LCWORKFILE)
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"
  USE oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" IN 0 SHARED
  SELECT &LCTMPFILE
  INDEX ON STYLE  TAG LCTMPFILE
  USE

  SELECT * FROM SCALE WHERE TYPE='S' INTO CURSOR LCSCALE READWRITE
  SELECT LCSCALE
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  TMPSCAL+ ".DBF" )
    ERASE (oAriaApplication.WorkDir +  TMPSCAL+ ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  TMPSCAL+ ".DBF"

  =lfAdjustCRSettings()

  *!*************************************************************
  *! Name      : lfvStyle
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Validate style.
  *!*************************************************************
FUNCTION lfvStyle

  lcStyle = VARREAD()
  lcTag = ORDER('STYLE')
  SET ORDER TO cStyle IN STYLE
  IF LASTKEY() = 13 AND !MDOWN()
    IF gfSEEK(&lcStyle.,'Style')
      &lcStyle = STYLE.cStyMajor
    ELSE
      &lcStyle = gfStyBrw('M',"","",.F.)
    ENDIF
  ELSE
    &lcStyle = ''
  ENDIF

  SET ORDER TO lcTag IN STYLE

  *--End of lfvStyle.
  *************************************************************
  *! Name      : lfCheckFilter
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
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
  *! Name      : lfStr2Curs
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Create cursor from string filters
  *!*************************************************************
FUNCTION lfStr2Curs
  PARAMETERS lcString , lccursor , lcFieldsName

  CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
  DO WHILE AT('|',lcString)> 0
    lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
    lcString = SUBSTR(lcString,AT('|',lcString)+1)
    SELECT (lcCursor)
    APPEND BLANK
    REPLACE &lcFieldsName. WITH lcFieldsValue
  ENDDO
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcString

  *!*************************************************************
  *! Name      : lfQtyBase
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Collect the quantity data of 'CUT QUANTITY BASE' and 'UNIT BASE BOOKINGS'
  *!*************************************************************
FUNCTION lfQtyBase

  STORE 0 TO lnCtQty1,lnCtQty2,lnCtQty3,lnCtQty4,lnCtQty5,lnCtQty6,lnCtQty7,lnCtQty8,lnCtTot
  STORE 0 TO lnOBook1,lnOBook2,lnOBook3,lnOBook4,lnOBook5,lnOBook6,lnOBook7,lnOBook8,lnTotOBook
  STORE 0 TO lnIBook1,lnIBook2,lnIBook3,lnIBook4,lnIBook5,lnIBook6,lnIBook7,lnIBook8,lnTotIBook
  STORE 0 TO lnOIBk1,lnOIBk2,lnOIBk3,lnOIBk4,lnOIBk5,lnOIBk6,lnOIBk7,lnOIBk8,lnTotOIBk

  IF &FStyle..MAKE
    *B608157,1 WLD 07/10/2007 The customer has styles with single quote so we handled it by adding another single  quote [Begin]
    *lcSQLStmtCUT = " SELECT  posln.style,POSHDR.Entered, SUM(qty1) AS POS1, SUM(qty2) AS POS2, SUM(qty3) AS POS3, SUM(qty4) AS POS4, SUM(qty5) AS POS5, SUM(qty6) AS POS6, SUM(qty7) AS POS7, SUM(qty8) AS POS8,SUM(totqty) AS totPOS " +;
    *  " FROM    POSHDR,posln" +;
    *  " WHERE    (PoSln.Po = PoSHdr.Po) AND (posln.style = '"+ ALLTRIM(XSTYLE) +"') AND (posln.cbusdocu = 'P') AND (posln.cstytype IN ('U')) AND (POSHDR.Status <> 'X') AND (posln.cinvtype = '0001') AND (posln.TRANCD='1')" +;
    *  " GROUP BY posln.style,POSHDR.Entered Order By POSHDR.Entered"
    lcSQLStmtCUT = " SELECT  posln.style,POSHDR.Entered, SUM(qty1) AS POS1, SUM(qty2) AS POS2, SUM(qty3) AS POS3, SUM(qty4) AS POS4, SUM(qty5) AS POS5, SUM(qty6) AS POS6, SUM(qty7) AS POS7, SUM(qty8) AS POS8,SUM(totqty) AS totPOS " +;
      " FROM    POSHDR,posln" +;
      " WHERE    (PoSln.Po = PoSHdr.Po) AND (posln.style = '"+ ALLTRIM(STRTRAN(XSTYLE,"'","''")) +"') AND (posln.cbusdocu = 'P') AND (posln.cstytype IN ('U')) AND (POSHDR.Status <> 'X') AND (posln.cinvtype = '0001') AND (posln.TRANCD='1')" +;
      " GROUP BY posln.style,POSHDR.Entered Order By POSHDR.Entered"
    *B608157,1 WLD [End]
  ELSE
    *B608157,1 WLD 07/10/2007 The customer has styles with single quote so we handled it by adding another single  quote [Begin]
    *lcSQLStmtCUT = " SELECT  posln.style,POSHDR.Entered, SUM(qty1) AS POS1, SUM(qty2) AS POS2, SUM(qty3) AS POS3, SUM(qty4) AS POS4, SUM(qty5) AS POS5, SUM(qty6) AS POS6, SUM(qty7) AS POS7, SUM(qty8) AS POS8,SUM(totqty) AS totPOS " +;
    *  " FROM    POSHDR,posln" +;
    *  " WHERE    (PoSln.Po = PoSHdr.Po) AND (posln.style = '"+ ALLTRIM(XSTYLE) +"') AND (posln.cbusdocu = 'P') AND (posln.cstytype IN ('P')) AND (POSHDR.Status <> 'X') AND (posln.cinvtype = '0001') AND (posln.TRANCD='1')" +;
    *  " GROUP BY posln.style,POSHDR.Entered Order By POSHDR.Entered"
    lcSQLStmtCUT = " SELECT  posln.style,POSHDR.Entered, SUM(qty1) AS POS1, SUM(qty2) AS POS2, SUM(qty3) AS POS3, SUM(qty4) AS POS4, SUM(qty5) AS POS5, SUM(qty6) AS POS6, SUM(qty7) AS POS7, SUM(qty8) AS POS8,SUM(totqty) AS totPOS " +;
      " FROM    POSHDR,posln" +;
      " WHERE    (PoSln.Po = PoSHdr.Po) AND (posln.style = '"+ ALLTRIM(STRTRAN(XSTYLE,"'","''")) +"') AND (posln.cbusdocu = 'P') AND (posln.cstytype IN ('P')) AND (POSHDR.Status <> 'X') AND (posln.cinvtype = '0001') AND (posln.TRANCD='1')" +;
      " GROUP BY posln.style,POSHDR.Entered Order By POSHDR.Entered"
    *B608157,1 WLD  [End]
  ENDIF
  lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmtCUT, lcCtkBaz ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
  IF lnResult = 1
    SELECT (lcWorkfile)
    SCATTER MEMVAR MEMO
    SELECT(lcCtkBaz)
    ldEntered = TTOD(&lcCtkBaz..Entered)
    *Cut Base
    REPLACE BCUT1 WITH &lcCtkBaz..POS1,BCUT2 WITH &lcCtkBaz..POS2,BCUT3 WITH &lcCtkBaz..POS3,BCUT4 WITH &lcCtkBaz..POS4,;
      BCUT5 WITH &lcCtkBaz..POS5,BCUT6 WITH &lcCtkBaz..POS6,BCUT7 WITH &lcCtkBaz..POS7,BCUT8 WITH &lcCtkBaz..POS8,BTOTCUT WITH &lcCtkBaz..TOTPOS IN (lcWorkfile)
    REPLACE CUTENT WITH TTOD(&lcCtkBaz..Entered) IN (lcWorkfile)
    lnCtQty1 = &lcCtkBaz..POS1
    lnCtQty2 = &lcCtkBaz..POS2
    lnCtQty3 = &lcCtkBaz..POS3
    lnCtQty4 = &lcCtkBaz..POS4
    lnCtQty5 = &lcCtkBaz..POS5
    lnCtQty6 = &lcCtkBaz..POS6
    lnCtQty7 = &lcCtkBaz..POS7
    lnCtQty8 = &lcCtkBaz..POS8
    lnCtTot = &lcCtkBaz..TOTPOS

    SELECT ORDLINE
    =GFSEEK(XSTYLE,'ORDLINE')
    SCAN REST WHILE STYLE = XSTYLE FOR ORDHDR.STATUS $ 'OH' .AND. ORDHDR.Entered<=ldEntered
      lnOBook1 = lnOBook1 + ORDLINE.QTY1
      lnOBook2 = lnOBook2 + ORDLINE.QTY2
      lnOBook3 = lnOBook3 + ORDLINE.QTY3
      lnOBook4 = lnOBook4 + ORDLINE.QTY4
      lnOBook5 = lnOBook5 + ORDLINE.QTY5
      lnOBook6 = lnOBook6 + ORDLINE.QTY6
      lnOBook7 = lnOBook7 + ORDLINE.QTY7
      lnOBook8 = lnOBook8 + ORDLINE.QTY8
      lnTotOBook = lnTotOBook + ORDLINE.TotQTY
    ENDSCAN

    SELECT INVLINE
    =GFSEEK(XSTYLE,'INVLINE')
    SCAN REST WHILE STYLE = XSTYLE FOR INVHDR.STATUS <> 'V' .AND. InvDate<=ldEntered
      lnIBook1 = lnIBook1 + INVLINE.QTY1
      lnIBook2 = lnIBook2 + INVLINE.QTY2
      lnIBook3 = lnIBook3 + INVLINE.QTY3
      lnIBook4 = lnIBook4 + INVLINE.QTY4
      lnIBook5 = lnIBook5 + INVLINE.QTY5
      lnIBook6 = lnIBook6 + INVLINE.QTY6
      lnIBook7 = lnIBook7 + INVLINE.QTY7
      lnIBook8 = lnIBook8 + INVLINE.QTY8
      lnTotIBook = lnTotIBook + INVLINE.TotQTY
    ENDSCAN

    REPLACE BUNT1 WITH lnOBook1+lnIBook1,BUNT2 WITH lnOBook2+lnIBook2,BUNT3 WITH lnOBook3+lnIBook3,BUNT4 WITH lnOBook4+lnIBook4,;
      BUNT5 WITH lnOBook5+lnIBook5,BUNT6 WITH lnOBook6+lnIBook6,BUNT7 WITH lnOBook7+lnIBook7,BUNT8 WITH lnOBook8+lnIBook8,BTOTUNT WITH lnTotOBook+lnTotIBook IN (lcWorkfile)

    REPLACE CUTENT WITH IIF(DAY(CUTENT)<>0,CUTENT,ldEntered),;
      UNTENT WITH IIF(DAY(UNTENT)<>0,UNTENT,ldEntered) IN (lcWorkfile)

    lnOIBk1 = lnOBook1 + lnIBook1
    lnOIBk2 = lnOBook2 + lnIBook2
    lnOIBk3 = lnOBook3 + lnIBook3
    lnOIBk4 = lnOBook4 + lnIBook4
    lnOIBk5 = lnOBook5 + lnIBook5
    lnOIBk6 = lnOBook6 + lnIBook6
    lnOIBk7 = lnOBook7 + lnIBook7
    lnOIBk8 = lnOBook8 + lnIBook8
    lnTotOIBk = lnTotOBook+lnTotIBook

    SELECT(lcCtkBaz)
    IF RECNO() <= RECCOUNT()
      SKIP

      SCAN REST
        ldEntered = TTOD(&lcCtkBaz..Entered)
        INSERT INTO (lcWorkfile) FROM MEMVAR
        REPLACE BCUT1 WITH &lcCtkBaz..POS1,BCUT2 WITH &lcCtkBaz..POS2,BCUT3 WITH &lcCtkBaz..POS3,BCUT4 WITH &lcCtkBaz..POS4,;
          BCUT5 WITH &lcCtkBaz..POS5,BCUT6 WITH &lcCtkBaz..POS6,BCUT7 WITH &lcCtkBaz..POS7,BCUT8 WITH &lcCtkBaz..POS8,BTOTCUT WITH &lcCtkBaz..TOTPOS IN (lcWorkfile)
        REPLACE CUTENT WITH IIF(DAY(&lcCtkBaz..Entered)<>0,TTOD(&lcCtkBaz..Entered),ldEntered) IN (lcWorkfile)
        lnCtQty1 = lnCtQty1 + &lcCtkBaz..POS1
        lnCtQty2 = lnCtQty2 + &lcCtkBaz..POS2
        lnCtQty3 = lnCtQty3 + &lcCtkBaz..POS3
        lnCtQty4 = lnCtQty4 + &lcCtkBaz..POS4
        lnCtQty5 = lnCtQty5 + &lcCtkBaz..POS5
        lnCtQty6 = lnCtQty6 + &lcCtkBaz..POS6
        lnCtQty7 = lnCtQty7 + &lcCtkBaz..POS7
        lnCtQty8 = lnCtQty8 + &lcCtkBaz..POS8
        lnCtTot = lnCtTot + &lcCtkBaz..TOTPOS

        STORE 0 TO lnOBook1,lnOBook2,lnOBook3,lnOBook4,lnOBook5,lnOBook6,lnOBook7,lnOBook8,lnTotOBook
        STORE 0 TO lnIBook1,lnIBook2,lnIBook3,lnIBook4,lnIBook5,lnIBook6,lnIBook7,lnIBook8,lnTotIBook

        SELECT ORDLINE
        =GFSEEK(XSTYLE,'ORDLINE')
        SCAN REST WHILE STYLE = XSTYLE FOR ORDHDR.STATUS $ 'OH' .AND. ORDHDR.Entered<=ldEntered
          lnOBook1 = lnOBook1 + ORDLINE.QTY1
          lnOBook2 = lnOBook2 + ORDLINE.QTY2
          lnOBook3 = lnOBook3 + ORDLINE.QTY3
          lnOBook4 = lnOBook4 + ORDLINE.QTY4
          lnOBook5 = lnOBook5 + ORDLINE.QTY5
          lnOBook6 = lnOBook6 + ORDLINE.QTY6
          lnOBook7 = lnOBook7 + ORDLINE.QTY7
          lnOBook8 = lnOBook8 + ORDLINE.QTY8
          lnTotOBook = lnTotOBook + ORDLINE.TotQTY
        ENDSCAN

        SELECT INVLINE
        =GFSEEK(XSTYLE,'INVLINE')
        SCAN REST WHILE STYLE = XSTYLE FOR INVHDR.STATUS <> 'V' .AND. InvDate<=ldEntered
          lnIBook1 = lnIBook1 + INVLINE.QTY1
          lnIBook2 = lnIBook2 + INVLINE.QTY2
          lnIBook3 = lnIBook3 + INVLINE.QTY3
          lnIBook4 = lnIBook4 + INVLINE.QTY4
          lnIBook5 = lnIBook5 + INVLINE.QTY5
          lnIBook6 = lnIBook6 + INVLINE.QTY6
          lnIBook7 = lnIBook7 + INVLINE.QTY7
          lnIBook8 = lnIBook8 + INVLINE.QTY8
          lnTotIBook = lnTotIBook + INVLINE.TotQTY
        ENDSCAN

        REPLACE BUNT1 WITH lnOBook1+lnIBook1,BUNT2 WITH lnOBook2+lnIBook2,BUNT3 WITH lnOBook3+lnIBook3,BUNT4 WITH lnOBook4+lnIBook4,;
          BUNT5 WITH lnOBook5+lnIBook5,BUNT6 WITH lnOBook6+lnIBook6,BUNT7 WITH lnOBook7+lnIBook7,BUNT8 WITH lnOBook8+lnIBook8,BTOTUNT WITH lnTotOBook+lnTotIBook IN (lcWorkfile)

        lnOIBk1 = lnOIBk1 + lnOBook1 + lnIBook1
        lnOIBk2 = lnOIBk2 + lnOBook2 + lnIBook2
        lnOIBk3 = lnOIBk3 + lnOBook3 + lnIBook3
        lnOIBk4 = lnOIBk4 + lnOBook4 + lnIBook4
        lnOIBk5 = lnOIBk5 + lnOBook5 + lnIBook5
        lnOIBk6 = lnOIBk6 + lnOBook6 + lnIBook6
        lnOIBk7 = lnOIBk7 + lnOBook7 + lnIBook7
        lnOIBk8 = lnOIBk8 + lnOBook8 + lnIBook8
        lnTotOIBk = lnTotOBook+lnTotIBook

        REPLACE CUTENT WITH IIF(DAY(CUTENT)<>0,CUTENT,ldEntered),;
          UNTENT WITH IIF(DAY(UNTENT)<>0,UNTENT,ldEntered) IN (lcWorkfile)
      ENDSCAN
    ENDIF

    REPLACE CUT1 WITH lnCtQty1,CUT2 WITH lnCtQty2,CUT3 WITH lnCtQty3,CUT4 WITH lnCtQty4,;
      CUT5 WITH lnCtQty5,CUT6 WITH lnCtQty6,CUT7 WITH lnCtQty7,CUT8 WITH lnCtQty8,TOTCUT WITH lnCtTot IN (lcWorkfile)
    REPLACE UNT1 WITH lnOIBk1,UNT2 WITH lnOIBk2,UNT3 WITH lnOIBk3,UNT4 WITH lnOIBk4,;
      UNT5 WITH lnOIBk5,UNT6 WITH lnOIBk6,UNT7 WITH lnOIBk7,UNT8 WITH lnOIBk8,TOTUNT WITH lnTotOIBk IN (lcWorkfile)

    REPLACE CUTENT WITH IIF(DAY(CUTENT)<>0,CUTENT,ldEntered),;
      UNTENT WITH IIF(DAY(UNTENT)<>0,UNTENT,ldEntered) IN (lcWorkfile)
  ELSE
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF

  *--End of lfQtyBase.
  *!*************************************************************
  *! Name      : lfRcvWhs
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Collect the quantity data of UNITS RECEVIED IN W.H.
  *!*************************************************************
FUNCTION lfRcvWhs

  *B608157,1 WLD 07/10/2007 The customer has styles with single quote so we handled it by adding another single  quote [Begin] 
  *lcSQLStmtPOS = " SELECT  style, SUM(qty1) AS POS1, SUM(qty2) AS POS2, SUM(qty3) AS POS3, SUM(qty4) AS POS4, SUM(qty5) AS POS5, SUM(qty6) AS POS6, SUM(qty7) AS POS7, SUM(qty8) AS POS8,SUM(totqty) AS totPOS " +;
  *  " FROM    posln" +;
  *  " WHERE   (style = '"+ ALLTRIM(XSTYLE) +"') AND (cbusdocu = 'P') AND (cstytype IN ('U', 'P')) AND (cinvtype = '0001') AND (TRANCD='2')" +;
  *  " GROUP BY style"
  lcSQLStmtPOS = " SELECT  style, SUM(qty1) AS POS1, SUM(qty2) AS POS2, SUM(qty3) AS POS3, SUM(qty4) AS POS4, SUM(qty5) AS POS5, SUM(qty6) AS POS6, SUM(qty7) AS POS7, SUM(qty8) AS POS8,SUM(totqty) AS totPOS " +;
    " FROM    posln" +;
    " WHERE   (style = '"+ ALLTRIM(STRTRAN(XSTYLE,"'","''")) +"') AND (cbusdocu = 'P') AND (cstytype IN ('U', 'P')) AND (cinvtype = '0001') AND (TRANCD='2')" +;
    " GROUP BY style"
  *B608157,1 WLD  [End]
  
  lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmtPOS, 'POS' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE")
  IF lnResult = 1
    SELE POS
    REPLACE RcvWH1 WITH POS.POS1,RcvWH2 WITH POS.POS2,RcvWH3 WITH POS.POS3,RcvWH4 WITH POS.POS4,;
      RcvWH5 WITH POS.POS5,RcvWH6 WITH POS.POS6,RcvWH7 WITH POS.POS7,RcvWH8 WITH POS.POS8,TOTRcvWH WITH POS.TOTPOS IN (lcWorkfile)
  ELSE
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF

  *--End of lfRcvWhs.
  *!*************************************************************
  *! Name      : lfQtyBok
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Collect the quantity data of BOOKINGS.
  *!*************************************************************
FUNCTION lfQtyBok

  REPLACE BOK1 WITH M.Ord1+M.Shp1,BOK2 WITH M.Ord2+M.Shp2,BOK3 WITH M.Ord3+M.Shp3,BOK4 WITH M.Ord4+M.Shp4,;
    BOK5 WITH M.Ord5+M.Shp5,BOK6 WITH M.Ord6+M.Shp6,BOK7 WITH M.Ord7+M.Shp7,BOK8 WITH M.Ord8+M.Shp8,TOTBOK WITH M.TOTOrd+M.TOTShp IN (lcWorkfile)

  *--End of lfQtyBok.
  *!*************************************************************
  *! Name      : lfOHots
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Collect the quantity data of O.T.S. (OPEN+HOLD ORDERS).
  *!*************************************************************
FUNCTION lfOHots

  REPLACE OHots1 WITH (M.WIP1+&lcWorkfile..RcvWH1)-(M.Ord1+M.Shp1),;
    OHots2 WITH (M.WIP2+&lcWorkfile..RcvWH2)-(M.Ord2+M.Shp2),;
    OHots3 WITH (M.WIP3+&lcWorkfile..RcvWH3)-(M.Ord3+M.Shp3),;
    OHots4 WITH (M.WIP4+&lcWorkfile..RcvWH4)-(M.Ord4+M.Shp4),;
    OHots5 WITH (M.WIP5+&lcWorkfile..RcvWH5)-(M.Ord5+M.Shp5),;
    OHots6 WITH (M.WIP6+&lcWorkfile..RcvWH6)-(M.Ord6+M.Shp6),;
    OHots7 WITH (M.WIP7+&lcWorkfile..RcvWH7)-(M.Ord7+M.Shp7),;
    OHots8 WITH (M.WIP8+&lcWorkfile..RcvWH8)-(M.Ord8+M.Shp8),;
    TOTOHots WITH (M.TOTWIP+&lcWorkfile..TOTRcvWH)-(M.TOTOrd+M.TOTShp) IN (lcWorkfile)

  *--End of lfOHots.
  *!*************************************************************
  *! Name      : lfOots
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Collect the quantity data of O.T.S. (OPEN ORDERS ONLY).
  *!*************************************************************
FUNCTION lfOots

  STORE 0 TO OOrd1,OOrd2,OOrd3,OOrd4,OOrd5,OOrd6,OOrd7,OOrd8,TotOOrd
  SELECT ORDLINE
  =GFSEEK(XSTYLE,'ORDLINE')
  SCAN REST WHILE STYLE = XSTYLE FOR ORDHDR.STATUS='O'
    m.OOrd1 = M.OOrd1 + ORDLINE.QTY1
    m.OOrd2 = M.OOrd2 + ORDLINE.QTY2
    m.OOrd3 = M.OOrd3 + ORDLINE.QTY3
    m.OOrd4 = M.OOrd4 + ORDLINE.QTY4
    m.OOrd5 = M.OOrd5 + ORDLINE.QTY5
    m.OOrd6 = M.OOrd6 + ORDLINE.QTY6
    m.OOrd7 = M.OOrd7 + ORDLINE.QTY7
    m.OOrd8 = M.OOrd8 + ORDLINE.QTY8
    m.TotOOrd = M.TotOOrd + ORDLINE.TotQTY
  ENDSCAN

  REPLACE Oots1 WITH (M.WIP1+&lcWorkfile..RcvWH1)-(M.OOrd1+M.Shp1),;
    Oots2 WITH (M.WIP2+&lcWorkfile..RcvWH2)-(M.OOrd2+M.Shp2),;
    Oots3 WITH (M.WIP3+&lcWorkfile..RcvWH3)-(M.OOrd3+M.Shp3),;
    Oots4 WITH (M.WIP4+&lcWorkfile..RcvWH4)-(M.OOrd4+M.Shp4),;
    Oots5 WITH (M.WIP5+&lcWorkfile..RcvWH5)-(M.OOrd5+M.Shp5),;
    Oots6 WITH (M.WIP6+&lcWorkfile..RcvWH6)-(M.OOrd6+M.Shp6),;
    Oots7 WITH (M.WIP7+&lcWorkfile..RcvWH7)-(M.OOrd7+M.Shp7),;
    Oots8 WITH (M.WIP8+&lcWorkfile..RcvWH8)-(M.OOrd8+M.Shp8),;
    TOTOots WITH (M.TOTWIP+&lcWorkfile..TOTRcvWH)-(M.TOTOOrd+M.TOTShp) IN (lcWorkfile)

  *--End of lfOots.
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/08/2004
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
*--
LOCAL lnAlias
lnAlias = SELECT()
*--

lnTotcomp = 0
SELECT(lcCursorLoc)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcCursorLoc)
  LOCATE 
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE cstymajor=lcFab AND DYELOT =''
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  

SELECT(lnAlias)

RETURN INT(lnTotcomp)
*-- end of lfFabSum.
