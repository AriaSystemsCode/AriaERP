*:***************************************************************************
*: Program file  : POSTPRD.PRG
*: Program desc. : Custom Style Production Summary [FLO09]
*: TRACK NO      : C200821
*: System        : Aria4XP
*: Module        : Style Purchase Order (PO)
*: Developer     : Walid Hamed (WLD)
*! Date          : 07/12/2007
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTPRD
*:***************************************************************************
*: Modification:
*: C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[T20080214.0006]
*: B608619,1 MMT 07/22/2008 Fix bug of Can not Filter by style [T20080711.0015]
*: C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [T20080214.0006]
*: C200968,3 MMT 08/07/2008 Fix bug of not printing Material info.  [T20080711.0015]
*: B608682,1 MMT 09/04/2008 Fix bug of not prinitng layout # 2      [T20080711.0015]
*:***************************************************************************

*--Initialize the variables.


IF llOgFltCh

  STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl,lnLineNo
  STORE '' TO lcRpStatus
  STORE '.T. ' TO lcSeleCond
  STORE '' TO xStyle,lcSblName,lcSblColor,lcTrmColor
  STORE 'N' TO MConsump
  =lfChkStrct()
  llExtSize = gfGetMemVar('M_USEEXSSC')
  llDontPrn = .F.
  lnMjrLen   = LEN(gfItemMask("PM"))             && Style major length.
  llMScale   = gfGetMemVar('M_USEEXSSC')
  lnSepMins  = lfvGtSclSp()                      && the length of Scale with Separator.
  lnColorLen = LEN(gfItemMask("PN"))-lnSepMins   && Style Color length.

  =lfcreatfilter()
  XFILTER = lcRPExp
  WAIT WINDOW 'Collecting data....' NOWAIT
  lcWorkfile  = loOGScroll.gfTempName()
  LCTMPFILE   = loOGScroll.gfTempName()

  SELECT STYLE
  SET ORDER TO  STYLE   && STYLE
  SET RELATION TO 'S' + SCALE INTO SCALE
  lcOOrdHdr = ORDER('OrdHdr')
  =lfBuildTmp()
  IF lpCollecData()
  ELSE
    RETURN .F.
  ENDIF

  WAIT CLEAR
  IF !llDontPrn
    IF USED(LCTMPFILE)
      USE IN (LCTMPFILE)
    ENDIF
    =gfDispRe()
  ENDIF
  SELECT ordHdr
  SET ORDER TO  &lcOOrdHdr
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
  =lfNonMaj()

  gfOpenTable('Item','cstyle','SH')
  
  *: C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[Start]
  gfOpenTable('PROFVALU','PROFILE','SH')
  *: C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[End]
	

  * lnClrSgPo = ASUBSCRIPT(laOGFxFlt,;
  ASCAN(laOGFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

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

  *-- If neither the "PO" module is installed
  IF !('PO' $ oAriaApplication.CompanyInstalledModules )
    *** Message : "Since neither the PO module"
    ***           "is installed, you cannot run this report.                 "
    ***
    *** Buttons : "                         < Ok >                           "
    =gfModalgen("TRM42171B00000","ALERT")

    *-- Terminate the Option Grid.
    llOgTrmnat = .T.
  ENDIF
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
  DIMENSION laFileStru[61,4]

  laFileStru[ 1 , 1 ] = 'STYLE'
  laFileStru[ 1 , 2 ] = 'C'
  laFileStru[ 1 , 3 ] = lnMjrLen
  laFileStru[ 1 , 4 ] = 0

  laFileStru[ 2 , 1 ] = 'Account'
  laFileStru[ 2 , 2 ] = 'C'
  laFileStru[ 2 , 3 ] = 5
  laFileStru[ 2 , 4 ] = 0

  laFileStru[ 3 , 1 ] = 'CustName'
  laFileStru[ 3 , 2 ] = 'C'
  laFileStru[ 3 , 3 ] = 30
  laFileStru[ 3 , 4 ] = 0

  laFileStru[ 4 , 1 ] = 'COLOR'
  laFileStru[ 4 , 2 ] = 'C'
  laFileStru[ 4 , 3 ] = lnColorLen
  laFileStru[ 4 , 4 ] = 0

  laFileStru[ 5 , 1 ] = 'CSBLNAME'
  laFileStru[ 5 , 2 ] = 'C'
  laFileStru[ 5 , 3 ] = 30
  laFileStru[ 5 , 4 ] = 0

  laFileStru[ 6, 1 ] = 'CSBLCOLOR'
  laFileStru[ 6, 2 ] = 'C'
  laFileStru[ 6, 3 ] = 30
  laFileStru[ 6, 4 ] = 0

  laFileStru[ 7, 1 ] = 'CTRMCOLOR'
  laFileStru[ 7, 2 ] = 'C'
  laFileStru[ 7, 3 ] = 30
  laFileStru[ 7, 4 ] = 0

  laFileStru[ 8, 1 ] = 'FABRIC'
  laFileStru[ 8, 2 ] = 'C'
  laFileStru[ 8, 3 ] = 7
  laFileStru[ 8, 4 ] = 0

  laFileStru[ 9, 1 ] = 'FABNAME'
  laFileStru[ 9, 2 ] = 'C'
  laFileStru[ 9, 3 ] = 30
  laFileStru[ 9, 4 ] = 0

  laFileStru[ 10, 1 ] = 'CLRNAME'
  laFileStru[ 10, 2 ] = 'C'
  laFileStru[ 10, 3 ] = 30
  laFileStru[ 10, 4 ] = 0


  FOR I=1 TO 16
    laFileStru[ I+10 , 1 ] = 'SIZ' + ALLTRIM(STR(I))
    laFileStru[ I+10 , 2 ] = 'C'
    laFileStru[ I+10 , 3 ] = 5
    laFileStru[ I+10 , 4 ] = 0
  ENDFOR
  FOR I=1 TO 16
    laFileStru[ I+26 , 1 ] = 'QTY' + ALLTRIM(STR(I))
    laFileStru[ I+26 , 2 ] = 'N'
    laFileStru[ I+26 , 3 ] = 5
    laFileStru[ I+26 , 4 ] = 0
  ENDFOR

  laFileStru[ 43 , 1 ] = 'TOTQTY'
  laFileStru[ 43 , 2 ] = 'N'
  laFileStru[ 43 , 3 ] = 6
  laFileStru[ 43 , 4 ] = 0

  FOR I=1 TO 16
    laFileStru[ I+43 , 1 ] = 'CONS' + ALLTRIM(STR(I))
    laFileStru[ I+43 , 2 ] = 'N'
    laFileStru[ I+43 , 3 ] = 6
    laFileStru[ I+43 , 4 ] = 2
  ENDFOR

  laFileStru[ 60 , 1 ] = 'TOTCONS'
  laFileStru[ 60 , 2 ] = 'N'
  laFileStru[ 60 , 3 ] = 7
  laFileStru[ 60 , 4 ] = 2

  laFileStru[ 61 , 1 ] = 'LINENO'
  laFileStru[ 61 , 2 ] = 'N'
  laFileStru[ 61 , 3 ] = 6
  laFileStru[ 61 , 4 ] = 0


  gfCrtTmp(lcWorkfile ,@laFileStru,,"",.F.)

  SELECT(lcWorkfile)
  INDEX ON STYLE+COLOR+ACCOUNT+ALLTRIM(CSBLNAME)+STR(LINENO,6) TAG (lcWorkfile)
  SET ORDER TO TAG (lcWorkfile)

  SET EXACT &lcExcStat

  *************************************************************
  *! Name      : lfAdjustCRSettings
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : To set the report data files and parameters
  *!*************************************************************
FUNCTION lfAdjustCRSettings

  DIMENSION loOgScroll.laCRTables[1]
  DIMENSION loOgScroll.laCRParams[2,2]

  loOGScroll.cCROrientation='L'
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"


  *:B608682,1 MMT 09/04/2008 Fix bug of not prinitng layout # 2 [Start]
  *loOgScroll.lcOGLastForm ='POSTPRD'
  IF lcRPLayout = 'A'
    loOgScroll.lcOGLastForm ='POSTPRD'
  ELSE
    loOgScroll.lcOGLastForm ='POSTPRDB'
  ENDIF 
  *:B608682,1 MMT 09/04/2008 Fix bug of not prinitng layout # 2 [End]



  loOgScroll.laCRParams[1,1] = 'ReportName'
  loOgScroll.laCRParams[1,2]= 'Custom Style Production Summary'

  loOgScroll.laCRParams[2,1] = 'Consumption'
  loOgScroll.laCRParams[2,2]= MConsump

  *!*************************************************************
  *! Name      : lfcreatfilter
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Creating filter
  *!*************************************************************
FUNCTION lfcreatfilter

  *Creting filter exp   .. begin
  lcRPExp = " .T. "
  * Check if there is a filter on Style CDIVISION
  lcCurName = lfCheckFilter(1, 'STYLE.CDIVISION')
  lcDiv   = loOgScroll.gfTempName()
  llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
  IF llDiv
    SELECT (lcDiv)
    INDEX ON CDivision TAG (lcDiv)
    lcRPExp=lcRPExp+" AND SEEK(Style.CDIVISION,'"+lcDiv+"')"
  ENDIF

  * Check if there is a filter on Style SEASON
  lcCurName = lfCheckFilter(1, 'STYLE.SEASON')
  lcSea  = loOgScroll.gfTempName()
  llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON")
  IF llSea
    SELECT (lcSea  )
    INDEX ON SEASON TAG (lcSea  )
    lcRPExp=lcRPExp+" AND SEEK(Style.SEASON,'"+lcSea+"')"
  ENDIF

  * Check if there is a filter on Style Group
  lcCurName = lfCheckFilter(1, 'STYLE.CSTYGROUP')
  lcGrp  = loOgScroll.gfTempName()
  llGrp   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcGrp  ,"Group")
  IF llGrp
    SELECT (lcGrp  )
    INDEX ON GROUP TAG (lcGrp  )
    lcRPExp=lcRPExp+" AND SEEK(Style.CSTYGROUP,'"+lcGrp+"')"
  ENDIF


  * Check if there is a filter on Style COLOR
  lcCurName = lfCheckFilter(1, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')

  lcCol  = loOgScroll.gfTempName()
  llCol   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol  ,"Color")
  IF llCol
    SELECT (lcCol )
    INDEX ON COLOR TAG (lcCol  )
    lcRPExp=lcRPExp+" AND SEEK(SUBSTR(Style,lnClrPo,lnColorLen),'"+lcCol  +"')"
  ENDIF

  *Check Consumption Filter
  MConsump = lcRPCon

  *--Date Check for date ENTERED
  lnPosEDate = ASCAN(loOgScroll.laOgFXFlt,"ORDHDR.ENTERED")
  IF lnPosEDate > 0
    lnPosEDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosEDate,1)
    SDATE = SUBSTR(laOgFxFlt[lnPosEDate,6],1,10)
    EDATE = SUBSTR(laOgFxFlt[lnPosEDate,6],12,20)
    IF !EMPTY(EDATE) AND !EMPTY(SDATE)
      lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(ORDHDR.ENTERED,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
    ENDIF
  ENDIF
  *--Date Check for date COMPLETE
  lnPosCDate = ASCAN(loOgScroll.laOgFXFlt,"ORDHDR.COMPLETE  ")
  IF lnPosCDate > 0
    lnPosCDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCDate,1)
    SCDATE = SUBSTR(laOgFxFlt[lnPosCDate,6],1,10)
    ECDATE = SUBSTR(laOgFxFlt[lnPosCDate,6],12,20)
    IF !EMPTY(ECDATE) AND !EMPTY(SCDATE)
      lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(COMPLETE,CTOD('"+SCDATE+"'),CTOD('"+ECDATE+"'))"
    ENDIF
  ENDIF

  *!*************************************************************
  *! Name      : lpCollecData
  *! Developer : Walid Hamed (WLD)
  *! Date      : 06/17/2007
  *! Purpose   : Collecting data in Temp. File
  *!*************************************************************

PROCEDURE lpCollecData

  *Major style filter
  lcCurSty= lfCheckFilter(1, 'STYLE.CSTYMAJOR')
  llCurSty   = !EMPTY(lcCurSty) AND USED(lcCurSty) AND RECCOUNT(lcCurSty) > 0
  IF llCurSty
    SELECT (lcCurSty)
    INDEX ON CSTYMAJOR TAG (lcCurSty)
    lcStyFlt = "SEEK(STYLE.CSTYMAJOR ,'"+lcCurSty+"')"
  ELSE
    IF TYPE("lcCurSty") = "C" AND USED(lcCurSty)
      USE IN (lcCurSty)
    ENDIF
    lcCurSty= ''
  ENDIF

  *--Check user selected accounts
  llAccSel =.F.
  lcAccFlt = '.T.'
  lnPosOrdAcc = ASCAN(loOgScroll.laOgFXFlt,"CUSTOMER.ACCOUNT")
  IF lnPosOrdAcc > 0
    lnPosOrdAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrdAcc,1)
    lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosOrdAcc,6]),laOgFxFlt[lnPosOrdAcc,6],'')
    llAccSel   = !EMPTY(lcAccSel) AND USED(lcAccSel) AND RECCOUNT(lcAccSel) > 0
  ENDIF
  IF llAccSel
    SELECT (lcAccSel)
    INDEX ON Account TAG (lcAccSel)
    lcAccFlt = "SEEK(ACCOUNT ,'"+lcAccSel+"')"
  ELSE
    IF TYPE("lcAccSel") = "C" AND USED(lcAccSel)
      USE IN (lcAccSel)
    ENDIF
    lcAccSel= ''
  ENDIF
  *--Check user selected Orders
  llOrdSel =.F.
  lcOrdFlt = '.T.'
  lnPosOrd = ASCAN(loOgScroll.laOgFXFlt,"ORDHDR.ORDER")
  IF lnPosOrd > 0
    lnPosOrd = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrd,1)
    lcOrdSel =IIF(!EMPTY(laOgFxFlt[lnPosOrd,6]),laOgFxFlt[lnPosOrd,6],'')
    llOrdSel   = !EMPTY(lcOrdSel) AND USED(lcOrdSel) AND RECCOUNT(lcOrdSel) > 0
  ENDIF
  IF llOrdSel
    SELECT (lcOrdSel)
    INDEX ON ORDER TAG (lcOrdSel)
    lcOrdFlt = "SEEK(ORDER ,'"+lcOrdSel+"')"
  ELSE
    IF TYPE("lcOrdSel") = "C" AND USED(lcOrdSel)
      USE IN (lcOrdSel)
    ENDIF
    lcOrdSel= ''
  ENDIF
  *----------------

  *-Get by Style
  DO CASE
    CASE llCurSty
      SELECT ordline
      SET ORDER TO  ORDLINES   && STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)
      SELECT STYLE
      *SET ORDER TO CSTYLE   && CSTYMAJOR
      SET ORDER TO Style
      SELECT (lcCurSty)
      SCAN
        lcStyMajor = cStyMajor
        SELECT STYLE
        
        *: B608619,1 MMT 07/22/2008 Fix bug of Can not Filter by style [Start]
        *=gfSEEK(lcStyMajor,'Style')
        =gfSEEK(ALLTRIM(lcStyMajor),'Style')
        *: B608619,1 MMT 07/22/2008 Fix bug of Can not Filter by style [End]
        
        SCAN REST WHILE SUBSTR(Style,1,lnMjrLen) = SUBSTR(lcStyMajor,1,lnMjrLen) FOR &XFILTER
          SELECT ordline
          =gfseek(Style.style,'ordline')
          SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = Style.style ;
              FOR cordtype = 'O' AND &lcAccFlt AND &lcOrdFlt AND &lcSeleCond AND SEEK('O'+Ordline.Order,'OrdHdr') AND ;
              OrdHdr.status='O' AND Ordline.Totqty > 0
             xStyle = STYLE            
            WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
            =lfUpdate()
            SELECT ordline
          ENDSCAN
          SELECT STYLE
        ENDSCAN
        SELECT (lcCurSty)
      ENDSCAN
    *-Collect by Account
    CASE llAccSel
      SELECT ordHdr
      SET ORDER TO ORDACCT   && ACCOUNT+CORDTYPE+ORDER
      SELECT ordline
      SET ORDER TO  ORDLINE   && CORDTYPE+ORDER+STR(LINENO,6)

      SELECT (lcAccSel)
      SCAN
        XACCOUNT = Account
        SELECT OrdHdr
        gfSEEK(XACCOUNT,'OrdHdr') 
        SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER  = XACCOUNT  FOR &lcOrdFlt AND  &lcSeleCond AND OrdHdr.status='O'
          SELECT ordline
          gfseek('O'+OrdHdr.ORDER,'ordline')
          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+OrdHdr.ORDER ;
              FOR gfSEEK(ordline.style,'style') AND &XFILTER AND Ordline.Totqty > 0
            xstyle= ordline.STYLE
            WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
            =lfUpdate()
            SELECT ordline
          ENDSCAN
          SELECT ordhdr
        ENDSCAN
        SELECT (lcAccSel)
      ENDSCAN

    *-Get by Order
    CASE llOrdSel
      SELECT ordHdr
      SET ORDER TO  ORDHDR   && CORDTYPE+ORDER
      SELECT ordline
      SET ORDER TO  ORDLINE   && CORDTYPE+ORDER+STR(LINENO,6)

      SELECT (lcOrdSel)
      SCAN
        xOrder = ORDER
        SELECT OrdHdr
        gfSEEK('O'+xOrder,'OrdHdr')
        SCAN REST WHILE CORDTYPE+ORDER  = 'O'+xOrder  FOR &lcAccFlt AND  &lcSeleCond
          XACCOUNT = Account
          SELECT ordline
          gfseek('O'+OrdHdr.ORDER,'ordline')
          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+OrdHdr.ORDER ;
              FOR gfSEEK(ordline.style,'style') AND &XFILTER AND Ordline.Totqty > 0  
            xstyle= ordline.STYLE
            WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
            =lfUpdate()
            SELECT ordline
          ENDSCAN
          SELECT ordhdr
        ENDSCAN
        SELECT (lcOrdSel)
      ENDSCAN

      *-Get without filters
    CASE !llCurSty AND !llAccSel  AND !llOrdSel
      SELECT ordline
      SET ORDER TO  ORDLINE   && CORDTYPE+ORDER+STR(LINENO,6)

      SELECT ordline
      =gfseek('')
      SCAN REST WHILE CORDTYPE+ORDER  = 'O' FOR  gfSEEK(ordline.style,'style') AND &XFILTER ;
                      AND gfSEEK('O'+Ordline.Order,'OrdHdr') AND &lcSeleCond AND OrdHdr.status='O' AND Ordline.Totqty > 0
        xStyle = STYLE
        WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
        =lfUpdate()
        SELECT ordline
      ENDSCAN
  ENDCASE

  SELECT (LCWORKFILE)
  GO TOP
  IF EOF()
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    llDontPrn = .T.
    RETURN
  ENDIF

  *-----------------

  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"
  USE oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" IN 0 SHARED
  SELECT &LCTMPFILE
  INDEX ON STYLE+ACCOUNT+ALLTRIM(CSBLNAME)+STR(LINENO,6)  TAG LCTMPFILE

  USE

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
  *! Name      : lfsrAcc
  *! Developer : BASSEM RAFAAT ERNEST(BWA)
  *! Date      : 03/05/2002
  *! Purpose   : Change account flag, in range browse screen.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Example   : =lfsrAcc()
  *!*************************************************************
  *! Note      : S symbol is [S,Set] , R symbol isReset
  *!*************************************************************
FUNCTION lfsrAcc
  PARAMETERS lcParm
  PRIVATE lcAliasCst

  lcAliasCst = SELECT(0)
  SELECT Customer
  SET ORDER TO Customer
  GO TOP
  SELECT(lcAliasCst)

  *-- End of lfsrAcc.
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
  *! Name      : lfvGtSclSp
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/09/2007
  *! Purpose   : Function returns the length of Scale with Separator.
  *!*************************************************************
  *! Returns   : len of Scale 3 or 4 if separator exist.
  *!*************************************************************
  *! Example   : = lfvGtSclSp()
  *!*************************************************************
FUNCTION lfvGtSclSp
  PRIVATE lnAlias
  lnAlias = SELECT(0)

  IF llMScale
    =gfOpenTable('ICISTRU','Segno','SH','ICISTRU')
    SELECT ICISTRU
    SCAN
      IF cItemRecty='U' AND cISegType='S'
        SKIP -1
        lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
        EXIT
      ELSE
        lnSepMins = 3
      ENDIF
    ENDSCAN
  ELSE
    lnSepMins = 0
  ENDIF
  SELECT(lnAlias)
  RETURN (lnSepMins)

  *--End of lfvGtSclSp.
  *!***************************************************************************
  *! Name      : lfGetSizes
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/09/2007
  *! Purpose   : to fill the lasize array with sizes of the scale file
  *!***************************************************************************
  *! Called from : Report
  *!***************************************************************************
  *! Example     : = lfGetSizes()
  *!***************************************************************************
FUNCTION lfGetSizes

  PRIVATE lnOldAls , lnRecNo , lnSizNo , lnScaleNo
  DECLARE laSize[2,16]
  STORE '' TO laSize
  STORE 0 TO lnOldAls , lnRecNo , lnSizNo , lnScaleNo
  lnOldAls = SELECT(0)
  lnRecNo  = RECNO('SCALE')
  SELECT SCALE
  SCAN FOR SUBSTR(SCALE,1) = lcScale
    lnScaleNo = lnScaleNo + 1
    
    FOR I=1 TO SCALE.CNT 
      lcI = STR(I,1)
      lnSizNo = lnSizNo + 1
      IF lnSizNo <=16
        laSize[1,lnSizNo] = SZ&lcI
        laSize[2,lnSizNo] = lnScaleNo
      endif
      IF EOF() .OR. lnSizNo >= 16
        EXIT
      ENDIF
    ENDFOR
  ENDSCAN
  IF BETWEEN (lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  SELECT(lnOldAls)

  *-- End of lfGetSizes.

  *!*************************************************************
  *! Name      : lfGetSblCO
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : Function To Collect data from the Socodes File
  *!             and save it in lcTmpCode with codes's Description
  *!*************************************************************
  *! Called from : POSTPRD.PRG
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfGetSblCO()
  *!*************************************************************
FUNCTION lfGetSblCO

  *-- Get Sablon Codes in the temp file
*: C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[Start]
*!*	  IF gfSEEK('O'+ OrdLine.ORDER + STR(OrdLine.LINENO,6),'SOCODES')
*!*	    lcSblName  = ALLTRIM(gfCodDes(SOCODES.CSblName  , 'CSBLNAME' ))
*!*	    lcSblColor = ALLTRIM(gfCodDes(SOCODES.CSblColor , 'CSBLCOLOR'))
*!*	    lcTrmColor = ALLTRIM(gfCodDes(SOCODES.CTrmColor , 'CTRMCOLOR'))
*!*	  ELSE
*!*	    STORE '' TO lcSblName,lcSblColor,lcTrmColor
*!*	  ENDIF
  lcAlias  = SELECT()
  IF gfSeek('SOO'+ OrdLine.ORDER + STR(OrdLine.LINENO,6),'PROFVALU')
    SELECT PROFVALU
    SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'SOO'+ OrdLine.ORDER + STR(OrdLine.LINENO,6)
      IF UPPER(ALLTRIM(gfCodDes(PROFVALU.CPRO_CODE  , 'CPRO_CODE' ))) = 'SCREEN CODE'
 		lcSblName = ALLTRIM(profvalu.cpro_value)       
 		LOOP 
      ENDIF 
      
      IF UPPER(ALLTRIM(gfCodDes(PROFVALU.CPRO_CODE  , 'CPRO_CODE' ))) = UPPER('Accessory Color')
        
        *: C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [Start]
        *lcSblColor = ALLTRIM(profvalu.cpro_value)       
        lcTrmColor= ALLTRIM(profvalu.cpro_value)       
 		*: C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [End]
 		
 		LOOP 
      ENDIF 

      IF UPPER(ALLTRIM(gfCodDes(PROFVALU.CPRO_CODE  , 'CPRO_CODE' ))) = UPPER('Screen Color')
        
        *: C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [Start]
        *lcTrmColor= ALLTRIM(profvalu.cpro_value)       
 		lcSblColor = ALLTRIM(profvalu.cpro_value)      
 		*: C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [End]
 		
 		LOOP 
      ENDIF 
    ENDSCAN 
  ELSE
    STORE '' TO lcSblName,lcSblColor,lcTrmColor
  ENDIF 
  SELECT(lcAlias)
*: C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[End]
  

  *-- END OF FUNCTION lfGetSblCO.
  *!*************************************************************
  *! Name      : lfUpdate
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : Function To Update data to tmp file for the report
  *!*************************************************************
  *! Called from : POSTPRD.PRG
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfUpdate()
  *!*************************************************************
FUNCTION lfUpdate


  XACCOUNT = OrdLine.Account
  =gfseek('M'+XACCOUNT,'Customer')
  lcCustName =  Customer.Stname
  =gfseek('S'+OrdLine.SCALE,'Scale')
  
  *: C200968,3 MMT 08/07/2008 Fix bug of not printing Material info.  [Start]
  *lcFabName = IIF(!EMPTY(STYLE.Fabric) and gfseek(STYLE.Fabric,'Item'), ITEM.DESC,'')
  lcFabName = IIF(!EMPTY(STYLE.Fabric) and gfseek('0002'+STYLE.Fabric,'Item'), ITEM.DESC,'')
  lcCustName  = Style.Desc
  *: C200968,3 MMT 08/07/2008 Fix bug of not printing Material info.  [End]
  
  lcClrName = gfCodDes(SUBSTR(xStyle,lnMjrLen+2,lnColorLen) , 'COLOR')

  IF lcScale <> SUBSTR(SCALE,1,1)
    IF !EMPTY(lcScale)
      lnLineNo = lnLineNo + 1
    ENDIF
    lcScale = SUBSTR(SCALE,1,1)
    =lfGetSizes()
  ENDIF
  =lfGetSblCO()

  IF gfSEEK(SUBSTR(xSTYLE,1,lnMjrLen) +SUBSTR(xStyle,lnMjrLen+2,lnColorLen)+ xAccount + lcSblName ,LCWORKFILE)
    SELECT (LCWORKFILE)
    FOR N = 1 TO SCALE.CNT 
      lcI      = STR(N,1)
      lnSizeNo = ASCAN(laSize , SCALE.SZ&lcI,1)
      IF lnSizeNo>0
        lcI2     = STR(lnSizeNo , IIF(lnSizeNo<10,1,2))
        REPLACE QTY&lcI2         WITH QTY&lcI2  + Ordline.Qty&lCI   ,;
          TotQty           WITH TotQty  + Ordline.Qty&lCI
      endif
    ENDFOR
  ELSE
    SELECT (LCWORKFILE)
    lnLineNo = lnLineNo + 1
    APPEND BLANK
    REPLACE STYLE        WITH SUBSTR(xStyle,1,lnMjrLen)               ,;
      Account            WITH xAccount                                ,;
      CustName           WITH lcCustName                              ,;
      LINENO             WITH lnLineNo                                ,;
      COLOR              WITH SUBSTR(xStyle,lnMjrLen+2,lnColorLen)    ,;
      CSblName           WITH lcSblName                               ,;
      CSblColor          WITH lcSblColor                              ,;
      CTrmColor          WITH lcTrmColor                              ,;
      Fabric             WITH STYLE.Fabric                            ,;
      FabName            WITH lcFabName                               ,;
      ClrName            WITH lcClrName

    FOR N = 1 TO SCALE.CNT 
      lcI      = STR(N,1)
      lnSizeNo = ASCAN(laSize , SCALE.SZ&lcI,1)
      IF lnSizeNo>0
        lcI2     = STR(lnSizeNo , IIF(lnSizeNo<10,1,2))
        REPLACE QTY&lcI2  WITH OrdLine.Qty&lCI ,;
          TotQty           WITH TotQty  + Ordline.Qty&lCI
      endif
    ENDFOR
    FOR N = 1 TO 16
      lcI = STR(N,IIF(N<10,1,2))
      REPLACE SIZ&lcI WITH laSize[1,N],;
        CONS&lcI WITH STYLE.nYeild&lcI
    ENDFOR

  ENDIF
  *-- END OF FUNCTION lfUpdate.
  *--------------------------------
