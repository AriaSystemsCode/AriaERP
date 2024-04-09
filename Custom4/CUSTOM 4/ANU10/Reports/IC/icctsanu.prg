*:************************************************************************
*: Program  : ICCTSANU.PRG
*: Desc     : CUT & SOLD REPORT CUSTOMIZED FOR ANU100 (CONVERTED FROM 2.6 TO 2.7)
*: System   : ARIA27
*: Developer: Ahmed Salah
*: Date     : 26/02/2007
*: Entry-A27: C#102316
*: Entry-A4 : C#200753
*:*************************************************************************
*:Modifications:
*:
*:*************************************************************************

llMultiWH  = gfGetMemVar('M_WareHouse')='Y'

XRPT_CD='C'
IF llMultiWH
  STORE 'N'  TO lcLoc   
ENDIF
*--------------------------------------------------------------
STORE SPACE(20) TO HPDESC
XFILTER = ' '

*Imported/Domistec
STORE .F. TO llMake,llByStyle,llBySeason,llByDiv,llByFab,llBySG,llByClr
STORE "" TO lcTempSty,lcSesStr,lcSesTemp,lcDivStr,lcDivTemp,lcTempFab,lcSGStr,lcSGTemp,;
            lcPattern,lcStatus,lnClrPos,lcClrStr,lcClrTemp,lcStyFile
STORE {} TO ldOrdDate1,ldOrdDate2,ldInvDate1,ldInvDate2,ldCTDate1,ldCTDate2
STORE 0 TO lnStyPos,lnSesPos,lnDivPos ,lnFabPos,lnSGPos,lnPatPos,lnStatPos,lnOrdDatePos,lnInvDatePos,;
           lnCTDatePos
IF llOgFltCh
  =lfGetFilterVars()
  *--Clear temp file if it is found
  =lfClearRep()
  SELECT STYLE
  lcStyTmp    = loOgScroll.gfTempName()
  lcPrintTemp = loOgScroll.gfTempName()
  SELECT * FROM Style WHERE .F. INTO DBF (oAriaApplication.WorkDir+lcStyTmp+".DBF") READWRITE
  =lfGetStyles()
  =lfCreateTmp2Print()
  SELECT(lcStyTmp)
  INDEX ON Season+Style TAG &lcStyTmp
  SELECT (lcPrintTemp)
  APPEND FROM (oAriaApplication.WorkDir+lcStyTmp+".DBF")
  GO TOP
  =lfColData()
  *SELECT(lcStyTmp)
  SELECT (lcPrintTemp)
  *-- If no styles copied then exit
  GO TOP
  IF EOF()
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF
  SELECT (lcPrintTemp)
  *APPEND FROM (oAriaApplication.WorkDir+lcStyTmp+".DBF")
  STORE {} TO ldFOrdDt,ldTOrdDt,ldFInvDt,ldTInvDt,ldFCutDt,ldTCutDt
  *--Get values setted from Option Grid
  ldFOrdDt=ldRpOrdFDt
  ldTOrdDt=ldRpOrdTDt
  ldFInvDt=ldRpInvFDt
  ldTInvDt=ldRpInvTDt
  ldFCutDt=ldRpCttFDt
  ldTCutDt=ldRpCttTDt
  USE IN (lcPrintTemp)
  DIMENSION loOgScroll.laCRTables[2]
  DIMENSION loOgScroll.laCRParams[2,2]
  =lfAdjustCRSettings()
ENDIF
=gfDispRe()
RETURN

*--Set relations without additive clause
*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : When funciton for option grid.
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRepWhen()
*:***************************************************************************
*:
FUNCTION lfwRepWhen

= gfOpenTable(oAriaApplication.DataDir + 'Style', 'Style', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'Codes', 'Codes', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'Warehous', 'Warehous', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'InvHdr', 'InvHdr', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'InvLine', 'InvLineO', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'OrdHdr', 'OrdHdr', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'OrdLine', 'OrdLineS', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'POSHDR', 'POSHDR', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'POSLN', 'POSLNS', 'SH')


*-- end of lfwRepWhen

*:**************************************************************************
*:* Name        : lfMajTtGet
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : To get the style major segement title
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMajTtGet()
*:***************************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*:**************************************************************************
*:* Name        : lfMajPic
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*!* Purpose     : Get major seg. picture
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMajPic()
*:***************************************************************************
FUNCTION lfMajPic
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

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

*:**************************************************************************
*:* Name        : lfSetSty  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : Set-valid -reset  valid function for style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSetSty  ()
*:***************************************************************************
FUNCTION lfSetSty  
PARAMETERS lcParam
PRIVATE lcSlct
lcSlct = SELECT(0)

SELECT STYLE
IF lcParam='S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   LOCATE
ELSE   
   SET ORDER TO TAG STYLE IN STYLE    
ENDIF

SELECT (lcSlct)
*-- end of lfSetSty.

*:**************************************************************************
*:* Name        : lfvFabric
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/06/2001
*:* Purpose     : Validate fabric
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvFabric()
*:***************************************************************************
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
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
*-- end of lfvFabric.

*:**************************************************************************
*:* Name        : lfClearRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/11/2001
*:* Purpose     : Closes opened temp files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfClearRep()
*:***************************************************************************
FUNCTION lfClearRep
llOGFltCh = .T.  
*IF USED(lcStyTmp)
*  USE IN (lcStyTmp)
*  ERASE &gcWorkDIr.&lcStyTmp..DBF
*  ERASE &gcWorkDIr.&lcStyTmp..CDX
*ENDIF
*-- end of lfClearRep.

*:**************************************************************************
*:* Name        : lfGetFilterVars
*:* Developer   : SSH - Ahmed Salah
*:* Date        : 26/02/2007
*:* Purpose     : Get Filter
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetFilterVars()
*:***************************************************************************
FUNCTION lfGetFilterVars


*Style File Filter
*Imported/Domistec
IF lcRPDomImp<>"B"
  llMake=(lcRPDomImp="D")
ENDIF
*Style

lnStyPos  = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"STYLE.CSTYMAJOR "),1)
lcTempSty = laOGVRFlt[lnStyPos,6]
llByStyle = USED(lcTempSty)
IF llByStyle
  SELECT(lcTempSty)
  DELETE FOR EMPTY(CSTYMAJOR)
  GO TOP
  llByStyle = !EOF()
ENDIF

*Season
*

lnSesPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"STYLE.SEASON    "),1)
lcSesStr = laOGVRFlt[lnSesPos,6]
*lcString , lccursor , lcFieldsName
lcSesTemp = loOgScroll.gfTempName()
=lfStr2Curs(lcSesStr,lcSesTemp,"Season")
llBySeason = USED(lcSesTemp)
IF llBySeason
  SELECT(lcSesTemp)
  DELETE FOR EMPTY(Season)
  GO TOP
  llBySeason = !EOF()
ENDIF


*Division
lnDivPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"STYLE.CDIVISION "),1)
lcDivStr = laOGVRFlt[lnDivPos,6]
*lcString , lccursor , lcFieldsName
lcDivTemp = loOgScroll.gfTempName()
=lfStr2Curs(lcDivStr,lcDivTemp,"Division")
llByDiv = USED(lcDivTemp)
IF llByDiv
  SELECT(lcDivTemp)
  DELETE FOR EMPTY(Division)
  GO TOP
  llByDiv = !EOF()
ENDIF

*Primarey Fabric

lnFabPos     = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"STYLE.FABRIC    "),1)
lcTempFab   = laOGVRFlt[lnFabPos,6]
llByFab = USED(lcTempFab)
IF llByFab
  SELECT(lcTempFab)
  DELETE FOR EMPTY(cStyMajor)
  GO TOP
  llByFab = !EOF()
ENDIF

*Style Group

lnSGPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"STYLE.CSTYGROUP "),1)
lcSGStr = laOGVRFlt[lnSGPos,6]
*lcString , lccursor , lcFieldsName
lcSGTemp = loOgScroll.gfTempName()
=lfStr2Curs(lcSGStr,lcSGTemp,"CSTYGROUP")
llBySG = USED(lcSGTemp)
IF llBySG
  SELECT(lcSGTemp)
  DELETE FOR EMPTY(CSTYGROUP)
  GO TOP
  llBySG = !EOF()
ENDIF


*Pattern
lnPatPos  = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"STYLE.PATTERN   "),1)
lcPattern = laOGVRFlt[lnPatPos,6]

*Status
lnStatPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"STYLE.STATUS    "),1)
lcStatus  = laOGVRFlt[lnStatPos,6]

*Color
lnClrPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)"),1)
lcClrStr = laOGVRFlt[lnClrPos,6]
*lcString , lccursor , lcFieldsName
lcClrTemp = loOgScroll.gfTempName()
=lfStr2Curs(lcClrStr,lcClrTemp,"Color")
llByClr = USED(lcSGTemp)
IF llByClr
  SELECT(lcClrTemp)
  DELETE FOR EMPTY(Color)
  GO TOP
  llByClr = !EOF()
ENDIF


*Sales Order Completeion Date
lnOrdDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"ORDHDR.COMPLETE"),1)
ldOrdDate1    = SUBSTR(laOGFxFlt[lnOrdDatePos,6],1,ATC('|',laOGFxFlt[lnOrdDatePos,6])-1)
ldOrdDate2    = SUBSTR(laOGFxFlt[lnOrdDatePos,6],  ATC('|',laOGFxFlt[lnOrdDatePos,6])+1)

*Invoice Date

lnInvDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"INVHDR.INVDATE"),1)
ldInvDate1    = SUBSTR(laOGFxFlt[lnInvDatePos,6],1,ATC('|',laOGFxFlt[lnInvDatePos,6])-1)
ldInvDate2    = SUBSTR(laOGFxFlt[lnInvDatePos,6],  ATC('|',laOGFxFlt[lnInvDatePos,6])+1)

*CT Entered Date

lnCTDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"CUTTKTH.ENTERED"),1)
ldCTDate1    = SUBSTR(laOGFxFlt[lnCTDatePos,6],1,ATC('|',laOGFxFlt[lnCTDatePos,6])-1)
ldCTDate2    = SUBSTR(laOGFxFlt[lnCTDatePos,6],  ATC('|',laOGFxFlt[lnCTDatePos,6])+1)


*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ahmed Salah
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
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


*:**************************************************************************
*:* Name        : lfGetStyles
*:* Developer   : SSH - Ahmed Salah
*:* Date        : 26/02/2007
*:* Purpose     : Get Selected Style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetStyles()
*:***************************************************************************
FUNCTION lfGetStyles
lcValidStr=lfValidSty()

SELECT Style
=gfSetOrder("Style")
IF llByStyle
  SELECT (lcTempSty)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT Style
    =gfSeek(ALLTRIM(m.cStyMajor))
    SCAN FOR cStyMajor=m.cStyMajor
      SCATTER MEMVAR MEMO
      IF &lcValidStr.
        SELECT(lcStyTmp)
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF
    ENDSCAN
  ENDSCAN
ELSE
  =gfSeek("")
  GO TOP
  SCAN FOR &lcValidStr.
    SCATTER MEMVAR MEMO
    INSERT INTO (lcStyTmp) FROM MEMVAR 
*!*	    SELECT(lcStyTmp)
*!*	    APPEND BLANK
*!*	    GATHER MEMVAR MEMO
  ENDSCAN
ENDIF
SELECT Style
=gfSetOrder("Style")

*:**************************************************************************
*:* Name        : lfValidSty
*:* Developer   : SSH - Ahmed Salah
*:* Date        : 26/02/2007
*:* Purpose     : Check if style Valid
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfValidSty()
*:***************************************************************************
FUNCTION lfValidSty
PRIVATE lcValidStr

lcValidStr = ".T. "
IF lcRPDomImp<>"B"
  lcValidStr = lcValidStr +" AND "+" IIF(lcRPDomImp=[D],Style.Make,!Style.Make)"
ENDIF
IF llBySeason
  lcValidStr = lcValidStr +" AND "+" lfValidCode([Style.Season],[Season],lcSesTemp)"
ENDIF
IF llByDiv
  lcValidStr = lcValidStr +" AND "+" lfValidCode([Style.cDivision],[Division],lcDivTemp)"
ENDIF
IF llByFab
  lcValidStr = lcValidStr +" AND "+" lfValidCode([Style.Fabric],[cStyMajor],lcTempFab)"
ENDIF
IF llBySG
  lcValidStr = lcValidStr +" AND "+" lfValidCode([STYLE.CSTYGROUP],[CSTYGROUP],lcSGTemp)"
ENDIF
IF llByClr
  lcValidStr = lcValidStr +" AND "+" lfValidCode([SUBSTR(STYLE.Style,lnClrPo,lnColorLen)],[Color],lcClrTemp)"
ENDIF
IF !EMPTY(lcStatus)
  lcValidStr = lcValidStr +" AND "+" STYLE.Status$'"+lcStatus+"'"
ENDIF
IF !EMPTY(lcPattern)
  lcValidStr = lcValidStr +" AND "+" STYLE.Pattern$'"+lcPattern+"'"
ENDIF

RETURN lcValidStr

*:**************************************************************************
*:* Name        : lfValidCode
*:* Developer   : SSH - Ahmed Salah
*:* Date        : 26/02/2007
*:* Purpose     : Code Validation
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfValidCode()
*:***************************************************************************
FUNCTION lfValidCode
LPARAMETERS lcCode,lcField,lcAlias

PRIVATE lnOldAls,llFound
lnOldAls = SELECT(0)
SELECT (lcAlias)
LOCATE FOR &lcField. = &lcCode.
llFound = FOUND()
SELECT(lnOldAls)
RETURN(llFound)

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Ahmed Salah
*! Date      : 26/02/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

loOgScroll.lcOGLastForm ='ICCTSANU'
loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcPrintTemp+ ".DBF"
  

loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'CUT AND SOLD REPORT'

loOgScroll.laCRParams[2,1] = 'lctitle'
loOgScroll.laCRParams[2,2] = lcRPTitle

*:**************************************************************************
*:* Name        : lfCreateTmp2Print
*:* Developer   : SSH - Ahmed Salah
*:* Date        : 26/02/2007
*:* Purpose     : Create Temp File
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCreateTmp2Print()
*:***************************************************************************
FUNCTION lfCreateTmp2Print

DIMENSION laDtlFile[17,4]
laDtlFile[1,1] = 'Style'
laDtlFile[1,2] = 'C'
laDtlFile[1,3] = 19
laDtlFile[1,4] = 0

laDtlFile[2,1] = 'Desc'
laDtlFile[2,2] = 'C'
laDtlFile[2,3] = 15
laDtlFile[2,4] = 0

laDtlFile[3,1] = 'nSldQty'
laDtlFile[3,2] = 'N'
laDtlFile[3,3] = 6
laDtlFile[3,4] = 0

laDtlFile[4,1] = 'nSldAmt'
laDtlFile[4,2] = 'N'
laDtlFile[4,3] = 9
laDtlFile[4,4] = 2

laDtlFile[5,1] = 'nCutQty'
laDtlFile[5,2] = 'N'
laDtlFile[5,3] = 6
laDtlFile[5,4] = 0


laDtlFile[6,1] = 'nCutAmt'
laDtlFile[6,2] = 'N'
laDtlFile[6,3] = 9
laDtlFile[6,4] = 2

laDtlFile[7,1] = 'nIssQty'
laDtlFile[7,2] = 'N'
laDtlFile[7,3] = 5
laDtlFile[7,4] = 0


laDtlFile[8,1] = 'nOnHQty'
laDtlFile[8,2] = 'N'
laDtlFile[8,3] = 7
laDtlFile[8,4] = 0

laDtlFile[9,1] = 'nShpQty'
laDtlFile[9,2] = 'N'
laDtlFile[9,3] = 7
laDtlFile[9,4] = 0

laDtlFile[10,1] = 'nShpAmt'
laDtlFile[10,2] = 'N'
laDtlFile[10,3] = 9
laDtlFile[10,4] = 2

laDtlFile[11,1] = 'nToShip'
laDtlFile[11,2] = 'N'
laDtlFile[11,3] = 7
laDtlFile[11,4] = 0

laDtlFile[12,1] = 'nToSell'
laDtlFile[12,2] = 'N'
laDtlFile[12,3] = 7
laDtlFile[12,4] = 0

laDtlFile[13,1] = 'PriceA'
laDtlFile[13,2] = 'N'
laDtlFile[13,3] = 10
laDtlFile[13,4] = 2

laDtlFile[14,1] = 'Season'
laDtlFile[14,2] = 'C'
laDtlFile[14,3] = 6
laDtlFile[14,4] = 0

laDtlFile[15,1] = 'cstymajor'
laDtlFile[15,2] = 'C'
laDtlFile[15,3] = 19
laDtlFile[15,4] = 0

laDtlFile[16,1] = 'nPosOTS'
laDtlFile[16,2] = 'N'
laDtlFile[16,3] = 12
laDtlFile[16,4] = 2


laDtlFile[17,1] = 'nPOsAmt'
laDtlFile[17,2] = 'N'
laDtlFile[17,3] = 12
laDtlFile[17,4] = 2

*-Create Temp File
=gfCrtTmp(lcPrintTemp,@laDtlFile)
SELECT (lcPrintTemp)
*SELECT * FROM (lcPrintTemp) INTO DBF C:\lcPrintTem.DBF

*:**************************************************************************
*:* Name        : lfColData
*:* Developer   : SSH - Ahmed Salah
*:* Date        : 26/02/2007
*:* Purpose     : Collecting Data
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfColData()
*:***************************************************************************
FUNCTION lfColData


STORE 0 TO lnPosAmt,lntotalOPS


lcOrdDateFilter = ".T. "
IF !EMPTY(ldOrdDate1) AND !EMPTY(ldOrdDate2)
  ldOrdDate1    = CTOD(ldOrdDate1)
  ldOrdDate2    = CTOD(ldOrdDate2)
  lcOrdDateFilter = lcOrdDateFilter+" .AND. "+'Complete>=ldOrdDate1 AND Complete<=ldOrdDate2'
ENDIF

lcCutDateFilter = ".T. "
IF !EMPTY(ldCTDate1) AND !EMPTY(ldCTDate2)
  ldCTDate1= CTOD(ldCTDate1)
  ldCTDate2= CTOD(ldCTDate2)
  lcCutDateFilter = lcCutDateFilter+" .AND. "+'POSHDR.Entered>=ldCTDate1 AND POSHDR.Entered<=ldCTDate2'
ENDIF

lcInvDateFilter = ".T. "
IF !EMPTY(ldInvDate1) AND !EMPTY(ldInvDate2)
  ldInvDate1= CTOD(ldInvDate1)
  ldInvDate2= CTOD(ldInvDate2)
  lcInvDateFilter = lcInvDateFilter+" .AND. "+'InvDate>=ldInvDate1 AND InvDate<=ldInvDate2'
ENDIF

SELECT (lcPrintTemp)
SCAN
  SCATTER MEMVAR MEMO
  WAIT WINDOW "Select Data for Style= "+m.Style NOWAIT 
  STORE 0 TO lnSldQty,lnSldAmt,lnShpAmt,lnCutQty,lnIssQty,lnShpQty
  * Get Sold Qty and Amount---[Begin]
  SELECT(lcStyTmp)
  LOCATE FOR Style=m.style
  SELECT OrdLine
  =gfSeek(m.Style)
  SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6)=m.Style;
            FOR &lcOrdDateFilter.
    SELECT OrdHdr
    =gfSeek("O"+OrdLine.Order)
    IF !(OrdHdr.Status $ 'X')
      lcOrdLineKey=OrdLine.Order + STR(OrdLine.LineNo,6)
      SELECT InvLine
      =gfSeek(lcOrdLineKey)
      SELECT OrdLine
      lnSldQty=lnSldQty+TotQty+InvLine.TotQty
      lnSldAmt=lnSldAmt+((TotQty+InvLine.TotQty)*Price)
    ENDIF
  ENDSCAN
  * Get Sold Qty and Amount---[End]
  
  * Get CutTktL Qty and Amount---[Begin]
  SELECT POSLN
  *"CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD"
  =gfSeek('0001'+m.Style+"PU")
  SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD='0001'+m.Style+"PU" &&;FOR POSLN.cBusDocu = 'P' AND POSLN.cStyType = 'U' AND POSLN.CINVTYPE='0001'
    SELECT POSHDR
    =gfSeek("PU"+POSLN.PO)
    IF POSHDR.Status $ 'OHAC' AND &lcCutDateFilter.
      SELECT POSLN
      IF TRANCD='1'
        lnCutQty=lnCutQty+TotQty
      ENDIF   
      IF POSHDR.Status $ 'OH'
        DO CASE
          CASE TRANCD='1'
            lnIssQty=lnIssQty+TotQty
          CASE TRANCD='2'
            lnIssQty=lnIssQty-TotQty
          CASE TRANCD='3'
            lnIssQty=lnIssQty-TotQty
          CASE TRANCD='4'
            lnIssQty=lnIssQty-TotQty
        ENDCASE
      ENDIF
    ENDIF
  ENDSCAN
  lnCutAmt = lnCutQty*&lcStyTmp..PriceA
  lnOnHQty = &lcStyTmp..TotStk
  * Get CutTktL Qty and Amount---[End]

  * shipped [Begind]  
  SELECT InvLine
  =gfSetOrder("Invlines")
  =gfSeek(m.Style)
  SCAN WHILE Style=m.Style FOR &lcInvDateFilter.
    lnShpQty=lnShpQty+TotQty
    lnShpAmt=lnShpAmt+(TotQty*Price)
  ENDSCAN
  SELECT InvLine
  =gfSetOrder("InvLineO")

  * shipped [End]  
  *--To ship
  lnToShip =lnSldQty -lnShpQty

  *--To sell
  lnToSell =lnCutQty -lnSldQty
  
  SELECT (lcPrintTemp)
  lntotalOPS = lntotalOPS + IIF(lnToSell > 0,lnToSell,0)
  lnPosAmt = lnPosAmt + IIF(lnToSell*&lcStyTmp..PriceA > 0, lnToSell*&lcStyTmp..PriceA, 0)
  
  REPLACE nSldQty WITH lnSldQty,;
          nSldAmt WITH lnSldAmt ,;
          nCutQty WITH lnCutQty,;
          nCutAmt WITH lnCutAmt,;
          nIssQty WITH lnIssQty,;
          nOnhQty WITH lnOnHQty,;
          nShpQty WITH lnShpQty,;
          nShpAmt WITH lnShpAmt,;
          nToShip WITH lnToShip,;
          nToSell WITH lnToSell,;
          Pricea  WITH &lcStyTmp..PriceA,;
          Season  WITH m.Season,;
          nPosOTS WITH lntotalOPS,;
          nPOsAmt WITH lnPosAmt
  
ENDSCAN
SELECT (lcPrintTemp)
SELECT MAX(nPosOTs) FROM ALIAS() INTO ARRAY lanPosOTs
SELECT MAX(nPOsAmt) FROM ALIAS() INTO ARRAY lanPOsAmt
REPLACE ALL nPosOTs WITH lanPosOTs,;
            nPOsAmt WITH lanPOsAmt