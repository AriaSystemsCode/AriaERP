*:***************************************************************************
*: Program file       : APFOLDER
*: Program description: Vendor Folder Label Report
*: Module             : Accounts Payable (AP)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: E303061.EXE
*: Date               : 02/13/2012
*:***************************************************************************
*Modifications:
*:***************************************************************************

SET STEP ON 
lcVendorTmp = gfTempName()

=lfCreateTemp()

=lfCollectData()

=lfAdjustCRSettings()

IF USED(lcVendorTmp)
  USE IN (lcVendorTmp)
ENDIF

DO gfDispRe WITH 'APFOLDER','FOR '+lcRpExp,.F.,'L'


*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Option Grid When function
*!*************************************************************
FUNCTION lfwRepWhen

ENDFUNC


*!*************************************************************
*! Name      : lfRepShow
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Option Grid Show
*!*************************************************************
FUNCTION lfRepShow

ENDFUNC


*:*************************************************************
*: Name      : lfSetVenRp
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*: Purpose   : Go top in Vendor file.
*:*************************************************************
FUNCTION lfSetVenRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT APVENDOR
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN

ENDFUNC


*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
PROCEDURE lfCreateTemp
LOCAL lnItemPos
lnItemPos = 0
DIMENSION laStruArr[4, 4]

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cVendCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 61
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cVenComp'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cvencont'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cphoneno'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 16
laStruArr[lnItemPos, 4] = 0

=gfCrtTmp(lcVendorTmp,@laStruArr,'CVENDCODE','VENCODE',.F.)

ENDPROC


*!*************************************************************
*! Name      : lfCollectData
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Procedure to Collect Data into temp file
*!*************************************************************
PROCEDURE lfCollectData

SELECT APVENDOR
SCAN FOR &lcRpExp.
  SCATTER MEMO MEMVAR
  m.cVendCode = ALLTRIM(CVENDCODE)
  m.cVenComp  = ALLTRIM(CVENCOMP)
  m.cVenCont  = ALLTRIM(CVENCONT)
  m.cPhoneNo  = ALLTRIM(CPHONENO)
  SELECT (lcVendorTmp)
  APPEND BLANK
  GATHER MEMO MEMVAR 
ENDSCAN

ENDPROC


*************************************************************
*! Name      : lfAdjustCRSettings
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : To set the report data files and parameters
*!*************************************************************
PROCEDURE lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]

LoOGScroll.llCrystal = .T.
LoOGScroll.lcOGLastForm = 'APFOLDER'
loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcVendorTmp + ".DBF"

ENDPROC


