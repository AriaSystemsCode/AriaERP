*:***************************************************************************
*: Program file       : APVENDLBL
*: Program description: Vendor Mailing Label Report
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

DO gfDispRe WITH EVAL('lcRpForm') ,'FOR '+lcRpExp,.F.,'L'


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


*!*************************************************************
*! Name      : lfAddress
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Address
*!*************************************************************
FUNCTION lfAddress
lnMin = MIN(MIN(SYCINT.NPART3ORD ,SYCINT.NPART4ORD),SYCINT.NPART5ORD)
lnMax = MAX(MAX(SYCINT.NPART3ORD ,SYCINT.NPART4ORD),SYCINT.NPART5ORD)
DO CASE
  CASE SYCINT.NPART3ORD = lnMin
    lcAddr = ALLTRIM(APVENDOR.CADDRESS3)
    IF SYCINT.NPART5ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;  
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)  
    ENDIF
    
  CASE SYCINT.NPART4ORD = lnMin
    lcAddr = ALLTRIM(APVENDOR.CADDRESS4)
    IF SYCINT.NPART5ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;  
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)  
    ENDIF           

  CASE SYCINT.NPART5ORD = lnMin
    lcAddr = ALLTRIM(APVENDOR.CADDRESS5)
    IF SYCINT.NPART4ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)  
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;  
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)
    ENDIF           
ENDCASE  
RETURN lcAddr

ENDFUNC

*!*************************************************************
*! Name      : lfvPltForm
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Plate Form
*!*************************************************************
FUNCTION lfvPltForm
= lfRepPltFr(lcRpForm)

ENDFUNC

*!*************************************************************
*! Name      : lfSetVenRp
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Go top in Vendor file.
*!*************************************************************
FUNCTION lfSetVenRp
PARAMETERS OpGrdParm
PRIVATE lnAlias
lnAlias = SELECT(0)

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
DIMENSION laStruArr[11, 4]

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

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAddress'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAddress1'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAddress2'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAddress3'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAddress4'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAddress5'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAddress6'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
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
  m.cVendCode = IIF(!EMPTY(CVENCONT), ALLTRIM(CVENCONT)+' '+ ALLTRIM(CVENTITLE),ALLTRIM(CVENTITLE))
  m.cVenComp  = ALLTRIM(CVENCOMP)
  m.cVenCont  = gfGetAdr("","",.F.,.F.,1)
  m.cPhoneNo  = gfGetAdr("","",.F.,.F.,2)
  m.cAddress  = lfAddress()
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
*DIMENSION loOgScroll.laCRParams[5,2]

LoOGScroll.llCrystal = .T.
LoOGScroll.lcOGLastForm = lcRpForm
loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcVendorTmp + ".DBF"

*LOCAL lnI 
*lnI  = 0
*lnI = lnI + 1
*loOgScroll.laCRParams[lnI, 1] = 'ReportName'
*loOgScroll.laCRParams[lnI ,2] = ""
 
*lnI = lnI + 1
*loOgScroll.laCRParams[lnI, 1] = 'Layout'
*loOgScroll.laCRParams[lnI, 2] = ""

*lnI = lnI + 1
*loOgScroll.laCRParams[lnI, 1] = 'SortBy'
*loOgScroll.laCRParams[lnI, 2] = ""

*lnI = lnI + 1
*loOgScroll.laCRParams[lnI, 1] = 'OpTitle'
*loOgScroll.laCRParams[lnI, 2] = ""

*lnI = lnI + 1
*loOgScroll.laCRParams[lnI, 1] = 'lnDateFormat'
*loOgScroll.laCRParams[lnI, 2] = IIF(UPPER(ALLTRIM(oAriaApplication.cActCompDateFormat)) == 'BRITISH', 1, 0)

ENDPROC
