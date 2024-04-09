*:***************************************************************************
*: Program file       : APDSTTMP
*: Program description: Distribution Template Report
*: Module             : Accounts Payable (AP)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: E303061.EXE
*: Date               : 02/13/2012
*:***************************************************************************
*Modifications:
*:***************************************************************************

SET STEP ON 

SELECT CODES
SET FILTER TO CODES.cRltField='N'

SELECT APINVAHD
SET ORDER TO TAG HTYPCOD 
SET ORDER TO TAG DTYPCOD IN APINVADT
SET RELATION TO APINVAHD.CAUTMTYPE+ APINVAHD.CAUTMCODE INTO APINVADT ADDITIVE
SET SKIP TO APINVADT

lcInvAdtTmp = gfTempName()

lcRpExp = lcRpExp + " AND CAUTMTYPE = 'T'"

=lfCreateTemp()

=lfCollectData()

IF RECCOUNT(lcInvAdtTmp) <= 0
  *- There are no records to display.
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF

=lfAdjustCRSettings()

IF USED(lcInvAdtTmp)
  USE IN (lcInvAdtTmp)
ENDIF

DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp
SET RELATION TO

SELECT CODES

SET FILTER TO

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
*! Name      : lfvTmpCode
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Automatic Code
*!*************************************************************
FUNCTION lfvTmpCode


_Screen.Visible = .T.
SET STEP ON 

LOCAL lcOrder, lcAutoCode
SELECT APINVAHD
lcOrder = SET("ORDER")
SET ORDER TO TAG HTYPCOD 

lcAutoCode = laOgVrFlt[1,6]

IF '?' $ lcAutoCode .OR. !SEEK('T'+lcAutoCode, 'APINVAHD')
  DIMENSION laTemp[2]   
  laTemp = ''
  lcSavBrFld=lcBrFields
  lcSavTitle=lcFile_Ttl

  lcBrFields="CAUTMTYPE :H= 'Template type',;
              CAUTMCODE :H= 'Template code'"
  lcFile_Ttl= "Templates"

  =gfBrows([FOR CAUTMTYPE="T"],'CAUTMTYPE,CAUTMCODE','laTemp')

  lcBrFields=lcSavBrFld
  lcFile_Ttl=lcSavTitle
    
  IF !EMPTY(laTemp[2])
    &lcFldNam=laTemp[2]
  ELSE
    &lcFldNam=SPACE(24)
  ENDIF
ENDIF

IF !EMPTY(lcOrder)
  SET ORDER TO &lcOrder.
ENDIF

ENDFUNC


*!*	SELECT APINVAHD
*!*	lcSavOrd=SET("ORDER")
*!*	SET ORDER TO TAG HTYPCOD 

*!*	lcFldNam=SYS(18)

*!*	IF SEEK("T"+EVAL(SYS(18)))
*!*	  &lcFldNam=APINVAHD.CAUTMCODE
*!*	ELSE
*!*	  IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
*!*	    DIMENSION laTemp[2]   
*!*	    laTemp = ''
*!*	    lcSavBrFld=lcBrFields
*!*	    lcSavTitle=lcFile_Ttl
*!*	  
*!*	    lcBrFields="CAUTMTYPE :H= 'Template type',;
*!*	                CAUTMCODE :H= 'Template code'"
*!*	    lcFile_Ttl= "Templates"

*!*	    =gfBrows([FOR CAUTMTYPE="T"],'CAUTMTYPE,CAUTMCODE','laTemp')

*!*	    lcBrFields=lcSavBrFld
*!*	    lcFile_Ttl=lcSavTitle
*!*	    
*!*	    IF !EMPTY(laTemp[2])
*!*	      &lcFldNam=laTemp[2]
*!*	    ELSE
*!*	      &lcFldNam=SPACE(24)
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF

*!*	IF !EMPTY(lcSavOrd)
*!*	  SET ORDER TO &lcSavOrd
*!*	ENDIF





*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
PROCEDURE lfCreateTemp
LOCAL lnItemPos
lnItemPos = 0
DIMENSION laStruArr[6, 4]

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAutmType'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 1
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAutmCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nApdLinNo'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 4
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cTaxCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cApdGlAct'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nApdAmnt'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 2

=gfCrtTmp(lcInvAdtTmp, @laStruArr, 'CAUTMTYPE+CAUTMCODE', 'HTYPCOD', .F.)

ENDPROC


*!*************************************************************
*! Name      : lfCollectData
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Procedure to Collect Data into temp file
*!*************************************************************
PROCEDURE lfCollectData

SELECT APINVAHD
SCAN FOR &lcRpExp.
  SELECT APINVADT
  SCAN FOR APINVADT.CAUTMTYPE+APINVADT.CAUTMCODE == APINVAHD.CAUTMTYPE+APINVAHD.CAUTMCODE
    m.cAutmType = APINVAHD.CAUTMTYPE
    m.cAutmCode = APINVAHD.CAUTMCODE
    m.nApdLinNo = APINVADT.NAPDLINNO
    m.cTaxCode  = IIF(EMPTY(APINVADT.CTAXCODE),"N/A",LOOKUP(CODES.CDISCREP,'N'+"CTAXCODE  "+APINVADT.CTAXCODE ,CODES.CFLD_NAME,"CCODE_NO"))
    m.cApdGlAct = APINVADT.CAPDGLACT
    m.nApdAmnt  = APINVADT.NAPDAMNT

    SELECT (lcInvAdtTmp)
    APPEND BLANK
    GATHER MEMO MEMVAR 
  ENDSCAN
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
DIMENSION loOgScroll.laCRParams[5,2]

LoOGScroll.llCrystal      = .T.
LoOGScroll.lcOGLastForm   =  LCRPFORM
loOGScroll.cCROrientation = 'P'
loOgScroll.laCRTables[1]  = oAriaApplication.WorkDir + lcInvAdtTmp + ".DBF"

LOCAL lnI 
lnI  = 0
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'ReportName'
loOgScroll.laCRParams[lnI ,2] = "Distribution Templates"
 
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'Layout'
loOgScroll.laCRParams[lnI, 2] = ""

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'SortBy'
loOgScroll.laCRParams[lnI, 2] = ""

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'OpTitle'
loOgScroll.laCRParams[lnI, 2] = lcRpOpTitl

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'lnDateFormat'
loOgScroll.laCRParams[lnI, 2] = IIF(UPPER(ALLTRIM(oAriaApplication.cActCompDateFormat)) == 'BRITISH', 1, 0)

ENDPROC